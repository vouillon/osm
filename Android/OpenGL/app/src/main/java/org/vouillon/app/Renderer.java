package org.vouillon.app;

import android.content.Context;
import android.content.res.AssetFileDescriptor;
import android.opengl.EGL14;
import android.opengl.GLES20;
import android.opengl.GLSurfaceView;
import android.os.Environment;
import android.util.Log;
import android.widget.OverScroller;

import org.vouillon.map.Geometry;
import org.vouillon.map.Lines;
import org.vouillon.map.Surfaces;
import org.vouillon.utils.LruCache;

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.List;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

public class Renderer implements GLSurfaceView.Renderer {
    private static final String TAG = "Renderer";

    private final OverScroller os;
    private final GLSurfaceView view;
    private int width;
    private int height;

    private Context context;
    private GraphicState g = new GraphicState();
    private Scene scene, prevScene;

    boolean [] usesCoverageAa;

    Renderer (Context ctx, boolean [] antialiased, GLSurfaceView v) {
        context = ctx;
        usesCoverageAa = antialiased;
        view = v;

        os = new OverScroller(context);

        updater = new SceneUpdater(context);
    }

    class SceneUpdater extends Thread {
        SceneUpdater (Context context) {
            double lat = 48.850, lon = 2.350;
            double level = 17;
            double scale = 256. / 360 * Math.pow(2, level);

            x = -(lon * scale) + width / 2;
            y = Geometry.latToY(lat * 10_000_000) / 10_000_000 * scale + height / 2;
            r = scale/1e7;
            initMap (context);
            start ();
        }

        boolean needUpdate, ready = true;

        @Override
        public void run() {
            //noinspection InfiniteLoopStatement
            while (true) {
                synchronized (this) {
                    while (!needUpdate || !ready)
                        try {
                            wait();
                        } catch (InterruptedException ignored) { }
                    needUpdate = false; ready = false;
                }
                Scene s = loadScene4(context);
                s.allocateBuffers();
                replaceScene(s);
            }
        }

        synchronized void update () {
            if (!needUpdate) {
                needUpdate = true; this.notify();
            }
        }

        synchronized void signalReady () {
            ready = true; this.notify ();
        }
    }

    SceneUpdater updater;

    @Override
    public void onSurfaceCreated(GL10 gl10, EGLConfig eglConfig) {
        EGL14.eglSwapInterval(EGL14.eglGetCurrentDisplay(), 0);

        g.init();
        if (scene != null)
            scene.prepareRendering();
    }

    @Override
    public void onSurfaceChanged(GL10 gl10, int w, int h) {
        GLES20.glViewport(0, 0, w, h);
        width = w; height = h;
        if (scene == null) updater.update();
    }

    public double x = 0, y = 0, r = 1, angle = 0, x0, y0;

    public synchronized void setPosition (double x2, double y2, double r2, double angle2) {
        x = x2; y = y2; r = r2; angle = angle2;
        updater.update();
        view.requestRender();
    }

    public synchronized void abortAnimation () {
        os.abortAnimation();
    }
    public synchronized void fling (float vx, float vy) {
        x0 = x; y0 = y;
        os.fling(0, 0, (int) vx, (int) vy, -10000, 10000, -10000, 100000);
    }

    private int frameCount = 0;
    private long startTime = System.nanoTime();

    @Override
    public void onDrawFrame(GL10 gl10) {
        float[] transform = {};
        synchronized (this) {
            if (os.computeScrollOffset()) {
                x = os.getCurrX() + x0;
                y = os.getCurrY() + y0;
                updater.update();
                view.requestRender();
            }
            double c = r * Math.cos (angle);
            double s = r * Math.sin (angle);
            if (scene != null) {
                transform = new float[]{(float) c, (float)- s, 0, (float)s, (float)c, 0,
                        (float) (x + scene.offsetX*c + scene.offsetY * s), (float) (-y+scene.offsetY*c-scene.offsetX * s), 1};
 //               Log.d(TAG, "x: " + (x + scene.offsetX*c + scene.offsetY * s) + " y: " + (-y+scene.offsetY*c-scene.offsetX * s));
 //               Log.d(TAG, "x: " + x + " " + (scene.offsetX*c + scene.offsetY * s) + " y: " + (-y)+ " " + (scene.offsetY*c-scene.offsetX * s));

            }
        }
        if (scene != null)
            scene.render (g, width, height, transform, usesCoverageAa[0]);
        else {
            GLES20.glClearColor(1, 1, 1, 1);
            int clearMask = GLES20.GL_COLOR_BUFFER_BIT | GLES20.GL_DEPTH_BUFFER_BIT;
            if (usesCoverageAa[0]) {
                final int GL_COVERAGE_BUFFER_BIT_NV = 0x8000;
                clearMask |= GL_COVERAGE_BUFFER_BIT_NV;
            }
            GLES20.glClear(clearMask);
        }
 //       Log.d(TAG, "time: " + (System.nanoTime()-MainActivity.startTime)/1e9);

        ++frameCount;
        if (frameCount % 60 == 0) {
            long now = System.nanoTime();
            double elapsedS = (now - startTime) / 1.0e9;
            double msPerFrame = (1000 * elapsedS / frameCount);
            Log.d("Renderer", "ms / frame: " + msPerFrame + " - fps: " + (1000 / msPerFrame));

            frameCount = 0;
            startTime = now;
        }
    }

    void replaceScene (final Scene s) {
        view.queueEvent(new Runnable() {
            @Override
            public void run() {
                s.prepareRendering();
                /*
            }
        });
        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        view.queueEvent(new Runnable() {
            @Override
            public void run() {
            */
                if (scene != null) { scene.stopRendering();
//                    Log.d(TAG, "RELEASE");
                    prevScene = scene; }
                scene = s;
                updater.signalReady();
                view.requestRender();
            }
        });
    }

    private ByteBuffer loadResourceToBuffer(Context context, int resource) {
        try {
            AssetFileDescriptor ad = context.getResources().openRawResourceFd(resource);
            // This will fail for compressed resources:
            FileChannel fc = ad.createInputStream().getChannel();
            MappedByteBuffer b =
                    fc.map(FileChannel.MapMode.READ_ONLY, ad.getStartOffset(), ad.getLength());
            fc.close();
            return b.order(ByteOrder.BIG_ENDIAN);
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    /*
    private Scene loadScene (Context context) {
        Scene scene = new Scene ();
        long start = System.nanoTime();
        DoubleBuffer buf = loadResourceToBuffer(context, R.raw.scene3).asDoubleBuffer();
        int len = (int) buf.get();
        Log.d("GLconfig", "len:" + len);
        double [] a = new double[1];
        for (int i = 0; i < len; i++) {
            int n = (int) buf.get();
            while (a.length < n) a = new double[2 * a.length];
            buf.get(a, 0, n);
            if (a[0] == 1) {
                int l = (n - 4) / 2;
                double r = a[1], g = a[2], b = a[3];
                scene.addPolygon(new Scene.Style(r, g, b), a, 4, l);
            } else {
                int l = (n - 8) / 2;
                double r = a[1], g = a[2], b = a[3], width = a[4];
                boolean closed = a[5] == 1.;
                double join = -a[6], cap = a[7];

                if (width == 1 && r == 0)
                    scene.addDashedPolyline(r, g, b, width, closed, 6f, 10f, a, 8, l);
                else if (width == 3 && r == 0) {
                    scene.addDashedPolyline(r, g, b, width, closed, 0.5f, 1f, a, 8, l);
                } else if (width == 3 && r > 0.26 && r < 0.28) {
                    scene.addDashedPolyline(r, g, b, width, closed, 2f/3f, 8f/3f, a, 8, l);
                } else
                    scene.addPolyline(r, g, b, width, closed, (int) join, (int) cap, a, 8, l);
            }

        }
        Log.d("GLconfig", "loading: " + (System.nanoTime() - start) / 1e9);
        return scene;
    }
    private Scene loadScene2 (Context context) {
        Scene scene = new Scene();
        for (int i = 1; i <= 20; i++) {
            scene.addPolyline(0., 1., 0., 8., false, -2, 2,
                        new double [] {0, 10 * i, 10. * i, 10 * i}, 0, 2);
        }
        scene.addPolyline(1., 0., 0., 15., false, -2, 2, new double [] {0., 0., 102., 102., 0., 102.}, 0, 3);
        return scene;
    }

    private Scene loadScene3 (Context context) {
        Scene scene = new Scene();
        Scene.Style red = new Scene.Style(1, 0, 0);
        scene.addPolygon(red, new double[]{0,0, 100, 0, 100, 100, 0, 100}, 0, 4);

        scene.addPolygon(red, new double[]{110,0, 190, 0, 150, 100}, 0, 3);

        scene.addPolygon(red, new double[]{0,200, 100, 200, 100, 300, 0, 300}, 0, 4);

        scene.addPolygon(red, new double[]{200,0, 300, 0, 300, 100, 200, 100, 250, 50}, 0, 5);

        scene.addPolygon(red, new double[]{0,400, 100, 400, 100, 500, 0, 500}, 0, 4);
        return scene;
    }
    */

    LruCache cache = new LruCache(80);
    Surfaces surfaceData;
    Lines lineData;

    void initMap (Context context) {
        File dir = context.getExternalFilesDir(null);
        try {
            surfaceData = new Surfaces(new File(dir, "osm/surfaces/rtrees"), cache);
            lineData = new Lines(new File(dir, "osm/linear/rtrees"), cache);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    List<Surfaces.Surface> surfaces = new ArrayList<>();

    public Scene loadScene4 (Context context) {
        double x0, y0, r0, angle0;
        synchronized (this) {
            x0 = -x; y0 = -y; r0 = r; angle0 = angle;
        }

        long start = System.nanoTime();

        final double offset = 100; //7; // Math.min(width,height) / 2;

        double c = Math.cos(angle0) / r0;
        double s = Math.sin (angle0) / r0;
        double x1 = c * (x0 - offset) + s * (y0 - offset);
        double y1 = s * (x0 - offset) - c * (y0 - offset);
        double x2 = c * (x0 - offset) + s * (y0 + height + offset);
        double y2 = s * (x0 - offset) - c * (y0 + height + offset);
        double x3 = c * (x0 + width + offset) + s * (y0 - offset);
        double y3 = s * (x0 + width + offset) - c * (y0 - offset);
        double x4 = c * (x0 + width + offset) + s * (y0 + height + offset);
        double y4 = s * (x0 + width + offset) - c * (y0 + height + offset);
        double offsetX = c * x0+ s * y0;
        double offsetY = s * x0 - c * y0;

        double scale = r0 * 1e7;
        double level = Math.log(scale / 256. * 360.) / Math.log(2.) - 1;

        double lonMin = Math.min (Math.min (x1, x2), Math.min (x3, x4)) / 1e7;
        double lonMax = Math.max (Math.max (x1, x2), Math.max (x3, x4)) / 1e7;
        double latMin = Math.min (Math.min (y1, y2), Math.min (y3, y4)) / 1e7;
        double latMax = Math.max (Math.max (y1, y2), Math.max (y3, y4)) / 1e7;

//        double lonMax = (x0 + width + offset) / scale;
//        double latMin = - (y0 + height + offset) / scale;
//        double latMax = - (y0 - offset) / scale;
  //      Log.d("Renderer", "load x: " + x0 /scale + " y: " + y0/scale + " level: " + level + " angle: " + angle0);

        long [] surfaceIndices = null;
        List<Lines.Line> lines = null;
        try {
 //           System.out.println("lonMin: " + lonMin + " latMin: " + latMin + " lonMax: " + lonMax + " latMax: " +latMax);
 //           surfaceData.find(level, lonMin, latMin, lonMax, latMax);
 //           Log.d("GLconfig", "load: " + (System.nanoTime() - start) / 1e9);
            surfaceIndices = surfaceData.load(level, lonMin, latMin, lonMax, latMax, surfaces);
            lines = lineData.load(level, lonMin, latMin, lonMax, latMax);
        } catch (IOException e) {
            e.printStackTrace();
        }
        Log.d("GLconfig", "decode: " + (System.nanoTime() - start) / 1e9);
//        Log.d(TAG, "coord: "+ surfaces.get(0).ways[0][0] + " " + surfaces.get(0).ways[0][1]);
//        Log.d(TAG, "scale: "+ scale + " " + x0 / scale + " " + y0 / scale);

        Scene scene;

//        Log.d(TAG, "ACQUIRE");
        if (prevScene == null)
            scene = new Scene(offsetX, offsetY);
//          scene = new Scene(x0 / r0, - y0 / r0);
        else {
            scene = prevScene;
            prevScene = null;
            scene.reset (offsetX, offsetY);
//            scene.reset (x0 / r0, - y0 / r0);
        }

        int prevCat = -1;
        int len = surfaces.size();
        for (int i = 0; i < len; i++) {
            long idx = surfaceIndices[i];
            Surfaces.Surface surface = surfaces.get((int)idx);
            double [][] ways = surface.ways;
            int category = surface.category;
//            if (surface.category != prevCat) Log.d(TAG, "category:" + surface.category); prevCat = surface.category;
        //    if (category == Surfaces.BUILDING || category != Surfaces.HIGHWAY_PEDESTRIAN)
  //          if (ways.length > 1) Log.d(TAG, "way count: " + ways.length);
   //         Log.d(TAG, "cat " + surface.category + " length " + ways.length);
    //        if (ways.length == 1)
            // TODO: these polygons should be rendered together
            for (int j = 0; j < ways.length; j++)
                scene.addPolygon(Surfaces.styles[category], ways[j], 0, ways[j].length / 2 - 1);
        }
/*
        for (Surfaces.Surface surface : surfaces) {
            double [][] ways = surface.ways;
            for (int i = 0; i < ways.length; i++)
                scene.addPolyline(0., 0., 0., 10., true, 0, 0, ways[i], 0, ways[i].length / 2 - 1);

        }
*/
        for (Lines.Line line : lines) {
            for (int i = 0; i < line.coords.length; i+= 4) {
                Scene.CommonLineStyle style = Lines.outlineStyles[line.category];
//                Log.d(TAG, "cat: " + line.category + "  order: " + Lines.order[line.category]);
                if (style != null) scene.addPolyline(style, line.coords, i, 2);
            }
        }
        for (Lines.Line line : lines) {
            for (int i = 0; i < line.coords.length; i+= 4) {
                scene.addPolyline(Lines.inlineStyles[line.category], line.coords, i, 2);
            }
        }
//      scene.addPolygon(1., 0., 0., new double [] {lonMin*1e7, latMin*1e7, lonMin*1e7, latMax*1e7, lonMax*1e7, latMax*1e7, lonMax*1e7,latMin*1e7}, 0, 4);
//        scene.addPolygon(1., 0., 0., new double [] {x0/r0, -y0/r0, (x0 + width) / r0, -y0 / r0, (x0 + width) / r0, -(y0 + height) / r0, x0/r0, -(y0 + height) / r0}, 0, 4);
//        scene.addPolygon(1., 0., 0., new double [] {x0/r0, -y0/r0, x0/r0, -(y0 + height) / r0, (x0 + width) / r0, -(y0 + height) / r0, (x0 + width) / r0, -y0 / r0}, 0, 4);

        Log.d("GLconfig", "loading: " + (System.nanoTime() - start) / 1e9);
        return scene;
    }
}
