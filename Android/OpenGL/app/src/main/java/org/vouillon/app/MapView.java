/*
XXX
Implement rotation ?
===> Point Set Registration with Integrated Scale Estimation
     http://nghiaho.com/?page_id=671

XXX
Rendering strategies:
- tile-based
  hardware accelerated
  cannot draw in another thread
- off-screen bitmap
  not hardware accelerated
  can draw in another thread
- OpenGL
  hardware accelerated
  can draw in another thread
  need to implement drawing primitives (lines, polygons) ourselves
*/
package org.vouillon.app;

import android.content.Context;
import android.opengl.GLSurfaceView;
import android.util.Log;
import android.view.GestureDetector;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;

import org.jetbrains.annotations.NotNull;

import javax.microedition.khronos.egl.EGL10;
import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.egl.EGLDisplay;

public class MapView extends GLSurfaceView {

    private static final String TAG = "MapView";

    final org.vouillon.app.Renderer renderer;

    public MapView(Context context) {
        super(context);

        final boolean[] usesCoverageAa = new boolean[]{false};

        setEGLContextClientVersion(2);
//        setDebugFlags(GLSurfaceView.DEBUG_CHECK_GL_ERROR | GLSurfaceView.DEBUG_LOG_GL_CALLS);
        setEGLConfigChooser(new GLSurfaceView.EGLConfigChooser() {
            public EGLConfig chooseConfig(EGL10 egl, EGLDisplay display) {
                int[] configSpec = new int[]
                        {EGL10.EGL_RENDERABLE_TYPE, 4, /*EGL10.EGL_CONFIG_CAVEAT, EGL10.EGL_NONE,*/
                                //EGL10.EGL_RED_SIZE, 8, EGL10.EGL_GREEN_SIZE, 8, EGL10.EGL_BLUE_SIZE, 8,
                                EGL10.EGL_STENCIL_SIZE, 8,
                                EGL10.EGL_NONE};
                int[] num_config = new int[1];
                if (!egl.eglChooseConfig(display, configSpec, null, 0, num_config))
                    throw new IllegalArgumentException("eglChooseConfig failed");
                int numConfigs = num_config[0];
                EGLConfig[] configs = new EGLConfig[numConfigs];
                if (!egl.eglChooseConfig(display, configSpec, configs, numConfigs, num_config))
                    throw new IllegalArgumentException("eglChooseConfig#2 failed");
                EGLConfig config = null;
                final int EGL_COVERAGE_SAMPLES_NV = 0x30E1;
                for (EGLConfig conf : configs) {
                    int r = findConfigAttrib(egl, display, conf, EGL10.EGL_RED_SIZE);
                    int g = findConfigAttrib(egl, display, conf, EGL10.EGL_GREEN_SIZE);
                    int b = findConfigAttrib(egl, display, conf, EGL10.EGL_BLUE_SIZE);
                    int samples1 = findConfigAttrib(egl, display, conf, EGL10.EGL_SAMPLES);
                    int samples2 = findConfigAttrib(egl, display, conf, EGL_COVERAGE_SAMPLES_NV);
                    Log.d("OPENGL", "r" + r + "g" + g + "b" + b + "s" + samples1 + "s" + samples2);
                    if (r == 8 && g == 8 && b == 8) {
                        if (config == null) config = conf;
                        if (samples2 > 1) usesCoverageAa[0] = true;
                        if (samples1 > 1 || samples2 > 1) return conf;
                    }
                }
                if (config == null) throw new IllegalArgumentException("No config chosen");
                return config;
            }

            int findConfigAttrib(EGL10 egl, EGLDisplay display, EGLConfig config, int attribute) {
                int[] value = new int[1];
                if (egl.eglGetConfigAttrib(display, config, attribute, value)) return value[0];
                return 0;
            }

        });
        renderer = new org.vouillon.app.Renderer(context, usesCoverageAa, this);
        setRenderer(renderer);
        setRenderMode(RENDERMODE_WHEN_DIRTY);

//        os = new OverScroller(context);
        gd = new GestureDetector(getContext(), new GestureDetector.OnGestureListener() {
            @Override
            public boolean onDown(MotionEvent motionEvent) {
                renderer.abortAnimation();
                synchronized (renderer) {
                    x = renderer.x;
                    y = renderer.y;
                    r = renderer.r;
                }
 //               Log.d(TAG, "down " + x + " " + y + " " + r);
//        x = motionEvent.getX();
//        y = motionEvent.getY();
//                ViewCompat.postInvalidateOnAnimation(MapView.this);
                return true;
            }

            @Override
            public void onShowPress(MotionEvent motionEvent) {
            }

            @Override
            public boolean onSingleTapUp(MotionEvent motionEvent) {
                return performClick();
            }

            @Override
            public boolean onScroll(MotionEvent motionEvent, MotionEvent motionEvent2, float dx, float dy) {
                x = x - dx;
                y = y - dy;
//                ViewCompat.postInvalidateOnAnimation(MapView.this);
//                Log.d(TAG, "scroll " + x + " " + y + " " + r);
                updatePosition();
                return true;
            }

            @Override
            public void onLongPress(MotionEvent motionEvent) {
                performLongClick();
            }

            @Override
            public boolean onFling(MotionEvent motionEvent, MotionEvent motionEvent2, final float vx, final float vy) {
 //               Log.d(TAG, "fling " + x + " " + y + " " + r);
                MapView.this.queueEvent(new Runnable() {
                    @Override
                    public void run() {
                        renderer.fling(vx, vy);
                        requestRender();
                    }
                });
 //               os.fling((int) x, (int) y, (int) vx, (int) vy, 0, getWidth(), 0, getHeight());
//                ViewCompat.postInvalidateOnAnimation(MapView.this);
                return false;
            }
        });

        sgd = new ScaleGestureDetector(context, new ScaleGestureDetector.OnScaleGestureListener() {
            @Override
            public boolean onScale(ScaleGestureDetector scaleGestureDetector) {
                float fx = scaleGestureDetector.getFocusX();
                float fy = scaleGestureDetector.getFocusY();
                float sc = scaleGestureDetector.getScaleFactor();
                x = (x - fx) * sc + fx;
                y = (y - fy) * sc + fy;
                r *= sc;
                updatePosition();
 //               Log.d(TAG, "scale " + x + " " + y + " " + r);
//                ViewCompat.postInvalidateOnAnimation(MapView.this);
                return true;
            }

            @Override
            public boolean onScaleBegin(ScaleGestureDetector scaleGestureDetector) {
                return true;
            }

            @Override
            public void onScaleEnd(ScaleGestureDetector scaleGestureDetector) {

            }
        });
        ogd = new OrientationGestureDetector(new OrientationGestureDetector.OnOrientationGestureListener() {
            @Override
            public void onChange(float fx, float fy, float sc, float a) {
                float s = (float) Math.sin(a);
                float c = (float) Math.cos(a);
                x -= fx;
                y -= fy;
                x *= sc;
                y *= sc;
                double x1 = x * c - y * s;
                double y1 = x * s + y * c;
                x = x1 + fx;
                y = y1 + fy;
                r *= sc;
                angle += a;
//                ViewCompat.postInvalidateOnAnimation(MapView.this);
//                Log.d(TAG, "rotate " + x + " " + y + " " + r);
                updatePosition();
            }
        });
    }

    boolean changed;
    private double x, y, r = 1;
    private double angle;

    private void updatePosition() {
        changed = true;
    }

    private GestureDetector gd;
    private ScaleGestureDetector sgd;
    private OrientationGestureDetector ogd;

    @Override
    public boolean onTouchEvent(@NotNull MotionEvent event) {
        boolean r1 = gd.onTouchEvent(event);
        boolean r2 = true;
        //r2 = sgd.onTouchEvent(event);
        ogd.onTouchEvent(event);
        if (changed) {
            changed = false;
            renderer.setPosition(x, y, r, angle);
        }
        return r1 || r2;
    }
}
