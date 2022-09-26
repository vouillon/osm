/*
TODO
- underground features
- map loading

IMPROVEMENTS
- curved dashed lines

Could use z-buffer to avoid sorting polygons
Line width and color could depend on the zoom level
  (adjust street width and make building border disappear)

OpenGL ES 3:
- use TRIANGLE_FAN + restart for polygons
- discard stencil (?)
- use vertex array objects + glDrawRangeElements
- store repeated point coordinates in a texture when drawing lines?
- use index to know the point position (edge/side)
- use bit tests
*/

package org.vouillon.app;

import android.opengl.GLES20;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class Scene {
    Scene (double x, double y) {
        reset(x, y);
    }
    Scene () {}

    void reset (double x, double y) {
        offsetX = x; offsetY = y;
        list.clear();
        pointPos = pointStart = 0;
        piPos = piStart = 0;
        pos = start = 0;
        indexPos = indexStart = 0;
    }

    private abstract class Primitive {
        abstract void render(GraphicState g, float[] scale, float[] transform, float zoom);
    }

    private static final boolean useStrip = false; // Strip seems slower
    private static final boolean backToFront = true;
    private static final boolean linesUseStrip = false; // BOGUS (need restart)

    double offsetX, offsetY;

    Random r = new Random();

    private class Polygon extends Primitive {
        float[] color;
        int pos, len, indexPos, indexLen;

        Polygon(float[] c, int p, int l, int ip, int il) {
//            Log.d(TAG, "polygon: " + l);
            color = c; pos = p; len = l; indexPos = ip; indexLen = il;
        }

        void render(GraphicState g, float[] scale, float[] transform, float zoom) {
            GLES20.glUseProgram(g.polyProgram);
            int colorLoc = GLES20.glGetUniformLocation(g.polyProgram, "color");
//            color = new float[]{(float) (r.nextDouble() % 1.), (float) (r.nextDouble() % 1.), (float) (r.nextDouble() % 1.), 1f};
            GLES20.glUniform4fv(colorLoc, 1, color, 0);
            int pointLoc = GLES20.glGetAttribLocation(g.polyProgram, "p");
            GLES20.glEnableVertexAttribArray(pointLoc);
            GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, pointBuf);
            GLES20.glVertexAttribPointer(pointLoc, 2, GLES20.GL_FLOAT, false, 0, 8 * pos);
            GLES20.glBindBuffer(GLES20.GL_ELEMENT_ARRAY_BUFFER, piBuf);

            GLES20.glEnable(GLES20.GL_STENCIL_TEST);
            GLES20.glDisable(GLES20.GL_CULL_FACE);
            GLES20.glColorMask(false, false, false, false);
            GLES20.glStencilFunc(GLES20.GL_ALWAYS, 0, 0xff);
            GLES20.glStencilOpSeparate
                    (GLES20.GL_FRONT, GLES20.GL_INCR_WRAP, GLES20.GL_INCR_WRAP,
                            GLES20.GL_INCR_WRAP);
            GLES20.glStencilOpSeparate
                    (GLES20.GL_BACK, GLES20.GL_DECR_WRAP, GLES20.GL_DECR_WRAP,
                            GLES20.GL_DECR_WRAP);
            int kind = useStrip?GLES20.GL_TRIANGLE_STRIP:GLES20.GL_TRIANGLES;
            GLES20.glDrawElements(kind, indexLen, GLES20.GL_UNSIGNED_SHORT, 2 * indexPos);
            if (!backToFront) GLES20.glEnable(GLES20.GL_DEPTH_TEST);
            GLES20.glEnable(GLES20.GL_CULL_FACE);
            GLES20.glCullFace(GLES20.GL_BACK);
            GLES20.glColorMask(true, true, true, true);
            GLES20.glStencilFunc(GLES20.GL_NOTEQUAL, 0, 0xff);
            GLES20.glStencilOp(GLES20.GL_REPLACE, GLES20.GL_REPLACE, GLES20.GL_REPLACE);
            GLES20.glDrawElements(kind, indexLen, GLES20.GL_UNSIGNED_SHORT, 2 * indexPos);
            GLES20.glDisable(GLES20.GL_CULL_FACE);
            GLES20.glDisable(GLES20.GL_STENCIL_TEST);
            if (!backToFront) GLES20.glDisable(GLES20.GL_DEPTH_TEST);
        }
    }

    private static String TAG = "Scene";

    private List<Primitive> list = new ArrayList<Primitive>();

    private float[] points = new float[100];
    private int pointPos = 0, pointStart = 0;
    private short [] pointIndices = new short [100];
    private int piPos = 0, piStart = 0;
    private float[] color = null;

    private void sizePolygonBuffer(int len) {
        while (points.length < 2 * (pointPos + len))
            points = Arrays.copyOf(points, points.length * 3 / 2);
        while (pointIndices.length < piPos + 3 * (len - 2))
            pointIndices = Arrays.copyOf(pointIndices, pointIndices.length * 3 / 2);
    }

    private void flushPolygons() {
        if (pointPos != pointStart) {
            list.add(new Polygon(color, pointStart, pointPos - pointStart,
                                 piStart, piPos - piStart));
            pointStart = pointPos;
            piStart = piPos;
        }
    }

    void addPolygon(Style s, double [] a, int i, int n) {
        if (n > 2) {
            float [] col = s.color;
            flushLines();
            if (!Arrays.equals(col, color)) {
                flushPolygons(); color = col;
            }
            if (pointPos - pointStart + n >= 65536) flushPolygons();
            sizePolygonBuffer(n);
            if (useStrip) {
                int offset = pointPos - pointStart;

                pointIndices[piPos++] = (short) offset;
                for (int j = 0; j < n ; j++) pointIndices[piPos++] = (short) (offset+j);
                pointIndices[piPos++] = (short) (offset + n - 1);

                int j;
                if (offset % 2 == 0) {
                    for (j = 0; j < (n + 1) / 2; j++) {
                        points[2 * pointPos + 4 * j    ] = (float) (a[i + 2 * j] - offsetX);
                        points[2 * pointPos + 4 * j + 1] = (float) (a[i + 2 * j + 1] - offsetY);
                    }
                    for (j = 0; j < n / 2; j++) {
                        points[2 * pointPos + 4 * j + 2] = (float) (a[i + 2 * (n - 1 - j)] - offsetX);
                        points[2 * pointPos + 4 * j + 3] = (float) (a[i + 2 * (n - 1 - j) + 1] - offsetY);
                    }
                } else {
                    for (j = 0; j < (n + 1) / 2; j++) {
                        points[2 * pointPos + 4 * j    ] = (float) (a[i + 2 * (n - 1 - j)] - offsetX);
                        points[2 * pointPos + 4 * j + 1] = (float) (a[i + 2 * (n - 1 - j) + 1] - offsetY);
                    }
                    for (j = 0; j < n / 2; j++) {
                        points[2 * pointPos + 4 * j + 2] = (float) (a[i + 2 * j] - offsetX);
                        points[2 * pointPos + 4 * j + 3] = (float) (a[i + 2 * j + 1] - offsetY);
                    }
                }
            } else {
                for (int j = 1; j < n - 1; j++) {
                    int offset = pointPos - pointStart;
                    pointIndices[piPos++] = (short) offset;
                    pointIndices[piPos++] = (short) (offset + j);
                    pointIndices[piPos++] = (short) (offset + j + 1);
                }
                for (int j = 0; j < n; j++) {
                    points[2*pointPos + 2 * j] = (float) (a[i + 2 * j] - offsetX);
                    points[2*pointPos + 2 * j + 1] = (float) (a[i + 2 * j + 1] - offsetY);
                }
            }
        }
        pointPos += n;
    }

    private float[] coords = new float[100];
    private int pos = 0, start = 0;
    private byte[] data = new byte[200];
    private short[] indices = new short[100];
    private int indexPos = 0, indexStart = 0;
    private float currentWidth;
    private float patternRepeat, patternLen;
    private boolean dashedLine;

    private void addPoint (boolean butt1, boolean butt2, double x, double y) {
        byte style = butt1 ? (byte)3 : 1;
        if (butt2) style = (byte) - style;
        addPoint2(x, y, style);
    }

    private void addPoint2(double x1, double y1, byte style) {
        float x = (float) (x1 - offsetX);
        float y = (float) (y1 - offsetY);
        int p;
        p = 8 * pos;
        coords[p++] = x; coords[p++] = y;
        coords[p++] = x; coords[p++] = y;
        coords[p++] = x; coords[p++] = y;
        coords[p++] = x; coords[p  ] = y;
        p = 8 * pos;
        data[p++] = style; data[p++] = 3;
        data[p++] = style; data[p++] = -3;
        data[p++] = style; data[p++] = 1;
        data[p++] = style; data[p  ] = -1;
        pos++;
    }

    private void sizeLineBuffers (int len) {
//        if  (coords.length < 8 * (pos + len) || data.length < 16 * (pos + len) ||indices.length < indexPos + 6 * len)
//        Log.d(TAG, "size: " + (pos + len) + "  " + (indexPos + 6 * len) + "  " + len);
        while (coords.length < 8 * (pos + len))
            coords = Arrays.copyOf(coords, coords.length * 3 / 2);
        while (data.length < 16 * (pos + len))
            data = Arrays.copyOf(data, data.length * 3 / 2);
        while (indices.length < indexPos + 6 * len)
            indices = Arrays.copyOf(indices, indices.length * 3 / 2);
    }

    void addPolyline(LineStyle style, double[] a, int i, int n) {
        if (n > 1 && (!style.closed || n > 2)) {
            flushPolygons();
            if (pos - start + n >= 65536 / 4) flushLines();
            float [] col = style.color;
            if (!Arrays.equals(col, color) || style.width != currentWidth || dashedLine) {
                flushLines(); color = col; currentWidth = style.width; dashedLine = false;
            }
            buildLineIndices(style.closed, n);
            if (n == 2) {
                addPoint(style.butt, style.butt, a[i], a[i + 1]);
                addPoint(style.butt, style.butt, a[i + 2], a[i + 3]);
            } else {
                boolean butt = !style.closed && style.butt;
                addPoint(butt, false, a[i], a[i + 1]);
                for (int j = 1; j < n - 2; j++)
                    addPoint(false, false, a[i + 2 * j], a[i + 2 * j + 1]);
                addPoint(false, butt, a[i + 2 * n - 4], a[i + 2 * n - 3]);
                addPoint(butt, butt, a[i + 2 * n - 2], a[i + 2 * n - 1]);
                if (style.closed) addPoint(false, false, a[i], a[i + 1]);
            }
        }
    }

    void addDashedPolyline(DashedLineStyle style, double[] a, int i, int n) {
        if (n > 1 && (!style.closed || n > 2)) {
            flushPolygons();
            if (pos - start + n >= 65536 / 4) flushLines();
            float [] col = style.color;
            if (!Arrays.equals(col, color) || style.width != currentWidth ||
                    !dashedLine || patternRepeat != style.repeat || patternLen != style.len) {
                flushLines(); color = col; currentWidth = style.width;
                dashedLine = true; patternRepeat = style.repeat; patternLen = style.len;
            }
            buildLineIndices(style.closed, n);
            double length = 0;
            byte pos = 0;
            for (int j = 0; j < n; j++) {
                addPoint2(a[i + 2 * j], a[i + 2 * j + 1], pos);
                int k = (j + 1) % n;
                double dx = a[i + 2 * k] - a[i + 2 * j];
                double dy = a[i + 2 * k + 1] - a[i + 2 * j + 1];
                length += Math.sqrt(dx * dx  + dy * dy);
                pos = (byte) (((length / patternRepeat / style.width * 2) % 1) * 256);
                //TODO: update length % patternRepeat
            }
            if (style.closed) addPoint2(a[i], a[i + 1], pos);
        }
    }

    void addPolyline(CommonLineStyle style, double [] a, int i, int n) {
        if (style instanceof LineStyle)
            addPolyline((LineStyle) style, a, i, n);
        else
            addDashedPolyline((DashedLineStyle) style, a, i, n);
    }

    private void buildLineIndices(boolean closed, int n) {
        sizeLineBuffers(n + 3);
        int m = closed ? n : n - 1;
        if (linesUseStrip) {
            int ofs = 4 * (pos - start);
            indices[indexPos++] = (short) ofs;
            for (int j = 0; j < m; j++, ofs += 4) {
                indices[indexPos++] = (short) (ofs    ); indices[indexPos++] = (short) (ofs + 1);
                indices[indexPos++] = (short) (ofs + 2); indices[indexPos++] = (short) (ofs + 3);
            }
            indices[indexPos++] = (short) (ofs - 1);
        } else {
            for (int j = 0, ofs = 4 * (pos - start); j < m; j++, ofs += 4) {
                indices[indexPos++] = (short) (ofs    ); indices[indexPos++] = (short) (ofs + 1);
                indices[indexPos++] = (short) (ofs + 2); indices[indexPos++] = (short) (ofs + 2);
                indices[indexPos++] = (short) (ofs + 1); indices[indexPos++] = (short) (ofs + 3);
            }
        }
    }

    private class Line extends Primitive {
        int pos, index, len;
        float [] color;
        float width;
        Line (float [] c, float w, int p, int i, int l) {
//            Log.d(TAG, "line: " + l);
            color = c; width = w; pos = p; index = i; len = l;
        }

        void render(GraphicState g, float[] scale, float[] transform, float zoom) {
            GLES20.glUseProgram(g.lineProgram);
            int colorLoc = GLES20.glGetUniformLocation(g.lineProgram, "color");
            GLES20.glUniform4fv(colorLoc, 1, color, 0);
            int widthLoc = GLES20.glGetUniformLocation(g.lineProgram, "half_width");
//            GLES20.glUniform1f(widthLoc, width == 1 && color[0] > 0.3 ? (float) Math.min(width / 2 * zoom, 0.5) : width / 2 * zoom);
            GLES20.glUniform1f(widthLoc, width / 2);
            int p1 = GLES20.glGetAttribLocation(g.lineProgram, "p1");
            GLES20.glEnableVertexAttribArray(p1);
            GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, coordBuf);
            GLES20.glVertexAttribPointer(p1, 2, GLES20.GL_FLOAT, false, 0, 32*pos);
            int p2 = GLES20.glGetAttribLocation(g.lineProgram, "p2");
            GLES20.glEnableVertexAttribArray(p2);
            GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, coordBuf);
            GLES20.glVertexAttribPointer(p2, 2, GLES20.GL_FLOAT, false, 0, 32*pos+32);
            int data = GLES20.glGetAttribLocation(g.lineProgram, "data");
            GLES20.glEnableVertexAttribArray(data);
            GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, dataBuf);
            GLES20.glVertexAttribPointer(data, 2, GLES20.GL_BYTE, false, 0, 8*pos);
            GLES20.glBindBuffer(GLES20.GL_ELEMENT_ARRAY_BUFFER, indexBuf);
            int kind = linesUseStrip?GLES20.GL_TRIANGLE_STRIP:GLES20.GL_TRIANGLES;
            if (linesUseStrip) {
                GLES20.glEnable(GLES20.GL_CULL_FACE);
                GLES20.glCullFace(GLES20.GL_FRONT);
            }
            GLES20.glDrawElements(kind, len, GLES20.GL_UNSIGNED_SHORT, 2 * index);
            GLES20.glDisableVertexAttribArray(p1);
            GLES20.glDisableVertexAttribArray(p2);
            GLES20.glDisableVertexAttribArray(data);
            if (linesUseStrip)
                GLES20.glDisable(GLES20.GL_CULL_FACE);
        }
    }

    private class DashedLine extends Primitive {
        int pos, index, len;
        float patternLen, patternRepeat;
        float [] color;
        float width;
        DashedLine (float [] c, float w, int p, int i, int l, float pl, float pr) {
            //Log.d(TAG, "line: " + l);
            color = c; width = w; pos = p; index = i; len = l;
            patternLen = pl; patternRepeat = pr;
        }

        void render(GraphicState g, float[] scale, float[] transform, float zoom) {
            GLES20.glUseProgram(g.dashedLineProgram);
            int patternLoc = GLES20.glGetUniformLocation(g.dashedLineProgram, "pattern");
            GLES20.glUniform2f(patternLoc, patternLen, patternRepeat);
            int colorLoc = GLES20.glGetUniformLocation(g.dashedLineProgram, "color");
            GLES20.glUniform4fv(colorLoc, 1, color, 0);
            int widthLoc = GLES20.glGetUniformLocation(g.dashedLineProgram, "half_width");
//            GLES20.glUniform1f(widthLoc, width == 1 && color[0] > 0.3 ? (float) Math.min(width / 2 * zoom, 0.5) : width / 2 * zoom);
            GLES20.glUniform1f(widthLoc, width / 2);
            int p1 = GLES20.glGetAttribLocation(g.dashedLineProgram, "p1");
            GLES20.glEnableVertexAttribArray(p1);
            GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, coordBuf);
            GLES20.glVertexAttribPointer(p1, 2, GLES20.GL_FLOAT, false, 0, 32*pos);
            int p2 = GLES20.glGetAttribLocation(g.dashedLineProgram, "p2");
            GLES20.glEnableVertexAttribArray(p2);
            GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, coordBuf);
            GLES20.glVertexAttribPointer(p2, 2, GLES20.GL_FLOAT, false, 0, 32*pos+32);
            int data = GLES20.glGetAttribLocation(g.dashedLineProgram, "data");
            GLES20.glEnableVertexAttribArray(data);
            GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, dataBuf);
            GLES20.glVertexAttribPointer(data, 2, GLES20.GL_BYTE, false, 0, 8*pos);
            GLES20.glBindBuffer(GLES20.GL_ELEMENT_ARRAY_BUFFER, indexBuf);
            int kind = linesUseStrip?GLES20.GL_TRIANGLE_STRIP:GLES20.GL_TRIANGLES;
            if (linesUseStrip) {
                GLES20.glEnable(GLES20.GL_CULL_FACE);
                GLES20.glCullFace(GLES20.GL_BACK);
            }
            GLES20.glDrawElements(kind, len, GLES20.GL_UNSIGNED_SHORT, 2 * index);
            GLES20.glDisableVertexAttribArray(p1);
            GLES20.glDisableVertexAttribArray(p2);
            GLES20.glDisableVertexAttribArray(data);
            if (linesUseStrip)
                GLES20.glDisable(GLES20.GL_CULL_FACE);
        }
    }

    private void flushLines() {
        if (pos != start) {
            if (!dashedLine)
                list.add(new Line(color, currentWidth, start, indexStart, indexPos - indexStart));
            else
                list.add(new DashedLine(color, currentWidth, start,
                                indexStart, indexPos - indexStart, patternLen, patternRepeat));
            start = pos; indexStart = indexPos;
        }
    }

    private Buffer pointBuffer, piBuffer, coordBuffer, dataBuffer, indexBuffer;

    void allocateBuffers() {
        flushPolygons(); flushLines();

        pointBuffer = FloatBuffer.wrap(points, 0, 2 * pointPos);
//        points = null;
        piBuffer = ShortBuffer.wrap(pointIndices, 0, piPos);
//        pointIndices = null;
        coordBuffer = FloatBuffer.wrap(coords, 0, 8 * pos);
//        coords = null;
        dataBuffer = ByteBuffer.wrap(data, 0, 8 * pos);
//        data = null;
        indexBuffer = ShortBuffer.wrap(indices, 0, indexPos);
//        indices = null;
        /*
        Log.d(TAG, "points: " + pointPos);
        Log.d(TAG, "points: " + 8 * pointPos + "bytes");
        Log.d(TAG, "point index: " + 2 * piPos + "bytes");
        Log.d(TAG, "coord: " + 4 * 8 * pos + "bytes");
        Log.d(TAG, "data: " + 8 * pos + "bytes");
        Log.d(TAG, "index: " + 2 * indexPos + "bytes");
        */
    }

    private int pointBuf, piBuf, coordBuf, dataBuf, indexBuf;
    private int [] buffers;

    void prepareRendering() {
        buffers = new int [6];
        GLES20.glGenBuffers(6, buffers, 0);
        coordBuf = buffers[0];
        GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, coordBuf);
        GLES20.glBufferData(GLES20.GL_ARRAY_BUFFER, 4 * coordBuffer.limit(),
                coordBuffer, GLES20.GL_STATIC_DRAW);
        GLES20.glBufferData(GLES20.GL_ARRAY_BUFFER, 4 * coordBuffer.limit(),
                            coordBuffer, GLES20.GL_STATIC_DRAW);
        dataBuf = buffers[2];
        GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, dataBuf);
        GLES20.glBufferData(GLES20.GL_ARRAY_BUFFER, dataBuffer.limit(),
                dataBuffer, GLES20.GL_STATIC_DRAW);
        indexBuf = buffers[3];
        GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, indexBuf);
        GLES20.glBufferData(GLES20.GL_ARRAY_BUFFER, 2 * indexBuffer.limit(),
                indexBuffer, GLES20.GL_STATIC_DRAW);
        pointBuf = buffers[4];
        GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, pointBuf);
        GLES20.glBufferData(GLES20.GL_ARRAY_BUFFER, 4 * pointBuffer.limit(),
                pointBuffer, GLES20.GL_STATIC_DRAW);
        piBuf = buffers[5];
        GLES20.glBindBuffer(GLES20.GL_ARRAY_BUFFER, piBuf);
        GLES20.glBufferData(GLES20.GL_ARRAY_BUFFER, 2 * piBuffer.limit(),
                piBuffer, GLES20.GL_STATIC_DRAW);
    }

    void stopRendering () {
        GLES20.glDeleteBuffers(6, buffers, 0);
    }

    void render(GraphicState g, float width, float height, float[] transform, boolean usesCoverageAa) {
        float[] scale = {2f / width, 2f / height};
        float det = transform[0] * transform[4] - transform[1] * transform[3];
        float zoom = (float) Math.sqrt(det);

        if (backToFront)
            GLES20.glClearColor(1, 1, 1, 1);
        else
            GLES20.glClearColor(0, 0, 0, 0);
        if (backToFront)
            GLES20.glBlendFunc(GLES20.GL_ONE, GLES20.GL_ONE_MINUS_SRC_ALPHA);
        else
            GLES20.glBlendFunc(GLES20.GL_ONE_MINUS_DST_ALPHA, GLES20.GL_ONE);
        GLES20.glEnable(GLES20.GL_BLEND);
        GLES20.glDisable(GLES20.GL_DEPTH_TEST);

        int clearMask = GLES20.GL_COLOR_BUFFER_BIT | GLES20.GL_DEPTH_BUFFER_BIT;
        if (usesCoverageAa) {
            final int GL_COVERAGE_BUFFER_BIT_NV = 0x8000;
            clearMask |= GL_COVERAGE_BUFFER_BIT_NV;
        }
        GLES20.glClear(clearMask);

        GLES20.glUseProgram(g.polyProgram);
        int scaleLoc = GLES20.glGetUniformLocation(g.polyProgram, "scale");
        GLES20.glUniform2f(scaleLoc, scale[0], scale[1]);
        int transformLoc = GLES20.glGetUniformLocation(g.polyProgram, "transform");
        GLES20.glUniformMatrix3fv(transformLoc, 1, false, transform, 0);

        GLES20.glUseProgram(g.lineProgram);
        scaleLoc = GLES20.glGetUniformLocation(g.lineProgram, "scale");
        GLES20.glUniform2f(scaleLoc, scale[0], scale[1]);
        transformLoc = GLES20.glGetUniformLocation(g.lineProgram, "transform");
        GLES20.glUniformMatrix3fv(transformLoc, 1, false, transform, 0);
        int zoomLoc = GLES20.glGetUniformLocation(g.lineProgram, "zoom");
        GLES20.glUniform1f(zoomLoc, zoom);

        GLES20.glUseProgram(g.dashedLineProgram);
        scaleLoc = GLES20.glGetUniformLocation(g.dashedLineProgram, "scale");
        GLES20.glUniform2f(scaleLoc, scale[0], scale[1]);
        transformLoc = GLES20.glGetUniformLocation(g.dashedLineProgram, "transform");
        GLES20.glUniformMatrix3fv(transformLoc, 1, false, transform, 0);
        zoomLoc = GLES20.glGetUniformLocation(g.dashedLineProgram, "zoom");
        GLES20.glUniform1f(zoomLoc, zoom);

        for (Primitive p : list)
            p.render(g, scale, transform, zoom);
    }

    public static class Style {
        public Style(double red, double green, double blue) {
            color = new float [] { (float) red, (float) green, (float) blue, 1 };
        }
        float [] color;
    }
    public static class CommonLineStyle extends Style {
        public CommonLineStyle(double r, double g, double b, double w, boolean c) {
            super(r, g, b);
            width = (float) w;
            closed = c;
        }
        float width;
        boolean closed;
    }
    public static class LineStyle extends CommonLineStyle {
        public LineStyle(double r, double g, double b, double w, boolean c, boolean bt) {
            super(r, g, b, w, c);
            butt = bt;
        }
        boolean butt;
    }
    public static class DashedLineStyle extends CommonLineStyle {
        public DashedLineStyle(double r, double g, double b, double w, boolean c, double ln, double rp) {
            super(r, g, b, w, c);
            len = (float) ln; repeat = (float) rp;
        }
        float len, repeat;
    }
}
