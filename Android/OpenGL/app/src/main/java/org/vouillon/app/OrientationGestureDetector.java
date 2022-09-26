package org.vouillon.app;

import android.util.Log;
import android.util.Pair;
import android.view.MotionEvent;

// Point Set Registration with Integrated Scale Estimation
/*
XXX TODO
     * report start and end of gesture
       (we may want to ignore rotations below a given amount
        ==> accumulate rotations and scaling ratios until the application consume them)
     * Handle double tap
     * Better name (?)
*/

public class OrientationGestureDetector {

    public interface OnOrientationGestureListener {
        void onChange(float cx, float cy, float ratio, float angle);
    }

    private OnOrientationGestureListener listener;

    OrientationGestureDetector(OnOrientationGestureListener l) {
        listener = l;
    }

    static float mean(float[] a) {
        float m = 0;
        for (float x : a) m += x;
        return m / a.length;
    }

    static float normalize(float[] a) {
        float m = mean(a);
        for (int i = 0; i < a.length; i++) a[i] -= m;
        return m;
    }

    static float scalarProd(float[] x1, float[] y1, float[] x2, float[] y2) {
        float s = 0;
        for (int i = 0; i < x1.length; i++) s += x1[i] * x2[i] + y1[i] * y2[i];
        return s;
    }

    static float vectProd(float[] x1, float[] y1, float[] x2, float[] y2) {
        float s = 0;
        for (int i = 0; i < x1.length; i++) s += x1[i] * y2[i] - y1[i] * x2[i];
        return s;
    }

    static Pair<float[], float[]> rotate(float u, float v, float[] x, float[] y) {
        float[] x1 = new float[x.length];
        float[] y1 = new float[y.length];
        for (int i = 0; i < x.length; i++) {
            x1[i] = u * x[i] - v * y[i];
            y1[i] = v * x[i] + u * y[i];
        }
        return new Pair<>(x1, y1);
    }

    static float computeAngle(float[] x1, float[] y1, float[] x2, float[] y2) {
        float e = scalarProd(x1, y1, x2, y2);
        float h = vectProd(x1, y1, x2, y2);
        return (float) Math.atan2(h, e);
    }

    static float computeScaleFactor(float[] x1, float[] y1, float[] x2, float[] y2, float angle) {
        Pair<float[], float[]> p = rotate((float) Math.cos(angle), (float) Math.sin(angle), x1, y1);
        return scalarProd(x2, y2, p.first, p.second) / scalarProd(x1, y1, x1, y1);
    }

    private float[] x, y;
    private int[] id;

    public void onTouchEvent(MotionEvent event) {
        int action = event.getActionMasked();
        int count = event.getPointerCount();
        int rem = action == MotionEvent.ACTION_POINTER_UP ? count - 1 : count;

        Log.d("Essai", "event " + event.getActionMasked() + " " + count + " " + (x == null) + " " + (x == null ? 0 : x.length));
        if (rem > 1) {
            float[] x1 = new float[rem];
            float[] y1 = new float[rem];
            if (action == MotionEvent.ACTION_MOVE) {
                for (int i = 0; i < count; i++) {
                    int j = event.findPointerIndex(id[i]);
                    x1[i] = event.getX(j);
                    y1[i] = event.getY(j);
                }
                float cx = normalize(x1);
                float cy = normalize(y1);
                float angle = computeAngle(x, y, x1, y1);
                float ratio = computeScaleFactor(x, y, x1, y1, angle);
                // TODO: stop if span not large enough
                listener.onChange(cx, cy, ratio, angle);
            } else {
                // TODO: if x is null, start gesture
                // TODO: maybe only start when the span is large enough
                id = new int[rem];
                int removedIndex = action == MotionEvent.ACTION_POINTER_UP ? event.getActionIndex() : -1;
                for (int i = 0, j = 0; i < count; i++)
                    if (i != removedIndex) {
                        id[j] = event.getPointerId(i);
                        x1[j] = event.getX(i);
                        y1[j] = event.getY(i);
                        j++;
                    }
                normalize(x1);
                normalize(y1);
            }
            x = x1; y = y1;
        } else {
            // TODO: if x != null, end gesture
            x = y = null; id = null;
        }

    }
}
