package org.vouillon.app;

import android.app.Activity;
import android.opengl.GLSurfaceView;
import android.os.Bundle;
import android.util.Log;

import java.io.File;

public class MainActivity extends Activity {
    public static double startTime;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        startTime = System.nanoTime();

        // Create data directory if it does not exist
        new File(getExternalFilesDir(null), "osm").mkdirs();

        GLSurfaceView view = new MapView(this);
        //   view.setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY);
        setContentView(view);
/*
        ViewGroup.LayoutParams layoutParams= view.getLayoutParams();
        layoutParams.width= 200;
        layoutParams.height=200;
        view.setLayoutParams(layoutParams);
*/
        getWindow().setBackgroundDrawable(null);


        Log.d("MainActivity", "" + getResources().getDisplayMetrics().density);
    }
}
