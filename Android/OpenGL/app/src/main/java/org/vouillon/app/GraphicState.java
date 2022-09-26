package org.vouillon.app;

import android.opengl.GLES20;
import android.util.Log;

public class GraphicState {
    static private final String TAG = "GraphicState";
    int polyProgram, lineProgram, dashedLineProgram;

    static private String polyVertexShader = "" +
            "  precision highp float;\n" +
            "  uniform vec2 scale;\n" +
            "  uniform mat3 transform;\n" +
            "  attribute vec2 p;\n" +
            "\n" +
            "  void main () {\n" +
            "    gl_Position = vec4((transform * vec3(p, 1)).xy * scale + vec2(-1., 1.), 0., 1.);\n" +
            "  }\n";
    static private String polyFragmentShader = "" +
            "  precision mediump float;\n" +
            "\n" +
            "  uniform vec4 color;\n" +
            "\n" +
            "  void main () {\n" +
            "    gl_FragColor = color;\n" +
            "  }\n";

    private static String lineVertexShader = "" +
            "precision highp float;\n" +
            "uniform vec2 scale;\n" +
            "uniform mat3 transform;\n" +
            "uniform float zoom;\n" +
            "uniform mediump float half_width;\n" +
            "attribute vec2 p1, p2; // the two extremities of the line\n" +
            "attribute vec2 data;\n" +
            "varying mediump vec4 dx;\n" +
            "varying mediump float sdy;\n" +
            "void main () {\n" +
            "  float style = data.x;\n" +
            "  float side = sign(data.y);\n" +
            "  float edge = abs(data.y)-2.;\n" +
            "  vec2 q1 = (transform * vec3(p1, 1.)).xy;\n" +
            "  vec2 q2 = (transform * vec3(p2, 1.)).xy;\n" +
            "  vec2 dq = q2 - q1;\n" +
            "  vec2 t1 = normalize(dq);\n" +
            "  vec2 n1 = vec2(-t1.y, t1.x);\n" +
            "  float border = half_width + 1.;\n" +
            "  float dy = border * side;\n" +
            "  vec2 q = edge > 0. ? (q1 - border * t1) : (q2 + border * t1);\n" +
            "  q += dy * n1;\n" +
            "  float d1 = dot(t1, q - q1);\n" +
            "  float d2 = dot(t1, q2 - q);\n" +
            "  float d3 = (style < 0.) ? (d1 - half_width) : d1;\n" +
            "  float d4 = (abs(style) > 2.) ? (d2 - half_width) : d2;\n" +
            "  dx = vec4(d1, d2, d3, d4);\n" +
            "  sdy = dy;\n" +
            "  gl_Position = vec4(q * scale + vec2(-1., 1.), 0., 1.);\n" +
            "}\n";

    private static String lineFragmentShader = ""+
            "  #define BLUR 0.6\n" +
            "  precision mediump float;\n" +
            "  uniform lowp vec4 color;\n" +
            "  uniform mediump float half_width;\n" +
            "  varying mediump vec4 dx;\n" +
            "  varying mediump float sdy;\n" +
            "  void main() {\n" +
            // We divide by half_width to avoid an overflow when computing the square
            "    float dy = sdy / half_width;\n" +
            "    vec2 ds = min (dx.xz, dx.yw) / half_width;\n" +
            "    float dx2 = min (0., ds.x);\n" +
            "    float d = sqrt(dx2 * dx2 + dy * dy);\n" +
            "    d = max(d, -ds.y);\n" +
            "    vec2 dists = vec2(1. - d,- 1. - d);\n" +
            "    vec2 alphas = smoothstep(0., 1., dists * (half_width * BLUR) + 0.5);\n" +
            "    float alpha = (alphas.x - alphas.y);\n" +
            "    gl_FragColor = color * alpha;\n" +
            "  }\n";

    private static String lineVertexShader2 = "" +
            "precision highp float;\n" +
            "uniform vec2 scale;\n" +
            "uniform mat3 transform;\n" +
            "uniform float zoom;\n" +
            "uniform mediump float half_width;\n" +
            "attribute vec2 p1, p2; // the two extremities of the line\n" +
            "attribute vec2 data;\n" +
            "varying mediump vec3 dx;\n" +
            "void main () {\n" +
            "  float side = sign(data.y);\n" +
            "  float edge = abs(data.y)-2.;\n" +
            "  vec2 q1 = (transform * vec3(p1, 1.)).xy;\n" +
            "  vec2 q2 = (transform * vec3(p2, 1.)).xy;\n" +
            "  vec2 dq = q2 - q1;\n" +
            "  vec2 t1 = normalize(dq);\n" +
            "  vec2 n1 = vec2(-t1.y, t1.x);\n" +
            "  float border = half_width + 1.;\n" +
            "  float dy = border * side;\n" +
            "  vec2 q = edge > 0. ? (q1 - border * t1) : (q2 + border * t1);\n" +
            "  q += dy * n1;\n" +
            "  float d1 = dot(t1, q - q1);\n" +
            "  float d2 = dot(t1, q2 - q);\n" +
            "  dx = vec3(d1, d2, dy);\n" +
            "  gl_Position = vec4(q * scale + vec2(-1., 1.), 0., 1.);\n" +
            "}\n";

    private static String lineFragmentShader2 = ""+
            "  #define BLUR 0.6\n" +
            "  precision mediump float;\n" +
            "  uniform lowp vec4 color;\n" +
            "  uniform mediump float half_width;\n" +
            "  varying mediump vec3 dx;\n" +
            "  void main() {\n" +
            "    float dy = dx.z / half_width;\n" +
            "    float ds = min (dx.x, dx.y) / half_width;\n" +
            "    float dx2 = min (0., ds);\n" +
            "    float d = sqrt(dx2 * dx2 + dy * dy);\n" +
            "    vec2 dists = vec2(1. - d,- 1. - d);\n" +
            "    vec2 alphas = smoothstep(0., 1., dists * (half_width * BLUR) + 0.5);\n" +
            "    float alpha = (alphas.x - alphas.y);\n" +
            "    gl_FragColor = color * alpha;\n" +
            "  }\n";

    private static String dashedLineVertexShader = "" +
            "precision highp float;\n" +
            "uniform vec2 scale;\n" +
            "uniform mat3 transform;\n" +
            "uniform float zoom;\n" +
            "uniform mediump float half_width;\n" +
            "uniform mediump vec2 pattern;\n" +
            "attribute vec2 p1, p2; // the two extremities of the line\n" +
            "attribute vec2 data;\n" +
            "varying mediump vec3 d;\n" +
            "varying mediump float c;\n" +
            "varying mediump vec2 t;\n" +
            "void main () {\n" +
            "  float pos = data.x / 256.;\n" +
            "  float side = sign(data.y);\n" +
            "  float edge = abs(data.y)-2.;\n" +
            "  vec2 q1 = (transform * vec3(p1, 1.)).xy;\n" +
            "  vec2 q2 = (transform * vec3(p2, 1.)).xy;\n" +
            "  vec2 dq = q2 - q1;\n" +
            "  vec2 t1 = normalize(dq);\n" +
            "  vec2 n1 = vec2(-t1.y, t1.x);\n" +
            "  float border = half_width + 1.;\n" +
            "  float dy = border * side;\n" +
            "  vec2 q = edge > 0. ? (q1 - t1) : (q2 + t1);\n" +
            "  q += dy * n1;\n" +
            "  float d1 = dot(t1, q - q1);\n" +
            "  float d2 = dot(t1, q2 - q);\n" +
            "  d = vec3(dy, d1, d2);\n" +
            "  c = mod(-dot(t1, q1 + vec2(0., 2./scale.y)) + pos * pattern.y * half_width," +
            "          pattern.y * half_width);\n" +
            "  t = vec2(t1.x,t1.y);\n" +
            "  gl_Position = vec4(q * scale + vec2(-1., 1.), 0., 1.);\n" +
            "}\n";

    private static String dashedLineFragmentShader = "" +
            "#define BLUR 0.6\n" +
            "precision mediump float;\n" +
            "uniform lowp vec4 color;\n" +
            "uniform mediump vec2 pattern;\n" +
            "uniform mediump float half_width;\n" +
            "varying mediump vec3 d;\n" +
            "varying mediump float c;\n" +
            "varying mediump vec2 t;\n" +
            "void main() {\n" +
            "  float dx = min (d.y, d.z);\n" +
            "  float dist = abs(d.x);\n" +
            "  dist = max(dist, -dx);\n" +
            "  float pos = dot(t, gl_FragCoord.xy) + c;\n" +
            "  float l = mod(pos + 1., pattern.y * half_width) - 1.;\n" +
            "  vec2 dists2 = vec2(l, l - pattern.x * half_width);\n" +
            "  vec2 alphas2 = smoothstep(0., 1., dists2 * BLUR + 0.5);\n" +
            "  float alpha2 = (alphas2.x - alphas2.y);\n" +
            "  vec2 dists = vec2(half_width - dist,- half_width - dist);\n" +
            "  vec2 alphas = smoothstep(0., 1., dists * BLUR + 0.5);\n" +
            "  float alpha = (alphas.x - alphas.y);\n" +
            "  gl_FragColor = color * alpha * alpha2;\n" +
            "}";

    private static int compile(String vertexShader, String fragmentShader) {
        int vs = GLES20.glCreateShader(GLES20.GL_VERTEX_SHADER);
        GLES20.glShaderSource(vs, vertexShader);
        GLES20.glCompileShader(vs);

        int[] compiled = new int[1];
        GLES20.glGetShaderiv(vs, GLES20.GL_COMPILE_STATUS, compiled, 0);
        if (compiled[0] == 0) Log.e(TAG, GLES20.glGetShaderInfoLog(vs));

        int fs = GLES20.glCreateShader(GLES20.GL_FRAGMENT_SHADER);
        GLES20.glShaderSource(fs, fragmentShader);
        GLES20.glCompileShader(fs);

        GLES20.glGetShaderiv(fs, GLES20.GL_COMPILE_STATUS, compiled, 0);
        if (compiled[0] == 0) Log.e(TAG, GLES20.glGetShaderInfoLog(fs));

        int program = GLES20.glCreateProgram();
        GLES20.glAttachShader(program, vs);
        GLES20.glAttachShader(program, fs);
        GLES20.glLinkProgram(program);

        GLES20.glGetProgramiv(program, GLES20.GL_LINK_STATUS, compiled, 0);
        if (compiled[0] == 0) Log.e(TAG, GLES20.glGetProgramInfoLog(program));
        return program;
    }

    void init () {
        polyProgram = compile(polyVertexShader, polyFragmentShader);
        lineProgram = compile(lineVertexShader, lineFragmentShader);
        dashedLineProgram = compile(dashedLineVertexShader, dashedLineFragmentShader);
    }

    private int framebuffer, texture;

    void handleFramebuffer (float width, float height) {
        if (framebuffer == 0) {
            int [] fb = new int[1], t = new int[1];
            GLES20.glGenFramebuffers(1, fb, 0);
            framebuffer = fb[0];
            GLES20.glGenTextures(1, t, 0);
            texture = t[0];
            GLES20.glBindTexture(GLES20.GL_TEXTURE_2D, texture);
            GLES20.glTexImage2D(GLES20.GL_TEXTURE_2D, 0,
                    GLES20.GL_RGBA, (int) width, (int) height, 0, GLES20.GL_RGBA,
                    GLES20.GL_UNSIGNED_BYTE, null);
            GLES20.glBindFramebuffer(GLES20.GL_FRAMEBUFFER, framebuffer);
            GLES20.glFramebufferTexture2D(GLES20.GL_FRAMEBUFFER, GLES20.GL_COLOR_ATTACHMENT0,
                    GLES20.GL_TEXTURE_2D, texture, 0);
            int status = GLES20.glCheckFramebufferStatus(GLES20.GL_FRAMEBUFFER);
            Log.d(TAG, "framebuffer: " + status);
        }

        GLES20.glBindFramebuffer(GLES20.GL_FRAMEBUFFER, framebuffer);
        GLES20.glClear(GLES20.GL_COLOR_BUFFER_BIT);
        GLES20.glBindFramebuffer(GLES20.GL_FRAMEBUFFER, 0);
    }

}
