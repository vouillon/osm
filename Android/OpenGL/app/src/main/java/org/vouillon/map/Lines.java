package org.vouillon.map;

import android.util.Log;

import org.vouillon.app.Scene;
import org.vouillon.database.RTree;
import org.vouillon.utils.Buffer;
import org.vouillon.utils.LruCache;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class Lines extends Buffer {

    private static int leafSize = 2048;

    private File directory;

    public static class Line {
        public int layer, category;
        public double [] coords;
        Line (int cat, int lay, double [] c) {
            layer = lay; category = cat; coords = c;
        }
    }

    private double [] coords = new double [leafSize / 2];
    private int [] nodes = new int [leafSize / 2];
    private int [] indices = new int [leafSize / 2];

    private class Tree {
        Tree (String nm, double min, double max, LruCache c) throws IOException {
            File f = new File(directory, nm);
            name = nm;
            minLevel = min; maxLevel = max;
            tree = new RTree(f);
            leaves = new RandomAccessFile(tree.leaves(), "r");
            cache = new LruCache.Cache<Long,List<Line>>(c) {
                protected List<Line> get(Long i) { try {
                    return decode(i);
                } catch (IOException e) {
                    e.printStackTrace();
                    return new ArrayList<>();
                } }
            };
        }

        private String name;
        private RTree tree;
        private RandomAccessFile leaves;
        private double minLevel, maxLevel;
        LruCache.Cache<Long, List<Line>> cache;

        static final double linearRatio = 50.;

        private List<Line> decode (long pos) throws IOException {
            leaves.seek(pos * leafSize);
            leaves.read(buffer, 0, leafSize);
            setPosition(0);
            int limit = readInt2(0) + 4;
            setPosition(4);
            int i = 0;
            double lat = 0, lon = 0;
            while (bufPos < limit) {
                lat += readSignedVarint();
                lon += readSignedVarint();
                coords[i++] = lon * linearRatio;
                coords[i++] = Geometry.latToY(lat * linearRatio);
            }
            setPosition(2);
            setPosition(limit);
            limit += readInt2(2);
            int node = 0;
            int j = 0;
            while (bufPos < limit) {
                node += readSignedVarint();
                nodes[2 * j] = node;
                node += readSignedVarint();
                nodes[2 * j + 1] = node;
                int cat = readByte();
                int layer = readByte();
                indices[j] = (cat << 24) | (layer << 16) | j;
                j ++;
            }
            Arrays.sort(indices, 0, j);
            int count = j;
            List<Line> l = new ArrayList<>();
            for (i = 0; i < count; i = j) {
                int layerCat = indices[i] >> 16;
                for (j = i + 1; j < count && (layerCat == indices[j] >> 16); j++);
                double coord [] = new double [4 * (j - i)];
                for (int k = 0; k < j - i; k++) {
                    int idx = indices[k + i] & 0xffff;
                    coord[4 * k    ] = coords[2 * nodes[2 * idx]];
                    coord[4 * k + 1] = coords[2 * nodes[2 * idx] + 1];
                    coord[4 * k + 2] = coords[2 * nodes[2 * idx + 1]];
                    coord[4 * k + 3] = coords[2 * nodes[2 * idx + 1] + 1];
                }
                l.add(new Line (layerCat >> 8, layerCat & 0xff, coord));
            }
            return l;
        }

        void load(double level, double xMin, double yMin, double xMax, double yMax, List<Line> l) throws IOException {
            if (level > minLevel && level <= maxLevel) {
//				System.out.println("Searching in " + name);
                RTree.BBox b = Geometry.boundingBox(linearRatio, xMin, yMin, xMax, yMax);
                List<Long> pos = new ArrayList<>();
                tree.find(b, pos);
                for (long i : pos) l.addAll(cache.find (i));
            }
        }
    }

    private Tree [] trees;

    public Lines (File dir, LruCache c) throws IOException {
        super(leafSize);
        directory = dir;
        trees = new Tree [] {new Tree ("large_3", -1, 11.5, c), new Tree ("large_2", 11.5, 12.5, c),
                             new Tree ("large_1", 12.5, 13.5, c), new Tree ("all", 13.5, 30., c)};
    }

    List<Line> l = new ArrayList<>();

    public List<Line> load(double level, double xMin, double yMin, double xMax, double yMax) throws IOException {
        l.clear();
        for (Tree tree : trees) tree.load(level, xMin, yMin, xMax, yMax, l);
        double t = System.nanoTime();
        Collections.sort(l, new LineComparator());
//		System.out.println("size: " + l.size());
        Log.d("Lines", "sort: " + (System.nanoTime() - t) / 1e9 + "  len:" + l.size());
        return l;
    }

    static final int STREAM = 0;
    static final int TAXIWAY = 1;
    static final int TRUNK_LINK = 2;
    static final int SUBWAY = 3;
    static final int SERVICE = 4;
    static final int PRIMARY = 5;
    static final int TERTIARY = 6;
    static final int MOTORWAY = 7;
    static final int CANAL = 8;
    static final int BRIDLEWAY = 9;
    static final int MOTORWAY_LINK = 10;
    static final int TERTIARY_LINK = 11;
    static final int RUNWAY = 12;
    static final int FOOTWAY = 13;
    static final int RIVER = 14;
    static final int SECONDARY_LINK = 15;
    static final int SECONDARY = 16;
    static final int STEPS = 17;
    static final int LIVING_STREET = 18;
    static final int UNCLASSIFIED = 19;
    static final int RESIDENTIAL = 20;
    static final int PEDESTRIAN = 21;
    static final int CYCLEWAY = 22;
    static final int TRACK = 23;
    static final int TRUNK = 24;
    static final int PRIMARY_LINK = 25;
    static final int PATH = 26;
    static final int RAIL = 27;
    static final int ROAD = 28;
    static final int TRAM = 29;

    static final int GR_WATER = 0;
    static final int GR_WAY = 1;

    final int group (int category) {
        switch (category) {
            case RIVER:case CANAL:case STREAM:
                return GR_WATER;
            default:
                return GR_WAY;
        }
    }

    static public int [] order =
            { 2, 4, 26, 7, 19, 25, 23, 29, 1, 11, 27, 20, 3, 12, 0, 21,
              24, 14, 17, 16, 15, 8, 10, 9, 28, 22, 13, 5, 18, 6};

    class LineComparator implements Comparator<Line> {
        public int compare(Line s1, Line s2) {
            int g1 = group(s1.category), g2 = group(s2.category);
            if (g1 != g2) return g1 - g2;
//            int l1 = s1.layer, l2 = s2.layer;
//            if (l1 != l2) return l1 - l2;
            return order[s1.category] - order[s2.category];
        }
    }

    public final static Scene.CommonLineStyle[] outlineStyles = new Scene.CommonLineStyle [30];
    public final static Scene.CommonLineStyle[] casingStyles = new Scene.CommonLineStyle [30];
    public final static Scene.CommonLineStyle[] inlineStyles = new Scene.CommonLineStyle [30];

    static {
        for (int cat = 0; cat < 30; cat++) {
            double r = 0, g = 0, b = 0, w = 0;
            switch (cat) {
                case TRUNK:case MOTORWAY:
                    w = 11; break;
                case TRUNK_LINK:case MOTORWAY_LINK:
                    w = 6.5; break;
                case TERTIARY:case SECONDARY:case PRIMARY:
                    w = 8; break;
                case TERTIARY_LINK:case SECONDARY_LINK:case PRIMARY_LINK:
                    w = 5.5; break;
                case RESIDENTIAL:case UNCLASSIFIED:case LIVING_STREET:case ROAD:
                    w = 6; break;
                case SERVICE:
                    w = 4; break;
                case PEDESTRIAN:case TRACK:case CYCLEWAY:case BRIDLEWAY:
                case FOOTWAY:case PATH:case STEPS:
//                    w = 4; r = g = b = 1; break;
                    break;
                case RAIL:
                    w = 5; r = g = b = 1; break;
                case TRAM:case SUBWAY:
                    w = 6; r = g = b = 1; break;
            }
            if (w > 0) {
                outlineStyles[cat] = new Scene.LineStyle(r, g, b, w, false, false);
                casingStyles[cat] = new Scene.LineStyle(r, g, b, w - 0.5, false, true);
            }
        }
        for (int cat = 0; cat < 30; cat++) {
            double r = 1, g = 1, b = 1, w = 2;
            switch (cat) {
                case TRUNK:case MOTORWAY:
                    r = 1; g = 0.8; b = 0; w = 6; break;
                case TRUNK_LINK:case MOTORWAY_LINK:
                    r = 1; g = 0.8; b = 0; w = 3; break;
                case TERTIARY:case SECONDARY:case PRIMARY:
                    w = 5; break;
                case TERTIARY_LINK:case SECONDARY_LINK:case PRIMARY_LINK:
                    w = 2.5; break;
                case RESIDENTIAL:case UNCLASSIFIED:case LIVING_STREET:case ROAD:
                    r = g = b = 0.8; w = 4; break;
                case SERVICE:
                    r = g = b = 0.8; w = 2.5; break;
                case PEDESTRIAN:case TRACK:case CYCLEWAY:case BRIDLEWAY:
                case FOOTWAY:case PATH:
                    r = g = b = 0; w = 1;
                    inlineStyles[cat] = new Scene.DashedLineStyle(r, g, b, w, false, 12., 20.);
                    continue;
                case STEPS:
                    r = g = b = 0; w = 3;
                    inlineStyles[cat] = new Scene.DashedLineStyle(r, g, b, w, false, 1, 2);
                    continue;
                case RAIL:
                    r = g = b = 0.27; w = 1; break;
                case TRAM:
                    r = g = b = 0.27; w = 2; break;
                case SUBWAY:
                    r = g = b = 0.6; w = 2; break;
                case RUNWAY:
                    r = g = 0.73; b = 0.8; w = 2; break; // should depend on the level...
                case TAXIWAY:
                    r = g = 0.73; b = 0.8; break;
            }
            inlineStyles[cat] = new Scene.LineStyle(r, g, b, w, false, false);
        }
    }
}