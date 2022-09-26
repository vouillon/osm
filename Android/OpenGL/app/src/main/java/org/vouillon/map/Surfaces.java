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
import java.util.Comparator;
import java.util.List;
import java.util.Scanner;

public class Surfaces extends Buffer {

    private static final String TAG = "Surfaces";
    private static int leafSize = 2048;

	private File directory;
	
	public static class Surface {
		public int category, layer;
		public double[][] ways;
		double area = 0;
		
		Surface (int c, int l, List<double []> w) {
			category = c; layer = l; ways = w.toArray(new double[w.size()][]);
			for (double [] coord : ways) area += Geometry.polygonArea(coord);
		}
	}

	private class Tree {
		Tree (String nm, double min, double max, LruCache c) throws IOException {
			File f = new File(directory, nm);
			name = nm;
			minLevel = min; maxLevel = max;
			tree = new RTree(f);
			Scanner s = new Scanner (new File (f, "ratio"));
			ratio = s.nextInt();
			s.close ();
			leaves = new RandomAccessFile (tree.leaves(), "r");
			cache = new LruCache.Cache<Long,List<Surface>>(c) {
				protected List<Surface> get(Long i) { try {
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
		private int ratio;
		private double minLevel, maxLevel;
		LruCache.Cache<Long, List<Surface>> cache;

        private void load (long pos) throws IOException {
            setPosition(0);
            leaves.seek(pos * leafSize);
            leaves.read(buffer, 0, leafSize);
            int len = readInt2();
            if (len > 1) {
                Log.d (TAG, "LARGE SURFACE  " + len);
                if (buffer.length < len * leafSize)
                    buffer = Arrays.copyOf(buffer, len * leafSize);
                leaves.seek((pos + 1) * leafSize);
                leaves.read(buffer, leafSize, (len - 1) * leafSize);
            }
        }
		private List<Surface> decode (long pos) throws IOException {
            load(pos);
			setPosition(2);
			int n = readInt2();
//			System.out.println("positions: " + positions + " len: " + len + " n: "+ n);
			long lat = 0, lon = 0;
			List<double[]> ways = new ArrayList<>();
			int category = 0, layer = 0;
			List<Surface> lst = new ArrayList<>();
			setPosition(4 + 4 * n);
            int sz = 0;
            int count = 0;
			for (int i = 0; i < n; i++) {
				int l = readInt2(4 * i + 4);
                sz += l;
				int cat = readByte(4 * i + 6);
				int lay = readByte(4 * i + 7) - 128;
  //              if (cat == 0) Log.d(TAG, "++");
				if (cat != 0) {
                    count ++;
					if (ways.size() > 0) lst.add (new Surface (category, layer, ways));
					category = cat; layer = lay; ways.clear();
					// System.out.println("cat: " +cat);
				}
				double [] coord = new double [2 * l + 2];
				for (int j = 0; j < l; j++) {
					lat += readSignedVarint();
					lon += readSignedVarint();
					coord[2 * j] = lon * ratio;
					coord[2 * j + 1] = Geometry.latToY (lat * ratio);
				}
				// System.out.println("" + coord[0] / 10_000_000 + " " + coord[1] / 10_000_000);
				coord[2 * l] = coord[0]; coord[2 * l + 1] = coord[1];
				ways.add(coord);
			}
            if (ways.size() > 0) lst.add (new Surface (category, layer, ways));
//            Log.d(TAG, "polygons: " + n + "("+ count +")  points: " + sz);
            return lst;
		}

        List<Long> positions = new ArrayList<>();

		void load(double level, double xMin, double yMin, double xMax, double yMax, List<Surface> l) throws IOException {
			if (level > minLevel && level <= maxLevel) {
//				System.out.println("Searching in " + name);
				RTree.BBox b = Geometry.boundingBox(ratio, xMin, yMin, xMax, yMax);
                positions.clear();
				tree.find(b, positions);
//                Log.d(TAG, "blocks: " + positions.size());
//                double t2 = System.nanoTime();
				for (long i : positions) l.addAll(cache.find (i));
 //               Log.d(TAG, "block loading: " + (System.nanoTime() - t2) / 1e9);
			}
		}

    }

	private Tree [] trees;

	public Surfaces (File dir, LruCache c) throws IOException {
        super (leafSize);
		directory = dir;
		trees = new Tree [] {new Tree ("06", -1, 6, c),
                             new Tree ("07", 6, 7, c),
                             new Tree ("08", 7, 8, c),
                             new Tree ("09", 8, 9, c),
                             new Tree ("10", 9, 10, c),
                             new Tree ("12", 10, 12, c),
                             new Tree ("large", 12., 30., c),
							 new Tree ("small", 15.5, 30., c)};
	}

    long [] index = {};

    public long [] load(double level, double xMin, double yMin, double xMax, double yMax, List<Surface> l) throws IOException {
        l.clear ();
        for (Tree tree : trees) tree.load(level, xMin, yMin, xMax, yMax, l);
//        double t = System.nanoTime();
        int len = l.size ();
        if (index.length < len) index = new long [len + len / 20];
        for (int i = 0; i < len; i++) {
            Surface s= l.get(i);
            index[i] = (((long) group(s.category)) << 61) | (((long) (s.layer + 128)) << 53) |
                       ((-Double.doubleToRawLongBits(s.area) >>> 27) << 16) | i;
        }
        Arrays.sort (index, 0, len);
        for (int i = 0; i < len; i++) index[i] = index[i] & 0xffff;
//        Log.d("Surfaces", "sort: " + (System.nanoTime() - t)/ 1e9);
/*
        t = System.nanoTime();
        Surface [] res = l.toArray(new Surface[l.size()]);
        Arrays.sort(res, new SurfaceComparator());
//		System.out.println("size: " + l.size());
        Log.d("Surfaces", "sort: " + (System.nanoTime() - t)/ 1e9 + " count: " + res.length);
*/
        return index;
    }

    static final int HIGHWAY_LIVING_STREET = 0;
	static final int GLACIER = 1;
	public static final int WATER = 2;
	static final int HIGHWAY_FOOTWAY = 3;
	static final int INDUSTRIAL = 4;
	static final int CEMETERY = 5;
	static final int HIGHWAY_TRACK = 6;
	static final int FARMLAND = 7;
	public static final int HIGHWAY_PEDESTRIAN = 8;
	static final int HIGHWAY_PATH = 9;
	static final int RESIDENTIAL = 10;
	static final int COMMERCIAL = 11;
	static final int GRASS = 12;
	public static final int BUILDING = 13;
	static final int HIGHWAY_RESIDENTIAL = 14;
	static final int PARKING = 15;
	public static final int FOREST = 16;
	static final int PARK = 17;
	static final int ROCK = 18;
	static final int SAND = 19;
	static final int HEATH = 20;
	static final int HIGHWAY_SERVICE = 21;
	static final int HIGHWAY_UNCLASSIFIED = 22;

	static final int GR_LANDUSE = 0;
	static final int GR_WATER = 1;
	static final int GR_BUILDING = 2;
	static final int GR_HIGHWAY = 3;
	
	final int group (int category) {
		switch (category) {
		case WATER:
			return GR_WATER;
		case BUILDING: case HIGHWAY_PEDESTRIAN: case HIGHWAY_TRACK:
		case HIGHWAY_FOOTWAY: case HIGHWAY_PATH:
			return GR_BUILDING;
		case HIGHWAY_RESIDENTIAL: case HIGHWAY_UNCLASSIFIED:
		case HIGHWAY_LIVING_STREET: case HIGHWAY_SERVICE:
			return GR_HIGHWAY;
		default:
			return GR_LANDUSE;
		}
	}

	class SurfaceComparator implements Comparator<Surface> {
		public int compare(Surface s1, Surface s2) {
			int g1 = group(s1.category), g2 = group(s2.category);
			if (g1 != g2) return g1 - g2;
			int l1 = s1.layer, l2 = s2.layer;
			if (l1 != l2) return l1 - l2;
			double a1 = s1.area, a2 = s2.area;
			if (a1 != a2) return a1 > a2 ? -1 : 1;
			return 0;
		}
	}

    static final Scene.Style residential = new Scene.Style(0.8, 0.8, 0.8);
    static final Scene.Style pedestrian = new Scene.Style(0.98, 0.98, 0.98);

    public static final Scene.Style[] styles = {
        residential,
        new Scene.Style(0.80, 0.94, 0.87),
        new Scene.Style(0.52, 0.94, 0.94),
        pedestrian,
        new Scene.Style(0.87, 0.82, 0.85),
        new Scene.Style(0.67, 0.8, 0.69),
        pedestrian,
        new Scene.Style(0.69, 0.94, 0.27),
        pedestrian,
        pedestrian,
        new Scene.Style(0.91, 0.94, 0.94),
        new Scene.Style(0.94, 0.78, 0.78),
        new Scene.Style(0.3, 0.9, 0.3),
        new Scene.Style(0.7, 0.7, 0.7),
        residential,
        new Scene.Style(0.97, 0.94, 0.72),
        new Scene.Style(0.1, 0.7, 0.2),
        new Scene.Style(0.6, 1.0, 0.6),
        new Scene.Style(0.37, 0.42, 0.49),
        new Scene.Style(0.94, 0.93, 0.22),
        new Scene.Style(0.59, 0.74, 0.42),
        residential,
        residential
    };
}
