package org.vouillon.database;
import android.util.Log;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.IntBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.util.ArrayList;
import java.util.List;


public class RTree {
	public static class BBox {
		int min_lat, max_lat, min_lon, max_lon;
		BBox () {
			min_lat = min_lon = Integer.MAX_VALUE;
			max_lat = max_lon = Integer.MIN_VALUE;
		}
		public BBox (int lat1, int lat2, int lon1, int lon2) {
			min_lat = lat1; min_lon = lon1;
			max_lat = lat2; max_lon = lon2;
		}
		BBox (BBox b1, BBox b2) {
			min_lat = Math.min (b1.min_lat, b2.min_lat);
			max_lat = Math.max (b1.max_lat, b2.max_lat);
			min_lon = Math.min (b1.min_lon, b2.min_lon);
			max_lon = Math.max (b1.max_lon, b2.max_lon);
		}
		BBox (BBox b, int lat, int lon) {
			min_lat = Math.min (b.min_lat, lat);
			max_lat = Math.max (b.max_lat, lat);
			min_lon = Math.min (b.min_lon, lon);
			max_lon = Math.max (b.max_lon, lon);
		}
		boolean overlaps (BBox b) {
			return min_lat <= b.max_lat && b.min_lat <= max_lat &&
		    	   min_lon <= b.max_lon && b.min_lon <= max_lon;
		}
        boolean overlaps (int min_lat_2, int max_lat_2,  int min_lon_2, int max_lon_2) {
            return min_lat <= max_lat_2 && min_lat_2 <= max_lat &&
                    min_lon <= max_lon_2 && min_lon_2 <= max_lon;
        }
		public String toString () {
		  return "[" + min_lat + " " + max_lat + " " + min_lon + " " + max_lon + "]";
		}
	}

	private File leaves;

	public RTree (File dir) throws IOException {
		this (dir, 1024);
	}
	RTree (File dir, int s) throws IOException {
		nodeSize = s;
		leaves = new File(dir, "0");
		int i = 1;
		ArrayList<IntBuffer> l = new ArrayList<> ();
		while (true) {
			File f = new File (dir, Integer.toString(i));
			if (!f.exists()) break;
			RandomAccessFile raf = new RandomAccessFile (f, "r");
			FileChannel c = raf.getChannel();
			ByteBuffer b = c.map(MapMode.READ_ONLY, 0, c.size());
			raf.close ();
			l.add(b.order(ByteOrder.LITTLE_ENDIAN).asIntBuffer());
			i++;
		}
		level = l.toArray(new IntBuffer[l.size()]);
	}
	public BBox boundingBox () {
		IntBuffer a = level[level.length - 1];
		BBox b = new BBox ();
		for (int i = 0; i < a.limit(); i+= 4) {
			b = new BBox (b, a.get(i), a.get(i + 2));
			b = new BBox (b, a.get(i + 1), a.get(i + 3));
		}
		return b;
	}
	public void find (BBox bbox, List<Long> l) {
		l.clear();
		findRec (bbox, level.length, 0, l);
	}

	private void findRec (BBox bbox, int i, long j, List<Long> l) {
		if (i == 0)
			l.add (j);
		else {
			i--;
			j *= nodeSize / 16;
			IntBuffer a = level[i];
			for (int k = 0; k < nodeSize / 16; k++) {
				int p = 4 * ((int) j + k);
				if (p < a.limit()) {
/*
					BBox b = new BBox (a.get(p), a.get(p + 1), a.get(p + 2), a.get (p + 3));
					if (bbox.overlaps (b)) findRec(bbox, i, j + k, l);
 */
                    if (bbox.overlaps (a.get(p), a.get(p + 1), a.get(p + 2), a.get (p + 3))) findRec(bbox, i, j + k, l);
				}
			}
		}

	}

	public File leaves() {
		return leaves;
	}

	private int nodeSize;
	private IntBuffer [] level;
}
