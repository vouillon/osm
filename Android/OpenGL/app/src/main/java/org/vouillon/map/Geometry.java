package org.vouillon.map;
import org.vouillon.database.RTree;


public class Geometry {

	private static final double pi_4 = Math.PI / 4;
	private static final double coeff_1 = 180. / Math.PI * 10_000_000.;
	private static final double coeff_2 = 1. / (2 * coeff_1);

	static double yToLat (double y) {
		return (Math.atan (Math.exp (y / coeff_1)) - pi_4) / coeff_2;
	}
	public static double latToY (double lat) {
		return coeff_1 * Math.log (Math.tan (pi_4 + lat * coeff_2));
	}

	private static int toLat (double ratio, double y) {
 		return (int) (yToLat(y * 10_000_000.) / ratio);
	}

	private static int toLon (double ratio, double x) {
 		return (int) (x * 10_000_000. / ratio);
	}

	static RTree.BBox boundingBox
				(double ratio, double xMin, double yMin, double xMax, double yMax) {
		return new RTree.BBox (toLat(ratio, yMin), toLat(ratio, yMax),
					           toLon(ratio, xMin), toLon(ratio, xMax));
	}
	
	static double polygonArea (double[] coord) {
		int l = coord.length / 2 - 1;
		double a = 0.;
		double x0 = coord[0], y0 = coord[1];
		for (int i = 0; i < l; i++) {
			double dx1 = coord[2 * i] - x0;
			double dy1 = coord[2 * i + 1] - y0;
			double dx2 = coord[2 * i + 2] - x0;
			double dy2 = coord[2 * i + 3] - y0;
			a += dx1 * dy2 - dx2 * dy1;
		}
		return a / 2;
	}
}
