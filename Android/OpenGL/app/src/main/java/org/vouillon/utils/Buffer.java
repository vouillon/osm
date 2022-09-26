package org.vouillon.utils;
public class Buffer {
    public Buffer(int sz) {
        buffer = new byte[sz];
    }
    protected final void setPosition(int p) {
        bufPos = p;
    }
    protected byte[] buffer;
    protected int bufPos = 0;
    protected final int readByte() {
        return ((int) buffer[bufPos++]) & 0xff;
    }
    protected final int readByte(int p) {
        return ((int) buffer[p]) & 0xff;
    }
    protected final int readInt2() {
        int i1 = readByte();
        int i2 = readByte();
        return i1 + (i2 << 8);
    }
    protected final int readInt2(int p) {
        int i1 = readByte(p);
        int i2 = readByte(p + 1);
        return i1 + (i2 << 8);
    }
    public final long readVarint() {
        byte b = buffer[bufPos++];
        if (b >= 0) return b;
        long v = b & 0x7f;
        b = buffer[bufPos++];
        if (b >= 0) return v | (b << 7);
        v |= (b & 0x7f) << 7;
        b = buffer[bufPos++];
        if (b >= 0) return v | (b << 14);
        v |= (b & 0x7f) << 14;
        int offs = 21;
        while (true) {
            b = buffer[bufPos++];
            if (b >= 0) return v | (b << offs);
            v |= (b & 0x7f) << offs;
            offs += 7;
        }
    }
    public final long readSignedVarint() {
        return sintOfInt (readVarint ());
    }

	private static long sintOfInt (long i) {
        return (i >>> 1) ^ -(i & 1);
        /*
		long j = i >>> 1;
		if ((i & 1) == 1) return (- j - 1);
		return j;
		*/
	}
}
