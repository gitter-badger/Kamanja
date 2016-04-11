http://www.mkyong.com/java/java-find-location-using-ip-address/
http://dev.maxmind.com/
http://maxmind.github.io/GeoIP2-java/doc/v2.6.0/

import java.net.InetAddress;
import java.net.UnknownHostException;

abstract class Util {
    public static byte byteOfInt(int value, int which) {
        int shift = which * 8;
        return (byte)(value >> shift);
    }
    public static InetAddress intToInet(int value) {
        byte[] bytes = new byte[4];
        for(int i = 0; i<4; i++) {
            bytes[i] = byteOfInt(value, i);
        }
        try {
            return InetAddress.getByAddress(bytes);
        } catch (UnknownHostException e) {
            // This only happens if the byte array has a bad length
            return null;
        }
    }

}
