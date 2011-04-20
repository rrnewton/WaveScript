//package sivantoledo.util;

import java.io.*;
import java.net.*;

class StreamReceiver {
   public static void main(String argv[]) throws Exception {
         String clientSentence;
         String capitalizedSentence;
         //InetAddress ia = InetAddress.getByName("128.30.76.71"); // beacon
         InetAddress ia = InetAddress.getByName("honor.csail.mit.edu"); 
         System.out.println(ia.toString());
         ServerSocket s = new ServerSocket(9002,1024,ia);

         Socket connectionSocket = s.accept();
         System.out.println("connected");
         InputStream is = connectionSocket.getInputStream();
         OutputStream os = new FileOutputStream("N80.wav");
         int n;
         byte[] buffer = new byte[512];
         while ((n = is.read(buffer)) != -1) {
           System.out.println("received "+n+" bytes");
           os.write(buffer,0,n);
         }
         os.close();
         is.close();
         connectionSocket.close();
         System.out.println("done receiving");
      }
}
