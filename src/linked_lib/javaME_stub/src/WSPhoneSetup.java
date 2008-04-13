import java.io.*;
import javax.microedition.io.*;
import javax.microedition.lcdui.*;
import javax.microedition.midlet.*;
import javax.microedition.media.*;
import javax.microedition.media.control.*;

public class WSPhoneSetup
    extends MIDlet 
    implements CommandListener {

        public WSQuery theQuery;

	public static class Math {
		public static int fixedInnerProduct(int[] x, int[] y) {
		    int s = 0;
		    int l = x.length;
		    for (int i=0; i<l; i++) {
		      s += (x[i] * y[i]) >> 16;
		    }
			return s;
		}
	}
	
	public static class TCPStream extends OutputStream {
		private static final int BUFSIZE = 1024;
		private int count = 0;
		private byte samples[] = new byte[BUFSIZE];

	    //private int tcp_state = 0;
	        private IOException tcp_state;
		SocketConnection client = null; 
		private OutputStream os = null;
		  
		public TCPStream() {
		    tcp_state = null;//1;
		  try {
  		    client = 
			//(SocketConnection) Connector.open("socket://beacon.csail.mit.edu:9002");
		      (SocketConnection) Connector.open("socket://honor.csail.mit.edu:9002");
		      //client.setSocketOption(DELAY, 0);
		      //client.setSocketOption(KEEPALIVE, 0);
		      os = client.openOutputStream();
		      tcp_state = null;//= 2;
		    } catch (IOException e) {
		    	//tcp_state = 104;
			tcp_state = e;
		    }
			
		}
		
		public void flush() {
			if (os != null) {
				try {
					os.write(samples,0 /* offset in samples[] */, count);
					tcp_state = null;//7;
				} catch (IOException e) {
				    tcp_state = e;//105;
				}
			}
			count = 0;
		}
		
		public void close() {
		  flush();
	      try {
			  super.close();
			  tcp_state = null;//203;
	      } catch (IOException e) {
	    	  tcp_state = e;//103;
	      }
		  try {
  		    os.close();
  		    tcp_state = null;//201;
		  } catch (IOException e) {
		      tcp_state = e;//101;
		  }
		  try {
		    client.close();
		    tcp_state = null;//202;
	      } catch (IOException e) {
		  tcp_state = e;//102;
	      }
		}
		
		public void write(int x) {
			samples[count] = (byte) x;
  			count++;
			if (count == BUFSIZE) flush();
		}
		
		public int getCount() { return count; }
  	        //public int getTcpState() { return tcp_state; }
		public IOException getTcpState() { return tcp_state; }
	}
  private Form mMainForm;
  
  public WSPhoneSetup() {
    mMainForm = new Form("WSQuery Executing");
    //mMainForm.append(new StringItem(null, "<"+System.getProperty("supports.audio.capture")+">"));
    //mMainForm.append(new StringItem(null, "<"+System.getProperty("audio.encodings")+">"));
    mMainForm.addCommand(new Command("Exit", Command.EXIT, 0));
    mMainForm.setCommandListener(this);
  }
  

  private RecordControl rc;
  private Player player;
  private TCPStream networkStream;
  
  // This should feed data to the WS query... not to the networkStream;
  public void requestData() {

      try {
        player.prefetch(); 
        rc.setRecordStream(networkStream);
        mMainForm.append(new StringItem(null, "recording\n"));

        rc.startRecord();
        player.start();
        Thread.sleep(10*60*1000);
        mMainForm.append(new StringItem(null, "recording done 4"));

        rc.commit();
      } catch (Exception e) {
        mMainForm.append(new StringItem(null, "!5!"+e.toString()));
      }
  }

  public void startApp() {
    Display.getDisplay(this).setCurrent(mMainForm);

    mMainForm.append(new StringItem(null, "<Output of query directed to socket>\n"));

    byte samples[] = null;
    try {

// [2008.04.12] Disabling actual audio recording for now!!
      //  Player player = Manager.createPlayer("capture://audio?encoding=pcm&channels=1&rate=8000");
      player = Manager.createPlayer("capture://audio?encoding=amr");
      player.realize();
      rc = (RecordControl) player.getControl("RecordControl");
      //rc.setRecordSizeLimit(100000);

      networkStream = new TCPStream();
      
    } catch (Exception e) {
    	mMainForm.append(new StringItem(null, "!4!"+e.toString()));
    } try {


      // Check on that TCP connection:
      mMainForm.append(new StringItem(null, "tcps="+networkStream.getTcpState()+"."));
      mMainForm.append(new StringItem(null, rc.getContentType()));

      // RRN: Plug the output stream into the WS Query:
      theQuery = new WSQuery(networkStream);
      //theQuery.setOut(networkStream);      
      
      // Give the query access to the form for on-the-phone printing:
      theQuery.setForm(mMainForm);

      // RRN: Run the query.
      theQuery.main(0,null);

    } catch (Exception e) {
    	mMainForm.append(new StringItem(null, "!3!"+e.toString()));
    } try {

      /*
      mMainForm.append(new StringItem(null, "RRN: MANUALLY HACKING SOME OUTPUT\n"));
      networkStream.write("TEST\n".getBytes());
      networkStream.flush();
      networkStream.close();
      mMainForm.append(new StringItem(null, "RRN: FLUSHED OUTPUT\n"));
      player.stop();
      player.close();
      */

      //samples = networkStream.toByteArray();

      // Close down and report some stats:
      player.stop();      
      player.close();


      mMainForm.append(new StringItem(null, "count="+networkStream.getCount() +"\n"));
      mMainForm.append(new StringItem(null, "tcps="+networkStream.getTcpState()+".\n"));
      networkStream.close();
      mMainForm.append(new StringItem(null, "tcps="+networkStream.getTcpState()+".\n"));

    } catch (Exception e) {
    	mMainForm.append(new StringItem(null, "!2!"+e.toString()));
    }
    
    
/*
    try	{
      DatagramConnection dgc = (DatagramConnection)
      Connector.open("datagram://18.26.4.182:9001");
      try {
        byte[] payload = "TstX".getBytes();
        Datagram datagram = 
          dgc.newDatagram(payload, payload.length);
        for (int i=1;i<10; i++) dgc.send(datagram);
    	mMainForm.append(new StringItem(null, "<udp done>"));
      } finally {
          dgc.close();
      }
    } catch (IOException e) {
    	mMainForm.append(new StringItem(null, "!udp!"+e.toString()));
    }
    try {
      mMainForm.append(new StringItem(null, "<tcp c>"));
//      SocketConnection client = (SocketConnection) Connector.open("socket://18.26.4.182:9002");
      SocketConnection client = (SocketConnection) Connector.open("socket://beacon.csail.mit.edu:9002");
  	  mMainForm.append(new StringItem(null, "<tcp c done>"));
      //client.setSocketOption(DELAY, 0);
      //client.setSocketOption(KEEPALIVE, 0);
      //InputStream is = client.openInputStream();
      OutputStream os = client.openOutputStream();
  	  mMainForm.append(new StringItem(null, "<tcp w>"));
      os.write("some string".getBytes());
  	  mMainForm.append(new StringItem(null, "<tcp w done>"));
      os.close();
      client.close();
      mMainForm.append(new StringItem(null, "<tcp done>"));
    } catch (IOException e) {
    	mMainForm.append(new StringItem(null, "!tcp!"+e.toString()));
    }
 */   
    
    /*
    try {
  	  mMainForm.append(new StringItem(null, "<"+samples.length+">"));
    } catch (Exception e) {
    	mMainForm.append(new StringItem(null, "!5!"+e.toString()));
    	
    }
    */
    //catch (IOException e) {
    //} catch (MediaException e) {
    //} catch (InterruptedException e) {}
  }
  
  public void pauseApp() {}
  
  public void destroyApp(boolean unconditional) {}
  
  public void commandAction(Command c, Displayable s) {
    notifyDestroyed();
  }
}
