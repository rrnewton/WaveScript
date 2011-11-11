
This directory contains longer running programs (as compared to
../tests/) that benchmark different aspects of the system.


Some notes about performance:
--------------------------------------------------------------------------------

[2011.11.10] {Round of testing}

Testing on one million tuples on a 3.1 GHz Westmere:

                     Time     Throughput/sec
  wc                0.894s
  1_TAQ_reader      1.192s    844,688

Slightly slower than wc.  Testing with two processes on the same
machine:

  2_TAQ_over_socket  3.75s    265,806

The throughput was over a one-second sample towards the end (not total
average throughput).  

    <socket.ws> inbound: Extended table of inbound socket-connecting helper threads with port=5002, val=0x199d530
    Starting timer to count outputs, frequency = 1 Hz
      <socket.ws> inbound: Spawned thread to connect to host localhost port 5002
      <socket.ws> inbound:   Helper thread starting connection loop.  (host localhost port 5002)
      <socket.ws> inbound: Established client connection, port 5002
      <socket.ws> inbound: Socket file descriptor set to non-blocking: 3 (flags 4002)
      <socket.ws> inbound: Client made connection (fd 3) on port 5002.  Helper thread finished.
      Counted 255510 tuples in interval 1 average 255510 total 255510
      Counted 270565 tuples in interval 2 average 263037 total 526075
      Counted 271343 tuples in interval 3 average 265806 total 797418
      <socket.ws> rd header: Read wrong length, taking this as a signal that upstream is closed.  Exiting.
      <socket.ws> Closing all sockets before exiting.
      <socket.ws>   Closed descriptor 3, port 5002

    real	0m3.755s

And it takes 21 seconds for a gigabyte file (500M read a second, less
than that transfered through the socket).

During that gigabyte transfer BOTH sender and receiver are pegged at
100% CPU... maybe it is somehow compute bound.


[2011.11.11] {Testing updated socket library}

Right now it gets a similar throughput (250K tuples/sec), but it fails
to terminate...

It's maybe a smidge higher throughput (avg 270K) when I enable doing
both the length and the payload in one firing of the inbound socket
"iterate".


