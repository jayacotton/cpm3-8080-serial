# cpm3-8080-serial

This project brings CPM3 to my 8080 IMSAI.  

The build space and tools are designed to run on windows 10.  I guess you can port it to
unix, I may do that at some point.

The h/w that is supported by this project are, the s100computers.com RAM+ROM board v2.0b, 
the s100computers.com Dual IDE board v3.0a, the IMSAI 8080 MPU-A, the Altair 88-2sio
and the very cool IMSAI front panel.

The software presented here is a combined effort from John Monahan, Jay Jeager, and 
others, includeing Digital Research Inc. (rip).  

There is a vast amount of documentation on the implementation and configureation 
of the software.  Take your time and understand it before diving in.

Once you get CPM3 up on your machine, you will find a lot of code that just does not
belong there.  I am planning to clean this out and add code that is appropreate to the 
8080.  Some of the .com files seem to be related to running under simh code.
