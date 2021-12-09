# cpm3-8080-serial

This project brings CPM3 to my 8080 IMSAI.

The build space and tools are designed to run on windows 10.

The h/w that is supported by this project are, the s100computers.com RAM+ROM board v2.0b, the s100computers.com Dual IDE board v3.0a, the IMSAI 8080 MPU-A, the Altair 88-2sio and the very cool IMSAI front panel.

The software presented here is a combined effort from John Monahan, Jay Jeager, and others, including Digital Research Inc. (rip).

There is a vast amount of documentation on the implementation and configuration of CP/M. Take your time and understand it before diving in.
Links to documents.  http://www.s100computers.com/Software%20Folder/CPM3%20BIOS%20Installation/CPM3%20HDISK%20BIOS%20Software.htm
And https://github.com/wwarthen/RomWBW/tree/master/Doc.
These are just a few links, there are many out there.


Once you get CPM3 up on your machine, you will find a lot of code that just does not belong there. I am planning to clean this out and add code that is appropriate to the 8080. Some of the .com files seem to be related to running under simh code.  Some this has been delt with.

Altmon was added to the project, its been modified to boot cpm3 directly. To run altmon goto start+3.
To boot CPM goto start.    i.e.  enter the address on the address switches, examine and press run.  
