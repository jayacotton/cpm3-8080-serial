This is the modified ALTMON code that I use on my IMSAI to boot CPM3 from the s100computers.com Dual IDE board.

The Makefile will assemble the code into a hex file and also modify and copy the source file to the 
CPM3 build directory.  There you can use the altairz80 command to run the simulator and select the
i: drive.  Use r prom.sub and then submit prom.  This will assemble the eprom code and put a copy
of the hex file on the windows system.

From there you can burn a boot rom from the file and its off to the races (or thud).
