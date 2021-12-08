Here you can run the altairz80 simh command and:
  simh> do cpm3
  < lots of stuff >
  i:
  r prom.sub        // get the eprom builder code...  Should not need to do this more that 1 time.
  submit prom
  < logs of stuff >
  
To generate a CP/M disk you need to say:  (in simh)

  i:
  submit hbuildcpm
  
  
Pro TIP:  the exit from simh type a ^E (control E), that gets you back to simh and then say exit.
  
