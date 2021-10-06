Rem *** Delete previous image ***

del cpm3-nonbanked-serial.img



Rem *** Copy system files from build folder ***

copy cpm3-nonbanked-serial\CPMLDR.COM CPMLDR.COM
copy cpm3-nonbanked-serial\CPM3.SYS CPM3.SYS



Rem *** Make new file system image ***

mkfs.cpm -f s100ide -b LBA0.COM -b CPMLDR.COM -t cpm3-nonbanked-serial.img



Rem *** Copy system files ***

cpmcp -f s100ide cpm3-nonbanked-serial.img cpm3.sys 0:
cpmcp -f s100ide cpm3-nonbanked-serial.img ccp.com 0:
cpmchattr -f s100ide cpm3-nonbanked-serial.img s ccp.com 0:



Rem *** Copy user files ***

cpmcp -f s100ide cpm3-nonbanked-serial.img ./CPM3-BASE/*.* 0:
cpmcp -f s100ide cpm3-nonbanked-serial.img ./S100-TOOLS/*.* 1:
cpmcp -f s100ide cpm3-nonbanked-serial.img ./WS4-FILES/*.* 2:
cpmcp -f s100ide cpm3-nonbanked-serial.img ./BASIC-FILES/*.* 3:
cpmcp -f s100ide cpm3-nonbanked-serial.img ./GAMES/*.* 4:

Rem pause


Rem *** Clean up ***

del CPMLDR.COM
del CPM3.SYS
