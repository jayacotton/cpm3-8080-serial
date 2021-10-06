S-100 Computers CP/M 3.0 CF Image Source Files

---------------------------------------------------------------------
S100-Computers Master Z80 CPU card and S100-Computers Serial I/O card
---------------------------------------------------------------------


CP/M 3.0 source directory :- cpm3-nonbanked-serial

To create a image file for writing to CF media [Run] the batch file 'Build_CF_img.bat'

the batch file will:-

1) Delete any previous image file with a conflicting name
2) Copy the system files CPMLDR.COM and CPM3.SYS from the CP/M 3.0 source directory
3) Create a new file system on image file named 'cpm3-nonbanked-serial.img'
4) Add the CP/M system files and user area files to the image file.
5) Perform cleanup



The User Areas Are

0  -  CP/M 3 Base
1  -  S100-TOOLS
2  -  WS4-FILES
3  -  BASIC-FILES
4  -  GAMES

Note:- On these images CCP.COM has the [SYSTEM] attribute set
so that is is available from any user area on program exit.