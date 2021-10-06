rm -f foo.dsk
mkfs.cpm -f s100ide -b dummy.file -b CPMLDR.COM foo.dsk
cpmcp -f s100ide foo.dsk CPM3.SYS 0:
cpmcp -f s100ide foo.dsk ../CPM3-BASE/*.* 0:
#cpmcp -f s100ide foo.dsk CCP.COM 0:
#cpmcp -f s100ide foo.dsk *.COM 0:
#cpmcp -f s100ide foo.dsk *.SPR 0:
#cpmcp -f s100ide foo.dsk *.DOC 0:
#cpmcp -f s100ide foo.dsk *.VDM 0:
#cpmcp -f s100ide foo.dsk *.INI 0:
#cpmcp -f s100ide foo.dsk *.SET 0:
#cpmcp -f s100ide foo.dsk *.ART 0:
#cpmcp -f s100ide foo.dsk *.HLP 0:
