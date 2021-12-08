all:	new_altmon

new_altmon: new_altmon.asm
	asm8080 new_altmon.asm -l
	unix2dos new_altmon.asm
	cp new_altmon.asm ../cpm3-nonbanked-serial/.
