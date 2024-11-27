SHELL = /bin/csh
COMPIL   = /usr/local/eleves/f95  -byte_kinds
CMP      = $(COMPIL) -c
LNK      = $(COMPIL)


geometry.exe: curves.o banded.o 
	($(LNK) -O -o geometry.exe curves.o banded.o geometry.f90)

banded.o: banded.f90
	($(CMP) -O banded.f90)

curves.o: curves.f90 banded.o 
	($(CMP) -O curves.f90)

