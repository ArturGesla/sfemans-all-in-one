all:
	(cd Code; make maill.exe; cd ../Collage; make colle_p1p2.exe; \
	make symmetrize.exe; make rescale.exe; cd ..)
clean:
	(cd Code; make clean; cd ../Collage; make clean; \
	cd ..; find . \( -name 'knots.*' -o -name 'geometry.*' \
	-o -name 'gridplot.*' -o -name '*.plt' -o -name '*.FEM' \
	-o -name 'FEM.*' -o -name 'fort.*' \) -exec echo Deleting :  {} \; \
	-exec rm -f  {} \;)
