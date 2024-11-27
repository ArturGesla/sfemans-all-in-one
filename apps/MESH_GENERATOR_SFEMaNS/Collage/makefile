include ../make.inc

colle_p1p2.exe : sub_plot.o colle_p1p2.o 
	($(LNK) $(OPT) -o colle_p1p2.exe colle_p1p2.o sub_plot.o)

colle_p1p2.o: colle_p1p2.f90
	($(CMP) $(OPT) colle_p1p2.f90)

symmetrize.exe : sub_plot.o symmetrize.o 
	($(LNK) $(OPT) -o symmetrize.exe symmetrize.o sub_plot.o)

symmetrize.o: symmetrize.f90
	($(CMP) $(OPT) symmetrize.f90)

sub_plot.o: sub_plot.f90 
	($(CMP) $(OPT) sub_plot.f90)

clean:  
	(rm -f *.o *.mod)


colle_p1.exe : sub_plot.o colle_p1.o
	($(LNK) $(OPT) -o colle_p1.exe colle_p1.o sub_plot.o)

colle_p1.o: colle_p1.f90
	$(CMP) $(OPT) colle_p1.f90

rescale.exe : sub_plot.o rescale.o
	($(LNK) $(OPT) -o rescale.exe rescale.o sub_plot.o)

rescale.o : rescale.f90
	($(CMP) $(OPT) rescale.f90)

rescale_VKS.exe : sub_plot.o rescale_VKS.o
	($(LNK) $(OPT) -o rescale_VKS.exe rescale_VKS.o sub_plot.o)

rescale_VKS.o : rescale_VKS.f90
	($(CMP) $(OPT) rescale_VKS.f90)
