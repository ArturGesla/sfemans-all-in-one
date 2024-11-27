
CMP	 = gfortran -c
LNK	 = gfortran

OPT	 = -C
LIB_TIME =  

RACINE = ..

DEL_DIR =  $(RACINE)/Delaunay
DEL_OBJ = $(DEL_DIR)/linsys.o\
	  $(DEL_DIR)/grid_types.o\
	  $(DEL_DIR)/list.o\
	  $(DEL_DIR)/delaunay.o


SPL_DIR =  $(RACINE)/Geometry
SPL_OBJ = $(SPL_DIR)/aux.o \
	  $(SPL_DIR)/banded.o \
	  $(SPL_DIR)/curves.o

GRD_DIR =  $(RACINE)/Code
GRD_OBJ = $(GRD_DIR)/domain_2d.o\
	  $(GRD_DIR)/refine_2d.o\
	  $(GRD_DIR)/grid_2d.o\
	  $(GRD_DIR)/back_2d.o\
	  $(GRD_DIR)/arrays.o\
	  $(GRD_DIR)/convert.o\
	  $(GRD_DIR)/grid_2d.o

UTIL = $(RACINE)/Code/UTIL
PONT_OBJ = $(RACINE)/Code/UTIL/sub_edit.o\
	   $(RACINE)/Geometry/sub_geometry.o

PLOT_OBJ = sub_plot.o
#----------------------------------------

unif_grid.exe : chaine_caractere.o unif_grid.o
	$(LNK) $(OPT) -o unif_grid.exe unif_grid.o chaine_caractere.o

unif_grid.o :  unif_grid.f90
	$(CMP) $(OPT) $< -o $(*).o

chaine_caractere.o: $(RACINE)/../SFEMaNS/FEMSUB/chaine_caractere.f90
	$(CMP) $(OPT) $< -o $(*).o

inter_p1.exe : inter_p1.o sub_plot.o
	($(LNK) $(OPT) -o inter_p1.exe inter_p1.o sub_plot.o)

inter_p1.o: inter_p1.f
	(cp inter_p1.f inter_p1.f90; \
	$(CMP) $(OPT) -I$(GRD_DIR) inter_p1.f90;  rm inter_p1.f90)

mv_fem.exe: $(PLOT_OBJ) inter_mv_p1p2_fem.o
	($(LNK) $(OPT) -o mv_fem.exe inter_mv_p1p2_fem.o \
	$(GRD_OBJ) $(SPL_OBJ) $(DEL_OBJ) $(PLOT_OBJ))

inter_mv_p1p2_fem.o: inter_mv_p1p2_fem.f $(GRD_OBJ) $(SPL_OBJ) $(DEL_OBJ) $(PLOT_OBJ)
	(cp inter_mv_p1p2_fem.f inter_mv_p1p2_fem.f90; \
	$(CMP) $(OPT) -I$(GRD_DIR) -I$(SPL_DIR) -I$(DEL_DIR) inter_mv_p1p2_fem.f90; \
	rm inter_mv_p1p2_fem.f90)

mv_poreux_fem.exe: inter_mv_p1p2_poreux_fem.o  $(PLOT_OBJ)
	($(LNK) $(OPT) -o mv_fem_poreux.exe inter_mv_p1p2_poreux_fem.o \
	$(GRD_OBJ) $(SPL_OBJ) $(DEL_OBJ) $(PLOT_OBJ))

inter_mv_p1p2_poreux_fem.o: inter_mv_p1p2_poreux_fem.f
	(cp inter_mv_p1p2_poreux_fem.f inter_mv_p1p2_poreux_fem.f90; \
	$(CMP) $(OPT) -I$(GRD_DIR) -I$(SPL_DIR) -I$(DEL_DIR) inter_mv_p1p2_poreux_fem.f90; \
	rm inter_mv_p1p2_poreux_fem.f90)

mv_iso_fem.exe: inter_mv_p1p2_isoparam_fem.o  $(PLOT_OBJ)
	($(LNK) $(OPT) -o mv_iso_fem.exe inter_mv_p1p2_isoparam_fem.o \
	$(GRD_OBJ) $(SPL_OBJ) $(DEL_OBJ) $(PLOT_OBJ))

inter_mv_p1p2_isoparam_fem.o: inter_mv_p1p2_isoparam_fem.f
	(cp inter_mv_p1p2_isoparam_fem.f inter_mv_p1p2_isoparam_fem.f90; \
	$(CMP) $(OPT) -I$(GRD_DIR) -I$(SPL_DIR) -I$(DEL_DIR) inter_mv_p1p2_isoparam_fem.f90; \
	rm inter_mv_p1p2_isoparam_fem.f90)

subgrid.exe : inter_p1p2_subgrid.o $(PLOT_OBJ) $(PONT_OBJ)
	($(LNK) $(OPT) -o subgrid.exe inter_p1p2_subgrid.o \
	$(GRD_OBJ) $(SPL_OBJ) $(DEL_OBJ) $(PLOT_OBJ) $(PONT_OBJ))

inter_p1p2_subgrid.o : $(SPL_OBJ) $(GRD_OBJ) inter_p1p2_subgrid.f
	(cp inter_p1p2_subgrid.f inter_p1p2_subgrid.f90; cp $(GRD_DIR)/*.mod .; \
	$(CMP) $(OPT) inter_p1p2_subgrid.f90; \
	rm inter_p1p2_subgrid.f90)

und.exe : $(PLOT_OBJ) p1p2_1D.o
	($(LNK) $(OPT) -o und.exe p1p2_1D.o \
	 $(PLOT_OBJ))

p1p2_1D.o : p1p2_1D.f90
	$(CMP) $(OPT) -I$(GRD_DIR) p1p2_1D.f90

und_nounif.exe : $(PLOT_OBJ) p1p2_1D_nounif.o 
	($(LNK) $(OPT) -o und_nounif.exe p1p2_1D_nounif.o \
 	$(PLOT_OBJ))

p1p2_1D_nounif.o : p1p2_1D_nounif.f90
	$(CMP) $(OPT) -I$(GRD_DIR) p1p2_1D_nounif.f90

sub_plot.o: sub_plot.f
	(cp sub_plot.f sub_plot.f90; \
	$(CMP) $(OPT) sub_plot.f90; \
	rm sub_plot.f90)

#
#                    * * * * * * * * * *
#                    *   GRID OBJECTS  *
#                    * * * * * * * * * *
#


$(DEL_DIR)/linsys.o: $(DEL_DIR)/linsys.f90
	(cd $(DEL_DIR); $(CMP) $(OPT) linsys.f90)
$(DEL_DIR)/grid_types.o: $(DEL_DIR)/grid_types.f90
	(cd $(DEL_DIR); $(CMP) $(OPT) grid_types.f90)
$(DEL_DIR)/list.o: $(DEL_DIR)/list.f90 $(DEL_DIR)/grid_types.o
	(cd $(DEL_DIR); $(CMP) $(OPT) list.f90)
$(DEL_DIR)/delaunay.o: $(DEL_DIR)/delaunay.f90 $(DEL_DIR)/list.o $(DEL_DIR)/linsys.o
	(cd $(DEL_DIR); $(CMP) $(OPT) delaunay.f90)

$(SPL_DIR)/aux.o: $(SPL_DIR)/aux.f90
	(cd $(SPL_DIR); $(CMP) $(OPT) aux.f90)
$(SPL_DIR)/banded.o: $(SPL_DIR)/banded.f90
	(cd $(SPL_DIR); $(CMP) $(OPT) banded.f90)
$(SPL_DIR)/curves.o: $(SPL_DIR)/curves.f90 $(SPL_DIR)/banded.o
	(cd $(SPL_DIR); $(CMP) $(OPT) curves.f90)

#$(GRD_DIR)/domain_2d.o: $(GRD_DIR)/domain_2d.f90 $(SPL_DIR)/curves.o $(DEL_DIR)/delaunay.o
#	(cd $(GRD_DIR); $(CMP) -I$(SPL_DIR) -I$(DEL_DIR) $(OPT) domain_2d.f90)
$(GRD_DIR)/domain_2d.o: $(GRD_DIR)/domain_2d.f90 $(SPL_DIR)/curves.o $(DEL_DIR)/delaunay.o
	(cd $(GRD_DIR); cp $(SPL_DIR)/*.mod .; cp $(DEL_DIR)/*.mod .; \
	$(CMP) -I$(SPL_DIR) -I$(DEL_DIR) $(OPT) domain_2d.f90)
$(GRD_DIR)/refine_2d.o: $(GRD_DIR)/convert.o $(GRD_DIR)/refine_2d.f90 $(GRD_DIR)/domain_2d.o
	(cd $(GRD_DIR); $(CMP) $(OPT) -I$(SPL_DIR) -I$(DEL_DIR) -I$(GRD_DIR) refine_2d.f90)
$(GRD_DIR)/convert.o: $(GRD_DIR)/convert.f90 $(GRD_DIR)/domain_2d.o
	(cd $(GRD_DIR); $(CMP) $(OPT) -I$(SPL_DIR) -I$(DEL_DIR) convert.f90)
$(GRD_DIR)/back_2d.o: $(GRD_DIR)/back_2d.f90 $(GRD_DIR)/convert.o
	(cd $(GRD_DIR); $(CMP) $(OPT) -I$(SPL_DIR) -I$(DEL_DIR) back_2d.f90)
$(GRD_DIR)/arrays.o: $(GRD_DIR)/arrays.f90 $(GRD_DIR)/convert.o
	(cd $(GRD_DIR); $(CMP) $(OPT) -I$(SPL_DIR) -I$(DEL_DIR) arrays.f90)
$(GRD_DIR)/grid_2d.o: $(GRD_DIR)/grid_2d.f90 $(GRD_DIR)/refine_2d.o \
	$(GRD_DIR)/back_2d.o $(GRD_DIR)/arrays.o
	(cd $(GRD_DIR); $(CMP) $(OPT) -I$(SPL_DIR) -I$(DEL_DIR) grid_2d.f90)

$(GRD_DIR)/main.o: $(GRD_DIR)/main.f90 $(GRD_DIR)/grid_2d.o
	(cd $(GRD_DIR); $(CMP) $(OPT) -I$(SPL_DIR) -I$(DEL_DIR) $(GRD_DIR)/main.f90)

$(GRD_DIR)/pont_main.o: $(GRD_DIR)/pont_main.f90 $(GRD_DIR)/grid_2d.o $(PONT_OBJ)
	(cd $(GRD_DIR); cp $(UTIL)/*.mod .; cp $(SPL_DIR)/*.mod .;\
	$(CMP) $(OPT) -I$(SPL_DIR) -I$(DEL_DIR) -I$(UTIL) $(GRD_DIR)/pont_main.f90)

$(RACINE)/Code/UTIL/sub_edit.o: $(RACINE)/Code/UTIL/sub_edit.f90
	(cd $(RACINE)/Code/UTIL/; $(CMP) $(OPT) sub_edit.f90)

$(RACINE)/Geometry/sub_geometry.o: $(RACINE)/Geometry/sub_geometry.f90
	(cd $(RACINE)/Geometry; $(CMP) $(OPT) sub_geometry.f90)
