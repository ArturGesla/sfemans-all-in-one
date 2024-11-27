include ../make.inc

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
	  $(GRD_DIR)/back_2d.o\
	  $(GRD_DIR)/arrays.o\
	  $(GRD_DIR)/convert.o\
	  $(GRD_DIR)/grid_2d.o

UTIL = $(RACINE)/Code/UTIL
PONT_OBJ = $(RACINE)/Code/UTIL/sub_edit.o\
	   $(RACINE)/Geometry/sub_geometry.o

clean:
	(cd $(DEL_DIR); rm -f *.o *.mod; cd $(SPL_DIR); rm -f *.o *.mod;\
	 cd $(GRD_DIR); rm -f *.o *.mod;  cd $(UTIL); rm -f *.o *.mod)

b.exe: $(SPL_OBJ) $(DEL_OBJ) $(GRD_OBJ) $(GRD_DIR)/main.o
	($(LNK) $(OPT) -o b.exe $(GRD_OBJ) $(SPL_OBJ) $(DEL_OBJ) $(GRD_DIR)/main.o)


maill.exe: $(SPL_OBJ) $(DEL_OBJ) $(GRD_OBJ) $(PONT_OBJ) $(GRD_DIR)/pont_main.o
	($(LNK) $(OPT) -o maill.exe $(GRD_OBJ) $(SPL_OBJ) $(DEL_OBJ) $(PONT_OBJ) $(GRD_DIR)/pont_main.o)


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
