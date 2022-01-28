FC=gfortran #fortran compiler
FFLAGS=-O3 -Wall -Wextra -std=f2008 #optimization (for level 3) flags, compiler warnings and the strictest adherence to the latest standards
SRC=determineMohoLayer.f90

determineMohoLayer:
	$(FC) $(FFLAGS) $(SRC) -o $@ 


clean: #cleans all the old compilation files
	@rm -f determineMohoLayer