f_file = main.f90  rand_generator.f90 dream.f90 linpack.f pdf_density.f90

mod_file = rand_generator.f90 pdf_density.f90 dream.f90 

all: $(f_file)
	gfortran -O3 -c $(mod_file)
	gfortran -O3 $(f_file)
	rm -f *.mod *.o

go:
	./a.out
