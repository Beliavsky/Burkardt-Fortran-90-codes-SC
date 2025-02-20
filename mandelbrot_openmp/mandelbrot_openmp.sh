#! /bin/bash
#
gfortran -c -Wall -fopenmp mandelbrot_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp mandelbrot_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mandelbrot_openmp.o
mv a.out mandelbrot_openmp
#
echo "Run with 1 thread."
export OMP_NUM_THREADS=1
./mandelbrot_openmp > mandelbrot_openmp.txt
#
echo "Run with 2 threads."
export OMP_NUM_THREADS=2
./mandelbrot_openmp >> mandelbrot_openmp.txt
#
echo "Run with 4 threads."
export OMP_NUM_THREADS=4
./mandelbrot_openmp >> mandelbrot_openmp.txt
#
rm mandelbrot_openmp
#
echo "Normal end of execution."
