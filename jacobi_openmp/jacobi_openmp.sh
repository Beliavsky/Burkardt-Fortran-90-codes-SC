#! /bin/bash
#
gfortran -c -Wall -fopenmp jacobi_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp jacobi_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm jacobi_openmp.o
mv a.out jacobi_openmp
#
echo "Run with 1 thread."
export OMP_NUM_THREADS=1
./jacobi_openmp >> jacobi_openmp.txt
#
echo "Run with 2 threads."
export OMP_NUM_THREADS=2
./jacobi_openmp >> jacobi_openmp.txt
#
echo "Run with 4 threads."
export OMP_NUM_THREADS=4
./jacobi_openmp >> jacobi_openmp.txt
#
echo "Run with 8 threads."
export OMP_NUM_THREADS=8
./jacobi_openmp >> jacobi_openmp.txt
#
rm jacobi_openmp
#
echo "Normal end of execution."
