#! /bin/bash
#
gfortran -c -Wall -fopenmp mxv_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp mxv_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mxv_openmp.o
mv a.out mxv_openmp
#
echo "Run with 1 thread."
export OMP_NUM_THREADS=1
./mxv_openmp > mxv_openmp.txt
#
echo "Run with 2 threads."
export OMP_NUM_THREADS=2
./mxv_openmp >> mxv_openmp.txt
#
echo "Run with 4 threads."
export OMP_NUM_THREADS=4
./mxv_openmp >> mxv_openmp.txt
#
echo "Run with 8 threads."
export OMP_NUM_THREADS=8
./mxv_openmp >> mxv_openmp.txt
#
rm mxv_openmp
#
echo "Normal end of execution."
