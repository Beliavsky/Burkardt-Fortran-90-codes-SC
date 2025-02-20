#! /bin/bash
#
gfortran -c -Wall -fopenmp heated_plate_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp heated_plate_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm heated_plate_openmp.o
mv a.out heated_plate_openmp
#
echo "Run with 1 thread."
export OMP_NUM_THREADS=1
./heated_plate_openmp > heated_plate_openmp.txt
#
echo "Run with 2 threads."
export OMP_NUM_THREADS=2
./heated_plate_openmp >> heated_plate_openmp.txt
#
echo "Run with 4 threads."
export OMP_NUM_THREADS=4
./heated_plate_openmp >> heated_plate_openmp.txt
#
echo "Run with 8 threads."
export OMP_NUM_THREADS=8
./heated_plate_openmp >> heated_plate_openmp.txt
#
rm heated_plate_openmp
#
echo "Normal end of execution."
