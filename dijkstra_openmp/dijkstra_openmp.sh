#! /bin/bash
#
gfortran -c -Wall -fopenmp dijkstra_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp dijkstra_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm dijkstra_openmp.o
mv a.out dijkstra_openmp
#
echo "Run with 1 thread."
export OMP_NUM_THREADS=1
./dijkstra_openmp > dijkstra_openmp.txt
#
echo "Run with 2 threads."
export OMP_NUM_THREADS=2
./dijkstra_openmp >> dijkstra_openmp.txt
#
echo "Run with 4 threads."
export OMP_NUM_THREADS=4
./dijkstra_openmp >> dijkstra_openmp.txt
#
rm dijkstra_openmp
#
echo "Normal end of execution."
