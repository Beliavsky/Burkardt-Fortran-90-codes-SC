#! /bin/bash
#
gfortran -c -Wall pblas_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pblas_test.o -L/$HOME/lib -lpblas -lblacs_f -lmpi
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm *.o
mv a.out pblas_test
#
pblas_test > pblas_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pblas_test
#
echo "Normal end of execution."

