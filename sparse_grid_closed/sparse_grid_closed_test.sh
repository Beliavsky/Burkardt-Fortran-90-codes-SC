#! /bin/bash
#
gfortran -c -Wall sparse_grid_closed_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sparse_grid_closed_test.o -L$HOME/lib -lsparse_grid_closed
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sparse_grid_closed_test.o
#
mv a.out sparse_grid_closed_test
./sparse_grid_closed_test > sparse_grid_closed_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sparse_grid_closed_test
#
echo "Normal end of execution."
