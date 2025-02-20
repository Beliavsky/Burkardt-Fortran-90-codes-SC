#! /bin/bash
#
gfortran -c -Wall square_hex_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran square_hex_grid_test.o -L$HOME/lib -lsquare_hex_grid
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm square_hex_grid_test.o
#
mv a.out square_hex_grid_test
./square_hex_grid_test > square_hex_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm square_hex_grid_test
#
echo "Normal end of execution."
