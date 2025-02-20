#! /bin/bash
#
gfortran -c -Wall hex_grid_dataset.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hex_grid_dataset.o -L$HOME/lib -lsquare_hex_grid
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hex_grid_dataset.o
#
chmod ugo+x a.out
mv a.out ~/bin/hex_grid_dataset
#
echo "Normal end of execution."
