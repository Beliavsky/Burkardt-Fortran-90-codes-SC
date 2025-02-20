#! /bin/bash
#
gfortran -c -Wall sparse_grid_closed_dataset.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sparse_grid_closed_dataset.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sparse_grid_closed_dataset.o
#
chmod ugo+x a.out
mv a.out ~/bin/sparse_grid_closed_dataset
#
echo "Normal end of execution."
