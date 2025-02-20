#! /bin/bash
#
gfortran -c -Wall grid_dataset.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran grid_dataset.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm grid_dataset.o
#
chmod ugo+x a.out
mv a.out ~/bin/grid_dataset
#
echo "Normal end of execution."
