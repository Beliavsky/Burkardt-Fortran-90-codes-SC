#! /bin/bash
#
gfortran -c -Wall md3glue.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran md3glue.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm md3glue.o
#
mv a.out ~/bin/md3glue
#
echo "Normal end of execution."
