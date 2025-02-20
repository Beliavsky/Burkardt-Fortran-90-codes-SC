#! /bin/bash
#
gfortran -c -Wall paranoia.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran paranoia.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm paranoia.o
#
mv a.out ~/bin/paranoia
#
echo "Normal end of execution."
