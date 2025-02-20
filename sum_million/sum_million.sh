#! /bin/bash
#
gfortran -c -Wall sum_million.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sum_million.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sum_million.o
#
chmod ugo+x a.out
mv a.out ~/bin/sum_million
#
echo "Normal end of execution."
