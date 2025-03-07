#! /bin/bash
#
gfortran -c -Wall table_histogram.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_histogram.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_histogram.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_histogram
#
echo "Normal end of execution."
