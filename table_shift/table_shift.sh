#! /bin/bash
#
gfortran -c -Wall table_shift.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_shift.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_shift.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_shift
#
echo "Normal end of execution."
