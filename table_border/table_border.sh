#! /bin/bash
#
gfortran -c -Wall table_border.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_border.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_border.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_border
#
echo "Normal end of execution."
