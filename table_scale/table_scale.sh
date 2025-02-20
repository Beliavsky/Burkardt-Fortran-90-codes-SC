#! /bin/bash
#
gfortran -c -Wall table_scale.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_scale.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_scale.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_scale
#
echo "Normal end of execution."
