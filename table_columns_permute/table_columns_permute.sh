#! /bin/bash
#
gfortran -c -Wall table_columns_permute.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_columns_permute.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_columns_permute.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_columns_permute
#
echo "Normal end of execution."
