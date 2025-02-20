#! /bin/bash
#
gfortran -c -Wall table_orthonormalize.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_orthonormalize.o -llapack
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_orthonormalize.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_orthonormalize
#
echo "Normal end of execution."
