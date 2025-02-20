#! /bin/bash
#
gfortran -c -Wall int_exactness_jacobi.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran int_exactness_jacobi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm int_exactness_jacobi.o
#
chmod ugo+x a.out
mv a.out ~/bin/int_exactness_jacobi
#
echo "Normal end of execution."
