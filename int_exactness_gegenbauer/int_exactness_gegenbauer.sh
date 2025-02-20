#! /bin/bash
#
gfortran -c -Wall int_exactness_gegenbauer.f90
if [ $? -ne 0 ]; then
    echo "Compile error."
  exit
fi
#
gfortran int_exactness_gegenbauer.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm int_exactness_gegenbauer.o
#
chmod ugo+x a.out
mv a.out ~/bin/int_exactness_gegenbauer
#
echo "Normal end of execution."
