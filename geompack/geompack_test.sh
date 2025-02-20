#! /bin/bash
#
gfortran -c -Wall geompack_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran geompack_test.o -L$HOME/lib -lgeompack
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm geompack_test.o
#
mv a.out geompack_test
./geompack_test > geompack_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm geompack_test
#
echo "Normal end of execution."
