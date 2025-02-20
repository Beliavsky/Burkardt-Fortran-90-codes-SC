#! /bin/bash
#
gfortran -c -Wall geompack2_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran geompack2_test.o -L$HOME/lib -lgeompack2
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm geompack2_test.o
#
mv a.out geompack2_test
./geompack2_test > geompack2_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm geompack2_test
#
echo "Normal end of execution."
