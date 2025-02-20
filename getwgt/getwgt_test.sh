#! /bin/bash
#
gfortran -c -Wall getwgt_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran getwgt_test.o -L$HOME/lib -lgetwgt
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm getwgt_test.o
#
mv a.out getwgt_test
./getwgt_test > getwgt_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm getwgt_test
#
echo "Normal end of execution."
