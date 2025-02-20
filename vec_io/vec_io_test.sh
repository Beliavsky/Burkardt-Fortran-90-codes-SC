#! /bin/bash
#
gfortran -c -Wall vec_io_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran vec_io_test.o -L$HOME/lib -lvec_io
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm vec_io_test.o
#
mv a.out vec_io_test
./vec_io_test > vec_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm vec_io_test
#
echo "Normal end of execution."
