#! /bin/bash
#
mpif90 -c -Wall blacs_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpif90 blacs_test.o -L /usr/lib/x86_64-linux-gnu -lscalapack-openmpi
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm blacs_test.o
#
mv a.out blacs_test
mpirun -v -np 4 ./blacs_test > blacs_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm blacs_test
#
echo "Normal end of execution."
