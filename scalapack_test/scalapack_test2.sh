#! /bin/bash
#
mpif90 -c -Wall scalapack_test2.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpif90 scalapack_test2.o -L /usr/lib/x86_64-linux-gnu -lscalapack-openmpi
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm scalapack_test2.o
#
#  Number of processes must be at least 6 for this test.
#
mv a.out scalapack_test2
mpirun -v -np 6 ./scalapack_test2 > scalapack_test2.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm scalapack_test2
#
echo "Normal end of execution."
