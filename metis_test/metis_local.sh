#! /bin/bash
#
gfortran -c -Wall metis_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran metis_test.o -L /usr/local/lib -lmetis
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm metis_test.o
#
mv a.out metis_test
./metis_test > metis_local.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm metis_test
#
echo "Normal end of execution."
