#! /bin/bash
#
gfortran -c -Wall sparsepak_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran sparsepak_test.o -L$HOME/lib -lsparsepak
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm sparsepak_test.o
#
mv a.out sparsepak_test
./sparsepak_test > sparsepak_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm sparsepak_test
#
echo "Normal end of execution."
