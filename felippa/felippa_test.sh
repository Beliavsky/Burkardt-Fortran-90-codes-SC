#! /bin/bash
#
gfortran -c -Wall felippa_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran felippa_test.o -L$HOME/lib -lfelippa
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm felippa_test.o
#
mv a.out felippa_test
./felippa_test > felippa_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm felippa_test
#
echo "Normal end of execution."
