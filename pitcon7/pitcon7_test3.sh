#! /bin/bash
#
gfortran -c -Wall pitcon7_test3.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pitcon7_test3.o -L$HOME/lib -lpitcon7
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pitcon7_test3.o
#
mv a.out pitcon7_test3
./pitcon7_test3 > pitcon7_test3.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pitcon7_test3
#
echo "Normal end of execution."
