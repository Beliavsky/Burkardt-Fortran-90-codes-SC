#! /bin/bash
#
gfortran -c -Wall point_merge_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran point_merge_test.o -L$HOME/lib -lpoint_merge
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm point_merge_test.o
#
mv a.out point_merge_test
./point_merge_test > point_merge_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm point_merge_test
#
echo "Normal end of execution."
