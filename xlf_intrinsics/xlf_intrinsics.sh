#! /bin/bash
#
XLF90 -c -g xlf_intrinsics.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
XLF90 xlf_intrinsics.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm xlf_intrinsics.o
#
mv a.out xlf_intrinsics
./xlf_intrinsics > xlf_intrinsics.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm xlf_intrinsics
#
echo "Normal end of execution."
