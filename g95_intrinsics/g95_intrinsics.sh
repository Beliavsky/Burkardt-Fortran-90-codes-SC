#! /bin/bash
#
g95 -c g95_intrinsics.f90
if [ $?-ne 0 ]; then
  echo "Compile error."
  exit
fi
#
g95 g95_intrinsics.o
if [ $?-ne 0 ]; then
  echo "Load error."
  exit
fi
rm g95_intrinsics.o
#
mv a.out g95_intrinsics
./g95_intrinsics > g95_intrinsics.txt
if [ $?-ne 0 ]; then
  echo "Run error."
  exit
fi
rm g95_intrinsics
#
echo "Normal end of execution."
