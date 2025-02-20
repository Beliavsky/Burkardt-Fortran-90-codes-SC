#! /bin/bash
#
g95 -c g95_quadmath.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
g95 g95_quadmath.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm g95_quadmath.o
#
mv a.out g95_quadmath
./g95_quadmath > g95_quadmath.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm g95_quadmath
#
echo "Normal end of execution."
