#! /bin/bash
#
gfortran -c -Wall q_demo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran q_demo.o -L$HOME/lib -lplot3d_io
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm q_demo.o
#
mv a.out q_demo
./q_demo > q_demo.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm q_demo
#
echo "Normal end of execution."
