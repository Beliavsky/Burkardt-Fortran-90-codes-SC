#! /bin/bash
#
gfortran -c -Wall xyz_demo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran xyz_demo.o -L$HOME/lib -lplot3d_io
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm xyz_demo.o
#
mv a.out xyz_demo
./xyz_demo > xyz_demo.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
endif
rm xyz_demo
#
echo "Normal end of execution."
