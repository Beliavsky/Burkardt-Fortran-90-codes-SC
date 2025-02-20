#! /bin/bash
#
gfortran -c -Wall inout.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ~/lib/fem2d_navier_stokes.o inout.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm inout.o
#
chmod ugo+x a.out
mv a.out inout
./inout nodes6.txt triangles6.txt > inout.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm inout
#
echo "Normal end of execution."
