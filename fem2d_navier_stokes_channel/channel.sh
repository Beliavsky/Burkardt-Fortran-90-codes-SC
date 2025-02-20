#! /bin/bash
#
gfortran -c -Wall channel.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ~/lib/fem2d_navier_stokes.o channel.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem2d_navier_stokes.o + channel.o"
  exit
fi
rm channel.o
#
chmod ugo+x a.out
mv a.out channel
./channel nodes6.txt triangles6.txt > channel.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm channel
#
echo "Normal end of execution."
