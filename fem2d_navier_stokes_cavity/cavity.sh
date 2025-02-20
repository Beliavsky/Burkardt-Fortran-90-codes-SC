#! /bin/bash
#
gfortran -c -Wall cavity.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran ~/lib/fem2d_navier_stokes.o cavity.o
if [ $? -ne 0 ]; then
  echo "Errors linking and loading fem2d_navier_stokes.o + cavity.o"
  exit
fi
rm cavity.o
#
chmod ugo+x a.out
mv a.out cavity
./cavity nodes6.txt triangles6.txt > cavity.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cavity
#
echo "Normal end of execution."
