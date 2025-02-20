#! /bin/bash
#
gfortran -c -Wall hex_grid_triangulate.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran hex_grid_triangulate.o -L$HOME/lib -lsquare_hex_grid -ltest_triangulation
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hex_grid_triangulate.o
#
mv a.out hex_grid_triangulate
./hex_grid_triangulate > hex_grid_triangulate.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hex_grid_triangulate
#
echo "Normal end of execution."
