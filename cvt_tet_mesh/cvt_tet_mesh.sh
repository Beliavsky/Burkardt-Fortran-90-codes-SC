#! /bin/bash
#
gfortran -c -Wall cvt_tet_mesh.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran cvt_tet_mesh.o -L$HOME/lib -ltest_tet_mesh
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cvt_tet_mesh.o
#
mv a.out cvt_tet_mesh
./cvt_tet_mesh > cvt_tet_mesh.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cvt_tet_mesh
#
echo "Normal end of execution."
