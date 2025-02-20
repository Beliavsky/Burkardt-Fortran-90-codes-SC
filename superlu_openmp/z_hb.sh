#! /bin/bash
#
#  Compile
#
gfortran -c -Wall z_hb.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gcc -c -I/$HOME/include c_bridge_pzgssv.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
#  Link and load
#
gfortran -fopenmp z_hb.o c_bridge_pzgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_openmp -lm -lblas
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm z_hb.o
rm c_bridge_pzgssv.o
mv a.out z_hb
#
#  Run with 1 processor.
#
export OMP_NUM_THREADS=1
./z_hb < cg20_cua.txt > z_hb_1.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#  Run with 4 processors.
#
export OMP_NUM_THREADS=4
./z_hb < cg20_cua.txt > z_hb_4.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#  Terminate.
#
rm z_hb
#
echo "Normal end of execution."
