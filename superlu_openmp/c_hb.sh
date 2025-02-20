#! /bin/bash
#
#  Compile
#
gfortran -c -Wall c_hb.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gcc -c -I/$HOME/include c_bridge_pcgssv.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
#  Link and load
#
gfortran -fopenmp c_hb.o c_bridge_pcgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_openmp -lm -lblas
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm c_hb.o
rm c_bridge_pcgssv.o
mv a.out c_hb
#
#  Run with 1 processor.
#
export OMP_NUM_THREADS=1
./c_hb < cg20_cua.txt > c_hb_1.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#  Run with 4 processors.
#
export OMP_NUM_THREADS=4
./c_hb < cg20_cua.txt > c_hb_4.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#  Terminate.
#
rm c_hb
#
echo "Normal end of execution."
