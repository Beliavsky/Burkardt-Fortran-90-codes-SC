#! /bin/bash
#
#  Compile
#
gfortran -c -Wall s_hb.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gcc -c -I/$HOME/include c_bridge_psgssv.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
#  Link and load
#
gfortran -fopenmp s_hb.o c_bridge_psgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_openmp -lm -lblas
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm s_hb.o
rm c_bridge_psgssv.o
mv a.out s_hb
#
#  Run with 1 processor.
#
export OMP_NUM_THREADS=1
./s_hb < g20_rua.txt > s_hb_1.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#  Run with 4 processors.
#
export OMP_NUM_THREADS=4
./s_hb < g20_rua.txt > s_hb_4.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#  Terminate.
#
rm s_hb
#
echo "Normal end of execution."
