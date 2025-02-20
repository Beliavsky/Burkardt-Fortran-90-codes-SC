#! /bin/bash
#
#  Compile
#
gfortran -c -Wall d_hb.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gcc -c -I/$HOME/include c_bridge_pdgssv.c
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
#  Link and load
#
gfortran -fopenmp d_hb.o c_bridge_pdgssv.o -L$HOME/lib \
  -L/$HOME/libc -lsuperlu_openmp -lm -lblas
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm d_hb.o
rm c_bridge_pdgssv.o
mv a.out d_hb
#
#  Run with 1 processor.
#
export OMP_NUM_THREADS=1
./d_hb < g20_rua.txt > d_hb_1.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#  Run with 4 processors.
#
export OMP_NUM_THREADS=4
./d_hb < g20_rua.txt > d_hb_4.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
#  Terminate.
#
rm d_hb
#
echo "Normal end of execution."
