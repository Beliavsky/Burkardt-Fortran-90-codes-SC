#! /bin/bash
#
ar x $HOME/lib/libtoms358.a mconst.mod 
ar x $HOME/lib/libtoms358.a msub90.mod
#
gfortran -c -Wall toms358_test.f90
if [ $? -ne 0 ]; then
  echo "Compile errors."
  exit
fi
#
rm *.mod
#
gfortran toms358_test.o -L$HOME/lib -ltoms358
if [ $? -ne 0 ]; then
  echo "Load errors."
  exit
fi
rm toms358_test.o
#
mv a.out toms358_test
./toms358_test > toms358_test.txt
if [ $? -ne 0 ]; then
  echo "Run errors."
  exit
fi
rm toms358_test
#
echo "Normal end of execution."
