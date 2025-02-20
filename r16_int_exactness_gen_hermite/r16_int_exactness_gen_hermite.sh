#! /bin/bash
#
gfortran -c -Wall r16_int_exactness_gen_hermite.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran r16_int_exactness_gen_hermite.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r16_int_exactness_gen_hermite.o
#
chmod ugo+x a.out
mv a.out ~/bin/r16_int_exactness_gen_hermite
#
echo "Normal end of execution."
