#! /bin/bash
#
gfortran -c -Wall table_uniform_noise.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_uniform_noise.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_uniform_noise.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_uniform_noise
#
echo "Normal end of execution."
