#! /bin/bash
#
gfortran -c -Wall st_to_dsp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran st_to_dsp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm st_to_dsp.o
#
chmod ugo+x a.out
mv a.out ~/bin/st_to_dsp
#
echo "Normal end of execution."
