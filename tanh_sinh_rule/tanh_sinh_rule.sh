#! /bin/bash
#
gfortran -c -g tanh_sinh_rule.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tanh_sinh_rule.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tanh_sinh_rule.o
#
chmod ugo+x a.out
mv a.out ~/bin/tanh_sinh_rule
#
echo "Normal end of execution."
