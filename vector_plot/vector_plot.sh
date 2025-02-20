#! /bin/bash
#
gfortran -c -Wall vector_plot.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran vector_plot.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm vector_plot.o
mv a.out ~/bin/vector_plot
#
echo "Normal end of execution."
