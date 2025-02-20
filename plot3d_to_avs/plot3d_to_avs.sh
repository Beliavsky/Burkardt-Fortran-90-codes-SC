#! /bin/bash
#
gfortran -c -Wall plot3d_to_avs.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran plot3d_to_avs.o -L$HOME/lib -lplot3d_io
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm plot3d_to_avs.o
#
mv a.out ~/bin/plot3d_to_avs
#
echo "Normal end of execution."
