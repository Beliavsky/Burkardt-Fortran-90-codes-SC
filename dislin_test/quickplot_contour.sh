#! /bin/bash
#
#  This script is designed to compile and run F90 DISLIN examples on my LINUX system.
#
export DISLIN=/usr/local/dislin
export LD_LIBRARY_PATH=$DISLIN:$LD_LIBRARY_PATH
#
gfortran -c -I$DISLIN/gf/real64 -Wall quickplot_contour.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
$DISLIN/bin/gf95link -r8 quickplot_contour
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quickplot_contour.o
#
./quickplot_contour > quickplot_contour.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quickplot_contour
#
echo "Normal end of execution."
