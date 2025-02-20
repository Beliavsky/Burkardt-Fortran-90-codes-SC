#! /bin/bash
#
gfortran -c -Wall mario.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o mario mario.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mario.o
#
./mario > mario.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm mario
#
#  Now run gnuplot.
#
gnuplot < mario_commands.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
