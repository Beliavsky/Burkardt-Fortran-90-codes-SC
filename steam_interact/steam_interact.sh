#! /bin/bash
#
gfortran -c -Wall steam_interact.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran steam_interact.o -L$HOME/lib -lsteam
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm steam_interact.o
#
chmod ugo+x a.out
mv a.out ~/bin/steam_interact
#
echo "Normal end of execution."
