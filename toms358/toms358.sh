#! /bin/bash
#
gfortran -c -Wall toms358.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
ar qc libtoms358.a *.o *.mod
rm *.o
rm *.mod
#
mv libtoms358.a ~/lib
#
echo "Normal end of execution."
