#! /bin/bash
#
gfortran -c -Wall table_unborder.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_unborder.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_unborder.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_unborder
#
echo "Normal end of execution."
