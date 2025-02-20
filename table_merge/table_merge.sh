#! /bin/bash
#
gfortran -c -Wall table_merge.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_merge.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_merge.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_merge
#
echo "Normal end of execution."
