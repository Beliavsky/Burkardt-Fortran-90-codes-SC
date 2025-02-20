#! /bin/bash
#
gfortran -c -Wall table_record_match.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_record_match.o -L$HOME/lib
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm table_record_match.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_record_match
#
echo "Normal end of execution."
