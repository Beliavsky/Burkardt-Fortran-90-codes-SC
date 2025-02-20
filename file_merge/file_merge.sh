#! /bin/bash
#
gfortran -c -Wall file_merge.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran file_merge.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm file_merge.o
#
chmod ugo+x a.out
mv a.out ~/bin/file_merge
#
echo "Normal end of execution."
