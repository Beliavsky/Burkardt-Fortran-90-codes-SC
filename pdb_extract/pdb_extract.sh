#! /bin/bash
#
gfortran -c -Wall pdb_extract.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pdb_extract.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm pdb_extract.o
mv a.out ~/bin/pdb_extract
#
echo "Normal end of execution."
