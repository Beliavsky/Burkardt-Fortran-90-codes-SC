#! /bin/bash
#
gfortran -c -Wall pdb_read_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran pdb_read_test.o -L$HOME/lib -lpdb_read
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pdb_read_test.o
#
mv a.out pdb_read_test
./pdb_read_test < pdb_read_test_input.txt > pdb_read_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pdb_read_test
#
echo "Normal end of execution."
