#! /bin/bash
#
#PBS -l walltime=00:05:00
#PBS -l nodes=1:ppn=1
#PBS -W group_list=newriver
#PBS -q open_q
#PBS -j oe
#
cd $PBS_O_WORKDIR
#
module purge
module load gcc/5.2.0
module load metis/5.1.0
#
gfortran -c -Wall metis_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran metis_test.o -L$METIS_LIB -lmetis
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm metis_test.o
#
mv a.out metis_test
./metis_test > metis_newriver.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm metis_test
#
echo "Normal end of execution."
