#! /bin/bash
#
mpifort -c -Wall communicator_mpi.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mpifort communicator_mpi.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm communicator_mpi.o
mv a.out communicator_mpi
#
mpirun -np 4 ./communicator_mpi > communicator_mpi.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm communicator
#
echo "Normal end of execution."
