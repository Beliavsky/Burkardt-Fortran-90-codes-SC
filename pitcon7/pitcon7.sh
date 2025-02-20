#! /bin/bash
#
mkdir temp
cd temp
rm -f *
~/bin/f90split ../pitcon7.f90
#
for FILE in `ls -1 *.f90`;
do
  gfortran -c -Wall $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f90
#
ar qc libpitcon7.a *.o
rm *.o
#
mv libpitcon7.a ~/lib
cd ..
rmdir temp
#
echo "Normal end of execution."
