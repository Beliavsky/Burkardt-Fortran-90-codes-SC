#! /bin/bash
#
mkdir temp
cd temp
rm -f *
~/bin/f90split ../r16lib.f90
#
for FILE in `ls -1 *.f90`;
do
  g95 -c -Wall $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
rm *.f90
#
ar qc libr16lib.a *.o
rm *.o
#
mv libr16lib.a ~/lib
cd ..
rmdir temp
#
echo "Normal end of execution."
