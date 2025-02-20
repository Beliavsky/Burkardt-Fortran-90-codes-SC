#! /bin/bash
#
gfortran -c -Wall table_barplot_ppma.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran table_barplot_ppma.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm table_barplot_ppma.o
#
chmod ugo+x a.out
mv a.out ~/bin/table_barplot_ppma
#
echo "Normal end of execution."
