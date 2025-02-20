#! /bin/bash
#
gfortran -c -Wall dsp_to_st.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran dsp_to_st.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm dsp_to_st.o
#
chmod ugo+x a.out
mv a.out ~/bin/dsp_to_st
#
echo "Normal end of execution."
