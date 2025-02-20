#! /bin/bash
#
~/bin/analemma 0.01671 77.1774 23.4397 > analemma_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
gnuplot analemma_commands.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
