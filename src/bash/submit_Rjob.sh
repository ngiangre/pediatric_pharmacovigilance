#!/bin/bash

if [ -f $1.o ]; then
rm $1.o
fi
if [ -f $1.e ]; then
rm $1.e
fi
bsub -o $1.o -e $1.e "Rscript $1" -J $1
