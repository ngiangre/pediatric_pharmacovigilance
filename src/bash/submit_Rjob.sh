#!/bin/bash

if [ -f ~/output/$1.o ]; then
rm ~/output/$1.o
fi
if [ -f ~/error/$1.e ]; then
rm ~/error/$1.e
fi
bsub -o ~/output/$1.o -e ~/error/$1.e "Rscript $1" -J $1
