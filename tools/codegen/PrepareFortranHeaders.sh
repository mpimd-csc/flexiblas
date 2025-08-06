#!/usr/bin/env bash

for i in *.f*; do
    NF=$(echo $i | cut -d. -f1)
    gfortran -fsyntax-only -fc-prototypes-external $i > $NF.h
done
