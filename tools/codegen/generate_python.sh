#!/bin/sh 
for i in *.ipynb 
do 
    jupytext --to script "$i"
done 
     
