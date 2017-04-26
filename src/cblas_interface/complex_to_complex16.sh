#!/bin/bash
for  i in cblas_c*.c
do
 func=`echo "$i" | sed -e 's/cblas_\(c[a-z0-9]*\)\.c/\1/g' -`
 sfunc=`echo "$func" | sed -e 's/c\([a-z0-9]*\)/z\1/g'`
 echo $func $sfunc
 sed -e 's,float,double,g' -e "s,$func,$sfunc,g" cblas_$func.c > cblas_$sfunc.c 

done
