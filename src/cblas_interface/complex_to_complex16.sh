#!/bin/bash
set -x

for  i in cblas_c*.c
do
 func=`echo "$i" | sed -e 's/cblas_\(c[a-z0-9]*\)\.c/\1/g' -`
 sfunc=`echo "$func" | sed -e 's/c\([a-z0-9]*\)/z\1/g'`
 ufunc=`echo "$func" | sed -e 's/\([a-z0-9]*\)/\U\1/g'`
 usfunc=`echo "$sfunc" | sed -e 's/\([a-z0-9]*\)/\U\1/g'`

 echo $func $sfunc $ufunc
 sed -e 's,float,double,g' -e "s,$func,$sfunc,g" -e "s,$ufunc,$usfunc,g" cblas_$func.c > cblas_$sfunc.c

done
