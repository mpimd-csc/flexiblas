#!/bin/bash
for  i in cblas_d*.c
do
	func=`echo "$i" | sed -e 's/cblas_\(d[a-z0-9]*\)\.c/\1/g' -`
	sfunc=`echo "$func" | sed -e 's/d\([a-z0-9]*\)/s\1/g'`
	ufunc=`echo "$func" | sed -e 's/\([a-z0-9]*\)/\U\1/g'`
	usfunc=`echo "$sfunc" | sed -e 's/\([a-z0-9]*\)/\U\1/g'`


	echo $func $sfunc
	sed -e 's,double,float,g' -e "s,$func,$sfunc,g" -e "s,$ufunc,$usfunc,g"  cblas_$func.c > cblas_$sfunc.c


done
