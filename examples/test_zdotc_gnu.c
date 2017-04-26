#include <stdlib.h>
#include <stdio.h>
#include <complex.h>
#include "f77blas_interface.h"
#include "cblas.h" 

int main ( int argc, char **argv ) {
	printf("zdotc gnu\n");
	double complex a[]={1+I,2+I,3,4}; 
	double complex b[]={1,1,1,1}; 
	int n = 4;
	int one = 1; 
	double complex ret = 0, ret2 = 0; 

#ifdef FLEXIBLAS_CBLAS
	n = 4; 
	one = 1; 
	cblas_zdotc_sub(n, a, one,b,one,&ret2); 
	printf("cblas_zdotc(test) = %lg + %lg\n", creal(ret2), cimag(ret2) ); 
#endif

	ret = zdotc_(&n, a, &one,b, &one); 
	printf("zdotc_(test)      = %lg  + %lg\n", creal(ret), cimag(ret) ); 

	return 0;
}

