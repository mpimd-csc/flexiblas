#include <stdlib.h>
#include <stdio.h>
#include "f77blas_interface.h"
#include "cblas.h" 

int main ( int argc, char **argv ) {
	printf("demo 1\n");
	double test1[]={1,2,3,4,5,6,7,8,9,10}; 
	Int N = 10;
	Int one = 1; 
	double ret = 0, ret2 = 0; 
	ret = dasum_(&N, test1, &one); 
	printf("dasum_(test)      = %lg\n", ret ); 

#ifdef FLEXIBLAS_CBLAS 
	ret2 = cblas_dasum(N, test1, one); 
	printf("cblas_dasum(test) = %lg\n", ret2 ); 
#endif
	return 0;
}

