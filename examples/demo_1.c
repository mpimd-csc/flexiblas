/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2015
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

/*-----------------------------------------------------------------------------
 *  Include the right header files 
 *-----------------------------------------------------------------------------*/
#ifdef BLAS_INTERFACE_INTEL 
#include "blas_intel.h"
#include "extblas_intel.h"
#else 
#include "blas_gnu.h"
#include "extblas_gnu.h"
#endif 
#ifdef FLEXIBLAS_CBLAS
#include "cblas.h" 
#endif 

#ifdef INTEGER8
#define Int int64_t
#else 
#define Int int
#endif 


int main ( int argc, char **argv ) {
	printf("demo 1\n");
	double test1[]={1,2,3,4,5,6,7,8,9,10}; 
	Int N = 10;
	Int one = 1; 
    int N32 = 10;
    int one32 = 1;
    int64_t N64 = 10;
    int64_t one64 = 1;
	double ret = 0, ret2 = 0;

    printf("Generic Interface\n");
	ret = dasum_(&N, test1, &one); 
	printf("dasum_(test)      = %lg\n", ret ); 

    printf("Int32 Interface\n");
	ret = dasum32_(&N32, test1, &one32); 
	printf("dasum32_(test)      = %lg\n", ret ); 

    printf("Int64 Interface\n");
	ret = dasum64_(&N64, test1, &one64); 
	printf("dasum64_(test)      = %lg\n", ret ); 

#ifdef FLEXIBLAS_CBLAS 
	ret2 = cblas_dasum(N, test1, one); 
	printf("cblas_dasum(test) = %lg\n", ret2 ); 
#endif
	return 0;
}

