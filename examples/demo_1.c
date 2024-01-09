//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
 */


#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "flexiblas_fortran_mangle.h"
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
	double ret = 0, ret2 = 0;

    printf("Generic Interface\n");
	ret = FC_GLOBAL(dasum,DASUM)(&N, test1, &one); 
	printf("dasum_(test)      = %lg\n", ret ); 

#ifdef FLEXIBLAS_CBLAS 
	ret2 = cblas_dasum(N, test1, one); 
	printf("cblas_dasum(test) = %lg\n", ret2 ); 
#endif
	return 0;
}

