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
#include <complex.h>
#include "cblas.h" 
#include "flexiblas_fortran_mangle.h"
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
	printf("zdotc gnu\n");
	double complex a[]={1+I,2+I,3,4}; 
	double complex b[]={1,1,1,1}; 
	Int n = 4;
	Int one = 1; 
	double complex ret = 0, ret2 = 0; 

#ifdef FLEXIBLAS_CBLAS
	n = 4; 
	one = 1; 
	cblas_zdotc_sub(n, a, one,b,one,&ret2); 
	printf("cblas_zdotc(test) = %lg + %lg\n", creal(ret2), cimag(ret2) ); 
#endif

	ret = FC_GLOBAL(zdotc,ZDOTC)(&n, a, &one,b, &one); 
	printf("zdotc_(test)      = %lg  + %lg\n", creal(ret), cimag(ret) ); 

	return 0;
}

