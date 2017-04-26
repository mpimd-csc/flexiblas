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
 * Copyright (C) Martin Koehler, 2015-2017
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Tue Mar 28 16:07:38 2017 */ 
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


#ifdef INTEGER8
#define blasint int64_t
#else 
#define blasint int 
#endif



#ifdef FLEXIBLAS_ABI_INTEL 
void FC_GLOBAL(zlatrs,ZLATRS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* a, blasint* lda, double complex* x, double* scale, double* cnorm, blasint* info)
#else
void FC_GLOBAL(zlatrs,ZLATRS)(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* a, blasint* lda, double complex* x, double* scale, double* cnorm, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* uplo, void* trans, void* diag, void* normin, void* n, void* a, void* lda, void* x, void* scale, void* cnorm, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.zlatrs.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) scale, (void*) cnorm, (void*) info); 
		current_backend->lapack.zlatrs.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.zlatrs.calls[0]++;
	} else { 
		fn((void*) uplo, (void*) trans, (void*) diag, (void*) normin, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) scale, (void*) cnorm, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void zlatrs_(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* a, blasint* lda, double complex* x, double* scale, double* cnorm, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlatrs,ZLATRS)))));
#else
void zlatrs(char* uplo, char* trans, char* diag, char* normin, blasint* n, double complex* a, blasint* lda, double complex* x, double* scale, double* cnorm, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zlatrs,ZLATRS)))));
#endif



