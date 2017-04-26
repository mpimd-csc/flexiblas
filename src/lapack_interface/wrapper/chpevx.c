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
 /* Generated: Tue Mar 28 16:07:32 2017 */ 
        
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
void FC_GLOBAL(chpevx,CHPEVX)(char* jobz, char* range, char* uplo, blasint* n, float complex* ap, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float complex* z, blasint* ldz, float complex* work, float* rwork, blasint* iwork, blasint* ifail, blasint* info)
#else
void FC_GLOBAL(chpevx,CHPEVX)(char* jobz, char* range, char* uplo, blasint* n, float complex* ap, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float complex* z, blasint* ldz, float complex* work, float* rwork, blasint* iwork, blasint* ifail, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* jobz, void* range, void* uplo, void* n, void* ap, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* rwork, void* iwork, void* ifail, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.chpevx.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) ap, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) rwork, (void*) iwork, (void*) ifail, (void*) info); 
		current_backend->lapack.chpevx.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.chpevx.calls[0]++;
	} else { 
		fn((void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) ap, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) rwork, (void*) iwork, (void*) ifail, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void chpevx_(char* jobz, char* range, char* uplo, blasint* n, float complex* ap, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float complex* z, blasint* ldz, float complex* work, float* rwork, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chpevx,CHPEVX)))));
#else
void chpevx(char* jobz, char* range, char* uplo, blasint* n, float complex* ap, float* vl, float* vu, blasint* il, blasint* iu, float* abstol, blasint* m, float* w, float complex* z, blasint* ldz, float complex* work, float* rwork, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(chpevx,CHPEVX)))));
#endif


