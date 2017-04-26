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
 /* Generated: Tue Mar 28 16:07:35 2017 */ 
        
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
void FC_GLOBAL(dsbgvx,DSBGVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* ka, blasint* kb, double* ab, blasint* ldab, double* bb, blasint* ldbb, double* q, blasint* ldq, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info)
#else
void FC_GLOBAL(dsbgvx,DSBGVX)(char* jobz, char* range, char* uplo, blasint* n, blasint* ka, blasint* kb, double* ab, blasint* ldab, double* bb, blasint* ldbb, double* q, blasint* ldq, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* jobz, void* range, void* uplo, void* n, void* ka, void* kb, void* ab, void* ldab, void* bb, void* ldbb, void* q, void* ldq, void* vl, void* vu, void* il, void* iu, void* abstol, void* m, void* w, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.dsbgvx.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) q, (void*) ldq, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); 
		current_backend->lapack.dsbgvx.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.dsbgvx.calls[0]++;
	} else { 
		fn((void*) jobz, (void*) range, (void*) uplo, (void*) n, (void*) ka, (void*) kb, (void*) ab, (void*) ldab, (void*) bb, (void*) ldbb, (void*) q, (void*) ldq, (void*) vl, (void*) vu, (void*) il, (void*) iu, (void*) abstol, (void*) m, (void*) w, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void dsbgvx_(char* jobz, char* range, char* uplo, blasint* n, blasint* ka, blasint* kb, double* ab, blasint* ldab, double* bb, blasint* ldbb, double* q, blasint* ldq, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsbgvx,DSBGVX)))));
#else
void dsbgvx(char* jobz, char* range, char* uplo, blasint* n, blasint* ka, blasint* kb, double* ab, blasint* ldab, double* bb, blasint* ldbb, double* q, blasint* ldq, double* vl, double* vu, blasint* il, blasint* iu, double* abstol, blasint* m, double* w, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dsbgvx,DSBGVX)))));
#endif



