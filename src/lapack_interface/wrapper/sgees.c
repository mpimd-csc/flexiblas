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
void FC_GLOBAL(sgees,SGEES)(char* jobvs, char* sort, blasint* select, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blasint* bwork, blasint* info)
#else
void FC_GLOBAL(sgees,SGEES)(char* jobvs, char* sort, blasint* select, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blasint* bwork, blasint* info)
#endif 
{
    double ts;
	void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info);
	if ( current_backend->post_init != 0 ) {
		__flexiblas_backend_init(current_backend); 
		current_backend->post_init = 0; 
	}
	fn = current_backend->lapack.sgees.call_fblas; 
	if ( __flexiblas_profile ) {
		ts = flexiblas_wtime(); 
		fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info); 
		current_backend->lapack.sgees.timings[0] += (flexiblas_wtime() -ts);
		current_backend->lapack.sgees.calls[0]++;
	} else { 
		fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info); 
	} 
	return;
}
#ifdef FLEXIBLAS_ABI_IBM
void sgees_(char* jobvs, char* sort, blasint* select, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgees,SGEES)))));
#else
void sgees(char* jobvs, char* sort, blasint* select, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blasint* bwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgees,SGEES)))));
#endif



