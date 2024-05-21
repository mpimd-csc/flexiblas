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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_sgees = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgees,SGEES)(char* jobvs, char* sort, blasint* select, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
#else
void FC_GLOBAL(sgees,SGEES)(char* jobvs, char* sort, blasint* select, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
#endif
{
	void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);
	void (*fn_hook) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sgees.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sgees.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort); 
		return;
	} else {
		hook_pos_sgees = 0;
		fn_hook((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgees_(char* jobvs, char* sort, blasint* select, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias(MTS(FC_GLOBAL(sgees,SGEES)))));
#else
#ifndef __APPLE__
void sgees(char* jobvs, char* sort, blasint* select, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias(MTS(FC_GLOBAL(sgees,SGEES)))));
#else
void sgees(char* jobvs, char* sort, blasint* select, blasint* n, float* a, blasint* lda, blasint* sdim, float* wr, float* wi, float* vs, blasint* ldvs, float* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort){ FC_GLOBAL(sgees,SGEES)((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgees_(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
{
	void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

	*(void **) & fn = current_backend->lapack.sgees.f77_blas_function; 

		fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_sgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias("flexiblas_real_sgees_")));
#else
void flexiblas_real_sgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort){flexiblas_real_sgees_((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgees_(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
{
	void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);
	void (*fn_hook) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

	*(void **) &fn      = current_backend->lapack.sgees.f77_blas_function; 

    hook_pos_sgees ++;
    if( hook_pos_sgees < __flexiblas_hooks->sgees.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgees.f77_hook_function[hook_pos_sgees];
        fn_hook((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort);
    } else {
        hook_pos_sgees = 0;
		fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_sgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias("flexiblas_chain_sgees_")));
#else
void flexiblas_chain_sgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort){flexiblas_chain_sgees_((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort);}
#endif



