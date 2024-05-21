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


static TLS_STORE uint8_t hook_pos_dgees = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dgees,DGEES)(char* jobvs, char* sort, blasint* select, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
#else
void FC_GLOBAL(dgees,DGEES)(char* jobvs, char* sort, blasint* select, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
#endif
{
	void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);
	void (*fn_hook) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dgees.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dgees.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort); 
		return;
	} else {
		hook_pos_dgees = 0;
		fn_hook((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dgees_(char* jobvs, char* sort, blasint* select, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias(MTS(FC_GLOBAL(dgees,DGEES)))));
#else
#ifndef __APPLE__
void dgees(char* jobvs, char* sort, blasint* select, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias(MTS(FC_GLOBAL(dgees,DGEES)))));
#else
void dgees(char* jobvs, char* sort, blasint* select, blasint* n, double* a, blasint* lda, blasint* sdim, double* wr, double* wi, double* vs, blasint* ldvs, double* work, blasint* lwork, blasint* bwork, blasint* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort){ FC_GLOBAL(dgees,DGEES)((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dgees_(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
{
	void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

	*(void **) & fn = current_backend->lapack.dgees.f77_blas_function; 

		fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias("flexiblas_real_dgees_")));
#else
void flexiblas_real_dgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort){flexiblas_real_dgees_((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dgees_(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort)
{
	void (*fn) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);
	void (*fn_hook) (void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort);

	*(void **) &fn      = current_backend->lapack.dgees.f77_blas_function; 

    hook_pos_dgees ++;
    if( hook_pos_dgees < __flexiblas_hooks->dgees.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dgees.f77_hook_function[hook_pos_dgees];
        fn_hook((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort);
    } else {
        hook_pos_dgees = 0;
		fn((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobvs, ( flexiblas_fortran_charlen_t ) len_sort); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort) __attribute__((alias("flexiblas_chain_dgees_")));
#else
void flexiblas_chain_dgees(void* jobvs, void* sort, void* select, void* n, void* a, void* lda, void* sdim, void* wr, void* wi, void* vs, void* ldvs, void* work, void* lwork, void* bwork, void* info, flexiblas_fortran_charlen_t len_jobvs, flexiblas_fortran_charlen_t len_sort){flexiblas_chain_dgees_((void*) jobvs, (void*) sort, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) sdim, (void*) wr, (void*) wi, (void*) vs, (void*) ldvs, (void*) work, (void*) lwork, (void*) bwork, (void*) info, (flexiblas_fortran_charlen_t) len_jobvs, (flexiblas_fortran_charlen_t) len_sort);}
#endif



