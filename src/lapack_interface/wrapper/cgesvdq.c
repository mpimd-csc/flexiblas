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


#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_cgesvdq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cgesvdq,CGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float complex* cwork, blasint* lcwork, float* rwork, blasint* lrwork, blasint* info)
#else
void FC_GLOBAL(cgesvdq,CGESVDQ)(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float complex* cwork, blasint* lcwork, float* rwork, blasint* lrwork, blasint* info)
#endif
{
	void (*fn) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info);
	void (*fn_hook) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cgesvdq.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cgesvdq.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) cwork, (void*) lcwork, (void*) rwork, (void*) lrwork, (void*) info); 
		return;
	} else {
		hook_pos_cgesvdq = 0;
		fn_hook((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) cwork, (void*) lcwork, (void*) rwork, (void*) lrwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cgesvdq_(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float complex* cwork, blasint* lcwork, float* rwork, blasint* lrwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgesvdq,CGESVDQ)))));
#else
#ifndef __APPLE__
void cgesvdq(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float complex* cwork, blasint* lcwork, float* rwork, blasint* lrwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cgesvdq,CGESVDQ)))));
#else
void cgesvdq(char* joba, char* jobp, char* jobr, char* jobu, char* jobv, blasint* m, blasint* n, float complex* a, blasint* lda, float* s, float complex* u, blasint* ldu, float complex* v, blasint* ldv, blasint* numrank, blasint* iwork, blasint* liwork, float complex* cwork, blasint* lcwork, float* rwork, blasint* lrwork, blasint* info){ FC_GLOBAL(cgesvdq,CGESVDQ)((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) cwork, (void*) lcwork, (void*) rwork, (void*) lrwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cgesvdq_(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info)
{
	void (*fn) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info);

	*(void **) & fn = current_backend->lapack.cgesvdq.f77_blas_function; 

		fn((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) cwork, (void*) lcwork, (void*) rwork, (void*) lrwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info) __attribute__((alias("flexiblas_real_cgesvdq_")));
#else
void flexiblas_real_cgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info){flexiblas_real_cgesvdq_((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) cwork, (void*) lcwork, (void*) rwork, (void*) lrwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cgesvdq_(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info)
{
	void (*fn) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info);
	void (*fn_hook) (void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info);

	*(void **) &fn      = current_backend->lapack.cgesvdq.f77_blas_function; 

    hook_pos_cgesvdq ++;
    if( hook_pos_cgesvdq < __flexiblas_hooks->cgesvdq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cgesvdq.f77_hook_function[hook_pos_cgesvdq];
        fn_hook((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) cwork, (void*) lcwork, (void*) rwork, (void*) lrwork, (void*) info);
    } else {
        hook_pos_cgesvdq = 0;
		fn((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) cwork, (void*) lcwork, (void*) rwork, (void*) lrwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info) __attribute__((alias("flexiblas_chain_cgesvdq_")));
#else
void flexiblas_chain_cgesvdq(void* joba, void* jobp, void* jobr, void* jobu, void* jobv, void* m, void* n, void* a, void* lda, void* s, void* u, void* ldu, void* v, void* ldv, void* numrank, void* iwork, void* liwork, void* cwork, void* lcwork, void* rwork, void* lrwork, void* info){flexiblas_chain_cgesvdq_((void*) joba, (void*) jobp, (void*) jobr, (void*) jobu, (void*) jobv, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) s, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) numrank, (void*) iwork, (void*) liwork, (void*) cwork, (void*) lcwork, (void*) rwork, (void*) lrwork, (void*) info);}
#endif



