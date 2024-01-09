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



static TLS_STORE uint8_t hook_pos_dggsvp3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dggsvp3,DGGSVP3)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double* u, blasint* ldu, double* v, blasint* ldv, double* q, blasint* ldq, blasint* iwork, double* tau, double* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(dggsvp3,DGGSVP3)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double* u, blasint* ldu, double* v, blasint* ldv, double* q, blasint* ldq, blasint* iwork, double* tau, double* work, blasint* lwork, blasint* info)
#endif
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info);
	void (*fn_hook) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dggsvp3.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dggsvp3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) tau, (void*) work, (void*) lwork, (void*) info); 
		return;
	} else {
		hook_pos_dggsvp3 = 0;
		fn_hook((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) tau, (void*) work, (void*) lwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dggsvp3_(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double* u, blasint* ldu, double* v, blasint* ldv, double* q, blasint* ldq, blasint* iwork, double* tau, double* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dggsvp3,DGGSVP3)))));
#else
#ifndef __APPLE__
void dggsvp3(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double* u, blasint* ldu, double* v, blasint* ldv, double* q, blasint* ldq, blasint* iwork, double* tau, double* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dggsvp3,DGGSVP3)))));
#else
void dggsvp3(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, double* a, blasint* lda, double* b, blasint* ldb, double* tola, double* tolb, blasint* k, blasint* l, double* u, blasint* ldu, double* v, blasint* ldv, double* q, blasint* ldq, blasint* iwork, double* tau, double* work, blasint* lwork, blasint* info){ FC_GLOBAL(dggsvp3,DGGSVP3)((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) tau, (void*) work, (void*) lwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dggsvp3_(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info)
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info);

	*(void **) & fn = current_backend->lapack.dggsvp3.f77_blas_function; 

		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) tau, (void*) work, (void*) lwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dggsvp3(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_real_dggsvp3_")));
#else
void flexiblas_real_dggsvp3(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info){flexiblas_real_dggsvp3_((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) tau, (void*) work, (void*) lwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dggsvp3_(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info)
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info);
	void (*fn_hook) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info);

	*(void **) &fn      = current_backend->lapack.dggsvp3.f77_blas_function; 

    hook_pos_dggsvp3 ++;
    if( hook_pos_dggsvp3 < __flexiblas_hooks->dggsvp3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dggsvp3.f77_hook_function[hook_pos_dggsvp3];
        fn_hook((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) tau, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_dggsvp3 = 0;
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) tau, (void*) work, (void*) lwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dggsvp3(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_chain_dggsvp3_")));
#else
void flexiblas_chain_dggsvp3(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* k, void* l, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* iwork, void* tau, void* work, void* lwork, void* info){flexiblas_chain_dggsvp3_((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) k, (void*) l, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) iwork, (void*) tau, (void*) work, (void*) lwork, (void*) info);}
#endif



