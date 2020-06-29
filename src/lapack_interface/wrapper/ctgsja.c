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
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
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



static TLS_STORE uint8_t hook_pos_ctgsja = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctgsja,CTGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* ncycle, blasint* info)
#else
void FC_GLOBAL(ctgsja,CTGSJA)(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* ncycle, blasint* info)
#endif
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info);
	void (*fn_hook) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.ctgsja.f77_blas_function; 
	fn_hook = __flexiblas_hooks->ctgsja.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info); 
		return;
	} else {
		hook_pos_ctgsja = 0;
		fn_hook((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ctgsja_(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* ncycle, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ctgsja,CTGSJA)))));
#else
void ctgsja(char* jobu, char* jobv, char* jobq, blasint* m, blasint* p, blasint* n, blasint* k, blasint* l, float complex* a, blasint* lda, float complex* b, blasint* ldb, float* tola, float* tolb, float* alpha, float* beta, float complex* u, blasint* ldu, float complex* v, blasint* ldv, float complex* q, blasint* ldq, float complex* work, blasint* ncycle, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(ctgsja,CTGSJA)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctgsja_(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info)
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info);

	fn = current_backend->lapack.ctgsja.f77_blas_function; 

		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info); 

	return;
}

void flexiblas_real_ctgsja(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info)  __attribute__((alias("flexiblas_real_ctgsja_")));





/* Chainloader for Hooks */


void flexiblas_chain_ctgsja_(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info)
{
	void (*fn) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info);
	void (*fn_hook) (void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info);

	fn      = current_backend->lapack.ctgsja.f77_blas_function; 

    hook_pos_ctgsja ++;
    if( hook_pos_ctgsja < __flexiblas_hooks->ctgsja.nhook) {
        fn_hook = __flexiblas_hooks->ctgsja.f77_hook_function[hook_pos_ctgsja];
        fn_hook((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info);
    } else {
        hook_pos_ctgsja = 0;
		fn((void*) jobu, (void*) jobv, (void*) jobq, (void*) m, (void*) p, (void*) n, (void*) k, (void*) l, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) tola, (void*) tolb, (void*) alpha, (void*) beta, (void*) u, (void*) ldu, (void*) v, (void*) ldv, (void*) q, (void*) ldq, (void*) work, (void*) ncycle, (void*) info); 
	}
	return;
}

void flexiblas_chain_ctgsja(void* jobu, void* jobv, void* jobq, void* m, void* p, void* n, void* k, void* l, void* a, void* lda, void* b, void* ldb, void* tola, void* tolb, void* alpha, void* beta, void* u, void* ldu, void* v, void* ldv, void* q, void* ldq, void* work, void* ncycle, void* info)  __attribute__((alias("flexiblas_chain_ctgsja_")));




