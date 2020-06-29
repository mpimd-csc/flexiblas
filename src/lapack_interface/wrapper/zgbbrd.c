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
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
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



static TLS_STORE uint8_t hook_pos_zgbbrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgbbrd,ZGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* d, double* e, double complex* q, blasint* ldq, double complex* pt, blasint* ldpt, double complex* c, blasint* ldc, double complex* work, double* rwork, blasint* info)
#else
void FC_GLOBAL(zgbbrd,ZGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* d, double* e, double complex* q, blasint* ldq, double complex* pt, blasint* ldpt, double complex* c, blasint* ldc, double complex* work, double* rwork, blasint* info)
#endif
{
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info);
	void (*fn_hook) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.zgbbrd.f77_blas_function; 
	fn_hook = __flexiblas_hooks->zgbbrd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info); 
		return;
	} else {
		hook_pos_zgbbrd = 0;
		fn_hook((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgbbrd_(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* d, double* e, double complex* q, blasint* ldq, double complex* pt, blasint* ldpt, double complex* c, blasint* ldc, double complex* work, double* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgbbrd,ZGBBRD)))));
#else
void zgbbrd(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* d, double* e, double complex* q, blasint* ldq, double complex* pt, blasint* ldpt, double complex* c, blasint* ldc, double complex* work, double* rwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgbbrd,ZGBBRD)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgbbrd_(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info)
{
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info);

	fn = current_backend->lapack.zgbbrd.f77_blas_function; 

		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info); 

	return;
}

void flexiblas_real_zgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info)  __attribute__((alias("flexiblas_real_zgbbrd_")));





/* Chainloader for Hooks */


void flexiblas_chain_zgbbrd_(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info)
{
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info);
	void (*fn_hook) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info);

	fn      = current_backend->lapack.zgbbrd.f77_blas_function; 

    hook_pos_zgbbrd ++;
    if( hook_pos_zgbbrd < __flexiblas_hooks->zgbbrd.nhook) {
        fn_hook = __flexiblas_hooks->zgbbrd.f77_hook_function[hook_pos_zgbbrd];
        fn_hook((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info);
    } else {
        hook_pos_zgbbrd = 0;
		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info); 
	}
	return;
}

void flexiblas_chain_zgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info)  __attribute__((alias("flexiblas_chain_zgbbrd_")));




