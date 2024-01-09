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



static TLS_STORE uint8_t hook_pos_zgedmd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgedmd,ZGEDMD)(char* jobs, char* jobz, char* jobr, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float complex* x, blasint* ldx, float complex* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float complex* eigs, float complex* z, blasint* ldz, float* res, float complex* b, blasint* ldb, float complex* w, blasint* ldw, float complex* s, blasint* lds, float complex* zwork, blasint* lzwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info)
#else
void FC_GLOBAL(zgedmd,ZGEDMD)(char* jobs, char* jobz, char* jobr, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float complex* x, blasint* ldx, float complex* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float complex* eigs, float complex* z, blasint* ldz, float* res, float complex* b, blasint* ldb, float complex* w, blasint* ldw, float complex* s, blasint* lds, float complex* zwork, blasint* lzwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info)
#endif
{
	void (*fn) (void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);
	void (*fn_hook) (void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zgedmd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zgedmd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) eigs, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) w, (void*) ldw, (void*) s, (void*) lds, (void*) zwork, (void*) lzwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info); 
		return;
	} else {
		hook_pos_zgedmd = 0;
		fn_hook((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) eigs, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) w, (void*) ldw, (void*) s, (void*) lds, (void*) zwork, (void*) lzwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgedmd_(char* jobs, char* jobz, char* jobr, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float complex* x, blasint* ldx, float complex* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float complex* eigs, float complex* z, blasint* ldz, float* res, float complex* b, blasint* ldb, float complex* w, blasint* ldw, float complex* s, blasint* lds, float complex* zwork, blasint* lzwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgedmd,ZGEDMD)))));
#else
#ifndef __APPLE__
void zgedmd(char* jobs, char* jobz, char* jobr, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float complex* x, blasint* ldx, float complex* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float complex* eigs, float complex* z, blasint* ldz, float* res, float complex* b, blasint* ldb, float complex* w, blasint* ldw, float complex* s, blasint* lds, float complex* zwork, blasint* lzwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgedmd,ZGEDMD)))));
#else
void zgedmd(char* jobs, char* jobz, char* jobr, char* jobf, blasint* whtsvd, blasint* m, blasint* n, float complex* x, blasint* ldx, float complex* y, blasint* ldy, blasint* nrnk, float* tol, blasint* k, float complex* eigs, float complex* z, blasint* ldz, float* res, float complex* b, blasint* ldb, float complex* w, blasint* ldw, float complex* s, blasint* lds, float complex* zwork, blasint* lzwork, float* rwork, blasint* lrwork, blasint* iwork, blasint* liwork, blasint* info){ FC_GLOBAL(zgedmd,ZGEDMD)((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) eigs, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) w, (void*) ldw, (void*) s, (void*) lds, (void*) zwork, (void*) lzwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgedmd_(void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info)
{
	void (*fn) (void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);

	*(void **) & fn = current_backend->lapack.zgedmd.f77_blas_function; 

		fn((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) eigs, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) w, (void*) ldw, (void*) s, (void*) lds, (void*) zwork, (void*) lzwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zgedmd(void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info) __attribute__((alias("flexiblas_real_zgedmd_")));
#else
void flexiblas_real_zgedmd(void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info){flexiblas_real_zgedmd_((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) eigs, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) w, (void*) ldw, (void*) s, (void*) lds, (void*) zwork, (void*) lzwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgedmd_(void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info)
{
	void (*fn) (void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);
	void (*fn_hook) (void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info);

	*(void **) &fn      = current_backend->lapack.zgedmd.f77_blas_function; 

    hook_pos_zgedmd ++;
    if( hook_pos_zgedmd < __flexiblas_hooks->zgedmd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgedmd.f77_hook_function[hook_pos_zgedmd];
        fn_hook((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) eigs, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) w, (void*) ldw, (void*) s, (void*) lds, (void*) zwork, (void*) lzwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info);
    } else {
        hook_pos_zgedmd = 0;
		fn((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) eigs, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) w, (void*) ldw, (void*) s, (void*) lds, (void*) zwork, (void*) lzwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zgedmd(void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info) __attribute__((alias("flexiblas_chain_zgedmd_")));
#else
void flexiblas_chain_zgedmd(void* jobs, void* jobz, void* jobr, void* jobf, void* whtsvd, void* m, void* n, void* x, void* ldx, void* y, void* ldy, void* nrnk, void* tol, void* k, void* eigs, void* z, void* ldz, void* res, void* b, void* ldb, void* w, void* ldw, void* s, void* lds, void* zwork, void* lzwork, void* rwork, void* lrwork, void* iwork, void* liwork, void* info){flexiblas_chain_zgedmd_((void*) jobs, (void*) jobz, (void*) jobr, (void*) jobf, (void*) whtsvd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) y, (void*) ldy, (void*) nrnk, (void*) tol, (void*) k, (void*) eigs, (void*) z, (void*) ldz, (void*) res, (void*) b, (void*) ldb, (void*) w, (void*) ldw, (void*) s, (void*) lds, (void*) zwork, (void*) lzwork, (void*) rwork, (void*) lrwork, (void*) iwork, (void*) liwork, (void*) info);}
#endif



