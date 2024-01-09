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



static TLS_STORE uint8_t hook_pos_dlasdq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasdq,DLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, double* d, double* e, double* vt, blasint* ldvt, double* u, blasint* ldu, double* c, blasint* ldc, double* work, blasint* info)
#else
void FC_GLOBAL(dlasdq,DLASDQ)(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, double* d, double* e, double* vt, blasint* ldvt, double* u, blasint* ldu, double* c, blasint* ldc, double* work, blasint* info)
#endif
{
	void (*fn) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info);
	void (*fn_hook) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlasdq.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlasdq.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info); 
		return;
	} else {
		hook_pos_dlasdq = 0;
		fn_hook((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasdq_(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, double* d, double* e, double* vt, blasint* ldvt, double* u, blasint* ldu, double* c, blasint* ldc, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasdq,DLASDQ)))));
#else
#ifndef __APPLE__
void dlasdq(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, double* d, double* e, double* vt, blasint* ldvt, double* u, blasint* ldu, double* c, blasint* ldc, double* work, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlasdq,DLASDQ)))));
#else
void dlasdq(char* uplo, blasint* sqre, blasint* n, blasint* ncvt, blasint* nru, blasint* ncc, double* d, double* e, double* vt, blasint* ldvt, double* u, blasint* ldu, double* c, blasint* ldc, double* work, blasint* info){ FC_GLOBAL(dlasdq,DLASDQ)((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasdq_(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info)
{
	void (*fn) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info);

	*(void **) & fn = current_backend->lapack.dlasdq.f77_blas_function; 

		fn((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlasdq(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info) __attribute__((alias("flexiblas_real_dlasdq_")));
#else
void flexiblas_real_dlasdq(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info){flexiblas_real_dlasdq_((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlasdq_(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info)
{
	void (*fn) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info);
	void (*fn_hook) (void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info);

	*(void **) &fn      = current_backend->lapack.dlasdq.f77_blas_function; 

    hook_pos_dlasdq ++;
    if( hook_pos_dlasdq < __flexiblas_hooks->dlasdq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlasdq.f77_hook_function[hook_pos_dlasdq];
        fn_hook((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info);
    } else {
        hook_pos_dlasdq = 0;
		fn((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlasdq(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info) __attribute__((alias("flexiblas_chain_dlasdq_")));
#else
void flexiblas_chain_dlasdq(void* uplo, void* sqre, void* n, void* ncvt, void* nru, void* ncc, void* d, void* e, void* vt, void* ldvt, void* u, void* ldu, void* c, void* ldc, void* work, void* info){flexiblas_chain_dlasdq_((void*) uplo, (void*) sqre, (void*) n, (void*) ncvt, (void*) nru, (void*) ncc, (void*) d, (void*) e, (void*) vt, (void*) ldvt, (void*) u, (void*) ldu, (void*) c, (void*) ldc, (void*) work, (void*) info);}
#endif



