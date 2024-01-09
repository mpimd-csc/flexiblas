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



static TLS_STORE uint8_t hook_pos_slaqz2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaqz2,SLAQZ2)(blasint* ilq, blasint* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, blasint* nq, blasint* qstart, float* q, blasint* ldq, blasint* nz, blasint* zstart, float* z, blasint* ldz)
#else
void FC_GLOBAL(slaqz2,SLAQZ2)(blasint* ilq, blasint* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, blasint* nq, blasint* qstart, float* q, blasint* ldq, blasint* nz, blasint* zstart, float* z, blasint* ldz)
#endif
{
	void (*fn) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);
	void (*fn_hook) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slaqz2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slaqz2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz); 
		return;
	} else {
		hook_pos_slaqz2 = 0;
		fn_hook((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaqz2_(blasint* ilq, blasint* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, blasint* nq, blasint* qstart, float* q, blasint* ldq, blasint* nz, blasint* zstart, float* z, blasint* ldz) __attribute__((alias(MTS(FC_GLOBAL(slaqz2,SLAQZ2)))));
#else
#ifndef __APPLE__
void slaqz2(blasint* ilq, blasint* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, blasint* nq, blasint* qstart, float* q, blasint* ldq, blasint* nz, blasint* zstart, float* z, blasint* ldz) __attribute__((alias(MTS(FC_GLOBAL(slaqz2,SLAQZ2)))));
#else
void slaqz2(blasint* ilq, blasint* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, float* a, blasint* lda, float* b, blasint* ldb, blasint* nq, blasint* qstart, float* q, blasint* ldq, blasint* nz, blasint* zstart, float* z, blasint* ldz){ FC_GLOBAL(slaqz2,SLAQZ2)((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaqz2_(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz)
{
	void (*fn) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);

	*(void **) & fn = current_backend->lapack.slaqz2.f77_blas_function; 

		fn((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slaqz2(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz) __attribute__((alias("flexiblas_real_slaqz2_")));
#else
void flexiblas_real_slaqz2(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz){flexiblas_real_slaqz2_((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaqz2_(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz)
{
	void (*fn) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);
	void (*fn_hook) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);

	*(void **) &fn      = current_backend->lapack.slaqz2.f77_blas_function; 

    hook_pos_slaqz2 ++;
    if( hook_pos_slaqz2 < __flexiblas_hooks->slaqz2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaqz2.f77_hook_function[hook_pos_slaqz2];
        fn_hook((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);
    } else {
        hook_pos_slaqz2 = 0;
		fn((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slaqz2(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz) __attribute__((alias("flexiblas_chain_slaqz2_")));
#else
void flexiblas_chain_slaqz2(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz){flexiblas_chain_slaqz2_((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);}
#endif



