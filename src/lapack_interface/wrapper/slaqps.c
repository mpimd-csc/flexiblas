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


#ifndef FLEXIBLAS_CHARLEN_T
#define FLEXIBLAS_CHARLEN_T
#if __GNUC__ > 7
typedef size_t flexiblas_fortran_charlen_t;
#else
typedef int flexiblas_fortran_charlen_t;
#endif
#endif

#ifndef blasint
#ifdef FLEXIBLAS_INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif
#endif



static TLS_STORE uint8_t hook_pos_slaqps = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaqps,SLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* auxv, float* f, blasint* ldf)
#else
void FC_GLOBAL(slaqps,SLAQPS)(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* auxv, float* f, blasint* ldf)
#endif
{
	void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);
	void (*fn_hook) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slaqps.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slaqps.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); 
		return;
	} else {
		hook_pos_slaqps = 0;
		fn_hook((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaqps_(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* auxv, float* f, blasint* ldf) __attribute__((alias(MTS(FC_GLOBAL(slaqps,SLAQPS)))));
#else
#ifndef __APPLE__
void slaqps(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* auxv, float* f, blasint* ldf) __attribute__((alias(MTS(FC_GLOBAL(slaqps,SLAQPS)))));
#else
void slaqps(blasint* m, blasint* n, blasint* offset, blasint* nb, blasint* kb, float* a, blasint* lda, blasint* jpvt, float* tau, float* vn1, float* vn2, float* auxv, float* f, blasint* ldf){ FC_GLOBAL(slaqps,SLAQPS)((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaqps_(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf)
{
	void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);

	*(void **) & fn = current_backend->lapack.slaqps.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slaqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf) __attribute__((alias("flexiblas_real_slaqps_")));
#else
void flexiblas_real_slaqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf){flexiblas_real_slaqps_((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaqps_(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf)
{
	void (*fn) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);
	void (*fn_hook) (void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf);

	*(void **) &fn      = current_backend->lapack.slaqps.f77_blas_function; 

    hook_pos_slaqps ++;
    if( hook_pos_slaqps < __flexiblas_hooks->slaqps.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaqps.f77_hook_function[hook_pos_slaqps];
        fn_hook((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);
    } else {
        hook_pos_slaqps = 0;
		fn((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slaqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf) __attribute__((alias("flexiblas_chain_slaqps_")));
#else
void flexiblas_chain_slaqps(void* m, void* n, void* offset, void* nb, void* kb, void* a, void* lda, void* jpvt, void* tau, void* vn1, void* vn2, void* auxv, void* f, void* ldf){flexiblas_chain_slaqps_((void*) m, (void*) n, (void*) offset, (void*) nb, (void*) kb, (void*) a, (void*) lda, (void*) jpvt, (void*) tau, (void*) vn1, (void*) vn2, (void*) auxv, (void*) f, (void*) ldf);}
#endif



