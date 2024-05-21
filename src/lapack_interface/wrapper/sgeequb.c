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


static TLS_STORE uint8_t hook_pos_sgeequb = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgeequb,SGEEQUB)(blasint* m, blasint* n, float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info)
#else
void FC_GLOBAL(sgeequb,SGEEQUB)(blasint* m, blasint* n, float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info)
#endif
{
	void (*fn) (void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info);
	void (*fn_hook) (void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sgeequb.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sgeequb.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) info); 
		return;
	} else {
		hook_pos_sgeequb = 0;
		fn_hook((void*) m, (void*) n, (void*) a, (void*) lda, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgeequb_(blasint* m, blasint* n, float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgeequb,SGEEQUB)))));
#else
#ifndef __APPLE__
void sgeequb(blasint* m, blasint* n, float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(sgeequb,SGEEQUB)))));
#else
void sgeequb(blasint* m, blasint* n, float* a, blasint* lda, float* r, float* c, float* rowcnd, float* colcnd, float* amax, blasint* info){ FC_GLOBAL(sgeequb,SGEEQUB)((void*) m, (void*) n, (void*) a, (void*) lda, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgeequb_(void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info)
{
	void (*fn) (void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info);

	*(void **) & fn = current_backend->lapack.sgeequb.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_sgeequb(void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info) __attribute__((alias("flexiblas_real_sgeequb_")));
#else
void flexiblas_real_sgeequb(void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info){flexiblas_real_sgeequb_((void*) m, (void*) n, (void*) a, (void*) lda, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgeequb_(void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info)
{
	void (*fn) (void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info);
	void (*fn_hook) (void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info);

	*(void **) &fn      = current_backend->lapack.sgeequb.f77_blas_function; 

    hook_pos_sgeequb ++;
    if( hook_pos_sgeequb < __flexiblas_hooks->sgeequb.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgeequb.f77_hook_function[hook_pos_sgeequb];
        fn_hook((void*) m, (void*) n, (void*) a, (void*) lda, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) info);
    } else {
        hook_pos_sgeequb = 0;
		fn((void*) m, (void*) n, (void*) a, (void*) lda, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_sgeequb(void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info) __attribute__((alias("flexiblas_chain_sgeequb_")));
#else
void flexiblas_chain_sgeequb(void* m, void* n, void* a, void* lda, void* r, void* c, void* rowcnd, void* colcnd, void* amax, void* info){flexiblas_chain_sgeequb_((void*) m, (void*) n, (void*) a, (void*) lda, (void*) r, (void*) c, (void*) rowcnd, (void*) colcnd, (void*) amax, (void*) info);}
#endif



