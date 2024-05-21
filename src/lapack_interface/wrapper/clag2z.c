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


static TLS_STORE uint8_t hook_pos_clag2z = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clag2z,CLAG2Z)(blasint* m, blasint* n, float complex* sa, blasint* ldsa, double complex* a, blasint* lda, blasint* info)
#else
void FC_GLOBAL(clag2z,CLAG2Z)(blasint* m, blasint* n, float complex* sa, blasint* ldsa, double complex* a, blasint* lda, blasint* info)
#endif
{
	void (*fn) (void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info);
	void (*fn_hook) (void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clag2z.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clag2z.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) m, (void*) n, (void*) sa, (void*) ldsa, (void*) a, (void*) lda, (void*) info); 
		return;
	} else {
		hook_pos_clag2z = 0;
		fn_hook((void*) m, (void*) n, (void*) sa, (void*) ldsa, (void*) a, (void*) lda, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void clag2z_(blasint* m, blasint* n, float complex* sa, blasint* ldsa, double complex* a, blasint* lda, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(clag2z,CLAG2Z)))));
#else
#ifndef __APPLE__
void clag2z(blasint* m, blasint* n, float complex* sa, blasint* ldsa, double complex* a, blasint* lda, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(clag2z,CLAG2Z)))));
#else
void clag2z(blasint* m, blasint* n, float complex* sa, blasint* ldsa, double complex* a, blasint* lda, blasint* info){ FC_GLOBAL(clag2z,CLAG2Z)((void*) m, (void*) n, (void*) sa, (void*) ldsa, (void*) a, (void*) lda, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clag2z_(void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info)
{
	void (*fn) (void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info);

	*(void **) & fn = current_backend->lapack.clag2z.f77_blas_function; 

		fn((void*) m, (void*) n, (void*) sa, (void*) ldsa, (void*) a, (void*) lda, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_clag2z(void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info) __attribute__((alias("flexiblas_real_clag2z_")));
#else
void flexiblas_real_clag2z(void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info){flexiblas_real_clag2z_((void*) m, (void*) n, (void*) sa, (void*) ldsa, (void*) a, (void*) lda, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clag2z_(void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info)
{
	void (*fn) (void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info);
	void (*fn_hook) (void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info);

	*(void **) &fn      = current_backend->lapack.clag2z.f77_blas_function; 

    hook_pos_clag2z ++;
    if( hook_pos_clag2z < __flexiblas_hooks->clag2z.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clag2z.f77_hook_function[hook_pos_clag2z];
        fn_hook((void*) m, (void*) n, (void*) sa, (void*) ldsa, (void*) a, (void*) lda, (void*) info);
    } else {
        hook_pos_clag2z = 0;
		fn((void*) m, (void*) n, (void*) sa, (void*) ldsa, (void*) a, (void*) lda, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_clag2z(void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info) __attribute__((alias("flexiblas_chain_clag2z_")));
#else
void flexiblas_chain_clag2z(void* m, void* n, void* sa, void* ldsa, void* a, void* lda, void* info){flexiblas_chain_clag2z_((void*) m, (void*) n, (void*) sa, (void*) ldsa, (void*) a, (void*) lda, (void*) info);}
#endif



