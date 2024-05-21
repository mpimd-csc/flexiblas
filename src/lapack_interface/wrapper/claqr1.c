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


static TLS_STORE uint8_t hook_pos_claqr1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(claqr1,CLAQR1)(blasint* n, float complex* h, blasint* ldh, float complex* s1, float complex* s2, float complex* v)
#else
void FC_GLOBAL(claqr1,CLAQR1)(blasint* n, float complex* h, blasint* ldh, float complex* s1, float complex* s2, float complex* v)
#endif
{
	void (*fn) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);
	void (*fn_hook) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.claqr1.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->claqr1.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v); 
		return;
	} else {
		hook_pos_claqr1 = 0;
		fn_hook((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void claqr1_(blasint* n, float complex* h, blasint* ldh, float complex* s1, float complex* s2, float complex* v) __attribute__((alias(MTS(FC_GLOBAL(claqr1,CLAQR1)))));
#else
#ifndef __APPLE__
void claqr1(blasint* n, float complex* h, blasint* ldh, float complex* s1, float complex* s2, float complex* v) __attribute__((alias(MTS(FC_GLOBAL(claqr1,CLAQR1)))));
#else
void claqr1(blasint* n, float complex* h, blasint* ldh, float complex* s1, float complex* s2, float complex* v){ FC_GLOBAL(claqr1,CLAQR1)((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_claqr1_(void* n, void* h, void* ldh, void* s1, void* s2, void* v)
{
	void (*fn) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);

	*(void **) & fn = current_backend->lapack.claqr1.f77_blas_function; 

		fn((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_claqr1(void* n, void* h, void* ldh, void* s1, void* s2, void* v) __attribute__((alias("flexiblas_real_claqr1_")));
#else
void flexiblas_real_claqr1(void* n, void* h, void* ldh, void* s1, void* s2, void* v){flexiblas_real_claqr1_((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_claqr1_(void* n, void* h, void* ldh, void* s1, void* s2, void* v)
{
	void (*fn) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);
	void (*fn_hook) (void* n, void* h, void* ldh, void* s1, void* s2, void* v);

	*(void **) &fn      = current_backend->lapack.claqr1.f77_blas_function; 

    hook_pos_claqr1 ++;
    if( hook_pos_claqr1 < __flexiblas_hooks->claqr1.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->claqr1.f77_hook_function[hook_pos_claqr1];
        fn_hook((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v);
    } else {
        hook_pos_claqr1 = 0;
		fn((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_claqr1(void* n, void* h, void* ldh, void* s1, void* s2, void* v) __attribute__((alias("flexiblas_chain_claqr1_")));
#else
void flexiblas_chain_claqr1(void* n, void* h, void* ldh, void* s1, void* s2, void* v){flexiblas_chain_claqr1_((void*) n, (void*) h, (void*) ldh, (void*) s1, (void*) s2, (void*) v);}
#endif



