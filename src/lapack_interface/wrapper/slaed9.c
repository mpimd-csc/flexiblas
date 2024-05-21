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


static TLS_STORE uint8_t hook_pos_slaed9 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaed9,SLAED9)(blasint* k, blasint* kstart, blasint* kstop, blasint* n, float* d, float* q, blasint* ldq, float* rho, float* dlambda, float* w, float* s, blasint* lds, blasint* info)
#else
void FC_GLOBAL(slaed9,SLAED9)(blasint* k, blasint* kstart, blasint* kstop, blasint* n, float* d, float* q, blasint* ldq, float* rho, float* dlambda, float* w, float* s, blasint* lds, blasint* info)
#endif
{
	void (*fn) (void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info);
	void (*fn_hook) (void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slaed9.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slaed9.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) k, (void*) kstart, (void*) kstop, (void*) n, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) w, (void*) s, (void*) lds, (void*) info); 
		return;
	} else {
		hook_pos_slaed9 = 0;
		fn_hook((void*) k, (void*) kstart, (void*) kstop, (void*) n, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) w, (void*) s, (void*) lds, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaed9_(blasint* k, blasint* kstart, blasint* kstop, blasint* n, float* d, float* q, blasint* ldq, float* rho, float* dlambda, float* w, float* s, blasint* lds, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaed9,SLAED9)))));
#else
#ifndef __APPLE__
void slaed9(blasint* k, blasint* kstart, blasint* kstop, blasint* n, float* d, float* q, blasint* ldq, float* rho, float* dlambda, float* w, float* s, blasint* lds, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaed9,SLAED9)))));
#else
void slaed9(blasint* k, blasint* kstart, blasint* kstop, blasint* n, float* d, float* q, blasint* ldq, float* rho, float* dlambda, float* w, float* s, blasint* lds, blasint* info){ FC_GLOBAL(slaed9,SLAED9)((void*) k, (void*) kstart, (void*) kstop, (void*) n, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) w, (void*) s, (void*) lds, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaed9_(void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info)
{
	void (*fn) (void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info);

	*(void **) & fn = current_backend->lapack.slaed9.f77_blas_function; 

		fn((void*) k, (void*) kstart, (void*) kstop, (void*) n, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) w, (void*) s, (void*) lds, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slaed9(void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info) __attribute__((alias("flexiblas_real_slaed9_")));
#else
void flexiblas_real_slaed9(void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info){flexiblas_real_slaed9_((void*) k, (void*) kstart, (void*) kstop, (void*) n, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) w, (void*) s, (void*) lds, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaed9_(void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info)
{
	void (*fn) (void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info);
	void (*fn_hook) (void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info);

	*(void **) &fn      = current_backend->lapack.slaed9.f77_blas_function; 

    hook_pos_slaed9 ++;
    if( hook_pos_slaed9 < __flexiblas_hooks->slaed9.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaed9.f77_hook_function[hook_pos_slaed9];
        fn_hook((void*) k, (void*) kstart, (void*) kstop, (void*) n, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) w, (void*) s, (void*) lds, (void*) info);
    } else {
        hook_pos_slaed9 = 0;
		fn((void*) k, (void*) kstart, (void*) kstop, (void*) n, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) w, (void*) s, (void*) lds, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slaed9(void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info) __attribute__((alias("flexiblas_chain_slaed9_")));
#else
void flexiblas_chain_slaed9(void* k, void* kstart, void* kstop, void* n, void* d, void* q, void* ldq, void* rho, void* dlambda, void* w, void* s, void* lds, void* info){flexiblas_chain_slaed9_((void*) k, (void*) kstart, (void*) kstop, (void*) n, (void*) d, (void*) q, (void*) ldq, (void*) rho, (void*) dlambda, (void*) w, (void*) s, (void*) lds, (void*) info);}
#endif



