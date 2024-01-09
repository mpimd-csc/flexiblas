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



static TLS_STORE uint8_t hook_pos_dlas2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlas2,DLAS2)(double* f, double* g, double* h, double* ssmin, double* ssmax)
#else
void FC_GLOBAL(dlas2,DLAS2)(double* f, double* g, double* h, double* ssmin, double* ssmax)
#endif
{
	void (*fn) (void* f, void* g, void* h, void* ssmin, void* ssmax);
	void (*fn_hook) (void* f, void* g, void* h, void* ssmin, void* ssmax);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlas2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlas2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax); 
		return;
	} else {
		hook_pos_dlas2 = 0;
		fn_hook((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlas2_(double* f, double* g, double* h, double* ssmin, double* ssmax) __attribute__((alias(MTS(FC_GLOBAL(dlas2,DLAS2)))));
#else
#ifndef __APPLE__
void dlas2(double* f, double* g, double* h, double* ssmin, double* ssmax) __attribute__((alias(MTS(FC_GLOBAL(dlas2,DLAS2)))));
#else
void dlas2(double* f, double* g, double* h, double* ssmin, double* ssmax){ FC_GLOBAL(dlas2,DLAS2)((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlas2_(void* f, void* g, void* h, void* ssmin, void* ssmax)
{
	void (*fn) (void* f, void* g, void* h, void* ssmin, void* ssmax);

	*(void **) & fn = current_backend->lapack.dlas2.f77_blas_function; 

		fn((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlas2(void* f, void* g, void* h, void* ssmin, void* ssmax) __attribute__((alias("flexiblas_real_dlas2_")));
#else
void flexiblas_real_dlas2(void* f, void* g, void* h, void* ssmin, void* ssmax){flexiblas_real_dlas2_((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlas2_(void* f, void* g, void* h, void* ssmin, void* ssmax)
{
	void (*fn) (void* f, void* g, void* h, void* ssmin, void* ssmax);
	void (*fn_hook) (void* f, void* g, void* h, void* ssmin, void* ssmax);

	*(void **) &fn      = current_backend->lapack.dlas2.f77_blas_function; 

    hook_pos_dlas2 ++;
    if( hook_pos_dlas2 < __flexiblas_hooks->dlas2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlas2.f77_hook_function[hook_pos_dlas2];
        fn_hook((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax);
    } else {
        hook_pos_dlas2 = 0;
		fn((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlas2(void* f, void* g, void* h, void* ssmin, void* ssmax) __attribute__((alias("flexiblas_chain_dlas2_")));
#else
void flexiblas_chain_dlas2(void* f, void* g, void* h, void* ssmin, void* ssmax){flexiblas_chain_dlas2_((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax);}
#endif



