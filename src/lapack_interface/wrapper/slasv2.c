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



static TLS_STORE uint8_t hook_pos_slasv2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slasv2,SLASV2)(float* f, float* g, float* h, float* ssmin, float* ssmax, float* snr, float* csr, float* snl, float* csl)
#else
void FC_GLOBAL(slasv2,SLASV2)(float* f, float* g, float* h, float* ssmin, float* ssmax, float* snr, float* csr, float* snl, float* csl)
#endif
{
	void (*fn) (void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl);
	void (*fn_hook) (void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slasv2.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slasv2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax, (void*) snr, (void*) csr, (void*) snl, (void*) csl); 
		return;
	} else {
		hook_pos_slasv2 = 0;
		fn_hook((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax, (void*) snr, (void*) csr, (void*) snl, (void*) csl);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slasv2_(float* f, float* g, float* h, float* ssmin, float* ssmax, float* snr, float* csr, float* snl, float* csl) __attribute__((alias(MTS(FC_GLOBAL(slasv2,SLASV2)))));
#else
#ifndef __APPLE__
void slasv2(float* f, float* g, float* h, float* ssmin, float* ssmax, float* snr, float* csr, float* snl, float* csl) __attribute__((alias(MTS(FC_GLOBAL(slasv2,SLASV2)))));
#else
void slasv2(float* f, float* g, float* h, float* ssmin, float* ssmax, float* snr, float* csr, float* snl, float* csl){ FC_GLOBAL(slasv2,SLASV2)((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax, (void*) snr, (void*) csr, (void*) snl, (void*) csl); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slasv2_(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl)
{
	void (*fn) (void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl);

	*(void **) & fn = current_backend->lapack.slasv2.f77_blas_function; 

		fn((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax, (void*) snr, (void*) csr, (void*) snl, (void*) csl); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slasv2(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl) __attribute__((alias("flexiblas_real_slasv2_")));
#else
void flexiblas_real_slasv2(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl){flexiblas_real_slasv2_((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax, (void*) snr, (void*) csr, (void*) snl, (void*) csl);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slasv2_(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl)
{
	void (*fn) (void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl);
	void (*fn_hook) (void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl);

	*(void **) &fn      = current_backend->lapack.slasv2.f77_blas_function; 

    hook_pos_slasv2 ++;
    if( hook_pos_slasv2 < __flexiblas_hooks->slasv2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slasv2.f77_hook_function[hook_pos_slasv2];
        fn_hook((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax, (void*) snr, (void*) csr, (void*) snl, (void*) csl);
    } else {
        hook_pos_slasv2 = 0;
		fn((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax, (void*) snr, (void*) csr, (void*) snl, (void*) csl); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slasv2(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl) __attribute__((alias("flexiblas_chain_slasv2_")));
#else
void flexiblas_chain_slasv2(void* f, void* g, void* h, void* ssmin, void* ssmax, void* snr, void* csr, void* snl, void* csl){flexiblas_chain_slasv2_((void*) f, (void*) g, (void*) h, (void*) ssmin, (void*) ssmax, (void*) snr, (void*) csr, (void*) snl, (void*) csl);}
#endif



