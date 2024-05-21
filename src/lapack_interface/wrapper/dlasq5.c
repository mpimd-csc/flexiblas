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


static TLS_STORE uint8_t hook_pos_dlasq5 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlasq5,DLASQ5)(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blasint* ieee, double* eps)
#else
void FC_GLOBAL(dlasq5,DLASQ5)(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blasint* ieee, double* eps)
#endif
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlasq5.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlasq5.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); 
		return;
	} else {
		hook_pos_dlasq5 = 0;
		fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlasq5_(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blasint* ieee, double* eps) __attribute__((alias(MTS(FC_GLOBAL(dlasq5,DLASQ5)))));
#else
#ifndef __APPLE__
void dlasq5(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blasint* ieee, double* eps) __attribute__((alias(MTS(FC_GLOBAL(dlasq5,DLASQ5)))));
#else
void dlasq5(blasint* i0, blasint* n0, double* z, blasint* pp, double* tau, double* sigma, double* dmin, double* dmin1, double* dmin2, double* dn, double* dnm1, double* dnm2, blasint* ieee, double* eps){ FC_GLOBAL(dlasq5,DLASQ5)((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlasq5_(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);

	*(void **) & fn = current_backend->lapack.dlasq5.f77_blas_function; 

		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps) __attribute__((alias("flexiblas_real_dlasq5_")));
#else
void flexiblas_real_dlasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps){flexiblas_real_dlasq5_((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlasq5_(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps)
{
	void (*fn) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);
	void (*fn_hook) (void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps);

	*(void **) &fn      = current_backend->lapack.dlasq5.f77_blas_function; 

    hook_pos_dlasq5 ++;
    if( hook_pos_dlasq5 < __flexiblas_hooks->dlasq5.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlasq5.f77_hook_function[hook_pos_dlasq5];
        fn_hook((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);
    } else {
        hook_pos_dlasq5 = 0;
		fn((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps) __attribute__((alias("flexiblas_chain_dlasq5_")));
#else
void flexiblas_chain_dlasq5(void* i0, void* n0, void* z, void* pp, void* tau, void* sigma, void* dmin, void* dmin1, void* dmin2, void* dn, void* dnm1, void* dnm2, void* ieee, void* eps){flexiblas_chain_dlasq5_((void*) i0, (void*) n0, (void*) z, (void*) pp, (void*) tau, (void*) sigma, (void*) dmin, (void*) dmin1, (void*) dmin2, (void*) dn, (void*) dnm1, (void*) dnm2, (void*) ieee, (void*) eps);}
#endif



