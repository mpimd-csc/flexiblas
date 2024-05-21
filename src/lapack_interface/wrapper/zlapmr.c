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


static TLS_STORE uint8_t hook_pos_zlapmr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlapmr,ZLAPMR)(blasint* forwrd, blasint* m, blasint* n, double complex* x, blasint* ldx, blasint* k)
#else
void FC_GLOBAL(zlapmr,ZLAPMR)(blasint* forwrd, blasint* m, blasint* n, double complex* x, blasint* ldx, blasint* k)
#endif
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);
	void (*fn_hook) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zlapmr.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zlapmr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 
		return;
	} else {
		hook_pos_zlapmr = 0;
		fn_hook((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlapmr_(blasint* forwrd, blasint* m, blasint* n, double complex* x, blasint* ldx, blasint* k) __attribute__((alias(MTS(FC_GLOBAL(zlapmr,ZLAPMR)))));
#else
#ifndef __APPLE__
void zlapmr(blasint* forwrd, blasint* m, blasint* n, double complex* x, blasint* ldx, blasint* k) __attribute__((alias(MTS(FC_GLOBAL(zlapmr,ZLAPMR)))));
#else
void zlapmr(blasint* forwrd, blasint* m, blasint* n, double complex* x, blasint* ldx, blasint* k){ FC_GLOBAL(zlapmr,ZLAPMR)((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlapmr_(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

	*(void **) & fn = current_backend->lapack.zlapmr.f77_blas_function; 

		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zlapmr(void* forwrd, void* m, void* n, void* x, void* ldx, void* k) __attribute__((alias("flexiblas_real_zlapmr_")));
#else
void flexiblas_real_zlapmr(void* forwrd, void* m, void* n, void* x, void* ldx, void* k){flexiblas_real_zlapmr_((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlapmr_(void* forwrd, void* m, void* n, void* x, void* ldx, void* k)
{
	void (*fn) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);
	void (*fn_hook) (void* forwrd, void* m, void* n, void* x, void* ldx, void* k);

	*(void **) &fn      = current_backend->lapack.zlapmr.f77_blas_function; 

    hook_pos_zlapmr ++;
    if( hook_pos_zlapmr < __flexiblas_hooks->zlapmr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlapmr.f77_hook_function[hook_pos_zlapmr];
        fn_hook((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);
    } else {
        hook_pos_zlapmr = 0;
		fn((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zlapmr(void* forwrd, void* m, void* n, void* x, void* ldx, void* k) __attribute__((alias("flexiblas_chain_zlapmr_")));
#else
void flexiblas_chain_zlapmr(void* forwrd, void* m, void* n, void* x, void* ldx, void* k){flexiblas_chain_zlapmr_((void*) forwrd, (void*) m, (void*) n, (void*) x, (void*) ldx, (void*) k);}
#endif



