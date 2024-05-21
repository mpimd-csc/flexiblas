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


static TLS_STORE uint8_t hook_pos_cstein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(cstein,CSTEIN)(blasint* n, float* d, float* e, blasint* m, float* w, blasint* iblock, blasint* isplit, float complex* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info)
#else
void FC_GLOBAL(cstein,CSTEIN)(blasint* n, float* d, float* e, blasint* m, float* w, blasint* iblock, blasint* isplit, float complex* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info)
#endif
{
	void (*fn) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.cstein.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->cstein.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); 
		return;
	} else {
		hook_pos_cstein = 0;
		fn_hook((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void cstein_(blasint* n, float* d, float* e, blasint* m, float* w, blasint* iblock, blasint* isplit, float complex* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cstein,CSTEIN)))));
#else
#ifndef __APPLE__
void cstein(blasint* n, float* d, float* e, blasint* m, float* w, blasint* iblock, blasint* isplit, float complex* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(cstein,CSTEIN)))));
#else
void cstein(blasint* n, float* d, float* e, blasint* m, float* w, blasint* iblock, blasint* isplit, float complex* z, blasint* ldz, float* work, blasint* iwork, blasint* ifail, blasint* info){ FC_GLOBAL(cstein,CSTEIN)((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_cstein_(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);

	*(void **) & fn = current_backend->lapack.cstein.f77_blas_function; 

		fn((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_cstein(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info) __attribute__((alias("flexiblas_real_cstein_")));
#else
void flexiblas_real_cstein(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info){flexiblas_real_cstein_((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_cstein_(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);

	*(void **) &fn      = current_backend->lapack.cstein.f77_blas_function; 

    hook_pos_cstein ++;
    if( hook_pos_cstein < __flexiblas_hooks->cstein.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->cstein.f77_hook_function[hook_pos_cstein];
        fn_hook((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info);
    } else {
        hook_pos_cstein = 0;
		fn((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_cstein(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info) __attribute__((alias("flexiblas_chain_cstein_")));
#else
void flexiblas_chain_cstein(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info){flexiblas_chain_cstein_((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info);}
#endif



