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


static TLS_STORE uint8_t hook_pos_slahqr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slahqr,SLAHQR)(blasint* wantt, blasint* wantz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* wr, float* wi, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, blasint* info)
#else
void FC_GLOBAL(slahqr,SLAHQR)(blasint* wantt, blasint* wantz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* wr, float* wi, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, blasint* info)
#endif
{
	void (*fn) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);
	void (*fn_hook) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slahqr.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slahqr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info); 
		return;
	} else {
		hook_pos_slahqr = 0;
		fn_hook((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slahqr_(blasint* wantt, blasint* wantz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* wr, float* wi, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slahqr,SLAHQR)))));
#else
#ifndef __APPLE__
void slahqr(blasint* wantt, blasint* wantz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* wr, float* wi, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slahqr,SLAHQR)))));
#else
void slahqr(blasint* wantt, blasint* wantz, blasint* n, blasint* ilo, blasint* ihi, float* h, blasint* ldh, float* wr, float* wi, blasint* iloz, blasint* ihiz, float* z, blasint* ldz, blasint* info){ FC_GLOBAL(slahqr,SLAHQR)((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slahqr_(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info)
{
	void (*fn) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);

	*(void **) & fn = current_backend->lapack.slahqr.f77_blas_function; 

		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slahqr(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info) __attribute__((alias("flexiblas_real_slahqr_")));
#else
void flexiblas_real_slahqr(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info){flexiblas_real_slahqr_((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slahqr_(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info)
{
	void (*fn) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);
	void (*fn_hook) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);

	*(void **) &fn      = current_backend->lapack.slahqr.f77_blas_function; 

    hook_pos_slahqr ++;
    if( hook_pos_slahqr < __flexiblas_hooks->slahqr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slahqr.f77_hook_function[hook_pos_slahqr];
        fn_hook((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info);
    } else {
        hook_pos_slahqr = 0;
		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slahqr(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info) __attribute__((alias("flexiblas_chain_slahqr_")));
#else
void flexiblas_chain_slahqr(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info){flexiblas_chain_slahqr_((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info);}
#endif



