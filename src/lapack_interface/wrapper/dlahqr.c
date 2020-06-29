/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Wed Mar 28 11:20:03 2018 */
        
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "fortran_mangle.h"

#include "flexiblas.h"


#ifdef INTEGER8
#define blasint int64_t
#else
#define blasint int
#endif



static TLS_STORE uint8_t hook_pos_dlahqr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlahqr,DLAHQR)(blasint* wantt, blasint* wantz, blasint* n, blasint* ilo, blasint* ihi, double* h, blasint* ldh, double* wr, double* wi, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* info)
#else
void FC_GLOBAL(dlahqr,DLAHQR)(blasint* wantt, blasint* wantz, blasint* n, blasint* ilo, blasint* ihi, double* h, blasint* ldh, double* wr, double* wi, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* info)
#endif
{
	void (*fn) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);
	void (*fn_hook) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlahqr.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlahqr.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info); 
		return;
	} else {
		hook_pos_dlahqr = 0;
		fn_hook((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlahqr_(blasint* wantt, blasint* wantz, blasint* n, blasint* ilo, blasint* ihi, double* h, blasint* ldh, double* wr, double* wi, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlahqr,DLAHQR)))));
#else
void dlahqr(blasint* wantt, blasint* wantz, blasint* n, blasint* ilo, blasint* ihi, double* h, blasint* ldh, double* wr, double* wi, blasint* iloz, blasint* ihiz, double* z, blasint* ldz, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlahqr,DLAHQR)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlahqr_(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info)
{
	void (*fn) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);

	fn = current_backend->lapack.dlahqr.f77_blas_function; 

		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info); 

	return;
}

void flexiblas_real_dlahqr(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info)  __attribute__((alias("flexiblas_real_dlahqr_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlahqr_(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info)
{
	void (*fn) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);
	void (*fn_hook) (void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info);

	fn      = current_backend->lapack.dlahqr.f77_blas_function; 

    hook_pos_dlahqr ++;
    if( hook_pos_dlahqr < __flexiblas_hooks->dlahqr.nhook) {
        fn_hook = __flexiblas_hooks->dlahqr.f77_hook_function[hook_pos_dlahqr];
        fn_hook((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info);
    } else {
        hook_pos_dlahqr = 0;
		fn((void*) wantt, (void*) wantz, (void*) n, (void*) ilo, (void*) ihi, (void*) h, (void*) ldh, (void*) wr, (void*) wi, (void*) iloz, (void*) ihiz, (void*) z, (void*) ldz, (void*) info); 
	}
	return;
}

void flexiblas_chain_dlahqr(void* wantt, void* wantz, void* n, void* ilo, void* ihi, void* h, void* ldh, void* wr, void* wi, void* iloz, void* ihiz, void* z, void* ldz, void* info)  __attribute__((alias("flexiblas_chain_dlahqr_")));




