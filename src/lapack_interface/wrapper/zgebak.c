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



static TLS_STORE uint8_t hook_pos_zgebak = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgebak,ZGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info)
#else
void FC_GLOBAL(zgebak,ZGEBAK)(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info)
#endif
{
	void (*fn) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info);
	void (*fn_hook) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zgebak.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zgebak.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info); 
		return;
	} else {
		hook_pos_zgebak = 0;
		fn_hook((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgebak_(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgebak,ZGEBAK)))));
#else
#ifndef __APPLE__
void zgebak(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(zgebak,ZGEBAK)))));
#else
void zgebak(char* job, char* side, blasint* n, blasint* ilo, blasint* ihi, double* scale, blasint* m, double complex* v, blasint* ldv, blasint* info){ FC_GLOBAL(zgebak,ZGEBAK)((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgebak_(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info)
{
	void (*fn) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info);

	*(void **) & fn = current_backend->lapack.zgebak.f77_blas_function; 

		fn((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zgebak(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info) __attribute__((alias("flexiblas_real_zgebak_")));
#else
void flexiblas_real_zgebak(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info){flexiblas_real_zgebak_((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgebak_(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info)
{
	void (*fn) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info);
	void (*fn_hook) (void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info);

	*(void **) &fn      = current_backend->lapack.zgebak.f77_blas_function; 

    hook_pos_zgebak ++;
    if( hook_pos_zgebak < __flexiblas_hooks->zgebak.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgebak.f77_hook_function[hook_pos_zgebak];
        fn_hook((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info);
    } else {
        hook_pos_zgebak = 0;
		fn((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zgebak(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info) __attribute__((alias("flexiblas_chain_zgebak_")));
#else
void flexiblas_chain_zgebak(void* job, void* side, void* n, void* ilo, void* ihi, void* scale, void* m, void* v, void* ldv, void* info){flexiblas_chain_zgebak_((void*) job, (void*) side, (void*) n, (void*) ilo, (void*) ihi, (void*) scale, (void*) m, (void*) v, (void*) ldv, (void*) info);}
#endif



