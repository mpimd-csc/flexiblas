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



static TLS_STORE uint8_t hook_pos_dlarrc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlarrc,DLARRC)(char* jobt, blasint* n, double* vl, double* vu, double* d, double* e, double* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt)
#else
void FC_GLOBAL(dlarrc,DLARRC)(char* jobt, blasint* n, double* vl, double* vu, double* d, double* e, double* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt)
#endif
{
	void (*fn) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);
	void (*fn_hook) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlarrc.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlarrc.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt); 
		return;
	} else {
		hook_pos_dlarrc = 0;
		fn_hook((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlarrc_(char* jobt, blasint* n, double* vl, double* vu, double* d, double* e, double* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt) __attribute__((alias(MTS(FC_GLOBAL(dlarrc,DLARRC)))));
#else
#ifndef __APPLE__
void dlarrc(char* jobt, blasint* n, double* vl, double* vu, double* d, double* e, double* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt) __attribute__((alias(MTS(FC_GLOBAL(dlarrc,DLARRC)))));
#else
void dlarrc(char* jobt, blasint* n, double* vl, double* vu, double* d, double* e, double* pivmin, blasint* eigcnt, blasint* lcnt, blasint* rcnt, blasint* info, flexiblas_fortran_charlen_t len_jobt){ FC_GLOBAL(dlarrc,DLARRC)((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, (flexiblas_fortran_charlen_t) len_jobt); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlarrc_(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt)
{
	void (*fn) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);

	*(void **) & fn = current_backend->lapack.dlarrc.f77_blas_function; 

		fn((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlarrc(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt) __attribute__((alias("flexiblas_real_dlarrc_")));
#else
void flexiblas_real_dlarrc(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt){flexiblas_real_dlarrc_((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, (flexiblas_fortran_charlen_t) len_jobt);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlarrc_(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt)
{
	void (*fn) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);
	void (*fn_hook) (void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt);

	*(void **) &fn      = current_backend->lapack.dlarrc.f77_blas_function; 

    hook_pos_dlarrc ++;
    if( hook_pos_dlarrc < __flexiblas_hooks->dlarrc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlarrc.f77_hook_function[hook_pos_dlarrc];
        fn_hook((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt);
    } else {
        hook_pos_dlarrc = 0;
		fn((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, ( flexiblas_fortran_charlen_t ) len_jobt); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlarrc(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt) __attribute__((alias("flexiblas_chain_dlarrc_")));
#else
void flexiblas_chain_dlarrc(void* jobt, void* n, void* vl, void* vu, void* d, void* e, void* pivmin, void* eigcnt, void* lcnt, void* rcnt, void* info, flexiblas_fortran_charlen_t len_jobt){flexiblas_chain_dlarrc_((void*) jobt, (void*) n, (void*) vl, (void*) vu, (void*) d, (void*) e, (void*) pivmin, (void*) eigcnt, (void*) lcnt, (void*) rcnt, (void*) info, (flexiblas_fortran_charlen_t) len_jobt);}
#endif



