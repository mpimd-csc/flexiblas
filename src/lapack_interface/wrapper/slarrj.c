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
 /* Generated: Wed Mar 28 11:20:04 2018 */
        
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



static TLS_STORE uint8_t hook_pos_slarrj = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slarrj,SLARRJ)(blasint* n, float* d, float* e2, blasint* ifirst, blasint* ilast, float* rtol, blasint* offset, float* w, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* info)
#else
void FC_GLOBAL(slarrj,SLARRJ)(blasint* n, float* d, float* e2, blasint* ifirst, blasint* ilast, float* rtol, blasint* offset, float* w, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* info)
#endif
{
	void (*fn) (void* n, void* d, void* e2, void* ifirst, void* ilast, void* rtol, void* offset, void* w, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* info);
	void (*fn_hook) (void* n, void* d, void* e2, void* ifirst, void* ilast, void* rtol, void* offset, void* w, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slarrj.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slarrj.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) d, (void*) e2, (void*) ifirst, (void*) ilast, (void*) rtol, (void*) offset, (void*) w, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) info); 
		return;
	} else {
		hook_pos_slarrj = 0;
		fn_hook((void*) n, (void*) d, (void*) e2, (void*) ifirst, (void*) ilast, (void*) rtol, (void*) offset, (void*) w, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slarrj_(blasint* n, float* d, float* e2, blasint* ifirst, blasint* ilast, float* rtol, blasint* offset, float* w, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrj,SLARRJ)))));
#else
void slarrj(blasint* n, float* d, float* e2, blasint* ifirst, blasint* ilast, float* rtol, blasint* offset, float* w, float* werr, float* work, blasint* iwork, float* pivmin, float* spdiam, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slarrj,SLARRJ)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slarrj_(void* n, void* d, void* e2, void* ifirst, void* ilast, void* rtol, void* offset, void* w, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* info)
{
	void (*fn) (void* n, void* d, void* e2, void* ifirst, void* ilast, void* rtol, void* offset, void* w, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* info);

	fn = current_backend->lapack.slarrj.f77_blas_function; 

		fn((void*) n, (void*) d, (void*) e2, (void*) ifirst, (void*) ilast, (void*) rtol, (void*) offset, (void*) w, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) info); 

	return;
}

void flexiblas_real_slarrj(void* n, void* d, void* e2, void* ifirst, void* ilast, void* rtol, void* offset, void* w, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* info)  __attribute__((alias("flexiblas_real_slarrj_")));





/* Chainloader for Hooks */


void flexiblas_chain_slarrj_(void* n, void* d, void* e2, void* ifirst, void* ilast, void* rtol, void* offset, void* w, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* info)
{
	void (*fn) (void* n, void* d, void* e2, void* ifirst, void* ilast, void* rtol, void* offset, void* w, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* info);
	void (*fn_hook) (void* n, void* d, void* e2, void* ifirst, void* ilast, void* rtol, void* offset, void* w, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* info);

	fn      = current_backend->lapack.slarrj.f77_blas_function; 

    hook_pos_slarrj ++;
    if( hook_pos_slarrj < __flexiblas_hooks->slarrj.nhook) {
        fn_hook = __flexiblas_hooks->slarrj.f77_hook_function[hook_pos_slarrj];
        fn_hook((void*) n, (void*) d, (void*) e2, (void*) ifirst, (void*) ilast, (void*) rtol, (void*) offset, (void*) w, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) info);
    } else {
        hook_pos_slarrj = 0;
		fn((void*) n, (void*) d, (void*) e2, (void*) ifirst, (void*) ilast, (void*) rtol, (void*) offset, (void*) w, (void*) werr, (void*) work, (void*) iwork, (void*) pivmin, (void*) spdiam, (void*) info); 
	}
	return;
}

void flexiblas_chain_slarrj(void* n, void* d, void* e2, void* ifirst, void* ilast, void* rtol, void* offset, void* w, void* werr, void* work, void* iwork, void* pivmin, void* spdiam, void* info)  __attribute__((alias("flexiblas_chain_slarrj_")));




