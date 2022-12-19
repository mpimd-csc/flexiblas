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
 * Linking FlexiBLAS statically or dynamically with other modules is making a combined
 * work based on FlexiBLAS. Thus, the terms and conditions of the GNU General
 * Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * Copyright (C) Martin Koehler, 2013-2022
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



static TLS_STORE uint8_t hook_pos_clanht = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(clanht,CLANHT)(char* norm, blasint* n, float* d, float complex* e)
#else
float FC_GLOBAL(clanht,CLANHT)(char* norm, blasint* n, float* d, float complex* e)
#endif
{
	float (*fn) (void* norm, void* n, void* d, void* e);
	float (*fn_hook) (void* norm, void* n, void* d, void* e);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.clanht.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->clanht.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) n, (void*) d, (void*) e); 
		return ret; 
	} else {
		hook_pos_clanht = 0;
		ret=fn_hook((void*) norm, (void*) n, (void*) d, (void*) e);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float clanht_(char* norm, blasint* n, float* d, float complex* e) __attribute__((alias(MTS(FC_GLOBAL(clanht,CLANHT)))));
#else
#ifndef __APPLE__
float clanht(char* norm, blasint* n, float* d, float complex* e) __attribute__((alias(MTS(FC_GLOBAL(clanht,CLANHT)))));
#else
float clanht(char* norm, blasint* n, float* d, float complex* e){ return FC_GLOBAL(clanht,CLANHT)((void*) norm, (void*) n, (void*) d, (void*) e); }
#endif
#endif




/* Real Implementation for Hooks */


float flexiblas_real_clanht_(void* norm, void* n, void* d, void* e)
{
	float (*fn) (void* norm, void* n, void* d, void* e);
	float ret;

	*(void **) & fn = current_backend->lapack.clanht.f77_blas_function; 

		ret = fn((void*) norm, (void*) n, (void*) d, (void*) e); 

	return ret ;
}
#ifndef __APPLE__
float flexiblas_real_clanht(void* norm, void* n, void* d, void* e) __attribute__((alias("flexiblas_real_clanht_")));
#else
float flexiblas_real_clanht(void* norm, void* n, void* d, void* e){return flexiblas_real_clanht_((void*) norm, (void*) n, (void*) d, (void*) e);}
#endif




/* Chainloader for Hooks */


float flexiblas_chain_clanht_(void* norm, void* n, void* d, void* e)
{
	float (*fn) (void* norm, void* n, void* d, void* e);
	float (*fn_hook) (void* norm, void* n, void* d, void* e);
	float ret;

	*(void **) &fn      = current_backend->lapack.clanht.f77_blas_function; 

    hook_pos_clanht ++;
    if( hook_pos_clanht < __flexiblas_hooks->clanht.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clanht.f77_hook_function[hook_pos_clanht];
        ret = fn_hook((void*) norm, (void*) n, (void*) d, (void*) e);
    } else {
        hook_pos_clanht = 0;
		ret = fn((void*) norm, (void*) n, (void*) d, (void*) e); 
	}
	return ret ;
}
#ifndef __APPLE__
float flexiblas_chain_clanht(void* norm, void* n, void* d, void* e) __attribute__((alias("flexiblas_chain_clanht_")));
#else
float flexiblas_chain_clanht(void* norm, void* n, void* d, void* e){return flexiblas_chain_clanht_((void*) norm, (void*) n, (void*) d, (void*) e);}
#endif



