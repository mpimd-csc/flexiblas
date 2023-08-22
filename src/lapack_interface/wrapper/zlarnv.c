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
 * Copyright (C) Martin Koehler, 2013-2023
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



static TLS_STORE uint8_t hook_pos_zlarnv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zlarnv,ZLARNV)(blasint* idist, blasint* iseed, blasint* n, double complex* x)
#else
void FC_GLOBAL(zlarnv,ZLARNV)(blasint* idist, blasint* iseed, blasint* n, double complex* x)
#endif
{
	void (*fn) (void* idist, void* iseed, void* n, void* x);
	void (*fn_hook) (void* idist, void* iseed, void* n, void* x);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zlarnv.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zlarnv.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) idist, (void*) iseed, (void*) n, (void*) x); 
		return;
	} else {
		hook_pos_zlarnv = 0;
		fn_hook((void*) idist, (void*) iseed, (void*) n, (void*) x);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zlarnv_(blasint* idist, blasint* iseed, blasint* n, double complex* x) __attribute__((alias(MTS(FC_GLOBAL(zlarnv,ZLARNV)))));
#else
#ifndef __APPLE__
void zlarnv(blasint* idist, blasint* iseed, blasint* n, double complex* x) __attribute__((alias(MTS(FC_GLOBAL(zlarnv,ZLARNV)))));
#else
void zlarnv(blasint* idist, blasint* iseed, blasint* n, double complex* x){ FC_GLOBAL(zlarnv,ZLARNV)((void*) idist, (void*) iseed, (void*) n, (void*) x); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zlarnv_(void* idist, void* iseed, void* n, void* x)
{
	void (*fn) (void* idist, void* iseed, void* n, void* x);

	*(void **) & fn = current_backend->lapack.zlarnv.f77_blas_function; 

		fn((void*) idist, (void*) iseed, (void*) n, (void*) x); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zlarnv(void* idist, void* iseed, void* n, void* x) __attribute__((alias("flexiblas_real_zlarnv_")));
#else
void flexiblas_real_zlarnv(void* idist, void* iseed, void* n, void* x){flexiblas_real_zlarnv_((void*) idist, (void*) iseed, (void*) n, (void*) x);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zlarnv_(void* idist, void* iseed, void* n, void* x)
{
	void (*fn) (void* idist, void* iseed, void* n, void* x);
	void (*fn_hook) (void* idist, void* iseed, void* n, void* x);

	*(void **) &fn      = current_backend->lapack.zlarnv.f77_blas_function; 

    hook_pos_zlarnv ++;
    if( hook_pos_zlarnv < __flexiblas_hooks->zlarnv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zlarnv.f77_hook_function[hook_pos_zlarnv];
        fn_hook((void*) idist, (void*) iseed, (void*) n, (void*) x);
    } else {
        hook_pos_zlarnv = 0;
		fn((void*) idist, (void*) iseed, (void*) n, (void*) x); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zlarnv(void* idist, void* iseed, void* n, void* x) __attribute__((alias("flexiblas_chain_zlarnv_")));
#else
void flexiblas_chain_zlarnv(void* idist, void* iseed, void* n, void* x){flexiblas_chain_zlarnv_((void*) idist, (void*) iseed, (void*) n, (void*) x);}
#endif



