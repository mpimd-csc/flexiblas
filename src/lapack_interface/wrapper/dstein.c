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

#include "fortran_mangle.h"

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



static TLS_STORE uint8_t hook_pos_dstein = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dstein,DSTEIN)(blasint* n, double* d, double* e, blasint* m, double* w, blasint* iblock, blasint* isplit, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info)
#else
void FC_GLOBAL(dstein,DSTEIN)(blasint* n, double* d, double* e, blasint* m, double* w, blasint* iblock, blasint* isplit, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info)
#endif
{
	void (*fn) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dstein.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dstein.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); 
		return;
	} else {
		hook_pos_dstein = 0;
		fn_hook((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dstein_(blasint* n, double* d, double* e, blasint* m, double* w, blasint* iblock, blasint* isplit, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dstein,DSTEIN)))));
#else
#ifndef __APPLE__
void dstein(blasint* n, double* d, double* e, blasint* m, double* w, blasint* iblock, blasint* isplit, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dstein,DSTEIN)))));
#else
void dstein(blasint* n, double* d, double* e, blasint* m, double* w, blasint* iblock, blasint* isplit, double* z, blasint* ldz, double* work, blasint* iwork, blasint* ifail, blasint* info){ FC_GLOBAL(dstein,DSTEIN)((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dstein_(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);

	fn = current_backend->lapack.dstein.f77_blas_function; 

		fn((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dstein(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info) __attribute__((alias("flexiblas_real_dstein_")));
#else
void flexiblas_real_dstein(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info){flexiblas_real_dstein_((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dstein_(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info)
{
	void (*fn) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);
	void (*fn_hook) (void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info);

	fn      = current_backend->lapack.dstein.f77_blas_function; 

    hook_pos_dstein ++;
    if( hook_pos_dstein < __flexiblas_hooks->dstein.nhook) {
        fn_hook = __flexiblas_hooks->dstein.f77_hook_function[hook_pos_dstein];
        fn_hook((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info);
    } else {
        hook_pos_dstein = 0;
		fn((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dstein(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info) __attribute__((alias("flexiblas_chain_dstein_")));
#else
void flexiblas_chain_dstein(void* n, void* d, void* e, void* m, void* w, void* iblock, void* isplit, void* z, void* ldz, void* work, void* iwork, void* ifail, void* info){flexiblas_chain_dstein_((void*) n, (void*) d, (void*) e, (void*) m, (void*) w, (void*) iblock, (void*) isplit, (void*) z, (void*) ldz, (void*) work, (void*) iwork, (void*) ifail, (void*) info);}
#endif



