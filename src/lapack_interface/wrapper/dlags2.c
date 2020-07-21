/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
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
 * Copyright (C) Martin Koehler, 2015-2017
 */
 /* This file it automatically generated. Please do not edit. */
 /* Generated: Fri Jul 10 16:25:23 2020 */
        
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



static TLS_STORE uint8_t hook_pos_dlags2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlags2,DLAGS2)(blasint* upper, double* a1, double* a2, double* a3, double* b1, double* b2, double* b3, double* csu, double* snu, double* csv, double* snv, double* csq, double* snq)
#else
void FC_GLOBAL(dlags2,DLAGS2)(blasint* upper, double* a1, double* a2, double* a3, double* b1, double* b2, double* b3, double* csu, double* snu, double* csv, double* snv, double* csq, double* snq)
#endif
{
	void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);
	void (*fn_hook) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.dlags2.f77_blas_function; 
	fn_hook = __flexiblas_hooks->dlags2.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); 
		return;
	} else {
		hook_pos_dlags2 = 0;
		fn_hook((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlags2_(blasint* upper, double* a1, double* a2, double* a3, double* b1, double* b2, double* b3, double* csu, double* snu, double* csv, double* snv, double* csq, double* snq) __attribute__((alias(MTS(FC_GLOBAL(dlags2,DLAGS2)))));
#else
void dlags2(blasint* upper, double* a1, double* a2, double* a3, double* b1, double* b2, double* b3, double* csu, double* snu, double* csv, double* snv, double* csq, double* snq) __attribute__((alias(MTS(FC_GLOBAL(dlags2,DLAGS2)))));
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlags2_(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)
{
	void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

	fn = current_backend->lapack.dlags2.f77_blas_function; 

		fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); 

	return;
}

void flexiblas_real_dlags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)  __attribute__((alias("flexiblas_real_dlags2_")));





/* Chainloader for Hooks */


void flexiblas_chain_dlags2_(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)
{
	void (*fn) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);
	void (*fn_hook) (void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq);

	fn      = current_backend->lapack.dlags2.f77_blas_function; 

    hook_pos_dlags2 ++;
    if( hook_pos_dlags2 < __flexiblas_hooks->dlags2.nhook) {
        fn_hook = __flexiblas_hooks->dlags2.f77_hook_function[hook_pos_dlags2];
        fn_hook((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq);
    } else {
        hook_pos_dlags2 = 0;
		fn((void*) upper, (void*) a1, (void*) a2, (void*) a3, (void*) b1, (void*) b2, (void*) b3, (void*) csu, (void*) snu, (void*) csv, (void*) snv, (void*) csq, (void*) snq); 
	}
	return;
}

void flexiblas_chain_dlags2(void* upper, void* a1, void* a2, void* a3, void* b1, void* b2, void* b3, void* csu, void* snu, void* csv, void* snv, void* csq, void* snq)  __attribute__((alias("flexiblas_chain_dlags2_")));




