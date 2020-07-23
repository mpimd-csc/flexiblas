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
 * Copyright (C) Martin Koehler, 2015-2020
 */
        
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



static TLS_STORE uint8_t hook_pos_slantp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
float FC_GLOBAL(slantp,SLANTP)(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* work)
#else
float FC_GLOBAL(slantp,SLANTP)(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* work)
#endif
{
	float (*fn) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work);
	float (*fn_hook) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work);
	float ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	fn = current_backend->lapack.slantp.f77_blas_function; 
	fn_hook = __flexiblas_hooks->slantp.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work); 
		return ret; 
	} else {
		hook_pos_slantp = 0;
		ret=fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
float slantp_(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* work) __attribute__((alias(MTS(FC_GLOBAL(slantp,SLANTP)))));
#else
float slantp(char* norm, char* uplo, char* diag, blasint* n, float* ap, float* work) __attribute__((alias(MTS(FC_GLOBAL(slantp,SLANTP)))));
#endif




/* Real Implementation for Hooks */


float flexiblas_real_slantp_(void* norm, void* uplo, void* diag, void* n, void* ap, void* work)
{
	float (*fn) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work);
	float ret;

	fn = current_backend->lapack.slantp.f77_blas_function; 

		ret = fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work); 

	return ret ;
}

float flexiblas_real_slantp(void* norm, void* uplo, void* diag, void* n, void* ap, void* work)  __attribute__((alias("flexiblas_real_slantp_")));





/* Chainloader for Hooks */


float flexiblas_chain_slantp_(void* norm, void* uplo, void* diag, void* n, void* ap, void* work)
{
	float (*fn) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work);
	float (*fn_hook) (void* norm, void* uplo, void* diag, void* n, void* ap, void* work);
	float ret;

	fn      = current_backend->lapack.slantp.f77_blas_function; 

    hook_pos_slantp ++;
    if( hook_pos_slantp < __flexiblas_hooks->slantp.nhook) {
        fn_hook = __flexiblas_hooks->slantp.f77_hook_function[hook_pos_slantp];
        ret = fn_hook((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work);
    } else {
        hook_pos_slantp = 0;
		ret = fn((void*) norm, (void*) uplo, (void*) diag, (void*) n, (void*) ap, (void*) work); 
	}
	return ret ;
}

float flexiblas_chain_slantp(void* norm, void* uplo, void* diag, void* n, void* ap, void* work)  __attribute__((alias("flexiblas_chain_slantp_")));




