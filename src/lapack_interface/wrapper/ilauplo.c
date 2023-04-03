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



static TLS_STORE uint8_t hook_pos_ilauplo = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(ilauplo,ILAUPLO)(char* uplo)
#else
int FC_GLOBAL(ilauplo,ILAUPLO)(char* uplo)
#endif
{
	blasint (*fn) (void* uplo);
	blasint (*fn_hook) (void* uplo);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ilauplo.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ilauplo.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) uplo); 
		return ret; 
	} else {
		hook_pos_ilauplo = 0;
		ret=fn_hook((void*) uplo);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int ilauplo_(char* uplo) __attribute__((alias(MTS(FC_GLOBAL(ilauplo,ILAUPLO)))));
#else
#ifndef __APPLE__
int ilauplo(char* uplo) __attribute__((alias(MTS(FC_GLOBAL(ilauplo,ILAUPLO)))));
#else
int ilauplo(char* uplo){ return FC_GLOBAL(ilauplo,ILAUPLO)((void*) uplo); }
#endif
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilauplo_(void* uplo)
{
	blasint (*fn) (void* uplo);
	blasint ret;

	*(void **) & fn = current_backend->lapack.ilauplo.f77_blas_function; 

		ret = fn((void*) uplo); 

	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_real_ilauplo(void* uplo) __attribute__((alias("flexiblas_real_ilauplo_")));
#else
blasint flexiblas_real_ilauplo(void* uplo){return flexiblas_real_ilauplo_((void*) uplo);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilauplo_(void* uplo)
{
	blasint (*fn) (void* uplo);
	blasint (*fn_hook) (void* uplo);
	blasint ret;

	*(void **) &fn      = current_backend->lapack.ilauplo.f77_blas_function; 

    hook_pos_ilauplo ++;
    if( hook_pos_ilauplo < __flexiblas_hooks->ilauplo.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilauplo.f77_hook_function[hook_pos_ilauplo];
        ret = fn_hook((void*) uplo);
    } else {
        hook_pos_ilauplo = 0;
		ret = fn((void*) uplo); 
	}
	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilauplo(void* uplo) __attribute__((alias("flexiblas_chain_ilauplo_")));
#else
blasint flexiblas_chain_ilauplo(void* uplo){return flexiblas_chain_ilauplo_((void*) uplo);}
#endif



