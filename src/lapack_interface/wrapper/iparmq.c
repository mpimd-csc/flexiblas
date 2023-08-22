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



static TLS_STORE uint8_t hook_pos_iparmq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
int FC_GLOBAL(iparmq,IPARMQ)(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts)
#else
int FC_GLOBAL(iparmq,IPARMQ)(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts)
#endif
{
	blasint (*fn) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts);
	blasint (*fn_hook) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts);
	blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.iparmq.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->iparmq.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( fortran_charlen_t ) len_name, ( fortran_charlen_t ) len_opts); 
		return ret; 
	} else {
		hook_pos_iparmq = 0;
		ret=fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( fortran_charlen_t ) len_name, ( fortran_charlen_t ) len_opts);
		return ret;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
int iparmq_(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(iparmq,IPARMQ)))));
#else
#ifndef __APPLE__
int iparmq(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(iparmq,IPARMQ)))));
#else
int iparmq(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts){ return FC_GLOBAL(iparmq,IPARMQ)((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, (fortran_charlen_t) len_name, (fortran_charlen_t) len_opts); }
#endif
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iparmq_(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts)
{
	blasint (*fn) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts);
	blasint ret;

	*(void **) & fn = current_backend->lapack.iparmq.f77_blas_function; 

		ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( fortran_charlen_t ) len_name, ( fortran_charlen_t ) len_opts); 

	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_real_iparmq(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts) __attribute__((alias("flexiblas_real_iparmq_")));
#else
blasint flexiblas_real_iparmq(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts){return flexiblas_real_iparmq_((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, (fortran_charlen_t) len_name, (fortran_charlen_t) len_opts);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_iparmq_(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts)
{
	blasint (*fn) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts);
	blasint (*fn_hook) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts);
	blasint ret;

	*(void **) &fn      = current_backend->lapack.iparmq.f77_blas_function; 

    hook_pos_iparmq ++;
    if( hook_pos_iparmq < __flexiblas_hooks->iparmq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->iparmq.f77_hook_function[hook_pos_iparmq];
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( fortran_charlen_t )len_name, ( fortran_charlen_t )len_opts);
    } else {
        hook_pos_iparmq = 0;
		ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( fortran_charlen_t ) len_name, ( fortran_charlen_t ) len_opts); 
	}
	return ret ;
}
#ifndef __APPLE__
blasint flexiblas_chain_iparmq(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts) __attribute__((alias("flexiblas_chain_iparmq_")));
#else
blasint flexiblas_chain_iparmq(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, fortran_charlen_t len_name, fortran_charlen_t len_opts){return flexiblas_chain_iparmq_((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, (fortran_charlen_t) len_name, (fortran_charlen_t) len_opts);}
#endif



