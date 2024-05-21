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

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_ctpttf = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctpttf,CTPTTF)(char* transr, char* uplo, blasint* n, float complex* ap, float complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(ctpttf,CTPTTF)(char* transr, char* uplo, blasint* n, float complex* ap, float complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#endif
{
	void (*fn) (void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
	void (*fn_hook) (void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ctpttf.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ctpttf.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) transr, (void*) uplo, (void*) n, (void*) ap, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo); 
		return;
	} else {
		hook_pos_ctpttf = 0;
		fn_hook((void*) transr, (void*) uplo, (void*) n, (void*) ap, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ctpttf_(char* transr, char* uplo, blasint* n, float complex* ap, float complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ctpttf,CTPTTF)))));
#else
#ifndef __APPLE__
void ctpttf(char* transr, char* uplo, blasint* n, float complex* ap, float complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ctpttf,CTPTTF)))));
#else
void ctpttf(char* transr, char* uplo, blasint* n, float complex* ap, float complex* arf, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(ctpttf,CTPTTF)((void*) transr, (void*) uplo, (void*) n, (void*) ap, (void*) arf, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctpttf_(void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
	void (*fn) (void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

	*(void **) & fn = current_backend->lapack.ctpttf.f77_blas_function; 

		fn((void*) transr, (void*) uplo, (void*) n, (void*) ap, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ctpttf(void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_ctpttf_")));
#else
void flexiblas_real_ctpttf(void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_ctpttf_((void*) transr, (void*) uplo, (void*) n, (void*) ap, (void*) arf, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ctpttf_(void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
	void (*fn) (void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
	void (*fn_hook) (void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

	*(void **) &fn      = current_backend->lapack.ctpttf.f77_blas_function; 

    hook_pos_ctpttf ++;
    if( hook_pos_ctpttf < __flexiblas_hooks->ctpttf.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ctpttf.f77_hook_function[hook_pos_ctpttf];
        fn_hook((void*) transr, (void*) uplo, (void*) n, (void*) ap, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_ctpttf = 0;
		fn((void*) transr, (void*) uplo, (void*) n, (void*) ap, (void*) arf, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ctpttf(void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_ctpttf_")));
#else
void flexiblas_chain_ctpttf(void* transr, void* uplo, void* n, void* ap, void* arf, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_ctpttf_((void*) transr, (void*) uplo, (void*) n, (void*) ap, (void*) arf, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



