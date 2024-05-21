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


static TLS_STORE uint8_t hook_pos_dlascl = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlascl,DLASCL)(char* type_bn, blasint* kl, blasint* ku, double* cfrom, double* cto, blasint* m, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn)
#else
void FC_GLOBAL(dlascl,DLASCL)(char* type_bn, blasint* kl, blasint* ku, double* cfrom, double* cto, blasint* m, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn)
#endif
{
	void (*fn) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);
	void (*fn_hook) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.dlascl.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->dlascl.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn); 
		return;
	} else {
		hook_pos_dlascl = 0;
		fn_hook((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void dlascl_(char* type_bn, blasint* kl, blasint* ku, double* cfrom, double* cto, blasint* m, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn) __attribute__((alias(MTS(FC_GLOBAL(dlascl,DLASCL)))));
#else
#ifndef __APPLE__
void dlascl(char* type_bn, blasint* kl, blasint* ku, double* cfrom, double* cto, blasint* m, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn) __attribute__((alias(MTS(FC_GLOBAL(dlascl,DLASCL)))));
#else
void dlascl(char* type_bn, blasint* kl, blasint* ku, double* cfrom, double* cto, blasint* m, blasint* n, double* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn){ FC_GLOBAL(dlascl,DLASCL)((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_type_bn); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlascl_(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn)
{
	void (*fn) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);

	*(void **) & fn = current_backend->lapack.dlascl.f77_blas_function; 

		fn((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_dlascl(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn) __attribute__((alias("flexiblas_real_dlascl_")));
#else
void flexiblas_real_dlascl(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn){flexiblas_real_dlascl_((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_type_bn);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlascl_(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn)
{
	void (*fn) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);
	void (*fn_hook) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);

	*(void **) &fn      = current_backend->lapack.dlascl.f77_blas_function; 

    hook_pos_dlascl ++;
    if( hook_pos_dlascl < __flexiblas_hooks->dlascl.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlascl.f77_hook_function[hook_pos_dlascl];
        fn_hook((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn);
    } else {
        hook_pos_dlascl = 0;
		fn((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_dlascl(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn) __attribute__((alias("flexiblas_chain_dlascl_")));
#else
void flexiblas_chain_dlascl(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn){flexiblas_chain_dlascl_((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_type_bn);}
#endif



