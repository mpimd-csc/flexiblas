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


static TLS_STORE uint8_t hook_pos_ztrsyl = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ztrsyl,ZTRSYL)(char* trana, char* tranb, blasint* isgn, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double* scale, blasint* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb)
#else
void FC_GLOBAL(ztrsyl,ZTRSYL)(char* trana, char* tranb, blasint* isgn, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double* scale, blasint* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb)
#endif
{
	void (*fn) (void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb);
	void (*fn_hook) (void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ztrsyl.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ztrsyl.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) trana, (void*) tranb, (void*) isgn, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) scale, (void*) info, ( flexiblas_fortran_charlen_t ) len_trana, ( flexiblas_fortran_charlen_t ) len_tranb); 
		return;
	} else {
		hook_pos_ztrsyl = 0;
		fn_hook((void*) trana, (void*) tranb, (void*) isgn, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) scale, (void*) info, ( flexiblas_fortran_charlen_t ) len_trana, ( flexiblas_fortran_charlen_t ) len_tranb);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ztrsyl_(char* trana, char* tranb, blasint* isgn, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double* scale, blasint* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb) __attribute__((alias(MTS(FC_GLOBAL(ztrsyl,ZTRSYL)))));
#else
#ifndef __APPLE__
void ztrsyl(char* trana, char* tranb, blasint* isgn, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double* scale, blasint* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb) __attribute__((alias(MTS(FC_GLOBAL(ztrsyl,ZTRSYL)))));
#else
void ztrsyl(char* trana, char* tranb, blasint* isgn, blasint* m, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* c, blasint* ldc, double* scale, blasint* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb){ FC_GLOBAL(ztrsyl,ZTRSYL)((void*) trana, (void*) tranb, (void*) isgn, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) scale, (void*) info, (flexiblas_fortran_charlen_t) len_trana, (flexiblas_fortran_charlen_t) len_tranb); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ztrsyl_(void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb)
{
	void (*fn) (void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb);

	*(void **) & fn = current_backend->lapack.ztrsyl.f77_blas_function; 

		fn((void*) trana, (void*) tranb, (void*) isgn, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) scale, (void*) info, ( flexiblas_fortran_charlen_t ) len_trana, ( flexiblas_fortran_charlen_t ) len_tranb); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ztrsyl(void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb) __attribute__((alias("flexiblas_real_ztrsyl_")));
#else
void flexiblas_real_ztrsyl(void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb){flexiblas_real_ztrsyl_((void*) trana, (void*) tranb, (void*) isgn, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) scale, (void*) info, (flexiblas_fortran_charlen_t) len_trana, (flexiblas_fortran_charlen_t) len_tranb);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ztrsyl_(void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb)
{
	void (*fn) (void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb);
	void (*fn_hook) (void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb);

	*(void **) &fn      = current_backend->lapack.ztrsyl.f77_blas_function; 

    hook_pos_ztrsyl ++;
    if( hook_pos_ztrsyl < __flexiblas_hooks->ztrsyl.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ztrsyl.f77_hook_function[hook_pos_ztrsyl];
        fn_hook((void*) trana, (void*) tranb, (void*) isgn, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) scale, (void*) info, ( flexiblas_fortran_charlen_t ) len_trana, ( flexiblas_fortran_charlen_t ) len_tranb);
    } else {
        hook_pos_ztrsyl = 0;
		fn((void*) trana, (void*) tranb, (void*) isgn, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) scale, (void*) info, ( flexiblas_fortran_charlen_t ) len_trana, ( flexiblas_fortran_charlen_t ) len_tranb); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ztrsyl(void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb) __attribute__((alias("flexiblas_chain_ztrsyl_")));
#else
void flexiblas_chain_ztrsyl(void* trana, void* tranb, void* isgn, void* m, void* n, void* a, void* lda, void* b, void* ldb, void* c, void* ldc, void* scale, void* info, flexiblas_fortran_charlen_t len_trana, flexiblas_fortran_charlen_t len_tranb){flexiblas_chain_ztrsyl_((void*) trana, (void*) tranb, (void*) isgn, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) c, (void*) ldc, (void*) scale, (void*) info, (flexiblas_fortran_charlen_t) len_trana, (flexiblas_fortran_charlen_t) len_tranb);}
#endif



