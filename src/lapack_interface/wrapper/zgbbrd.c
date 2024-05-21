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


static TLS_STORE uint8_t hook_pos_zgbbrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(zgbbrd,ZGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* d, double* e, double complex* q, blasint* ldq, double complex* pt, blasint* ldpt, double complex* c, blasint* ldc, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_vect)
#else
void FC_GLOBAL(zgbbrd,ZGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* d, double* e, double complex* q, blasint* ldq, double complex* pt, blasint* ldpt, double complex* c, blasint* ldc, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_vect)
#endif
{
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect);
	void (*fn_hook) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.zgbbrd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->zgbbrd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect); 
		return;
	} else {
		hook_pos_zgbbrd = 0;
		fn_hook((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void zgbbrd_(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* d, double* e, double complex* q, blasint* ldq, double complex* pt, blasint* ldpt, double complex* c, blasint* ldc, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias(MTS(FC_GLOBAL(zgbbrd,ZGBBRD)))));
#else
#ifndef __APPLE__
void zgbbrd(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* d, double* e, double complex* q, blasint* ldq, double complex* pt, blasint* ldpt, double complex* c, blasint* ldc, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias(MTS(FC_GLOBAL(zgbbrd,ZGBBRD)))));
#else
void zgbbrd(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, double complex* ab, blasint* ldab, double* d, double* e, double complex* q, blasint* ldq, double complex* pt, blasint* ldpt, double complex* c, blasint* ldc, double complex* work, double* rwork, blasint* info, flexiblas_fortran_charlen_t len_vect){ FC_GLOBAL(zgbbrd,ZGBBRD)((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_zgbbrd_(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect)
{
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect);

	*(void **) & fn = current_backend->lapack.zgbbrd.f77_blas_function; 

		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_zgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias("flexiblas_real_zgbbrd_")));
#else
void flexiblas_real_zgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect){flexiblas_real_zgbbrd_((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_zgbbrd_(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect)
{
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect);
	void (*fn_hook) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect);

	*(void **) &fn      = current_backend->lapack.zgbbrd.f77_blas_function; 

    hook_pos_zgbbrd ++;
    if( hook_pos_zgbbrd < __flexiblas_hooks->zgbbrd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->zgbbrd.f77_hook_function[hook_pos_zgbbrd];
        fn_hook((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
    } else {
        hook_pos_zgbbrd = 0;
		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_zgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias("flexiblas_chain_zgbbrd_")));
#else
void flexiblas_chain_zgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* rwork, void* info, flexiblas_fortran_charlen_t len_vect){flexiblas_chain_zgbbrd_((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) rwork, (void*) info, (flexiblas_fortran_charlen_t) len_vect);}
#endif



