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


static TLS_STORE uint8_t hook_pos_sgbbrd = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(sgbbrd,SGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* d, float* e, float* q, blasint* ldq, float* pt, blasint* ldpt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect)
#else
void FC_GLOBAL(sgbbrd,SGBBRD)(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* d, float* e, float* q, blasint* ldq, float* pt, blasint* ldpt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect)
#endif
{
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect);
	void (*fn_hook) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.sgbbrd.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->sgbbrd.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect); 
		return;
	} else {
		hook_pos_sgbbrd = 0;
		fn_hook((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void sgbbrd_(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* d, float* e, float* q, blasint* ldq, float* pt, blasint* ldpt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias(MTS(FC_GLOBAL(sgbbrd,SGBBRD)))));
#else
#ifndef __APPLE__
void sgbbrd(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* d, float* e, float* q, blasint* ldq, float* pt, blasint* ldpt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias(MTS(FC_GLOBAL(sgbbrd,SGBBRD)))));
#else
void sgbbrd(char* vect, blasint* m, blasint* n, blasint* ncc, blasint* kl, blasint* ku, float* ab, blasint* ldab, float* d, float* e, float* q, blasint* ldq, float* pt, blasint* ldpt, float* c, blasint* ldc, float* work, blasint* info, flexiblas_fortran_charlen_t len_vect){ FC_GLOBAL(sgbbrd,SGBBRD)((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_vect); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_sgbbrd_(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect)
{
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect);

	*(void **) & fn = current_backend->lapack.sgbbrd.f77_blas_function; 

		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_sgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias("flexiblas_real_sgbbrd_")));
#else
void flexiblas_real_sgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect){flexiblas_real_sgbbrd_((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_vect);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_sgbbrd_(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect)
{
	void (*fn) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect);
	void (*fn_hook) (void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect);

	*(void **) &fn      = current_backend->lapack.sgbbrd.f77_blas_function; 

    hook_pos_sgbbrd ++;
    if( hook_pos_sgbbrd < __flexiblas_hooks->sgbbrd.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->sgbbrd.f77_hook_function[hook_pos_sgbbrd];
        fn_hook((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect);
    } else {
        hook_pos_sgbbrd = 0;
		fn((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) info, ( flexiblas_fortran_charlen_t ) len_vect); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_sgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect) __attribute__((alias("flexiblas_chain_sgbbrd_")));
#else
void flexiblas_chain_sgbbrd(void* vect, void* m, void* n, void* ncc, void* kl, void* ku, void* ab, void* ldab, void* d, void* e, void* q, void* ldq, void* pt, void* ldpt, void* c, void* ldc, void* work, void* info, flexiblas_fortran_charlen_t len_vect){flexiblas_chain_sgbbrd_((void*) vect, (void*) m, (void*) n, (void*) ncc, (void*) kl, (void*) ku, (void*) ab, (void*) ldab, (void*) d, (void*) e, (void*) q, (void*) ldq, (void*) pt, (void*) ldpt, (void*) c, (void*) ldc, (void*) work, (void*) info, (flexiblas_fortran_charlen_t) len_vect);}
#endif



