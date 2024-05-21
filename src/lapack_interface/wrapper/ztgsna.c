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


static TLS_STORE uint8_t hook_pos_ztgsna = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ztgsna,ZTGSNA)(char* job, char* howmny, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* dif, blasint* mm, blasint* m, double complex* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
#else
void FC_GLOBAL(ztgsna,ZTGSNA)(char* job, char* howmny, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* dif, blasint* mm, blasint* m, double complex* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
#endif
{
	void (*fn) (void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);
	void (*fn_hook) (void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.ztgsna.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->ztgsna.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) dif, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny); 
		return;
	} else {
		hook_pos_ztgsna = 0;
		fn_hook((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) dif, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void ztgsna_(char* job, char* howmny, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* dif, blasint* mm, blasint* m, double complex* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(ztgsna,ZTGSNA)))));
#else
#ifndef __APPLE__
void ztgsna(char* job, char* howmny, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* dif, blasint* mm, blasint* m, double complex* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias(MTS(FC_GLOBAL(ztgsna,ZTGSNA)))));
#else
void ztgsna(char* job, char* howmny, blasint* select, blasint* n, double complex* a, blasint* lda, double complex* b, blasint* ldb, double complex* vl, blasint* ldvl, double complex* vr, blasint* ldvr, double* s, double* dif, blasint* mm, blasint* m, double complex* work, blasint* lwork, blasint* iwork, blasint* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny){ FC_GLOBAL(ztgsna,ZTGSNA)((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) dif, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_howmny); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ztgsna_(void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
{
	void (*fn) (void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

	*(void **) & fn = current_backend->lapack.ztgsna.f77_blas_function; 

		fn((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) dif, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_ztgsna(void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_real_ztgsna_")));
#else
void flexiblas_real_ztgsna(void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny){flexiblas_real_ztgsna_((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) dif, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_howmny);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ztgsna_(void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny)
{
	void (*fn) (void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);
	void (*fn_hook) (void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny);

	*(void **) &fn      = current_backend->lapack.ztgsna.f77_blas_function; 

    hook_pos_ztgsna ++;
    if( hook_pos_ztgsna < __flexiblas_hooks->ztgsna.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ztgsna.f77_hook_function[hook_pos_ztgsna];
        fn_hook((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) dif, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny);
    } else {
        hook_pos_ztgsna = 0;
		fn((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) dif, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) iwork, (void*) info, ( flexiblas_fortran_charlen_t ) len_job, ( flexiblas_fortran_charlen_t ) len_howmny); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_ztgsna(void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny) __attribute__((alias("flexiblas_chain_ztgsna_")));
#else
void flexiblas_chain_ztgsna(void* job, void* howmny, void* select, void* n, void* a, void* lda, void* b, void* ldb, void* vl, void* ldvl, void* vr, void* ldvr, void* s, void* dif, void* mm, void* m, void* work, void* lwork, void* iwork, void* info, flexiblas_fortran_charlen_t len_job, flexiblas_fortran_charlen_t len_howmny){flexiblas_chain_ztgsna_((void*) job, (void*) howmny, (void*) select, (void*) n, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) vl, (void*) ldvl, (void*) vr, (void*) ldvr, (void*) s, (void*) dif, (void*) mm, (void*) m, (void*) work, (void*) lwork, (void*) iwork, (void*) info, (flexiblas_fortran_charlen_t) len_job, (flexiblas_fortran_charlen_t) len_howmny);}
#endif



