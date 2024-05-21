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


static TLS_STORE uint8_t hook_pos_slaqz3 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(slaqz3,SLAQZ3)(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* ns, blasint* nd, float* alphar, float* alphai, float* beta, float* qc, blasint* ldqc, float* zc, blasint* ldzc, float* work, blasint* lwork, blasint* rec, blasint* info)
#else
void FC_GLOBAL(slaqz3,SLAQZ3)(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* ns, blasint* nd, float* alphar, float* alphai, float* beta, float* qc, blasint* ldqc, float* zc, blasint* ldzc, float* work, blasint* lwork, blasint* rec, blasint* info)
#endif
{
	void (*fn) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info);
	void (*fn_hook) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
	*(void **) & fn = current_backend->lapack.slaqz3.f77_blas_function; 
	*(void **) & fn_hook = __flexiblas_hooks->slaqz3.f77_hook_function[0]; 
	if ( fn_hook == NULL ) { 
		fn((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alphar, (void*) alphai, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rec, (void*) info); 
		return;
	} else {
		hook_pos_slaqz3 = 0;
		fn_hook((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alphar, (void*) alphai, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rec, (void*) info);
		return;
	}
}
#ifdef FLEXIBLAS_ABI_IBM
void slaqz3_(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* ns, blasint* nd, float* alphar, float* alphai, float* beta, float* qc, blasint* ldqc, float* zc, blasint* ldzc, float* work, blasint* lwork, blasint* rec, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaqz3,SLAQZ3)))));
#else
#ifndef __APPLE__
void slaqz3(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* ns, blasint* nd, float* alphar, float* alphai, float* beta, float* qc, blasint* ldqc, float* zc, blasint* ldzc, float* work, blasint* lwork, blasint* rec, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(slaqz3,SLAQZ3)))));
#else
void slaqz3(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nw, float* a, blasint* lda, float* b, blasint* ldb, float* q, blasint* ldq, float* z, blasint* ldz, blasint* ns, blasint* nd, float* alphar, float* alphai, float* beta, float* qc, blasint* ldqc, float* zc, blasint* ldzc, float* work, blasint* lwork, blasint* rec, blasint* info){ FC_GLOBAL(slaqz3,SLAQZ3)((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alphar, (void*) alphai, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rec, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_slaqz3_(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info)
{
	void (*fn) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info);

	*(void **) & fn = current_backend->lapack.slaqz3.f77_blas_function; 

		fn((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alphar, (void*) alphai, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rec, (void*) info); 

	return;
}
#ifndef __APPLE__
void flexiblas_real_slaqz3(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info) __attribute__((alias("flexiblas_real_slaqz3_")));
#else
void flexiblas_real_slaqz3(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info){flexiblas_real_slaqz3_((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alphar, (void*) alphai, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rec, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_slaqz3_(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info)
{
	void (*fn) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info);
	void (*fn_hook) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info);

	*(void **) &fn      = current_backend->lapack.slaqz3.f77_blas_function; 

    hook_pos_slaqz3 ++;
    if( hook_pos_slaqz3 < __flexiblas_hooks->slaqz3.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->slaqz3.f77_hook_function[hook_pos_slaqz3];
        fn_hook((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alphar, (void*) alphai, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rec, (void*) info);
    } else {
        hook_pos_slaqz3 = 0;
		fn((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alphar, (void*) alphai, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rec, (void*) info); 
	}
	return;
}
#ifndef __APPLE__
void flexiblas_chain_slaqz3(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info) __attribute__((alias("flexiblas_chain_slaqz3_")));
#else
void flexiblas_chain_slaqz3(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nw, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* ns, void* nd, void* alphar, void* alphai, void* beta, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* rec, void* info){flexiblas_chain_slaqz3_((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nw, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) ns, (void*) nd, (void*) alphar, (void*) alphai, (void*) beta, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) rec, (void*) info);}
#endif



