//  SPDX-License-Identifier: LGPL-3.0-or-later
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


static TLS_STORE uint8_t hook_pos_dlaqz4 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaqz4,DLAQZ4)(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nshifts, blasint* nblock_desired, double* sr, double* si, double* ss, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* qc, blasint* ldqc, double* zc, blasint* ldzc, double* work, blasint* lwork, blasint* info)
#else
void FC_GLOBAL(dlaqz4,DLAQZ4)(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nshifts, blasint* nblock_desired, double* sr, double* si, double* ss, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* qc, blasint* ldqc, double* zc, blasint* ldzc, double* work, blasint* lwork, blasint* info)
#endif
{
    void (*fn) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info);
    void (*fn_hook) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlaqz4.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlaqz4.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nshifts, (void*) nblock_desired, (void*) sr, (void*) si, (void*) ss, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) info);
        return;
    } else {
        hook_pos_dlaqz4 = 0;
        fn_hook((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nshifts, (void*) nblock_desired, (void*) sr, (void*) si, (void*) ss, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) info);
        return;
    }
}
#ifdef FLEXIBLAS_ABI_IBM
void dlaqz4_(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nshifts, blasint* nblock_desired, double* sr, double* si, double* ss, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* qc, blasint* ldqc, double* zc, blasint* ldzc, double* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaqz4,DLAQZ4)))));
#else
#ifndef __APPLE__
void dlaqz4(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nshifts, blasint* nblock_desired, double* sr, double* si, double* ss, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* qc, blasint* ldqc, double* zc, blasint* ldzc, double* work, blasint* lwork, blasint* info) __attribute__((alias(MTS(FC_GLOBAL(dlaqz4,DLAQZ4)))));
#else
void dlaqz4(blasint* ilschur, blasint* ilq, blasint* ilz, blasint* n, blasint* ilo, blasint* ihi, blasint* nshifts, blasint* nblock_desired, double* sr, double* si, double* ss, double* a, blasint* lda, double* b, blasint* ldb, double* q, blasint* ldq, double* z, blasint* ldz, double* qc, blasint* ldqc, double* zc, blasint* ldzc, double* work, blasint* lwork, blasint* info){ FC_GLOBAL(dlaqz4,DLAQZ4)((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nshifts, (void*) nblock_desired, (void*) sr, (void*) si, (void*) ss, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) info); }
#endif
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaqz4_(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info)
{
    void (*fn) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info);

    *(void **) & fn = current_backend->lapack.dlaqz4.f77_blas_function;

    fn((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nshifts, (void*) nblock_desired, (void*) sr, (void*) si, (void*) ss, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) info);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlaqz4(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_real_dlaqz4_")));
#else
void flexiblas_real_dlaqz4(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info){flexiblas_real_dlaqz4_((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nshifts, (void*) nblock_desired, (void*) sr, (void*) si, (void*) ss, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) info);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaqz4_(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info)
{
    void (*fn) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info);
    void (*fn_hook) (void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info);

    *(void **) &fn      = current_backend->lapack.dlaqz4.f77_blas_function;

    hook_pos_dlaqz4 ++;
    if( hook_pos_dlaqz4 < __flexiblas_hooks->dlaqz4.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaqz4.f77_hook_function[hook_pos_dlaqz4];
        fn_hook((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nshifts, (void*) nblock_desired, (void*) sr, (void*) si, (void*) ss, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) info);
    } else {
        hook_pos_dlaqz4 = 0;
        fn((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nshifts, (void*) nblock_desired, (void*) sr, (void*) si, (void*) ss, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) info);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaqz4(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info) __attribute__((alias("flexiblas_chain_dlaqz4_")));
#else
void flexiblas_chain_dlaqz4(void* ilschur, void* ilq, void* ilz, void* n, void* ilo, void* ihi, void* nshifts, void* nblock_desired, void* sr, void* si, void* ss, void* a, void* lda, void* b, void* ldb, void* q, void* ldq, void* z, void* ldz, void* qc, void* ldqc, void* zc, void* ldzc, void* work, void* lwork, void* info){flexiblas_chain_dlaqz4_((void*) ilschur, (void*) ilq, (void*) ilz, (void*) n, (void*) ilo, (void*) ihi, (void*) nshifts, (void*) nblock_desired, (void*) sr, (void*) si, (void*) ss, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) q, (void*) ldq, (void*) z, (void*) ldz, (void*) qc, (void*) ldqc, (void*) zc, (void*) ldzc, (void*) work, (void*) lwork, (void*) info);}
#endif



