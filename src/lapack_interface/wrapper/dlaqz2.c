//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2025 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <complex.h>

#include "flexiblas_config.h"

#include "flexiblas_fortran_mangle.h"

#include "flexiblas.h"

#include "flexiblas_fortran_char_len.h"


static TLS_STORE uint8_t hook_pos_dlaqz2 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(dlaqz2,DLAQZ2)(blaslogical* ilq, blaslogical* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, blasint* nq, blasint* qstart, double* q, blasint* ldq, blasint* nz, blasint* zstart, double* z, blasint* ldz)
#else
void FC_GLOBAL(dlaqz2,DLAQZ2)(blaslogical* ilq, blaslogical* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, blasint* nq, blasint* qstart, double* q, blasint* ldq, blasint* nz, blasint* zstart, double* z, blasint* ldz)
#endif
{
    void (*fn) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);
    void (*fn_hook) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.dlaqz2.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->dlaqz2.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);
        return;
    } else {
        hook_pos_dlaqz2 = 0;
        fn_hook((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(dlaqz2,DLAQZ2)(blaslogical* ilq, blaslogical* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, blasint* nq, blasint* qstart, double* q, blasint* ldq, blasint* nz, blasint* zstart, double* z, blasint* ldz) __attribute__((alias(MTS(FC_GLOBAL(dlaqz2,DLAQZ2)))));
void FC_GLOBAL3(dlaqz2,DLAQZ2)(blaslogical* ilq, blaslogical* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, blasint* nq, blasint* qstart, double* q, blasint* ldq, blasint* nz, blasint* zstart, double* z, blasint* ldz) __attribute__((alias(MTS(FC_GLOBAL(dlaqz2,DLAQZ2)))));
#else
void FC_GLOBAL2(dlaqz2,DLAQZ2)(blaslogical* ilq, blaslogical* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, blasint* nq, blasint* qstart, double* q, blasint* ldq, blasint* nz, blasint* zstart, double* z, blasint* ldz){ FC_GLOBAL(dlaqz2,DLAQZ2)((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz); }
void FC_GLOBAL3(dlaqz2,DLAQZ2)(blaslogical* ilq, blaslogical* ilz, blasint* k, blasint* istartm, blasint* istopm, blasint* ihi, double* a, blasint* lda, double* b, blasint* ldb, blasint* nq, blasint* qstart, double* q, blasint* ldq, blasint* nz, blasint* zstart, double* z, blasint* ldz){ FC_GLOBAL(dlaqz2,DLAQZ2)((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_dlaqz2_(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz)
{
    void (*fn) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);

    *(void **) & fn = current_backend->lapack.dlaqz2.f77_blas_function;

    fn((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);

    return;
}
#ifndef __APPLE__
void flexiblas_real_dlaqz2(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz) __attribute__((alias("flexiblas_real_dlaqz2_")));
#else
void flexiblas_real_dlaqz2(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz){flexiblas_real_dlaqz2_((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_dlaqz2_(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz)
{
    void (*fn) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);
    void (*fn_hook) (void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz);

    *(void **) &fn      = current_backend->lapack.dlaqz2.f77_blas_function;

    hook_pos_dlaqz2 ++;
    if( hook_pos_dlaqz2 < __flexiblas_hooks->dlaqz2.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->dlaqz2.f77_hook_function[hook_pos_dlaqz2];
        fn_hook((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);
    } else {
        hook_pos_dlaqz2 = 0;
        fn((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_dlaqz2(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz) __attribute__((alias("flexiblas_chain_dlaqz2_")));
#else
void flexiblas_chain_dlaqz2(void* ilq, void* ilz, void* k, void* istartm, void* istopm, void* ihi, void* a, void* lda, void* b, void* ldb, void* nq, void* qstart, void* q, void* ldq, void* nz, void* zstart, void* z, void* ldz){flexiblas_chain_dlaqz2_((void*) ilq, (void*) ilz, (void*) k, (void*) istartm, (void*) istopm, (void*) ihi, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) nq, (void*) qstart, (void*) q, (void*) ldq, (void*) nz, (void*) zstart, (void*) z, (void*) ldz);}
#endif



