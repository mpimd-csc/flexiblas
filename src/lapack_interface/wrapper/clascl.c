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


static TLS_STORE uint8_t hook_pos_clascl = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(clascl,CLASCL)(char* type_bn, blasint* kl, blasint* ku, float* cfrom, float* cto, blasint* m, blasint* n, float complex* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn)
#else
void FC_GLOBAL(clascl,CLASCL)(char* type_bn, blasint* kl, blasint* ku, float* cfrom, float* cto, blasint* m, blasint* n, float complex* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn)
#endif
{
    void (*fn) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);
    void (*fn_hook) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.clascl.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->clascl.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn);
        return;
    } else {
        hook_pos_clascl = 0;
        fn_hook((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(clascl,CLASCL)(char* type_bn, blasint* kl, blasint* ku, float* cfrom, float* cto, blasint* m, blasint* n, float complex* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn) __attribute__((alias(MTS(FC_GLOBAL(clascl,CLASCL)))));
void FC_GLOBAL3(clascl,CLASCL)(char* type_bn, blasint* kl, blasint* ku, float* cfrom, float* cto, blasint* m, blasint* n, float complex* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn) __attribute__((alias(MTS(FC_GLOBAL(clascl,CLASCL)))));
#else
void FC_GLOBAL2(clascl,CLASCL)(char* type_bn, blasint* kl, blasint* ku, float* cfrom, float* cto, blasint* m, blasint* n, float complex* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn){ FC_GLOBAL(clascl,CLASCL)((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_type_bn); }
void FC_GLOBAL3(clascl,CLASCL)(char* type_bn, blasint* kl, blasint* ku, float* cfrom, float* cto, blasint* m, blasint* n, float complex* a, blasint* lda, blasint* info, flexiblas_fortran_charlen_t len_type_bn){ FC_GLOBAL(clascl,CLASCL)((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_type_bn); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_clascl_(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn)
{
    void (*fn) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);

    *(void **) & fn = current_backend->lapack.clascl.f77_blas_function;

    fn((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn);

    return;
}
#ifndef __APPLE__
void flexiblas_real_clascl(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn) __attribute__((alias("flexiblas_real_clascl_")));
#else
void flexiblas_real_clascl(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn){flexiblas_real_clascl_((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_type_bn);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_clascl_(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn)
{
    void (*fn) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);
    void (*fn_hook) (void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn);

    *(void **) &fn      = current_backend->lapack.clascl.f77_blas_function;

    hook_pos_clascl ++;
    if( hook_pos_clascl < __flexiblas_hooks->clascl.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->clascl.f77_hook_function[hook_pos_clascl];
        fn_hook((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn);
    } else {
        hook_pos_clascl = 0;
        fn((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, ( flexiblas_fortran_charlen_t ) len_type_bn);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_clascl(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn) __attribute__((alias("flexiblas_chain_clascl_")));
#else
void flexiblas_chain_clascl(void* type_bn, void* kl, void* ku, void* cfrom, void* cto, void* m, void* n, void* a, void* lda, void* info, flexiblas_fortran_charlen_t len_type_bn){flexiblas_chain_clascl_((void*) type_bn, (void*) kl, (void*) ku, (void*) cfrom, (void*) cto, (void*) m, (void*) n, (void*) a, (void*) lda, (void*) info, (flexiblas_fortran_charlen_t) len_type_bn);}
#endif



