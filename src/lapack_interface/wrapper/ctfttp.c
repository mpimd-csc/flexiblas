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


static TLS_STORE uint8_t hook_pos_ctfttp = 0;
#ifdef FLEXIBLAS_ABI_INTEL
void FC_GLOBAL(ctfttp,CTFTTP)(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#else
void FC_GLOBAL(ctfttp,CTFTTP)(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    void (*fn) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ctfttp.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ctfttp.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        fn((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    } else {
        hook_pos_ctfttp = 0;
        fn_hook((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
        return;
    }
}
#ifndef __APPLE__
void FC_GLOBAL2(ctfttp,CTFTTP)(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ctfttp,CTFTTP)))));
void FC_GLOBAL3(ctfttp,CTFTTP)(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ctfttp,CTFTTP)))));
#else
void FC_GLOBAL2(ctfttp,CTFTTP)(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(ctfttp,CTFTTP)((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo); }
void FC_GLOBAL3(ctfttp,CTFTTP)(char* transr, char* uplo, blasint* n, float complex* arf, float complex* ap, blasint* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){ FC_GLOBAL(ctfttp,CTFTTP)((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


void flexiblas_real_ctfttp_(void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    *(void **) & fn = current_backend->lapack.ctfttp.f77_blas_function;

    fn((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);

    return;
}
#ifndef __APPLE__
void flexiblas_real_ctfttp(void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_ctfttp_")));
#else
void flexiblas_real_ctfttp(void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){flexiblas_real_ctfttp_((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


void flexiblas_chain_ctfttp_(void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo)
{
    void (*fn) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);
    void (*fn_hook) (void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo);

    *(void **) &fn      = current_backend->lapack.ctfttp.f77_blas_function;

    hook_pos_ctfttp ++;
    if( hook_pos_ctfttp < __flexiblas_hooks->ctfttp.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ctfttp.f77_hook_function[hook_pos_ctfttp];
        fn_hook((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
    } else {
        hook_pos_ctfttp = 0;
        fn((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info, ( flexiblas_fortran_charlen_t ) len_transr, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return;
}
#ifndef __APPLE__
void flexiblas_chain_ctfttp(void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_ctfttp_")));
#else
void flexiblas_chain_ctfttp(void* transr, void* uplo, void* n, void* arf, void* ap, void* info, flexiblas_fortran_charlen_t len_transr, flexiblas_fortran_charlen_t len_uplo){flexiblas_chain_ctfttp_((void*) transr, (void*) uplo, (void*) n, (void*) arf, (void*) ap, (void*) info, (flexiblas_fortran_charlen_t) len_transr, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



