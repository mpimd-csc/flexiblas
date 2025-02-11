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


static TLS_STORE uint8_t hook_pos_icmax1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(icmax1,ICMAX1)(blasint* n, float complex* cx, blasint* incx)
#else
blasint FC_GLOBAL(icmax1,ICMAX1)(blasint* n, float complex* cx, blasint* incx)
#endif
{
    blasint (*fn) (void* n, void* cx, void* incx);
    blasint (*fn_hook) (void* n, void* cx, void* incx);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.icmax1.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->icmax1.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) n, (void*) cx, (void*) incx);
        return ret;
    } else {
        hook_pos_icmax1 = 0;
        ret = fn_hook((void*) n, (void*) cx, (void*) incx);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(icmax1,ICMAX1)(blasint* n, float complex* cx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(icmax1,ICMAX1)))));
blasint FC_GLOBAL3(icmax1,ICMAX1)(blasint* n, float complex* cx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(icmax1,ICMAX1)))));
#else
blasint FC_GLOBAL2(icmax1,ICMAX1)(blasint* n, float complex* cx, blasint* incx){ return FC_GLOBAL(icmax1,ICMAX1)((void*) n, (void*) cx, (void*) incx); }
blasint FC_GLOBAL3(icmax1,ICMAX1)(blasint* n, float complex* cx, blasint* incx){ return FC_GLOBAL(icmax1,ICMAX1)((void*) n, (void*) cx, (void*) incx); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_icmax1_(void* n, void* cx, void* incx)
{
    blasint (*fn) (void* n, void* cx, void* incx);
    blasint ret;

    *(void **) & fn = current_backend->lapack.icmax1.f77_blas_function;

    ret = fn((void*) n, (void*) cx, (void*) incx);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_icmax1(void* n, void* cx, void* incx) __attribute__((alias("flexiblas_real_icmax1_")));
#else
blasint flexiblas_real_icmax1(void* n, void* cx, void* incx){return flexiblas_real_icmax1_((void*) n, (void*) cx, (void*) incx);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_icmax1_(void* n, void* cx, void* incx)
{
    blasint (*fn) (void* n, void* cx, void* incx);
    blasint (*fn_hook) (void* n, void* cx, void* incx);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.icmax1.f77_blas_function;

    hook_pos_icmax1 ++;
    if( hook_pos_icmax1 < __flexiblas_hooks->icmax1.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->icmax1.f77_hook_function[hook_pos_icmax1];
        ret = fn_hook((void*) n, (void*) cx, (void*) incx);
    } else {
        hook_pos_icmax1 = 0;
        ret = fn((void*) n, (void*) cx, (void*) incx);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_icmax1(void* n, void* cx, void* incx) __attribute__((alias("flexiblas_chain_icmax1_")));
#else
blasint flexiblas_chain_icmax1(void* n, void* cx, void* incx){return flexiblas_chain_icmax1_((void*) n, (void*) cx, (void*) incx);}
#endif



static TLS_STORE uint8_t hook_pos_ieeeck = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ieeeck,IEEECK)(blasint* ispec, float* zero, float* one)
#else
blasint FC_GLOBAL(ieeeck,IEEECK)(blasint* ispec, float* zero, float* one)
#endif
{
    blasint (*fn) (void* ispec, void* zero, void* one);
    blasint (*fn_hook) (void* ispec, void* zero, void* one);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ieeeck.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ieeeck.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) ispec, (void*) zero, (void*) one);
        return ret;
    } else {
        hook_pos_ieeeck = 0;
        ret = fn_hook((void*) ispec, (void*) zero, (void*) one);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ieeeck,IEEECK)(blasint* ispec, float* zero, float* one) __attribute__((alias(MTS(FC_GLOBAL(ieeeck,IEEECK)))));
blasint FC_GLOBAL3(ieeeck,IEEECK)(blasint* ispec, float* zero, float* one) __attribute__((alias(MTS(FC_GLOBAL(ieeeck,IEEECK)))));
#else
blasint FC_GLOBAL2(ieeeck,IEEECK)(blasint* ispec, float* zero, float* one){ return FC_GLOBAL(ieeeck,IEEECK)((void*) ispec, (void*) zero, (void*) one); }
blasint FC_GLOBAL3(ieeeck,IEEECK)(blasint* ispec, float* zero, float* one){ return FC_GLOBAL(ieeeck,IEEECK)((void*) ispec, (void*) zero, (void*) one); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ieeeck_(void* ispec, void* zero, void* one)
{
    blasint (*fn) (void* ispec, void* zero, void* one);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ieeeck.f77_blas_function;

    ret = fn((void*) ispec, (void*) zero, (void*) one);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ieeeck(void* ispec, void* zero, void* one) __attribute__((alias("flexiblas_real_ieeeck_")));
#else
blasint flexiblas_real_ieeeck(void* ispec, void* zero, void* one){return flexiblas_real_ieeeck_((void*) ispec, (void*) zero, (void*) one);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ieeeck_(void* ispec, void* zero, void* one)
{
    blasint (*fn) (void* ispec, void* zero, void* one);
    blasint (*fn_hook) (void* ispec, void* zero, void* one);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ieeeck.f77_blas_function;

    hook_pos_ieeeck ++;
    if( hook_pos_ieeeck < __flexiblas_hooks->ieeeck.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ieeeck.f77_hook_function[hook_pos_ieeeck];
        ret = fn_hook((void*) ispec, (void*) zero, (void*) one);
    } else {
        hook_pos_ieeeck = 0;
        ret = fn((void*) ispec, (void*) zero, (void*) one);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ieeeck(void* ispec, void* zero, void* one) __attribute__((alias("flexiblas_chain_ieeeck_")));
#else
blasint flexiblas_chain_ieeeck(void* ispec, void* zero, void* one){return flexiblas_chain_ieeeck_((void*) ispec, (void*) zero, (void*) one);}
#endif



static TLS_STORE uint8_t hook_pos_ilaclc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilaclc,ILACLC)(blasint* m, blasint* n, float complex* a, blasint* lda)
#else
blasint FC_GLOBAL(ilaclc,ILACLC)(blasint* m, blasint* n, float complex* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilaclc.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilaclc.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_ilaclc = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilaclc,ILACLC)(blasint* m, blasint* n, float complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaclc,ILACLC)))));
blasint FC_GLOBAL3(ilaclc,ILACLC)(blasint* m, blasint* n, float complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaclc,ILACLC)))));
#else
blasint FC_GLOBAL2(ilaclc,ILACLC)(blasint* m, blasint* n, float complex* a, blasint* lda){ return FC_GLOBAL(ilaclc,ILACLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(ilaclc,ILACLC)(blasint* m, blasint* n, float complex* a, blasint* lda){ return FC_GLOBAL(ilaclc,ILACLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilaclc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilaclc.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilaclc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_ilaclc_")));
#else
blasint flexiblas_real_ilaclc(void* m, void* n, void* a, void* lda){return flexiblas_real_ilaclc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilaclc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilaclc.f77_blas_function;

    hook_pos_ilaclc ++;
    if( hook_pos_ilaclc < __flexiblas_hooks->ilaclc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilaclc.f77_hook_function[hook_pos_ilaclc];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_ilaclc = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilaclc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_ilaclc_")));
#else
blasint flexiblas_chain_ilaclc(void* m, void* n, void* a, void* lda){return flexiblas_chain_ilaclc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



static TLS_STORE uint8_t hook_pos_ilaclr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilaclr,ILACLR)(blasint* m, blasint* n, float complex* a, blasint* lda)
#else
blasint FC_GLOBAL(ilaclr,ILACLR)(blasint* m, blasint* n, float complex* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilaclr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilaclr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_ilaclr = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilaclr,ILACLR)(blasint* m, blasint* n, float complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaclr,ILACLR)))));
blasint FC_GLOBAL3(ilaclr,ILACLR)(blasint* m, blasint* n, float complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaclr,ILACLR)))));
#else
blasint FC_GLOBAL2(ilaclr,ILACLR)(blasint* m, blasint* n, float complex* a, blasint* lda){ return FC_GLOBAL(ilaclr,ILACLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(ilaclr,ILACLR)(blasint* m, blasint* n, float complex* a, blasint* lda){ return FC_GLOBAL(ilaclr,ILACLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilaclr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilaclr.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilaclr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_ilaclr_")));
#else
blasint flexiblas_real_ilaclr(void* m, void* n, void* a, void* lda){return flexiblas_real_ilaclr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilaclr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilaclr.f77_blas_function;

    hook_pos_ilaclr ++;
    if( hook_pos_ilaclr < __flexiblas_hooks->ilaclr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilaclr.f77_hook_function[hook_pos_ilaclr];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_ilaclr = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilaclr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_ilaclr_")));
#else
blasint flexiblas_chain_ilaclr(void* m, void* n, void* a, void* lda){return flexiblas_chain_ilaclr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



static TLS_STORE uint8_t hook_pos_iladiag = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag)
#else
blasint FC_GLOBAL(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag)
#endif
{
    blasint (*fn) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint (*fn_hook) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.iladiag.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->iladiag.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) diag, ( flexiblas_fortran_charlen_t ) len_diag);
        return ret;
    } else {
        hook_pos_iladiag = 0;
        ret = fn_hook((void*) diag, ( flexiblas_fortran_charlen_t ) len_diag);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(iladiag,ILADIAG)))));
blasint FC_GLOBAL3(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag) __attribute__((alias(MTS(FC_GLOBAL(iladiag,ILADIAG)))));
#else
blasint FC_GLOBAL2(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag){ return FC_GLOBAL(iladiag,ILADIAG)((void*) diag, (flexiblas_fortran_charlen_t) len_diag); }
blasint FC_GLOBAL3(iladiag,ILADIAG)(char* diag, flexiblas_fortran_charlen_t len_diag){ return FC_GLOBAL(iladiag,ILADIAG)((void*) diag, (flexiblas_fortran_charlen_t) len_diag); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iladiag_(void* diag, flexiblas_fortran_charlen_t len_diag)
{
    blasint (*fn) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint ret;

    *(void **) & fn = current_backend->lapack.iladiag.f77_blas_function;

    ret = fn((void*) diag, ( flexiblas_fortran_charlen_t ) len_diag);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_iladiag(void* diag, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_real_iladiag_")));
#else
blasint flexiblas_real_iladiag(void* diag, flexiblas_fortran_charlen_t len_diag){return flexiblas_real_iladiag_((void*) diag, (flexiblas_fortran_charlen_t) len_diag);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_iladiag_(void* diag, flexiblas_fortran_charlen_t len_diag)
{
    blasint (*fn) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint (*fn_hook) (void* diag, flexiblas_fortran_charlen_t len_diag);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.iladiag.f77_blas_function;

    hook_pos_iladiag ++;
    if( hook_pos_iladiag < __flexiblas_hooks->iladiag.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->iladiag.f77_hook_function[hook_pos_iladiag];
        ret = fn_hook((void*) diag, ( flexiblas_fortran_charlen_t )len_diag);
    } else {
        hook_pos_iladiag = 0;
        ret = fn((void*) diag, ( flexiblas_fortran_charlen_t ) len_diag);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_iladiag(void* diag, flexiblas_fortran_charlen_t len_diag) __attribute__((alias("flexiblas_chain_iladiag_")));
#else
blasint flexiblas_chain_iladiag(void* diag, flexiblas_fortran_charlen_t len_diag){return flexiblas_chain_iladiag_((void*) diag, (flexiblas_fortran_charlen_t) len_diag);}
#endif



static TLS_STORE uint8_t hook_pos_iladlc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(iladlc,ILADLC)(blasint* m, blasint* n, double* a, blasint* lda)
#else
blasint FC_GLOBAL(iladlc,ILADLC)(blasint* m, blasint* n, double* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.iladlc.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->iladlc.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_iladlc = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(iladlc,ILADLC)(blasint* m, blasint* n, double* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(iladlc,ILADLC)))));
blasint FC_GLOBAL3(iladlc,ILADLC)(blasint* m, blasint* n, double* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(iladlc,ILADLC)))));
#else
blasint FC_GLOBAL2(iladlc,ILADLC)(blasint* m, blasint* n, double* a, blasint* lda){ return FC_GLOBAL(iladlc,ILADLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(iladlc,ILADLC)(blasint* m, blasint* n, double* a, blasint* lda){ return FC_GLOBAL(iladlc,ILADLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iladlc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.iladlc.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_iladlc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_iladlc_")));
#else
blasint flexiblas_real_iladlc(void* m, void* n, void* a, void* lda){return flexiblas_real_iladlc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_iladlc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.iladlc.f77_blas_function;

    hook_pos_iladlc ++;
    if( hook_pos_iladlc < __flexiblas_hooks->iladlc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->iladlc.f77_hook_function[hook_pos_iladlc];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_iladlc = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_iladlc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_iladlc_")));
#else
blasint flexiblas_chain_iladlc(void* m, void* n, void* a, void* lda){return flexiblas_chain_iladlc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



static TLS_STORE uint8_t hook_pos_iladlr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(iladlr,ILADLR)(blasint* m, blasint* n, double* a, blasint* lda)
#else
blasint FC_GLOBAL(iladlr,ILADLR)(blasint* m, blasint* n, double* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.iladlr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->iladlr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_iladlr = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(iladlr,ILADLR)(blasint* m, blasint* n, double* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(iladlr,ILADLR)))));
blasint FC_GLOBAL3(iladlr,ILADLR)(blasint* m, blasint* n, double* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(iladlr,ILADLR)))));
#else
blasint FC_GLOBAL2(iladlr,ILADLR)(blasint* m, blasint* n, double* a, blasint* lda){ return FC_GLOBAL(iladlr,ILADLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(iladlr,ILADLR)(blasint* m, blasint* n, double* a, blasint* lda){ return FC_GLOBAL(iladlr,ILADLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iladlr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.iladlr.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_iladlr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_iladlr_")));
#else
blasint flexiblas_real_iladlr(void* m, void* n, void* a, void* lda){return flexiblas_real_iladlr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_iladlr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.iladlr.f77_blas_function;

    hook_pos_iladlr ++;
    if( hook_pos_iladlr < __flexiblas_hooks->iladlr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->iladlr.f77_hook_function[hook_pos_iladlr];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_iladlr = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_iladlr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_iladlr_")));
#else
blasint flexiblas_chain_iladlr(void* m, void* n, void* a, void* lda){return flexiblas_chain_iladlr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



static TLS_STORE uint8_t hook_pos_ilaenv2stage = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilaenv2stage,ILAENV2STAGE)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#else
blasint FC_GLOBAL(ilaenv2stage,ILAENV2STAGE)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#endif
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilaenv2stage.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilaenv2stage.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    } else {
        hook_pos_ilaenv2stage = 0;
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilaenv2stage,ILAENV2STAGE)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(ilaenv2stage,ILAENV2STAGE)))));
blasint FC_GLOBAL3(ilaenv2stage,ILAENV2STAGE)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(ilaenv2stage,ILAENV2STAGE)))));
#else
blasint FC_GLOBAL2(ilaenv2stage,ILAENV2STAGE)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(ilaenv2stage,ILAENV2STAGE)((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
blasint FC_GLOBAL3(ilaenv2stage,ILAENV2STAGE)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(ilaenv2stage,ILAENV2STAGE)((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilaenv2stage_(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilaenv2stage.f77_blas_function;

    ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilaenv2stage(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_real_ilaenv2stage_")));
#else
blasint flexiblas_real_ilaenv2stage(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_real_ilaenv2stage_((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilaenv2stage_(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilaenv2stage.f77_blas_function;

    hook_pos_ilaenv2stage ++;
    if( hook_pos_ilaenv2stage < __flexiblas_hooks->ilaenv2stage.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilaenv2stage.f77_hook_function[hook_pos_ilaenv2stage];
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t )len_name, ( flexiblas_fortran_charlen_t )len_opts);
    } else {
        hook_pos_ilaenv2stage = 0;
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilaenv2stage(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_chain_ilaenv2stage_")));
#else
blasint flexiblas_chain_ilaenv2stage(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_chain_ilaenv2stage_((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif



static TLS_STORE uint8_t hook_pos_ilaenv = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilaenv,ILAENV)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#else
blasint FC_GLOBAL(ilaenv,ILAENV)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#endif
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilaenv.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilaenv.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    } else {
        hook_pos_ilaenv = 0;
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilaenv,ILAENV)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(ilaenv,ILAENV)))));
blasint FC_GLOBAL3(ilaenv,ILAENV)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(ilaenv,ILAENV)))));
#else
blasint FC_GLOBAL2(ilaenv,ILAENV)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(ilaenv,ILAENV)((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
blasint FC_GLOBAL3(ilaenv,ILAENV)(blasint* ispec, char* name, char* opts, blasint* n1, blasint* n2, blasint* n3, blasint* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(ilaenv,ILAENV)((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilaenv_(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilaenv.f77_blas_function;

    ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilaenv(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_real_ilaenv_")));
#else
blasint flexiblas_real_ilaenv(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_real_ilaenv_((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilaenv_(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilaenv.f77_blas_function;

    hook_pos_ilaenv ++;
    if( hook_pos_ilaenv < __flexiblas_hooks->ilaenv.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilaenv.f77_hook_function[hook_pos_ilaenv];
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t )len_name, ( flexiblas_fortran_charlen_t )len_opts);
    } else {
        hook_pos_ilaenv = 0;
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilaenv(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_chain_ilaenv_")));
#else
blasint flexiblas_chain_ilaenv(void* ispec, void* name, void* opts, void* n1, void* n2, void* n3, void* n4, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_chain_ilaenv_((void*) ispec, (void*) name, (void*) opts, (void*) n1, (void*) n2, (void*) n3, (void*) n4, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif



static TLS_STORE uint8_t hook_pos_ilaprec = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilaprec,ILAPREC)(char* prec, flexiblas_fortran_charlen_t len_prec)
#else
blasint FC_GLOBAL(ilaprec,ILAPREC)(char* prec, flexiblas_fortran_charlen_t len_prec)
#endif
{
    blasint (*fn) (void* prec, flexiblas_fortran_charlen_t len_prec);
    blasint (*fn_hook) (void* prec, flexiblas_fortran_charlen_t len_prec);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilaprec.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilaprec.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) prec, ( flexiblas_fortran_charlen_t ) len_prec);
        return ret;
    } else {
        hook_pos_ilaprec = 0;
        ret = fn_hook((void*) prec, ( flexiblas_fortran_charlen_t ) len_prec);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilaprec,ILAPREC)(char* prec, flexiblas_fortran_charlen_t len_prec) __attribute__((alias(MTS(FC_GLOBAL(ilaprec,ILAPREC)))));
blasint FC_GLOBAL3(ilaprec,ILAPREC)(char* prec, flexiblas_fortran_charlen_t len_prec) __attribute__((alias(MTS(FC_GLOBAL(ilaprec,ILAPREC)))));
#else
blasint FC_GLOBAL2(ilaprec,ILAPREC)(char* prec, flexiblas_fortran_charlen_t len_prec){ return FC_GLOBAL(ilaprec,ILAPREC)((void*) prec, (flexiblas_fortran_charlen_t) len_prec); }
blasint FC_GLOBAL3(ilaprec,ILAPREC)(char* prec, flexiblas_fortran_charlen_t len_prec){ return FC_GLOBAL(ilaprec,ILAPREC)((void*) prec, (flexiblas_fortran_charlen_t) len_prec); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilaprec_(void* prec, flexiblas_fortran_charlen_t len_prec)
{
    blasint (*fn) (void* prec, flexiblas_fortran_charlen_t len_prec);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilaprec.f77_blas_function;

    ret = fn((void*) prec, ( flexiblas_fortran_charlen_t ) len_prec);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilaprec(void* prec, flexiblas_fortran_charlen_t len_prec) __attribute__((alias("flexiblas_real_ilaprec_")));
#else
blasint flexiblas_real_ilaprec(void* prec, flexiblas_fortran_charlen_t len_prec){return flexiblas_real_ilaprec_((void*) prec, (flexiblas_fortran_charlen_t) len_prec);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilaprec_(void* prec, flexiblas_fortran_charlen_t len_prec)
{
    blasint (*fn) (void* prec, flexiblas_fortran_charlen_t len_prec);
    blasint (*fn_hook) (void* prec, flexiblas_fortran_charlen_t len_prec);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilaprec.f77_blas_function;

    hook_pos_ilaprec ++;
    if( hook_pos_ilaprec < __flexiblas_hooks->ilaprec.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilaprec.f77_hook_function[hook_pos_ilaprec];
        ret = fn_hook((void*) prec, ( flexiblas_fortran_charlen_t )len_prec);
    } else {
        hook_pos_ilaprec = 0;
        ret = fn((void*) prec, ( flexiblas_fortran_charlen_t ) len_prec);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilaprec(void* prec, flexiblas_fortran_charlen_t len_prec) __attribute__((alias("flexiblas_chain_ilaprec_")));
#else
blasint flexiblas_chain_ilaprec(void* prec, flexiblas_fortran_charlen_t len_prec){return flexiblas_chain_ilaprec_((void*) prec, (flexiblas_fortran_charlen_t) len_prec);}
#endif



static TLS_STORE uint8_t hook_pos_ilaslc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda)
#else
blasint FC_GLOBAL(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilaslc.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilaslc.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_ilaslc = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaslc,ILASLC)))));
blasint FC_GLOBAL3(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaslc,ILASLC)))));
#else
blasint FC_GLOBAL2(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda){ return FC_GLOBAL(ilaslc,ILASLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(ilaslc,ILASLC)(blasint* m, blasint* n, float* a, blasint* lda){ return FC_GLOBAL(ilaslc,ILASLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilaslc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilaslc.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilaslc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_ilaslc_")));
#else
blasint flexiblas_real_ilaslc(void* m, void* n, void* a, void* lda){return flexiblas_real_ilaslc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilaslc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilaslc.f77_blas_function;

    hook_pos_ilaslc ++;
    if( hook_pos_ilaslc < __flexiblas_hooks->ilaslc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilaslc.f77_hook_function[hook_pos_ilaslc];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_ilaslc = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilaslc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_ilaslc_")));
#else
blasint flexiblas_chain_ilaslc(void* m, void* n, void* a, void* lda){return flexiblas_chain_ilaslc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



static TLS_STORE uint8_t hook_pos_ilaslr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilaslr,ILASLR)(blasint* m, blasint* n, float* a, blasint* lda)
#else
blasint FC_GLOBAL(ilaslr,ILASLR)(blasint* m, blasint* n, float* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilaslr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilaslr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_ilaslr = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilaslr,ILASLR)(blasint* m, blasint* n, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaslr,ILASLR)))));
blasint FC_GLOBAL3(ilaslr,ILASLR)(blasint* m, blasint* n, float* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilaslr,ILASLR)))));
#else
blasint FC_GLOBAL2(ilaslr,ILASLR)(blasint* m, blasint* n, float* a, blasint* lda){ return FC_GLOBAL(ilaslr,ILASLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(ilaslr,ILASLR)(blasint* m, blasint* n, float* a, blasint* lda){ return FC_GLOBAL(ilaslr,ILASLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilaslr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilaslr.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilaslr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_ilaslr_")));
#else
blasint flexiblas_real_ilaslr(void* m, void* n, void* a, void* lda){return flexiblas_real_ilaslr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilaslr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilaslr.f77_blas_function;

    hook_pos_ilaslr ++;
    if( hook_pos_ilaslr < __flexiblas_hooks->ilaslr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilaslr.f77_hook_function[hook_pos_ilaslr];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_ilaslr = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilaslr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_ilaslr_")));
#else
blasint flexiblas_chain_ilaslr(void* m, void* n, void* a, void* lda){return flexiblas_chain_ilaslr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



static TLS_STORE uint8_t hook_pos_ilatrans = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilatrans,ILATRANS)(char* trans, flexiblas_fortran_charlen_t len_trans)
#else
blasint FC_GLOBAL(ilatrans,ILATRANS)(char* trans, flexiblas_fortran_charlen_t len_trans)
#endif
{
    blasint (*fn) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint (*fn_hook) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilatrans.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilatrans.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) trans, ( flexiblas_fortran_charlen_t ) len_trans);
        return ret;
    } else {
        hook_pos_ilatrans = 0;
        ret = fn_hook((void*) trans, ( flexiblas_fortran_charlen_t ) len_trans);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilatrans,ILATRANS)(char* trans, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(ilatrans,ILATRANS)))));
blasint FC_GLOBAL3(ilatrans,ILATRANS)(char* trans, flexiblas_fortran_charlen_t len_trans) __attribute__((alias(MTS(FC_GLOBAL(ilatrans,ILATRANS)))));
#else
blasint FC_GLOBAL2(ilatrans,ILATRANS)(char* trans, flexiblas_fortran_charlen_t len_trans){ return FC_GLOBAL(ilatrans,ILATRANS)((void*) trans, (flexiblas_fortran_charlen_t) len_trans); }
blasint FC_GLOBAL3(ilatrans,ILATRANS)(char* trans, flexiblas_fortran_charlen_t len_trans){ return FC_GLOBAL(ilatrans,ILATRANS)((void*) trans, (flexiblas_fortran_charlen_t) len_trans); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilatrans_(void* trans, flexiblas_fortran_charlen_t len_trans)
{
    blasint (*fn) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilatrans.f77_blas_function;

    ret = fn((void*) trans, ( flexiblas_fortran_charlen_t ) len_trans);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilatrans(void* trans, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_real_ilatrans_")));
#else
blasint flexiblas_real_ilatrans(void* trans, flexiblas_fortran_charlen_t len_trans){return flexiblas_real_ilatrans_((void*) trans, (flexiblas_fortran_charlen_t) len_trans);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilatrans_(void* trans, flexiblas_fortran_charlen_t len_trans)
{
    blasint (*fn) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint (*fn_hook) (void* trans, flexiblas_fortran_charlen_t len_trans);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilatrans.f77_blas_function;

    hook_pos_ilatrans ++;
    if( hook_pos_ilatrans < __flexiblas_hooks->ilatrans.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilatrans.f77_hook_function[hook_pos_ilatrans];
        ret = fn_hook((void*) trans, ( flexiblas_fortran_charlen_t )len_trans);
    } else {
        hook_pos_ilatrans = 0;
        ret = fn((void*) trans, ( flexiblas_fortran_charlen_t ) len_trans);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilatrans(void* trans, flexiblas_fortran_charlen_t len_trans) __attribute__((alias("flexiblas_chain_ilatrans_")));
#else
blasint flexiblas_chain_ilatrans(void* trans, flexiblas_fortran_charlen_t len_trans){return flexiblas_chain_ilatrans_((void*) trans, (flexiblas_fortran_charlen_t) len_trans);}
#endif



static TLS_STORE uint8_t hook_pos_ilauplo = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilauplo,ILAUPLO)(char* uplo, flexiblas_fortran_charlen_t len_uplo)
#else
blasint FC_GLOBAL(ilauplo,ILAUPLO)(char* uplo, flexiblas_fortran_charlen_t len_uplo)
#endif
{
    blasint (*fn) (void* uplo, flexiblas_fortran_charlen_t len_uplo);
    blasint (*fn_hook) (void* uplo, flexiblas_fortran_charlen_t len_uplo);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilauplo.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilauplo.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) uplo, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    } else {
        hook_pos_ilauplo = 0;
        ret = fn_hook((void*) uplo, ( flexiblas_fortran_charlen_t ) len_uplo);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilauplo,ILAUPLO)(char* uplo, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ilauplo,ILAUPLO)))));
blasint FC_GLOBAL3(ilauplo,ILAUPLO)(char* uplo, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias(MTS(FC_GLOBAL(ilauplo,ILAUPLO)))));
#else
blasint FC_GLOBAL2(ilauplo,ILAUPLO)(char* uplo, flexiblas_fortran_charlen_t len_uplo){ return FC_GLOBAL(ilauplo,ILAUPLO)((void*) uplo, (flexiblas_fortran_charlen_t) len_uplo); }
blasint FC_GLOBAL3(ilauplo,ILAUPLO)(char* uplo, flexiblas_fortran_charlen_t len_uplo){ return FC_GLOBAL(ilauplo,ILAUPLO)((void*) uplo, (flexiblas_fortran_charlen_t) len_uplo); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilauplo_(void* uplo, flexiblas_fortran_charlen_t len_uplo)
{
    blasint (*fn) (void* uplo, flexiblas_fortran_charlen_t len_uplo);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilauplo.f77_blas_function;

    ret = fn((void*) uplo, ( flexiblas_fortran_charlen_t ) len_uplo);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilauplo(void* uplo, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_real_ilauplo_")));
#else
blasint flexiblas_real_ilauplo(void* uplo, flexiblas_fortran_charlen_t len_uplo){return flexiblas_real_ilauplo_((void*) uplo, (flexiblas_fortran_charlen_t) len_uplo);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilauplo_(void* uplo, flexiblas_fortran_charlen_t len_uplo)
{
    blasint (*fn) (void* uplo, flexiblas_fortran_charlen_t len_uplo);
    blasint (*fn_hook) (void* uplo, flexiblas_fortran_charlen_t len_uplo);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilauplo.f77_blas_function;

    hook_pos_ilauplo ++;
    if( hook_pos_ilauplo < __flexiblas_hooks->ilauplo.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilauplo.f77_hook_function[hook_pos_ilauplo];
        ret = fn_hook((void*) uplo, ( flexiblas_fortran_charlen_t )len_uplo);
    } else {
        hook_pos_ilauplo = 0;
        ret = fn((void*) uplo, ( flexiblas_fortran_charlen_t ) len_uplo);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilauplo(void* uplo, flexiblas_fortran_charlen_t len_uplo) __attribute__((alias("flexiblas_chain_ilauplo_")));
#else
blasint flexiblas_chain_ilauplo(void* uplo, flexiblas_fortran_charlen_t len_uplo){return flexiblas_chain_ilauplo_((void*) uplo, (flexiblas_fortran_charlen_t) len_uplo);}
#endif



static TLS_STORE uint8_t hook_pos_ilazlc = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilazlc,ILAZLC)(blasint* m, blasint* n, double complex* a, blasint* lda)
#else
blasint FC_GLOBAL(ilazlc,ILAZLC)(blasint* m, blasint* n, double complex* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilazlc.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilazlc.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_ilazlc = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilazlc,ILAZLC)(blasint* m, blasint* n, double complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilazlc,ILAZLC)))));
blasint FC_GLOBAL3(ilazlc,ILAZLC)(blasint* m, blasint* n, double complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilazlc,ILAZLC)))));
#else
blasint FC_GLOBAL2(ilazlc,ILAZLC)(blasint* m, blasint* n, double complex* a, blasint* lda){ return FC_GLOBAL(ilazlc,ILAZLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(ilazlc,ILAZLC)(blasint* m, blasint* n, double complex* a, blasint* lda){ return FC_GLOBAL(ilazlc,ILAZLC)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilazlc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilazlc.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilazlc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_ilazlc_")));
#else
blasint flexiblas_real_ilazlc(void* m, void* n, void* a, void* lda){return flexiblas_real_ilazlc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilazlc_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilazlc.f77_blas_function;

    hook_pos_ilazlc ++;
    if( hook_pos_ilazlc < __flexiblas_hooks->ilazlc.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilazlc.f77_hook_function[hook_pos_ilazlc];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_ilazlc = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilazlc(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_ilazlc_")));
#else
blasint flexiblas_chain_ilazlc(void* m, void* n, void* a, void* lda){return flexiblas_chain_ilazlc_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



static TLS_STORE uint8_t hook_pos_ilazlr = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda)
#else
blasint FC_GLOBAL(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda)
#endif
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.ilazlr.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->ilazlr.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    } else {
        hook_pos_ilazlr = 0;
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilazlr,ILAZLR)))));
blasint FC_GLOBAL3(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda) __attribute__((alias(MTS(FC_GLOBAL(ilazlr,ILAZLR)))));
#else
blasint FC_GLOBAL2(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda){ return FC_GLOBAL(ilazlr,ILAZLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
blasint FC_GLOBAL3(ilazlr,ILAZLR)(blasint* m, blasint* n, double complex* a, blasint* lda){ return FC_GLOBAL(ilazlr,ILAZLR)((void*) m, (void*) n, (void*) a, (void*) lda); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_ilazlr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) & fn = current_backend->lapack.ilazlr.f77_blas_function;

    ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_ilazlr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_real_ilazlr_")));
#else
blasint flexiblas_real_ilazlr(void* m, void* n, void* a, void* lda){return flexiblas_real_ilazlr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_ilazlr_(void* m, void* n, void* a, void* lda)
{
    blasint (*fn) (void* m, void* n, void* a, void* lda);
    blasint (*fn_hook) (void* m, void* n, void* a, void* lda);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.ilazlr.f77_blas_function;

    hook_pos_ilazlr ++;
    if( hook_pos_ilazlr < __flexiblas_hooks->ilazlr.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->ilazlr.f77_hook_function[hook_pos_ilazlr];
        ret = fn_hook((void*) m, (void*) n, (void*) a, (void*) lda);
    } else {
        hook_pos_ilazlr = 0;
        ret = fn((void*) m, (void*) n, (void*) a, (void*) lda);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_ilazlr(void* m, void* n, void* a, void* lda) __attribute__((alias("flexiblas_chain_ilazlr_")));
#else
blasint flexiblas_chain_ilazlr(void* m, void* n, void* a, void* lda){return flexiblas_chain_ilazlr_((void*) m, (void*) n, (void*) a, (void*) lda);}
#endif



static TLS_STORE uint8_t hook_pos_iparam2stage = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#else
blasint FC_GLOBAL(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#endif
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.iparam2stage.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->iparam2stage.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    } else {
        hook_pos_iparam2stage = 0;
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(iparam2stage,IPARAM2STAGE)))));
blasint FC_GLOBAL3(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(iparam2stage,IPARAM2STAGE)))));
#else
blasint FC_GLOBAL2(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(iparam2stage,IPARAM2STAGE)((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
blasint FC_GLOBAL3(iparam2stage,IPARAM2STAGE)(blasint* ispec, char* name, char* opts, blasint* ni, blasint* nbi, blasint* ibi, blasint* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(iparam2stage,IPARAM2STAGE)((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iparam2stage_(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) & fn = current_backend->lapack.iparam2stage.f77_blas_function;

    ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_iparam2stage(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_real_iparam2stage_")));
#else
blasint flexiblas_real_iparam2stage(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_real_iparam2stage_((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_iparam2stage_(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.iparam2stage.f77_blas_function;

    hook_pos_iparam2stage ++;
    if( hook_pos_iparam2stage < __flexiblas_hooks->iparam2stage.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->iparam2stage.f77_hook_function[hook_pos_iparam2stage];
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t )len_name, ( flexiblas_fortran_charlen_t )len_opts);
    } else {
        hook_pos_iparam2stage = 0;
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_iparam2stage(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_chain_iparam2stage_")));
#else
blasint flexiblas_chain_iparam2stage(void* ispec, void* name, void* opts, void* ni, void* nbi, void* ibi, void* nxi, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_chain_iparam2stage_((void*) ispec, (void*) name, (void*) opts, (void*) ni, (void*) nbi, (void*) ibi, (void*) nxi, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif



static TLS_STORE uint8_t hook_pos_iparmq = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(iparmq,IPARMQ)(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#else
blasint FC_GLOBAL(iparmq,IPARMQ)(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
#endif
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.iparmq.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->iparmq.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    } else {
        hook_pos_iparmq = 0;
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(iparmq,IPARMQ)(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(iparmq,IPARMQ)))));
blasint FC_GLOBAL3(iparmq,IPARMQ)(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias(MTS(FC_GLOBAL(iparmq,IPARMQ)))));
#else
blasint FC_GLOBAL2(iparmq,IPARMQ)(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(iparmq,IPARMQ)((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
blasint FC_GLOBAL3(iparmq,IPARMQ)(blasint* ispec, char* name, char* opts, blasint* n, blasint* ilo, blasint* ihi, blasint* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){ return FC_GLOBAL(iparmq,IPARMQ)((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_iparmq_(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) & fn = current_backend->lapack.iparmq.f77_blas_function;

    ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_iparmq(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_real_iparmq_")));
#else
blasint flexiblas_real_iparmq(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_real_iparmq_((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_iparmq_(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts)
{
    blasint (*fn) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint (*fn_hook) (void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.iparmq.f77_blas_function;

    hook_pos_iparmq ++;
    if( hook_pos_iparmq < __flexiblas_hooks->iparmq.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->iparmq.f77_hook_function[hook_pos_iparmq];
        ret = fn_hook((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( flexiblas_fortran_charlen_t )len_name, ( flexiblas_fortran_charlen_t )len_opts);
    } else {
        hook_pos_iparmq = 0;
        ret = fn((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, ( flexiblas_fortran_charlen_t ) len_name, ( flexiblas_fortran_charlen_t ) len_opts);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_iparmq(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts) __attribute__((alias("flexiblas_chain_iparmq_")));
#else
blasint flexiblas_chain_iparmq(void* ispec, void* name, void* opts, void* n, void* ilo, void* ihi, void* lwork, flexiblas_fortran_charlen_t len_name, flexiblas_fortran_charlen_t len_opts){return flexiblas_chain_iparmq_((void*) ispec, (void*) name, (void*) opts, (void*) n, (void*) ilo, (void*) ihi, (void*) lwork, (flexiblas_fortran_charlen_t) len_name, (flexiblas_fortran_charlen_t) len_opts);}
#endif



static TLS_STORE uint8_t hook_pos_izmax1 = 0;
#ifdef FLEXIBLAS_ABI_INTEL
blasint FC_GLOBAL(izmax1,IZMAX1)(blasint* n, double complex* zx, blasint* incx)
#else
blasint FC_GLOBAL(izmax1,IZMAX1)(blasint* n, double complex* zx, blasint* incx)
#endif
{
    blasint (*fn) (void* n, void* zx, void* incx);
    blasint (*fn_hook) (void* n, void* zx, void* incx);
    blasint ret;

    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }
    *(void **) & fn = current_backend->lapack.izmax1.f77_blas_function;
    *(void **) & fn_hook = __flexiblas_hooks->izmax1.f77_hook_function[0];
    if ( fn_hook == NULL ) {
        ret = fn((void*) n, (void*) zx, (void*) incx);
        return ret;
    } else {
        hook_pos_izmax1 = 0;
        ret = fn_hook((void*) n, (void*) zx, (void*) incx);
        return ret;
    }
}
#ifndef __APPLE__
blasint FC_GLOBAL2(izmax1,IZMAX1)(blasint* n, double complex* zx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(izmax1,IZMAX1)))));
blasint FC_GLOBAL3(izmax1,IZMAX1)(blasint* n, double complex* zx, blasint* incx) __attribute__((alias(MTS(FC_GLOBAL(izmax1,IZMAX1)))));
#else
blasint FC_GLOBAL2(izmax1,IZMAX1)(blasint* n, double complex* zx, blasint* incx){ return FC_GLOBAL(izmax1,IZMAX1)((void*) n, (void*) zx, (void*) incx); }
blasint FC_GLOBAL3(izmax1,IZMAX1)(blasint* n, double complex* zx, blasint* incx){ return FC_GLOBAL(izmax1,IZMAX1)((void*) n, (void*) zx, (void*) incx); }
#endif




/* Real Implementation for Hooks */


blasint flexiblas_real_izmax1_(void* n, void* zx, void* incx)
{
    blasint (*fn) (void* n, void* zx, void* incx);
    blasint ret;

    *(void **) & fn = current_backend->lapack.izmax1.f77_blas_function;

    ret = fn((void*) n, (void*) zx, (void*) incx);

    return ret;
}
#ifndef __APPLE__
blasint flexiblas_real_izmax1(void* n, void* zx, void* incx) __attribute__((alias("flexiblas_real_izmax1_")));
#else
blasint flexiblas_real_izmax1(void* n, void* zx, void* incx){return flexiblas_real_izmax1_((void*) n, (void*) zx, (void*) incx);}
#endif




/* Chainloader for Hooks */


blasint flexiblas_chain_izmax1_(void* n, void* zx, void* incx)
{
    blasint (*fn) (void* n, void* zx, void* incx);
    blasint (*fn_hook) (void* n, void* zx, void* incx);
    blasint ret;

    *(void **) &fn      = current_backend->lapack.izmax1.f77_blas_function;

    hook_pos_izmax1 ++;
    if( hook_pos_izmax1 < __flexiblas_hooks->izmax1.nhook) {
        *(void **) &fn_hook = __flexiblas_hooks->izmax1.f77_hook_function[hook_pos_izmax1];
        ret = fn_hook((void*) n, (void*) zx, (void*) incx);
    } else {
        hook_pos_izmax1 = 0;
        ret = fn((void*) n, (void*) zx, (void*) incx);
    }
    return ret;
}
#ifndef __APPLE__
blasint flexiblas_chain_izmax1(void* n, void* zx, void* incx) __attribute__((alias("flexiblas_chain_izmax1_")));
#else
blasint flexiblas_chain_izmax1(void* n, void* zx, void* incx){return flexiblas_chain_izmax1_((void*) n, (void*) zx, (void*) incx);}
#endif



