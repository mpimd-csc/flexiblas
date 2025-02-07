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
#include "flexiblas.h"
#include "flexiblas_fortran_char_len.h"
#include "flexiblas_fortran_mangle.h"
#include "helper.h"

#if __GNUC__ >= 7
#define likely(x)      __builtin_expect(!!(x), 1)
#define unlikely(x)    __builtin_expect(!!(x), 0)
#else
#define likely(x) x
#define unlikely(x) x
#endif

#include "cblas.h"
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_caxpby = 0;
#endif

/* Wrapper for cblas_caxpby */
void cblas_caxpby (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_caxpby = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_caxpby;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_caxpby.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX, beta, Y, incY);
    } else {
        fn(N, alpha, X, incX, beta, Y, incY);
    }
    #else
    fn(N, alpha, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_caxpby_(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_caxpby;
    fn(N, alpha, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_caxpby_(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_caxpby++;
    if ( hook_pos_cblas_caxpby < __flexiblas_hooks->cblas_caxpby.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_caxpby.hook_function[hook_pos_cblas_caxpby];
    } else {
        hook_pos_cblas_caxpby = 0;
        *(void **) &fn = current_backend->cblas.cblas_caxpby;
    }
    fn(N, alpha, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_caxpy = 0;
#endif

/* Wrapper for cblas_caxpy */
void cblas_caxpy (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_caxpy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_caxpy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_caxpy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX, Y, incY);
    } else {
        fn(N, alpha, X, incX, Y, incY);
    }
    #else
    fn(N, alpha, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_caxpy_(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_caxpy;
    fn(N, alpha, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_caxpy_(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_caxpy++;
    if ( hook_pos_cblas_caxpy < __flexiblas_hooks->cblas_caxpy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_caxpy.hook_function[hook_pos_cblas_caxpy];
    } else {
        hook_pos_cblas_caxpy = 0;
        *(void **) &fn = current_backend->cblas.cblas_caxpy;
    }
    fn(N, alpha, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ccopy = 0;
#endif

/* Wrapper for cblas_ccopy */
void cblas_ccopy (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_ccopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ccopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ccopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY);
    } else {
        fn(N, X, incX, Y, incY);
    }
    #else
    fn(N, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ccopy_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_ccopy;
    fn(N, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_ccopy_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_ccopy++;
    if ( hook_pos_cblas_ccopy < __flexiblas_hooks->cblas_ccopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ccopy.hook_function[hook_pos_cblas_ccopy];
    } else {
        hook_pos_cblas_ccopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_ccopy;
    }
    fn(N, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cdotc_sub = 0;
#endif

/* Wrapper for cblas_cdotc_sub */
void cblas_cdotc_sub (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
    hook_pos_cblas_cdotc_sub = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cdotc_sub;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cdotc_sub.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, dotc);
    } else {
        fn(N, X, incX, Y, incY, dotc);
    }
    #else
    fn(N, X, incX, Y, incY, dotc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cdotc_sub_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
    *(void **) &fn = current_backend->cblas.cblas_cdotc_sub;
    fn(N, X, incX, Y, incY, dotc);
    return;
}

void flexiblas_chain_cblas_cdotc_sub_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
    hook_pos_cblas_cdotc_sub++;
    if ( hook_pos_cblas_cdotc_sub < __flexiblas_hooks->cblas_cdotc_sub.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cdotc_sub.hook_function[hook_pos_cblas_cdotc_sub];
    } else {
        hook_pos_cblas_cdotc_sub = 0;
        *(void **) &fn = current_backend->cblas.cblas_cdotc_sub;
    }
    fn(N, X, incX, Y, incY, dotc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cdotu_sub = 0;
#endif

/* Wrapper for cblas_cdotu_sub */
void cblas_cdotu_sub (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
    hook_pos_cblas_cdotu_sub = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cdotu_sub;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cdotu_sub.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, dotu);
    } else {
        fn(N, X, incX, Y, incY, dotu);
    }
    #else
    fn(N, X, incX, Y, incY, dotu);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cdotu_sub_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
    *(void **) &fn = current_backend->cblas.cblas_cdotu_sub;
    fn(N, X, incX, Y, incY, dotu);
    return;
}

void flexiblas_chain_cblas_cdotu_sub_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
    hook_pos_cblas_cdotu_sub++;
    if ( hook_pos_cblas_cdotu_sub < __flexiblas_hooks->cblas_cdotu_sub.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cdotu_sub.hook_function[hook_pos_cblas_cdotu_sub];
    } else {
        hook_pos_cblas_cdotu_sub = 0;
        *(void **) &fn = current_backend->cblas.cblas_cdotu_sub;
    }
    fn(N, X, incX, Y, incY, dotu);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cgbmv = 0;
#endif

/* Wrapper for cblas_cgbmv */
void cblas_cgbmv (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_cgbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cgbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cgbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cgbmv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_cgbmv;
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_cgbmv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_cgbmv++;
    if ( hook_pos_cblas_cgbmv < __flexiblas_hooks->cblas_cgbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cgbmv.hook_function[hook_pos_cblas_cgbmv];
    } else {
        hook_pos_cblas_cgbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_cgbmv;
    }
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cgeadd = 0;
#endif

/* Wrapper for cblas_cgeadd */
void cblas_cgeadd (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_cgeadd = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cgeadd;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cgeadd.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    } else {
        fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    }
    #else
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cgeadd_(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_cgeadd;
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    return;
}

void flexiblas_chain_cblas_cgeadd_(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_cgeadd++;
    if ( hook_pos_cblas_cgeadd < __flexiblas_hooks->cblas_cgeadd.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cgeadd.hook_function[hook_pos_cblas_cgeadd];
    } else {
        hook_pos_cblas_cgeadd = 0;
        *(void **) &fn = current_backend->cblas.cblas_cgeadd;
    }
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cgemm = 0;
#endif

/* Wrapper for cblas_cgemm */
void cblas_cgemm (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_cgemm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cgemm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cgemm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cgemm_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_cgemm;
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_cgemm_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_cgemm++;
    if ( hook_pos_cblas_cgemm < __flexiblas_hooks->cblas_cgemm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cgemm.hook_function[hook_pos_cblas_cgemm];
    } else {
        hook_pos_cblas_cgemm = 0;
        *(void **) &fn = current_backend->cblas.cblas_cgemm;
    }
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cgemmtr = 0;
#endif

/* Wrapper for cblas_cgemmtr */
void cblas_cgemmtr (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_cgemmtr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cgemmtr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cgemmtr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cgemmtr_(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_cgemmtr;
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_cgemmtr_(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_cgemmtr++;
    if ( hook_pos_cblas_cgemmtr < __flexiblas_hooks->cblas_cgemmtr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cgemmtr.hook_function[hook_pos_cblas_cgemmtr];
    } else {
        hook_pos_cblas_cgemmtr = 0;
        *(void **) &fn = current_backend->cblas.cblas_cgemmtr;
    }
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
/* Alternative function name for cblas_cgemmtr -- cblas_cgemmt */
#ifndef __APPLE__
void cblas_cgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc) __attribute__((alias(MTS(cblas_cgemmtr))));
#else
void cblas_cgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc) 
{
    cblas_cgemmtr(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
}

#endif

#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cgemv = 0;
#endif

/* Wrapper for cblas_cgemv */
void cblas_cgemv (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_cgemv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cgemv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cgemv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cgemv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_cgemv;
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_cgemv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_cgemv++;
    if ( hook_pos_cblas_cgemv < __flexiblas_hooks->cblas_cgemv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cgemv.hook_function[hook_pos_cblas_cgemv];
    } else {
        hook_pos_cblas_cgemv = 0;
        *(void **) &fn = current_backend->cblas.cblas_cgemv;
    }
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cgerc = 0;
#endif

/* Wrapper for cblas_cgerc */
void cblas_cgerc (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_cgerc = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cgerc;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cgerc.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cgerc_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_cgerc;
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_cgerc_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_cgerc++;
    if ( hook_pos_cblas_cgerc < __flexiblas_hooks->cblas_cgerc.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cgerc.hook_function[hook_pos_cblas_cgerc];
    } else {
        hook_pos_cblas_cgerc = 0;
        *(void **) &fn = current_backend->cblas.cblas_cgerc;
    }
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cgeru = 0;
#endif

/* Wrapper for cblas_cgeru */
void cblas_cgeru (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_cgeru = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cgeru;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cgeru.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cgeru_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_cgeru;
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_cgeru_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_cgeru++;
    if ( hook_pos_cblas_cgeru < __flexiblas_hooks->cblas_cgeru.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cgeru.hook_function[hook_pos_cblas_cgeru];
    } else {
        hook_pos_cblas_cgeru = 0;
        *(void **) &fn = current_backend->cblas.cblas_cgeru;
    }
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_chbmv = 0;
#endif

/* Wrapper for cblas_chbmv */
void cblas_chbmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_chbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_chbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_chbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_chbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_chbmv;
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_chbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_chbmv++;
    if ( hook_pos_cblas_chbmv < __flexiblas_hooks->cblas_chbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_chbmv.hook_function[hook_pos_cblas_chbmv];
    } else {
        hook_pos_cblas_chbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_chbmv;
    }
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_chemm = 0;
#endif

/* Wrapper for cblas_chemm */
void cblas_chemm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_chemm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_chemm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_chemm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_chemm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_chemm;
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_chemm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_chemm++;
    if ( hook_pos_cblas_chemm < __flexiblas_hooks->cblas_chemm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_chemm.hook_function[hook_pos_cblas_chemm];
    } else {
        hook_pos_cblas_chemm = 0;
        *(void **) &fn = current_backend->cblas.cblas_chemm;
    }
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_chemv = 0;
#endif

/* Wrapper for cblas_chemv */
void cblas_chemv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_chemv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_chemv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_chemv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_chemv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_chemv;
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_chemv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_chemv++;
    if ( hook_pos_cblas_chemv < __flexiblas_hooks->cblas_chemv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_chemv.hook_function[hook_pos_cblas_chemv];
    } else {
        hook_pos_cblas_chemv = 0;
        *(void **) &fn = current_backend->cblas.cblas_chemv;
    }
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cher = 0;
#endif

/* Wrapper for cblas_cher */
void cblas_cher (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    hook_pos_cblas_cher = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cher;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cher.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, A, lda);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, A, lda);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cher_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_cher;
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    return;
}

void flexiblas_chain_cblas_cher_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    hook_pos_cblas_cher++;
    if ( hook_pos_cblas_cher < __flexiblas_hooks->cblas_cher.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cher.hook_function[hook_pos_cblas_cher];
    } else {
        hook_pos_cblas_cher = 0;
        *(void **) &fn = current_backend->cblas.cblas_cher;
    }
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cher2 = 0;
#endif

/* Wrapper for cblas_cher2 */
void cblas_cher2 (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_cher2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cher2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cher2.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cher2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_cher2;
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_cher2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_cher2++;
    if ( hook_pos_cblas_cher2 < __flexiblas_hooks->cblas_cher2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cher2.hook_function[hook_pos_cblas_cher2];
    } else {
        hook_pos_cblas_cher2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_cher2;
    }
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cher2k = 0;
#endif

/* Wrapper for cblas_cher2k */
void cblas_cher2k (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_cher2k = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cher2k;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cher2k.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cher2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_cher2k;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_cher2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_cher2k++;
    if ( hook_pos_cblas_cher2k < __flexiblas_hooks->cblas_cher2k.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cher2k.hook_function[hook_pos_cblas_cher2k];
    } else {
        hook_pos_cblas_cher2k = 0;
        *(void **) &fn = current_backend->cblas.cblas_cher2k;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cherk = 0;
#endif

/* Wrapper for cblas_cherk */
void cblas_cherk (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_cherk = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cherk;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cherk.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cherk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_cherk;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_cherk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_cherk++;
    if ( hook_pos_cblas_cherk < __flexiblas_hooks->cblas_cherk.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cherk.hook_function[hook_pos_cblas_cherk];
    } else {
        hook_pos_cblas_cherk = 0;
        *(void **) &fn = current_backend->cblas.cblas_cherk;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_chpmv = 0;
#endif

/* Wrapper for cblas_chpmv */
void cblas_chpmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_chpmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_chpmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_chpmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_chpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_chpmv;
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_chpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_chpmv++;
    if ( hook_pos_cblas_chpmv < __flexiblas_hooks->cblas_chpmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_chpmv.hook_function[hook_pos_cblas_chpmv];
    } else {
        hook_pos_cblas_chpmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_chpmv;
    }
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_chpr = 0;
#endif

/* Wrapper for cblas_chpr */
void cblas_chpr (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A);
    hook_pos_cblas_chpr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_chpr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_chpr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, A);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, A);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, A);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_chpr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A);
    *(void **) &fn = current_backend->cblas.cblas_chpr;
    fn(layout, Uplo, N, alpha, X, incX, A);
    return;
}

void flexiblas_chain_cblas_chpr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A);
    hook_pos_cblas_chpr++;
    if ( hook_pos_cblas_chpr < __flexiblas_hooks->cblas_chpr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_chpr.hook_function[hook_pos_cblas_chpr];
    } else {
        hook_pos_cblas_chpr = 0;
        *(void **) &fn = current_backend->cblas.cblas_chpr;
    }
    fn(layout, Uplo, N, alpha, X, incX, A);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_chpr2 = 0;
#endif

/* Wrapper for cblas_chpr2 */
void cblas_chpr2 (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
    hook_pos_cblas_chpr2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_chpr2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_chpr2.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_chpr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
    *(void **) &fn = current_backend->cblas.cblas_chpr2;
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    return;
}

void flexiblas_chain_cblas_chpr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
    hook_pos_cblas_chpr2++;
    if ( hook_pos_cblas_chpr2 < __flexiblas_hooks->cblas_chpr2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_chpr2.hook_function[hook_pos_cblas_chpr2];
    } else {
        hook_pos_cblas_chpr2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_chpr2;
    }
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cimatcopy = 0;
#endif

/* Wrapper for cblas_cimatcopy */
void cblas_cimatcopy (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    hook_pos_cblas_cimatcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cimatcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cimatcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    } else {
        fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    }
    #else
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cimatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_cimatcopy;
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    return;
}

void flexiblas_chain_cblas_cimatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    hook_pos_cblas_cimatcopy++;
    if ( hook_pos_cblas_cimatcopy < __flexiblas_hooks->cblas_cimatcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cimatcopy.hook_function[hook_pos_cblas_cimatcopy];
    } else {
        hook_pos_cblas_cimatcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_cimatcopy;
    }
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_comatcopy = 0;
#endif

/* Wrapper for cblas_comatcopy */
void cblas_comatcopy (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_comatcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_comatcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_comatcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    } else {
        fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    }
    #else
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_comatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_comatcopy;
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    return;
}

void flexiblas_chain_cblas_comatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_comatcopy++;
    if ( hook_pos_cblas_comatcopy < __flexiblas_hooks->cblas_comatcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_comatcopy.hook_function[hook_pos_cblas_comatcopy];
    } else {
        hook_pos_cblas_comatcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_comatcopy;
    }
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_crotg = 0;
#endif

/* Wrapper for cblas_crotg */
void cblas_crotg (void *a, void *b, float *c, void *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (void *a, void *b, void *c, void *s);
    hook_pos_cblas_crotg = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_crotg;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_crotg.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(a, b, c, s);
    } else {
        fn(a, b, c, s);
    }
    #else
    fn(a, b, c, s);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_crotg_(void *a, void *b, float *c, void *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    *(void **) &fn = current_backend->cblas.cblas_crotg;
    fn(a, b, c, s);
    return;
}

void flexiblas_chain_cblas_crotg_(void *a, void *b, void *c, void *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    hook_pos_cblas_crotg++;
    if ( hook_pos_cblas_crotg < __flexiblas_hooks->cblas_crotg.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_crotg.hook_function[hook_pos_cblas_crotg];
    } else {
        hook_pos_cblas_crotg = 0;
        *(void **) &fn = current_backend->cblas.cblas_crotg;
    }
    fn(a, b, c, s);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cscal = 0;
#endif

/* Wrapper for cblas_cscal */
void cblas_cscal (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_cscal = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cscal;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cscal.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX);
    } else {
        fn(N, alpha, X, incX);
    }
    #else
    fn(N, alpha, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cscal_(const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_cscal;
    fn(N, alpha, X, incX);
    return;
}

void flexiblas_chain_cblas_cscal_(const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_cscal++;
    if ( hook_pos_cblas_cscal < __flexiblas_hooks->cblas_cscal.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cscal.hook_function[hook_pos_cblas_cscal];
    } else {
        hook_pos_cblas_cscal = 0;
        *(void **) &fn = current_backend->cblas.cblas_cscal;
    }
    fn(N, alpha, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_csrot = 0;
#endif

/* Wrapper for cblas_csrot */
void cblas_csrot (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
    hook_pos_cblas_csrot = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_csrot;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_csrot.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, c, s);
    } else {
        fn(N, X, incX, Y, incY, c, s);
    }
    #else
    fn(N, X, incX, Y, incY, c, s);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_csrot_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
    *(void **) &fn = current_backend->cblas.cblas_csrot;
    fn(N, X, incX, Y, incY, c, s);
    return;
}

void flexiblas_chain_cblas_csrot_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
    hook_pos_cblas_csrot++;
    if ( hook_pos_cblas_csrot < __flexiblas_hooks->cblas_csrot.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_csrot.hook_function[hook_pos_cblas_csrot];
    } else {
        hook_pos_cblas_csrot = 0;
        *(void **) &fn = current_backend->cblas.cblas_csrot;
    }
    fn(N, X, incX, Y, incY, c, s);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_csscal = 0;
#endif

/* Wrapper for cblas_csscal */
void cblas_csscal (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_csscal = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_csscal;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_csscal.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX);
    } else {
        fn(N, alpha, X, incX);
    }
    #else
    fn(N, alpha, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_csscal_(const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_csscal;
    fn(N, alpha, X, incX);
    return;
}

void flexiblas_chain_cblas_csscal_(const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_csscal++;
    if ( hook_pos_cblas_csscal < __flexiblas_hooks->cblas_csscal.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_csscal.hook_function[hook_pos_cblas_csscal];
    } else {
        hook_pos_cblas_csscal = 0;
        *(void **) &fn = current_backend->cblas.cblas_csscal;
    }
    fn(N, alpha, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_cswap = 0;
#endif

/* Wrapper for cblas_cswap */
void cblas_cswap (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_cswap = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_cswap;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_cswap.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY);
    } else {
        fn(N, X, incX, Y, incY);
    }
    #else
    fn(N, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_cswap_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_cswap;
    fn(N, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_cswap_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_cswap++;
    if ( hook_pos_cblas_cswap < __flexiblas_hooks->cblas_cswap.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_cswap.hook_function[hook_pos_cblas_cswap];
    } else {
        hook_pos_cblas_cswap = 0;
        *(void **) &fn = current_backend->cblas.cblas_cswap;
    }
    fn(N, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_csymm = 0;
#endif

/* Wrapper for cblas_csymm */
void cblas_csymm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_csymm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_csymm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_csymm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_csymm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_csymm;
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_csymm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_csymm++;
    if ( hook_pos_cblas_csymm < __flexiblas_hooks->cblas_csymm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_csymm.hook_function[hook_pos_cblas_csymm];
    } else {
        hook_pos_cblas_csymm = 0;
        *(void **) &fn = current_backend->cblas.cblas_csymm;
    }
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_csyr2k = 0;
#endif

/* Wrapper for cblas_csyr2k */
void cblas_csyr2k (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_csyr2k = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_csyr2k;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_csyr2k.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_csyr2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_csyr2k;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_csyr2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_csyr2k++;
    if ( hook_pos_cblas_csyr2k < __flexiblas_hooks->cblas_csyr2k.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_csyr2k.hook_function[hook_pos_cblas_csyr2k];
    } else {
        hook_pos_cblas_csyr2k = 0;
        *(void **) &fn = current_backend->cblas.cblas_csyr2k;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_csyrk = 0;
#endif

/* Wrapper for cblas_csyrk */
void cblas_csyrk (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_csyrk = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_csyrk;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_csyrk.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_csyrk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_csyrk;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_csyrk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_csyrk++;
    if ( hook_pos_cblas_csyrk < __flexiblas_hooks->cblas_csyrk.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_csyrk.hook_function[hook_pos_cblas_csyrk];
    } else {
        hook_pos_cblas_csyrk = 0;
        *(void **) &fn = current_backend->cblas.cblas_csyrk;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ctbmv = 0;
#endif

/* Wrapper for cblas_ctbmv */
void cblas_ctbmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ctbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ctbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ctbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ctbmv;
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_ctbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctbmv++;
    if ( hook_pos_cblas_ctbmv < __flexiblas_hooks->cblas_ctbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ctbmv.hook_function[hook_pos_cblas_ctbmv];
    } else {
        hook_pos_cblas_ctbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ctbmv;
    }
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ctbsv = 0;
#endif

/* Wrapper for cblas_ctbsv */
void cblas_ctbsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctbsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ctbsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ctbsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ctbsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ctbsv;
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_ctbsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctbsv++;
    if ( hook_pos_cblas_ctbsv < __flexiblas_hooks->cblas_ctbsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ctbsv.hook_function[hook_pos_cblas_ctbsv];
    } else {
        hook_pos_cblas_ctbsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ctbsv;
    }
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ctpmv = 0;
#endif

/* Wrapper for cblas_ctpmv */
void cblas_ctpmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctpmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ctpmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ctpmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ctpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ctpmv;
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

void flexiblas_chain_cblas_ctpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctpmv++;
    if ( hook_pos_cblas_ctpmv < __flexiblas_hooks->cblas_ctpmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ctpmv.hook_function[hook_pos_cblas_ctpmv];
    } else {
        hook_pos_cblas_ctpmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ctpmv;
    }
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ctpsv = 0;
#endif

/* Wrapper for cblas_ctpsv */
void cblas_ctpsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctpsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ctpsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ctpsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ctpsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ctpsv;
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

void flexiblas_chain_cblas_ctpsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctpsv++;
    if ( hook_pos_cblas_ctpsv < __flexiblas_hooks->cblas_ctpsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ctpsv.hook_function[hook_pos_cblas_ctpsv];
    } else {
        hook_pos_cblas_ctpsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ctpsv;
    }
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ctrmm = 0;
#endif

/* Wrapper for cblas_ctrmm */
void cblas_ctrmm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_ctrmm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ctrmm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ctrmm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    } else {
        fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    }
    #else
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ctrmm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    *(void **) &fn = current_backend->cblas.cblas_ctrmm;
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

void flexiblas_chain_cblas_ctrmm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_ctrmm++;
    if ( hook_pos_cblas_ctrmm < __flexiblas_hooks->cblas_ctrmm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ctrmm.hook_function[hook_pos_cblas_ctrmm];
    } else {
        hook_pos_cblas_ctrmm = 0;
        *(void **) &fn = current_backend->cblas.cblas_ctrmm;
    }
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ctrmv = 0;
#endif

/* Wrapper for cblas_ctrmv */
void cblas_ctrmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctrmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ctrmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ctrmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ctrmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ctrmv;
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_ctrmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctrmv++;
    if ( hook_pos_cblas_ctrmv < __flexiblas_hooks->cblas_ctrmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ctrmv.hook_function[hook_pos_cblas_ctrmv];
    } else {
        hook_pos_cblas_ctrmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ctrmv;
    }
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ctrsm = 0;
#endif

/* Wrapper for cblas_ctrsm */
void cblas_ctrsm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_ctrsm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ctrsm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ctrsm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    } else {
        fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    }
    #else
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ctrsm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    *(void **) &fn = current_backend->cblas.cblas_ctrsm;
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

void flexiblas_chain_cblas_ctrsm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_ctrsm++;
    if ( hook_pos_cblas_ctrsm < __flexiblas_hooks->cblas_ctrsm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ctrsm.hook_function[hook_pos_cblas_ctrsm];
    } else {
        hook_pos_cblas_ctrsm = 0;
        *(void **) &fn = current_backend->cblas.cblas_ctrsm;
    }
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ctrsv = 0;
#endif

/* Wrapper for cblas_ctrsv */
void cblas_ctrsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctrsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ctrsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ctrsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ctrsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ctrsv;
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_ctrsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ctrsv++;
    if ( hook_pos_cblas_ctrsv < __flexiblas_hooks->cblas_ctrsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ctrsv.hook_function[hook_pos_cblas_ctrsv];
    } else {
        hook_pos_cblas_ctrsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ctrsv;
    }
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dasum = 0;
#endif

/* Wrapper for cblas_dasum */
double  cblas_dasum (const CBLAS_INT N, const double *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    double (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_dasum = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dasum;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dasum.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
double  flexiblas_real_cblas_dasum(const CBLAS_INT N, const double *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dasum;
    return fn(N, X, incX);
}

double flexiblas_chain_cblas_dasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_dasum++;
    if ( hook_pos_cblas_dasum < __flexiblas_hooks->cblas_dasum.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dasum.hook_function[hook_pos_cblas_dasum];
    } else {
        hook_pos_cblas_dasum = 0;
        *(void **) &fn = current_backend->cblas.cblas_dasum;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_daxpby = 0;
#endif

/* Wrapper for cblas_daxpby */
void cblas_daxpby (const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_daxpby = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_daxpby;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_daxpby.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX, beta, Y, incY);
    } else {
        fn(N, alpha, X, incX, beta, Y, incY);
    }
    #else
    fn(N, alpha, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_daxpby_(const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_daxpby;
    fn(N, alpha, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_daxpby_(const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_daxpby++;
    if ( hook_pos_cblas_daxpby < __flexiblas_hooks->cblas_daxpby.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_daxpby.hook_function[hook_pos_cblas_daxpby];
    } else {
        hook_pos_cblas_daxpby = 0;
        *(void **) &fn = current_backend->cblas.cblas_daxpby;
    }
    fn(N, alpha, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_daxpy = 0;
#endif

/* Wrapper for cblas_daxpy */
void cblas_daxpy (const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_daxpy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_daxpy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_daxpy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX, Y, incY);
    } else {
        fn(N, alpha, X, incX, Y, incY);
    }
    #else
    fn(N, alpha, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_daxpy_(const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_daxpy;
    fn(N, alpha, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_daxpy_(const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_daxpy++;
    if ( hook_pos_cblas_daxpy < __flexiblas_hooks->cblas_daxpy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_daxpy.hook_function[hook_pos_cblas_daxpy];
    } else {
        hook_pos_cblas_daxpy = 0;
        *(void **) &fn = current_backend->cblas.cblas_daxpy;
    }
    fn(N, alpha, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dcopy = 0;
#endif

/* Wrapper for cblas_dcopy */
void cblas_dcopy (const CBLAS_INT N, const double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY);
    } else {
        fn(N, X, incX, Y, incY);
    }
    #else
    fn(N, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dcopy_(const CBLAS_INT N, const double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_dcopy;
    fn(N, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_dcopy_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dcopy++;
    if ( hook_pos_cblas_dcopy < __flexiblas_hooks->cblas_dcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dcopy.hook_function[hook_pos_cblas_dcopy];
    } else {
        hook_pos_cblas_dcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_dcopy;
    }
    fn(N, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ddot = 0;
#endif

/* Wrapper for cblas_ddot */
double  cblas_ddot (const CBLAS_INT N, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    double (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    hook_pos_cblas_ddot = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ddot;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ddot.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX, Y, incY);
    } else {
        return fn(N, X, incX, Y, incY);
    }
    #else
    return fn(N, X, incX, Y, incY);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
double  flexiblas_real_cblas_ddot(const CBLAS_INT N, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_ddot;
    return fn(N, X, incX, Y, incY);
}

double flexiblas_chain_cblas_ddot(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    hook_pos_cblas_ddot++;
    if ( hook_pos_cblas_ddot < __flexiblas_hooks->cblas_ddot.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ddot.hook_function[hook_pos_cblas_ddot];
    } else {
        hook_pos_cblas_ddot = 0;
        *(void **) &fn = current_backend->cblas.cblas_ddot;
    }
    return fn(N, X, incX, Y, incY);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dgbmv = 0;
#endif

/* Wrapper for cblas_dgbmv */
void cblas_dgbmv (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dgbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dgbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dgbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dgbmv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_dgbmv;
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_dgbmv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dgbmv++;
    if ( hook_pos_cblas_dgbmv < __flexiblas_hooks->cblas_dgbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dgbmv.hook_function[hook_pos_cblas_dgbmv];
    } else {
        hook_pos_cblas_dgbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dgbmv;
    }
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dgeadd = 0;
#endif

/* Wrapper for cblas_dgeadd */
void cblas_dgeadd (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, double *a, const CBLAS_INT clda, const double cbeta, double *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const double cbeta, void *b, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const double cbeta, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_dgeadd = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dgeadd;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dgeadd.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    } else {
        fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    }
    #else
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dgeadd_(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, double *a, const CBLAS_INT clda, const double cbeta, double *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const double cbeta, void *b, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_dgeadd;
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    return;
}

void flexiblas_chain_cblas_dgeadd_(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const double cbeta, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const double cbeta, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_dgeadd++;
    if ( hook_pos_cblas_dgeadd < __flexiblas_hooks->cblas_dgeadd.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dgeadd.hook_function[hook_pos_cblas_dgeadd];
    } else {
        hook_pos_cblas_dgeadd = 0;
        *(void **) &fn = current_backend->cblas.cblas_dgeadd;
    }
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dgemm = 0;
#endif

/* Wrapper for cblas_dgemm */
void cblas_dgemm (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dgemm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dgemm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dgemm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dgemm_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_dgemm;
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_dgemm_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dgemm++;
    if ( hook_pos_cblas_dgemm < __flexiblas_hooks->cblas_dgemm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dgemm.hook_function[hook_pos_cblas_dgemm];
    } else {
        hook_pos_cblas_dgemm = 0;
        *(void **) &fn = current_backend->cblas.cblas_dgemm;
    }
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dgemmtr = 0;
#endif

/* Wrapper for cblas_dgemmtr */
void cblas_dgemmtr (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dgemmtr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dgemmtr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dgemmtr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dgemmtr_(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_dgemmtr;
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_dgemmtr_(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dgemmtr++;
    if ( hook_pos_cblas_dgemmtr < __flexiblas_hooks->cblas_dgemmtr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dgemmtr.hook_function[hook_pos_cblas_dgemmtr];
    } else {
        hook_pos_cblas_dgemmtr = 0;
        *(void **) &fn = current_backend->cblas.cblas_dgemmtr;
    }
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
/* Alternative function name for cblas_dgemmtr -- cblas_dgemmt */
#ifndef __APPLE__
void cblas_dgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc) __attribute__((alias(MTS(cblas_dgemmtr))));
#else
void cblas_dgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc) 
{
    cblas_dgemmtr(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
}

#endif

#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dgemv = 0;
#endif

/* Wrapper for cblas_dgemv */
void cblas_dgemv (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dgemv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dgemv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dgemv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dgemv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_dgemv;
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_dgemv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dgemv++;
    if ( hook_pos_cblas_dgemv < __flexiblas_hooks->cblas_dgemv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dgemv.hook_function[hook_pos_cblas_dgemv];
    } else {
        hook_pos_cblas_dgemv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dgemv;
    }
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dger = 0;
#endif

/* Wrapper for cblas_dger */
void cblas_dger (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY, double *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_dger = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dger;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dger.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dger_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY, double *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_dger;
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_dger_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_dger++;
    if ( hook_pos_cblas_dger < __flexiblas_hooks->cblas_dger.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dger.hook_function[hook_pos_cblas_dger];
    } else {
        hook_pos_cblas_dger = 0;
        *(void **) &fn = current_backend->cblas.cblas_dger;
    }
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dimatcopy = 0;
#endif

/* Wrapper for cblas_dimatcopy */
void cblas_dimatcopy (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, double *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    hook_pos_cblas_dimatcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dimatcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dimatcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    } else {
        fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    }
    #else
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dimatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, double *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_dimatcopy;
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    return;
}

void flexiblas_chain_cblas_dimatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    hook_pos_cblas_dimatcopy++;
    if ( hook_pos_cblas_dimatcopy < __flexiblas_hooks->cblas_dimatcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dimatcopy.hook_function[hook_pos_cblas_dimatcopy];
    } else {
        hook_pos_cblas_dimatcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_dimatcopy;
    }
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dnrm2 = 0;
#endif

/* Wrapper for cblas_dnrm2 */
double  cblas_dnrm2 (const CBLAS_INT N, const double *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    double (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_dnrm2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dnrm2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dnrm2.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
double  flexiblas_real_cblas_dnrm2(const CBLAS_INT N, const double *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dnrm2;
    return fn(N, X, incX);
}

double flexiblas_chain_cblas_dnrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_dnrm2++;
    if ( hook_pos_cblas_dnrm2 < __flexiblas_hooks->cblas_dnrm2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dnrm2.hook_function[hook_pos_cblas_dnrm2];
    } else {
        hook_pos_cblas_dnrm2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_dnrm2;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_domatcopy = 0;
#endif

/* Wrapper for cblas_domatcopy */
void cblas_domatcopy (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const double *a, const CBLAS_INT clda, double *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_domatcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_domatcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_domatcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    } else {
        fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    }
    #else
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_domatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const double *a, const CBLAS_INT clda, double *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_domatcopy;
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    return;
}

void flexiblas_chain_cblas_domatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_domatcopy++;
    if ( hook_pos_cblas_domatcopy < __flexiblas_hooks->cblas_domatcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_domatcopy.hook_function[hook_pos_cblas_domatcopy];
    } else {
        hook_pos_cblas_domatcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_domatcopy;
    }
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_drot = 0;
#endif

/* Wrapper for cblas_drot */
void cblas_drot (const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY, const double c, const double s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
    hook_pos_cblas_drot = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_drot;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_drot.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, c, s);
    } else {
        fn(N, X, incX, Y, incY, c, s);
    }
    #else
    fn(N, X, incX, Y, incY, c, s);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_drot_(const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY, const double c, const double s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
    *(void **) &fn = current_backend->cblas.cblas_drot;
    fn(N, X, incX, Y, incY, c, s);
    return;
}

void flexiblas_chain_cblas_drot_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
    hook_pos_cblas_drot++;
    if ( hook_pos_cblas_drot < __flexiblas_hooks->cblas_drot.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_drot.hook_function[hook_pos_cblas_drot];
    } else {
        hook_pos_cblas_drot = 0;
        *(void **) &fn = current_backend->cblas.cblas_drot;
    }
    fn(N, X, incX, Y, incY, c, s);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_drotg = 0;
#endif

/* Wrapper for cblas_drotg */
void cblas_drotg (double *a, double *b, double *c, double *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (void *a, void *b, void *c, void *s);
    hook_pos_cblas_drotg = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_drotg;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_drotg.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(a, b, c, s);
    } else {
        fn(a, b, c, s);
    }
    #else
    fn(a, b, c, s);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_drotg_(double *a, double *b, double *c, double *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    *(void **) &fn = current_backend->cblas.cblas_drotg;
    fn(a, b, c, s);
    return;
}

void flexiblas_chain_cblas_drotg_(void *a, void *b, void *c, void *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    hook_pos_cblas_drotg++;
    if ( hook_pos_cblas_drotg < __flexiblas_hooks->cblas_drotg.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_drotg.hook_function[hook_pos_cblas_drotg];
    } else {
        hook_pos_cblas_drotg = 0;
        *(void **) &fn = current_backend->cblas.cblas_drotg;
    }
    fn(a, b, c, s);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_drotm = 0;
#endif

/* Wrapper for cblas_drotm */
void cblas_drotm (const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY, const double *P)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
    hook_pos_cblas_drotm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_drotm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_drotm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, P);
    } else {
        fn(N, X, incX, Y, incY, P);
    }
    #else
    fn(N, X, incX, Y, incY, P);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_drotm_(const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY, const double *P)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
    *(void **) &fn = current_backend->cblas.cblas_drotm;
    fn(N, X, incX, Y, incY, P);
    return;
}

void flexiblas_chain_cblas_drotm_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
    hook_pos_cblas_drotm++;
    if ( hook_pos_cblas_drotm < __flexiblas_hooks->cblas_drotm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_drotm.hook_function[hook_pos_cblas_drotm];
    } else {
        hook_pos_cblas_drotm = 0;
        *(void **) &fn = current_backend->cblas.cblas_drotm;
    }
    fn(N, X, incX, Y, incY, P);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_drotmg = 0;
#endif

/* Wrapper for cblas_drotmg */
void cblas_drotmg (double *d1, double *d2, double *b1, const double b2, double *P)
{
    void (*fn) (void *d1, void *d2, void *b1, const double b2, void *P);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (void *d1, void *d2, void *b1, const double b2, void *P);
    hook_pos_cblas_drotmg = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_drotmg;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_drotmg.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(d1, d2, b1, b2, P);
    } else {
        fn(d1, d2, b1, b2, P);
    }
    #else
    fn(d1, d2, b1, b2, P);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_drotmg_(double *d1, double *d2, double *b1, const double b2, double *P)
{
    void (*fn) (void *d1, void *d2, void *b1, const double b2, void *P);
    *(void **) &fn = current_backend->cblas.cblas_drotmg;
    fn(d1, d2, b1, b2, P);
    return;
}

void flexiblas_chain_cblas_drotmg_(void *d1, void *d2, void *b1, const double b2, void *P)
{
    void (*fn) (void *d1, void *d2, void *b1, const double b2, void *P);
    hook_pos_cblas_drotmg++;
    if ( hook_pos_cblas_drotmg < __flexiblas_hooks->cblas_drotmg.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_drotmg.hook_function[hook_pos_cblas_drotmg];
    } else {
        hook_pos_cblas_drotmg = 0;
        *(void **) &fn = current_backend->cblas.cblas_drotmg;
    }
    fn(d1, d2, b1, b2, P);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dsbmv = 0;
#endif

/* Wrapper for cblas_dsbmv */
void cblas_dsbmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dsbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dsbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dsbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dsbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_dsbmv;
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_dsbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dsbmv++;
    if ( hook_pos_cblas_dsbmv < __flexiblas_hooks->cblas_dsbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dsbmv.hook_function[hook_pos_cblas_dsbmv];
    } else {
        hook_pos_cblas_dsbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dsbmv;
    }
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dscal = 0;
#endif

/* Wrapper for cblas_dscal */
void cblas_dscal (const CBLAS_INT N, const double alpha, double *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dscal = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dscal;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dscal.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX);
    } else {
        fn(N, alpha, X, incX);
    }
    #else
    fn(N, alpha, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dscal_(const CBLAS_INT N, const double alpha, double *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dscal;
    fn(N, alpha, X, incX);
    return;
}

void flexiblas_chain_cblas_dscal_(const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dscal++;
    if ( hook_pos_cblas_dscal < __flexiblas_hooks->cblas_dscal.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dscal.hook_function[hook_pos_cblas_dscal];
    } else {
        hook_pos_cblas_dscal = 0;
        *(void **) &fn = current_backend->cblas.cblas_dscal;
    }
    fn(N, alpha, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dsdot = 0;
#endif

/* Wrapper for cblas_dsdot */
double  cblas_dsdot (const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    double (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dsdot = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dsdot;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dsdot.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX, Y, incY);
    } else {
        return fn(N, X, incX, Y, incY);
    }
    #else
    return fn(N, X, incX, Y, incY);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
double  flexiblas_real_cblas_dsdot(const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_dsdot;
    return fn(N, X, incX, Y, incY);
}

double flexiblas_chain_cblas_dsdot(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dsdot++;
    if ( hook_pos_cblas_dsdot < __flexiblas_hooks->cblas_dsdot.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dsdot.hook_function[hook_pos_cblas_dsdot];
    } else {
        hook_pos_cblas_dsdot = 0;
        *(void **) &fn = current_backend->cblas.cblas_dsdot;
    }
    return fn(N, X, incX, Y, incY);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dspmv = 0;
#endif

/* Wrapper for cblas_dspmv */
void cblas_dspmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *Ap, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *Ap, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *Ap, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dspmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dspmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dspmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dspmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *Ap, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *Ap, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_dspmv;
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_dspmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *Ap, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *Ap, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dspmv++;
    if ( hook_pos_cblas_dspmv < __flexiblas_hooks->cblas_dspmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dspmv.hook_function[hook_pos_cblas_dspmv];
    } else {
        hook_pos_cblas_dspmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dspmv;
    }
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dspr = 0;
#endif

/* Wrapper for cblas_dspr */
void cblas_dspr (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, double *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Ap);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Ap);
    hook_pos_cblas_dspr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dspr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dspr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Ap);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Ap);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Ap);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dspr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, double *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Ap);
    *(void **) &fn = current_backend->cblas.cblas_dspr;
    fn(layout, Uplo, N, alpha, X, incX, Ap);
    return;
}

void flexiblas_chain_cblas_dspr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Ap);
    hook_pos_cblas_dspr++;
    if ( hook_pos_cblas_dspr < __flexiblas_hooks->cblas_dspr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dspr.hook_function[hook_pos_cblas_dspr];
    } else {
        hook_pos_cblas_dspr = 0;
        *(void **) &fn = current_backend->cblas.cblas_dspr;
    }
    fn(layout, Uplo, N, alpha, X, incX, Ap);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dspr2 = 0;
#endif

/* Wrapper for cblas_dspr2 */
void cblas_dspr2 (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY, double *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
    hook_pos_cblas_dspr2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dspr2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dspr2.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dspr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY, double *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
    *(void **) &fn = current_backend->cblas.cblas_dspr2;
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    return;
}

void flexiblas_chain_cblas_dspr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
    hook_pos_cblas_dspr2++;
    if ( hook_pos_cblas_dspr2 < __flexiblas_hooks->cblas_dspr2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dspr2.hook_function[hook_pos_cblas_dspr2];
    } else {
        hook_pos_cblas_dspr2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_dspr2;
    }
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dswap = 0;
#endif

/* Wrapper for cblas_dswap */
void cblas_dswap (const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dswap = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dswap;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dswap.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY);
    } else {
        fn(N, X, incX, Y, incY);
    }
    #else
    fn(N, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dswap_(const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_dswap;
    fn(N, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_dswap_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dswap++;
    if ( hook_pos_cblas_dswap < __flexiblas_hooks->cblas_dswap.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dswap.hook_function[hook_pos_cblas_dswap];
    } else {
        hook_pos_cblas_dswap = 0;
        *(void **) &fn = current_backend->cblas.cblas_dswap;
    }
    fn(N, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dsymm = 0;
#endif

/* Wrapper for cblas_dsymm */
void cblas_dsymm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dsymm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dsymm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dsymm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dsymm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_dsymm;
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_dsymm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dsymm++;
    if ( hook_pos_cblas_dsymm < __flexiblas_hooks->cblas_dsymm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dsymm.hook_function[hook_pos_cblas_dsymm];
    } else {
        hook_pos_cblas_dsymm = 0;
        *(void **) &fn = current_backend->cblas.cblas_dsymm;
    }
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dsymv = 0;
#endif

/* Wrapper for cblas_dsymv */
void cblas_dsymv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dsymv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dsymv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dsymv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dsymv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_dsymv;
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_dsymv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_dsymv++;
    if ( hook_pos_cblas_dsymv < __flexiblas_hooks->cblas_dsymv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dsymv.hook_function[hook_pos_cblas_dsymv];
    } else {
        hook_pos_cblas_dsymv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dsymv;
    }
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dsyr = 0;
#endif

/* Wrapper for cblas_dsyr */
void cblas_dsyr (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, double *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    hook_pos_cblas_dsyr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dsyr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dsyr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, A, lda);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, A, lda);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dsyr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, double *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_dsyr;
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    return;
}

void flexiblas_chain_cblas_dsyr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    hook_pos_cblas_dsyr++;
    if ( hook_pos_cblas_dsyr < __flexiblas_hooks->cblas_dsyr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dsyr.hook_function[hook_pos_cblas_dsyr];
    } else {
        hook_pos_cblas_dsyr = 0;
        *(void **) &fn = current_backend->cblas.cblas_dsyr;
    }
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dsyr2 = 0;
#endif

/* Wrapper for cblas_dsyr2 */
void cblas_dsyr2 (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY, double *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_dsyr2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dsyr2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dsyr2.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dsyr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY, double *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_dsyr2;
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_dsyr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_dsyr2++;
    if ( hook_pos_cblas_dsyr2 < __flexiblas_hooks->cblas_dsyr2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dsyr2.hook_function[hook_pos_cblas_dsyr2];
    } else {
        hook_pos_cblas_dsyr2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_dsyr2;
    }
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dsyr2k = 0;
#endif

/* Wrapper for cblas_dsyr2k */
void cblas_dsyr2k (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dsyr2k = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dsyr2k;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dsyr2k.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dsyr2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_dsyr2k;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_dsyr2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dsyr2k++;
    if ( hook_pos_cblas_dsyr2k < __flexiblas_hooks->cblas_dsyr2k.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dsyr2k.hook_function[hook_pos_cblas_dsyr2k];
    } else {
        hook_pos_cblas_dsyr2k = 0;
        *(void **) &fn = current_backend->cblas.cblas_dsyr2k;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dsyrk = 0;
#endif

/* Wrapper for cblas_dsyrk */
void cblas_dsyrk (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dsyrk = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dsyrk;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dsyrk.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dsyrk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double beta, double *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_dsyrk;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_dsyrk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_dsyrk++;
    if ( hook_pos_cblas_dsyrk < __flexiblas_hooks->cblas_dsyrk.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dsyrk.hook_function[hook_pos_cblas_dsyrk];
    } else {
        hook_pos_cblas_dsyrk = 0;
        *(void **) &fn = current_backend->cblas.cblas_dsyrk;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dtbmv = 0;
#endif

/* Wrapper for cblas_dtbmv */
void cblas_dtbmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dtbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dtbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dtbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dtbmv;
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_dtbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtbmv++;
    if ( hook_pos_cblas_dtbmv < __flexiblas_hooks->cblas_dtbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dtbmv.hook_function[hook_pos_cblas_dtbmv];
    } else {
        hook_pos_cblas_dtbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dtbmv;
    }
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dtbsv = 0;
#endif

/* Wrapper for cblas_dtbsv */
void cblas_dtbsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtbsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dtbsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dtbsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dtbsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dtbsv;
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_dtbsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtbsv++;
    if ( hook_pos_cblas_dtbsv < __flexiblas_hooks->cblas_dtbsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dtbsv.hook_function[hook_pos_cblas_dtbsv];
    } else {
        hook_pos_cblas_dtbsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dtbsv;
    }
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dtpmv = 0;
#endif

/* Wrapper for cblas_dtpmv */
void cblas_dtpmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *Ap, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtpmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dtpmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dtpmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dtpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *Ap, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dtpmv;
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

void flexiblas_chain_cblas_dtpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtpmv++;
    if ( hook_pos_cblas_dtpmv < __flexiblas_hooks->cblas_dtpmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dtpmv.hook_function[hook_pos_cblas_dtpmv];
    } else {
        hook_pos_cblas_dtpmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dtpmv;
    }
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dtpsv = 0;
#endif

/* Wrapper for cblas_dtpsv */
void cblas_dtpsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *Ap, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtpsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dtpsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dtpsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dtpsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *Ap, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dtpsv;
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

void flexiblas_chain_cblas_dtpsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtpsv++;
    if ( hook_pos_cblas_dtpsv < __flexiblas_hooks->cblas_dtpsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dtpsv.hook_function[hook_pos_cblas_dtpsv];
    } else {
        hook_pos_cblas_dtpsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dtpsv;
    }
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dtrmm = 0;
#endif

/* Wrapper for cblas_dtrmm */
void cblas_dtrmm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, double *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_dtrmm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dtrmm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dtrmm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    } else {
        fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    }
    #else
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dtrmm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, double *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    *(void **) &fn = current_backend->cblas.cblas_dtrmm;
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

void flexiblas_chain_cblas_dtrmm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_dtrmm++;
    if ( hook_pos_cblas_dtrmm < __flexiblas_hooks->cblas_dtrmm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dtrmm.hook_function[hook_pos_cblas_dtrmm];
    } else {
        hook_pos_cblas_dtrmm = 0;
        *(void **) &fn = current_backend->cblas.cblas_dtrmm;
    }
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dtrmv = 0;
#endif

/* Wrapper for cblas_dtrmv */
void cblas_dtrmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtrmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dtrmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dtrmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dtrmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dtrmv;
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_dtrmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtrmv++;
    if ( hook_pos_cblas_dtrmv < __flexiblas_hooks->cblas_dtrmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dtrmv.hook_function[hook_pos_cblas_dtrmv];
    } else {
        hook_pos_cblas_dtrmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dtrmv;
    }
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dtrsm = 0;
#endif

/* Wrapper for cblas_dtrsm */
void cblas_dtrsm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, double *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_dtrsm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dtrsm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dtrsm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    } else {
        fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    }
    #else
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dtrsm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, double *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    *(void **) &fn = current_backend->cblas.cblas_dtrsm;
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

void flexiblas_chain_cblas_dtrsm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_dtrsm++;
    if ( hook_pos_cblas_dtrsm < __flexiblas_hooks->cblas_dtrsm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dtrsm.hook_function[hook_pos_cblas_dtrsm];
    } else {
        hook_pos_cblas_dtrsm = 0;
        *(void **) &fn = current_backend->cblas.cblas_dtrsm;
    }
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dtrsv = 0;
#endif

/* Wrapper for cblas_dtrsv */
void cblas_dtrsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtrsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dtrsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dtrsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_dtrsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dtrsv;
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_dtrsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_dtrsv++;
    if ( hook_pos_cblas_dtrsv < __flexiblas_hooks->cblas_dtrsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dtrsv.hook_function[hook_pos_cblas_dtrsv];
    } else {
        hook_pos_cblas_dtrsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_dtrsv;
    }
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dzasum = 0;
#endif

/* Wrapper for cblas_dzasum */
double  cblas_dzasum (const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    double (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_dzasum = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dzasum;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dzasum.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
double  flexiblas_real_cblas_dzasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dzasum;
    return fn(N, X, incX);
}

double flexiblas_chain_cblas_dzasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_dzasum++;
    if ( hook_pos_cblas_dzasum < __flexiblas_hooks->cblas_dzasum.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dzasum.hook_function[hook_pos_cblas_dzasum];
    } else {
        hook_pos_cblas_dzasum = 0;
        *(void **) &fn = current_backend->cblas.cblas_dzasum;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_dznrm2 = 0;
#endif

/* Wrapper for cblas_dznrm2 */
double  cblas_dznrm2 (const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    double (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_dznrm2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_dznrm2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_dznrm2.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
double  flexiblas_real_cblas_dznrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_dznrm2;
    return fn(N, X, incX);
}

double flexiblas_chain_cblas_dznrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    double (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_dznrm2++;
    if ( hook_pos_cblas_dznrm2 < __flexiblas_hooks->cblas_dznrm2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_dznrm2.hook_function[hook_pos_cblas_dznrm2];
    } else {
        hook_pos_cblas_dznrm2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_dznrm2;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_icamax = 0;
#endif

/* Wrapper for cblas_icamax */
size_t  cblas_icamax (const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    size_t (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_icamax = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_icamax;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_icamax.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
size_t  flexiblas_real_cblas_icamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_icamax;
    return fn(N, X, incX);
}

size_t flexiblas_chain_cblas_icamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_icamax++;
    if ( hook_pos_cblas_icamax < __flexiblas_hooks->cblas_icamax.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_icamax.hook_function[hook_pos_cblas_icamax];
    } else {
        hook_pos_cblas_icamax = 0;
        *(void **) &fn = current_backend->cblas.cblas_icamax;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_idamax = 0;
#endif

/* Wrapper for cblas_idamax */
size_t  cblas_idamax (const CBLAS_INT N, const double *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    size_t (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_idamax = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_idamax;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_idamax.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
size_t  flexiblas_real_cblas_idamax(const CBLAS_INT N, const double *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_idamax;
    return fn(N, X, incX);
}

size_t flexiblas_chain_cblas_idamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_idamax++;
    if ( hook_pos_cblas_idamax < __flexiblas_hooks->cblas_idamax.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_idamax.hook_function[hook_pos_cblas_idamax];
    } else {
        hook_pos_cblas_idamax = 0;
        *(void **) &fn = current_backend->cblas.cblas_idamax;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_isamax = 0;
#endif

/* Wrapper for cblas_isamax */
size_t  cblas_isamax (const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    size_t (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_isamax = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_isamax;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_isamax.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
size_t  flexiblas_real_cblas_isamax(const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_isamax;
    return fn(N, X, incX);
}

size_t flexiblas_chain_cblas_isamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_isamax++;
    if ( hook_pos_cblas_isamax < __flexiblas_hooks->cblas_isamax.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_isamax.hook_function[hook_pos_cblas_isamax];
    } else {
        hook_pos_cblas_isamax = 0;
        *(void **) &fn = current_backend->cblas.cblas_isamax;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_izamax = 0;
#endif

/* Wrapper for cblas_izamax */
size_t  cblas_izamax (const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    size_t (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_izamax = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_izamax;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_izamax.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
size_t  flexiblas_real_cblas_izamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_izamax;
    return fn(N, X, incX);
}

size_t flexiblas_chain_cblas_izamax(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    size_t (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_izamax++;
    if ( hook_pos_cblas_izamax < __flexiblas_hooks->cblas_izamax.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_izamax.hook_function[hook_pos_cblas_izamax];
    } else {
        hook_pos_cblas_izamax = 0;
        *(void **) &fn = current_backend->cblas.cblas_izamax;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sasum = 0;
#endif

/* Wrapper for cblas_sasum */
float  cblas_sasum (const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    float (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_sasum = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sasum;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sasum.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
float  flexiblas_real_cblas_sasum(const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_sasum;
    return fn(N, X, incX);
}

float flexiblas_chain_cblas_sasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_sasum++;
    if ( hook_pos_cblas_sasum < __flexiblas_hooks->cblas_sasum.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sasum.hook_function[hook_pos_cblas_sasum];
    } else {
        hook_pos_cblas_sasum = 0;
        *(void **) &fn = current_backend->cblas.cblas_sasum;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_saxpby = 0;
#endif

/* Wrapper for cblas_saxpby */
void cblas_saxpby (const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_saxpby = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_saxpby;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_saxpby.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX, beta, Y, incY);
    } else {
        fn(N, alpha, X, incX, beta, Y, incY);
    }
    #else
    fn(N, alpha, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_saxpby_(const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_saxpby;
    fn(N, alpha, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_saxpby_(const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_saxpby++;
    if ( hook_pos_cblas_saxpby < __flexiblas_hooks->cblas_saxpby.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_saxpby.hook_function[hook_pos_cblas_saxpby];
    } else {
        hook_pos_cblas_saxpby = 0;
        *(void **) &fn = current_backend->cblas.cblas_saxpby;
    }
    fn(N, alpha, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_saxpy = 0;
#endif

/* Wrapper for cblas_saxpy */
void cblas_saxpy (const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_saxpy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_saxpy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_saxpy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX, Y, incY);
    } else {
        fn(N, alpha, X, incX, Y, incY);
    }
    #else
    fn(N, alpha, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_saxpy_(const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_saxpy;
    fn(N, alpha, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_saxpy_(const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_saxpy++;
    if ( hook_pos_cblas_saxpy < __flexiblas_hooks->cblas_saxpy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_saxpy.hook_function[hook_pos_cblas_saxpy];
    } else {
        hook_pos_cblas_saxpy = 0;
        *(void **) &fn = current_backend->cblas.cblas_saxpy;
    }
    fn(N, alpha, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_scasum = 0;
#endif

/* Wrapper for cblas_scasum */
float  cblas_scasum (const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    float (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_scasum = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_scasum;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_scasum.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
float  flexiblas_real_cblas_scasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_scasum;
    return fn(N, X, incX);
}

float flexiblas_chain_cblas_scasum(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_scasum++;
    if ( hook_pos_cblas_scasum < __flexiblas_hooks->cblas_scasum.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_scasum.hook_function[hook_pos_cblas_scasum];
    } else {
        hook_pos_cblas_scasum = 0;
        *(void **) &fn = current_backend->cblas.cblas_scasum;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_scnrm2 = 0;
#endif

/* Wrapper for cblas_scnrm2 */
float  cblas_scnrm2 (const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    float (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_scnrm2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_scnrm2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_scnrm2.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
float  flexiblas_real_cblas_scnrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_scnrm2;
    return fn(N, X, incX);
}

float flexiblas_chain_cblas_scnrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_scnrm2++;
    if ( hook_pos_cblas_scnrm2 < __flexiblas_hooks->cblas_scnrm2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_scnrm2.hook_function[hook_pos_cblas_scnrm2];
    } else {
        hook_pos_cblas_scnrm2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_scnrm2;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_scopy = 0;
#endif

/* Wrapper for cblas_scopy */
void cblas_scopy (const CBLAS_INT N, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_scopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_scopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_scopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY);
    } else {
        fn(N, X, incX, Y, incY);
    }
    #else
    fn(N, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_scopy_(const CBLAS_INT N, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_scopy;
    fn(N, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_scopy_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_scopy++;
    if ( hook_pos_cblas_scopy < __flexiblas_hooks->cblas_scopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_scopy.hook_function[hook_pos_cblas_scopy];
    } else {
        hook_pos_cblas_scopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_scopy;
    }
    fn(N, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sdot = 0;
#endif

/* Wrapper for cblas_sdot */
float  cblas_sdot (const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    float (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sdot = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sdot;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sdot.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX, Y, incY);
    } else {
        return fn(N, X, incX, Y, incY);
    }
    #else
    return fn(N, X, incX, Y, incY);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
float  flexiblas_real_cblas_sdot(const CBLAS_INT N, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_sdot;
    return fn(N, X, incX, Y, incY);
}

float flexiblas_chain_cblas_sdot(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sdot++;
    if ( hook_pos_cblas_sdot < __flexiblas_hooks->cblas_sdot.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sdot.hook_function[hook_pos_cblas_sdot];
    } else {
        hook_pos_cblas_sdot = 0;
        *(void **) &fn = current_backend->cblas.cblas_sdot;
    }
    return fn(N, X, incX, Y, incY);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sdsdot = 0;
#endif

/* Wrapper for cblas_sdsdot */
float  cblas_sdsdot (const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
    float (*fn) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    float (*fn_hook) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sdsdot = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sdsdot;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sdsdot.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, alpha, X, incX, Y, incY);
    } else {
        return fn(N, alpha, X, incX, Y, incY);
    }
    #else
    return fn(N, alpha, X, incX, Y, incY);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
float  flexiblas_real_cblas_sdsdot(const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
    float (*fn) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_sdsdot;
    return fn(N, alpha, X, incX, Y, incY);
}

float flexiblas_chain_cblas_sdsdot(const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY)
{
    float (*fn) (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sdsdot++;
    if ( hook_pos_cblas_sdsdot < __flexiblas_hooks->cblas_sdsdot.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sdsdot.hook_function[hook_pos_cblas_sdsdot];
    } else {
        hook_pos_cblas_sdsdot = 0;
        *(void **) &fn = current_backend->cblas.cblas_sdsdot;
    }
    return fn(N, alpha, X, incX, Y, incY);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sgbmv = 0;
#endif

/* Wrapper for cblas_sgbmv */
void cblas_sgbmv (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sgbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sgbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sgbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sgbmv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_sgbmv;
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_sgbmv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sgbmv++;
    if ( hook_pos_cblas_sgbmv < __flexiblas_hooks->cblas_sgbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sgbmv.hook_function[hook_pos_cblas_sgbmv];
    } else {
        hook_pos_cblas_sgbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_sgbmv;
    }
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sgeadd = 0;
#endif

/* Wrapper for cblas_sgeadd */
void cblas_sgeadd (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda, const float cbeta, float *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const float cbeta, void *b, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const float cbeta, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_sgeadd = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sgeadd;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sgeadd.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    } else {
        fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    }
    #else
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sgeadd_(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda, const float cbeta, float *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const float cbeta, void *b, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_sgeadd;
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    return;
}

void flexiblas_chain_cblas_sgeadd_(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const float cbeta, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const float cbeta, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_sgeadd++;
    if ( hook_pos_cblas_sgeadd < __flexiblas_hooks->cblas_sgeadd.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sgeadd.hook_function[hook_pos_cblas_sgeadd];
    } else {
        hook_pos_cblas_sgeadd = 0;
        *(void **) &fn = current_backend->cblas.cblas_sgeadd;
    }
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sgemm = 0;
#endif

/* Wrapper for cblas_sgemm */
void cblas_sgemm (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_sgemm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sgemm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sgemm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sgemm_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_sgemm;
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_sgemm_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_sgemm++;
    if ( hook_pos_cblas_sgemm < __flexiblas_hooks->cblas_sgemm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sgemm.hook_function[hook_pos_cblas_sgemm];
    } else {
        hook_pos_cblas_sgemm = 0;
        *(void **) &fn = current_backend->cblas.cblas_sgemm;
    }
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sgemmtr = 0;
#endif

/* Wrapper for cblas_sgemmtr */
void cblas_sgemmtr (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_sgemmtr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sgemmtr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sgemmtr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sgemmtr_(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_sgemmtr;
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_sgemmtr_(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_sgemmtr++;
    if ( hook_pos_cblas_sgemmtr < __flexiblas_hooks->cblas_sgemmtr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sgemmtr.hook_function[hook_pos_cblas_sgemmtr];
    } else {
        hook_pos_cblas_sgemmtr = 0;
        *(void **) &fn = current_backend->cblas.cblas_sgemmtr;
    }
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
/* Alternative function name for cblas_sgemmtr -- cblas_sgemmt */
#ifndef __APPLE__
void cblas_sgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc) __attribute__((alias(MTS(cblas_sgemmtr))));
#else
void cblas_sgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc) 
{
    cblas_sgemmtr(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
}

#endif

#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sgemv = 0;
#endif

/* Wrapper for cblas_sgemv */
void cblas_sgemv (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sgemv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sgemv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sgemv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sgemv_(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_sgemv;
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_sgemv_(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sgemv++;
    if ( hook_pos_cblas_sgemv < __flexiblas_hooks->cblas_sgemv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sgemv.hook_function[hook_pos_cblas_sgemv];
    } else {
        hook_pos_cblas_sgemv = 0;
        *(void **) &fn = current_backend->cblas.cblas_sgemv;
    }
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sger = 0;
#endif

/* Wrapper for cblas_sger */
void cblas_sger (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY, float *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_sger = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sger;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sger.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sger_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY, float *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_sger;
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_sger_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_sger++;
    if ( hook_pos_cblas_sger < __flexiblas_hooks->cblas_sger.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sger.hook_function[hook_pos_cblas_sger];
    } else {
        hook_pos_cblas_sger = 0;
        *(void **) &fn = current_backend->cblas.cblas_sger;
    }
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_simatcopy = 0;
#endif

/* Wrapper for cblas_simatcopy */
void cblas_simatcopy (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    hook_pos_cblas_simatcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_simatcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_simatcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    } else {
        fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    }
    #else
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_simatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_simatcopy;
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    return;
}

void flexiblas_chain_cblas_simatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    hook_pos_cblas_simatcopy++;
    if ( hook_pos_cblas_simatcopy < __flexiblas_hooks->cblas_simatcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_simatcopy.hook_function[hook_pos_cblas_simatcopy];
    } else {
        hook_pos_cblas_simatcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_simatcopy;
    }
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_snrm2 = 0;
#endif

/* Wrapper for cblas_snrm2 */
float  cblas_snrm2 (const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    float (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_snrm2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_snrm2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_snrm2.hook_function[0];

    if ( fn_hook != NULL ) {
        return fn_hook(N, X, incX);
    } else {
        return fn(N, X, incX);
    }
    #else
    return fn(N, X, incX);
    #endif
}

#ifdef FLEXIBLAS_HOOK_API
float  flexiblas_real_cblas_snrm2(const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_snrm2;
    return fn(N, X, incX);
}

float flexiblas_chain_cblas_snrm2(const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
    float (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX);
    hook_pos_cblas_snrm2++;
    if ( hook_pos_cblas_snrm2 < __flexiblas_hooks->cblas_snrm2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_snrm2.hook_function[hook_pos_cblas_snrm2];
    } else {
        hook_pos_cblas_snrm2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_snrm2;
    }
    return fn(N, X, incX);
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_somatcopy = 0;
#endif

/* Wrapper for cblas_somatcopy */
void cblas_somatcopy (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const float *a, const CBLAS_INT clda, float *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_somatcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_somatcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_somatcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    } else {
        fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    }
    #else
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_somatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const float *a, const CBLAS_INT clda, float *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_somatcopy;
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    return;
}

void flexiblas_chain_cblas_somatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_somatcopy++;
    if ( hook_pos_cblas_somatcopy < __flexiblas_hooks->cblas_somatcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_somatcopy.hook_function[hook_pos_cblas_somatcopy];
    } else {
        hook_pos_cblas_somatcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_somatcopy;
    }
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_srot = 0;
#endif

/* Wrapper for cblas_srot */
void cblas_srot (const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY, const float c, const float s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
    hook_pos_cblas_srot = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_srot;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_srot.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, c, s);
    } else {
        fn(N, X, incX, Y, incY, c, s);
    }
    #else
    fn(N, X, incX, Y, incY, c, s);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_srot_(const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY, const float c, const float s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
    *(void **) &fn = current_backend->cblas.cblas_srot;
    fn(N, X, incX, Y, incY, c, s);
    return;
}

void flexiblas_chain_cblas_srot_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s);
    hook_pos_cblas_srot++;
    if ( hook_pos_cblas_srot < __flexiblas_hooks->cblas_srot.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_srot.hook_function[hook_pos_cblas_srot];
    } else {
        hook_pos_cblas_srot = 0;
        *(void **) &fn = current_backend->cblas.cblas_srot;
    }
    fn(N, X, incX, Y, incY, c, s);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_srotg = 0;
#endif

/* Wrapper for cblas_srotg */
void cblas_srotg (float *a, float *b, float *c, float *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (void *a, void *b, void *c, void *s);
    hook_pos_cblas_srotg = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_srotg;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_srotg.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(a, b, c, s);
    } else {
        fn(a, b, c, s);
    }
    #else
    fn(a, b, c, s);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_srotg_(float *a, float *b, float *c, float *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    *(void **) &fn = current_backend->cblas.cblas_srotg;
    fn(a, b, c, s);
    return;
}

void flexiblas_chain_cblas_srotg_(void *a, void *b, void *c, void *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    hook_pos_cblas_srotg++;
    if ( hook_pos_cblas_srotg < __flexiblas_hooks->cblas_srotg.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_srotg.hook_function[hook_pos_cblas_srotg];
    } else {
        hook_pos_cblas_srotg = 0;
        *(void **) &fn = current_backend->cblas.cblas_srotg;
    }
    fn(a, b, c, s);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_srotm = 0;
#endif

/* Wrapper for cblas_srotm */
void cblas_srotm (const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY, const float *P)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
    hook_pos_cblas_srotm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_srotm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_srotm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, P);
    } else {
        fn(N, X, incX, Y, incY, P);
    }
    #else
    fn(N, X, incX, Y, incY, P);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_srotm_(const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY, const float *P)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
    *(void **) &fn = current_backend->cblas.cblas_srotm;
    fn(N, X, incX, Y, incY, P);
    return;
}

void flexiblas_chain_cblas_srotm_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P);
    hook_pos_cblas_srotm++;
    if ( hook_pos_cblas_srotm < __flexiblas_hooks->cblas_srotm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_srotm.hook_function[hook_pos_cblas_srotm];
    } else {
        hook_pos_cblas_srotm = 0;
        *(void **) &fn = current_backend->cblas.cblas_srotm;
    }
    fn(N, X, incX, Y, incY, P);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_srotmg = 0;
#endif

/* Wrapper for cblas_srotmg */
void cblas_srotmg (float *d1, float *d2, float *b1, const float b2, float *P)
{
    void (*fn) (void *d1, void *d2, void *b1, const float b2, void *P);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (void *d1, void *d2, void *b1, const float b2, void *P);
    hook_pos_cblas_srotmg = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_srotmg;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_srotmg.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(d1, d2, b1, b2, P);
    } else {
        fn(d1, d2, b1, b2, P);
    }
    #else
    fn(d1, d2, b1, b2, P);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_srotmg_(float *d1, float *d2, float *b1, const float b2, float *P)
{
    void (*fn) (void *d1, void *d2, void *b1, const float b2, void *P);
    *(void **) &fn = current_backend->cblas.cblas_srotmg;
    fn(d1, d2, b1, b2, P);
    return;
}

void flexiblas_chain_cblas_srotmg_(void *d1, void *d2, void *b1, const float b2, void *P)
{
    void (*fn) (void *d1, void *d2, void *b1, const float b2, void *P);
    hook_pos_cblas_srotmg++;
    if ( hook_pos_cblas_srotmg < __flexiblas_hooks->cblas_srotmg.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_srotmg.hook_function[hook_pos_cblas_srotmg];
    } else {
        hook_pos_cblas_srotmg = 0;
        *(void **) &fn = current_backend->cblas.cblas_srotmg;
    }
    fn(d1, d2, b1, b2, P);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ssbmv = 0;
#endif

/* Wrapper for cblas_ssbmv */
void cblas_ssbmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_ssbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ssbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ssbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ssbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_ssbmv;
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_ssbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_ssbmv++;
    if ( hook_pos_cblas_ssbmv < __flexiblas_hooks->cblas_ssbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ssbmv.hook_function[hook_pos_cblas_ssbmv];
    } else {
        hook_pos_cblas_ssbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ssbmv;
    }
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sscal = 0;
#endif

/* Wrapper for cblas_sscal */
void cblas_sscal (const CBLAS_INT N, const float alpha, float *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_sscal = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sscal;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sscal.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX);
    } else {
        fn(N, alpha, X, incX);
    }
    #else
    fn(N, alpha, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sscal_(const CBLAS_INT N, const float alpha, float *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_sscal;
    fn(N, alpha, X, incX);
    return;
}

void flexiblas_chain_cblas_sscal_(const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_sscal++;
    if ( hook_pos_cblas_sscal < __flexiblas_hooks->cblas_sscal.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sscal.hook_function[hook_pos_cblas_sscal];
    } else {
        hook_pos_cblas_sscal = 0;
        *(void **) &fn = current_backend->cblas.cblas_sscal;
    }
    fn(N, alpha, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sspmv = 0;
#endif

/* Wrapper for cblas_sspmv */
void cblas_sspmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *Ap, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *Ap, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *Ap, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sspmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sspmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sspmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sspmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *Ap, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *Ap, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_sspmv;
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_sspmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *Ap, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *Ap, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sspmv++;
    if ( hook_pos_cblas_sspmv < __flexiblas_hooks->cblas_sspmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sspmv.hook_function[hook_pos_cblas_sspmv];
    } else {
        hook_pos_cblas_sspmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_sspmv;
    }
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sspr = 0;
#endif

/* Wrapper for cblas_sspr */
void cblas_sspr (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Ap);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Ap);
    hook_pos_cblas_sspr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sspr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sspr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Ap);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Ap);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Ap);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sspr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Ap);
    *(void **) &fn = current_backend->cblas.cblas_sspr;
    fn(layout, Uplo, N, alpha, X, incX, Ap);
    return;
}

void flexiblas_chain_cblas_sspr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Ap);
    hook_pos_cblas_sspr++;
    if ( hook_pos_cblas_sspr < __flexiblas_hooks->cblas_sspr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sspr.hook_function[hook_pos_cblas_sspr];
    } else {
        hook_pos_cblas_sspr = 0;
        *(void **) &fn = current_backend->cblas.cblas_sspr;
    }
    fn(layout, Uplo, N, alpha, X, incX, Ap);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sspr2 = 0;
#endif

/* Wrapper for cblas_sspr2 */
void cblas_sspr2 (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY, float *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
    hook_pos_cblas_sspr2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sspr2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sspr2.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sspr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY, float *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
    *(void **) &fn = current_backend->cblas.cblas_sspr2;
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    return;
}

void flexiblas_chain_cblas_sspr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A);
    hook_pos_cblas_sspr2++;
    if ( hook_pos_cblas_sspr2 < __flexiblas_hooks->cblas_sspr2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sspr2.hook_function[hook_pos_cblas_sspr2];
    } else {
        hook_pos_cblas_sspr2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_sspr2;
    }
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_sswap = 0;
#endif

/* Wrapper for cblas_sswap */
void cblas_sswap (const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sswap = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_sswap;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_sswap.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY);
    } else {
        fn(N, X, incX, Y, incY);
    }
    #else
    fn(N, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_sswap_(const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_sswap;
    fn(N, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_sswap_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_sswap++;
    if ( hook_pos_cblas_sswap < __flexiblas_hooks->cblas_sswap.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_sswap.hook_function[hook_pos_cblas_sswap];
    } else {
        hook_pos_cblas_sswap = 0;
        *(void **) &fn = current_backend->cblas.cblas_sswap;
    }
    fn(N, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ssymm = 0;
#endif

/* Wrapper for cblas_ssymm */
void cblas_ssymm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_ssymm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ssymm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ssymm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ssymm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_ssymm;
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_ssymm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_ssymm++;
    if ( hook_pos_cblas_ssymm < __flexiblas_hooks->cblas_ssymm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ssymm.hook_function[hook_pos_cblas_ssymm];
    } else {
        hook_pos_cblas_ssymm = 0;
        *(void **) &fn = current_backend->cblas.cblas_ssymm;
    }
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ssymv = 0;
#endif

/* Wrapper for cblas_ssymv */
void cblas_ssymv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_ssymv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ssymv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ssymv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ssymv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_ssymv;
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_ssymv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_ssymv++;
    if ( hook_pos_cblas_ssymv < __flexiblas_hooks->cblas_ssymv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ssymv.hook_function[hook_pos_cblas_ssymv];
    } else {
        hook_pos_cblas_ssymv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ssymv;
    }
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ssyr = 0;
#endif

/* Wrapper for cblas_ssyr */
void cblas_ssyr (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    hook_pos_cblas_ssyr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ssyr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ssyr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, A, lda);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, A, lda);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ssyr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_ssyr;
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    return;
}

void flexiblas_chain_cblas_ssyr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    hook_pos_cblas_ssyr++;
    if ( hook_pos_cblas_ssyr < __flexiblas_hooks->cblas_ssyr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ssyr.hook_function[hook_pos_cblas_ssyr];
    } else {
        hook_pos_cblas_ssyr = 0;
        *(void **) &fn = current_backend->cblas.cblas_ssyr;
    }
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ssyr2 = 0;
#endif

/* Wrapper for cblas_ssyr2 */
void cblas_ssyr2 (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY, float *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_ssyr2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ssyr2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ssyr2.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ssyr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY, float *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_ssyr2;
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_ssyr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_ssyr2++;
    if ( hook_pos_cblas_ssyr2 < __flexiblas_hooks->cblas_ssyr2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ssyr2.hook_function[hook_pos_cblas_ssyr2];
    } else {
        hook_pos_cblas_ssyr2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_ssyr2;
    }
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ssyr2k = 0;
#endif

/* Wrapper for cblas_ssyr2k */
void cblas_ssyr2k (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_ssyr2k = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ssyr2k;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ssyr2k.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ssyr2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_ssyr2k;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_ssyr2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_ssyr2k++;
    if ( hook_pos_cblas_ssyr2k < __flexiblas_hooks->cblas_ssyr2k.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ssyr2k.hook_function[hook_pos_cblas_ssyr2k];
    } else {
        hook_pos_cblas_ssyr2k = 0;
        *(void **) &fn = current_backend->cblas.cblas_ssyr2k;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ssyrk = 0;
#endif

/* Wrapper for cblas_ssyrk */
void cblas_ssyrk (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_ssyrk = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ssyrk;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ssyrk.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ssyrk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float beta, float *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_ssyrk;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_ssyrk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_ssyrk++;
    if ( hook_pos_cblas_ssyrk < __flexiblas_hooks->cblas_ssyrk.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ssyrk.hook_function[hook_pos_cblas_ssyrk];
    } else {
        hook_pos_cblas_ssyrk = 0;
        *(void **) &fn = current_backend->cblas.cblas_ssyrk;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_stbmv = 0;
#endif

/* Wrapper for cblas_stbmv */
void cblas_stbmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_stbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_stbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_stbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_stbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_stbmv;
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_stbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_stbmv++;
    if ( hook_pos_cblas_stbmv < __flexiblas_hooks->cblas_stbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_stbmv.hook_function[hook_pos_cblas_stbmv];
    } else {
        hook_pos_cblas_stbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_stbmv;
    }
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_stbsv = 0;
#endif

/* Wrapper for cblas_stbsv */
void cblas_stbsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_stbsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_stbsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_stbsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_stbsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_stbsv;
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_stbsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_stbsv++;
    if ( hook_pos_cblas_stbsv < __flexiblas_hooks->cblas_stbsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_stbsv.hook_function[hook_pos_cblas_stbsv];
    } else {
        hook_pos_cblas_stbsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_stbsv;
    }
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_stpmv = 0;
#endif

/* Wrapper for cblas_stpmv */
void cblas_stpmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *Ap, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_stpmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_stpmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_stpmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_stpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *Ap, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_stpmv;
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

void flexiblas_chain_cblas_stpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_stpmv++;
    if ( hook_pos_cblas_stpmv < __flexiblas_hooks->cblas_stpmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_stpmv.hook_function[hook_pos_cblas_stpmv];
    } else {
        hook_pos_cblas_stpmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_stpmv;
    }
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_stpsv = 0;
#endif

/* Wrapper for cblas_stpsv */
void cblas_stpsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *Ap, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_stpsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_stpsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_stpsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_stpsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *Ap, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_stpsv;
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

void flexiblas_chain_cblas_stpsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_stpsv++;
    if ( hook_pos_cblas_stpsv < __flexiblas_hooks->cblas_stpsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_stpsv.hook_function[hook_pos_cblas_stpsv];
    } else {
        hook_pos_cblas_stpsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_stpsv;
    }
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_strmm = 0;
#endif

/* Wrapper for cblas_strmm */
void cblas_strmm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, float *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_strmm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_strmm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_strmm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    } else {
        fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    }
    #else
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_strmm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, float *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    *(void **) &fn = current_backend->cblas.cblas_strmm;
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

void flexiblas_chain_cblas_strmm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_strmm++;
    if ( hook_pos_cblas_strmm < __flexiblas_hooks->cblas_strmm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_strmm.hook_function[hook_pos_cblas_strmm];
    } else {
        hook_pos_cblas_strmm = 0;
        *(void **) &fn = current_backend->cblas.cblas_strmm;
    }
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_strmv = 0;
#endif

/* Wrapper for cblas_strmv */
void cblas_strmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_strmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_strmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_strmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_strmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_strmv;
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_strmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_strmv++;
    if ( hook_pos_cblas_strmv < __flexiblas_hooks->cblas_strmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_strmv.hook_function[hook_pos_cblas_strmv];
    } else {
        hook_pos_cblas_strmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_strmv;
    }
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_strsm = 0;
#endif

/* Wrapper for cblas_strsm */
void cblas_strsm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, float *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_strsm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_strsm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_strsm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    } else {
        fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    }
    #else
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_strsm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, float *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    *(void **) &fn = current_backend->cblas.cblas_strsm;
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

void flexiblas_chain_cblas_strsm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_strsm++;
    if ( hook_pos_cblas_strsm < __flexiblas_hooks->cblas_strsm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_strsm.hook_function[hook_pos_cblas_strsm];
    } else {
        hook_pos_cblas_strsm = 0;
        *(void **) &fn = current_backend->cblas.cblas_strsm;
    }
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_strsv = 0;
#endif

/* Wrapper for cblas_strsv */
void cblas_strsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_strsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_strsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_strsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_strsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_strsv;
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_strsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_strsv++;
    if ( hook_pos_cblas_strsv < __flexiblas_hooks->cblas_strsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_strsv.hook_function[hook_pos_cblas_strsv];
    } else {
        hook_pos_cblas_strsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_strsv;
    }
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zaxpby = 0;
#endif

/* Wrapper for cblas_zaxpby */
void cblas_zaxpby (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zaxpby = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zaxpby;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zaxpby.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX, beta, Y, incY);
    } else {
        fn(N, alpha, X, incX, beta, Y, incY);
    }
    #else
    fn(N, alpha, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zaxpby_(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_zaxpby;
    fn(N, alpha, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_zaxpby_(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zaxpby++;
    if ( hook_pos_cblas_zaxpby < __flexiblas_hooks->cblas_zaxpby.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zaxpby.hook_function[hook_pos_cblas_zaxpby];
    } else {
        hook_pos_cblas_zaxpby = 0;
        *(void **) &fn = current_backend->cblas.cblas_zaxpby;
    }
    fn(N, alpha, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zaxpy = 0;
#endif

/* Wrapper for cblas_zaxpy */
void cblas_zaxpy (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zaxpy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zaxpy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zaxpy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX, Y, incY);
    } else {
        fn(N, alpha, X, incX, Y, incY);
    }
    #else
    fn(N, alpha, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zaxpy_(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_zaxpy;
    fn(N, alpha, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_zaxpy_(const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zaxpy++;
    if ( hook_pos_cblas_zaxpy < __flexiblas_hooks->cblas_zaxpy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zaxpy.hook_function[hook_pos_cblas_zaxpy];
    } else {
        hook_pos_cblas_zaxpy = 0;
        *(void **) &fn = current_backend->cblas.cblas_zaxpy;
    }
    fn(N, alpha, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zcopy = 0;
#endif

/* Wrapper for cblas_zcopy */
void cblas_zcopy (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY);
    } else {
        fn(N, X, incX, Y, incY);
    }
    #else
    fn(N, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zcopy_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_zcopy;
    fn(N, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_zcopy_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zcopy++;
    if ( hook_pos_cblas_zcopy < __flexiblas_hooks->cblas_zcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zcopy.hook_function[hook_pos_cblas_zcopy];
    } else {
        hook_pos_cblas_zcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_zcopy;
    }
    fn(N, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zdotc_sub = 0;
#endif

/* Wrapper for cblas_zdotc_sub */
void cblas_zdotc_sub (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
    hook_pos_cblas_zdotc_sub = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zdotc_sub;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zdotc_sub.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, dotc);
    } else {
        fn(N, X, incX, Y, incY, dotc);
    }
    #else
    fn(N, X, incX, Y, incY, dotc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zdotc_sub_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
    *(void **) &fn = current_backend->cblas.cblas_zdotc_sub;
    fn(N, X, incX, Y, incY, dotc);
    return;
}

void flexiblas_chain_cblas_zdotc_sub_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc);
    hook_pos_cblas_zdotc_sub++;
    if ( hook_pos_cblas_zdotc_sub < __flexiblas_hooks->cblas_zdotc_sub.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zdotc_sub.hook_function[hook_pos_cblas_zdotc_sub];
    } else {
        hook_pos_cblas_zdotc_sub = 0;
        *(void **) &fn = current_backend->cblas.cblas_zdotc_sub;
    }
    fn(N, X, incX, Y, incY, dotc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zdotu_sub = 0;
#endif

/* Wrapper for cblas_zdotu_sub */
void cblas_zdotu_sub (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
    hook_pos_cblas_zdotu_sub = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zdotu_sub;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zdotu_sub.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, dotu);
    } else {
        fn(N, X, incX, Y, incY, dotu);
    }
    #else
    fn(N, X, incX, Y, incY, dotu);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zdotu_sub_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
    *(void **) &fn = current_backend->cblas.cblas_zdotu_sub;
    fn(N, X, incX, Y, incY, dotu);
    return;
}

void flexiblas_chain_cblas_zdotu_sub_(const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu)
{
    void (*fn) (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu);
    hook_pos_cblas_zdotu_sub++;
    if ( hook_pos_cblas_zdotu_sub < __flexiblas_hooks->cblas_zdotu_sub.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zdotu_sub.hook_function[hook_pos_cblas_zdotu_sub];
    } else {
        hook_pos_cblas_zdotu_sub = 0;
        *(void **) &fn = current_backend->cblas.cblas_zdotu_sub;
    }
    fn(N, X, incX, Y, incY, dotu);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zdrot = 0;
#endif

/* Wrapper for cblas_zdrot */
void cblas_zdrot (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
    hook_pos_cblas_zdrot = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zdrot;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zdrot.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY, c, s);
    } else {
        fn(N, X, incX, Y, incY, c, s);
    }
    #else
    fn(N, X, incX, Y, incY, c, s);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zdrot_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
    *(void **) &fn = current_backend->cblas.cblas_zdrot;
    fn(N, X, incX, Y, incY, c, s);
    return;
}

void flexiblas_chain_cblas_zdrot_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s);
    hook_pos_cblas_zdrot++;
    if ( hook_pos_cblas_zdrot < __flexiblas_hooks->cblas_zdrot.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zdrot.hook_function[hook_pos_cblas_zdrot];
    } else {
        hook_pos_cblas_zdrot = 0;
        *(void **) &fn = current_backend->cblas.cblas_zdrot;
    }
    fn(N, X, incX, Y, incY, c, s);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zdscal = 0;
#endif

/* Wrapper for cblas_zdscal */
void cblas_zdscal (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_zdscal = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zdscal;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zdscal.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX);
    } else {
        fn(N, alpha, X, incX);
    }
    #else
    fn(N, alpha, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zdscal_(const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_zdscal;
    fn(N, alpha, X, incX);
    return;
}

void flexiblas_chain_cblas_zdscal_(const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_zdscal++;
    if ( hook_pos_cblas_zdscal < __flexiblas_hooks->cblas_zdscal.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zdscal.hook_function[hook_pos_cblas_zdscal];
    } else {
        hook_pos_cblas_zdscal = 0;
        *(void **) &fn = current_backend->cblas.cblas_zdscal;
    }
    fn(N, alpha, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zgbmv = 0;
#endif

/* Wrapper for cblas_zgbmv */
void cblas_zgbmv (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zgbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zgbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zgbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zgbmv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_zgbmv;
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_zgbmv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zgbmv++;
    if ( hook_pos_cblas_zgbmv < __flexiblas_hooks->cblas_zgbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zgbmv.hook_function[hook_pos_cblas_zgbmv];
    } else {
        hook_pos_cblas_zgbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_zgbmv;
    }
    fn(layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zgeadd = 0;
#endif

/* Wrapper for cblas_zgeadd */
void cblas_zgeadd (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_zgeadd = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zgeadd;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zgeadd.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    } else {
        fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    }
    #else
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zgeadd_(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_zgeadd;
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    return;
}

void flexiblas_chain_cblas_zgeadd_(const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_zgeadd++;
    if ( hook_pos_cblas_zgeadd < __flexiblas_hooks->cblas_zgeadd.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zgeadd.hook_function[hook_pos_cblas_zgeadd];
    } else {
        hook_pos_cblas_zgeadd = 0;
        *(void **) &fn = current_backend->cblas.cblas_zgeadd;
    }
    fn(CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zgemm = 0;
#endif

/* Wrapper for cblas_zgemm */
void cblas_zgemm (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zgemm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zgemm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zgemm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zgemm_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_zgemm;
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_zgemm_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zgemm++;
    if ( hook_pos_cblas_zgemm < __flexiblas_hooks->cblas_zgemm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zgemm.hook_function[hook_pos_cblas_zgemm];
    } else {
        hook_pos_cblas_zgemm = 0;
        *(void **) &fn = current_backend->cblas.cblas_zgemm;
    }
    fn(layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zgemmtr = 0;
#endif

/* Wrapper for cblas_zgemmtr */
void cblas_zgemmtr (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zgemmtr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zgemmtr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zgemmtr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zgemmtr_(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_zgemmtr;
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_zgemmtr_(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zgemmtr++;
    if ( hook_pos_cblas_zgemmtr < __flexiblas_hooks->cblas_zgemmtr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zgemmtr.hook_function[hook_pos_cblas_zgemmtr];
    } else {
        hook_pos_cblas_zgemmtr = 0;
        *(void **) &fn = current_backend->cblas.cblas_zgemmtr;
    }
    fn(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
/* Alternative function name for cblas_zgemmtr -- cblas_zgemmt */
#ifndef __APPLE__
void cblas_zgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc) __attribute__((alias(MTS(cblas_zgemmtr))));
#else
void cblas_zgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc) 
{
    cblas_zgemmtr(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
}

#endif

#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zgemv = 0;
#endif

/* Wrapper for cblas_zgemv */
void cblas_zgemv (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zgemv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zgemv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zgemv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zgemv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_zgemv;
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_zgemv_(CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zgemv++;
    if ( hook_pos_cblas_zgemv < __flexiblas_hooks->cblas_zgemv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zgemv.hook_function[hook_pos_cblas_zgemv];
    } else {
        hook_pos_cblas_zgemv = 0;
        *(void **) &fn = current_backend->cblas.cblas_zgemv;
    }
    fn(layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zgerc = 0;
#endif

/* Wrapper for cblas_zgerc */
void cblas_zgerc (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_zgerc = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zgerc;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zgerc.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zgerc_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_zgerc;
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_zgerc_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_zgerc++;
    if ( hook_pos_cblas_zgerc < __flexiblas_hooks->cblas_zgerc.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zgerc.hook_function[hook_pos_cblas_zgerc];
    } else {
        hook_pos_cblas_zgerc = 0;
        *(void **) &fn = current_backend->cblas.cblas_zgerc;
    }
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zgeru = 0;
#endif

/* Wrapper for cblas_zgeru */
void cblas_zgeru (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_zgeru = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zgeru;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zgeru.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zgeru_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_zgeru;
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_zgeru_(CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_zgeru++;
    if ( hook_pos_cblas_zgeru < __flexiblas_hooks->cblas_zgeru.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zgeru.hook_function[hook_pos_cblas_zgeru];
    } else {
        hook_pos_cblas_zgeru = 0;
        *(void **) &fn = current_backend->cblas.cblas_zgeru;
    }
    fn(layout, M, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zhbmv = 0;
#endif

/* Wrapper for cblas_zhbmv */
void cblas_zhbmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zhbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zhbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zhbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zhbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_zhbmv;
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_zhbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zhbmv++;
    if ( hook_pos_cblas_zhbmv < __flexiblas_hooks->cblas_zhbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zhbmv.hook_function[hook_pos_cblas_zhbmv];
    } else {
        hook_pos_cblas_zhbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_zhbmv;
    }
    fn(layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zhemm = 0;
#endif

/* Wrapper for cblas_zhemm */
void cblas_zhemm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zhemm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zhemm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zhemm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zhemm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_zhemm;
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_zhemm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zhemm++;
    if ( hook_pos_cblas_zhemm < __flexiblas_hooks->cblas_zhemm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zhemm.hook_function[hook_pos_cblas_zhemm];
    } else {
        hook_pos_cblas_zhemm = 0;
        *(void **) &fn = current_backend->cblas.cblas_zhemm;
    }
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zhemv = 0;
#endif

/* Wrapper for cblas_zhemv */
void cblas_zhemv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zhemv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zhemv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zhemv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zhemv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_zhemv;
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_zhemv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zhemv++;
    if ( hook_pos_cblas_zhemv < __flexiblas_hooks->cblas_zhemv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zhemv.hook_function[hook_pos_cblas_zhemv];
    } else {
        hook_pos_cblas_zhemv = 0;
        *(void **) &fn = current_backend->cblas.cblas_zhemv;
    }
    fn(layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zher = 0;
#endif

/* Wrapper for cblas_zher */
void cblas_zher (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    hook_pos_cblas_zher = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zher;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zher.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, A, lda);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, A, lda);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zher_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_zher;
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    return;
}

void flexiblas_chain_cblas_zher_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda);
    hook_pos_cblas_zher++;
    if ( hook_pos_cblas_zher < __flexiblas_hooks->cblas_zher.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zher.hook_function[hook_pos_cblas_zher];
    } else {
        hook_pos_cblas_zher = 0;
        *(void **) &fn = current_backend->cblas.cblas_zher;
    }
    fn(layout, Uplo, N, alpha, X, incX, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zher2 = 0;
#endif

/* Wrapper for cblas_zher2 */
void cblas_zher2 (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_zher2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zher2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zher2.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zher2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    *(void **) &fn = current_backend->cblas.cblas_zher2;
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

void flexiblas_chain_cblas_zher2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda);
    hook_pos_cblas_zher2++;
    if ( hook_pos_cblas_zher2 < __flexiblas_hooks->cblas_zher2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zher2.hook_function[hook_pos_cblas_zher2];
    } else {
        hook_pos_cblas_zher2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_zher2;
    }
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, A, lda);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zher2k = 0;
#endif

/* Wrapper for cblas_zher2k */
void cblas_zher2k (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zher2k = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zher2k;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zher2k.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zher2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_zher2k;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_zher2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zher2k++;
    if ( hook_pos_cblas_zher2k < __flexiblas_hooks->cblas_zher2k.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zher2k.hook_function[hook_pos_cblas_zher2k];
    } else {
        hook_pos_cblas_zher2k = 0;
        *(void **) &fn = current_backend->cblas.cblas_zher2k;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zherk = 0;
#endif

/* Wrapper for cblas_zherk */
void cblas_zherk (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zherk = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zherk;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zherk.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zherk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_zherk;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_zherk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zherk++;
    if ( hook_pos_cblas_zherk < __flexiblas_hooks->cblas_zherk.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zherk.hook_function[hook_pos_cblas_zherk];
    } else {
        hook_pos_cblas_zherk = 0;
        *(void **) &fn = current_backend->cblas.cblas_zherk;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zhpmv = 0;
#endif

/* Wrapper for cblas_zhpmv */
void cblas_zhpmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zhpmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zhpmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zhpmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    } else {
        fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    }
    #else
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zhpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_zhpmv;
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    return;
}

void flexiblas_chain_cblas_zhpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zhpmv++;
    if ( hook_pos_cblas_zhpmv < __flexiblas_hooks->cblas_zhpmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zhpmv.hook_function[hook_pos_cblas_zhpmv];
    } else {
        hook_pos_cblas_zhpmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_zhpmv;
    }
    fn(layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zhpr = 0;
#endif

/* Wrapper for cblas_zhpr */
void cblas_zhpr (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A);
    hook_pos_cblas_zhpr = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zhpr;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zhpr.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, A);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, A);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, A);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zhpr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A);
    *(void **) &fn = current_backend->cblas.cblas_zhpr;
    fn(layout, Uplo, N, alpha, X, incX, A);
    return;
}

void flexiblas_chain_cblas_zhpr_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A);
    hook_pos_cblas_zhpr++;
    if ( hook_pos_cblas_zhpr < __flexiblas_hooks->cblas_zhpr.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zhpr.hook_function[hook_pos_cblas_zhpr];
    } else {
        hook_pos_cblas_zhpr = 0;
        *(void **) &fn = current_backend->cblas.cblas_zhpr;
    }
    fn(layout, Uplo, N, alpha, X, incX, A);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zhpr2 = 0;
#endif

/* Wrapper for cblas_zhpr2 */
void cblas_zhpr2 (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
    hook_pos_cblas_zhpr2 = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zhpr2;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zhpr2.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    } else {
        fn(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    }
    #else
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zhpr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
    *(void **) &fn = current_backend->cblas.cblas_zhpr2;
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    return;
}

void flexiblas_chain_cblas_zhpr2_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap);
    hook_pos_cblas_zhpr2++;
    if ( hook_pos_cblas_zhpr2 < __flexiblas_hooks->cblas_zhpr2.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zhpr2.hook_function[hook_pos_cblas_zhpr2];
    } else {
        hook_pos_cblas_zhpr2 = 0;
        *(void **) &fn = current_backend->cblas.cblas_zhpr2;
    }
    fn(layout, Uplo, N, alpha, X, incX, Y, incY, Ap);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zimatcopy = 0;
#endif

/* Wrapper for cblas_zimatcopy */
void cblas_zimatcopy (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    hook_pos_cblas_zimatcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zimatcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zimatcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    } else {
        fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    }
    #else
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zimatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_zimatcopy;
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    return;
}

void flexiblas_chain_cblas_zimatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb);
    hook_pos_cblas_zimatcopy++;
    if ( hook_pos_cblas_zimatcopy < __flexiblas_hooks->cblas_zimatcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zimatcopy.hook_function[hook_pos_cblas_zimatcopy];
    } else {
        hook_pos_cblas_zimatcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_zimatcopy;
    }
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zomatcopy = 0;
#endif

/* Wrapper for cblas_zomatcopy */
void cblas_zomatcopy (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_zomatcopy = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zomatcopy;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zomatcopy.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    } else {
        fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    }
    #else
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zomatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    *(void **) &fn = current_backend->cblas.cblas_zomatcopy;
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    return;
}

void flexiblas_chain_cblas_zomatcopy_(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb)
{
    void (*fn) (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb);
    hook_pos_cblas_zomatcopy++;
    if ( hook_pos_cblas_zomatcopy < __flexiblas_hooks->cblas_zomatcopy.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zomatcopy.hook_function[hook_pos_cblas_zomatcopy];
    } else {
        hook_pos_cblas_zomatcopy = 0;
        *(void **) &fn = current_backend->cblas.cblas_zomatcopy;
    }
    fn(CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zrotg = 0;
#endif

/* Wrapper for cblas_zrotg */
void cblas_zrotg (void *a, void *b, double *c, void *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (void *a, void *b, void *c, void *s);
    hook_pos_cblas_zrotg = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zrotg;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zrotg.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(a, b, c, s);
    } else {
        fn(a, b, c, s);
    }
    #else
    fn(a, b, c, s);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zrotg_(void *a, void *b, double *c, void *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    *(void **) &fn = current_backend->cblas.cblas_zrotg;
    fn(a, b, c, s);
    return;
}

void flexiblas_chain_cblas_zrotg_(void *a, void *b, void *c, void *s)
{
    void (*fn) (void *a, void *b, void *c, void *s);
    hook_pos_cblas_zrotg++;
    if ( hook_pos_cblas_zrotg < __flexiblas_hooks->cblas_zrotg.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zrotg.hook_function[hook_pos_cblas_zrotg];
    } else {
        hook_pos_cblas_zrotg = 0;
        *(void **) &fn = current_backend->cblas.cblas_zrotg;
    }
    fn(a, b, c, s);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zscal = 0;
#endif

/* Wrapper for cblas_zscal */
void cblas_zscal (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_zscal = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zscal;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zscal.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, alpha, X, incX);
    } else {
        fn(N, alpha, X, incX);
    }
    #else
    fn(N, alpha, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zscal_(const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_zscal;
    fn(N, alpha, X, incX);
    return;
}

void flexiblas_chain_cblas_zscal_(const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX)
{
    void (*fn) (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX);
    hook_pos_cblas_zscal++;
    if ( hook_pos_cblas_zscal < __flexiblas_hooks->cblas_zscal.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zscal.hook_function[hook_pos_cblas_zscal];
    } else {
        hook_pos_cblas_zscal = 0;
        *(void **) &fn = current_backend->cblas.cblas_zscal;
    }
    fn(N, alpha, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zswap = 0;
#endif

/* Wrapper for cblas_zswap */
void cblas_zswap (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zswap = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zswap;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zswap.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(N, X, incX, Y, incY);
    } else {
        fn(N, X, incX, Y, incY);
    }
    #else
    fn(N, X, incX, Y, incY);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zswap_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    *(void **) &fn = current_backend->cblas.cblas_zswap;
    fn(N, X, incX, Y, incY);
    return;
}

void flexiblas_chain_cblas_zswap_(const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
    void (*fn) (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY);
    hook_pos_cblas_zswap++;
    if ( hook_pos_cblas_zswap < __flexiblas_hooks->cblas_zswap.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zswap.hook_function[hook_pos_cblas_zswap];
    } else {
        hook_pos_cblas_zswap = 0;
        *(void **) &fn = current_backend->cblas.cblas_zswap;
    }
    fn(N, X, incX, Y, incY);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zsymm = 0;
#endif

/* Wrapper for cblas_zsymm */
void cblas_zsymm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zsymm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zsymm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zsymm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zsymm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_zsymm;
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_zsymm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zsymm++;
    if ( hook_pos_cblas_zsymm < __flexiblas_hooks->cblas_zsymm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zsymm.hook_function[hook_pos_cblas_zsymm];
    } else {
        hook_pos_cblas_zsymm = 0;
        *(void **) &fn = current_backend->cblas.cblas_zsymm;
    }
    fn(layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zsyr2k = 0;
#endif

/* Wrapper for cblas_zsyr2k */
void cblas_zsyr2k (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zsyr2k = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zsyr2k;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zsyr2k.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zsyr2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_zsyr2k;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_zsyr2k_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zsyr2k++;
    if ( hook_pos_cblas_zsyr2k < __flexiblas_hooks->cblas_zsyr2k.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zsyr2k.hook_function[hook_pos_cblas_zsyr2k];
    } else {
        hook_pos_cblas_zsyr2k = 0;
        *(void **) &fn = current_backend->cblas.cblas_zsyr2k;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_zsyrk = 0;
#endif

/* Wrapper for cblas_zsyrk */
void cblas_zsyrk (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zsyrk = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_zsyrk;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_zsyrk.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    } else {
        fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    }
    #else
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_zsyrk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
    *(void **) &fn = current_backend->cblas.cblas_zsyrk;
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

void flexiblas_chain_cblas_zsyrk_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc);
    hook_pos_cblas_zsyrk++;
    if ( hook_pos_cblas_zsyrk < __flexiblas_hooks->cblas_zsyrk.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_zsyrk.hook_function[hook_pos_cblas_zsyrk];
    } else {
        hook_pos_cblas_zsyrk = 0;
        *(void **) &fn = current_backend->cblas.cblas_zsyrk;
    }
    fn(layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ztbmv = 0;
#endif

/* Wrapper for cblas_ztbmv */
void cblas_ztbmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztbmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ztbmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ztbmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ztbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ztbmv;
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_ztbmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztbmv++;
    if ( hook_pos_cblas_ztbmv < __flexiblas_hooks->cblas_ztbmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ztbmv.hook_function[hook_pos_cblas_ztbmv];
    } else {
        hook_pos_cblas_ztbmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ztbmv;
    }
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ztbsv = 0;
#endif

/* Wrapper for cblas_ztbsv */
void cblas_ztbsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztbsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ztbsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ztbsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ztbsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ztbsv;
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_ztbsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztbsv++;
    if ( hook_pos_cblas_ztbsv < __flexiblas_hooks->cblas_ztbsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ztbsv.hook_function[hook_pos_cblas_ztbsv];
    } else {
        hook_pos_cblas_ztbsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ztbsv;
    }
    fn(layout, Uplo, TransA, Diag, N, K, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ztpmv = 0;
#endif

/* Wrapper for cblas_ztpmv */
void cblas_ztpmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztpmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ztpmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ztpmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ztpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ztpmv;
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

void flexiblas_chain_cblas_ztpmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztpmv++;
    if ( hook_pos_cblas_ztpmv < __flexiblas_hooks->cblas_ztpmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ztpmv.hook_function[hook_pos_cblas_ztpmv];
    } else {
        hook_pos_cblas_ztpmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ztpmv;
    }
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ztpsv = 0;
#endif

/* Wrapper for cblas_ztpsv */
void cblas_ztpsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztpsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ztpsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ztpsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ztpsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ztpsv;
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

void flexiblas_chain_cblas_ztpsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztpsv++;
    if ( hook_pos_cblas_ztpsv < __flexiblas_hooks->cblas_ztpsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ztpsv.hook_function[hook_pos_cblas_ztpsv];
    } else {
        hook_pos_cblas_ztpsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ztpsv;
    }
    fn(layout, Uplo, TransA, Diag, N, Ap, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ztrmm = 0;
#endif

/* Wrapper for cblas_ztrmm */
void cblas_ztrmm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_ztrmm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ztrmm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ztrmm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    } else {
        fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    }
    #else
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ztrmm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    *(void **) &fn = current_backend->cblas.cblas_ztrmm;
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

void flexiblas_chain_cblas_ztrmm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_ztrmm++;
    if ( hook_pos_cblas_ztrmm < __flexiblas_hooks->cblas_ztrmm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ztrmm.hook_function[hook_pos_cblas_ztrmm];
    } else {
        hook_pos_cblas_ztrmm = 0;
        *(void **) &fn = current_backend->cblas.cblas_ztrmm;
    }
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ztrmv = 0;
#endif

/* Wrapper for cblas_ztrmv */
void cblas_ztrmv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztrmv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ztrmv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ztrmv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ztrmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ztrmv;
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_ztrmv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztrmv++;
    if ( hook_pos_cblas_ztrmv < __flexiblas_hooks->cblas_ztrmv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ztrmv.hook_function[hook_pos_cblas_ztrmv];
    } else {
        hook_pos_cblas_ztrmv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ztrmv;
    }
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ztrsm = 0;
#endif

/* Wrapper for cblas_ztrsm */
void cblas_ztrsm (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_ztrsm = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ztrsm;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ztrsm.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    } else {
        fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    }
    #else
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ztrsm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    *(void **) &fn = current_backend->cblas.cblas_ztrsm;
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

void flexiblas_chain_cblas_ztrsm_(CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb);
    hook_pos_cblas_ztrsm++;
    if ( hook_pos_cblas_ztrsm < __flexiblas_hooks->cblas_ztrsm.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ztrsm.hook_function[hook_pos_cblas_ztrsm];
    } else {
        hook_pos_cblas_ztrsm = 0;
        *(void **) &fn = current_backend->cblas.cblas_ztrsm;
    }
    fn(layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb);
    return;
}

#endif
#ifdef FLEXIBLAS_HOOK_API
static TLS_STORE uint8_t hook_pos_cblas_ztrsv = 0;
#endif

/* Wrapper for cblas_ztrsv */
void cblas_ztrsv (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    #ifdef FLEXIBLAS_HOOK_API
    void (*fn_hook) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztrsv = 0;
    #endif

    /* Post init if necessary */
    if ( unlikely(current_backend->post_init != 0) ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    *(void **) &fn = current_backend->cblas.cblas_ztrsv;
    #ifdef FLEXIBLAS_HOOK_API
    *(void **) &fn_hook = __flexiblas_hooks->cblas_ztrsv.hook_function[0];

    if ( fn_hook != NULL ) {
        fn_hook(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    } else {
        fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    }
    #else
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    #endif
    return;
}

#ifdef FLEXIBLAS_HOOK_API
void flexiblas_real_cblas_ztrsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    *(void **) &fn = current_backend->cblas.cblas_ztrsv;
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

void flexiblas_chain_cblas_ztrsv_(CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX)
{
    void (*fn) (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX);
    hook_pos_cblas_ztrsv++;
    if ( hook_pos_cblas_ztrsv < __flexiblas_hooks->cblas_ztrsv.nhook ) {
        *(void **) &fn = __flexiblas_hooks->cblas_ztrsv.hook_function[hook_pos_cblas_ztrsv];
    } else {
        hook_pos_cblas_ztrsv = 0;
        *(void **) &fn = current_backend->cblas.cblas_ztrsv;
    }
    fn(layout, Uplo, TransA, Diag, N, A, lda, X, incX);
    return;
}

#endif

HIDDEN int __flexiblas_load_cblas( flexiblas_backend_t *backend )  {
    int only_fallback = 0;
    int failed = 0;
#ifdef __WIN32__
    void * cblas_in_blis = GetProcAddress(backend->library_handle, "bli_info_get_enable_cblas");
#else
    void * cblas_in_blis = dlsym(backend->library_handle, "bli_info_get_enable_cblas");
#endif

    if ( cblas_in_blis ) {
        DPRINTF_WARN(1, "The desired BLAS library is BLIS. We do not load their CBLAS wrapper since it might alter the behavior of your programs.\n");
        only_fallback = 1;
    }

    
    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_caxpby", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_caxpby", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_caxpby.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_caxpby from internal fallback BLAS.\n");
            backend->cblas.cblas_caxpby = ptr_fallback;
        } else {
            backend->cblas.cblas_caxpby = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_caxpy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_caxpy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_caxpy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_caxpy from internal fallback BLAS.\n");
            backend->cblas.cblas_caxpy = ptr_fallback;
        } else {
            backend->cblas.cblas_caxpy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ccopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ccopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ccopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ccopy from internal fallback BLAS.\n");
            backend->cblas.cblas_ccopy = ptr_fallback;
        } else {
            backend->cblas.cblas_ccopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cdotc_sub", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cdotc_sub", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cdotc_sub.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cdotc_sub from internal fallback BLAS.\n");
            backend->cblas.cblas_cdotc_sub = ptr_fallback;
        } else {
            backend->cblas.cblas_cdotc_sub = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cdotu_sub", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cdotu_sub", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cdotu_sub.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cdotu_sub from internal fallback BLAS.\n");
            backend->cblas.cblas_cdotu_sub = ptr_fallback;
        } else {
            backend->cblas.cblas_cdotu_sub = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cgbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cgbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cgbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cgbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_cgbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_cgbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cgeadd", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cgeadd", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cgeadd.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cgeadd from internal fallback BLAS.\n");
            backend->cblas.cblas_cgeadd = ptr_fallback;
        } else {
            backend->cblas.cblas_cgeadd = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cgemm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cgemm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cgemm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cgemm from internal fallback BLAS.\n");
            backend->cblas.cblas_cgemm = ptr_fallback;
        } else {
            backend->cblas.cblas_cgemm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cgemmtr", "cblas_cgemmt", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cgemmtr", "cblas_cgemmt", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cgemmtr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cgemmtr from internal fallback BLAS.\n");
            backend->cblas.cblas_cgemmtr = ptr_fallback;
        } else {
            backend->cblas.cblas_cgemmtr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cgemv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cgemv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cgemv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cgemv from internal fallback BLAS.\n");
            backend->cblas.cblas_cgemv = ptr_fallback;
        } else {
            backend->cblas.cblas_cgemv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cgerc", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cgerc", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cgerc.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cgerc from internal fallback BLAS.\n");
            backend->cblas.cblas_cgerc = ptr_fallback;
        } else {
            backend->cblas.cblas_cgerc = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cgeru", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cgeru", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cgeru.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cgeru from internal fallback BLAS.\n");
            backend->cblas.cblas_cgeru = ptr_fallback;
        } else {
            backend->cblas.cblas_cgeru = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_chbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_chbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_chbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_chbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_chbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_chbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_chemm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_chemm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_chemm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_chemm from internal fallback BLAS.\n");
            backend->cblas.cblas_chemm = ptr_fallback;
        } else {
            backend->cblas.cblas_chemm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_chemv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_chemv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_chemv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_chemv from internal fallback BLAS.\n");
            backend->cblas.cblas_chemv = ptr_fallback;
        } else {
            backend->cblas.cblas_chemv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cher", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cher", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cher.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cher from internal fallback BLAS.\n");
            backend->cblas.cblas_cher = ptr_fallback;
        } else {
            backend->cblas.cblas_cher = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cher2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cher2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cher2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cher2 from internal fallback BLAS.\n");
            backend->cblas.cblas_cher2 = ptr_fallback;
        } else {
            backend->cblas.cblas_cher2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cher2k", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cher2k", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cher2k.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cher2k from internal fallback BLAS.\n");
            backend->cblas.cblas_cher2k = ptr_fallback;
        } else {
            backend->cblas.cblas_cher2k = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cherk", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cherk", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cherk.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cherk from internal fallback BLAS.\n");
            backend->cblas.cblas_cherk = ptr_fallback;
        } else {
            backend->cblas.cblas_cherk = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_chpmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_chpmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_chpmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_chpmv from internal fallback BLAS.\n");
            backend->cblas.cblas_chpmv = ptr_fallback;
        } else {
            backend->cblas.cblas_chpmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_chpr", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_chpr", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_chpr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_chpr from internal fallback BLAS.\n");
            backend->cblas.cblas_chpr = ptr_fallback;
        } else {
            backend->cblas.cblas_chpr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_chpr2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_chpr2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_chpr2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_chpr2 from internal fallback BLAS.\n");
            backend->cblas.cblas_chpr2 = ptr_fallback;
        } else {
            backend->cblas.cblas_chpr2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cimatcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cimatcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cimatcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cimatcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_cimatcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_cimatcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_comatcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_comatcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_comatcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_comatcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_comatcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_comatcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_crotg", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_crotg", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_crotg.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_crotg from internal fallback BLAS.\n");
            backend->cblas.cblas_crotg = ptr_fallback;
        } else {
            backend->cblas.cblas_crotg = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cscal", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cscal", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cscal.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cscal from internal fallback BLAS.\n");
            backend->cblas.cblas_cscal = ptr_fallback;
        } else {
            backend->cblas.cblas_cscal = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_csrot", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_csrot", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_csrot.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_csrot from internal fallback BLAS.\n");
            backend->cblas.cblas_csrot = ptr_fallback;
        } else {
            backend->cblas.cblas_csrot = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_csscal", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_csscal", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_csscal.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_csscal from internal fallback BLAS.\n");
            backend->cblas.cblas_csscal = ptr_fallback;
        } else {
            backend->cblas.cblas_csscal = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_cswap", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_cswap", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_cswap.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_cswap from internal fallback BLAS.\n");
            backend->cblas.cblas_cswap = ptr_fallback;
        } else {
            backend->cblas.cblas_cswap = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_csymm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_csymm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_csymm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_csymm from internal fallback BLAS.\n");
            backend->cblas.cblas_csymm = ptr_fallback;
        } else {
            backend->cblas.cblas_csymm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_csyr2k", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_csyr2k", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_csyr2k.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_csyr2k from internal fallback BLAS.\n");
            backend->cblas.cblas_csyr2k = ptr_fallback;
        } else {
            backend->cblas.cblas_csyr2k = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_csyrk", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_csyrk", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_csyrk.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_csyrk from internal fallback BLAS.\n");
            backend->cblas.cblas_csyrk = ptr_fallback;
        } else {
            backend->cblas.cblas_csyrk = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ctbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ctbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ctbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ctbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_ctbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_ctbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ctbsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ctbsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ctbsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ctbsv from internal fallback BLAS.\n");
            backend->cblas.cblas_ctbsv = ptr_fallback;
        } else {
            backend->cblas.cblas_ctbsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ctpmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ctpmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ctpmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ctpmv from internal fallback BLAS.\n");
            backend->cblas.cblas_ctpmv = ptr_fallback;
        } else {
            backend->cblas.cblas_ctpmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ctpsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ctpsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ctpsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ctpsv from internal fallback BLAS.\n");
            backend->cblas.cblas_ctpsv = ptr_fallback;
        } else {
            backend->cblas.cblas_ctpsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ctrmm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ctrmm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ctrmm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ctrmm from internal fallback BLAS.\n");
            backend->cblas.cblas_ctrmm = ptr_fallback;
        } else {
            backend->cblas.cblas_ctrmm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ctrmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ctrmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ctrmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ctrmv from internal fallback BLAS.\n");
            backend->cblas.cblas_ctrmv = ptr_fallback;
        } else {
            backend->cblas.cblas_ctrmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ctrsm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ctrsm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ctrsm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ctrsm from internal fallback BLAS.\n");
            backend->cblas.cblas_ctrsm = ptr_fallback;
        } else {
            backend->cblas.cblas_ctrsm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ctrsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ctrsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ctrsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ctrsv from internal fallback BLAS.\n");
            backend->cblas.cblas_ctrsv = ptr_fallback;
        } else {
            backend->cblas.cblas_ctrsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dasum", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dasum", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dasum.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dasum from internal fallback BLAS.\n");
            backend->cblas.cblas_dasum = ptr_fallback;
        } else {
            backend->cblas.cblas_dasum = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_daxpby", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_daxpby", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_daxpby.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_daxpby from internal fallback BLAS.\n");
            backend->cblas.cblas_daxpby = ptr_fallback;
        } else {
            backend->cblas.cblas_daxpby = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_daxpy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_daxpy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_daxpy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_daxpy from internal fallback BLAS.\n");
            backend->cblas.cblas_daxpy = ptr_fallback;
        } else {
            backend->cblas.cblas_daxpy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_dcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_dcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ddot", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ddot", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ddot.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ddot from internal fallback BLAS.\n");
            backend->cblas.cblas_ddot = ptr_fallback;
        } else {
            backend->cblas.cblas_ddot = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dgbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dgbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dgbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dgbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_dgbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_dgbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dgeadd", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dgeadd", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dgeadd.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dgeadd from internal fallback BLAS.\n");
            backend->cblas.cblas_dgeadd = ptr_fallback;
        } else {
            backend->cblas.cblas_dgeadd = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dgemm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dgemm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dgemm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dgemm from internal fallback BLAS.\n");
            backend->cblas.cblas_dgemm = ptr_fallback;
        } else {
            backend->cblas.cblas_dgemm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dgemmtr", "cblas_dgemmt", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dgemmtr", "cblas_dgemmt", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dgemmtr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dgemmtr from internal fallback BLAS.\n");
            backend->cblas.cblas_dgemmtr = ptr_fallback;
        } else {
            backend->cblas.cblas_dgemmtr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dgemv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dgemv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dgemv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dgemv from internal fallback BLAS.\n");
            backend->cblas.cblas_dgemv = ptr_fallback;
        } else {
            backend->cblas.cblas_dgemv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dger", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dger", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dger.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dger from internal fallback BLAS.\n");
            backend->cblas.cblas_dger = ptr_fallback;
        } else {
            backend->cblas.cblas_dger = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dimatcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dimatcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dimatcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dimatcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_dimatcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_dimatcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dnrm2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dnrm2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dnrm2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dnrm2 from internal fallback BLAS.\n");
            backend->cblas.cblas_dnrm2 = ptr_fallback;
        } else {
            backend->cblas.cblas_dnrm2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_domatcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_domatcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_domatcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_domatcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_domatcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_domatcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_drot", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_drot", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_drot.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_drot from internal fallback BLAS.\n");
            backend->cblas.cblas_drot = ptr_fallback;
        } else {
            backend->cblas.cblas_drot = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_drotg", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_drotg", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_drotg.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_drotg from internal fallback BLAS.\n");
            backend->cblas.cblas_drotg = ptr_fallback;
        } else {
            backend->cblas.cblas_drotg = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_drotm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_drotm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_drotm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_drotm from internal fallback BLAS.\n");
            backend->cblas.cblas_drotm = ptr_fallback;
        } else {
            backend->cblas.cblas_drotm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_drotmg", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_drotmg", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_drotmg.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_drotmg from internal fallback BLAS.\n");
            backend->cblas.cblas_drotmg = ptr_fallback;
        } else {
            backend->cblas.cblas_drotmg = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dsbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dsbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dsbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dsbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_dsbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_dsbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dscal", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dscal", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dscal.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dscal from internal fallback BLAS.\n");
            backend->cblas.cblas_dscal = ptr_fallback;
        } else {
            backend->cblas.cblas_dscal = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dsdot", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dsdot", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dsdot.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dsdot from internal fallback BLAS.\n");
            backend->cblas.cblas_dsdot = ptr_fallback;
        } else {
            backend->cblas.cblas_dsdot = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dspmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dspmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dspmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dspmv from internal fallback BLAS.\n");
            backend->cblas.cblas_dspmv = ptr_fallback;
        } else {
            backend->cblas.cblas_dspmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dspr", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dspr", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dspr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dspr from internal fallback BLAS.\n");
            backend->cblas.cblas_dspr = ptr_fallback;
        } else {
            backend->cblas.cblas_dspr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dspr2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dspr2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dspr2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dspr2 from internal fallback BLAS.\n");
            backend->cblas.cblas_dspr2 = ptr_fallback;
        } else {
            backend->cblas.cblas_dspr2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dswap", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dswap", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dswap.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dswap from internal fallback BLAS.\n");
            backend->cblas.cblas_dswap = ptr_fallback;
        } else {
            backend->cblas.cblas_dswap = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dsymm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dsymm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dsymm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dsymm from internal fallback BLAS.\n");
            backend->cblas.cblas_dsymm = ptr_fallback;
        } else {
            backend->cblas.cblas_dsymm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dsymv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dsymv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dsymv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dsymv from internal fallback BLAS.\n");
            backend->cblas.cblas_dsymv = ptr_fallback;
        } else {
            backend->cblas.cblas_dsymv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dsyr", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dsyr", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dsyr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dsyr from internal fallback BLAS.\n");
            backend->cblas.cblas_dsyr = ptr_fallback;
        } else {
            backend->cblas.cblas_dsyr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dsyr2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dsyr2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dsyr2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dsyr2 from internal fallback BLAS.\n");
            backend->cblas.cblas_dsyr2 = ptr_fallback;
        } else {
            backend->cblas.cblas_dsyr2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dsyr2k", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dsyr2k", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dsyr2k.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dsyr2k from internal fallback BLAS.\n");
            backend->cblas.cblas_dsyr2k = ptr_fallback;
        } else {
            backend->cblas.cblas_dsyr2k = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dsyrk", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dsyrk", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dsyrk.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dsyrk from internal fallback BLAS.\n");
            backend->cblas.cblas_dsyrk = ptr_fallback;
        } else {
            backend->cblas.cblas_dsyrk = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dtbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dtbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dtbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dtbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_dtbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_dtbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dtbsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dtbsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dtbsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dtbsv from internal fallback BLAS.\n");
            backend->cblas.cblas_dtbsv = ptr_fallback;
        } else {
            backend->cblas.cblas_dtbsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dtpmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dtpmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dtpmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dtpmv from internal fallback BLAS.\n");
            backend->cblas.cblas_dtpmv = ptr_fallback;
        } else {
            backend->cblas.cblas_dtpmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dtpsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dtpsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dtpsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dtpsv from internal fallback BLAS.\n");
            backend->cblas.cblas_dtpsv = ptr_fallback;
        } else {
            backend->cblas.cblas_dtpsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dtrmm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dtrmm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dtrmm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dtrmm from internal fallback BLAS.\n");
            backend->cblas.cblas_dtrmm = ptr_fallback;
        } else {
            backend->cblas.cblas_dtrmm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dtrmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dtrmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dtrmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dtrmv from internal fallback BLAS.\n");
            backend->cblas.cblas_dtrmv = ptr_fallback;
        } else {
            backend->cblas.cblas_dtrmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dtrsm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dtrsm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dtrsm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dtrsm from internal fallback BLAS.\n");
            backend->cblas.cblas_dtrsm = ptr_fallback;
        } else {
            backend->cblas.cblas_dtrsm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dtrsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dtrsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dtrsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dtrsv from internal fallback BLAS.\n");
            backend->cblas.cblas_dtrsv = ptr_fallback;
        } else {
            backend->cblas.cblas_dtrsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dzasum", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dzasum", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dzasum.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dzasum from internal fallback BLAS.\n");
            backend->cblas.cblas_dzasum = ptr_fallback;
        } else {
            backend->cblas.cblas_dzasum = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_dznrm2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_dznrm2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_dznrm2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_dznrm2 from internal fallback BLAS.\n");
            backend->cblas.cblas_dznrm2 = ptr_fallback;
        } else {
            backend->cblas.cblas_dznrm2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_icamax", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_icamax", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_icamax.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_icamax from internal fallback BLAS.\n");
            backend->cblas.cblas_icamax = ptr_fallback;
        } else {
            backend->cblas.cblas_icamax = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_idamax", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_idamax", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_idamax.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_idamax from internal fallback BLAS.\n");
            backend->cblas.cblas_idamax = ptr_fallback;
        } else {
            backend->cblas.cblas_idamax = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_isamax", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_isamax", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_isamax.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_isamax from internal fallback BLAS.\n");
            backend->cblas.cblas_isamax = ptr_fallback;
        } else {
            backend->cblas.cblas_isamax = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_izamax", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_izamax", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_izamax.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_izamax from internal fallback BLAS.\n");
            backend->cblas.cblas_izamax = ptr_fallback;
        } else {
            backend->cblas.cblas_izamax = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sasum", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sasum", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sasum.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sasum from internal fallback BLAS.\n");
            backend->cblas.cblas_sasum = ptr_fallback;
        } else {
            backend->cblas.cblas_sasum = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_saxpby", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_saxpby", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_saxpby.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_saxpby from internal fallback BLAS.\n");
            backend->cblas.cblas_saxpby = ptr_fallback;
        } else {
            backend->cblas.cblas_saxpby = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_saxpy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_saxpy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_saxpy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_saxpy from internal fallback BLAS.\n");
            backend->cblas.cblas_saxpy = ptr_fallback;
        } else {
            backend->cblas.cblas_saxpy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_scasum", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_scasum", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_scasum.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_scasum from internal fallback BLAS.\n");
            backend->cblas.cblas_scasum = ptr_fallback;
        } else {
            backend->cblas.cblas_scasum = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_scnrm2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_scnrm2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_scnrm2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_scnrm2 from internal fallback BLAS.\n");
            backend->cblas.cblas_scnrm2 = ptr_fallback;
        } else {
            backend->cblas.cblas_scnrm2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_scopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_scopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_scopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_scopy from internal fallback BLAS.\n");
            backend->cblas.cblas_scopy = ptr_fallback;
        } else {
            backend->cblas.cblas_scopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sdot", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sdot", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sdot.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sdot from internal fallback BLAS.\n");
            backend->cblas.cblas_sdot = ptr_fallback;
        } else {
            backend->cblas.cblas_sdot = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sdsdot", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sdsdot", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sdsdot.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sdsdot from internal fallback BLAS.\n");
            backend->cblas.cblas_sdsdot = ptr_fallback;
        } else {
            backend->cblas.cblas_sdsdot = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sgbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sgbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sgbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sgbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_sgbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_sgbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sgeadd", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sgeadd", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sgeadd.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sgeadd from internal fallback BLAS.\n");
            backend->cblas.cblas_sgeadd = ptr_fallback;
        } else {
            backend->cblas.cblas_sgeadd = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sgemm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sgemm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sgemm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sgemm from internal fallback BLAS.\n");
            backend->cblas.cblas_sgemm = ptr_fallback;
        } else {
            backend->cblas.cblas_sgemm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sgemmtr", "cblas_sgemmt", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sgemmtr", "cblas_sgemmt", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sgemmtr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sgemmtr from internal fallback BLAS.\n");
            backend->cblas.cblas_sgemmtr = ptr_fallback;
        } else {
            backend->cblas.cblas_sgemmtr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sgemv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sgemv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sgemv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sgemv from internal fallback BLAS.\n");
            backend->cblas.cblas_sgemv = ptr_fallback;
        } else {
            backend->cblas.cblas_sgemv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sger", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sger", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sger.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sger from internal fallback BLAS.\n");
            backend->cblas.cblas_sger = ptr_fallback;
        } else {
            backend->cblas.cblas_sger = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_simatcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_simatcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_simatcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_simatcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_simatcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_simatcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_snrm2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_snrm2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_snrm2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_snrm2 from internal fallback BLAS.\n");
            backend->cblas.cblas_snrm2 = ptr_fallback;
        } else {
            backend->cblas.cblas_snrm2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_somatcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_somatcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_somatcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_somatcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_somatcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_somatcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_srot", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_srot", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_srot.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_srot from internal fallback BLAS.\n");
            backend->cblas.cblas_srot = ptr_fallback;
        } else {
            backend->cblas.cblas_srot = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_srotg", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_srotg", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_srotg.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_srotg from internal fallback BLAS.\n");
            backend->cblas.cblas_srotg = ptr_fallback;
        } else {
            backend->cblas.cblas_srotg = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_srotm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_srotm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_srotm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_srotm from internal fallback BLAS.\n");
            backend->cblas.cblas_srotm = ptr_fallback;
        } else {
            backend->cblas.cblas_srotm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_srotmg", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_srotmg", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_srotmg.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_srotmg from internal fallback BLAS.\n");
            backend->cblas.cblas_srotmg = ptr_fallback;
        } else {
            backend->cblas.cblas_srotmg = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ssbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ssbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ssbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ssbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_ssbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_ssbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sscal", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sscal", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sscal.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sscal from internal fallback BLAS.\n");
            backend->cblas.cblas_sscal = ptr_fallback;
        } else {
            backend->cblas.cblas_sscal = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sspmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sspmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sspmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sspmv from internal fallback BLAS.\n");
            backend->cblas.cblas_sspmv = ptr_fallback;
        } else {
            backend->cblas.cblas_sspmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sspr", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sspr", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sspr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sspr from internal fallback BLAS.\n");
            backend->cblas.cblas_sspr = ptr_fallback;
        } else {
            backend->cblas.cblas_sspr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sspr2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sspr2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sspr2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sspr2 from internal fallback BLAS.\n");
            backend->cblas.cblas_sspr2 = ptr_fallback;
        } else {
            backend->cblas.cblas_sspr2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_sswap", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_sswap", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_sswap.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_sswap from internal fallback BLAS.\n");
            backend->cblas.cblas_sswap = ptr_fallback;
        } else {
            backend->cblas.cblas_sswap = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ssymm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ssymm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ssymm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ssymm from internal fallback BLAS.\n");
            backend->cblas.cblas_ssymm = ptr_fallback;
        } else {
            backend->cblas.cblas_ssymm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ssymv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ssymv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ssymv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ssymv from internal fallback BLAS.\n");
            backend->cblas.cblas_ssymv = ptr_fallback;
        } else {
            backend->cblas.cblas_ssymv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ssyr", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ssyr", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ssyr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ssyr from internal fallback BLAS.\n");
            backend->cblas.cblas_ssyr = ptr_fallback;
        } else {
            backend->cblas.cblas_ssyr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ssyr2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ssyr2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ssyr2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ssyr2 from internal fallback BLAS.\n");
            backend->cblas.cblas_ssyr2 = ptr_fallback;
        } else {
            backend->cblas.cblas_ssyr2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ssyr2k", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ssyr2k", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ssyr2k.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ssyr2k from internal fallback BLAS.\n");
            backend->cblas.cblas_ssyr2k = ptr_fallback;
        } else {
            backend->cblas.cblas_ssyr2k = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ssyrk", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ssyrk", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ssyrk.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ssyrk from internal fallback BLAS.\n");
            backend->cblas.cblas_ssyrk = ptr_fallback;
        } else {
            backend->cblas.cblas_ssyrk = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_stbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_stbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_stbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_stbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_stbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_stbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_stbsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_stbsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_stbsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_stbsv from internal fallback BLAS.\n");
            backend->cblas.cblas_stbsv = ptr_fallback;
        } else {
            backend->cblas.cblas_stbsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_stpmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_stpmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_stpmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_stpmv from internal fallback BLAS.\n");
            backend->cblas.cblas_stpmv = ptr_fallback;
        } else {
            backend->cblas.cblas_stpmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_stpsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_stpsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_stpsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_stpsv from internal fallback BLAS.\n");
            backend->cblas.cblas_stpsv = ptr_fallback;
        } else {
            backend->cblas.cblas_stpsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_strmm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_strmm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_strmm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_strmm from internal fallback BLAS.\n");
            backend->cblas.cblas_strmm = ptr_fallback;
        } else {
            backend->cblas.cblas_strmm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_strmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_strmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_strmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_strmv from internal fallback BLAS.\n");
            backend->cblas.cblas_strmv = ptr_fallback;
        } else {
            backend->cblas.cblas_strmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_strsm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_strsm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_strsm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_strsm from internal fallback BLAS.\n");
            backend->cblas.cblas_strsm = ptr_fallback;
        } else {
            backend->cblas.cblas_strsm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_strsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_strsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_strsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_strsv from internal fallback BLAS.\n");
            backend->cblas.cblas_strsv = ptr_fallback;
        } else {
            backend->cblas.cblas_strsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zaxpby", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zaxpby", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zaxpby.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zaxpby from internal fallback BLAS.\n");
            backend->cblas.cblas_zaxpby = ptr_fallback;
        } else {
            backend->cblas.cblas_zaxpby = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zaxpy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zaxpy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zaxpy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zaxpy from internal fallback BLAS.\n");
            backend->cblas.cblas_zaxpy = ptr_fallback;
        } else {
            backend->cblas.cblas_zaxpy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_zcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_zcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zdotc_sub", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zdotc_sub", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zdotc_sub.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zdotc_sub from internal fallback BLAS.\n");
            backend->cblas.cblas_zdotc_sub = ptr_fallback;
        } else {
            backend->cblas.cblas_zdotc_sub = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zdotu_sub", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zdotu_sub", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zdotu_sub.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zdotu_sub from internal fallback BLAS.\n");
            backend->cblas.cblas_zdotu_sub = ptr_fallback;
        } else {
            backend->cblas.cblas_zdotu_sub = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zdrot", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zdrot", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zdrot.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zdrot from internal fallback BLAS.\n");
            backend->cblas.cblas_zdrot = ptr_fallback;
        } else {
            backend->cblas.cblas_zdrot = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zdscal", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zdscal", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zdscal.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zdscal from internal fallback BLAS.\n");
            backend->cblas.cblas_zdscal = ptr_fallback;
        } else {
            backend->cblas.cblas_zdscal = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zgbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zgbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zgbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zgbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_zgbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_zgbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zgeadd", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zgeadd", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zgeadd.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zgeadd from internal fallback BLAS.\n");
            backend->cblas.cblas_zgeadd = ptr_fallback;
        } else {
            backend->cblas.cblas_zgeadd = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zgemm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zgemm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zgemm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zgemm from internal fallback BLAS.\n");
            backend->cblas.cblas_zgemm = ptr_fallback;
        } else {
            backend->cblas.cblas_zgemm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zgemmtr", "cblas_zgemmt", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zgemmtr", "cblas_zgemmt", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zgemmtr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zgemmtr from internal fallback BLAS.\n");
            backend->cblas.cblas_zgemmtr = ptr_fallback;
        } else {
            backend->cblas.cblas_zgemmtr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zgemv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zgemv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zgemv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zgemv from internal fallback BLAS.\n");
            backend->cblas.cblas_zgemv = ptr_fallback;
        } else {
            backend->cblas.cblas_zgemv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zgerc", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zgerc", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zgerc.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zgerc from internal fallback BLAS.\n");
            backend->cblas.cblas_zgerc = ptr_fallback;
        } else {
            backend->cblas.cblas_zgerc = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zgeru", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zgeru", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zgeru.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zgeru from internal fallback BLAS.\n");
            backend->cblas.cblas_zgeru = ptr_fallback;
        } else {
            backend->cblas.cblas_zgeru = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zhbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zhbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zhbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zhbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_zhbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_zhbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zhemm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zhemm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zhemm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zhemm from internal fallback BLAS.\n");
            backend->cblas.cblas_zhemm = ptr_fallback;
        } else {
            backend->cblas.cblas_zhemm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zhemv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zhemv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zhemv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zhemv from internal fallback BLAS.\n");
            backend->cblas.cblas_zhemv = ptr_fallback;
        } else {
            backend->cblas.cblas_zhemv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zher", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zher", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zher.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zher from internal fallback BLAS.\n");
            backend->cblas.cblas_zher = ptr_fallback;
        } else {
            backend->cblas.cblas_zher = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zher2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zher2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zher2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zher2 from internal fallback BLAS.\n");
            backend->cblas.cblas_zher2 = ptr_fallback;
        } else {
            backend->cblas.cblas_zher2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zher2k", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zher2k", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zher2k.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zher2k from internal fallback BLAS.\n");
            backend->cblas.cblas_zher2k = ptr_fallback;
        } else {
            backend->cblas.cblas_zher2k = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zherk", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zherk", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zherk.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zherk from internal fallback BLAS.\n");
            backend->cblas.cblas_zherk = ptr_fallback;
        } else {
            backend->cblas.cblas_zherk = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zhpmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zhpmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zhpmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zhpmv from internal fallback BLAS.\n");
            backend->cblas.cblas_zhpmv = ptr_fallback;
        } else {
            backend->cblas.cblas_zhpmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zhpr", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zhpr", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zhpr.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zhpr from internal fallback BLAS.\n");
            backend->cblas.cblas_zhpr = ptr_fallback;
        } else {
            backend->cblas.cblas_zhpr = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zhpr2", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zhpr2", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zhpr2.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zhpr2 from internal fallback BLAS.\n");
            backend->cblas.cblas_zhpr2 = ptr_fallback;
        } else {
            backend->cblas.cblas_zhpr2 = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zimatcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zimatcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zimatcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zimatcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_zimatcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_zimatcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zomatcopy", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zomatcopy", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zomatcopy.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zomatcopy from internal fallback BLAS.\n");
            backend->cblas.cblas_zomatcopy = ptr_fallback;
        } else {
            backend->cblas.cblas_zomatcopy = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zrotg", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zrotg", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zrotg.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zrotg from internal fallback BLAS.\n");
            backend->cblas.cblas_zrotg = ptr_fallback;
        } else {
            backend->cblas.cblas_zrotg = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zscal", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zscal", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zscal.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zscal from internal fallback BLAS.\n");
            backend->cblas.cblas_zscal = ptr_fallback;
        } else {
            backend->cblas.cblas_zscal = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zswap", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zswap", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zswap.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zswap from internal fallback BLAS.\n");
            backend->cblas.cblas_zswap = ptr_fallback;
        } else {
            backend->cblas.cblas_zswap = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zsymm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zsymm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zsymm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zsymm from internal fallback BLAS.\n");
            backend->cblas.cblas_zsymm = ptr_fallback;
        } else {
            backend->cblas.cblas_zsymm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zsyr2k", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zsyr2k", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zsyr2k.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zsyr2k from internal fallback BLAS.\n");
            backend->cblas.cblas_zsyr2k = ptr_fallback;
        } else {
            backend->cblas.cblas_zsyr2k = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_zsyrk", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_zsyrk", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_zsyrk.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_zsyrk from internal fallback BLAS.\n");
            backend->cblas.cblas_zsyrk = ptr_fallback;
        } else {
            backend->cblas.cblas_zsyrk = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ztbmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ztbmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ztbmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ztbmv from internal fallback BLAS.\n");
            backend->cblas.cblas_ztbmv = ptr_fallback;
        } else {
            backend->cblas.cblas_ztbmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ztbsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ztbsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ztbsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ztbsv from internal fallback BLAS.\n");
            backend->cblas.cblas_ztbsv = ptr_fallback;
        } else {
            backend->cblas.cblas_ztbsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ztpmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ztpmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ztpmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ztpmv from internal fallback BLAS.\n");
            backend->cblas.cblas_ztpmv = ptr_fallback;
        } else {
            backend->cblas.cblas_ztpmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ztpsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ztpsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ztpsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ztpsv from internal fallback BLAS.\n");
            backend->cblas.cblas_ztpsv = ptr_fallback;
        } else {
            backend->cblas.cblas_ztpsv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ztrmm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ztrmm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ztrmm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ztrmm from internal fallback BLAS.\n");
            backend->cblas.cblas_ztrmm = ptr_fallback;
        } else {
            backend->cblas.cblas_ztrmm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ztrmv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ztrmv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ztrmv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ztrmv from internal fallback BLAS.\n");
            backend->cblas.cblas_ztrmv = ptr_fallback;
        } else {
            backend->cblas.cblas_ztrmv = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ztrsm", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ztrsm", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ztrsm.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ztrsm from internal fallback BLAS.\n");
            backend->cblas.cblas_ztrsm = ptr_fallback;
        } else {
            backend->cblas.cblas_ztrsm = ptr_library;
        }
    } while(0);



    do {
        void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, "cblas_ztrsv", NULL);
        void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, "cblas_ztrsv", NULL);
        if ( ptr_library == NULL && ptr_fallback == NULL ) {
            flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load cblas_ztrsv.\n");
            failed++;
            break;
        } 
        if ( only_fallback || ptr_library == NULL) {
            DPRINTF(2, "Load cblas_ztrsv from internal fallback BLAS.\n");
            backend->cblas.cblas_ztrsv = ptr_fallback;
        } else {
            backend->cblas.cblas_ztrsv = ptr_library;
        }
    } while(0);



    return failed;
}



#ifdef FLEXIBLAS_HOOK_API
HIDDEN int __flexiblas_load_cblas_hooks ( flexiblas_hook_t *hooks, void *hook_handle)  {

    LOAD_CHOOK(hooks,hook_handle,caxpby,cblas_caxpby);
    LOAD_CHOOK(hooks,hook_handle,caxpy,cblas_caxpy);
    LOAD_CHOOK(hooks,hook_handle,ccopy,cblas_ccopy);
    LOAD_CHOOK(hooks,hook_handle,cdotc_sub,cblas_cdotc_sub);
    LOAD_CHOOK(hooks,hook_handle,cdotu_sub,cblas_cdotu_sub);
    LOAD_CHOOK(hooks,hook_handle,cgbmv,cblas_cgbmv);
    LOAD_CHOOK(hooks,hook_handle,cgeadd,cblas_cgeadd);
    LOAD_CHOOK(hooks,hook_handle,cgemm,cblas_cgemm);
    LOAD_CHOOK(hooks,hook_handle,cgemmtr,cblas_cgemmtr);
    LOAD_CHOOK(hooks,hook_handle,cgemv,cblas_cgemv);
    LOAD_CHOOK(hooks,hook_handle,cgerc,cblas_cgerc);
    LOAD_CHOOK(hooks,hook_handle,cgeru,cblas_cgeru);
    LOAD_CHOOK(hooks,hook_handle,chbmv,cblas_chbmv);
    LOAD_CHOOK(hooks,hook_handle,chemm,cblas_chemm);
    LOAD_CHOOK(hooks,hook_handle,chemv,cblas_chemv);
    LOAD_CHOOK(hooks,hook_handle,cher,cblas_cher);
    LOAD_CHOOK(hooks,hook_handle,cher2,cblas_cher2);
    LOAD_CHOOK(hooks,hook_handle,cher2k,cblas_cher2k);
    LOAD_CHOOK(hooks,hook_handle,cherk,cblas_cherk);
    LOAD_CHOOK(hooks,hook_handle,chpmv,cblas_chpmv);
    LOAD_CHOOK(hooks,hook_handle,chpr,cblas_chpr);
    LOAD_CHOOK(hooks,hook_handle,chpr2,cblas_chpr2);
    LOAD_CHOOK(hooks,hook_handle,cimatcopy,cblas_cimatcopy);
    LOAD_CHOOK(hooks,hook_handle,comatcopy,cblas_comatcopy);
    LOAD_CHOOK(hooks,hook_handle,crotg,cblas_crotg);
    LOAD_CHOOK(hooks,hook_handle,cscal,cblas_cscal);
    LOAD_CHOOK(hooks,hook_handle,csrot,cblas_csrot);
    LOAD_CHOOK(hooks,hook_handle,csscal,cblas_csscal);
    LOAD_CHOOK(hooks,hook_handle,cswap,cblas_cswap);
    LOAD_CHOOK(hooks,hook_handle,csymm,cblas_csymm);
    LOAD_CHOOK(hooks,hook_handle,csyr2k,cblas_csyr2k);
    LOAD_CHOOK(hooks,hook_handle,csyrk,cblas_csyrk);
    LOAD_CHOOK(hooks,hook_handle,ctbmv,cblas_ctbmv);
    LOAD_CHOOK(hooks,hook_handle,ctbsv,cblas_ctbsv);
    LOAD_CHOOK(hooks,hook_handle,ctpmv,cblas_ctpmv);
    LOAD_CHOOK(hooks,hook_handle,ctpsv,cblas_ctpsv);
    LOAD_CHOOK(hooks,hook_handle,ctrmm,cblas_ctrmm);
    LOAD_CHOOK(hooks,hook_handle,ctrmv,cblas_ctrmv);
    LOAD_CHOOK(hooks,hook_handle,ctrsm,cblas_ctrsm);
    LOAD_CHOOK(hooks,hook_handle,ctrsv,cblas_ctrsv);
    LOAD_CHOOK(hooks,hook_handle,dasum,cblas_dasum);
    LOAD_CHOOK(hooks,hook_handle,daxpby,cblas_daxpby);
    LOAD_CHOOK(hooks,hook_handle,daxpy,cblas_daxpy);
    LOAD_CHOOK(hooks,hook_handle,dcopy,cblas_dcopy);
    LOAD_CHOOK(hooks,hook_handle,ddot,cblas_ddot);
    LOAD_CHOOK(hooks,hook_handle,dgbmv,cblas_dgbmv);
    LOAD_CHOOK(hooks,hook_handle,dgeadd,cblas_dgeadd);
    LOAD_CHOOK(hooks,hook_handle,dgemm,cblas_dgemm);
    LOAD_CHOOK(hooks,hook_handle,dgemmtr,cblas_dgemmtr);
    LOAD_CHOOK(hooks,hook_handle,dgemv,cblas_dgemv);
    LOAD_CHOOK(hooks,hook_handle,dger,cblas_dger);
    LOAD_CHOOK(hooks,hook_handle,dimatcopy,cblas_dimatcopy);
    LOAD_CHOOK(hooks,hook_handle,dnrm2,cblas_dnrm2);
    LOAD_CHOOK(hooks,hook_handle,domatcopy,cblas_domatcopy);
    LOAD_CHOOK(hooks,hook_handle,drot,cblas_drot);
    LOAD_CHOOK(hooks,hook_handle,drotg,cblas_drotg);
    LOAD_CHOOK(hooks,hook_handle,drotm,cblas_drotm);
    LOAD_CHOOK(hooks,hook_handle,drotmg,cblas_drotmg);
    LOAD_CHOOK(hooks,hook_handle,dsbmv,cblas_dsbmv);
    LOAD_CHOOK(hooks,hook_handle,dscal,cblas_dscal);
    LOAD_CHOOK(hooks,hook_handle,dsdot,cblas_dsdot);
    LOAD_CHOOK(hooks,hook_handle,dspmv,cblas_dspmv);
    LOAD_CHOOK(hooks,hook_handle,dspr,cblas_dspr);
    LOAD_CHOOK(hooks,hook_handle,dspr2,cblas_dspr2);
    LOAD_CHOOK(hooks,hook_handle,dswap,cblas_dswap);
    LOAD_CHOOK(hooks,hook_handle,dsymm,cblas_dsymm);
    LOAD_CHOOK(hooks,hook_handle,dsymv,cblas_dsymv);
    LOAD_CHOOK(hooks,hook_handle,dsyr,cblas_dsyr);
    LOAD_CHOOK(hooks,hook_handle,dsyr2,cblas_dsyr2);
    LOAD_CHOOK(hooks,hook_handle,dsyr2k,cblas_dsyr2k);
    LOAD_CHOOK(hooks,hook_handle,dsyrk,cblas_dsyrk);
    LOAD_CHOOK(hooks,hook_handle,dtbmv,cblas_dtbmv);
    LOAD_CHOOK(hooks,hook_handle,dtbsv,cblas_dtbsv);
    LOAD_CHOOK(hooks,hook_handle,dtpmv,cblas_dtpmv);
    LOAD_CHOOK(hooks,hook_handle,dtpsv,cblas_dtpsv);
    LOAD_CHOOK(hooks,hook_handle,dtrmm,cblas_dtrmm);
    LOAD_CHOOK(hooks,hook_handle,dtrmv,cblas_dtrmv);
    LOAD_CHOOK(hooks,hook_handle,dtrsm,cblas_dtrsm);
    LOAD_CHOOK(hooks,hook_handle,dtrsv,cblas_dtrsv);
    LOAD_CHOOK(hooks,hook_handle,dzasum,cblas_dzasum);
    LOAD_CHOOK(hooks,hook_handle,dznrm2,cblas_dznrm2);
    LOAD_CHOOK(hooks,hook_handle,icamax,cblas_icamax);
    LOAD_CHOOK(hooks,hook_handle,idamax,cblas_idamax);
    LOAD_CHOOK(hooks,hook_handle,isamax,cblas_isamax);
    LOAD_CHOOK(hooks,hook_handle,izamax,cblas_izamax);
    LOAD_CHOOK(hooks,hook_handle,sasum,cblas_sasum);
    LOAD_CHOOK(hooks,hook_handle,saxpby,cblas_saxpby);
    LOAD_CHOOK(hooks,hook_handle,saxpy,cblas_saxpy);
    LOAD_CHOOK(hooks,hook_handle,scasum,cblas_scasum);
    LOAD_CHOOK(hooks,hook_handle,scnrm2,cblas_scnrm2);
    LOAD_CHOOK(hooks,hook_handle,scopy,cblas_scopy);
    LOAD_CHOOK(hooks,hook_handle,sdot,cblas_sdot);
    LOAD_CHOOK(hooks,hook_handle,sdsdot,cblas_sdsdot);
    LOAD_CHOOK(hooks,hook_handle,sgbmv,cblas_sgbmv);
    LOAD_CHOOK(hooks,hook_handle,sgeadd,cblas_sgeadd);
    LOAD_CHOOK(hooks,hook_handle,sgemm,cblas_sgemm);
    LOAD_CHOOK(hooks,hook_handle,sgemmtr,cblas_sgemmtr);
    LOAD_CHOOK(hooks,hook_handle,sgemv,cblas_sgemv);
    LOAD_CHOOK(hooks,hook_handle,sger,cblas_sger);
    LOAD_CHOOK(hooks,hook_handle,simatcopy,cblas_simatcopy);
    LOAD_CHOOK(hooks,hook_handle,snrm2,cblas_snrm2);
    LOAD_CHOOK(hooks,hook_handle,somatcopy,cblas_somatcopy);
    LOAD_CHOOK(hooks,hook_handle,srot,cblas_srot);
    LOAD_CHOOK(hooks,hook_handle,srotg,cblas_srotg);
    LOAD_CHOOK(hooks,hook_handle,srotm,cblas_srotm);
    LOAD_CHOOK(hooks,hook_handle,srotmg,cblas_srotmg);
    LOAD_CHOOK(hooks,hook_handle,ssbmv,cblas_ssbmv);
    LOAD_CHOOK(hooks,hook_handle,sscal,cblas_sscal);
    LOAD_CHOOK(hooks,hook_handle,sspmv,cblas_sspmv);
    LOAD_CHOOK(hooks,hook_handle,sspr,cblas_sspr);
    LOAD_CHOOK(hooks,hook_handle,sspr2,cblas_sspr2);
    LOAD_CHOOK(hooks,hook_handle,sswap,cblas_sswap);
    LOAD_CHOOK(hooks,hook_handle,ssymm,cblas_ssymm);
    LOAD_CHOOK(hooks,hook_handle,ssymv,cblas_ssymv);
    LOAD_CHOOK(hooks,hook_handle,ssyr,cblas_ssyr);
    LOAD_CHOOK(hooks,hook_handle,ssyr2,cblas_ssyr2);
    LOAD_CHOOK(hooks,hook_handle,ssyr2k,cblas_ssyr2k);
    LOAD_CHOOK(hooks,hook_handle,ssyrk,cblas_ssyrk);
    LOAD_CHOOK(hooks,hook_handle,stbmv,cblas_stbmv);
    LOAD_CHOOK(hooks,hook_handle,stbsv,cblas_stbsv);
    LOAD_CHOOK(hooks,hook_handle,stpmv,cblas_stpmv);
    LOAD_CHOOK(hooks,hook_handle,stpsv,cblas_stpsv);
    LOAD_CHOOK(hooks,hook_handle,strmm,cblas_strmm);
    LOAD_CHOOK(hooks,hook_handle,strmv,cblas_strmv);
    LOAD_CHOOK(hooks,hook_handle,strsm,cblas_strsm);
    LOAD_CHOOK(hooks,hook_handle,strsv,cblas_strsv);
    LOAD_CHOOK(hooks,hook_handle,zaxpby,cblas_zaxpby);
    LOAD_CHOOK(hooks,hook_handle,zaxpy,cblas_zaxpy);
    LOAD_CHOOK(hooks,hook_handle,zcopy,cblas_zcopy);
    LOAD_CHOOK(hooks,hook_handle,zdotc_sub,cblas_zdotc_sub);
    LOAD_CHOOK(hooks,hook_handle,zdotu_sub,cblas_zdotu_sub);
    LOAD_CHOOK(hooks,hook_handle,zdrot,cblas_zdrot);
    LOAD_CHOOK(hooks,hook_handle,zdscal,cblas_zdscal);
    LOAD_CHOOK(hooks,hook_handle,zgbmv,cblas_zgbmv);
    LOAD_CHOOK(hooks,hook_handle,zgeadd,cblas_zgeadd);
    LOAD_CHOOK(hooks,hook_handle,zgemm,cblas_zgemm);
    LOAD_CHOOK(hooks,hook_handle,zgemmtr,cblas_zgemmtr);
    LOAD_CHOOK(hooks,hook_handle,zgemv,cblas_zgemv);
    LOAD_CHOOK(hooks,hook_handle,zgerc,cblas_zgerc);
    LOAD_CHOOK(hooks,hook_handle,zgeru,cblas_zgeru);
    LOAD_CHOOK(hooks,hook_handle,zhbmv,cblas_zhbmv);
    LOAD_CHOOK(hooks,hook_handle,zhemm,cblas_zhemm);
    LOAD_CHOOK(hooks,hook_handle,zhemv,cblas_zhemv);
    LOAD_CHOOK(hooks,hook_handle,zher,cblas_zher);
    LOAD_CHOOK(hooks,hook_handle,zher2,cblas_zher2);
    LOAD_CHOOK(hooks,hook_handle,zher2k,cblas_zher2k);
    LOAD_CHOOK(hooks,hook_handle,zherk,cblas_zherk);
    LOAD_CHOOK(hooks,hook_handle,zhpmv,cblas_zhpmv);
    LOAD_CHOOK(hooks,hook_handle,zhpr,cblas_zhpr);
    LOAD_CHOOK(hooks,hook_handle,zhpr2,cblas_zhpr2);
    LOAD_CHOOK(hooks,hook_handle,zimatcopy,cblas_zimatcopy);
    LOAD_CHOOK(hooks,hook_handle,zomatcopy,cblas_zomatcopy);
    LOAD_CHOOK(hooks,hook_handle,zrotg,cblas_zrotg);
    LOAD_CHOOK(hooks,hook_handle,zscal,cblas_zscal);
    LOAD_CHOOK(hooks,hook_handle,zswap,cblas_zswap);
    LOAD_CHOOK(hooks,hook_handle,zsymm,cblas_zsymm);
    LOAD_CHOOK(hooks,hook_handle,zsyr2k,cblas_zsyr2k);
    LOAD_CHOOK(hooks,hook_handle,zsyrk,cblas_zsyrk);
    LOAD_CHOOK(hooks,hook_handle,ztbmv,cblas_ztbmv);
    LOAD_CHOOK(hooks,hook_handle,ztbsv,cblas_ztbsv);
    LOAD_CHOOK(hooks,hook_handle,ztpmv,cblas_ztpmv);
    LOAD_CHOOK(hooks,hook_handle,ztpsv,cblas_ztpsv);
    LOAD_CHOOK(hooks,hook_handle,ztrmm,cblas_ztrmm);
    LOAD_CHOOK(hooks,hook_handle,ztrmv,cblas_ztrmv);
    LOAD_CHOOK(hooks,hook_handle,ztrsm,cblas_ztrsm);
    LOAD_CHOOK(hooks,hook_handle,ztrsv,cblas_ztrsv);


    return 0; 
}
#endif 


