//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2026 Martin Koehler

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
#define CBLAS_FUNC_HOOK_VAR(function_name) \
  static TLS_STORE uint8_t hook_pos_ ## function_name = 0;
#define CBLAS_FUNC_HOOK_FUNCS(return_type, function_name, call_kw, aftercall_kw, arg_list, arg_list_void, call_list, component) \
  return_type flexiblas_real_ ## function_name ## _ arg_list \
  {{ \
      return_type (*fn) arg_list_void; \
      *(void **) &fn = current_backend->component.function_name; \
      call_kw fn call_list; \
      aftercall_kw; \
  }} \
  return_type flexiblas_chain_ ## function_name ## _ arg_list_void \
  {{ \
      return_type (*fn) arg_list_void; \
      hook_pos_ ## function_name++; \
      if ( hook_pos_ ## function_name < __flexiblas_hooks->function_name.nhook ) {{ \
          *(void **) &fn = __flexiblas_hooks->function_name.hook_function[hook_pos_ ## function_name]; \
      }} else {{ \
          hook_pos_ ## function_name = 0; \
          *(void **) &fn = current_backend->component.function_name; \
      }} \
      call_kw fn call_list; \
      aftercall_kw; \
  }}
#define CBLAS_FUNC_HOOK_BEFORE(return_type, function_name, arg_list_void) \
    return_type (*fn_hook) arg_list_void; \
    hook_pos_ ## function_name = 0;
#define CBLAS_FUNC_HOOK_AFTER(function_name, call_kw, call_list) \
    *(void **) &fn_hook = __flexiblas_hooks->function_name.hook_function[0]; \
    if ( fn_hook != NULL ) {{ \
        call_kw fn_hook call_list; \
    }} else {{ \
        call_kw fn call_list; \
    }}
#else
#define CBLAS_FUNC_HOOK_VAR(function_name)
#define CBLAS_FUNC_HOOK_FUNCS(return_type, function_name, call_kw, aftercall_kw, arg_list, arg_list_void, call_list, component)
#define CBLAS_FUNC_HOOK_BEFORE(return_type, function_name, arg_list_void)
#define CBLAS_FUNC_HOOK_AFTER(function_name, call_kw, call_list) \
    call_kw fn call_list;
#endif
#define CBLAS_FUNC(return_type, function_name, call_kw, aftercall_kw, arg_list, arg_list_void, call_list, component) \
  CBLAS_FUNC_HOOK_VAR(function_name) \
  return_type function_name arg_list \
  {{ \
      return_type (*fn) arg_list_void; \
      CBLAS_FUNC_HOOK_BEFORE(return_type, function_name, arg_list_void) \
      /* Post init if necessary */ \
      if ( unlikely(current_backend->post_init != 0) ) {{ \
          __flexiblas_backend_init(current_backend); \
          current_backend->post_init = 0; \
      }} \
      *(void **) &fn = current_backend->component.function_name; \
      CBLAS_FUNC_HOOK_AFTER(function_name, call_kw, call_list) \
      aftercall_kw; \
  }} \
  CBLAS_FUNC_HOOK_FUNCS(return_type, function_name, call_kw, aftercall_kw, arg_list, arg_list_void, call_list, component)

CBLAS_FUNC(void, cblas_caxpby, , return,
  (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (N, alpha, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_caxpy, , return,
  (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, alpha, X, incX, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_ccopy, , return,
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, X, incX, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_cdotc_sub, , return,
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc),
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc),
  (N, X, incX, Y, incY, dotc),
  cblas)
CBLAS_FUNC(void, cblas_cdotu_sub, , return,
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu),
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu),
  (N, X, incX, Y, incY, dotu),
  cblas)
CBLAS_FUNC(void, cblas_cgbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_cgeadd, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb),
  (CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb),
  cblas)
CBLAS_FUNC(void, cblas_cgemm, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_cgemmtr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
/* Alternative function name for cblas_cgemmtr -- cblas_cgemmt */
#ifndef __APPLE__
void cblas_cgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc) __attribute__((alias(MTS(cblas_cgemmtr))));
#else
void cblas_cgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc) 
{
    cblas_cgemmtr(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
}

#endif

CBLAS_FUNC(void, cblas_cgemv, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_cgerc, , return,
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, M, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_cgeru, , return,
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, M, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_chbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_chemm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_chemv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_cher, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda),
  (layout, Uplo, N, alpha, X, incX, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_cher2, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, Uplo, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_cher2k, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_cherk, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_chpmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_chpr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A),
  (layout, Uplo, N, alpha, X, incX, A),
  cblas)
CBLAS_FUNC(void, cblas_chpr2, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap),
  (layout, Uplo, N, alpha, X, incX, Y, incY, Ap),
  cblas)
CBLAS_FUNC(void, cblas_cimatcopy, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb),
  (CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb),
  cblas)
CBLAS_FUNC(void, cblas_comatcopy, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb),
  (CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb),
  cblas)
CBLAS_FUNC(void, cblas_crotg, , return,
  (void *a, void *b, float *c, void *s),
  (void *a, void *b, void *c, void *s),
  (a, b, c, s),
  cblas)
CBLAS_FUNC(void, cblas_cscal, , return,
  (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX),
  (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX),
  (N, alpha, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_csrot, , return,
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s),
  (N, X, incX, Y, incY, c, s),
  cblas)
CBLAS_FUNC(void, cblas_csscal, , return,
  (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX),
  (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX),
  (N, alpha, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_cswap, , return,
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, X, incX, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_csymm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_csyr2k, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_csyrk, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_ctbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, K, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ctbsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, K, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ctpmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, Ap, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ctpsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, Ap, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ctrmm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb),
  cblas)
CBLAS_FUNC(void, cblas_ctrmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ctrsm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb),
  cblas)
CBLAS_FUNC(void, cblas_ctrsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, A, lda, X, incX),
  cblas)
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
CBLAS_FUNC(void, cblas_daxpby, , return,
  (const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY),
  (N, alpha, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_daxpy, , return,
  (const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, alpha, X, incX, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_dcopy, , return,
  (const CBLAS_INT N, const double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, X, incX, Y, incY),
  cblas)
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
CBLAS_FUNC(void, cblas_dgbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY),
  (layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_dgeadd, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, double *a, const CBLAS_INT clda, const double cbeta, double *b, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const double cbeta, void *b, const CBLAS_INT cldb),
  (CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb),
  cblas)
CBLAS_FUNC(void, cblas_dgemm, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc),
  (layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_dgemmtr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc),
  (layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
/* Alternative function name for cblas_dgemmtr -- cblas_dgemmt */
#ifndef __APPLE__
void cblas_dgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc) __attribute__((alias(MTS(cblas_dgemmtr))));
#else
void cblas_dgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc) 
{
    cblas_dgemmtr(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
}

#endif

CBLAS_FUNC(void, cblas_dgemv, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY),
  (layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_dger, , return,
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY, double *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, M, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_dimatcopy, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, double *a, const CBLAS_INT clda, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb),
  (CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb),
  cblas)
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
CBLAS_FUNC(void, cblas_domatcopy, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const double *a, const CBLAS_INT clda, double *b, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const double calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb),
  (CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb),
  cblas)
CBLAS_FUNC(void, cblas_drot, , return,
  (const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY, const double c, const double s),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s),
  (N, X, incX, Y, incY, c, s),
  cblas)
CBLAS_FUNC(void, cblas_drotg, , return,
  (double *a, double *b, double *c, double *s),
  (void *a, void *b, void *c, void *s),
  (a, b, c, s),
  cblas)
CBLAS_FUNC(void, cblas_drotm, , return,
  (const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY, const double *P),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P),
  (N, X, incX, Y, incY, P),
  cblas)
CBLAS_FUNC(void, cblas_drotmg, , return,
  (double *d1, double *d2, double *b1, const double b2, double *P),
  (void *d1, void *d2, void *b1, const double b2, void *P),
  (d1, d2, b1, b2, P),
  cblas)
CBLAS_FUNC(void, cblas_dsbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_dscal, , return,
  (const CBLAS_INT N, const double alpha, double *X, const CBLAS_INT incX),
  (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX),
  (N, alpha, X, incX),
  cblas)
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
CBLAS_FUNC(void, cblas_dspmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *Ap, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *Ap, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_dspr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, double *Ap),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *Ap),
  (layout, Uplo, N, alpha, X, incX, Ap),
  cblas)
CBLAS_FUNC(void, cblas_dspr2, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY, double *A),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A),
  (layout, Uplo, N, alpha, X, incX, Y, incY, A),
  cblas)
CBLAS_FUNC(void, cblas_dswap, , return,
  (const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, X, incX, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_dsymm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc),
  (layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_dsymv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, const double *X, const CBLAS_INT incX, const double beta, double *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const double beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_dsyr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, double *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda),
  (layout, Uplo, N, alpha, X, incX, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_dsyr2, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const double *X, const CBLAS_INT incX, const double *Y, const CBLAS_INT incY, double *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, Uplo, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_dsyr2k, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double *B, const CBLAS_INT ldb, const double beta, double *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_dsyrk, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const double *A, const CBLAS_INT lda, const double beta, double *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_dtbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, K, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_dtbsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, K, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_dtpmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *Ap, double *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, Ap, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_dtpsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *Ap, double *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, Ap, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_dtrmm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, double *B, const CBLAS_INT ldb),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb),
  cblas)
CBLAS_FUNC(void, cblas_dtrmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_dtrsm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const double *A, const CBLAS_INT lda, double *B, const CBLAS_INT ldb),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const double alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb),
  cblas)
CBLAS_FUNC(void, cblas_dtrsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const double *A, const CBLAS_INT lda, double *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, A, lda, X, incX),
  cblas)
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
CBLAS_FUNC(void, cblas_saxpby, , return,
  (const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY),
  (N, alpha, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_saxpy, , return,
  (const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, alpha, X, incX, Y, incY),
  cblas)
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
CBLAS_FUNC(void, cblas_scopy, , return,
  (const CBLAS_INT N, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, X, incX, Y, incY),
  cblas)
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
CBLAS_FUNC(void, cblas_sgbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY),
  (layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_sgeadd, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda, const float cbeta, float *b, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const float cbeta, void *b, const CBLAS_INT cldb),
  (CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb),
  cblas)
CBLAS_FUNC(void, cblas_sgemm, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc),
  (layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_sgemmtr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc),
  (layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
/* Alternative function name for cblas_sgemmtr -- cblas_sgemmt */
#ifndef __APPLE__
void cblas_sgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc) __attribute__((alias(MTS(cblas_sgemmtr))));
#else
void cblas_sgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc) 
{
    cblas_sgemmtr(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
}

#endif

CBLAS_FUNC(void, cblas_sgemv, , return,
  (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY),
  (const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY),
  (layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_sger, , return,
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY, float *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, M, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_simatcopy, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb),
  (CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb),
  cblas)
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
CBLAS_FUNC(void, cblas_somatcopy, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const float *a, const CBLAS_INT clda, float *b, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb),
  (CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb),
  cblas)
CBLAS_FUNC(void, cblas_srot, , return,
  (const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY, const float c, const float s),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const float c, const float s),
  (N, X, incX, Y, incY, c, s),
  cblas)
CBLAS_FUNC(void, cblas_srotg, , return,
  (float *a, float *b, float *c, float *s),
  (void *a, void *b, void *c, void *s),
  (a, b, c, s),
  cblas)
CBLAS_FUNC(void, cblas_srotm, , return,
  (const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY, const float *P),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const void *P),
  (N, X, incX, Y, incY, P),
  cblas)
CBLAS_FUNC(void, cblas_srotmg, , return,
  (float *d1, float *d2, float *b1, const float b2, float *P),
  (void *d1, void *d2, void *b1, const float b2, void *P),
  (d1, d2, b1, b2, P),
  cblas)
CBLAS_FUNC(void, cblas_ssbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_sscal, , return,
  (const CBLAS_INT N, const float alpha, float *X, const CBLAS_INT incX),
  (const CBLAS_INT N, const float alpha, void *X, const CBLAS_INT incX),
  (N, alpha, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_sspmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *Ap, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *Ap, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_sspr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Ap),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *Ap),
  (layout, Uplo, N, alpha, X, incX, Ap),
  cblas)
CBLAS_FUNC(void, cblas_sspr2, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY, float *A),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A),
  (layout, Uplo, N, alpha, X, incX, Y, incY, A),
  cblas)
CBLAS_FUNC(void, cblas_sswap, , return,
  (const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, X, incX, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_ssymm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc),
  (layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_ssymv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, const float *X, const CBLAS_INT incX, const float beta, float *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const float beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_ssyr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda),
  (layout, Uplo, N, alpha, X, incX, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_ssyr2, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, const float *Y, const CBLAS_INT incY, float *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, Uplo, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_ssyr2k, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float *B, const CBLAS_INT ldb, const float beta, float *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const float beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_ssyrk, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A, const CBLAS_INT lda, const float beta, float *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const float alpha, const void *A, const CBLAS_INT lda, const float beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_stbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, K, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_stbsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, K, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_stpmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *Ap, float *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, Ap, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_stpsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *Ap, float *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, Ap, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_strmm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, float *B, const CBLAS_INT ldb),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb),
  cblas)
CBLAS_FUNC(void, cblas_strmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_strsm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const float *A, const CBLAS_INT lda, float *B, const CBLAS_INT ldb),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const float alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb),
  cblas)
CBLAS_FUNC(void, cblas_strsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const float *A, const CBLAS_INT lda, float *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_zaxpby, , return,
  (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (N, alpha, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_zaxpy, , return,
  (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, alpha, X, incX, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_zcopy, , return,
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, X, incX, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_zdotc_sub, , return,
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc),
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotc),
  (N, X, incX, Y, incY, dotc),
  cblas)
CBLAS_FUNC(void, cblas_zdotu_sub, , return,
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu),
  (const CBLAS_INT N, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *dotu),
  (N, X, incX, Y, incY, dotu),
  cblas)
CBLAS_FUNC(void, cblas_zdrot, , return,
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY, const double c, const double s),
  (N, X, incX, Y, incY, c, s),
  cblas)
CBLAS_FUNC(void, cblas_zdscal, , return,
  (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX),
  (const CBLAS_INT N, const double alpha, void *X, const CBLAS_INT incX),
  (N, alpha, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_zgbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT KL, const CBLAS_INT KU, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, TransA, M, N, KL, KU, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_zgeadd, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const void *cbeta, void *b, const CBLAS_INT cldb),
  (CORDER, crows, ccols, calpha, a, clda, cbeta, b, cldb),
  cblas)
CBLAS_FUNC(void, cblas_zgemm, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, TransA, TransB, M, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_zgemmtr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
/* Alternative function name for cblas_zgemmtr -- cblas_zgemmt */
#ifndef __APPLE__
void cblas_zgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc) __attribute__((alias(MTS(cblas_zgemmtr))));
#else
void cblas_zgemmt(CBLAS_LAYOUT layout, CBLAS_UPLO uplo, CBLAS_TRANSPOSE TransA, CBLAS_TRANSPOSE TransB, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc) 
{
    cblas_zgemmtr(layout, uplo, TransA, TransB, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
}

#endif

CBLAS_FUNC(void, cblas_zgemv, , return,
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, TransA, M, N, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_zgerc, , return,
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, M, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_zgeru, , return,
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, M, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_zhbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, K, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_zhemm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_zhemv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, alpha, A, lda, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_zher, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A, const CBLAS_INT lda),
  (layout, Uplo, N, alpha, X, incX, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_zher2, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda),
  (layout, Uplo, N, alpha, X, incX, Y, incY, A, lda),
  cblas)
CBLAS_FUNC(void, cblas_zher2k, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const double beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_zherk, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const double alpha, const void *A, const CBLAS_INT lda, const double beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_zhpmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *Ap, const void *X, const CBLAS_INT incX, const void *beta, void *Y, const CBLAS_INT incY),
  (layout, Uplo, N, alpha, Ap, X, incX, beta, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_zhpr, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX, void *A),
  (layout, Uplo, N, alpha, X, incX, A),
  cblas)
CBLAS_FUNC(void, cblas_zhpr2, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX, const void *Y, const CBLAS_INT incY, void *Ap),
  (layout, Uplo, N, alpha, X, incX, Y, incY, Ap),
  cblas)
CBLAS_FUNC(void, cblas_zimatcopy, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, void *a, const CBLAS_INT clda, const CBLAS_INT cldb),
  (CORDER, CTRANS, crows, ccols, calpha, a, clda, cldb),
  cblas)
CBLAS_FUNC(void, cblas_zomatcopy, , return,
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb),
  (const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS, const CBLAS_INT crows, const CBLAS_INT ccols, const void *calpha, const void *a, const CBLAS_INT clda, void *b, const CBLAS_INT cldb),
  (CORDER, CTRANS, crows, ccols, calpha, a, clda, b, cldb),
  cblas)
CBLAS_FUNC(void, cblas_zrotg, , return,
  (void *a, void *b, double *c, void *s),
  (void *a, void *b, void *c, void *s),
  (a, b, c, s),
  cblas)
CBLAS_FUNC(void, cblas_zscal, , return,
  (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX),
  (const CBLAS_INT N, const void *alpha, void *X, const CBLAS_INT incX),
  (N, alpha, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_zswap, , return,
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y, const CBLAS_INT incY),
  (N, X, incX, Y, incY),
  cblas)
CBLAS_FUNC(void, cblas_zsymm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_zsyr2k, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *B, const CBLAS_INT ldb, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, B, ldb, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_zsyrk, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K, const void *alpha, const void *A, const CBLAS_INT lda, const void *beta, void *C, const CBLAS_INT ldc),
  (layout, Uplo, Trans, N, K, alpha, A, lda, beta, C, ldc),
  cblas)
CBLAS_FUNC(void, cblas_ztbmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, K, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ztbsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const CBLAS_INT K, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, K, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ztpmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, Ap, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ztpsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *Ap, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, Ap, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ztrmm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb),
  cblas)
CBLAS_FUNC(void, cblas_ztrmv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, A, lda, X, incX),
  cblas)
CBLAS_FUNC(void, cblas_ztrsm, , return,
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (CBLAS_LAYOUT layout, CBLAS_SIDE Side, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N, const void *alpha, const void *A, const CBLAS_INT lda, void *B, const CBLAS_INT ldb),
  (layout, Side, Uplo, TransA, Diag, M, N, alpha, A, lda, B, ldb),
  cblas)
CBLAS_FUNC(void, cblas_ztrsv, , return,
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (CBLAS_LAYOUT layout, CBLAS_UPLO Uplo, CBLAS_TRANSPOSE TransA, CBLAS_DIAG Diag, const CBLAS_INT N, const void *A, const CBLAS_INT lda, void *X, const CBLAS_INT incX),
  (layout, Uplo, TransA, Diag, N, A, lda, X, incX),
  cblas)

#define REMOVE_BRACKETS(...) __VA_ARGS__
#define FBLAS_CBLAS_LOAD(name, function_blas_name, symbol_names, symbol_names_api) \
  do { \
    void *ptr_library = __flexiblas_lookup_cblas_function(backend->library_handle, REMOVE_BRACKETS symbol_names, NULL); \
    void *ptr_fallback = __flexiblas_lookup_cblas_function(__flexiblas_blas_fallback, REMOVE_BRACKETS symbol_names, NULL); \
    if ( ptr_library == NULL && ptr_fallback == NULL ) { \
        flexiblas_print_error("flexiblas",__FILE__, __LINE__, "Can not load " #name ".\n"); \
        failed++; \
        break; \
    }  \
    if ( only_fallback || ptr_library == NULL) { \
        DPRINTF(2, "Load " #name " from internal fallback BLAS.\n"); \
        backend->cblas.name = ptr_fallback; \
    } else { \
        backend->cblas.name = ptr_library; \
    } \
  } while(0)
HIDDEN int __flexiblas_load_cblas( flexiblas_backend_t *backend )  {
    int only_fallback = 0;
    int failed = 0;
    void * cblas_in_blis = __flexiblas_dlsym(backend->library_handle, "bli_info_get_enable_cblas");
    if ( cblas_in_blis ) {
        DPRINTF_WARN(1, "The desired BLAS library is BLIS. We do not load their CBLAS wrapper since it might alter the behavior of your programs.\n");
        only_fallback = 1;
    }

    FBLAS_CBLAS_LOAD(cblas_caxpby, caxpby, ("cblas_caxpby"), ("cblas_caxpby_64"));
    FBLAS_CBLAS_LOAD(cblas_caxpy, caxpy, ("cblas_caxpy"), ("cblas_caxpy_64"));
    FBLAS_CBLAS_LOAD(cblas_ccopy, ccopy, ("cblas_ccopy"), ("cblas_ccopy_64"));
    FBLAS_CBLAS_LOAD(cblas_cdotc_sub, cdotc_sub, ("cblas_cdotc_sub"), ("cblas_cdotc_sub_64"));
    FBLAS_CBLAS_LOAD(cblas_cdotu_sub, cdotu_sub, ("cblas_cdotu_sub"), ("cblas_cdotu_sub_64"));
    FBLAS_CBLAS_LOAD(cblas_cgbmv, cgbmv, ("cblas_cgbmv"), ("cblas_cgbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_cgeadd, cgeadd, ("cblas_cgeadd"), ("cblas_cgeadd_64"));
    FBLAS_CBLAS_LOAD(cblas_cgemm, cgemm, ("cblas_cgemm"), ("cblas_cgemm_64"));
    FBLAS_CBLAS_LOAD(cblas_cgemmtr, cgemmtr, ("cblas_cgemmtr", "cblas_cgemmt"), ("cblas_cgemmtr_64", "cblas_cgemmt_64"));
    FBLAS_CBLAS_LOAD(cblas_cgemv, cgemv, ("cblas_cgemv"), ("cblas_cgemv_64"));
    FBLAS_CBLAS_LOAD(cblas_cgerc, cgerc, ("cblas_cgerc"), ("cblas_cgerc_64"));
    FBLAS_CBLAS_LOAD(cblas_cgeru, cgeru, ("cblas_cgeru"), ("cblas_cgeru_64"));
    FBLAS_CBLAS_LOAD(cblas_chbmv, chbmv, ("cblas_chbmv"), ("cblas_chbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_chemm, chemm, ("cblas_chemm"), ("cblas_chemm_64"));
    FBLAS_CBLAS_LOAD(cblas_chemv, chemv, ("cblas_chemv"), ("cblas_chemv_64"));
    FBLAS_CBLAS_LOAD(cblas_cher, cher, ("cblas_cher"), ("cblas_cher_64"));
    FBLAS_CBLAS_LOAD(cblas_cher2, cher2, ("cblas_cher2"), ("cblas_cher2_64"));
    FBLAS_CBLAS_LOAD(cblas_cher2k, cher2k, ("cblas_cher2k"), ("cblas_cher2k_64"));
    FBLAS_CBLAS_LOAD(cblas_cherk, cherk, ("cblas_cherk"), ("cblas_cherk_64"));
    FBLAS_CBLAS_LOAD(cblas_chpmv, chpmv, ("cblas_chpmv"), ("cblas_chpmv_64"));
    FBLAS_CBLAS_LOAD(cblas_chpr, chpr, ("cblas_chpr"), ("cblas_chpr_64"));
    FBLAS_CBLAS_LOAD(cblas_chpr2, chpr2, ("cblas_chpr2"), ("cblas_chpr2_64"));
    FBLAS_CBLAS_LOAD(cblas_cimatcopy, cimatcopy, ("cblas_cimatcopy"), ("cblas_cimatcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_comatcopy, comatcopy, ("cblas_comatcopy"), ("cblas_comatcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_crotg, crotg, ("cblas_crotg"), ("cblas_crotg_64"));
    FBLAS_CBLAS_LOAD(cblas_cscal, cscal, ("cblas_cscal"), ("cblas_cscal_64"));
    FBLAS_CBLAS_LOAD(cblas_csrot, csrot, ("cblas_csrot"), ("cblas_csrot_64"));
    FBLAS_CBLAS_LOAD(cblas_csscal, csscal, ("cblas_csscal"), ("cblas_csscal_64"));
    FBLAS_CBLAS_LOAD(cblas_cswap, cswap, ("cblas_cswap"), ("cblas_cswap_64"));
    FBLAS_CBLAS_LOAD(cblas_csymm, csymm, ("cblas_csymm"), ("cblas_csymm_64"));
    FBLAS_CBLAS_LOAD(cblas_csyr2k, csyr2k, ("cblas_csyr2k"), ("cblas_csyr2k_64"));
    FBLAS_CBLAS_LOAD(cblas_csyrk, csyrk, ("cblas_csyrk"), ("cblas_csyrk_64"));
    FBLAS_CBLAS_LOAD(cblas_ctbmv, ctbmv, ("cblas_ctbmv"), ("cblas_ctbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_ctbsv, ctbsv, ("cblas_ctbsv"), ("cblas_ctbsv_64"));
    FBLAS_CBLAS_LOAD(cblas_ctpmv, ctpmv, ("cblas_ctpmv"), ("cblas_ctpmv_64"));
    FBLAS_CBLAS_LOAD(cblas_ctpsv, ctpsv, ("cblas_ctpsv"), ("cblas_ctpsv_64"));
    FBLAS_CBLAS_LOAD(cblas_ctrmm, ctrmm, ("cblas_ctrmm"), ("cblas_ctrmm_64"));
    FBLAS_CBLAS_LOAD(cblas_ctrmv, ctrmv, ("cblas_ctrmv"), ("cblas_ctrmv_64"));
    FBLAS_CBLAS_LOAD(cblas_ctrsm, ctrsm, ("cblas_ctrsm"), ("cblas_ctrsm_64"));
    FBLAS_CBLAS_LOAD(cblas_ctrsv, ctrsv, ("cblas_ctrsv"), ("cblas_ctrsv_64"));
    FBLAS_CBLAS_LOAD(cblas_dasum, dasum, ("cblas_dasum"), ("cblas_dasum_64"));
    FBLAS_CBLAS_LOAD(cblas_daxpby, daxpby, ("cblas_daxpby"), ("cblas_daxpby_64"));
    FBLAS_CBLAS_LOAD(cblas_daxpy, daxpy, ("cblas_daxpy"), ("cblas_daxpy_64"));
    FBLAS_CBLAS_LOAD(cblas_dcopy, dcopy, ("cblas_dcopy"), ("cblas_dcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_ddot, ddot, ("cblas_ddot"), ("cblas_ddot_64"));
    FBLAS_CBLAS_LOAD(cblas_dgbmv, dgbmv, ("cblas_dgbmv"), ("cblas_dgbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_dgeadd, dgeadd, ("cblas_dgeadd"), ("cblas_dgeadd_64"));
    FBLAS_CBLAS_LOAD(cblas_dgemm, dgemm, ("cblas_dgemm"), ("cblas_dgemm_64"));
    FBLAS_CBLAS_LOAD(cblas_dgemmtr, dgemmtr, ("cblas_dgemmtr", "cblas_dgemmt"), ("cblas_dgemmtr_64", "cblas_dgemmt_64"));
    FBLAS_CBLAS_LOAD(cblas_dgemv, dgemv, ("cblas_dgemv"), ("cblas_dgemv_64"));
    FBLAS_CBLAS_LOAD(cblas_dger, dger, ("cblas_dger"), ("cblas_dger_64"));
    FBLAS_CBLAS_LOAD(cblas_dimatcopy, dimatcopy, ("cblas_dimatcopy"), ("cblas_dimatcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_dnrm2, dnrm2, ("cblas_dnrm2"), ("cblas_dnrm2_64"));
    FBLAS_CBLAS_LOAD(cblas_domatcopy, domatcopy, ("cblas_domatcopy"), ("cblas_domatcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_drot, drot, ("cblas_drot"), ("cblas_drot_64"));
    FBLAS_CBLAS_LOAD(cblas_drotg, drotg, ("cblas_drotg"), ("cblas_drotg_64"));
    FBLAS_CBLAS_LOAD(cblas_drotm, drotm, ("cblas_drotm"), ("cblas_drotm_64"));
    FBLAS_CBLAS_LOAD(cblas_drotmg, drotmg, ("cblas_drotmg"), ("cblas_drotmg_64"));
    FBLAS_CBLAS_LOAD(cblas_dsbmv, dsbmv, ("cblas_dsbmv"), ("cblas_dsbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_dscal, dscal, ("cblas_dscal"), ("cblas_dscal_64"));
    FBLAS_CBLAS_LOAD(cblas_dsdot, dsdot, ("cblas_dsdot"), ("cblas_dsdot_64"));
    FBLAS_CBLAS_LOAD(cblas_dspmv, dspmv, ("cblas_dspmv"), ("cblas_dspmv_64"));
    FBLAS_CBLAS_LOAD(cblas_dspr, dspr, ("cblas_dspr"), ("cblas_dspr_64"));
    FBLAS_CBLAS_LOAD(cblas_dspr2, dspr2, ("cblas_dspr2"), ("cblas_dspr2_64"));
    FBLAS_CBLAS_LOAD(cblas_dswap, dswap, ("cblas_dswap"), ("cblas_dswap_64"));
    FBLAS_CBLAS_LOAD(cblas_dsymm, dsymm, ("cblas_dsymm"), ("cblas_dsymm_64"));
    FBLAS_CBLAS_LOAD(cblas_dsymv, dsymv, ("cblas_dsymv"), ("cblas_dsymv_64"));
    FBLAS_CBLAS_LOAD(cblas_dsyr, dsyr, ("cblas_dsyr"), ("cblas_dsyr_64"));
    FBLAS_CBLAS_LOAD(cblas_dsyr2, dsyr2, ("cblas_dsyr2"), ("cblas_dsyr2_64"));
    FBLAS_CBLAS_LOAD(cblas_dsyr2k, dsyr2k, ("cblas_dsyr2k"), ("cblas_dsyr2k_64"));
    FBLAS_CBLAS_LOAD(cblas_dsyrk, dsyrk, ("cblas_dsyrk"), ("cblas_dsyrk_64"));
    FBLAS_CBLAS_LOAD(cblas_dtbmv, dtbmv, ("cblas_dtbmv"), ("cblas_dtbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_dtbsv, dtbsv, ("cblas_dtbsv"), ("cblas_dtbsv_64"));
    FBLAS_CBLAS_LOAD(cblas_dtpmv, dtpmv, ("cblas_dtpmv"), ("cblas_dtpmv_64"));
    FBLAS_CBLAS_LOAD(cblas_dtpsv, dtpsv, ("cblas_dtpsv"), ("cblas_dtpsv_64"));
    FBLAS_CBLAS_LOAD(cblas_dtrmm, dtrmm, ("cblas_dtrmm"), ("cblas_dtrmm_64"));
    FBLAS_CBLAS_LOAD(cblas_dtrmv, dtrmv, ("cblas_dtrmv"), ("cblas_dtrmv_64"));
    FBLAS_CBLAS_LOAD(cblas_dtrsm, dtrsm, ("cblas_dtrsm"), ("cblas_dtrsm_64"));
    FBLAS_CBLAS_LOAD(cblas_dtrsv, dtrsv, ("cblas_dtrsv"), ("cblas_dtrsv_64"));
    FBLAS_CBLAS_LOAD(cblas_dzasum, dzasum, ("cblas_dzasum"), ("cblas_dzasum_64"));
    FBLAS_CBLAS_LOAD(cblas_dznrm2, dznrm2, ("cblas_dznrm2"), ("cblas_dznrm2_64"));
    FBLAS_CBLAS_LOAD(cblas_icamax, icamax, ("cblas_icamax"), ("cblas_icamax_64"));
    FBLAS_CBLAS_LOAD(cblas_idamax, idamax, ("cblas_idamax"), ("cblas_idamax_64"));
    FBLAS_CBLAS_LOAD(cblas_isamax, isamax, ("cblas_isamax"), ("cblas_isamax_64"));
    FBLAS_CBLAS_LOAD(cblas_izamax, izamax, ("cblas_izamax"), ("cblas_izamax_64"));
    FBLAS_CBLAS_LOAD(cblas_sasum, sasum, ("cblas_sasum"), ("cblas_sasum_64"));
    FBLAS_CBLAS_LOAD(cblas_saxpby, saxpby, ("cblas_saxpby"), ("cblas_saxpby_64"));
    FBLAS_CBLAS_LOAD(cblas_saxpy, saxpy, ("cblas_saxpy"), ("cblas_saxpy_64"));
    FBLAS_CBLAS_LOAD(cblas_scasum, scasum, ("cblas_scasum"), ("cblas_scasum_64"));
    FBLAS_CBLAS_LOAD(cblas_scnrm2, scnrm2, ("cblas_scnrm2"), ("cblas_scnrm2_64"));
    FBLAS_CBLAS_LOAD(cblas_scopy, scopy, ("cblas_scopy"), ("cblas_scopy_64"));
    FBLAS_CBLAS_LOAD(cblas_sdot, sdot, ("cblas_sdot"), ("cblas_sdot_64"));
    FBLAS_CBLAS_LOAD(cblas_sdsdot, sdsdot, ("cblas_sdsdot"), ("cblas_sdsdot_64"));
    FBLAS_CBLAS_LOAD(cblas_sgbmv, sgbmv, ("cblas_sgbmv"), ("cblas_sgbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_sgeadd, sgeadd, ("cblas_sgeadd"), ("cblas_sgeadd_64"));
    FBLAS_CBLAS_LOAD(cblas_sgemm, sgemm, ("cblas_sgemm"), ("cblas_sgemm_64"));
    FBLAS_CBLAS_LOAD(cblas_sgemmtr, sgemmtr, ("cblas_sgemmtr", "cblas_sgemmt"), ("cblas_sgemmtr_64", "cblas_sgemmt_64"));
    FBLAS_CBLAS_LOAD(cblas_sgemv, sgemv, ("cblas_sgemv"), ("cblas_sgemv_64"));
    FBLAS_CBLAS_LOAD(cblas_sger, sger, ("cblas_sger"), ("cblas_sger_64"));
    FBLAS_CBLAS_LOAD(cblas_simatcopy, simatcopy, ("cblas_simatcopy"), ("cblas_simatcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_snrm2, snrm2, ("cblas_snrm2"), ("cblas_snrm2_64"));
    FBLAS_CBLAS_LOAD(cblas_somatcopy, somatcopy, ("cblas_somatcopy"), ("cblas_somatcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_srot, srot, ("cblas_srot"), ("cblas_srot_64"));
    FBLAS_CBLAS_LOAD(cblas_srotg, srotg, ("cblas_srotg"), ("cblas_srotg_64"));
    FBLAS_CBLAS_LOAD(cblas_srotm, srotm, ("cblas_srotm"), ("cblas_srotm_64"));
    FBLAS_CBLAS_LOAD(cblas_srotmg, srotmg, ("cblas_srotmg"), ("cblas_srotmg_64"));
    FBLAS_CBLAS_LOAD(cblas_ssbmv, ssbmv, ("cblas_ssbmv"), ("cblas_ssbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_sscal, sscal, ("cblas_sscal"), ("cblas_sscal_64"));
    FBLAS_CBLAS_LOAD(cblas_sspmv, sspmv, ("cblas_sspmv"), ("cblas_sspmv_64"));
    FBLAS_CBLAS_LOAD(cblas_sspr, sspr, ("cblas_sspr"), ("cblas_sspr_64"));
    FBLAS_CBLAS_LOAD(cblas_sspr2, sspr2, ("cblas_sspr2"), ("cblas_sspr2_64"));
    FBLAS_CBLAS_LOAD(cblas_sswap, sswap, ("cblas_sswap"), ("cblas_sswap_64"));
    FBLAS_CBLAS_LOAD(cblas_ssymm, ssymm, ("cblas_ssymm"), ("cblas_ssymm_64"));
    FBLAS_CBLAS_LOAD(cblas_ssymv, ssymv, ("cblas_ssymv"), ("cblas_ssymv_64"));
    FBLAS_CBLAS_LOAD(cblas_ssyr, ssyr, ("cblas_ssyr"), ("cblas_ssyr_64"));
    FBLAS_CBLAS_LOAD(cblas_ssyr2, ssyr2, ("cblas_ssyr2"), ("cblas_ssyr2_64"));
    FBLAS_CBLAS_LOAD(cblas_ssyr2k, ssyr2k, ("cblas_ssyr2k"), ("cblas_ssyr2k_64"));
    FBLAS_CBLAS_LOAD(cblas_ssyrk, ssyrk, ("cblas_ssyrk"), ("cblas_ssyrk_64"));
    FBLAS_CBLAS_LOAD(cblas_stbmv, stbmv, ("cblas_stbmv"), ("cblas_stbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_stbsv, stbsv, ("cblas_stbsv"), ("cblas_stbsv_64"));
    FBLAS_CBLAS_LOAD(cblas_stpmv, stpmv, ("cblas_stpmv"), ("cblas_stpmv_64"));
    FBLAS_CBLAS_LOAD(cblas_stpsv, stpsv, ("cblas_stpsv"), ("cblas_stpsv_64"));
    FBLAS_CBLAS_LOAD(cblas_strmm, strmm, ("cblas_strmm"), ("cblas_strmm_64"));
    FBLAS_CBLAS_LOAD(cblas_strmv, strmv, ("cblas_strmv"), ("cblas_strmv_64"));
    FBLAS_CBLAS_LOAD(cblas_strsm, strsm, ("cblas_strsm"), ("cblas_strsm_64"));
    FBLAS_CBLAS_LOAD(cblas_strsv, strsv, ("cblas_strsv"), ("cblas_strsv_64"));
    FBLAS_CBLAS_LOAD(cblas_zaxpby, zaxpby, ("cblas_zaxpby"), ("cblas_zaxpby_64"));
    FBLAS_CBLAS_LOAD(cblas_zaxpy, zaxpy, ("cblas_zaxpy"), ("cblas_zaxpy_64"));
    FBLAS_CBLAS_LOAD(cblas_zcopy, zcopy, ("cblas_zcopy"), ("cblas_zcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_zdotc_sub, zdotc_sub, ("cblas_zdotc_sub"), ("cblas_zdotc_sub_64"));
    FBLAS_CBLAS_LOAD(cblas_zdotu_sub, zdotu_sub, ("cblas_zdotu_sub"), ("cblas_zdotu_sub_64"));
    FBLAS_CBLAS_LOAD(cblas_zdrot, zdrot, ("cblas_zdrot"), ("cblas_zdrot_64"));
    FBLAS_CBLAS_LOAD(cblas_zdscal, zdscal, ("cblas_zdscal"), ("cblas_zdscal_64"));
    FBLAS_CBLAS_LOAD(cblas_zgbmv, zgbmv, ("cblas_zgbmv"), ("cblas_zgbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_zgeadd, zgeadd, ("cblas_zgeadd"), ("cblas_zgeadd_64"));
    FBLAS_CBLAS_LOAD(cblas_zgemm, zgemm, ("cblas_zgemm"), ("cblas_zgemm_64"));
    FBLAS_CBLAS_LOAD(cblas_zgemmtr, zgemmtr, ("cblas_zgemmtr", "cblas_zgemmt"), ("cblas_zgemmtr_64", "cblas_zgemmt_64"));
    FBLAS_CBLAS_LOAD(cblas_zgemv, zgemv, ("cblas_zgemv"), ("cblas_zgemv_64"));
    FBLAS_CBLAS_LOAD(cblas_zgerc, zgerc, ("cblas_zgerc"), ("cblas_zgerc_64"));
    FBLAS_CBLAS_LOAD(cblas_zgeru, zgeru, ("cblas_zgeru"), ("cblas_zgeru_64"));
    FBLAS_CBLAS_LOAD(cblas_zhbmv, zhbmv, ("cblas_zhbmv"), ("cblas_zhbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_zhemm, zhemm, ("cblas_zhemm"), ("cblas_zhemm_64"));
    FBLAS_CBLAS_LOAD(cblas_zhemv, zhemv, ("cblas_zhemv"), ("cblas_zhemv_64"));
    FBLAS_CBLAS_LOAD(cblas_zher, zher, ("cblas_zher"), ("cblas_zher_64"));
    FBLAS_CBLAS_LOAD(cblas_zher2, zher2, ("cblas_zher2"), ("cblas_zher2_64"));
    FBLAS_CBLAS_LOAD(cblas_zher2k, zher2k, ("cblas_zher2k"), ("cblas_zher2k_64"));
    FBLAS_CBLAS_LOAD(cblas_zherk, zherk, ("cblas_zherk"), ("cblas_zherk_64"));
    FBLAS_CBLAS_LOAD(cblas_zhpmv, zhpmv, ("cblas_zhpmv"), ("cblas_zhpmv_64"));
    FBLAS_CBLAS_LOAD(cblas_zhpr, zhpr, ("cblas_zhpr"), ("cblas_zhpr_64"));
    FBLAS_CBLAS_LOAD(cblas_zhpr2, zhpr2, ("cblas_zhpr2"), ("cblas_zhpr2_64"));
    FBLAS_CBLAS_LOAD(cblas_zimatcopy, zimatcopy, ("cblas_zimatcopy"), ("cblas_zimatcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_zomatcopy, zomatcopy, ("cblas_zomatcopy"), ("cblas_zomatcopy_64"));
    FBLAS_CBLAS_LOAD(cblas_zrotg, zrotg, ("cblas_zrotg"), ("cblas_zrotg_64"));
    FBLAS_CBLAS_LOAD(cblas_zscal, zscal, ("cblas_zscal"), ("cblas_zscal_64"));
    FBLAS_CBLAS_LOAD(cblas_zswap, zswap, ("cblas_zswap"), ("cblas_zswap_64"));
    FBLAS_CBLAS_LOAD(cblas_zsymm, zsymm, ("cblas_zsymm"), ("cblas_zsymm_64"));
    FBLAS_CBLAS_LOAD(cblas_zsyr2k, zsyr2k, ("cblas_zsyr2k"), ("cblas_zsyr2k_64"));
    FBLAS_CBLAS_LOAD(cblas_zsyrk, zsyrk, ("cblas_zsyrk"), ("cblas_zsyrk_64"));
    FBLAS_CBLAS_LOAD(cblas_ztbmv, ztbmv, ("cblas_ztbmv"), ("cblas_ztbmv_64"));
    FBLAS_CBLAS_LOAD(cblas_ztbsv, ztbsv, ("cblas_ztbsv"), ("cblas_ztbsv_64"));
    FBLAS_CBLAS_LOAD(cblas_ztpmv, ztpmv, ("cblas_ztpmv"), ("cblas_ztpmv_64"));
    FBLAS_CBLAS_LOAD(cblas_ztpsv, ztpsv, ("cblas_ztpsv"), ("cblas_ztpsv_64"));
    FBLAS_CBLAS_LOAD(cblas_ztrmm, ztrmm, ("cblas_ztrmm"), ("cblas_ztrmm_64"));
    FBLAS_CBLAS_LOAD(cblas_ztrmv, ztrmv, ("cblas_ztrmv"), ("cblas_ztrmv_64"));
    FBLAS_CBLAS_LOAD(cblas_ztrsm, ztrsm, ("cblas_ztrsm"), ("cblas_ztrsm_64"));
    FBLAS_CBLAS_LOAD(cblas_ztrsv, ztrsv, ("cblas_ztrsv"), ("cblas_ztrsv_64"));

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


