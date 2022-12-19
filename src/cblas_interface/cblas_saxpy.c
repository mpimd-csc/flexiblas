/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a
 * combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
 * General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2022
 */

#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_saxpy_pos = 0;

void cblas_saxpy( const CBLAS_INT N, const float alpha, const float *X,
        const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{

    void (*fn)  ( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY);
    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(saxpy);

    fn(N,alpha,X,incX,Y,incY);
}


void flexiblas_real_cblas_saxpy( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    if ( current_backend->blas.saxpy.cblas_function != NULL ) {
        void (*fn)  ( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY);
        *(void **) &fn = current_backend->blas.saxpy.cblas_function;
        fn(N,alpha,X,incX,Y,incY);
    } else {
        Int F77_N=N, F77_incX=incX, F77_incY=incY;
        FC_GLOBAL(saxpy,SAXPY)( &F77_N, &alpha, X, &F77_incX, Y, &F77_incY);
    }
}

void flexiblas_chain_cblas_saxpy( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
    void (*fn)  ( const CBLAS_INT N, const float alpha, const float *X, const CBLAS_INT incX, float *Y, const CBLAS_INT incY);
    CBLAS_HOOK_ADVANCE(saxpy);
    fn(N,alpha,X,incX,Y,incY);
}



