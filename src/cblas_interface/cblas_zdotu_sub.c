
/*
   Copyright (C) 2013  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
   */

#include "flexiblas_config.h"
#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include <complex.h>

static TLS_STORE uint8_t hook_cblas_zdotu_sub_pos = 0;


void cblas_zdotu_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc)
{
    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    if ( current_backend->blas.zdotu_sub.cblas_function != NULL ) {
        void (*fn)  ( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
        if ( __flexiblas_hooks->zdotu_sub.cblas_hook_function[0] != NULL) {
            fn  = __flexiblas_hooks->zdotu_sub.cblas_hook_function[0];
            hook_cblas_zdotu_sub_pos = 0;
        } else {
            fn = current_backend->blas.zdotu_sub.cblas_function;
        }
        fn(N,X,incX,Y,incY,dotc);

    } else {
        double complex d;
        Int F77_N=N, F77_incX=incX, F77_incY=incY;
#ifdef FLEXIBLAS_ABI_INTEL
        FC_GLOBAL(zdotu,ZDOTU)( &d, &F77_N, X, &F77_incX, Y, &F77_incY);
#else
        d = FC_GLOBAL(zdotu,ZDOTU)( &F77_N, X, &F77_incX, Y, &F77_incY);
#endif
        *((double complex *) dotc) = d;
    }
}

void flexiblas_real_cblas_zdotu_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc)
{
    void (*fn)  ( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
    fn = current_backend->blas.zdotu_sub.cblas_function;
    fn(N,X,incX,Y,incY,dotc);
}

void flexiblas_chain_cblas_zdotu_sub( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc)
{
    void (*fn)  ( const int N, const void *X, const int incX, const void *Y, const int incY,void *dotc);
    hook_cblas_zdotu_sub_pos ++;
    if ( hook_cblas_zdotu_sub_pos < __flexiblas_hooks->zdotu_sub.cblas_nhook) {
        fn  = __flexiblas_hooks->zdotu_sub.cblas_hook_function[hook_cblas_zdotu_sub_pos];
    } else {
        hook_cblas_zdotu_sub_pos = 0;
        fn = current_backend->blas.zdotu_sub.cblas_function;
    }

    fn(N,X,incX,Y,incY,dotc);
}


