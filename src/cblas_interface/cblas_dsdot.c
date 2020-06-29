

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

static TLS_STORE uint8_t hook_cblas_dsdot_pos = 0;


double cblas_dsdot( const int N, const float *X, const int incX, const float *Y, const int incY)
{
    float d;
    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }

    if ( current_backend->blas.dsdot.cblas_function != NULL ) {
        double (*fn)  ( const int N, const float *X, const int incX, const float *Y, const int incY);
        if ( __flexiblas_hooks->dsdot.cblas_hook_function[0] != NULL) {
            fn  = __flexiblas_hooks->dsdot.cblas_hook_function[0];
            hook_cblas_dsdot_pos = 0;
        } else {
            fn = current_backend->blas.dsdot.cblas_function;
        }
        d = fn(N,X,incX,Y,incY);
    } else {
        Int F77_N=N, F77_incX=incX, F77_incY=incY;
        d = FC_GLOBAL(dsdot,DSDOT)( &F77_N, X, &F77_incX, Y, &F77_incY);
    }
    return d;
}

double flexiblas_real_cblas_dsdot( const int N, const float *X, const int incX, const float *Y, const int incY)
{
    double (*fn)  ( const int N, const float *X, const int incX, const float *Y, const int incY);
    fn = current_backend->blas.dsdot.cblas_function;
    return fn(N,X,incX,Y,incY);
}

double flexiblas_chain_cblas_dsdot( const int N, const float *X, const int incX, const float *Y, const int incY)
{
    double (*fn)  ( const int N, const float *X, const int incX, const float *Y, const int incY);
    hook_cblas_dsdot_pos ++;
    if ( hook_cblas_dsdot_pos < __flexiblas_hooks->dsdot.cblas_nhook) {
        fn  = __flexiblas_hooks->dsdot.cblas_hook_function[hook_cblas_dsdot_pos];
    } else {
        hook_cblas_dsdot_pos = 0;
        fn = current_backend->blas.dsdot.cblas_function;
    }
    return fn(N,X,incX,Y,incY);
}


