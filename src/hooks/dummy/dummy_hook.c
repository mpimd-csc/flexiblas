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

#include "cscutils/table.h"


#include "flexiblas.h"
#include "flexiblas_mgmt.h"
#include "flexiblas_backend.h"
#include "flexiblas_real_calls.h"

FLEXIBLAS_HOOK_OPTIONS(
        FLEXIBLAS_HOOK_OPTION("debug", "Enable Debug Output", FLEXIBLAS_OPTIONS_INT, "0"),
        FLEXIBLAS_HOOK_OPTION("debug-float", "Enable Debug with Float", FLEXIBLAS_OPTIONS_FLOAT, "0"),
        FLEXIBLAS_HOOK_OPTION("debug-string", "Enable Debug with Float", FLEXIBLAS_OPTIONS_STRING, "0"),
        FLEXIBLAS_HOOK_OPTIONS_END
        )


FLEXIBLAS_HOOK_REGISTER(
        "DUMMY",  // Name of the Hook
        "DUMMY",  // Name if the section in the config
        dummy,  // Namespace for option getters
        "This is a Dummy Hook \n"
        "On Multiline comment.", // Description
        "Martin Koehler")

FLEXIBLAS_HOOK_INIT_FUNCTION (void) {
    fprintf(stderr, "Dummy Init.\n");
    int dgb = FLEXIBLAS_HOOK_GET_OPTION_INT(dummy, "debug");
    printf("Debug Setting: %d\n", dgb);
    double dgbd = FLEXIBLAS_HOOK_GET_OPTION_FLOAT(dummy, "debug-float");
    printf("Debug Setting (float): %g\n", dgbd);
    char *v = FLEXIBLAS_HOOK_GET_OPTION_STRING(dummy, "debug-string");
    printf("Debug Setting (string): %s\n", v);


}


FLEXIBLAS_HOOK_EXIT_FUNCTION (void) {
    fprintf(stderr, "Dummy Exit\n");
}

double hook_dasum(Int *N, double *A, Int *INCX)
{
    Int k;
    Int incx = *INCX;
    Int n = *N;
    double * aptr = A;
    double ret;

    printf("dasum called with: N = %d, INCX = %d\n", (int)n, (int) incx);
    printf("x = [ \n");
    for (k = 0; k < n; k++) {
        printf("    %lg \n", *aptr);
        aptr += incx;
    }
    printf("]\n");

    ret = flexiblas_chain_dasum(N, A, INCX);
    printf("... returns %lg\n", ret);
    return ret;
}


static inline void d2s(Int n, double *dptr, Int dinc, float *sptr, Int sinc)
{
    Int k;
    double dtmp;
    for (k = 0; k < n; k++) {
        dtmp = *dptr;
        *sptr = (float) dtmp;
        dptr += dinc;
        sptr += sinc;
    }
}

static inline void s2d(Int n, float *sptr, Int sinc, double *dptr, Int dinc)
{
    Int k;
    float stmp;
    sptr += ((n-1) * sinc);
    dptr += ((n-1) * dinc);
    for (k = 0; k < n; k++) {
        stmp = *sptr;
        *dptr = (double) stmp;
        dptr -= dinc;
        sptr -= sinc;
    }
}




extern void FC_GLOBAL(saxpby,SAXPBY)(Int *_N, float *alpha, float *_X, Int *_INCX, float *_beta, float *Y, Int *_INCY);

void hook_daxpby(Int *_N, double *alpha, double *_X, Int *_INCX, double *beta, double *_Y, Int *_INCY)
{
    Int n = *_N;
    Int incx = *_INCX;
    Int incy = *_INCY;
    Int sincx = incx;
    Int sincy = incy;
    float *sx = (float *) _X;
    float *sy = (float *) _Y;

    float salpha = (float) (*alpha);
    float sbeta  = (float) (*beta);

    /* Convert to Single Precision */
    d2s(n, _X, incx, sx, sincx);
    d2s(n, _Y, incy, sy, sincy);

    /* Call BLAS  */
    FC_GLOBAL(saxpby,SAXPBY)(&n, &salpha, sx, &sincx, &sbeta, sy, &sincy);

    /* Move data back */
    s2d(n, sx, sincx, _X, incx);
    s2d(n, sy, sincy, _Y, incy);
}



