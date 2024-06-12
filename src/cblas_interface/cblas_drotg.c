//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2024 Martin Koehler

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with this program. If not, see <https://www.gnu.org/licenses/>.
   */





#include "cblas.h"
#include "cblas_f77.h"
#include "../flexiblas.h"
#include "cblas_flexiblas.h"

static TLS_STORE uint8_t hook_cblas_drotg_pos = 0;

void cblas_drotg(  double *a, double *b, double *c, double *s)
{
    void (*fn)(double *, double *, double *, double *);

    CBLAS_BACKEND_INIT();
    CBLAS_HOOK_SELECT(drotg);
    fn(a,b,c,s);

}

void flexiblas_chain_cblas_drotg(  double *a, double *b, double *c, double *s)
{
    void (*fn)(double *, double *, double *, double *);
    CBLAS_HOOK_ADVANCE(drotg);
    fn(a,b,c,s);

}

void flexiblas_real_cblas_drotg(  double *a, double *b, double *c, double *s)
{
    if ( current_backend->blas.drotg.cblas_function != NULL ) {
        void (*fn)(double *, double *, double *, double *);
        *(void **) & fn = current_backend->blas.drotg.cblas_function;
        fn(a,b,c,s);
    } else {
        FC_GLOBAL(drotg,DROTG)(a,b,c,s);
    }
    return;
}

