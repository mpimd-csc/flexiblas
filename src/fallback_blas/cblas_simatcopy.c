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

#include "cblas.h"
#include "cblas_f77.h"
#include "cblas_f77_ext.h"



void cblas_simatcopy(const CBLAS_ORDER CORDER, const CBLAS_TRANSPOSE CTRANS,
        const CBLAS_INT crows, const CBLAS_INT ccols, const float calpha, float *a, const CBLAS_INT clda,
        const CBLAS_INT cldb)
{
#ifdef F77_INT
    F77_INT F77_ROWS=crows;
    F77_INT F77_COLS=ccols;
    F77_INT F77_LDA =clda;
    F77_INT F77_LDB =cldb;
#else
#define F77_ROWS crows
#define F77_COLS ccols
#define F77_LDA  clda
#define F77_LDB  cldb
#endif
    char ORDER[2]=" ";
    char TRANS[2]=" ";
    switch(CORDER){
        case CblasColMajor:
            ORDER[0]='C';
            break;
        case CblasRowMajor:
            ORDER[0]='R';
            break;
        default:
            ORDER[0]='X';
    }
    switch(CTRANS){
        case CblasNoTrans:
        case CblasConjNoTrans:
            TRANS[0]='N';
            break;
        case CblasTrans:
        case CblasConjTrans:
            TRANS[0]='T';
            break;
        default:
            TRANS[0]='X';
    }
    F77_simatcopy( ORDER, TRANS, &F77_ROWS, &F77_COLS,  &calpha, a, &F77_LDA, &F77_LDB);
}

