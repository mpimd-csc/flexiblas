/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2016
 */




#ifndef EXTBLAS_H
#define EXTBLAS_H

#include "fortran_mangle.h"
#ifdef __cplusplus
extern "C" { 
#endif
	
/* AXPBY Operation  */
void FC_GLOBAL(fcaxpby,FCAXPBY)(const Int *N, const float complex *CA, const float complex *CX, const Int *INCX, const float complex *CB, float complex *CY, const Int *INCY); 
void FC_GLOBAL(fzaxpby,FZAXPBY)(const Int *N, const double complex *CA, const double complex *CX, const Int *INCX, const double complex *CB, double complex *CY, const Int *INCY); 
void FC_GLOBAL(fsaxpby,FSAXPBY)(const Int *N, const float *CA, const float *CX, const Int *INCX, const float *CB, float *CY, const Int *INCY); 
void FC_GLOBAL(fdaxpby,FDAXPBY)(const Int *N, const double *CA, const double *CX, const Int *INCX, const double *CB, double *CY, const Int *INCY); 


/* OMATCOPY  */
void FC_GLOBAL(fsomatcopy,FSOMATCOPY)( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const float *alpha, const float *a, const Int *lda, float *b, const Int *ldb); 
void FC_GLOBAL(fdomatcopy,FDOMATCOPY)( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const double *alpha, const double *a, const Int *lda, double *b, const Int *ldb); 
void FC_GLOBAL(fcomatcopy,FCOMATCOPY)( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const float complex *alpha, const float complex *a, const Int *lda, float complex *b, const Int *ldb); 
void FC_GLOBAL(fzomatcopy,FZOMATCOPY)( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const double complex *alpha, const double complex *a, const Int *lda, double complex *b, const Int *ldb); 


/* IMATCOPY  */
void FC_GLOBAL(fsimatcopy,FSIMATCOPY)( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const float *alpha, const float *a, const Int *lda, const Int *ldb); 
void FC_GLOBAL(fdimatcopy,FDIMATCOPY)( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const double *alpha, const double *a, const Int *lda, const Int *ldb); 
void FC_GLOBAL(fcimatcopy,FCIMATCOPY)( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const float complex *alpha, const float complex *a, const Int *lda, const Int *ldb); 
void FC_GLOBAL(fzimatcopy,FZIMATCOPY)( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const double complex *alpha, const double complex *a, const Int *lda, const Int *ldb); 


/* GEADD  */
void FC_GLOBAL(fsgeadd,FSGEADD)(Int* m, Int* n, float* alpha, float* a, Int* lda, float* beta, float* b, Int* ldb);
void FC_GLOBAL(fcgeadd,FCGEADD)(Int* m, Int* n, float complex * alpha, float complex * a, Int* lda, float complex* beta, float complex* b, Int* ldb);
void FC_GLOBAL(fdgeadd,FDGEADD)(Int* m, Int* n, double* alpha, double* a, Int* lda, double* beta, double* b, Int* ldb);
void FC_GLOBAL(fzgeadd,FZGEADD)(Int* m, Int* n, double complex * alpha, double complex* a, Int* lda, double complex* beta, double complex* b, Int* ldb);

#ifdef __cplusplus
}; 
#endif 


#endif /* end of include guard: EXTBLAS_H */

