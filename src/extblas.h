/* $Id$ */
/* 
 Copyright (C) 2014  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

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

#ifndef EXTBLAS_H
#define EXTBLAS_H


/* AXPBY Operation  */
void fcaxpby_(const Int *N, const float complex *CA, const float complex *CX, const Int *INCX, const float complex *CB, float complex *CY, const Int *INCY); 
void fzaxpby_(const Int *N, const double complex *CA, const double complex *CX, const Int *INCX, const double complex *CB, double complex *CY, const Int *INCY); 
void fsaxpby_(const Int *N, const float *CA, const float *CX, const Int *INCX, const float *CB, float *CY, const Int *INCY); 
void fdaxpby_(const Int *N, const double *CA, const double *CX, const Int *INCX, const double *CB, double *CY, const Int *INCY); 

/* OMATCOPY  */
void fsomatcopy_( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const float *alpha, const float *a, const Int *lda, float *b, const Int *ldb); 
void fdomatcopy_( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const double *alpha, const double *a, const Int *lda, double *b, const Int *ldb); 
void fcomatcopy_( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const float complex *alpha, const float complex *a, const Int *lda, float complex *b, const Int *ldb); 
void fzomatcopy_( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const double complex *alpha, const double complex *a, const Int *lda, double complex *b, const Int *ldb); 

/* IMATCOPY  */
void fsimatcopy_( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const float *alpha, const float *a, const Int *lda, const Int *ldb); 
void fdimatcopy_( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const double *alpha, const double *a, const Int *lda, const Int *ldb); 
void fcimatcopy_( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const float complex *alpha, const float complex *a, const Int *lda, const Int *ldb); 
void fzimatcopy_( char* ORDER, char* TRANS, const Int *rows, const Int *cols, const double complex *alpha, const double complex *a, const Int *lda, const Int *ldb); 



#endif /* end of include guard: EXTBLAS_H */
