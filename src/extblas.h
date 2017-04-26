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
void fcaxpby32_(const int32_t *N, const float complex *CA, const float complex *CX, const int32_t *INCX, const float complex *CB, float complex *CY, const int32_t *INCY); 
void fzaxpby32_(const int32_t *N, const double complex *CA, const double complex *CX, const int32_t *INCX, const double complex *CB, double complex *CY, const int32_t *INCY); 
void fsaxpby32_(const int32_t *N, const float *CA, const float *CX, const int32_t *INCX, const float *CB, float *CY, const int32_t *INCY); 
void fdaxpby32_(const int32_t *N, const double *CA, const double *CX, const int32_t *INCX, const double *CB, double *CY, const int32_t *INCY); 

void fcaxpby64_(const int64_t *N, const float complex *CA, const float complex *CX, const int64_t *INCX, const float complex *CB, float complex *CY, const int64_t *INCY); 
void fzaxpby64_(const int64_t *N, const double complex *CA, const double complex *CX, const int64_t *INCX, const double complex *CB, double complex *CY, const int64_t *INCY); 
void fsaxpby64_(const int64_t *N, const float *CA, const float *CX, const int64_t *INCX, const float *CB, float *CY, const int64_t *INCY); 
void fdaxpby64_(const int64_t *N, const double *CA, const double *CX, const int64_t *INCX, const double *CB, double *CY, const int64_t *INCY); 

/* OMATCOPY  */
void fsomatcopy32_( char* ORDER, char* TRANS, const int32_t *rows, const int32_t *cols, const float *alpha, const float *a, const int32_t *lda, float *b, const int32_t *ldb); 
void fdomatcopy32_( char* ORDER, char* TRANS, const int32_t *rows, const int32_t *cols, const double *alpha, const double *a, const int32_t *lda, double *b, const int32_t *ldb); 
void fcomatcopy32_( char* ORDER, char* TRANS, const int32_t *rows, const int32_t *cols, const float complex *alpha, const float complex *a, const int32_t *lda, float complex *b, const int32_t *ldb); 
void fzomatcopy32_( char* ORDER, char* TRANS, const int32_t *rows, const int32_t *cols, const double complex *alpha, const double complex *a, const int32_t *lda, double complex *b, const int32_t *ldb); 

void fsomatcopy64_( char* ORDER, char* TRANS, const int64_t *rows, const int64_t *cols, const float *alpha, const float *a, const int64_t *lda, float *b, const int64_t *ldb); 
void fdomatcopy64_( char* ORDER, char* TRANS, const int64_t *rows, const int64_t *cols, const double *alpha, const double *a, const int64_t *lda, double *b, const int64_t *ldb); 
void fcomatcopy64_( char* ORDER, char* TRANS, const int64_t *rows, const int64_t *cols, const float complex *alpha, const float complex *a, const int64_t *lda, float complex *b, const int64_t *ldb); 
void fzomatcopy64_( char* ORDER, char* TRANS, const int64_t *rows, const int64_t *cols, const double complex *alpha, const double complex *a, const int64_t *lda, double complex *b, const int64_t *ldb); 

/* IMATCOPY  */
void fsimatcopy32_( char* ORDER, char* TRANS, const int32_t *rows, const int32_t *cols, const float *alpha, const float *a, const int32_t *lda, const int32_t *ldb); 
void fdimatcopy32_( char* ORDER, char* TRANS, const int32_t *rows, const int32_t *cols, const double *alpha, const double *a, const int32_t *lda, const int32_t *ldb); 
void fcimatcopy32_( char* ORDER, char* TRANS, const int32_t *rows, const int32_t *cols, const float complex *alpha, const float complex *a, const int32_t *lda, const int32_t *ldb); 
void fzimatcopy32_( char* ORDER, char* TRANS, const int32_t *rows, const int32_t *cols, const double complex *alpha, const double complex *a, const int32_t *lda, const int32_t *ldb); 

void fsimatcopy64_( char* ORDER, char* TRANS, const int64_t *rows, const int64_t *cols, const float *alpha, const float *a, const int64_t *lda, const int64_t *ldb); 
void fdimatcopy64_( char* ORDER, char* TRANS, const int64_t *rows, const int64_t *cols, const double *alpha, const double *a, const int64_t *lda, const int64_t *ldb); 
void fcimatcopy64_( char* ORDER, char* TRANS, const int64_t *rows, const int64_t *cols, const float complex *alpha, const float complex *a, const int64_t *lda, const int64_t *ldb); 
void fzimatcopy64_( char* ORDER, char* TRANS, const int64_t *rows, const int64_t *cols, const double complex *alpha, const double complex *a, const int64_t *lda, const int64_t *ldb); 

/* GEADD  */
void fsgeadd32_(int32_t* m, int32_t* n, float* alpha, float* a, int32_t* lda, float* beta, float* b, int32_t* ldb);
void fsgeadd64_(int64_t* m, int64_t* n, float* alpha, float* a, int64_t* lda, float* beta, float* b, int64_t* ldb);
void fdgeadd32_(int32_t* m, int32_t* n, double* alpha, double* a, int32_t* lda, double* beta, double* b, int32_t* ldb);
void fdgeadd64_(int64_t* m, int64_t* n, double* alpha, double* a, int64_t* lda, double* beta, double* b, int64_t* ldb);
void fcgeadd32_(int32_t* m, int32_t* n, float complex* alpha, float complex* a, int32_t* lda, float complex* beta, float complex* b, int32_t* ldb);
void fcgeadd64_(int64_t* m, int64_t* n, float complex* alpha, float complex* a, int64_t* lda, float complex* beta, float complex* b, int64_t* ldb);
void fzgeadd32_(int32_t* m, int32_t* n, double complex* alpha, double complex* a, int32_t* lda, double complex* beta, double complex* b, int32_t* ldb);
void fzgeadd64_(int64_t* m, int64_t* n, double complex* alpha, double complex* a, int64_t* lda, double complex* beta, double complex* b, int64_t* ldb);



#endif /* end of include guard: EXTBLAS_H */

