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
 * Copyright (C) Martin Koehler, 2013-2015
 */

#include <stdio.h>
#include <stdlib.h> 
#include <stdint.h>
#include <complex.h>
#include <dlfcn.h>
#include "flexiblas_backend.h" 

/* Necessary to tell FlexiBLAS that this backend needs to be loaded using RTLD_GLOBAL|RTLD_LOCAL */
#ifdef MKL_CUSTOM 
FLEXIBLAS_LAZY_BINDING; 
#else 
FLEXIBLAS_GLOBAL_BINDING; 
FLEXIBLAS_DEEP_BINDING;
FLEXIBLAS_LAZY_BINDING; 
#endif 


/*-----------------------------------------------------------------------------
 * Info function, called once before  FlexiBLAS initializes the back end 
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_INFO_FUNCTION(info) {
/* The back end should use the post init mode. Important for CUDA */  
	info->post_init = 0; 
/* Specify the integer width  */
#ifdef  BACKEND_INTEGER8
	info -> backend_integer_size = 8; 
#else 
	info -> backend_integer_size = sizeof(int); 
#endif 

/* Specify that the interface is intel compatible */
#ifdef ZDOTC_MKL 
	info -> intel_interface = 1; 
#else 
	info -> intel_interface = 0; 
#endif 
}



/*-----------------------------------------------------------------------------
 *  Init function, called once when FlexiBLAS initializes the backend. 
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_INIT_FUNCTION() {
	/* Return 0 on success, != 0 otherwise   */
	return 0 ; 
}



/*-----------------------------------------------------------------------------
 *  Exit function, called once when the program finishes. 
 *-----------------------------------------------------------------------------*/
FLEXIBLAS_EXIT_FUNCTION() {
	return; 
}


/*-----------------------------------------------------------------------------
 * Calling Sequence Adjustment for MKL 
 *-----------------------------------------------------------------------------*/
#if defined(EXTBLAS_ENABLED)
void mkl_domatcopy_(void *, void *, void *, void *, void *, void *, void *, void *, void *); 
void mkl_somatcopy_(void *, void *, void *, void *, void *, void *, void *, void *, void *); 
void mkl_comatcopy_(void *, void *, void *, void *, void *, void *, void *, void *, void *); 
void mkl_zomatcopy_(void *, void *, void *, void *, void *, void *, void *, void *, void *); 

void mkl_dimatcopy_(void *, void *, void *, void *, void *, void *, void *, void *); 
void mkl_simatcopy_(void *, void *, void *, void *, void *, void *, void *, void *); 
void mkl_cimatcopy_(void *, void *, void *, void *, void *, void *, void *, void *); 
void mkl_zimatcopy_(void *, void *, void *, void *, void *, void *, void *, void *); 

void domatcopy_( char* ORDER, char* TRANS, int *rows, int *cols, double *alpha, double *A, int *lda, double *B, int *ldb){
	mkl_domatcopy_(ORDER, TRANS, rows, cols, alpha, A, lda, B, ldb); 
}
void somatcopy_( char* ORDER, char* TRANS, int *rows, int *cols, float *alpha, float *A, int *lda, float *B, int *ldb){
	mkl_somatcopy_(ORDER, TRANS, rows, cols, alpha, A, lda, B, ldb); 
}
void comatcopy_( char* ORDER, char* TRANS, int *rows, int *cols,  float complex *alpha,  float complex *A, int *lda,  float complex *B, int *ldb){
	mkl_comatcopy_(ORDER, TRANS, rows, cols, alpha, A, lda, B, ldb); 
}
void zomatcopy_( char* ORDER, char* TRANS, int *rows, int *cols, double complex *alpha, double complex *A, int *lda, double complex *B, int *ldb){
	mkl_zomatcopy_(ORDER, TRANS, rows, cols, alpha, A, lda, B, ldb); 
}

void dimatcopy_( char* ORDER, char* TRANS, int *rows, int *cols, double *alpha, double *A, int *lda, int *ldb){
	mkl_dimatcopy_(ORDER, TRANS, rows, cols, alpha, A, lda, ldb); 
}
void simatcopy_( char* ORDER, char* TRANS, int *rows, int *cols, float *alpha, float *A, int *lda, int *ldb){
	mkl_simatcopy_(ORDER, TRANS, rows, cols, alpha, A, lda, ldb); 
}
void cimatcopy_( char* ORDER, char* TRANS, int *rows, int *cols,  float complex *alpha,  float complex *A, int *lda,  int *ldb){
	mkl_cimatcopy_(ORDER, TRANS, rows, cols, alpha, A, lda,ldb); 
}
void zimatcopy_( char* ORDER, char* TRANS, int *rows, int *cols, double complex *alpha, double complex *A, int *lda, int *ldb){
	mkl_zimatcopy_(ORDER, TRANS, rows, cols, alpha, A, lda,ldb); 
}


#endif


/*-----------------------------------------------------------------------------
 *  Include the remaining dumming functions to cheat LD 
 *-----------------------------------------------------------------------------*/
#include "flexiblas_dummy_fortran.h"
#ifdef CBLAS_INTERFACE
#include "flexiblas_dummy_cblas.h"
#endif
