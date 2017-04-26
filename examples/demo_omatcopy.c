/* $Id$ */
/**
 * @file demo_omatcopy.c
 * @brief Demonstrate OMATCOPY
 * @version $Id$ 
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
#include "f77blas_interface.h"
#ifdef FLEXIBLAS_CBLAS
#include "cblas.h"
#endif


void somatcopy_( char* ORDER, char* TRANS, Int *rows, Int *cols, float *alpha, float *a, Int *lda, float *b, Int *ldb); 
void domatcopy_( char* ORDER, char* TRANS, Int *rows, Int *cols, double *alpha, double *a, Int *lda, double *b, Int *ldb); 
void comatcopy_( char* ORDER, char* TRANS, Int *rows, Int *cols, float complex *alpha, float complex *a, Int *lda, float complex *b, Int *ldb); 
void zomatcopy_( char* ORDER, char* TRANS, Int *rows, Int *cols, double complex *alpha, double complex *a, Int *lda, double complex *b, Int *ldb); 

#define SWAP_INT(X,Y) {int _x = (X); (X) = (Y); (Y) = _x; } 

int main(int argc, const char *argv[])
{
	{
		float  col_A[8] = {1,2,3,4,5,6,7,8}; 
		float  col_B[16]; 
		float  alpha = 2; 
		Int rows, cols, lda, ldb, i,j; 
		Int rowst, colst; 
		rows = lda = 4; 
		cols = 2; 
		ldb = 4; 
		rowst = 2; 
		colst = 4; 

		printf("Single\n");
		/* OMATCOPY_2N */
		somatcopy_("C", "N", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2N\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2R */
		somatcopy_("C", "R", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2R\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2T */
		somatcopy_("C", "T", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2T\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2C */
		somatcopy_("C", "C", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2C\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}


	}

	{
		double   col_A[8] = {1,2,3,4,5,6,7,8}; 
		double   col_B[16]; 
		double   alpha = 2; 
		Int rows, cols, lda, ldb, i,j; 
		Int rowst, colst; 
		rows = lda = 4; 
		cols = 2; 
		ldb = 4; 
		rowst = 2; 
		colst = 4; 

		printf("Double\n");
		/* OMATCOPY_2N */
		domatcopy_("C", "N", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2N\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2R */
		domatcopy_("C", "R", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2R\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2T */
		domatcopy_("C", "T", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2T\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2C */
		domatcopy_("C", "C", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2C\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}


	}

	{
		double complex   col_A[8] = {1+I,2+I,3,4,5,6,7,8}; 
		double complex   col_B[16]; 
		double complex   alpha = 2+2*I; 
		Int rows, cols, lda, ldb, i,j; 
		Int rowst, colst; 
		rows = lda = 4; 
		cols = 2; 
		ldb = 4; 
		rowst = 2; 
		colst = 4; 

		printf("COMPLEX16\n");
		/* OMATCOPY_2N */
		zomatcopy_("C", "N", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2N\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2R */
		zomatcopy_("C", "R", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2R\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2T */
		zomatcopy_("C", "T", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2T\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2C */
		zomatcopy_("C", "C", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2C\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}


	}

	{
		float complex   col_A[8] = {1+I,2+I,3,4,5,6,7,8}; 
		float complex   col_B[16]; 
		float complex   alpha = 2+2*I; 
		Int rows, cols, lda, ldb, i,j; 
		Int rowst, colst; 
		rows = lda = 4; 
		cols = 2; 
		ldb = 4; 
		rowst = 2; 
		colst = 4; 

		printf("COMPLEX\n");
		/* OMATCOPY_2N */
		comatcopy_("C", "N", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2N\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2R */
		comatcopy_("C", "R", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2R\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2T */
		comatcopy_("C", "T", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2T\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2C */
		comatcopy_("C", "C", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb); 
		printf("OMATCOPY_2C\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}


	}

#ifdef FLEXIBLAS_CBLAS
	printf("\n\n");
	printf("CBLAS\n");
	printf("\n\n");
	{
		float  col_A[8] = {1,2,3,4,5,6,7,8}; 
		float  col_B[16]; 
		float  alpha = 2; 
		int rows, cols, lda, ldb, i,j; 
		int rowst, colst; 
		rows = lda = 4; 
		cols = 2; 
		ldb = 4; 
		rowst = 2; 
		colst = 4; 

		printf("Single\n");
		/* OMATCOPY_2N */
		cblas_somatcopy(CblasColMajor, CblasNoTrans, rows, cols, alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2N\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2R */
		cblas_somatcopy(CblasColMajor, CblasConjNoTrans, rows, cols, alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2R\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2T */
		cblas_somatcopy(CblasColMajor, CblasTrans, rows, cols, alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2T\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2C */
		cblas_somatcopy(CblasColMajor, CblasConjTrans, rows, cols, alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2C\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}


	}
	{
		double   col_A[8] = {1,2,3,4,5,6,7,8}; 
		double   col_B[16]; 
		double   alpha = 2; 
		int rows, cols, lda, ldb, i,j; 
		int rowst, colst; 
		rows = lda = 4; 
		cols = 2; 
		ldb = 4; 
		rowst = 2; 
		colst = 4; 

		printf("Double\n");
		/* OMATCOPY_2N */
		cblas_domatcopy(CblasColMajor, CblasNoTrans, rows, cols, alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2N\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2R */
		cblas_domatcopy(CblasColMajor, CblasConjNoTrans, rows, cols, alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2R\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2T */
		cblas_domatcopy(CblasColMajor, CblasTrans, rows, cols, alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2T\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}
		/* OMATCOPY_2C */
		cblas_domatcopy(CblasColMajor, CblasConjTrans, rows, cols, alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2C\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g\t", col_B[i+j*ldb]);
			}
			printf("\n");
		}


	}
	{
		float complex   col_A[8] = {1+I,2+I,3,4,5,6,7,8}; 
		float complex   col_B[16]; 
		float complex   alpha = 2+2*I; 
		int rows, cols, lda, ldb, i,j; 
		int rowst, colst; 
		rows = lda = 4; 
		cols = 2; 
		ldb = 4; 
		rowst = 2; 
		colst = 4; 

		printf("COMPLEX8\n");
		/* OMATCOPY_2N */
		cblas_comatcopy(CblasColMajor, CblasNoTrans, rows, cols, &alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2N\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));

			}
			printf("\n");
		}
		/* OMATCOPY_2R */
		cblas_comatcopy(CblasColMajor, CblasConjNoTrans, rows, cols, &alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2R\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2T */
		cblas_comatcopy(CblasColMajor, CblasTrans, rows, cols, &alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2T\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2C */
		cblas_comatcopy(CblasColMajor, CblasConjTrans, rows, cols, &alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2C\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}


	}

	{
		double complex   col_A[8] = {1+I,2+I,3,4,5,6,7,8}; 
		double complex   col_B[16]; 
		double complex   alpha = 2+2*I; 
		int rows, cols, lda, ldb, i,j; 
		int rowst, colst; 
		rows = lda = 4; 
		cols = 2; 
		ldb = 4; 
		rowst = 2; 
		colst = 4; 

		printf("COMPLEX16\n");
		/* OMATCOPY_2N */
		cblas_zomatcopy(CblasColMajor, CblasNoTrans, rows, cols, &alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2N\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));

			}
			printf("\n");
		}
		/* OMATCOPY_2R */
		cblas_zomatcopy(CblasColMajor, CblasConjNoTrans, rows, cols, &alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2R\n");
		for (i = 0; i < rows; i++) {
			for (j = 0; j < cols; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2T */
		cblas_zomatcopy(CblasColMajor, CblasTrans, rows, cols, &alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2T\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}
		/* OMATCOPY_2C */
		cblas_zomatcopy(CblasColMajor, CblasConjTrans, rows, cols, &alpha, col_A, lda, col_B, ldb); 
		printf("OMATCOPY_2C\n");
		for (i = 0; i < rowst; i++) {
			for (j = 0; j < colst; j++) {
				printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
			}
			printf("\n");
		}


	}




#endif

	return 0;
}
