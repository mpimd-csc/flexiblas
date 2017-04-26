#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>
#include <stdint.h>

void xerbla_(char *str, int *info, int len); 
static void xerbla64_(char *str, int64_t *info, int len){
	int in = *info; 
	xerbla_(str, &in, len); 
}


#define FNAME fcomatcopy64_
#define ENAME "COMATCOPY"
#define FLOAT float complex

#include "zomatcopy_kernel.c"

void fcomatcopy32_(char* ORDER, char* TRANS, int32_t *rows, int32_t *cols, FLOAT *alpha, FLOAT *a, int32_t *lda, FLOAT *b, int32_t *ldb)
{
	int64_t _rows  = *rows; 
	int64_t _cols  = *cols; 
	int64_t _lda  = *lda; 
	int64_t _ldb  = *ldb; 
	fcomatcopy64_(ORDER, TRANS, &_rows, &_cols, alpha, a, &_lda, b, &_ldb); 

}


