/**
 * @file demo_xerbla.c
 * @brief Demonstrates the usage of XERBLA 
 * @author Martin Köhler
 * @version $Id$ 
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifndef Int
#ifndef INTEGER8 
#define Int 	int
#else 
#include <stdint.h>
#define Int 	int64_t
#endif
#endif

void dgemm_(char *, char *, Int *, Int *, Int *, double *, double *, Int *, double *, Int *, double *, double *, Int *); 


void xerbla_(char *name, Int *info, Int len){
	char * ptr = malloc ( sizeof(char) * (len + 1)); 
	strncpy(ptr, name, len); 
	ptr[len] = '\0'; 
	printf("name: %s\n", ptr);
	printf("info: %d\n", (int) *info);
	printf("len:  %d\n", (int) len);
	free(ptr); 
}

int main(int argc, const char *argv[])
{
	Int n,m,k; 
	Int lda, ldb, ldc; 
	double alpha, beta; 
	double A[10],B[10],C[10]; 

	m = 2; 
	n = 2; 
	k = 2; 
	lda = 1; 
	ldb = 2; 
	ldc = 0; 
	alpha = beta = 1; 
	// printf("xerbla_: %lx\n", (unsigned long)((void*)&xerbla_));  
	dgemm_("N", "N", &m, &n, &k, &alpha, A, &lda, B, &ldb, &beta, C, &ldc); 
	

	return 0;
}
