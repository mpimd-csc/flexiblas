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





#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
#include "flexiblas_fortran_mangle.h"

#ifdef INTEGER8
#define USE_BLAS_64

#define Int int64_t
#else
#define Int int
#endif



#ifdef BLAS_INTERFACE_INTEL
#include "blas_intel.h"
#include "extblas_intel.h"
#else
#include "blas_gnu.h"
#include "extblas_gnu.h"
#endif
#ifdef FLEXIBLAS_CBLAS
#include "cblas.h"
#endif



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
        FC_GLOBAL(somatcopy,SOMATCOPY)("C", "N", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_B[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        FC_GLOBAL(somatcopy,SOMATCOPY)("C", "R", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_B[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        FC_GLOBAL(somatcopy,SOMATCOPY)("C", "T", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_B[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        FC_GLOBAL(somatcopy,SOMATCOPY)("C", "C", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
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
        FC_GLOBAL(domatcopy,DOMATCOPY)("C", "N", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_B[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        FC_GLOBAL(domatcopy,DOMATCOPY)("C", "R", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_B[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        FC_GLOBAL(domatcopy,DOMATCOPY)("C", "T", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_B[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        FC_GLOBAL(domatcopy,DOMATCOPY)("C", "C", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
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
        FC_GLOBAL(zomatcopy,ZOMATCOPY)("C", "N", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        FC_GLOBAL(zomatcopy,ZOMATCOPY)("C", "R", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        FC_GLOBAL(zomatcopy,ZOMATCOPY)("C", "T", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        FC_GLOBAL(zomatcopy,ZOMATCOPY)("C", "C", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
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
        FC_GLOBAL(comatcopy,COMATCOPY)("C", "N", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        FC_GLOBAL(comatcopy,COMATCOPY)("C", "R", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        FC_GLOBAL(comatcopy,COMATCOPY)("C", "T", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4gI\t", creal(col_B[i+j*ldb]), cimag(col_B[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        FC_GLOBAL(comatcopy,COMATCOPY)("C", "C", &rows, &cols, &alpha, col_A, &lda, col_B, &ldb, 1, 1);
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
