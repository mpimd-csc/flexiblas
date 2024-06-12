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
#define Int int64_t
#define USE_BLAS_64
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
    /* Square matrices  */
    {
        float  col_A[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
        float  alpha = 2;
        Int rows, cols, lda, ldb, i,j;
        Int rowst, colst;
        rows = lda = 4;
        cols = 4;
        ldb = 4;
        rowst = 4;
        colst = 4;

        printf("Single\n");
        /* OMATCOPY_2N */
        FC_GLOBAL(simatcopy,SIMATCOPY)("C", "N", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("IMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        FC_GLOBAL(simatcopy,SIMATCOPY)("C", "R", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("IMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        FC_GLOBAL(simatcopy,SIMATCOPY)("C", "T", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        FC_GLOBAL(simatcopy,SIMATCOPY)("C", "C", &rows, &cols, &alpha, col_A, &lda,&ldb, 1, 1);
        printf("OMATCOPY_2C\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }


    }

    {
        double   col_A[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
        double   alpha = 2;
        Int rows, cols, lda, ldb, i,j;
        Int rowst, colst;
        rows = lda = 4;
        cols = 4;
        ldb = 4;
        rowst = 4;
        colst = 4;

        printf("Double\n");
        /* OMATCOPY_2N */
        FC_GLOBAL(dimatcopy,DIMATCOPY)("C", "N", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        FC_GLOBAL(dimatcopy,DIMATCOPY)("C", "R", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        FC_GLOBAL(dimatcopy,DIMATCOPY)("C", "T", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        FC_GLOBAL(dimatcopy,DIMATCOPY)("C", "C", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2C\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }


    }

    /* Square matrices  */
    {
        float complex  col_A[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
        float complex  alpha = 2+I;
        Int rows, cols, lda, ldb, i,j;
        Int rowst, colst;
        rows = lda = 4;
        cols = 4;
        ldb = 4;
        rowst = 4;
        colst = 4;

        printf("Single\n");
        /* OMATCOPY_2N */
        FC_GLOBAL(cimatcopy,CIMATCOPY)("C", "N", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("IMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        FC_GLOBAL(cimatcopy,CIMATCOPY)("C", "R", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("IMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        FC_GLOBAL(cimatcopy,CIMATCOPY)("C", "T", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        FC_GLOBAL(cimatcopy,CIMATCOPY)("C", "C", &rows, &cols, &alpha, col_A, &lda,&ldb, 1, 1);
        printf("OMATCOPY_2C\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }


    }

    {
        double complex  col_A[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
        double complex  alpha = 2+I;
        Int rows, cols, lda, ldb, i,j;
        Int rowst, colst;
        rows = lda = 4;
        cols = 4;
        ldb = 4;
        rowst = 4;
        colst = 4;

        printf("Double\n");
        /* OMATCOPY_2N */
        FC_GLOBAL(zimatcopy,ZIMATCOPY)("C", "N", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        FC_GLOBAL(zimatcopy,ZIMATCOPY)("C", "R", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        FC_GLOBAL(zimatcopy,ZIMATCOPY)("C", "T", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        FC_GLOBAL(zimatcopy,ZIMATCOPY)("C", "C", &rows, &cols, &alpha, col_A, &lda, &ldb, 1, 1);
        printf("OMATCOPY_2C\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }


    }
#ifdef FLEXIBLAS_CBLAS
    printf("\n\n");
    printf("CBLAS\n");
    printf("\n\n");

    /* Square matrices  */
    {
        float  col_A[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
        float  alpha = 2;
        int rows, cols, lda, ldb, i,j;
        int rowst, colst;
        rows = lda = 4;
        cols = 4;
        ldb = 4;
        rowst = 4;
        colst = 4;

        printf("Single\n");
        /* OMATCOPY_2N */
        cblas_simatcopy(CblasColMajor, CblasNoTrans, rows, cols, alpha, col_A, lda, ldb);
        printf("IMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        cblas_simatcopy(CblasColMajor, CblasConjNoTrans, rows, cols, alpha, col_A, lda, ldb);
        printf("IMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        cblas_simatcopy(CblasColMajor, CblasTrans, rows, cols, alpha, col_A, lda, ldb);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        cblas_simatcopy(CblasColMajor, CblasConjTrans, rows, cols, alpha, col_A, lda,ldb);
        printf("OMATCOPY_2C\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }


    }

    {
        double   col_A[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
        double   alpha = 2;
        int rows, cols, lda, ldb, i,j;
        int rowst, colst;
        rows = lda = 4;
        cols = 4;
        ldb = 4;
        rowst = 4;
        colst = 4;

        printf("Double\n");
        /* OMATCOPY_2N */
        cblas_dimatcopy(CblasColMajor, CblasNoTrans, rows, cols, alpha, col_A, lda, ldb);
        printf("OMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        cblas_dimatcopy(CblasColMajor, CblasConjNoTrans, rows, cols, alpha, col_A, lda, ldb);
        printf("OMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        cblas_dimatcopy(CblasColMajor, CblasTrans, rows, cols, alpha, col_A, lda, ldb);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        cblas_dimatcopy(CblasColMajor, CblasConjTrans, rows, cols, alpha, col_A, lda, ldb);
        printf("OMATCOPY_2C\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g\t", col_A[i+j*ldb]);
            }
            printf("\n");
        }


    }

    /* Square matrices  */
    {
        float complex  col_A[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
        float complex  alpha = 2+I;
        int rows, cols, lda, ldb, i,j;
        int rowst, colst;
        rows = lda = 4;
        cols = 4;
        ldb = 4;
        rowst = 4;
        colst = 4;

        printf("Single\n");
        /* OMATCOPY_2N */
        cblas_cimatcopy(CblasColMajor, CblasNoTrans, rows, cols, &alpha, col_A, lda, ldb);
        printf("IMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        cblas_cimatcopy(CblasColMajor, CblasConjNoTrans, rows, cols, &alpha, col_A, lda, ldb);
        printf("IMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        cblas_cimatcopy(CblasColMajor, CblasTrans, rows, cols, &alpha, col_A, lda, ldb);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        cblas_cimatcopy(CblasColMajor, CblasConjTrans, rows, cols, &alpha, col_A, lda,ldb);
        printf("OMATCOPY_2C\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }


    }

    {
        double complex  col_A[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
        double complex  alpha = 2+I;
        int rows, cols, lda, ldb, i,j;
        int rowst, colst;
        rows = lda = 4;
        cols = 4;
        ldb = 4;
        rowst = 4;
        colst = 4;

        printf("Double\n");
        /* OMATCOPY_2N */
        cblas_zimatcopy(CblasColMajor, CblasNoTrans, rows, cols, &alpha, col_A, lda, ldb);
        printf("OMATCOPY_2N\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2R */
        cblas_zimatcopy(CblasColMajor, CblasConjNoTrans, rows, cols, &alpha, col_A, lda, ldb);
        printf("OMATCOPY_2R\n");
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2T */
        cblas_zimatcopy(CblasColMajor, CblasTrans, rows, cols, &alpha, col_A, lda, ldb);
        printf("OMATCOPY_2T\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }
        /* OMATCOPY_2C */
        cblas_zimatcopy(CblasColMajor, CblasConjTrans, rows, cols, &alpha, col_A, lda, ldb);
        printf("OMATCOPY_2C\n");
        for (i = 0; i < rowst; i++) {
            for (j = 0; j < colst; j++) {
                printf("%4g %4g\t", creal(col_A[i+j*ldb]), cimag(col_A[i+j*ldb]));
            }
            printf("\n");
        }


    }

#endif

    return 0;
}
