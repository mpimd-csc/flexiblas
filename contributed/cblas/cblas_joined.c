/*
 * cblas_caxpy.c
 *
 * The program is a C interface to caxpy.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_caxpy)( const CBLAS_INT N, const void *alpha, const void *X,
                       const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_caxpy( &F77_N, alpha, X, &F77_incX, Y, &F77_incY);
}

/*
 * cblas_ccopy.c
 *
 * The program is a C interface to ccopy.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ccopy)( const CBLAS_INT N, const void *X,
                      const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_ccopy( &F77_N, X, &F77_incX, Y, &F77_incY);
}

/*
 * cblas_cdotc_sub.c
 *
 * The program is a C interface to cdotc.
 * It calls the fortran wrapper before calling cdotc.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cdotc_sub)( const CBLAS_INT N, const void *X, const CBLAS_INT incX,
                      const void *Y, const CBLAS_INT incY, void *dotc)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_cdotc_sub( &F77_N, X, &F77_incX, Y, &F77_incY, dotc);
   return;
}

/*
 * cblas_cdotu_sub.c
 *
 * The program is a C interface to cdotu.
 * It calls the fortran wrapper before calling cdotu.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cdotu_sub)( const CBLAS_INT N, const void *X, const CBLAS_INT incX,
		      const void *Y, const CBLAS_INT incY, void *dotu)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_cdotu_sub( &F77_N, X, &F77_incX, Y, &F77_incY, dotu);
   return;
}

/*
 * cblas_cgbmv.c
 * The program is a C interface of cgbmv
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cgbmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                 const CBLAS_INT KL, const CBLAS_INT KU,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void  *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char TA;
#ifdef F77_CHAR
   F77_CHAR F77_TA;
#else
   #define F77_TA &TA
#endif
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
   F77_INT F77_KL=KL,F77_KU=KU;
#else
   CBLAS_INT incx=incX;
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_KL KL
   #define F77_KU KU
   #define F77_incX incx
   #define F77_incY incY
#endif
   CBLAS_INT n=0, i=0;
   const float *xx= (const float *)X, *alp= (const float *)alpha, *bet = (const float *)beta;
   float ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   float *x, *y, *st=0, *tx=0;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

    memcpy(&x, &X, sizeof(float*));
    memcpy(&y, &Y, sizeof(float*));


   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cgbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_cgbmv(F77_TA, &F77_M, &F77_N, &F77_KL, &F77_KU, alpha,
                     A, &F77_lda, X, &F77_incX, beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         ALPHA[0]= *alp;
         ALPHA[1]= -alp[1];
         BETA[0]= *bet;
         BETA[1]= -bet[1];
         TA = 'N';
         if (M > 0)
         {
            n = M << 1;
            x = malloc(n*sizeof(float));
            tx = x;

            if( incX > 0 ) {
               i = incX << 1 ;
               tincx = 2;
               st= x+n;
            } else {
               i = incX *(-2);
               tincx = -2;
               st = x-2;
               x +=(n-2);
            }
            do
            {
               *x = *xx;
               x[1] = -xx[1];
               x += tincx ;
               xx += i;
            }
            while (x != st);
            x=tx;

            #ifdef F77_INT
               F77_incX = 1;
            #else
               incx = 1;
            #endif

            if( incY > 0 )
              tincY = incY;
            else
              tincY = -incY;

            y++;

            if (N > 0)
            {
               i = tincY << 1;
               n = i * N ;
               st = y + n;
               do {
                  *y = -(*y);
                  y += i;
               } while(y != st);
               y -= n;
            }
         }
         else memcpy(&x, &X, sizeof(float*));


      }
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cgbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      if (TransA == CblasConjTrans)
         F77_cgbmv(F77_TA, &F77_N, &F77_M, &F77_KU, &F77_KL, ALPHA,
                        A ,&F77_lda, x,&F77_incX, BETA, Y, &F77_incY);
      else
         F77_cgbmv(F77_TA, &F77_N, &F77_M, &F77_KU, &F77_KL, alpha,
                        A ,&F77_lda, x,&F77_incX, beta, Y, &F77_incY);
      if (TransA == CblasConjTrans)
      {
         if (x != X) free(x);
         if (N > 0)
         {
            do
            {
               *y = -(*y);
               y += i;
            }
            while (y != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_cgbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
}

/*
 *
 * cblas_cgemm.c
 * This program is a C interface to cgemm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cgemm)(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
                 const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
                 const CBLAS_INT K, const void *alpha, const void  *A,
                 const CBLAS_INT lda, const void  *B, const CBLAS_INT ldb,
                 const void *beta, void  *C, const CBLAS_INT ldc)
{
   char TA, TB;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_TB;
#else
   #define F77_TA &TA
   #define F77_TB &TB
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if(TransA == CblasTrans) TA='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cgemm", "Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if(TransB == CblasTrans) TB='T';
      else if ( TransB == CblasConjTrans ) TB='C';
      else if ( TransB == CblasNoTrans )   TB='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_cgemm", "Illegal TransB setting, %d\n", TransB);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
         F77_TB = C2F_CHAR(&TB);
      #endif

      F77_cgemm(F77_TA, F77_TB, &F77_M, &F77_N, &F77_K, alpha, A,
                     &F77_lda, B, &F77_ldb, beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if(TransA == CblasTrans) TB='T';
      else if ( TransA == CblasConjTrans ) TB='C';
      else if ( TransA == CblasNoTrans )   TB='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cgemm", "Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if(TransB == CblasTrans) TA='T';
      else if ( TransB == CblasConjTrans ) TA='C';
      else if ( TransB == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cgemm", "Illegal TransB setting, %d\n", TransB);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
         F77_TB = C2F_CHAR(&TB);
      #endif

      F77_cgemm(F77_TA, F77_TB, &F77_N, &F77_M, &F77_K, alpha, B,
                  &F77_ldb, A, &F77_lda, beta, C, &F77_ldc);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_cgemm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_cgemmtr.c
 * This program is a C interface to cgemmtr.
 * Written by Martin Koehler, MPI Magdeburg
 * 06/24/2024
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cgemmtr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
        const CBLAS_INT K, const void *alpha, const void  *A,
        const CBLAS_INT lda, const void  *B, const CBLAS_INT ldb,
        const void *beta, void  *C, const CBLAS_INT ldc)
{
    char TA, TB;
    char UL;
#ifdef F77_CHAR
    F77_CHAR F77_TA, F77_TB, F77_UL;
#else
#define F77_TA &TA
#define F77_TB &TB
#define F77_UL &UL
#endif

#ifdef F77_INT
    F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
    F77_INT F77_ldc=ldc;
#else
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldb ldb
#define F77_ldc ldc
#endif

    extern int CBLAS_CallFromC;
    extern int RowMajorStrg;
    RowMajorStrg = 0;
    CBLAS_CallFromC = 1;


    if( layout == CblasColMajor )
    {
        if ( Uplo == CblasUpper ) UL = 'U';
        else if (Uplo == CblasLower) UL= 'L';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_cgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransA == CblasTrans) TA='T';
        else if ( TransA == CblasConjTrans ) TA='C';
        else if ( TransA == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "cblas_cgemmtr", "Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransB == CblasTrans) TB='T';
        else if ( TransB == CblasConjTrans ) TB='C';
        else if ( TransB == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "cblas_cgemmtr", "Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);
#endif

        F77_cgemmtr(F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, alpha, A,
                &F77_lda, B, &F77_ldb, beta, C, &F77_ldc);
    }
    else if (layout == CblasRowMajor)
    {
        RowMajorStrg = 1;

        if ( Uplo == CblasUpper ) UL = 'L';
        else if (Uplo == CblasLower) UL= 'U';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_cgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransA == CblasTrans) TB='T';
        else if ( TransA == CblasConjTrans ) TB='C';
        else if ( TransA == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "cblas_cgemmtr", "Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
        if(TransB == CblasTrans) TA='T';
        else if ( TransB == CblasConjTrans ) TA='C';
        else if ( TransB == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "cblas_cgemmtr", "Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);

#endif

        F77_cgemmtr(F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, alpha, B,
                &F77_ldb, A, &F77_lda, beta, C, &F77_ldc);
    }
    else API_SUFFIX(cblas_xerbla)(1, "cblas_cgemmtr", "Illegal layout setting, %d\n", layout);
    CBLAS_CallFromC = 0;
    RowMajorStrg = 0;
    return;
}

/*
 * cblas_cgemv.c
 * The program is a C interface of cgemv
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cgemv)(const CBLAS_LAYOUT layout,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void  *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char TA;
#ifdef F77_CHAR
   F77_CHAR F77_TA;
#else
   #define F77_TA &TA
#endif
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incx
   #define F77_incY incY
#endif

   CBLAS_INT n=0, i=0;
   const float *xx= (const float *)X;
   float ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   float *x, *y, *st=0, *tx=0;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;

   memcpy(&x, &X, sizeof(float *));
   memcpy(&y, &Y, sizeof(float *));

   const float *stx = x;

   if (layout == CblasColMajor)
   {
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cgemv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_cgemv(F77_TA, &F77_M, &F77_N, alpha, A, &F77_lda, X, &F77_incX,
                beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         ALPHA[0]=    *( (const float *)  alpha    );
         ALPHA[1]= -( *( (const float *)  alpha+1) );
         BETA[0]=     *( (const float *)  beta     );
         BETA[1]= -(  *( (const float *)  beta+1 ) );
         TA = 'N';
         if (M > 0)
         {
            n = M << 1;
            x = malloc(n*sizeof(float));
            tx = x;
            if( incX > 0 ) {
               i = incX << 1 ;
               tincx = 2;
               st= x+n;
            } else {
               i = incX *(-2);
               tincx = -2;
               st = x-2;
               x +=(n-2);
            }

            do
            {
               *x = *xx;
               x[1] = -xx[1];
               x += tincx ;
               xx += i;
            }
            while (x != st);
            x=tx;

            F77_incX = 1;

            if(incY > 0)
               tincY = incY;
            else
               tincY = -incY;

            y++;

            if (N > 0)
            {
               i = tincY << 1;
               n = i * N ;
               st = y + n;
               do {
                  *y = -(*y);
                  y += i;
               } while(y != st);
               y -= n;
            }
            stx = x;
         }
         else stx = (const float *)X;
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cgemv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      if (TransA == CblasConjTrans)
         F77_cgemv(F77_TA, &F77_N, &F77_M, ALPHA, A, &F77_lda, stx,
                &F77_incX, BETA, Y, &F77_incY);
      else
         F77_cgemv(F77_TA, &F77_N, &F77_M, alpha, A, &F77_lda, x,
                &F77_incX, beta, Y, &F77_incY);

      if (TransA == CblasConjTrans)
      {
         if (x != (const float *)X) free(x);
         if (N > 0)
         {
            do
            {
               *y = -(*y);
               y += i;
            }
            while (y != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_cgemv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_cgerc.c
 * The program is a C interface to cgerc.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cgerc)(const CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void *X, const CBLAS_INT incX,
                 const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incy = incY;
   #define F77_M M
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incy
   #define F77_lda lda
#endif

   CBLAS_INT n, i, tincy;
   float *y, *yy, *ty, *st;
   memcpy(&y,&Y,sizeof(float*));
   memcpy(&yy,&Y,sizeof(float*));

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      F77_cgerc( &F77_M, &F77_N, alpha, X, &F77_incX, Y, &F77_incY, A,
                      &F77_lda);
   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (N > 0)
      {
         n = N << 1;
         y = malloc(n*sizeof(float));

         ty = y;
         if( incY > 0 ) {
            i = incY << 1;
            tincy = 2;
            st= y+n;
         } else {
            i = incY *(-2);
            tincy = -2;
            st = y-2;
            y +=(n-2);
         }
         do
         {
            *y = *yy;
            y[1] = -yy[1];
            y += tincy ;
            yy += i;
         }
         while (y != st);
         y = ty;

         #ifdef F77_INT
            F77_incY = 1;
         #else
            incy = 1;
         #endif
      }
      else
        memcpy(&y,&Y,sizeof(float*));

      F77_cgeru( &F77_N, &F77_M, alpha, y, &F77_incY, X, &F77_incX, A,
                      &F77_lda);
      if(Y!=y)
         free(y);

   } else API_SUFFIX(cblas_xerbla)(1, "cblas_cgerc", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_cgeru.c
 * The program is a C interface to cgeru.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cgeru)(const CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void *X, const CBLAS_INT incX,
                 const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_M M
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
   #define F77_lda lda
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;

   if (layout == CblasColMajor)
   {
      F77_cgeru( &F77_M, &F77_N, alpha, X, &F77_incX, Y, &F77_incY, A,
                      &F77_lda);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      F77_cgeru( &F77_N, &F77_M, alpha, Y, &F77_incY, X, &F77_incX, A,
                      &F77_lda);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_cgeru","Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_chbmv.c
 * The program is a C interface to chbmv
 *
 * Keita Teranishi  5/18/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
void API_SUFFIX(cblas_chbmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo,const CBLAS_INT N,const CBLAS_INT K,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void  *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX;
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incx
   #define F77_incY incY
#endif
   CBLAS_INT n, i=0;
   const float *xx= (const float *)X, *alp= (const float *)alpha, *bet = (const float *)beta;
   float ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   float *x, *y, *st=0, *tx;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x, &X, sizeof(float*));
   memcpy(&y, &Y, sizeof(float*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chbmv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_chbmv(F77_UL, &F77_N, &F77_K, alpha, A, &F77_lda, X,
                     &F77_incX, beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      ALPHA[0]= *alp;
      ALPHA[1]= -alp[1];
      BETA[0]= *bet;
      BETA[1]= -bet[1];

      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(float));

         tx = x;
         if( incX > 0 ) {
           i = incX << 1 ;
           tincx = 2;
           st= x+n;
         } else {
           i = incX *(-2);
           tincx = -2;
           st = x-2;
           x +=(n-2);
         }

         do
         {
           *x = *xx;
           x[1] = -xx[1];
           x += tincx ;
           xx += i;
         }
         while (x != st);
         x=tx;


         #ifdef F77_INT
            F77_incX = 1;
         #else
            incx = 1;
         #endif

         if(incY > 0)
           tincY = incY;
         else
           tincY = -incY;
         y++;

         i = tincY << 1;
         n = i * N ;
         st = y + n;
         do {
            *y = -(*y);
            y += i;
         } while(y != st);
         y -= n;
      }  else
         memcpy(&x, &X, sizeof(float*));

      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_chbmv(F77_UL, &F77_N, &F77_K, ALPHA,
                     A ,&F77_lda, x,&F77_incX, BETA, Y, &F77_incY);
   }
   else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_chbmv","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if ( layout == CblasRowMajor )
   {
      RowMajorStrg = 1;
      if(X!=x)
         free(x);
      if (N > 0)
      {
         do
         {
            *y = -(*y);
            y += i;
         }
         while (y != st);
      }
   }
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_chemm.c
 * This program is a C interface to chemm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_chemm)(const CBLAS_LAYOUT layout, const  CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void *A, const CBLAS_INT lda,
                 const void *B, const CBLAS_INT ldb, const void *beta,
                 void *C, const CBLAS_INT ldc)
{
   char SD, UL;
#ifdef F77_CHAR
   F77_CHAR F77_SD, F77_UL;
#else
   #define F77_SD &SD
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chemm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_chemm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_chemm(F77_SD, F77_UL, &F77_M, &F77_N, alpha, A, &F77_lda,
                     B, &F77_ldb, beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chemm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_chemm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_chemm(F77_SD, F77_UL, &F77_N, &F77_M, alpha, A,
                 &F77_lda, B, &F77_ldb, beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_chemm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_chemv.c
 * The program is a C interface to chemv
 *
 * Keita Teranishi  5/18/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_chemv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo, const CBLAS_INT N,
                 const void *alpha, const void *A, const CBLAS_INT lda,
                 const void *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX;
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incx
   #define F77_incY incY
#endif
   CBLAS_INT n=0, i=0;
   const float *xx= (const float *)X, *alp= (const float *)alpha, *bet = (const float *)beta;
   float ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   float *x, *y, *st=0, *tx;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x, &X, sizeof(float*));
   memcpy(&y, &Y, sizeof(float*));


   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chemv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_chemv(F77_UL, &F77_N, alpha, A, &F77_lda, X, &F77_incX,
                beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      ALPHA[0]= *alp;
      ALPHA[1]= -alp[1];
      BETA[0]= *bet;
      BETA[1]= -bet[1];

      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(float));

         tx = x;
         if( incX > 0 ) {
           i = incX << 1 ;
           tincx = 2;
           st= x+n;
         } else {
           i = incX *(-2);
           tincx = -2;
           st = x-2;
           x +=(n-2);
         }

         do
         {
           *x = *xx;
           x[1] = -xx[1];
           x += tincx ;
           xx += i;
         }
         while (x != st);
         x=tx;


         #ifdef F77_INT
            F77_incX = 1;
         #else
            incx = 1;
         #endif

         if(incY > 0)
           tincY = incY;
         else
           tincY = -incY;
         y++;

         i = tincY << 1;
         n = i * N ;
         st = y + n;
         do {
            *y = -(*y);
            y += i;
         } while(y != st);
         y -= n;
      }  else
         memcpy(&x, &X, sizeof(float*));


      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chemv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_chemv(F77_UL, &F77_N, ALPHA, A, &F77_lda, x, &F77_incX,
                BETA, Y, &F77_incY);
   }
   else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_chemv","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if ( layout == CblasRowMajor )
   {
      RowMajorStrg = 1;
      if ( X != x )
         free(x);
      if (N > 0)
      {
         do
         {
            *y = -(*y);
            y += i;
         }
         while (y != st);
     }
   }
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_cher2.c
 * The program is a C interface to cher2.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cher2)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX,
                 const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX, incy = incY;
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incx
   #define F77_incY incy
#endif
   CBLAS_INT n, i, j, tincx, tincy;
   float *x, *xx, *y,
         *yy, *tx, *ty, *stx, *sty;

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(float*));
   memcpy(&xx,&X,sizeof(float*));
   memcpy(&y,&Y,sizeof(float*));
   memcpy(&yy,&Y,sizeof(float*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cher2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_cher2(F77_UL, &F77_N, alpha, X, &F77_incX,
                                            Y, &F77_incY, A, &F77_lda);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cher2","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(float));
         y = malloc(n*sizeof(float));
         tx = x;
         ty = y;
         if( incX > 0 ) {
            i = incX << 1 ;
            tincx = 2;
            stx= x+n;
         } else {
            i = incX *(-2);
            tincx = -2;
            stx = x-2;
            x +=(n-2);
         }

         if( incY > 0 ) {
            j = incY << 1;
            tincy = 2;
            sty= y+n;
         } else {
            j = incY *(-2);
            tincy = -2;
            sty = y-2;
            y +=(n-2);
         }

         do
         {
            *x = *xx;
            x[1] = -xx[1];
            x += tincx ;
            xx += i;
         }
         while (x != stx);

         do
         {
            *y = *yy;
            y[1] = -yy[1];
            y += tincy ;
            yy += j;
         }
         while (y != sty);

         x=tx;
         y=ty;

         #ifdef F77_INT
            F77_incX = 1;
            F77_incY = 1;
         #else
            incx = 1;
            incy = 1;
         #endif
      }  else
      {
         memcpy(&x,&X,sizeof(float*));
         memcpy(&y,&Y,sizeof(float*));
      }
      F77_cher2(F77_UL, &F77_N, alpha, y, &F77_incY, x,
                                      &F77_incX, A, &F77_lda);
   } else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_cher2","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if(X!=x)
      free(x);
   if(Y!=y)
      free(y);

   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_cher2k.c
 * This program is a C interface to cher2k.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cher2k)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                  const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                  const void *alpha, const void *A, const CBLAS_INT lda,
                  const void *B, const CBLAS_INT ldb, const float beta,
                  void *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   float ALPHA[2];
   const float *alp=(const float *)alpha;

   CBLAS_CallFromC = 1;
   RowMajorStrg = 0;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cher2k", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_cher2k", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_cher2k(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cher2k", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_cher2k", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      ALPHA[0]= *alp;
      ALPHA[1]= -alp[1];
      F77_cher2k(F77_UL,F77_TR, &F77_N, &F77_K, ALPHA, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_cher2k", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_cher.c
 * The program is a C interface to cher.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cher)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const float alpha, const void *X, const CBLAS_INT incX
                ,void *A, const CBLAS_INT lda)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   CBLAS_INT incx;
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incx
#endif
   CBLAS_INT n, i, tincx;
   float *x, *xx, *tx, *st;

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(float*));
   memcpy(&xx,&X,sizeof(float*));


   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cher","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_cher(F77_UL, &F77_N, &alpha, X, &F77_incX, A, &F77_lda);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cher","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(float));
         tx = x;
         if( incX > 0 ) {
            i = incX << 1 ;
            tincx = 2;
            st= x+n;
         } else {
            i = incX *(-2);
            tincx = -2;
            st = x-2;
            x +=(n-2);
         }
         do
         {
            *x = *xx;
            x[1] = -xx[1];
            x += tincx ;
            xx += i;
         }
         while (x != st);
         x=tx;

         #ifdef F77_INT
           F77_incX = 1;
         #else
           incx = 1;
         #endif
      }
      else
        memcpy(&x,&X,sizeof(float*));

      F77_cher(F77_UL, &F77_N, &alpha, x, &F77_incX, A, &F77_lda);
   } else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_cher","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if(X!=x)
      free(x);

   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_cherk.c
 * This program is a C interface to cherk.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cherk)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                 const float alpha, const void *A, const CBLAS_INT lda,
                 const float beta, void *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_cherk", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_cherk", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_cherk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda,
                     &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_cherk", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_cherk", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_cherk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda,
                &beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_cherk", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_chpmv.c
 * The program is a C interface of chpmv
 *
 * Keita Teranishi  5/18/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_chpmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo,const CBLAS_INT N,
                 const void *alpha, const void  *AP,
                 const void  *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX;
   #define F77_N N
   #define F77_incX incx
   #define F77_incY incY
#endif
   CBLAS_INT n, i=0;
   const float *xx= (const float *)X, *alp= (const float *)alpha, *bet = (const float *)beta;
   float ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   float *x, *y, *st=0, *tx;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(float*));
   memcpy(&y,&Y,sizeof(float*));


   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chpmv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_chpmv(F77_UL, &F77_N, alpha, AP, X,
                     &F77_incX, beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      ALPHA[0]= *alp;
      ALPHA[1]= -alp[1];
      BETA[0]= *bet;
      BETA[1]= -bet[1];

      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(float));

         tx = x;
         if( incX > 0 ) {
           i = incX << 1;
           tincx = 2;
           st= x+n;
         } else {
           i = incX *(-2);
           tincx = -2;
           st = x-2;
           x +=(n-2);
         }

         do
         {
           *x = *xx;
           x[1] = -xx[1];
           x += tincx ;
           xx += i;
         }
         while (x != st);
         x=tx;


         #ifdef F77_INT
            F77_incX = 1;
         #else
            incx = 1;
         #endif

         if(incY > 0)
           tincY = incY;
         else
           tincY = -incY;
         y++;

         i = tincY << 1;
         n = i * N ;
         st = y + n;
         do {
            *y = -(*y);
            y += i;
         } while(y != st);
         y -= n;
      }  else
         memcpy(&x,&X,sizeof(float*));

      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chpmv","Illegal Uplo setting, %d\n", Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_chpmv(F77_UL, &F77_N, ALPHA,
                     AP, x, &F77_incX, BETA, Y, &F77_incY);
   }
   else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_chpmv","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if ( layout == CblasRowMajor )
   {
      RowMajorStrg = 1;
      if(X!=x)
         free(x);
      if (N > 0)
      {
         do
         {
            *y = -(*y);
            y += i;
         }
         while (y != st);
     }
  }

   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_chpr2.c
 * The program is a C interface to chpr2.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_chpr2)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                      const CBLAS_INT N,const void *alpha, const void *X,
                      const CBLAS_INT incX,const void *Y, const CBLAS_INT incY, void *Ap)

{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N,  F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX;
   CBLAS_INT incy = incY;
   #define F77_N N
   #define F77_incX incx
   #define F77_incY incy
#endif
   CBLAS_INT n, i, j, tincx, tincy;
   float *x, *xx, *y,
         *yy, *tx, *ty, *stx, *sty;

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(float*));
   memcpy(&xx,&X,sizeof(float*));
   memcpy(&y,&Y,sizeof(float*));
   memcpy(&yy,&Y,sizeof(float*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chpr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_chpr2(F77_UL, &F77_N, alpha, X, &F77_incX, Y, &F77_incY, Ap);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chpr2","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(float));
         y = malloc(n*sizeof(float));
         tx = x;
         ty = y;
         if( incX > 0 ) {
            i = incX << 1 ;
            tincx = 2;
            stx= x+n;
         } else {
            i = incX *(-2);
            tincx = -2;
            stx = x-2;
            x +=(n-2);
         }

         if( incY > 0 ) {
            j = incY << 1;
            tincy = 2;
            sty= y+n;
         } else {
            j = incY *(-2);
            tincy = -2;
            sty = y-2;
            y +=(n-2);
         }

         do
         {
            *x = *xx;
            x[1] = -xx[1];
            x += tincx ;
            xx += i;
         }
         while (x != stx);
         do
         {
            *y = *yy;
            y[1] = -yy[1];
            y += tincy ;
            yy += j;
         }
         while (y != sty);

         x=tx;
         y=ty;

         #ifdef F77_INT
            F77_incX = 1;
            F77_incY = 1;
         #else
            incx = 1;
            incy = 1;
         #endif

      }  else
      {
         memcpy(&x,&X,sizeof(float*));
         memcpy(&y,&Y,sizeof(float*));
      }
      F77_chpr2(F77_UL, &F77_N, alpha, y, &F77_incY, x, &F77_incX, Ap);
   } else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_chpr2","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if(X!=x)
      free(x);
   if(Y!=y)
      free(y);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_chpr.c
 * The program is a C interface to chpr.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_chpr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const float alpha, const void *X,
                const CBLAS_INT incX, void *A)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   CBLAS_INT incx = incX;
   #define F77_N N
   #define F77_incX incx
#endif
   CBLAS_INT n, i, tincx;
   float *x, *xx, *tx, *st;

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(float*));
   memcpy(&xx,&X,sizeof(float*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chpr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_chpr(F77_UL, &F77_N, &alpha, X, &F77_incX, A);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_chpr","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(float));
         tx = x;
         if( incX > 0 ) {
            i = incX << 1;
            tincx = 2;
            st= x+n;
         } else {
            i = incX *(-2);
            tincx = -2;
            st = x-2;
            x +=(n-2);
         }
         do
         {
            *x = *xx;
            x[1] = -xx[1];
            x += tincx ;
            xx += i;
         }
         while (x != st);
         x=tx;
         #ifdef F77_INT
            F77_incX = 1;
         #else
            incx = 1;
         #endif
      }
      else
          memcpy(&x,&X,sizeof(float*));

      F77_chpr(F77_UL, &F77_N, &alpha, x, &F77_incX, A);

   } else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_chpr","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if(X!=x)
     free(x);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_crotg.c
 *
 * The program is a C interface to crotg.
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_crotg)(void *a, void *b, float *c, void *s)
{
   F77_crotg(a,b,c,s);
}


/*
 * cblas_cscal.c
 *
 * The program is a C interface to cscal.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cscal)( const CBLAS_INT N, const void *alpha, void *X,
                       const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_cscal( &F77_N, alpha, X, &F77_incX);
}

/*
 * cblas_csrot.c
 *
 * The program is a C interface to csrot.
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_csrot)(const CBLAS_INT N, void *X, const CBLAS_INT incX,
   void *Y, const CBLAS_INT incY, const float c, const float s)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_csrot(&F77_N, X, &F77_incX, Y, &F77_incY, &c, &s);
   return;
}

/*
 * cblas_csscal.c
 *
 * The program is a C interface to csscal.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_csscal)( const CBLAS_INT N, const float alpha, void *X,
                       const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_csscal( &F77_N, &alpha, X, &F77_incX);
}

/*
 * cblas_cswap.c
 *
 * The program is a C interface to cswap.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_cswap)( const CBLAS_INT N, void *X, const CBLAS_INT incX, void *Y,
                       const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_cswap( &F77_N, X, &F77_incX, Y, &F77_incY);
}

/*
 *
 * cblas_csymm.c
 * This program is a C interface to csymm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_csymm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void  *B, const CBLAS_INT ldb, const void *beta,
                 void  *C, const CBLAS_INT ldc)
{
   char SD, UL;
#ifdef F77_CHAR
   F77_CHAR F77_SD, F77_UL;
#else
   #define F77_SD &SD
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_csymm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_csymm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_csymm(F77_SD, F77_UL, &F77_M, &F77_N, alpha, A, &F77_lda,
                      B, &F77_ldb, beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_csymm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_csymm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_csymm(F77_SD, F77_UL, &F77_N, &F77_M, alpha, A, &F77_lda,
                     B, &F77_ldb, beta, C, &F77_ldc);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_csymm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_csyr2k.c
 * This program is a C interface to csyr2k.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_csyr2k)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                  const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                  const void *alpha, const void  *A, const CBLAS_INT lda,
                  const void  *B, const CBLAS_INT ldb, const void *beta,
                  void  *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_csyr2k", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_csyr2k", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }


      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_csyr2k(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda,
                      B, &F77_ldb, beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_csyr2k", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='T';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_csyr2k", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_csyr2k(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda, B, &F77_ldb, beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_csyr2k", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_csyrk.c
 * This program is a C interface to csyrk.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_csyrk)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void *beta, void  *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_csyrk", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_csyrk", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }


      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_csyrk(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda,
                beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_csyrk", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='T';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_csyrk", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_csyrk(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda,
                     beta, C, &F77_ldc);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_csyrk", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}


/*
 * cblas_ctbmv.c
 * The program is a C interface to ctbmv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ctbmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const CBLAS_INT K, const void  *A, const CBLAS_INT lda,
                 void  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_K=K, F77_incX=incX;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   float *st=0, *x=(float *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctbmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ctbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if(incX > 0)
               tincX = incX;
            else
               tincX = -incX;
            i = tincX << 1;
            n = i * N;
            x++;
            st = x + n;
            do
            {
               *x = -(*x);
               x+= i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ctbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);

      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ctbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ctbsv.c
 * The program is a C interface to ctbsv.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ctbsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const CBLAS_INT K, const void  *A, const CBLAS_INT lda,
                 void  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_K=K, F77_incX=incX;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   float *st=0,*x=(float *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctbsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctbsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctbsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ctbsv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctbsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if ( incX > 0 )
               tincX = incX;
            else
               tincX = -incX;

            n = N*2*(tincX);

            x++;

            st=x+n;

            i = tincX << 1;
            do
            {
               *x = -(*x);
               x+=i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctbsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctbsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ctbsv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);

      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x+= i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ctbsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ctpmv.c
 * The program is a C interface to ctpmv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ctpmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const void  *Ap, void  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   float *st=0,*x=(float *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctpmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctpmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctpmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ctpmv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X, &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctpmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if(incX > 0)
               tincX = incX;
            else
               tincX = -incX;
            i = tincX << 1;
            n = i * N;
            x++;
            st = x + n;
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctpmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctpmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ctpmv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X,&F77_incX);
      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ctpmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ctpsv.c
 * The program is a C interface to ctpsv.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ctpsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const void  *Ap, void  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   float *st=0, *x=(float*)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctpsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctpsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctpsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ctpsv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X, &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctpsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if ( incX > 0 )
               tincX = incX;
            else
               tincX = -incX;

            n = N*2*(tincX);

            x++;

            st=x+n;

            i = tincX << 1;
            do
            {
               *x = -(*x);
               x+=i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctpsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctpsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ctpsv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X,&F77_incX);

      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ctpsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_ctrmm.c
 * This program is a C interface to ctrmm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ctrmm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 void  *B, const CBLAS_INT ldb)
{
   char UL, TA, SD, DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_SD, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_SD &SD
   #define F77_DI &DI
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight ) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctrmm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Uplo == CblasUpper ) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctrmm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans ) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctrmm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else API_SUFFIX(cblas_xerbla)(5, "cblas_ctrmm",
                       "Illegal Diag setting, %d\n", Diag);

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ctrmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, alpha, A, &F77_lda, B, &F77_ldb);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight ) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctrmm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper ) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctrmm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans ) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctrmm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_ctrmm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ctrmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, alpha, A, &F77_lda, B, &F77_ldb);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_ctrmm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ctrmv.c
 * The program is a C interface to ctrmv.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ctrmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const void  *A, const CBLAS_INT lda,
                 void  *X, const CBLAS_INT incX)

{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   float *st=0,*x=(float *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctrmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctrmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctrmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ctrmv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctrmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if(incX > 0)
               tincX = incX;
            else
               tincX = -incX;
            i = tincX << 1;
            n = i * N;
            st = x + n;
            do
            {
               x[1] = -x[1];
               x+= i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctrmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctrmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
         F77_ctrmv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               x[1] = -x[1];
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ctrmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_ctrsm.c
 * This program is a C interface to ctrsm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ctrsm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
                 const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 void  *B, const CBLAS_INT ldb)
{
   char UL, TA, SD, DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_SD, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_SD &SD
   #define F77_DI &DI
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctrsm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctrsm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctrsm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_ctrsm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ctrsm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, alpha, A,
                &F77_lda, B, &F77_ldb);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;

      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctrsm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctrsm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctrsm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_ctrsm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif


      F77_ctrsm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, alpha, A,
                &F77_lda, B, &F77_ldb);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ctrsm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ctrsv.c
 * The program is a C interface to ctrsv.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ctrsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const void  *A, const CBLAS_INT lda, void  *X,
                 const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   float *st=0,*x=(float *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctrsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctrsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctrsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ctrsv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ctrsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if ( incX > 0 )
               tincX = incX;
            else
               tincX = -incX;

            n = N*2*(tincX);
            x++;
            st=x+n;
            i = tincX << 1;
            do
            {
               *x = -(*x);
               x+=i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ctrsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ctrsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ctrsv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ctrsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_dasum.c
 *
 * The program is a C interface to dasum.
 * It calls the fortran wrapper before calling dasum.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
double API_SUFFIX(cblas_dasum)( const CBLAS_INT N, const double *X, const CBLAS_INT incX)
{
   double asum;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_dasum_sub( &F77_N, X, &F77_incX, &asum);
   return asum;
}

/*
 * cblas_daxpy.c
 *
 * The program is a C interface to daxpy.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_daxpy)( const CBLAS_INT N, const double alpha, const double *X,
                       const CBLAS_INT incX, double *Y, const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_daxpy( &F77_N, &alpha, X, &F77_incX, Y, &F77_incY);
}

/*
 * cblas_scabs1.c
 *
 * The program is a C interface to scabs1.
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
double API_SUFFIX(cblas_dcabs1)(const void *c)
{
   double cabs1 = 0.0;
   F77_dcabs1_sub(c, &cabs1);
   return cabs1; 
}


/*
 * cblas_dcopy.c
 *
 * The program is a C interface to dcopy.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dcopy)( const CBLAS_INT N, const double *X,
                      const CBLAS_INT incX, double *Y, const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_dcopy( &F77_N, X, &F77_incX, Y, &F77_incY);
}

/*
 * cblas_ddot.c
 *
 * The program is a C interface to ddot.
 * It calls the fortran wrapper before calling ddot.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
double API_SUFFIX(cblas_ddot)( const CBLAS_INT N, const double *X,
                      const CBLAS_INT incX, const double *Y, const CBLAS_INT incY)
{
   double dot;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_ddot_sub( &F77_N, X, &F77_incX, Y, &F77_incY, &dot);
   return dot;
}

/*
 *
 * cblas_dgbmv.c
 * This program is a C interface to dgbmv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dgbmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                 const CBLAS_INT KL, const CBLAS_INT KU,
                 const double alpha, const double  *A, const CBLAS_INT lda,
                 const double  *X, const CBLAS_INT incX, const double beta,
                 double  *Y, const CBLAS_INT incY)
{
   char TA;
#ifdef F77_CHAR
   F77_CHAR F77_TA;
#else
   #define F77_TA &TA
#endif
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
   F77_INT F77_KL=KL,F77_KU=KU;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_KL KL
   #define F77_KU KU
   #define F77_incX incX
   #define F77_incY incY
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dgbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_dgbmv(F77_TA, &F77_M, &F77_N, &F77_KL, &F77_KU, &alpha,
                     A, &F77_lda, X, &F77_incX, &beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dgbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_dgbmv(F77_TA, &F77_N, &F77_M, &F77_KU, &F77_KL, &alpha,
                     A ,&F77_lda, X,&F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dgbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
}

/*
 *
 * cblas_dgemm.c
 * This program is a C interface to dgemm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dgemm)(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
                 const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
                 const CBLAS_INT K, const double alpha, const double  *A,
                 const CBLAS_INT lda, const double  *B, const CBLAS_INT ldb,
                 const double beta, double  *C, const CBLAS_INT ldc)
{
   char TA, TB;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_TB;
#else
   #define F77_TA &TA
   #define F77_TB &TB
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if(TransA == CblasTrans) TA='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dgemm","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if(TransB == CblasTrans) TB='T';
      else if ( TransB == CblasConjTrans ) TB='C';
      else if ( TransB == CblasNoTrans )   TB='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dgemm","Illegal TransB setting, %d\n", TransB);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
         F77_TB = C2F_CHAR(&TB);
      #endif

      F77_dgemm(F77_TA, F77_TB, &F77_M, &F77_N, &F77_K, &alpha, A,
       &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if(TransA == CblasTrans) TB='T';
      else if ( TransA == CblasConjTrans ) TB='C';
      else if ( TransA == CblasNoTrans )   TB='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dgemm","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if(TransB == CblasTrans) TA='T';
      else if ( TransB == CblasConjTrans ) TA='C';
      else if ( TransB == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dgemm","Illegal TransB setting, %d\n", TransB);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
         F77_TB = C2F_CHAR(&TB);
      #endif

      F77_dgemm(F77_TA, F77_TB, &F77_N, &F77_M, &F77_K, &alpha, B,
                  &F77_ldb, A, &F77_lda, &beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_dgemm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dgemmtr.c
 * This program is a C interface to dgemmtr.
 * Written by Martin Koehler, MPI Magdeburg
 * 06/24/2024
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dgemmtr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
        const CBLAS_INT K, const double alpha, const double  *A,
        const CBLAS_INT lda, const double  *B, const CBLAS_INT ldb,
        const double beta, double  *C, const CBLAS_INT ldc)
{
    char TA, TB, UL;
#ifdef F77_CHAR
    F77_CHAR F77_TA, F77_TB. F77_UL;
#else
#define F77_TA &TA
#define F77_TB &TB
#define F77_UL &UL
#endif

#ifdef F77_INT
    F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
    F77_INT F77_ldc=ldc;
#else
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldb ldb
#define F77_ldc ldc
#endif

    extern int CBLAS_CallFromC;
    extern int RowMajorStrg;
    RowMajorStrg = 0;
    CBLAS_CallFromC = 1;


    if( layout == CblasColMajor )
    {
        if ( Uplo == CblasUpper ) UL = 'U';
        else if (Uplo == CblasLower) UL= 'L';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_dgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }



        if(TransA == CblasTrans) TA='T';
        else if ( TransA == CblasConjTrans ) TA='C';
        else if ( TransA == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "cblas_dgemmtr","Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransB == CblasTrans) TB='T';
        else if ( TransB == CblasConjTrans ) TB='C';
        else if ( TransB == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "cblas_dgemmtr","Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);
#endif

        F77_dgemmtr(F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, &alpha, A,
                &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
    }
    else if (layout == CblasRowMajor)
    {
        if ( Uplo == CblasUpper ) UL = 'L';
        else if (Uplo == CblasLower) UL= 'U';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_dgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }


        RowMajorStrg = 1;
        if(TransA == CblasTrans) TB='T';
        else if ( TransA == CblasConjTrans ) TB='C';
        else if ( TransA == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "cblas_dgemmtr","Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
        if(TransB == CblasTrans) TA='T';
        else if ( TransB == CblasConjTrans ) TA='C';
        else if ( TransB == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "cblas_dgemmtr","Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);
#endif

        F77_dgemmtr( F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, &alpha, B,
                &F77_ldb, A, &F77_lda, &beta, C, &F77_ldc);
    }
    else  API_SUFFIX(cblas_xerbla)(1, "cblas_dgemmtr", "Illegal layout setting, %d\n", layout);
    CBLAS_CallFromC = 0;
    RowMajorStrg = 0;
    return;
}

/*
 *
 * cblas_dgemv.c
 * This program is a C interface to dgemv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dgemv)(const CBLAS_LAYOUT layout,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                 const double alpha, const double  *A, const CBLAS_INT lda,
                 const double  *X, const CBLAS_INT incX, const double beta,
                 double  *Y, const CBLAS_INT incY)
{
   char TA;
#ifdef F77_CHAR
   F77_CHAR F77_TA;
#else
   #define F77_TA &TA
#endif
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
   #define F77_incY incY
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dgemv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_dgemv(F77_TA, &F77_M, &F77_N, &alpha, A, &F77_lda, X, &F77_incX,
                &beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dgemv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_dgemv(F77_TA, &F77_N, &F77_M, &alpha, A, &F77_lda, X,
                &F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dgemv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dger.c
 * This program is a C interface to dger.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dger)(const CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N,
                const double alpha, const double  *X, const CBLAS_INT incX,
                const double  *Y, const CBLAS_INT incY, double  *A, const CBLAS_INT lda)
{
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_M M
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
   #define F77_lda lda
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      F77_dger( &F77_M, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY, A,
                      &F77_lda);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      F77_dger( &F77_N, &F77_M ,&alpha, Y, &F77_incY, X, &F77_incX, A,
                      &F77_lda);

   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dger", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_dnrm2.c
 *
 * The program is a C interface to dnrm2.
 * It calls the fortranwrapper before calling dnrm2.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
double API_SUFFIX(cblas_dnrm2)( const CBLAS_INT N, const double *X, const CBLAS_INT incX)
{
   double nrm2;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_dnrm2_sub( &F77_N, X, &F77_incX, &nrm2);
   return nrm2;
}

/*
 * cblas_drot.c
 *
 * The program is a C interface to drot.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_drot)(const CBLAS_INT N, double *X, const CBLAS_INT incX,
   double *Y, const CBLAS_INT incY, const double c, const double s)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_drot(&F77_N, X, &F77_incX, Y, &F77_incY, &c, &s);
   return;
}

/*
 * cblas_drotg.c
 *
 * The program is a C interface to drotg.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_drotg)(  double *a, double *b, double *c, double *s)
{
   F77_drotg(a,b,c,s);
}

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_drotm)( const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y,
                       const CBLAS_INT incY, const double *P)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_drotm( &F77_N, X, &F77_incX, Y, &F77_incY, P);
}

/*
 * cblas_drotmg.c
 *
 * The program is a C interface to drotmg.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_drotmg)( double *d1, double *d2, double *b1,
                        const double b2, double *p)
{
   F77_drotmg(d1,d2,b1,&b2,p);
}

/*
 *
 * cblas_dsbmv.c
 * This program is a C interface to dsbmv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dsbmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo, const CBLAS_INT N, const CBLAS_INT K,
                 const double alpha, const double  *A, const CBLAS_INT lda,
                 const double  *X, const CBLAS_INT incX, const double beta,
                 double  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
   #define F77_incY incY
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsbmv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dsbmv(F77_UL, &F77_N, &F77_K, &alpha, A, &F77_lda, X,
                     &F77_incX, &beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dsbmv(F77_UL, &F77_N, &F77_K, &alpha,
                     A ,&F77_lda, X,&F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dsbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_dscal.c
 *
 * The program is a C interface to dscal.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dscal)( const CBLAS_INT N, const double alpha, double *X,
                       const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_dscal( &F77_N, &alpha, X, &F77_incX);
}

/*
 * cblas_dsdot.c
 *
 * The program is a C interface to dsdot.
 * It calls fthe fortran wrapper before calling dsdot.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
double  API_SUFFIX(cblas_dsdot)( const CBLAS_INT N, const float *X,
                      const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
   double dot;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_dsdot_sub( &F77_N, X, &F77_incX, Y, &F77_incY, &dot);
   return dot;
}

/*
 *
 * cblas_dspmv.c
 * This program is a C interface to dspmv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */


#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dspmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo, const CBLAS_INT N,
                 const double alpha, const double  *AP,
                 const double  *X, const CBLAS_INT incX, const double beta,
                 double  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dspmv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dspmv(F77_UL, &F77_N, &alpha, AP, X,
                     &F77_incX, &beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dspmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dspmv(F77_UL, &F77_N, &alpha,
                     AP, X,&F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dspmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_dspr2.c
 * The program is a C interface to dspr2.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dspr2)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const double  alpha, const double  *X,
                const CBLAS_INT incX, const double  *Y, const CBLAS_INT incY, double  *A)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dspr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_dspr2(F77_UL, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY, A);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasLower) UL = 'U';
      else if (Uplo == CblasUpper) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dspr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dspr2(F77_UL, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY,  A);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_dspr2", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dspr.c
 * This program is a C interface to dspr.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dspr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const double alpha, const double *X,
                const CBLAS_INT incX, double *Ap)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dspr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_dspr(F77_UL, &F77_N, &alpha, X, &F77_incX, Ap);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasLower) UL = 'U';
      else if (Uplo == CblasUpper) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dspr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dspr(F77_UL, &F77_N, &alpha, X, &F77_incX, Ap);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_dspr", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_dswap.c
 *
 * The program is a C interface to dswap.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dswap)( const CBLAS_INT N, double *X, const CBLAS_INT incX, double *Y,
                       const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_dswap( &F77_N, X, &F77_incX, Y, &F77_incY);
}

/*
 *
 * cblas_dsymm.c
 * This program is a C interface to dsymm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dsymm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
                 const double alpha, const double  *A, const CBLAS_INT lda,
                 const double  *B, const CBLAS_INT ldb, const double beta,
                 double  *C, const CBLAS_INT ldc)
{
   char SD, UL;
#ifdef F77_CHAR
   F77_CHAR F77_SD, F77_UL;
#else
   #define F77_SD &SD
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsymm","Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dsymm","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_dsymm(F77_SD, F77_UL, &F77_M, &F77_N, &alpha, A, &F77_lda,
                      B, &F77_ldb, &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsymm","Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dsymm","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_dsymm(F77_SD, F77_UL, &F77_N, &F77_M, &alpha, A, &F77_lda, B,
                 &F77_ldb, &beta, C, &F77_ldc);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dsymm","Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dsymv.c
 * This program is a C interface to dsymv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dsymv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo, const CBLAS_INT N,
                 const double alpha, const double  *A, const CBLAS_INT lda,
                 const double  *X, const CBLAS_INT incX, const double beta,
                 double  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
   #define F77_incY incY
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsymv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dsymv(F77_UL, &F77_N, &alpha, A, &F77_lda, X,
                     &F77_incX, &beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsymv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dsymv(F77_UL, &F77_N, &alpha,
                     A ,&F77_lda, X,&F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dsymv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dsyr2.c
 * This program is a C interface to dsyr2.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dsyr2)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const double  alpha, const double  *X,
                const CBLAS_INT incX, const double  *Y, const CBLAS_INT incY, double  *A,
                const CBLAS_INT lda)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY, F77_lda=lda;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
   #define F77_lda  lda
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsyr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_dsyr2(F77_UL, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY, A,
                    &F77_lda);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasLower) UL = 'U';
      else if (Uplo == CblasUpper) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsyr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dsyr2(F77_UL, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY,  A,
                    &F77_lda);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_dsyr2", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dsyr2k.c
 * This program is a C interface to dsyr2k.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dsyr2k)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                  const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                  const double alpha, const double  *A, const CBLAS_INT lda,
                  const double  *B, const CBLAS_INT ldb, const double beta,
                  double  *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsyr2k","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dsyr2k","Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }


      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_dsyr2k(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda,
                      B, &F77_ldb, &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dsyr2k","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='T';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dsyr2k","Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_dsyr2k(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda, B,
                &F77_ldb, &beta, C, &F77_ldc);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dsyr2k","Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dsyr.c
 * This program is a C interface to dsyr.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dsyr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const double  alpha, const double  *X,
                const CBLAS_INT incX, double  *A, const CBLAS_INT lda)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_lda=lda;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_lda  lda
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsyr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_dsyr(F77_UL, &F77_N, &alpha, X, &F77_incX, A, &F77_lda);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasLower) UL = 'U';
      else if (Uplo == CblasUpper) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsyr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_dsyr(F77_UL, &F77_N, &alpha, X, &F77_incX, A, &F77_lda);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_dsyr", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dsyrk.c
 * This program is a C interface to dsyrk.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dsyrk)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                 const double alpha, const double  *A, const CBLAS_INT lda,
                 const double beta, double  *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dsyrk","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dsyrk","Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }


      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_dsyrk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda,
                &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dsyrk","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='T';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dsyrk","Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_dsyrk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda,
                     &beta, C, &F77_ldc);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dsyrk","Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}


/*
 * cblas_dtbmv.c
 * The program is a C interface to dtbmv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dtbmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const CBLAS_INT K, const double  *A, const CBLAS_INT lda,
                 double  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_K=K, F77_incX=incX;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtbmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_dtbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_dtbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);

   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dtbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
}

/*
 * cblas_dtbsv.c
 * The program is a C interface to dtbsv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dtbsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const CBLAS_INT K, const double  *A, const CBLAS_INT lda,
                 double  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_K=K, F77_incX=incX;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtbsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtbsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtbsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_dtbsv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtbsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtbsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtbsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_dtbsv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dtbsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_dtpmv.c
 * The program is a C interface to dtpmv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dtpmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const double  *Ap, double  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtpmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtpmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtpmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_dtpmv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X, &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtpmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtpmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtpmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_dtpmv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X,&F77_incX);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dtpmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_dtpsv.c
 * The program is a C interface to dtpsv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dtpsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const double  *Ap, double  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtpsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtpsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtpsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_dtpsv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X, &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtpsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtpsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtpsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_dtpsv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X,&F77_incX);

   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dtpsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dtrmm.c
 * This program is a C interface to dtrmm.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dtrmm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
                 const double alpha, const double  *A, const CBLAS_INT lda,
                 double  *B, const CBLAS_INT ldb)
{
   char UL, TA, SD, DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_SD, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_SD &SD
   #define F77_DI &DI
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtrmm","Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtrmm","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtrmm","Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_dtrmm","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_dtrmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, &alpha, A, &F77_lda, B, &F77_ldb);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtrmm","Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtrmm","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtrmm","Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_dtrmm","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_dtrmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, &alpha, A, &F77_lda, B, &F77_ldb);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dtrmm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dtrmv.c
 * This program is a C interface to sgemv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dtrmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const double  *A, const CBLAS_INT lda,
                 double  *X, const CBLAS_INT incX)

{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtrmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtrmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtrmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_dtrmv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtrmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtrmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtrmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_dtrmv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_dtrmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_dtrsm.c
 * This program is a C interface to dtrsm.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dtrsm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
                 const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
                 const double alpha, const double  *A, const CBLAS_INT lda,
                 double  *B, const CBLAS_INT ldb)

{
   char UL, TA, SD, DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_SD, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_SD &SD
   #define F77_DI &DI
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if      ( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtrsm","Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if      ( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtrsm","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if      ( TransA == CblasTrans    ) TA='T';
      else if ( TransA == CblasConjTrans) TA='C';
      else if ( TransA == CblasNoTrans  ) TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtrsm","Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if      ( Diag == CblasUnit   ) DI='U';
      else if ( Diag == CblasNonUnit) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_dtrsm","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_dtrsm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, &alpha,
                A, &F77_lda, B, &F77_ldb);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if      ( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtrsm","Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if      ( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtrsm","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if      ( TransA == CblasTrans    ) TA='T';
      else if ( TransA == CblasConjTrans) TA='C';
      else if ( TransA == CblasNoTrans  ) TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtrsm","Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if      ( Diag == CblasUnit   ) DI='U';
      else if ( Diag == CblasNonUnit) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_dtrsm","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_dtrsm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, &alpha, A,
               &F77_lda, B, &F77_ldb);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dtrsm","Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_dtrsv.c
 * The program is a C interface to dtrsv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_dtrsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const double  *A, const CBLAS_INT lda, double  *X,
                 const CBLAS_INT incX)

{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtrsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtrsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtrsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_dtrsv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_dtrsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_dtrsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_dtrsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_dtrsv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_dtrsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_dzasum.c
 *
 * The program is a C interface to dzasum.
 * It calls the fortran wrapper before calling dzasum.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
double API_SUFFIX(cblas_dzasum)( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
   double asum;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_dzasum_sub( &F77_N, X, &F77_incX, &asum);
   return asum;
}

/*
 * cblas_dznrm2.c
 *
 * The program is a C interface to dznrm2.
 * It calls the fortran wrapper before calling dznrm2.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
double API_SUFFIX(cblas_dznrm2)( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
   double nrm2;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_dznrm2_sub( &F77_N, X, &F77_incX, &nrm2);
   return nrm2;
}

   int CBLAS_CallFromC=0;

/*
 * cblas_icamax.c
 *
 * The program is a C interface to icamax.
 * It calls the fortran wrapper before calling icamax.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
CBLAS_INDEX API_SUFFIX(cblas_icamax)( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_iamax;
#else
   #define F77_N N
   #define F77_incX incX
   CBLAS_INT F77_iamax;
#endif
   F77_icamax_sub( &F77_N, X, &F77_incX, &F77_iamax );
   return ( F77_iamax > 0 )
      ? (CBLAS_INDEX)( F77_iamax-1 )
      : (CBLAS_INDEX) 0;
}

/*
 * cblas_idamax.c
 *
 * The program is a C interface to idamax.
 * It calls the fortran wrapper before calling idamax.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
CBLAS_INDEX API_SUFFIX(cblas_idamax)( const CBLAS_INT N, const double *X, const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_iamax;
#else
   #define F77_N N
   #define F77_incX incX
   CBLAS_INT F77_iamax;
#endif
   F77_idamax_sub( &F77_N, X, &F77_incX, &F77_iamax );
   return ( F77_iamax > 0 )
      ? (CBLAS_INDEX)( F77_iamax-1 )
      : (CBLAS_INDEX) 0;
}

/*
 * cblas_isamax.c
 *
 * The program is a C interface to isamax.
 * It calls the fortran wrapper before calling isamax.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
CBLAS_INDEX API_SUFFIX(cblas_isamax)( const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_iamax;
#else
   #define F77_N N
   #define F77_incX incX
   CBLAS_INT F77_iamax;
#endif
   F77_isamax_sub( &F77_N, X, &F77_incX, &F77_iamax );
   return ( F77_iamax > 0 )
      ? (CBLAS_INDEX)( F77_iamax-1 )
      : (CBLAS_INDEX) 0;
}

/*
 * cblas_izamax.c
 *
 * The program is a C interface to izamax.
 * It calls the fortran wrapper before calling izamax.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
CBLAS_INDEX API_SUFFIX(cblas_izamax)( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_iamax;
#else
   #define F77_N N
   #define F77_incX incX
   CBLAS_INT F77_iamax;
#endif
   F77_izamax_sub( &F77_N, X, &F77_incX, &F77_iamax );
   return ( F77_iamax > 0 )
      ? (CBLAS_INDEX)( F77_iamax-1 )
      : (CBLAS_INDEX) 0;
}

/*
 * cblas_sasum.c
 *
 * The program is a C interface to sasum.
 * It calls the fortran wrapper before calling sasum.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
float API_SUFFIX(cblas_sasum)( const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
   float asum;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_sasum_sub( &F77_N, X, &F77_incX, &asum);
   return asum;
}

/*
 * cblas_saxpy.c
 *
 * The program is a C interface to saxpy.
 * It calls the fortran wrapper before calling saxpy.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_saxpy)( const CBLAS_INT N, const float alpha, const float *X,
                       const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_saxpy( &F77_N, &alpha, X, &F77_incX, Y, &F77_incY);
}

/*
 * cblas_scabs1.c
 *
 * The program is a C interface to scabs1.
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
float API_SUFFIX(cblas_scabs1)(const void *c)
{
   float cabs1 = 0.0;
   F77_scabs1_sub(c, &cabs1);
   return cabs1; 
}


/*
 * cblas_scasum.c
 *
 * The program is a C interface to scasum.
 * It calls the fortran wrapper before calling scasum.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
float API_SUFFIX(cblas_scasum)( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
   float asum;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_scasum_sub( &F77_N, X, &F77_incX, &asum);
   return asum;
}

/*
 * cblas_scnrm2.c
 *
 * The program is a C interface to scnrm2.
 * It calls the fortran wrapper before calling scnrm2.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
float API_SUFFIX(cblas_scnrm2)( const CBLAS_INT N, const void *X, const CBLAS_INT incX)
{
   float nrm2;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_scnrm2_sub( &F77_N, X, &F77_incX, &nrm2);
   return nrm2;
}

/*
 * cblas_scopy.c
 *
 * The program is a C interface to scopy.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_scopy)( const CBLAS_INT N, const float *X,
                      const CBLAS_INT incX, float *Y, const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_scopy( &F77_N, X, &F77_incX, Y, &F77_incY);
}

/*
 * cblas_sdot.c
 *
 * The program is a C interface to sdot.
 * It calls the fortran wrapper before calling sdot.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
float API_SUFFIX(cblas_sdot)( const CBLAS_INT N, const float *X,
                      const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
   float dot;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_sdot_sub( &F77_N, X, &F77_incX, Y, &F77_incY, &dot);
   return dot;
}

/*
 * cblas_sdsdot.c
 *
 * The program is a C interface to sdsdot.
 * It calls the fortran wrapper before calling sdsdot.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
float API_SUFFIX(cblas_sdsdot)( const CBLAS_INT N, const float alpha, const float *X,
                      const CBLAS_INT incX, const float *Y, const CBLAS_INT incY)
{
   float dot;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_sdsdot_sub( &F77_N, &alpha, X, &F77_incX, Y, &F77_incY, &dot);
   return dot;
}

/*
 *
 * cblas_sgbmv.c
 * This program is a C interface to sgbmv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sgbmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                 const CBLAS_INT KL, const CBLAS_INT KU,
                 const float alpha, const float *A, const CBLAS_INT lda,
                 const float  *X, const CBLAS_INT incX, const float beta,
                 float  *Y, const CBLAS_INT incY)
{
   char TA;
#ifdef F77_CHAR
   F77_CHAR F77_TA;
#else
   #define F77_TA &TA
#endif
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
   F77_INT F77_KL=KL,F77_KU=KU;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_KL KL
   #define F77_KU KU
   #define F77_incX incX
   #define F77_incY incY
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sgbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_sgbmv(F77_TA, &F77_M, &F77_N, &F77_KL, &F77_KU, &alpha,
                     A, &F77_lda, X, &F77_incX, &beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sgbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_sgbmv(F77_TA, &F77_N, &F77_M, &F77_KU, &F77_KL, &alpha,
                     A ,&F77_lda, X, &F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_sgbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_sgemm.c
 * This program is a C interface to sgemm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sgemm)(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
                 const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
                 const CBLAS_INT K, const float alpha, const float  *A,
                 const CBLAS_INT lda, const float  *B, const CBLAS_INT ldb,
                 const float beta, float  *C, const CBLAS_INT ldc)
{
   char TA, TB;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_TB;
#else
   #define F77_TA &TA
   #define F77_TB &TB
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;
   if( layout == CblasColMajor )
   {
      if(TransA == CblasTrans) TA='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sgemm",
                       "Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if(TransB == CblasTrans) TB='T';
      else if ( TransB == CblasConjTrans ) TB='C';
      else if ( TransB == CblasNoTrans )   TB='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_sgemm",
                       "Illegal TransB setting, %d\n", TransB);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
         F77_TB = C2F_CHAR(&TB);
      #endif

      F77_sgemm(F77_TA, F77_TB, &F77_M, &F77_N, &F77_K, &alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if(TransA == CblasTrans) TB='T';
      else if ( TransA == CblasConjTrans ) TB='C';
      else if ( TransA == CblasNoTrans )   TB='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sgemm",
                       "Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if(TransB == CblasTrans) TA='T';
      else if ( TransB == CblasConjTrans ) TA='C';
      else if ( TransB == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_sgemm",
                       "Illegal TransB setting, %d\n", TransB);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
         F77_TB = C2F_CHAR(&TB);
      #endif

      F77_sgemm(F77_TA, F77_TB, &F77_N, &F77_M, &F77_K, &alpha, B, &F77_ldb, A, &F77_lda, &beta, C, &F77_ldc);
   } else
     API_SUFFIX(cblas_xerbla)(1, "cblas_sgemm",
                     "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
}


/*
 *
 * cblas_sgemmtr.c
 * This program is a C interface to sgemmtr.
 * Written by Martin Koehler, MPI Magdeburg
 * 06/24/2024
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sgemmtr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
        const CBLAS_INT K, const float alpha, const float  *A,
        const CBLAS_INT lda, const float  *B, const CBLAS_INT ldb,
        const float beta, float  *C, const CBLAS_INT ldc)
{
    char TA, TB, UL;
#ifdef F77_CHAR
    F77_CHAR F77_TA, F77_TB, F77_UL;
#else
#define F77_TA &TA
#define F77_TB &TB
#define F77_UL &UL
#endif

#ifdef F77_INT
    F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
    F77_INT F77_ldc=ldc;
#else
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldb ldb
#define F77_ldc ldc
#endif

    extern int CBLAS_CallFromC;
    extern int RowMajorStrg;
    RowMajorStrg = 0;
    CBLAS_CallFromC = 1;


    if( layout == CblasColMajor )
    {
        if ( Uplo == CblasUpper ) UL = 'U';
        else if (Uplo == CblasLower) UL= 'L';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_sgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }


        if(TransA == CblasTrans) TA='T';
        else if ( TransA == CblasConjTrans ) TA='C';
        else if ( TransA == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "cblas_sgemmtr",
                    "Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransB == CblasTrans) TB='T';
        else if ( TransB == CblasConjTrans ) TB='C';
        else if ( TransB == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "cblas_sgemmtr",
                    "Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);
#endif

        F77_sgemmtr(F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, &alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
    }
    else if (layout == CblasRowMajor)
    {
        if ( Uplo == CblasUpper ) UL = 'L';
        else if (Uplo == CblasLower) UL= 'U';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_sgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }


        RowMajorStrg = 1;
        if(TransA == CblasTrans) TB='T';
        else if ( TransA == CblasConjTrans ) TB='C';
        else if ( TransA == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "cblas_sgemmtr",
                    "Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
        if(TransB == CblasTrans) TA='T';
        else if ( TransB == CblasConjTrans ) TA='C';
        else if ( TransB == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "cblas_sgemmtr",
                    "Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);
#endif

        F77_sgemmtr(F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, &alpha, B, &F77_ldb, A, &F77_lda, &beta, C, &F77_ldc);
    } else
        API_SUFFIX(cblas_xerbla)(1, "cblas_sgemmtr",
                "Illegal layout setting, %d\n", layout);
    CBLAS_CallFromC = 0;
    RowMajorStrg = 0;
}

/*
 *
 * cblas_sgemv.c
 * This program is a C interface to sgemv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sgemv)(const CBLAS_LAYOUT layout,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                 const float alpha, const float  *A, const CBLAS_INT lda,
                 const float  *X, const CBLAS_INT incX, const float beta,
                 float  *Y, const CBLAS_INT incY)
{
   char TA;
#ifdef F77_CHAR
   F77_CHAR F77_TA;
#else
   #define F77_TA &TA
#endif
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
   #define F77_incY incY
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sgemv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_sgemv(F77_TA, &F77_M, &F77_N, &alpha, A, &F77_lda, X, &F77_incX,
                &beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sgemv", "Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_sgemv(F77_TA, &F77_N, &F77_M, &alpha, A, &F77_lda, X,
                &F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_sgemv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_sger.c
 * This program is a C interface to sger.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sger)(const CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N,
                const float  alpha, const float  *X, const CBLAS_INT incX,
                const float  *Y, const CBLAS_INT incY, float  *A, const CBLAS_INT lda)
{
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_M M
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
   #define F77_lda lda
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      F77_sger( &F77_M, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY, A,
       &F77_lda);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      F77_sger( &F77_N, &F77_M, &alpha, Y, &F77_incY, X, &F77_incX, A,
        &F77_lda);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_sger", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_snrm2.c
 *
 * The program is a C interface to snrm2.
 * It calls the fortran wrapper before calling snrm2.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
float API_SUFFIX(cblas_snrm2)( const CBLAS_INT N, const float *X, const CBLAS_INT incX)
{
   float nrm2;
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_snrm2_sub( &F77_N, X, &F77_incX, &nrm2);
   return nrm2;
}

/*
 * cblas_srot.c
 *
 * The program is a C interface to srot.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_srot)( const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y,
                      const CBLAS_INT incY, const float  c, const float  s)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_srot(&F77_N, X, &F77_incX, Y, &F77_incY, &c, &s);
}

/*
 * cblas_srotg.c
 *
 * The program is a C interface to srotg.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_srotg)(  float *a, float *b, float *c, float *s)
{
   F77_srotg(a,b,c,s);
}

/*
 * cblas_srotm.c
 *
 * The program is a C interface to srotm.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_srotm)( const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y,
                       const CBLAS_INT incY, const float *P)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_srotm( &F77_N, X, &F77_incX, Y, &F77_incY, P);
}

/*
 * cblas_srotmg.c
 *
 * The program is a C interface to srotmg.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_srotmg)( float *d1, float *d2, float *b1,
                        const float b2, float *p)
{
   F77_srotmg(d1,d2,b1,&b2,p);
}

/*
 *
 * cblas_ssbmv.c
 * This program is a C interface to ssbmv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ssbmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
           const CBLAS_INT N, const CBLAS_INT K, const float alpha, const float *A,
           const CBLAS_INT lda, const float *X, const CBLAS_INT incX,
           const float beta, float *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
   #define F77_incY incY
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {

      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssbmv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_ssbmv(F77_UL, &F77_N, &F77_K, &alpha, A, &F77_lda, X,
      &F77_incX, &beta, Y, &F77_incY);
   }else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_ssbmv(F77_UL, &F77_N, &F77_K, &alpha, A, &F77_lda, X,
      &F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ssbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_sscal.c
 *
 * The program is a C interface to sscal.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sscal)( const CBLAS_INT N, const float alpha, float *X,
                       const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_sscal( &F77_N, &alpha, X, &F77_incX);
}

/*
 *
 * cblas_sspmv.c
 * This program is a C interface to sspmv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sspmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo, const CBLAS_INT N,
                 const float alpha, const float  *AP,
                 const float  *X, const CBLAS_INT incX, const float beta,
                 float  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sspmv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_sspmv(F77_UL, &F77_N, &alpha, AP, X,
                     &F77_incX, &beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sspmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_sspmv(F77_UL, &F77_N, &alpha,
                     AP, X,&F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_sspmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
}

/*
 *
 * cblas_sspr2.c
 * This program is a C interface to sspr2.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sspr2)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const float  alpha, const float  *X,
                const CBLAS_INT incX, const float  *Y, const CBLAS_INT incY, float  *A)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sspr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_sspr2(F77_UL, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY, A);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasLower) UL = 'U';
      else if (Uplo == CblasUpper) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sspr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_sspr2(F77_UL, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY,  A);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_sspr2", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
}

/*
 *
 * cblas_sspr.c
 * This program is a C interface to sspr.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sspr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const  float alpha, const float *X,
                const CBLAS_INT incX, float *Ap)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sspr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_sspr(F77_UL, &F77_N, &alpha, X, &F77_incX, Ap);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasLower) UL = 'U';
      else if (Uplo == CblasUpper) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_sspr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_sspr(F77_UL, &F77_N, &alpha, X, &F77_incX, Ap);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_sspr", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_sswap.c
 *
 * The program is a C interface to sswap.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_sswap)( const CBLAS_INT N, float *X, const CBLAS_INT incX, float *Y,
                       const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_sswap( &F77_N, X, &F77_incX, Y, &F77_incY);
}

/*
 *
 * cblas_ssymm.c
 * This program is a C interface to ssymm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ssymm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
                 const float alpha, const float  *A, const CBLAS_INT lda,
                 const float  *B, const CBLAS_INT ldb, const float beta,
                 float  *C, const CBLAS_INT ldc)
{
   char SD, UL;
#ifdef F77_CHAR
   F77_CHAR F77_SD, F77_UL;
#else
   #define F77_SD &SD
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssymm",
                       "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ssymm",
                       "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_ssymm(F77_SD, F77_UL, &F77_M, &F77_N, &alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssymm",
                       "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ssymm",
                       "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_ssymm(F77_SD, F77_UL, &F77_N, &F77_M, &alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   } else  API_SUFFIX(cblas_xerbla)(1, "cblas_ssymm",
                     "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_ssymv.c
 * This program is a C interface to ssymv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ssymv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo, const CBLAS_INT N,
                 const float alpha, const float  *A, const CBLAS_INT lda,
                 const float  *X, const CBLAS_INT incX, const float beta,
                 float  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
   #define F77_incY incY
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssymv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_ssymv(F77_UL, &F77_N, &alpha, A, &F77_lda, X,
                     &F77_incX, &beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssymv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_ssymv(F77_UL, &F77_N, &alpha,
                     A ,&F77_lda, X,&F77_incX, &beta, Y, &F77_incY);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ssymv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_ssyr2.c
 * This program is a C interface to ssyr2.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ssyr2)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const float  alpha, const float  *X,
                const CBLAS_INT incX, const float  *Y, const CBLAS_INT incY, float  *A,
                const CBLAS_INT lda)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY, F77_lda=lda;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
   #define F77_lda  lda
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssyr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_ssyr2(F77_UL, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY, A,
                    &F77_lda);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasLower) UL = 'U';
      else if (Uplo == CblasUpper) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssyr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_ssyr2(F77_UL, &F77_N, &alpha, X, &F77_incX, Y, &F77_incY,  A,
                    &F77_lda);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_ssyr2", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_ssyr2k.c
 * This program is a C interface to ssyr2k.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ssyr2k)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                  const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                  const float alpha, const float  *A, const CBLAS_INT lda,
                  const float  *B, const CBLAS_INT ldb, const float beta,
                  float  *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssyr2k",
                       "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ssyr2k",
                       "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }


      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_ssyr2k(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ssyr2k",
                       "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='T';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ssyr2k",
                       "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_ssyr2k(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   } else  API_SUFFIX(cblas_xerbla)(1, "cblas_ssyr2k",
                     "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_ssyr.c
 * This program is a C interface to ssyr.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ssyr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const float  alpha, const float  *X,
                const CBLAS_INT incX, float  *A, const CBLAS_INT lda)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_lda=lda;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_lda  lda
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssyr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_ssyr(F77_UL, &F77_N, &alpha, X, &F77_incX, A, &F77_lda);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasLower) UL = 'U';
      else if (Uplo == CblasUpper) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssyr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_ssyr(F77_UL, &F77_N, &alpha, X, &F77_incX, A, &F77_lda);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_ssyr", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_ssyrk.c
 * This program is a C interface to ssyrk.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ssyrk)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                 const float alpha, const float  *A, const CBLAS_INT lda,
                 const float beta, float  *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ssyrk",
                       "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ssyrk",
                       "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }


      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_ssyrk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda, &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ssyrk",
                       "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='T';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ssyrk",
                       "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_ssyrk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda, &beta, C, &F77_ldc);
   } else  API_SUFFIX(cblas_xerbla)(1, "cblas_ssyrk",
                     "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}


/*
 * cblas_stbmv.c
 * This program is a C interface to stbmv.
 * Written by Keita Teranishi
 * 3/3/1998
 */
#include "cblas.h"
#include "cblas_f77.h"

void API_SUFFIX(cblas_stbmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const CBLAS_INT K, const float  *A, const CBLAS_INT lda,
                 float  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_K=K, F77_incX=incX;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_stbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_stbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_stbmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_stbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_stbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_stbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_stbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_stbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_stbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_stbsv.c
 * The program is a C interface to stbsv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_stbsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const CBLAS_INT K, const float  *A, const CBLAS_INT lda,
                 float  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_K=K, F77_incX=incX;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL  = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_stbsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_stbsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_stbsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_stbsv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_stbsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_stbsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_stbsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_stbsv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_stbsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_stpmv.c
 * This program is a C interface to stpmv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_stpmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const float  *Ap, float  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_stpmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_stpmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_stpmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_stpmv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X, &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_stpmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_stpmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_stpmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_stpmv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X,&F77_incX);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_stpmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_stpsv.c
 * The program is a C interface to stpsv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_stpsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const float  *Ap, float  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_stpsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_stpsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_stpsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_stpsv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X, &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_stpsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_stpsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_stpsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_stpsv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X,&F77_incX);

   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_stpsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_strmm.c
 * This program is a C interface to strmm.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_strmm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
                 const float alpha, const float *A, const CBLAS_INT lda,
                 float *B, const CBLAS_INT ldb)
{
   char UL, TA, SD, DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_SD, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_SD &SD
   #define F77_DI &DI
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_strmm","Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_strmm","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_strmm","Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_strmm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_strmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, &alpha, A, &F77_lda, B, &F77_ldb);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_strmm","Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_strmm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_strmm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_strmm","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
#ifdef F77_CHAR
    F77_UL = C2F_CHAR(&UL);
    F77_TA = C2F_CHAR(&TA);
    F77_SD = C2F_CHAR(&SD);
    F77_DI = C2F_CHAR(&DI);
#endif
      F77_strmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, &alpha, A,
      &F77_lda, B, &F77_ldb);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_strmm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_strmv.c
 * This program is a C interface to strmv.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_strmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const float  *A, const CBLAS_INT lda,
                 float  *X, const CBLAS_INT incX)

{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_strmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_strmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_strmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_strmv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_strmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_strmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_strmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_strmv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_strmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_strsm.c
 * This program is a C interface to strsm.
 * Written by Keita Teranishi
 * 4/6/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_strsm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
                 const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
                 const float alpha, const float  *A, const CBLAS_INT lda,
                 float  *B, const CBLAS_INT ldb)

{
   char UL, TA, SD, DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_SD, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_SD &SD
   #define F77_DI &DI
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_strsm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_strsm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_strsm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_strsm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_strsm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, &alpha, A, &F77_lda, B, &F77_ldb);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_strsm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_strsm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_strsm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_strsm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_strsm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, &alpha, A, &F77_lda, B, &F77_ldb);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_strsm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_strsv.c
 * The program is a C interface to strsv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_strsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const float  *A, const CBLAS_INT lda, float  *X,
                 const CBLAS_INT incX)

{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
#endif
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_strsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_strsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_strsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_strsv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_strsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans) TA = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_strsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_strsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_strsv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_strsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "cblas.h"
#include "cblas_f77.h"

#if defined(_WIN32) || defined(_WIN64)
int RowMajorStrg = 0;
#endif 
 
void
#ifdef HAS_ATTRIBUTE_WEAK_SUPPORT
__attribute__((weak))
#endif
API_SUFFIX(cblas_xerbla)(CBLAS_INT info, const char *rout, const char *form, ...)
{
   extern int RowMajorStrg;
   char empty[1] = "";
   va_list argptr;

   va_start(argptr, form);

   if (RowMajorStrg)
   {
      if (strstr(rout,"gemm") != 0)
      {
         if      (info == 5 ) info =  4;
         else if (info == 4 ) info =  5;
         else if (info == 11) info =  9;
         else if (info == 9 ) info = 11;
      }
      else if (strstr(rout,"symm") != 0 || strstr(rout,"hemm") != 0)
      {
         if      (info == 5 ) info =  4;
         else if (info == 4 ) info =  5;
      }
      else if (strstr(rout,"trmm") != 0 || strstr(rout,"trsm") != 0)
      {
         if      (info == 7 ) info =  6;
         else if (info == 6 ) info =  7;
      }
      else if (strstr(rout,"gemv") != 0)
      {
         if      (info == 4)  info = 3;
         else if (info == 3)  info = 4;
      }
      else if (strstr(rout,"gbmv") != 0)
      {
         if      (info == 4)  info = 3;
         else if (info == 3)  info = 4;
         else if (info == 6)  info = 5;
         else if (info == 5)  info = 6;
      }
      else if (strstr(rout,"ger") != 0)
      {
         if      (info == 3) info = 2;
         else if (info == 2) info = 3;
         else if (info == 8) info = 6;
         else if (info == 6) info = 8;
      }
      else if ( (strstr(rout,"her2") != 0 || strstr(rout,"hpr2") != 0)
                 && strstr(rout,"her2k") == 0 )
      {
         if      (info == 8) info = 6;
         else if (info == 6) info = 8;
      }
   }
   if (info)
      fprintf(stderr, "Parameter %" CBLAS_IFMT " to routine %s was incorrect\n", info, rout);
   vfprintf(stderr, form, argptr);
   va_end(argptr);
   if (info && !info)
      F77_xerbla(empty, &info); /* Force link of our F77 error handler */
   exit(-1);
}

/*
 * cblas_zaxpy.c
 *
 * The program is a C interface to zaxpy.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zaxpy)( const CBLAS_INT N, const void *alpha, const void *X,
                       const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_zaxpy( &F77_N, alpha, X, &F77_incX, Y, &F77_incY);
}

/*
 * cblas_zcopy.c
 *
 * The program is a C interface to zcopy.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zcopy)( const CBLAS_INT N, const void *X,
                      const CBLAS_INT incX, void *Y, const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_zcopy( &F77_N, X, &F77_incX, Y, &F77_incY);
}

/*
 * cblas_zdotc_sub.c
 *
 * The program is a C interface to zdotc.
 * It calls the fortran wrapper before calling zdotc.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zdotc_sub)( const CBLAS_INT N, const void *X, const CBLAS_INT incX,
                      const void *Y, const CBLAS_INT incY, void *dotc)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_zdotc_sub( &F77_N, X, &F77_incX, Y, &F77_incY, dotc);
   return;
}

/*
 * cblas_zdotu_sub.c
 *
 * The program is a C interface to zdotu.
 * It calls the fortran wrapper before calling zdotu.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zdotu_sub)( const CBLAS_INT N, const void *X, const CBLAS_INT incX,
                      const void *Y, const CBLAS_INT incY, void *dotu)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_zdotu_sub( &F77_N, X, &F77_incX, Y, &F77_incY, dotu);
   return;
}

/*
 * cblas_zdrot.c
 *
 * The program is a C interface to zdrot.
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zdrot)(const CBLAS_INT N, void *X, const CBLAS_INT incX,
   void *Y, const CBLAS_INT incY, const double c, const double s)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_zdrot(&F77_N, X, &F77_incX, Y, &F77_incY, &c, &s);
   return;
}

/*
 * cblas_zdscal.c
 *
 * The program is a C interface to zdscal.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zdscal)( const CBLAS_INT N, const double alpha, void  *X,
                       const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_zdscal( &F77_N, &alpha, X, &F77_incX);
}

/*
 * cblas_zgbmv.c
 * The program is a C interface of zgbmv
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zgbmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                 const CBLAS_INT KL, const CBLAS_INT KU,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void  *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char TA;
#ifdef F77_CHAR
   F77_CHAR F77_TA;
#else
   #define F77_TA &TA
#endif
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
   F77_INT F77_KL=KL,F77_KU=KU;
#else
   CBLAS_INT incx = incX;
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_KL KL
   #define F77_KU KU
   #define F77_incX incx
   #define F77_incY incY
#endif
   CBLAS_INT n, i=0;
   const double *xx= (const double *)X, *alp= (const double *)alpha, *bet = (const double *)beta;
   double ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   double *x, *y, *st=0, *tx;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(double*));
   memcpy(&y,&Y,sizeof(double*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zgbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_zgbmv(F77_TA, &F77_M, &F77_N, &F77_KL, &F77_KU, alpha,
                     A, &F77_lda, X, &F77_incX, beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         ALPHA[0]= *alp;
         ALPHA[1]= -alp[1];
         BETA[0]= *bet;
         BETA[1]= -bet[1];
         TA = 'N';
         if (M > 0)
         {
            n = M << 1;
            x = malloc(n*sizeof(double));
            tx = x;

            if( incX > 0 ) {
               i = incX << 1 ;
               tincx = 2;
               st= x+n;
            } else {
               i = incX *(-2);
               tincx = -2;
               st = x-2;
               x +=(n-2);
            }
            do
            {
               *x = *xx;
               x[1] = -xx[1];
               x += tincx ;
               xx += i;
            }
            while (x != st);
            x=tx;

            #ifdef F77_INT
               F77_incX = 1;
            #else
               incx = 1;
            #endif

            if( incY > 0 )
              tincY = incY;
            else
              tincY = -incY;

            y++;

            if (N > 0)
            {
               i = tincY << 1;
               n = i * N ;
               st = y + n;
               do {
                  *y = -(*y);
                  y += i;
               } while(y != st);
               y -= n;
            }
         }
         else
            memcpy(&x,&X,sizeof(double*));


      }
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zgbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      if (TransA == CblasConjTrans)
         F77_zgbmv(F77_TA, &F77_N, &F77_M, &F77_KU, &F77_KL, ALPHA,
                        A ,&F77_lda, x,&F77_incX, BETA, Y, &F77_incY);
      else
         F77_zgbmv(F77_TA, &F77_N, &F77_M, &F77_KU, &F77_KL, alpha,
                        A ,&F77_lda, x,&F77_incX, beta, Y, &F77_incY);
      if (TransA == CblasConjTrans)
      {
         if (x != X) free(x);
         if (N > 0)
         {
            do
            {
               *y = -(*y);
               y += i;
            }
            while (y != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_zgbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_zgemm.c
 * This program is a C interface to zgemm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zgemm)(const CBLAS_LAYOUT layout, const CBLAS_TRANSPOSE TransA,
                 const CBLAS_TRANSPOSE TransB, const CBLAS_INT M, const CBLAS_INT N,
                 const CBLAS_INT K, const void *alpha, const void  *A,
                 const CBLAS_INT lda, const void  *B, const CBLAS_INT ldb,
                 const void *beta, void  *C, const CBLAS_INT ldc)
{
   char TA, TB;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_TB;
#else
   #define F77_TA &TA
   #define F77_TB &TB
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if(TransA == CblasTrans) TA='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zgemm","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if(TransB == CblasTrans) TB='T';
      else if ( TransB == CblasConjTrans ) TB='C';
      else if ( TransB == CblasNoTrans )   TB='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zgemm","Illegal TransB setting, %d\n", TransB);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
         F77_TB = C2F_CHAR(&TB);
      #endif

      F77_zgemm(F77_TA, F77_TB, &F77_M, &F77_N, &F77_K, alpha, A,
                     &F77_lda, B, &F77_ldb, beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if(TransA == CblasTrans) TB='T';
      else if ( TransA == CblasConjTrans ) TB='C';
      else if ( TransA == CblasNoTrans )   TB='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zgemm","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if(TransB == CblasTrans) TA='T';
      else if ( TransB == CblasConjTrans ) TA='C';
      else if ( TransB == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zgemm","Illegal TransB setting, %d\n", TransB);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
         F77_TB = C2F_CHAR(&TB);
      #endif

      F77_zgemm(F77_TA, F77_TB, &F77_N, &F77_M, &F77_K, alpha, B,
                  &F77_ldb, A, &F77_lda, beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_zgemm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_zgemmtr.c
 * This program is a C interface to zgemmtr.
 * Written by Martin Koehler, MPI Magdeburg
 * 06/24/2024
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zgemmtr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
        const CBLAS_TRANSPOSE TransB, const CBLAS_INT N,
        const CBLAS_INT K, const void *alpha, const void  *A,
        const CBLAS_INT lda, const void  *B, const CBLAS_INT ldb,
        const void *beta, void  *C, const CBLAS_INT ldc)
{
    char TA, TB, UL;
#ifdef F77_CHAR
    F77_CHAR F77_TA, F77_TB, F77_UL;
#else
#define F77_TA &TA
#define F77_TB &TB
#define F77_UL &UL
#endif

#ifdef F77_INT
    F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
    F77_INT F77_ldc=ldc;
#else
#define F77_N N
#define F77_K K
#define F77_lda lda
#define F77_ldb ldb
#define F77_ldc ldc
#endif

    extern int CBLAS_CallFromC;
    extern int RowMajorStrg;
    RowMajorStrg = 0;
    CBLAS_CallFromC = 1;


    if( layout == CblasColMajor )
    {
        if ( Uplo == CblasUpper ) UL = 'U';
        else if (Uplo == CblasLower) UL= 'L';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_zgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }


        if(TransA == CblasTrans) TA='T';
        else if ( TransA == CblasConjTrans ) TA='C';
        else if ( TransA == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "cblas_zgemmtr","Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransB == CblasTrans) TB='T';
        else if ( TransB == CblasConjTrans ) TB='C';
        else if ( TransB == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "cblas_zgemmtr","Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);
#endif

        F77_zgemmtr(F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, alpha, A,
                &F77_lda, B, &F77_ldb, beta, C, &F77_ldc);
    }
    else if (layout == CblasRowMajor)
    {
        RowMajorStrg = 1;

        if ( Uplo == CblasUpper ) UL = 'L';
        else if (Uplo == CblasLower) UL= 'U';
        else {
            API_SUFFIX(cblas_xerbla)(2, "cblas_cgemmtr", "Illegal Uplo setting, %d\n", Uplo);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }

        if(TransA == CblasTrans) TB='T';
        else if ( TransA == CblasConjTrans ) TB='C';
        else if ( TransA == CblasNoTrans )   TB='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(3, "zblas_cgemmtr", "Illegal TransA setting, %d\n", TransA);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
        if(TransB == CblasTrans) TA='T';
        else if ( TransB == CblasConjTrans ) TA='C';
        else if ( TransB == CblasNoTrans )   TA='N';
        else
        {
            API_SUFFIX(cblas_xerbla)(4, "zblas_cgemmtr", "Illegal TransB setting, %d\n", TransB);
            CBLAS_CallFromC = 0;
            RowMajorStrg = 0;
            return;
        }
#ifdef F77_CHAR
        F77_TA = C2F_CHAR(&TA);
        F77_TB = C2F_CHAR(&TB);
        F77_UL = C2F_CHAR(&UL);

#endif

        F77_zgemmtr(F77_UL, F77_TA, F77_TB, &F77_N, &F77_K, alpha, B,
                &F77_ldb, A, &F77_lda, beta, C, &F77_ldc);
    }

    else  API_SUFFIX(cblas_xerbla)(1, "cblas_zgemmtr", "Illegal layout setting, %d\n", layout);
    CBLAS_CallFromC = 0;
    RowMajorStrg = 0;
    return;
}

/*
 * cblas_zgemv.c
 * The program is a C interface of zgemv
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zgemv)(const CBLAS_LAYOUT layout,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void  *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char TA;
#ifdef F77_CHAR
   F77_CHAR F77_TA;
#else
   #define F77_TA &TA
#endif
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX;
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incx
   #define F77_incY incY
#endif

   CBLAS_INT n, i=0;
   const double *xx= (const double *)X, *alp= (const double *)alpha, *bet = (const double *)beta;
   double ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   double *x, *y, *st=0, *tx;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(double*));
   memcpy(&y,&Y,sizeof(double*));

   CBLAS_CallFromC = 1;

   if (layout == CblasColMajor)
   {
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zgemv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      F77_zgemv(F77_TA, &F77_M, &F77_N, alpha, A, &F77_lda, X, &F77_incX,
                beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         ALPHA[0]= *alp;
         ALPHA[1]= -alp[1];
         BETA[0]= *bet;
         BETA[1]= -bet[1];
         TA = 'N';
         if (M > 0)
         {
            n = M << 1;
            x = malloc(n*sizeof(double));
            tx = x;
            if( incX > 0 ) {
               i = incX << 1 ;
               tincx = 2;
               st= x+n;
            } else {
               i = incX *(-2);
               tincx = -2;
               st = x-2;
               x +=(n-2);
            }

            do
            {
               *x = *xx;
               x[1] = -xx[1];
               x += tincx ;
               xx += i;
            }
            while (x != st);
            x=tx;

            #ifdef F77_INT
               F77_incX = 1;
            #else
               incx = 1;
            #endif

            if(incY > 0)
               tincY = incY;
            else
               tincY = -incY;

            y++;

            if (N > 0)
            {
               i = tincY << 1;
               n = i * N ;
               st = y + n;
               do {
                  *y = -(*y);
                  y += i;
               } while(y != st);
               y -= n;
            }
         }
         else
            memcpy(&x,&X,sizeof(double*));
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zgemv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_TA = C2F_CHAR(&TA);
      #endif
      if (TransA == CblasConjTrans)
         F77_zgemv(F77_TA, &F77_N, &F77_M, ALPHA, A, &F77_lda, x,
                &F77_incX, BETA, Y, &F77_incY);
      else
         F77_zgemv(F77_TA, &F77_N, &F77_M, alpha, A, &F77_lda, x,
                &F77_incX, beta, Y, &F77_incY);

      if (TransA == CblasConjTrans)
      {
         if (x != X) free(x);
         if (N > 0)
         {
            do
            {
               *y = -(*y);
               y += i;
            }
            while (y != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_zgemv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zgerc.c
 * The program is a C interface to zgerc.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zgerc)(const CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void *X, const CBLAS_INT incX,
                 const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incy = incY;
   #define F77_M M
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incy
   #define F77_lda lda
#endif

   CBLAS_INT n, i, tincy;
   double *y,  *yy, *ty, *st;

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&y,&Y,sizeof(double*));
   memcpy(&yy,&Y,sizeof(double*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      F77_zgerc( &F77_M, &F77_N, alpha, X, &F77_incX, Y, &F77_incY, A,
                      &F77_lda);
   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (N > 0)
      {
         n = N << 1;
         y = malloc(n*sizeof(double));

         ty = y;
         if( incY > 0 ) {
            i = incY << 1;
            tincy = 2;
            st= y+n;
         } else {
            i = incY *(-2);
            tincy = -2;
            st = y-2;
            y +=(n-2);
         }
         do
         {
            *y = (double) *yy;
            y[1] = -yy[1];
            y += tincy ;
            yy += i;
         }
         while (y != st);
         y = ty;

         #ifdef F77_INT
            F77_incY = 1;
         #else
            incy = 1;
         #endif
      }
      else
          memcpy(&y,&Y,sizeof(double*));

      F77_zgeru( &F77_N, &F77_M, alpha, y, &F77_incY, X, &F77_incX, A,
                      &F77_lda);
      if(Y!=y)
         free(y);

   } else API_SUFFIX(cblas_xerbla)(1, "cblas_zgerc", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zgeru.c
 * The program is a C interface to zgeru.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zgeru)(const CBLAS_LAYOUT layout, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void *X, const CBLAS_INT incX,
                 const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   #define F77_M M
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
   #define F77_lda lda
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if (layout == CblasColMajor)
   {
      F77_zgeru( &F77_M, &F77_N, alpha, X, &F77_incX, Y, &F77_incY, A,
                      &F77_lda);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      F77_zgeru( &F77_N, &F77_M, alpha, Y, &F77_incY, X, &F77_incX, A,
                      &F77_lda);
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_zgeru", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zhbmv.c
 * The program is a C interface to zhbmv
 *
 * Keita Teranishi  5/18/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void API_SUFFIX(cblas_zhbmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo,const CBLAS_INT N,const CBLAS_INT K,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void  *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX;
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incx
   #define F77_incY incY
#endif
   CBLAS_INT n, i=0;
   const double *xx= (const double *)X, *alp= (const double *)alpha, *bet = (const double *)beta;
   double ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   double *x, *y, *st=0, *tx;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(double*));
   memcpy(&y,&Y,sizeof(double*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhbmv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_zhbmv(F77_UL, &F77_N, &F77_K, alpha, A, &F77_lda, X,
                     &F77_incX, beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      ALPHA[0]= *alp;
      ALPHA[1]= -alp[1];
      BETA[0]= *bet;
      BETA[1]= -bet[1];

      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(double));

         tx = x;
         if( incX > 0 ) {
           i = incX << 1 ;
           tincx = 2;
           st= x+n;
         } else {
           i = incX *(-2);
           tincx = -2;
           st = x-2;
           x +=(n-2);
         }

         do
         {
           *x = *xx;
           x[1] = -xx[1];
           x += tincx ;
           xx += i;
         }
         while (x != st);
         x=tx;


         #ifdef F77_INT
            F77_incX = 1;
         #else
            incx = 1;
         #endif

         if(incY > 0)
           tincY = incY;
         else
           tincY = -incY;
         y++;

         i = tincY << 1;
         n = i * N ;
         st = y + n;
         do {
            *y = -(*y);
            y += i;
         } while(y != st);
         y -= n;
      }  else
        memcpy(&x,&X,sizeof(double*));

      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_zhbmv(F77_UL, &F77_N, &F77_K, ALPHA,
                     A ,&F77_lda, x,&F77_incX, BETA, Y, &F77_incY);
   }
   else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_zhbmv","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if ( layout == CblasRowMajor )
   {
      RowMajorStrg = 1;
      if(X!=x)
         free(x);
      if (N > 0)
      {
         do
         {
            *y = -(*y);
            y += i;
         }
         while (y != st);
      }
   }
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_zhemm.c
 * This program is a C interface to zhemm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zhemm)(const CBLAS_LAYOUT layout, const  CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void *A, const CBLAS_INT lda,
                 const void *B, const CBLAS_INT ldb, const void *beta,
                 void *C, const CBLAS_INT ldc)
{
   char SD, UL;
#ifdef F77_CHAR
   F77_CHAR F77_SD, F77_UL;
#else
   #define F77_SD &SD
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhemm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zhemm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_zhemm(F77_SD, F77_UL, &F77_M, &F77_N, alpha, A, &F77_lda,
                     B, &F77_ldb, beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhemm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zhemm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_zhemm(F77_SD, F77_UL, &F77_N, &F77_M, alpha, A,
                 &F77_lda, B, &F77_ldb, beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_zhemm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zhemv.c
 * The program is a C interface to zhemv
 *
 * Keita Teranishi  5/18/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zhemv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo, const CBLAS_INT N,
                 const void *alpha, const void *A, const CBLAS_INT lda,
                 const void *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX;
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incx
   #define F77_incY incY
#endif
   CBLAS_INT n, i=0;
   const double *xx= (const double *)X, *alp= (const double *)alpha, *bet = (const double *)beta;
   double ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   double *x, *y, *st=0, *tx;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(double*));
   memcpy(&y,&Y,sizeof(double*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhemv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_zhemv(F77_UL, &F77_N, alpha, A, &F77_lda, X, &F77_incX,
                beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      ALPHA[0]= *alp;
      ALPHA[1]= -alp[1];
      BETA[0]= *bet;
      BETA[1]= -bet[1];

      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(double));

         tx = x;
         if( incX > 0 ) {
           i = incX << 1 ;
           tincx = 2;
           st= x+n;
         } else {
           i = incX *(-2);
           tincx = -2;
           st = x-2;
           x +=(n-2);
         }

         do
         {
           *x = *xx;
           x[1] = -xx[1];
           x += tincx ;
           xx += i;
         }
         while (x != st);
         x=tx;


         #ifdef F77_INT
            F77_incX = 1;
         #else
            incx = 1;
         #endif

         if(incY > 0)
           tincY = incY;
         else
           tincY = -incY;
         y++;

         i = tincY << 1;
         n = i * N ;
         st = y + n;
         do {
            *y = -(*y);
            y += i;
         } while(y != st);
         y -= n;
      }  else
         memcpy(&x,&X,sizeof(double*));



      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhemv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_zhemv(F77_UL, &F77_N, ALPHA, A, &F77_lda, x, &F77_incX,
                BETA, Y, &F77_incY);
   }
   else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_zhemv","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if ( layout == CblasRowMajor )
   {
      RowMajorStrg = 1;
      if ( X != x )
         free(x);
      if (N > 0)
      {
         do
         {
            *y = -(*y);
            y += i;
         }
         while (y != st);
     }
   }
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zher2.c
 * The program is a C interface to zher2.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zher2)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_INT N, const void *alpha, const void *X, const CBLAS_INT incX,
                 const void *Y, const CBLAS_INT incY, void *A, const CBLAS_INT lda)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX, incy = incY;
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incx
   #define F77_incY incy
#endif
   CBLAS_INT n, i, j, tincx, tincy;
   double *x, *xx, *y,
         *yy, *tx, *ty, *stx, *sty;

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;


   memcpy(&x,&X,sizeof(double*));
   memcpy(&xx,&X,sizeof(double*));
   memcpy(&y,&Y,sizeof(double*));
   memcpy(&yy,&Y,sizeof(double*));


   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zher2", "Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_zher2(F77_UL, &F77_N, alpha, X, &F77_incX,
                                            Y, &F77_incY, A, &F77_lda);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zher2", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(double));
         y = malloc(n*sizeof(double));
         tx = x;
         ty = y;
         if( incX > 0 ) {
            i = incX << 1 ;
            tincx = 2;
            stx= x+n;
         } else {
            i = incX *(-2);
            tincx = -2;
            stx = x-2;
            x +=(n-2);
         }

         if( incY > 0 ) {
            j = incY << 1;
            tincy = 2;
            sty= y+n;
         } else {
            j = incY *(-2);
            tincy = -2;
            sty = y-2;
            y +=(n-2);
         }

         do
         {
            *x = *xx;
            x[1] = -xx[1];
            x += tincx ;
            xx += i;
         }
         while (x != stx);

         do
         {
            *y = *yy;
            y[1] = -yy[1];
            y += tincy ;
            yy += j;
         }
         while (y != sty);

         x=tx;
         y=ty;

         #ifdef F77_INT
            F77_incX = 1;
            F77_incY = 1;
         #else
            incx = 1;
            incy = 1;
         #endif
      }  else
      {

        memcpy(&x,&X,sizeof(double*));
        memcpy(&y,&Y,sizeof(double*));
      }
      F77_zher2(F77_UL, &F77_N, alpha, y, &F77_incY, x,
                                      &F77_incX, A, &F77_lda);
   }
   else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_zher2", "Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if(X!=x)
      free(x);
   if(Y!=y)
      free(y);

   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_zher2k.c
 * This program is a C interface to zher2k.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zher2k)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                  const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                  const void *alpha, const void *A, const CBLAS_INT lda,
                  const void *B, const CBLAS_INT ldb, const double beta,
                  void *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   double ALPHA[2];
   const double *alp=(const double *)alpha;

   CBLAS_CallFromC = 1;
   RowMajorStrg = 0;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zher2k", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zher2k", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_zher2k(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zher2k", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zher2k", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      ALPHA[0]= *alp;
      ALPHA[1]= -alp[1];
      F77_zher2k(F77_UL,F77_TR, &F77_N, &F77_K, ALPHA, A, &F77_lda, B, &F77_ldb, &beta, C, &F77_ldc);
   } else  API_SUFFIX(cblas_xerbla)(1, "cblas_zher2k", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zher.c
 * The program is a C interface to zher.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zher)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const double alpha, const void *X, const CBLAS_INT incX
                ,void *A, const CBLAS_INT lda)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   CBLAS_INT incx = incX;
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incx
#endif
   CBLAS_INT n, i, tincx;
   double *x, *xx, *tx, *st;

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;


   memcpy(&x,&X,sizeof(double*));
   memcpy(&xx,&X,sizeof(double*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zher","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_zher(F77_UL, &F77_N, &alpha, X, &F77_incX, A, &F77_lda);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zher","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(double));
         tx = x;
         if( incX > 0 ) {
            i = incX << 1 ;
            tincx = 2;
            st= x+n;
         } else {
            i = incX *(-2);
            tincx = -2;
            st = x-2;
            x +=(n-2);
         }
         do
         {
            *x = *xx;
            x[1] = -xx[1];
            x += tincx ;
            xx += i;
         }
         while (x != st);
         x=tx;

         #ifdef F77_INT
           F77_incX = 1;
         #else
           incx = 1;
         #endif
      }
      else
          memcpy(&x,&X,sizeof(double*));
      F77_zher(F77_UL, &F77_N, &alpha, x, &F77_incX, A, &F77_lda);
   } else API_SUFFIX(cblas_xerbla)(1, "cblas_zher", "Illegal layout setting, %d\n", layout);
   if(X!=x)
      free(x);

   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_zherk.c
 * This program is a C interface to zherk.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zherk)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                 const double alpha, const void *A, const CBLAS_INT lda,
                 const double beta, void *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zherk", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zherk", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_zherk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda,
                     &beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zherk", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zherk", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_zherk(F77_UL, F77_TR, &F77_N, &F77_K, &alpha, A, &F77_lda,
                &beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_zherk", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zhpmv.c
 * The program is a C interface of zhpmv
 *
 * Keita Teranishi  5/18/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zhpmv)(const CBLAS_LAYOUT layout,
                 const CBLAS_UPLO Uplo,const CBLAS_INT N,
                 const void *alpha, const void  *AP,
                 const void  *X, const CBLAS_INT incX, const void *beta,
                 void  *Y, const CBLAS_INT incY)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX;
   #define F77_N N
   #define F77_incX incx
   #define F77_incY incY
#endif
   CBLAS_INT n, i=0;
   const double *xx= (const double *)X, *alp= (const double *)alpha, *bet = (const double *)beta;
   double ALPHA[2],BETA[2];
   CBLAS_INT tincY, tincx;
   double *x, *y, *st=0, *tx;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(double*));
   memcpy(&y,&Y,sizeof(double*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhpmv","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      F77_zhpmv(F77_UL, &F77_N, alpha, AP, X,
                     &F77_incX, beta, Y, &F77_incY);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      ALPHA[0]= *alp;
      ALPHA[1]= -alp[1];
      BETA[0]= *bet;
      BETA[1]= -bet[1];

      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(double));

         tx = x;
         if( incX > 0 ) {
           i = incX << 1;
           tincx = 2;
           st= x+n;
         } else {
           i = incX *(-2);
           tincx = -2;
           st = x-2;
           x +=(n-2);
         }

         do
         {
           *x = *xx;
           x[1] = -xx[1];
           x += tincx ;
           xx += i;
         }
         while (x != st);
         x=tx;


         #ifdef F77_INT
            F77_incX = 1;
         #else
            incx = 1;
         #endif

         if(incY > 0)
           tincY = incY;
         else
           tincY = -incY;
         y++;

         i = tincY << 1;
         n = i * N ;
         st = y + n;
         do {
            *y = -(*y);
            y += i;
         } while(y != st);
         y -= n;
      }  else
        memcpy(&x,&X,sizeof(double*));

      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhpmv","Illegal Uplo setting, %d\n", Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_zhpmv(F77_UL, &F77_N, ALPHA,
                     AP, x, &F77_incX, BETA, Y, &F77_incY);
   }
   else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_zhpmv","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if ( layout == CblasRowMajor )
   {
      RowMajorStrg = 1;
      if(X!=x)
         free(x);
      if (N > 0)
      {
         do
         {
            *y = -(*y);
            y += i;
         }
         while (y != st);
     }
  }

   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zhpr2.c
 * The program is a C interface to zhpr2.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zhpr2)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                      const CBLAS_INT N,const void *alpha, const void *X,
                      const CBLAS_INT incX,const void *Y, const CBLAS_INT incY, void *Ap)

{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N,  F77_incX=incX, F77_incY=incY;
#else
   CBLAS_INT incx = incX, incy = incY;
   #define F77_N N
   #define F77_incX incx
   #define F77_incY incy
#endif
   CBLAS_INT n, i, j;
   double *x, *xx, *y,
         *yy, *stx, *sty;

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(double*));
   memcpy(&xx,&X,sizeof(double*));
   memcpy(&y,&Y,sizeof(double*));
   memcpy(&yy,&Y,sizeof(double*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhpr2","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_zhpr2(F77_UL, &F77_N, alpha, X, &F77_incX, Y, &F77_incY, Ap);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhpr2","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(double));
         y = malloc(n*sizeof(double));
         stx = x + n;
         sty = y + n;
         if( incX > 0 )
            i = incX << 1;
         else
            i = incX *(-2);

         if( incY > 0 )
            j = incY << 1;
         else
            j = incY *(-2);
         do
         {
            *x = *xx;
            x[1] = -xx[1];
            x += 2;
            xx += i;
         } while (x != stx);
         do
         {
            *y = *yy;
            y[1] = -yy[1];
            y += 2;
            yy += j;
         }
         while (y != sty);
         x -= n;
         y -= n;

         #ifdef F77_INT
            if(incX > 0 )
               F77_incX = 1;
            else
               F77_incX = -1;

            if(incY > 0 )
               F77_incY = 1;
            else
               F77_incY = -1;

         #else
            if(incX > 0 )
               incx = 1;
            else
               incx = -1;

            if(incY > 0 )
               incy = 1;
            else
               incy = -1;
         #endif

      }  else
      {

        memcpy(&x,&X,sizeof(double*));
        memcpy(&y,&Y,sizeof(double*));
      }
      F77_zhpr2(F77_UL, &F77_N, alpha, y, &F77_incY, x, &F77_incX, Ap);
   }
   else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_zhpr2","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if(X!=x)
      free(x);
   if(Y!=y)
      free(y);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zhpr.c
 * The program is a C interface to zhpr.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zhpr)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                const CBLAS_INT N, const double alpha, const void *X,
                const CBLAS_INT incX, void *A)
{
   char UL;
#ifdef F77_CHAR
   F77_CHAR F77_UL;
#else
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   CBLAS_INT incx = incX;
   #define F77_N N
   #define F77_incX incx
#endif
   CBLAS_INT n, i, tincx;
   double *x, *xx, *tx, *st;

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   memcpy(&x,&X,sizeof(double*));
   memcpy(&xx,&X,sizeof(double*));

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasLower) UL = 'L';
      else if (Uplo == CblasUpper) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhpr","Illegal Uplo setting, %d\n",Uplo );
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif

      F77_zhpr(F77_UL, &F77_N, &alpha, X, &F77_incX, A);

   }  else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zhpr","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
      #endif
      if (N > 0)
      {
         n = N << 1;
         x = malloc(n*sizeof(double));
         tx = x;
         if( incX > 0 ) {
            i = incX << 1;
            tincx = 2;
            st= x+n;
         } else {
            i = incX *(-2);
            tincx = -2;
            st = x-2;
            x +=(n-2);
         }
         do
         {
            *x = *xx;
            x[1] = -xx[1];
            x += tincx ;
            xx += i;
         }
         while (x != st);
         x=tx;
         #ifdef F77_INT
            F77_incX = 1;
         #else
            incx = 1;
         #endif
      }
      else
          memcpy(&x,&X,sizeof(double*));

      F77_zhpr(F77_UL, &F77_N, &alpha, x, &F77_incX, A);

   } else
   {
      API_SUFFIX(cblas_xerbla)(1, "cblas_zhpr","Illegal layout setting, %d\n", layout);
      CBLAS_CallFromC = 0;
      RowMajorStrg = 0;
      return;
   }
   if(X!=x)
     free(x);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_zrotg.c
 *
 * The program is a C interface to zrotg.
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zrotg)(void *a, void *b, double *c, void *s)
{
   F77_zrotg(a,b,c,s);
}


/*
 * cblas_zscal.c
 *
 * The program is a C interface to zscal.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zscal)( const CBLAS_INT N, const void *alpha, void *X,
                       const CBLAS_INT incX)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   F77_zscal( &F77_N, alpha, X, &F77_incX);
}

/*
 * cblas_zswap.c
 *
 * The program is a C interface to zswap.
 *
 * Written by Keita Teranishi.  2/11/1998
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zswap)( const CBLAS_INT N, void  *X, const CBLAS_INT incX, void  *Y,
                       const CBLAS_INT incY)
{
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX, F77_incY=incY;
#else
   #define F77_N N
   #define F77_incX incX
   #define F77_incY incY
#endif
   F77_zswap( &F77_N, X, &F77_incX, Y, &F77_incY);
}

/*
 *
 * cblas_zsymm.c
 * This program is a C interface to zsymm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zsymm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void  *B, const CBLAS_INT ldb, const void *beta,
                 void  *C, const CBLAS_INT ldc)
{
   char SD, UL;
#ifdef F77_CHAR
   F77_CHAR F77_SD, F77_UL;
#else
   #define F77_SD &SD
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zsymm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zsymm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_zsymm(F77_SD, F77_UL, &F77_M, &F77_N, alpha, A, &F77_lda,
                      B, &F77_ldb, beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zsymm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zsymm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_SD = C2F_CHAR(&SD);
      #endif

      F77_zsymm(F77_SD, F77_UL, &F77_N, &F77_M, alpha, A, &F77_lda,
                     B, &F77_ldb, beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_zsymm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_zsyr2k.c
 * This program is a C interface to zsyr2k.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zsyr2k)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                  const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                  const void *alpha, const void  *A, const CBLAS_INT lda,
                  const void  *B, const CBLAS_INT ldb, const void *beta,
                  void  *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda, F77_ldb=ldb;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldb ldb
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zsyr2k", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zsyr2k", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }


      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_zsyr2k(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda,
                      B, &F77_ldb, beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zsyr2k", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='T';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zsyr2k", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_zsyr2k(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda, B, &F77_ldb, beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_zsyr2k", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_zsyrk.c
 * This program is a C interface to zsyrk.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_zsyrk)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE Trans, const CBLAS_INT N, const CBLAS_INT K,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 const void *beta, void  *C, const CBLAS_INT ldc)
{
   char UL, TR;
#ifdef F77_CHAR
   F77_CHAR F77_TR, F77_UL;
#else
   #define F77_TR &TR
   #define F77_UL &UL
#endif

#ifdef F77_INT
   F77_INT F77_N=N, F77_K=K, F77_lda=lda;
   F77_INT F77_ldc=ldc;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_ldc ldc
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_zsyrk", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Trans == CblasTrans) TR ='T';
      else if ( Trans == CblasConjTrans ) TR='C';
      else if ( Trans == CblasNoTrans )   TR='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zsyrk", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }


      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_zsyrk(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda,
                beta, C, &F77_ldc);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zsyrk", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Trans == CblasTrans) TR ='N';
      else if ( Trans == CblasConjTrans ) TR='N';
      else if ( Trans == CblasNoTrans )   TR='T';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_zsyrk", "Illegal Trans setting, %d\n", Trans);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TR = C2F_CHAR(&TR);
      #endif

      F77_zsyrk(F77_UL, F77_TR, &F77_N, &F77_K, alpha, A, &F77_lda,
                     beta, C, &F77_ldc);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_zsyrk", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ztbmv.c
 * The program is a C interface to ztbmv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ztbmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const CBLAS_INT K, const void  *A, const CBLAS_INT lda,
                 void  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_K=K, F77_incX=incX;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   double *st=0, *x=(double *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztbmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ztbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if(incX > 0)
               tincX = incX;
            else
               tincX = -incX;
            i = tincX << 1;
            n = i * N;
            x++;
            st = x + n;
            do
            {
               *x = -(*x);
               x+= i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztbmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztbmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ztbmv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);

      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ztbmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ztbsv.c
 * The program is a C interface to ztbsv.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ztbsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const CBLAS_INT K, const void  *A, const CBLAS_INT lda,
                 void  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_K=K, F77_incX=incX;
#else
   #define F77_N N
   #define F77_K K
   #define F77_lda lda
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   double *st=0,*x=(double *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztbsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztbsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztbsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ztbsv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztbsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if ( incX > 0 )
               tincX = incX;
            else
               tincX = -incX;

            n = N*2*(tincX);

            x++;

            st=x+n;

            i = tincX << 1;
            do
            {
               *x = -(*x);
               x+=i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztbsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztbsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ztbsv( F77_UL, F77_TA, F77_DI, &F77_N, &F77_K, A, &F77_lda, X,
                      &F77_incX);

      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x+= i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ztbsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ztpmv.c
 * The program is a C interface to ztpmv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ztpmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const void  *Ap, void  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   double *st=0,*x=(double *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztpmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztpmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztpmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ztpmv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X, &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztpmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if(incX > 0)
               tincX = incX;
            else
               tincX = -incX;
            i = tincX << 1;
            n = i * N;
            x++;
            st = x + n;
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztpmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztpmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ztpmv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X,&F77_incX);
      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ztpmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ztpsv.c
 * The program is a C interface to ztpsv.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ztpsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const void  *Ap, void  *X, const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_incX=incX;
#else
   #define F77_N N
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   double *st=0, *x=(double*)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztpsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztpsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztpsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ztpsv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X, &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztpsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if ( incX > 0 )
               tincX = incX;
            else
               tincX = -incX;

            n = N*2*(tincX);

            x++;

            st=x+n;

            i = tincX << 1;
            do
            {
               *x = -(*x);
               x+=i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztpsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztpsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ztpsv( F77_UL, F77_TA, F77_DI, &F77_N, Ap, X,&F77_incX);

      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ztpsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_ztrmm.c
 * This program is a C interface to ztrmm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ztrmm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const  CBLAS_TRANSPOSE TransA,
                 const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 void  *B, const CBLAS_INT ldb)
{
   char UL, TA, SD, DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_SD, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_SD &SD
   #define F77_DI &DI
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {
      if( Side == CblasRight ) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztrmm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if( Uplo == CblasUpper ) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztrmm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans ) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztrmm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_ztrmm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ztrmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, alpha, A, &F77_lda, B, &F77_ldb);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if( Side == CblasRight ) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztrmm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper ) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztrmm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans ) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztrmm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_ztrmm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ztrmm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, alpha, A, &F77_lda, B, &F77_ldb);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_ztrmm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ztrmv.c
 * The program is a C interface to ztrmv.
 *
 * Keita Teranishi  5/20/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ztrmv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const void  *A, const CBLAS_INT lda,
                 void  *X, const CBLAS_INT incX)

{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   double *st=0,*x=(double *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztrmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztrmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztrmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ztrmv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztrmv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if(incX > 0)
               tincX = incX;
            else
               tincX = -incX;
            i = tincX << 1;
            n = i * N;
            x++;
            st = x + n;
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztrmv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztrmv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
         F77_ztrmv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ztrmv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 *
 * cblas_ztrsm.c
 * This program is a C interface to ztrsm.
 * Written by Keita Teranishi
 * 4/8/1998
 *
 */

#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ztrsm)(const CBLAS_LAYOUT layout, const CBLAS_SIDE Side,
                 const CBLAS_UPLO Uplo, const CBLAS_TRANSPOSE TransA,
                 const CBLAS_DIAG Diag, const CBLAS_INT M, const CBLAS_INT N,
                 const void *alpha, const void  *A, const CBLAS_INT lda,
                 void  *B, const CBLAS_INT ldb)
{
   char UL, TA, SD, DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_SD, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_SD &SD
   #define F77_DI &DI
#endif

#ifdef F77_INT
   F77_INT F77_M=M, F77_N=N, F77_lda=lda, F77_ldb=ldb;
#else
   #define F77_M M
   #define F77_N N
   #define F77_lda lda
   #define F77_ldb ldb
#endif

   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;
   CBLAS_CallFromC = 1;

   if( layout == CblasColMajor )
   {

      if( Side == CblasRight) SD='R';
      else if ( Side == CblasLeft ) SD='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztrsm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='U';
      else if ( Uplo == CblasLower ) UL='L';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztrsm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztrsm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_ztrsm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif

      F77_ztrsm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_M, &F77_N, alpha, A,
                &F77_lda, B, &F77_ldb);
   } else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;

      if( Side == CblasRight) SD='L';
      else if ( Side == CblasLeft ) SD='R';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztrsm", "Illegal Side setting, %d\n", Side);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Uplo == CblasUpper) UL='L';
      else if ( Uplo == CblasLower ) UL='U';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztrsm", "Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( TransA == CblasTrans) TA ='T';
      else if ( TransA == CblasConjTrans ) TA='C';
      else if ( TransA == CblasNoTrans )   TA='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztrsm", "Illegal Trans setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if( Diag == CblasUnit ) DI='U';
      else if ( Diag == CblasNonUnit ) DI='N';
      else
      {
         API_SUFFIX(cblas_xerbla)(5, "cblas_ztrsm", "Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_SD = C2F_CHAR(&SD);
         F77_DI = C2F_CHAR(&DI);
      #endif


      F77_ztrsm(F77_SD, F77_UL, F77_TA, F77_DI, &F77_N, &F77_M, alpha, A,
                &F77_lda, B, &F77_ldb);
   }
   else  API_SUFFIX(cblas_xerbla)(1, "cblas_ztrsm", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

/*
 * cblas_ztrsv.c
 * The program is a C interface to ztrsv.
 *
 * Keita Teranishi  3/23/98
 *
 */
#include "cblas.h"
#include "cblas_f77.h"
void API_SUFFIX(cblas_ztrsv)(const CBLAS_LAYOUT layout, const CBLAS_UPLO Uplo,
                 const CBLAS_TRANSPOSE TransA, const CBLAS_DIAG Diag,
                 const CBLAS_INT N, const void  *A, const CBLAS_INT lda, void  *X,
                 const CBLAS_INT incX)
{
   char TA;
   char UL;
   char DI;
#ifdef F77_CHAR
   F77_CHAR F77_TA, F77_UL, F77_DI;
#else
   #define F77_TA &TA
   #define F77_UL &UL
   #define F77_DI &DI
#endif
#ifdef F77_INT
   F77_INT F77_N=N, F77_lda=lda, F77_incX=incX;
#else
   #define F77_N N
   #define F77_lda lda
   #define F77_incX incX
#endif
   CBLAS_INT n, i=0, tincX;
   double *st=0,*x=(double *)X;
   extern int CBLAS_CallFromC;
   extern int RowMajorStrg;
   RowMajorStrg = 0;

   CBLAS_CallFromC = 1;
   if (layout == CblasColMajor)
   {
      if (Uplo == CblasUpper) UL = 'U';
      else if (Uplo == CblasLower) UL = 'L';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztrsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (TransA == CblasNoTrans) TA = 'N';
      else if (TransA == CblasTrans) TA = 'T';
      else if (TransA == CblasConjTrans) TA = 'C';
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztrsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztrsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ztrsv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
   }
   else if (layout == CblasRowMajor)
   {
      RowMajorStrg = 1;
      if (Uplo == CblasUpper) UL = 'L';
      else if (Uplo == CblasLower) UL = 'U';
      else
      {
         API_SUFFIX(cblas_xerbla)(2, "cblas_ztrsv","Illegal Uplo setting, %d\n", Uplo);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (TransA == CblasNoTrans) TA = 'T';
      else if (TransA == CblasTrans) TA = 'N';
      else if (TransA == CblasConjTrans)
      {
         TA = 'N';
         if ( N > 0)
         {
            if ( incX > 0 )
               tincX = incX;
            else
               tincX = -incX;

            n = N*2*(tincX);
            x++;
            st=x+n;
            i = tincX << 1;
            do
            {
               *x = -(*x);
               x+=i;
            }
            while (x != st);
            x -= n;
         }
      }
      else
      {
         API_SUFFIX(cblas_xerbla)(3, "cblas_ztrsv","Illegal TransA setting, %d\n", TransA);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }

      if (Diag == CblasUnit) DI = 'U';
      else if (Diag == CblasNonUnit) DI = 'N';
      else
      {
         API_SUFFIX(cblas_xerbla)(4, "cblas_ztrsv","Illegal Diag setting, %d\n", Diag);
         CBLAS_CallFromC = 0;
         RowMajorStrg = 0;
         return;
      }
      #ifdef F77_CHAR
         F77_UL = C2F_CHAR(&UL);
         F77_TA = C2F_CHAR(&TA);
         F77_DI = C2F_CHAR(&DI);
      #endif
      F77_ztrsv( F77_UL, F77_TA, F77_DI, &F77_N, A, &F77_lda, X,
                      &F77_incX);
      if (TransA == CblasConjTrans)
      {
         if (N > 0)
         {
            do
            {
               *x = -(*x);
               x += i;
            }
            while (x != st);
         }
      }
   }
   else API_SUFFIX(cblas_xerbla)(1, "cblas_ztrsv", "Illegal layout setting, %d\n", layout);
   CBLAS_CallFromC = 0;
   RowMajorStrg = 0;
   return;
}

#include <stdio.h>
#include <ctype.h>
#include "cblas.h"
#include "cblas_f77.h"

#define XerblaStrLen 6
#define XerblaStrLen1 7




void
#ifdef HAS_ATTRIBUTE_WEAK_SUPPORT
__attribute__((weak))
#endif
F77_xerbla_base
#ifdef F77_CHAR
(F77_CHAR F77_srname, void *vinfo
#else
(char *srname, void *vinfo
#endif
#ifdef BLAS_FORTRAN_STRLEN_END
, FORTRAN_STRLEN len
#endif
)
{
#ifdef F77_CHAR
   char *srname;
#endif

   char rout[] = {'c','b','l','a','s','_','\0','\0','\0','\0','\0','\0','\0'};

   int *info=vinfo;
   int i;

   extern int CBLAS_CallFromC;

#ifdef F77_CHAR
   srname = F2C_STR(F77_srname, XerblaStrLen);
#endif

   if (CBLAS_CallFromC)
   {
      for(i=0; i != XerblaStrLen; i++) rout[i+6] = tolower(srname[i]);
      rout[XerblaStrLen+6] = '\0';
      API_SUFFIX(cblas_xerbla)(*info+1,rout,"");
   }
   else
   {
      fprintf(stderr, "Parameter %d to routine %s was incorrect\n",
              *info, srname);
   }
}

