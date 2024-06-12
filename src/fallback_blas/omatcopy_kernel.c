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

void FC_GLOBAL(xerbla,XERBLA)(char *name, Int *info, flexiblas_fortran_charlen_t len);

void FNAME ( char* ORDER, char* TRANS, Int *rows, Int *cols, FLOAT *alpha, FLOAT *a, Int *lda, FLOAT *b, Int *ldb, flexiblas_fortran_charlen_t len1, flexiblas_fortran_charlen_t len2)
{

    char Order, Trans;
    Int info = -1;

    Order = *ORDER;
    Trans = *TRANS;

    Order = toupper(Order);
    Trans = toupper(Trans);


    if ( len1 == 0 || len2 == 0 ) info = 0;
    if ( Order != 'C' && Order != 'R' ) {
        info = 1;
    } else if ( Trans != 'N' && Trans != 'R'
            && Trans != 'T'
            && Trans != 'C' ) {
        info = 2;
    } else if ( *rows <= 0 ) {
        info = 3;
    } else if ( *cols <= 0 ) {
        info = 4;
    } else if ( ( Order == 'C' && *lda < *rows ) || (Order == 'R' && *lda < *cols )){
        info = 7;
    }
    if ( Order == 'C')
    {
        if ( (Trans == 'N' || Trans == 'R')  &&  *ldb < *rows ) info = 9;
        if ( (Trans == 'T' || Trans == 'C')  &&  *ldb < *cols ) info = 9;
    }
    if ( Order == 'R')
    {
        if ( (Trans == 'N' || Trans == 'R' ) &&  *ldb < *cols ) info = 9;
        if ( (Trans == 'T' || Trans == 'C' ) &&  *ldb < *rows ) info = 9;
    }

    if (info >= 0) {
        FC_GLOBAL(xerbla,XERBLA) ( ENAME , &info, strlen(ENAME));
        return;
    }

    if ( Order == 'C' )
    {
        if ( Trans == 'N' || Trans == 'R' )
        {
            Int i,j;
            FLOAT *aptr,*bptr;
            FLOAT ALPHA;
            aptr = a;
            bptr = b;

            ALPHA = *alpha;

            if ( ALPHA == 0.0 )
            {
                for ( i=0; i< *cols ; i++ )
                {
                    for(j=0; j< *rows; j++)
                    {
                        bptr[j] = 0.0;
                    }
                    bptr += (*ldb);
                }
            }
            else if ( ALPHA == 1.0 )
            {
                for ( i=0; i< *cols ; i++ )
                {
                    for(j=0; j< *rows; j++)
                    {
                        bptr[j] = aptr[j];
                    }
                    aptr += (*lda);
                    bptr += (*ldb);
                }
            }
            else
            {
                for ( i=0; i< *cols ; i++ )
                {
                    for(j=0; j< *rows; j++)
                    {
                        bptr[j] = ALPHA * aptr[j];
                    }
                    aptr += (*lda);
                    bptr += (*ldb);
                }
            }
        }
        else
        {
            Int i,j;
            FLOAT *aptr,*bptr;
            FLOAT ALPHA;

            aptr = a;
            ALPHA = *alpha;

            if ( ALPHA == 0.0 )
            {
                for ( i=0; i< *cols ; i++ )
                {
                    bptr = &b[i];
                    for(j=0; j< *rows; j++)
                    {
                        bptr[j*(*ldb)] = 0.0;
                    }
                }
            }

            if ( ALPHA == 1.0 )
            {
                for ( i=0; i< *cols ; i++ )
                {
                    bptr = &b[i];
                    for(j=0; j< *rows; j++)
                    {
                        bptr[j*(*ldb)] = aptr[j];
                    }
                    aptr += (*lda);
                }
            }

            for ( i=0; i<*cols ; i++ )
            {
                bptr = &b[i];
                for(j=0; j<*rows; j++)
                {
                    bptr[j*(*ldb)] = ALPHA * aptr[j];
                }
                aptr += (*lda);
            }
        }
    }
    else
    {
        if ( Trans == 'N' || Trans == 'R'  )
        {
            Int i,j;
            FLOAT *aptr,*bptr;
            FLOAT ALPHA;

            ALPHA = * alpha;
            aptr = a;
            bptr = b;

            if ( ALPHA == 0.0 )
            {
                for ( i=0; i<*rows ; i++ )
                {
                    for(j=0; j<*cols; j++)
                    {
                        bptr[j] = 0.0;
                    }
                    bptr += (*ldb);
                }
            }
            else if ( ALPHA == 1.0 )
            {
                for ( i=0; i< *rows ; i++ )
                {
                    for(j=0; j<*cols; j++)
                    {
                        bptr[j] = aptr[j];
                    }
                    aptr += (*lda);
                    bptr += (*ldb);
                }
            }
            else
            {
                for ( i=0; i<*rows ; i++ )
                {
                    for(j=0; j<*cols; j++)
                    {
                        bptr[j] = ALPHA * aptr[j];
                    }
                    aptr += (*lda);
                    bptr += (*ldb);
                }
            }
        }
        else
        {
            Int i,j;
            FLOAT *aptr,*bptr;
            FLOAT ALPHA;
            ALPHA = *alpha;

            aptr = a;

            for ( i=0; i<*rows ; i++ )
            {
                bptr = &b[i];
                for(j=0; j<*cols; j++)
                {
                    bptr[j*(*ldb)] = ALPHA * aptr[j];
                }
                aptr += (*lda);
            }
        }
    }

    return;

}


