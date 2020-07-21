/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a
 * combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
 * General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */




void FC_GLOBAL(xerbla,XERBLA)(char *name, Int *info, Int len);

void FNAME( char* ORDER, char* TRANS, Int *rows, Int *cols, FLOAT *alpha, FLOAT *a, Int *lda, FLOAT *b, Int *ldb)
{

	char Order, Trans;
	Int info = -1;
	FLOAT ALPHA;

	Order = toupper(*ORDER);
	Trans = toupper(*TRANS);

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
    		FC_GLOBAL(xerbla,XERBLA)( ENAME , &info, strlen(ENAME));
    		return;
  	}

	ALPHA = *alpha;

	if ( Order == 'C' )
	{

		if ( Trans == 'N' )
		{
			Int i,j;
			FLOAT *aptr,*bptr;

			aptr = a;
			bptr = b;

			for ( i=0; i<*cols ; i++ )
			{
				for(j=0; j<*rows; j++)
				{
					bptr[j]   = ALPHA * aptr[j];
				}
				aptr += (*lda);
				bptr += (*ldb);
			}
		}
		if ( Trans == 'R' )
		{
			Int i,j;
			FLOAT *aptr,*bptr;

			aptr = a;
			bptr = b;

			for ( i=0; i<*cols ; i++ )
			{
				for(j=0; j<*rows; j++)
				{
					bptr[j]   =   ALPHA * conj(aptr[j]);
				}
				aptr += (*lda);
				bptr += (*ldb);
			}
		}
		if ( Trans == 'T' )
		{
			Int i,j;
			FLOAT *aptr,*bptr;
			aptr = a;
			bptr = b;

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
		if ( Trans == 'C' )
		{
			Int i,j;
			FLOAT *aptr,*bptr;
			aptr = a;
			bptr = b;

			for ( i=0; i<*cols ; i++ )
			{
				bptr = &b[i];
				for(j=0; j<*rows; j++)
				{
					bptr[j*(*ldb)] = ALPHA * conj(aptr[j]);
				}
				aptr += (*lda);
			}


		}

	}
	else
	{
		Int i,j;
		FLOAT *aptr,*bptr;
		aptr = a;
		bptr = b;


		if ( Trans == 'N' )
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
		if ( Trans == 'R' )
		{
			for ( i=0; i<*rows ; i++ )
			{
				for(j=0; j<*cols; j++)
				{
					bptr[j] = ALPHA * conj(aptr[j]);
				}
				aptr += (*lda);
				bptr += (*ldb);
			}
		}
		if ( Trans == 'T' )
		{
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
		if ( Trans == 'C' )
		{
			aptr = a;

			for ( i=0; i<*rows ; i++ )
			{
				bptr = &b[i];
				for(j=0; j<*cols; j++)
				{
					bptr[j*(*ldb)] = ALPHA * conj(aptr[j]);
				}
				aptr += (*lda);
			}

		}

	}

	return;

}


