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
 * Copyright (C) Martin Koehler, 2013-2022
 */





void FC_GLOBAL(somatcopy,SOMATCOPY)( char* ORDER, char* TRANS, Int *rows, Int *cols, float *alpha, float *a, Int *lda, float *b, Int *ldb);
void FC_GLOBAL(domatcopy,DOMATCOPY)( char* ORDER, char* TRANS, Int *rows, Int *cols, double *alpha, double *a, Int *lda, double *b, Int *ldb);
void FC_GLOBAL(comatcopy,COMATCOPY)( char* ORDER, char* TRANS, Int *rows, Int *cols, float complex *alpha, float complex *a, Int *lda, float complex *b, Int *ldb);
void FC_GLOBAL(zomatcopy,ZOMATCOPY)( char* ORDER, char* TRANS, Int *rows, Int *cols, double complex *alpha, double complex *a, Int *lda, double complex *b, Int *ldb);
void FC_GLOBAL(xerbla,XERBLA)(char *name, Int *code, Int len);

void FNAME( char* ORDER, char* TRANS, Int *rows, Int *cols, FLOAT *alpha, FLOAT *a, Int *lda, Int *ldb)
{

	char Order, Trans;
	Int info = -1;
	FLOAT *b;
	size_t msize;
	Int i,j;
	Int _rows, _cols, _lda, _ldb;
	FLOAT _alpha;
	FLOAT _temp;

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

	_rows = *rows;
	_cols = *cols;
	_lda  = *lda;
	_ldb  = *ldb;
	_alpha = *alpha;

	/* rows == cols and lda == ldb  */
	if ( _rows == _cols
	     && _lda == _ldb)
	{
		if (Trans == 'N' || Trans == 'R') {
			if (Order == 'C') {
				for (i = 0; i < _cols; i++) {
					for(j = 0; j < _rows; j++) {
						a[j+i*(_lda)] *= (_alpha);
					}
				}
			} else {
				for (j = 0; j < _rows; j++) {
					for (i = 0; i < _cols; i++) {
						a[j*_lda+i] *= _alpha;
					}
				}
			}
		} else {
			if (Order == 'C') {
				for (i = 0; i < _cols; i++) {
					a[i+i*_lda] *= _alpha;
					for (j = i+1; j < _rows; j++) {
						_temp = a[i+j*_lda];
						a[i+j*_lda] = _alpha * a[j+i*_lda];
						a[j+i*_lda] = _alpha * _temp;
					}
				}

			} else {
				for (i = 0; i < _rows; i++) {
					a[i+i*_lda] *= _alpha;
					for (j = i+1; j < _cols; j++) {
						_temp = a[i+j*_lda];
						a[i+j*_lda] = _alpha * a[j+i*_lda];
						a[j+i*_lda] = _alpha * _temp;
					}
				}


			}
		}
		return;
	}

	/* All other cases   */
	if ( _lda >  _ldb )
		msize = (_lda) * (_ldb)  * sizeof(FLOAT);
	else
		msize = (_ldb) * (_ldb)  * sizeof(FLOAT);

	b = malloc(msize);
	if ( b == NULL )
	{
		printf("Memory alloc failed\n");
		exit(1);
	}

	if ( Order == 'C')
	{
		if ( Trans == 'N' || Trans == 'R' )
		{
#ifndef _DOUBLE_PRECISION
			_temp = (FLOAT) 1.0;
			FC_GLOBAL(somatcopy,SOMATCOPY)("C","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(somatcopy,SOMATCOPY)("C","N",rows, cols, &_temp, b, ldb, a, ldb);
#else
			_temp = (FLOAT) 1.0;
			FC_GLOBAL(domatcopy,DOMATCOPY)("C","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(domatcopy,DOMATCOPY)("C","N",rows, cols, &_temp, b, ldb, a, ldb);
#endif
		}
		else
		{
#ifndef _DOUBLE_PRECISION
			_temp = (FLOAT) 1.0;
			FC_GLOBAL(somatcopy,SOMATCOPY)("C","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(somatcopy,SOMATCOPY)("C","N",rows, cols, &_temp, b, ldb, a, ldb);
#else
			_temp = (FLOAT) 1.0;
			FC_GLOBAL(domatcopy,DOMATCOPY)("C","T",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(domatcopy,DOMATCOPY)("C","N",rows, cols, &_temp, b, ldb, a, ldb);
#endif
		}
	}
	else
	{
		if ( Trans == 'N' || Trans == 'R' )
		{
#ifndef _DOUBLE_PRECISION
			_temp = (FLOAT) 1.0;
			FC_GLOBAL(somatcopy,SOMATCOPY)("R","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(somatcopy,SOMATCOPY)("R","N",rows, cols, &_temp, b, ldb, a, ldb);
#else
			_temp = (FLOAT) 1.0;
			FC_GLOBAL(domatcopy,DOMATCOPY)("R","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(domatcopy,DOMATCOPY)("R","N",rows, cols, &_temp, b, ldb, a, ldb);
#endif
		}
		else
		{
#ifndef _DOUBLE_PRECISION
			_temp = (FLOAT) 1.0;
			FC_GLOBAL(somatcopy,SOMATCOPY)("R","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(somatcopy,SOMATCOPY)("R","N",rows, cols, &_temp, b, ldb, a, ldb);
#else
			_temp = (FLOAT) 1.0;
			FC_GLOBAL(domatcopy,DOMATCOPY)("R","T",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(domatcopy,DOMATCOPY)("R","N",rows, cols, &_temp, b, ldb, a, ldb);
#endif
		}
	}

	free(b);
	return;

}


