void FC_GLOBAL(fsomatcopy,FSOMATCOPY)( char* ORDER, char* TRANS, Int *rows, Int *cols, float *alpha, float *a, Int *lda, float *b, Int *ldb); 
void FC_GLOBAL(fdomatcopy,FDOMATCOPY)( char* ORDER, char* TRANS, Int *rows, Int *cols, double *alpha, double *a, Int *lda, double *b, Int *ldb); 
void FC_GLOBAL(fcomatcopy,FCOMATCOPY)( char* ORDER, char* TRANS, Int *rows, Int *cols, float complex *alpha, float complex *a, Int *lda, float complex *b, Int *ldb); 
void FC_GLOBAL(fzomatcopy,FZOMATCOPY)( char* ORDER, char* TRANS, Int *rows, Int *cols, double complex *alpha, double complex *a, Int *lda, double complex *b, Int *ldb); 
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
			FC_GLOBAL(fsomatcopy,FSOMATCOPY)("C","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(fsomatcopy,FSOMATCOPY)("C","N",rows, cols, &_temp, b, ldb, a, ldb); 
#else
			_temp = (FLOAT) 1.0; 
			FC_GLOBAL(fdomatcopy,FDOMATCOPY)("C","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(fdomatcopy,FDOMATCOPY)("C","N",rows, cols, &_temp, b, ldb, a, ldb); 
#endif
		}
		else
		{
#ifndef _DOUBLE_PRECISION
			_temp = (FLOAT) 1.0; 
			FC_GLOBAL(fsomatcopy,FSOMATCOPY)("C","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(fsomatcopy,FSOMATCOPY)("C","N",rows, cols, &_temp, b, ldb, a, ldb); 
#else
			_temp = (FLOAT) 1.0; 
			FC_GLOBAL(fdomatcopy,FDOMATCOPY)("C","T",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(fdomatcopy,FDOMATCOPY)("C","N",rows, cols, &_temp, b, ldb, a, ldb); 
#endif
		}
	}
	else
	{
		if ( Trans == 'N' || Trans == 'R' )
		{
#ifndef _DOUBLE_PRECISION
			_temp = (FLOAT) 1.0; 
			FC_GLOBAL(fsomatcopy,FSOMATCOPY)("R","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(fsomatcopy,FSOMATCOPY)("R","N",rows, cols, &_temp, b, ldb, a, ldb); 
#else
			_temp = (FLOAT) 1.0; 
			FC_GLOBAL(fdomatcopy,FDOMATCOPY)("R","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(fdomatcopy,FDOMATCOPY)("R","N",rows, cols, &_temp, b, ldb, a, ldb); 
#endif
		}
		else
		{
#ifndef _DOUBLE_PRECISION
			_temp = (FLOAT) 1.0; 
			FC_GLOBAL(fsomatcopy,FSOMATCOPY)("R","N",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(fsomatcopy,FSOMATCOPY)("R","N",rows, cols, &_temp, b, ldb, a, ldb); 
#else
			_temp = (FLOAT) 1.0; 
			FC_GLOBAL(fdomatcopy,FDOMATCOPY)("R","T",rows, cols, alpha, a, lda, b, ldb );
			FC_GLOBAL(fdomatcopy,FDOMATCOPY)("R","N",rows, cols, &_temp, b, ldb, a, ldb); 
#endif
		}
	}

	free(b);
	return;

}

