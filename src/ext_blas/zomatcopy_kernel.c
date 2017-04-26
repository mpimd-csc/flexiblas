
void FNAME( char* ORDER, char* TRANS, int64_t *rows, int64_t *cols, FLOAT *alpha, FLOAT *a, int64_t *lda, FLOAT *b, int64_t *ldb)
{

	char Order, Trans;
	int64_t info = -1;
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
    		xerbla64_( ENAME , &info, sizeof(ENAME));
    		return;
  	}

	ALPHA = *alpha; 

	if ( Order == 'C' )
	{

		if ( Trans == 'N' )
		{
			int64_t i,j;
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
			int64_t i,j;
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
			int64_t i,j;
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
			int64_t i,j;
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
		int64_t i,j;
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


