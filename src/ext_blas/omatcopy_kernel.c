#include <ctype.h>

void FNAME ( char* ORDER, char* TRANS, Int *rows, Int *cols, FLOAT *alpha, FLOAT *a, Int *lda, FLOAT *b, Int *ldb)
{

	char Order, Trans;
	Int info = -1;

	Order = *ORDER;
	Trans = *TRANS;

	Order = toupper(Order);
	Trans = toupper(Trans);

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
    		xerbla_( ENAME , &info, sizeof(ENAME));
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


