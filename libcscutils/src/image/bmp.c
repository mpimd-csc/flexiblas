/* 
* CSCUTILS - A collection of various software routines uses in CSC projects
* Copyright (C) 2015 Martin Koehler
* 
* This library is free software; you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published
* by the Free Software Foundation; either version 2.1 of the License, or
* (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public License
* along with this library; if not, see <http://www.gnu.org/licenses/>.
* 
*/ 

#include <stdio.h>
#include <stdlib.h>

#include "cscutils/image.h"
#include "cscutils/error_message.h"  

static int read_u_long_int   ( unsigned long int *u_long_int_val, FILE *filein );
static int read_u_short_int  ( unsigned short int *u_short_int_val, FILE *filein );
static int write_u_long_int  ( unsigned long int u_long_int_val, FILE *fileout );
static int write_u_short_int ( unsigned short int u_short_int_val, FILE *fileout );
static int csc_image_bmp_read_data ( FILE *filein, int xsize, int ysize, int bsize,
				   unsigned char *rarray, unsigned char *garray, unsigned char *barray );
static int csc_image_bmp_read_header ( FILE *filein, int *xsize, int *ysize, int *bsize, int *psize );
static int csc_image_bmp_read_palette ( FILE *filein, int psize );
static int csc_image_bmp_write_data ( FILE *fileout, int xsize, int ysize, int bsize,
					unsigned char *rarray, unsigned char *garray, unsigned char *barray );
static int csc_image_bmp_write_palette( FILE *fileout);
static int csc_image_bmp_write_header ( FILE *fileout, int xsize, int ysize, int bsize );



# define ERROR 1
# define SUCCESS 0
#define TRUE 1

static int byte_swap = TRUE;

int csc_image_bmp_read ( char *filein_name, csc_image_bmp *pic )   {
	FILE *filein;
	int   numbytes;
	int   psize;
	int   result;
	int 	xsize;
	int 	ysize;
	int 	bsize;
	/* Open the input file. */
	filein = fopen ( filein_name, "rb" );

	if ( filein == NULL ) {
		csc_error_message("csc_image_bmp_read - Could not open Input file %s\n", filein_name); 
		return ERROR;
	}

	/* Read the header. */
	result = csc_image_bmp_read_header ( filein, &xsize, &ysize, &bsize, &psize );

	if ( result == ERROR ) {
		csc_error_message("csc_image_bmp_read_header failed. \n"); 
		return ERROR;
	}
	/* Check for supported formats. */
	if ( bsize !=8 && bsize != 24)
	{
		csc_error_message("csc_image_bmp_read: bit size = %d is not supported\n", bsize);
		return ERROR;
	}

	if (bsize == 8)
		psize = 256;
	/* Read the palette. This is dummy read. */
	result = csc_image_bmp_read_palette ( filein, psize );

	if ( result == ERROR ) {
		csc_error_message("csc_image_bmp_read_palette failed.\n"); 
		return ERROR;
	}

	/*  Allocate storage. */
	numbytes = ( xsize ) * ( ysize ) * sizeof ( unsigned char );
	pic->red = ( unsigned char * ) malloc ( numbytes );
	if ( pic->red == NULL ) {
		csc_error_message("csc_image_bmp_read: Could not allocate data storage.\n" );
		return ERROR;
	}
	if (bsize == 24)
	{
		pic->green = ( unsigned char * ) malloc ( numbytes );
		if ( pic->green == NULL ) {
			csc_error_message("csc_image_bmp_read: Could not allocate data storage.\n" );
			return ERROR;
		}

		pic->blue = ( unsigned char * ) malloc ( numbytes );
		if ( pic->blue == NULL ) {
			csc_error_message("csc_image_bmp_read: Could not allocate data storage.\n" );
			return ERROR;
		}
	} 
	/* Read the data. */
	result = csc_image_bmp_read_data ( filein, xsize, ysize, bsize, pic->red, pic->green, pic->blue );
	pic->bpp = bsize;
	pic->height = ysize;
	pic->width  = xsize;

	if ( result == ERROR ) {
		csc_error_message("csc_image_bmp_read_data failed\n"); 
		return ERROR;
	}
	/* Close the file. */
	fclose ( filein );

	return SUCCESS;
}

static int csc_image_bmp_read_data ( FILE *filein, int xsize, int ysize, int bsize,
		unsigned char *rarray, unsigned char *garray, unsigned char *barray ) {
	int  i,j;
	unsigned char *indexb;
	unsigned char *indexg;
	unsigned char *indexr;
	int temp;
	int  numbyte;

	numbyte = 0;

	for ( j = ysize-1; j > 0; j-- ) {
		indexr = rarray + xsize*j*sizeof(unsigned char);
		indexg = garray + xsize*j*sizeof(unsigned char);
		indexb = barray + xsize*j*sizeof(unsigned char);

		for ( i = 0; i < xsize; i++ ) {
			if (bsize == 24)
			{

				temp = fgetc ( filein );
				if ( temp == EOF ) {
					csc_error_message( "csc_image_bmp_read_data: Failed reading data byte %d.\n", numbyte );
					return ERROR;
				}
				*indexb= (unsigned char)temp;
				numbyte = numbyte + 1;
				indexb = indexb + 1;

				temp = fgetc ( filein );
				if ( temp == EOF ) {
					csc_error_message( "csc_image_bmp_read_data: Failed reading data byte %d.\n", numbyte );
					return ERROR;
				}
				*indexg = (unsigned char)temp;
				numbyte = numbyte + 1;
				indexg = indexg + 1;

				temp = fgetc ( filein );
				if ( temp == EOF ) {
					csc_error_message( "csc_image_bmp_read_data: Failed reading data byte %d.\n", numbyte );
					return ERROR;
				}
				*indexr  = (unsigned char)temp;
				numbyte = numbyte + 1;
				indexr = indexr + 1;

			}
			else if (bsize == 8)
			{
				temp = fgetc ( filein );
				if ( temp == EOF ) {
					csc_error_message( "csc_image_bmp_read_data: Failed reading data byte %d.\n", numbyte );
					return ERROR;
				}
				*indexr = (unsigned char) temp;
				numbyte = numbyte + 1;
				indexr = indexr + 1;
			}
		}
		while(numbyte % 4)
		{
			fgetc(filein);
			numbyte++;
		}

	}

	return SUCCESS;
}

static int csc_image_bmp_read_header ( FILE *filein, int *xsize, int *ysize, int *bsize, int *psize ) {
	int                 c1;
	int                 c2;
	int                 retval;
	unsigned long int   u_long_int_val;
	unsigned short int  u_short_int_val;
	/*
	   Header, 14 bytes.
	   16 bytes FileType;        Magic number: "BM",
	   32 bytes FileSize;        Size of file in 32 byte integers,
	   16 bytes Reserved1;       Always 0,
	   16 bytes Reserved2;       Always 0,
	   32 bytes BitmapOffset.    Starting position of image data, in bytes.
	   */
	c1 = fgetc ( filein );
	if ( c1 == EOF ) {
		return ERROR;
	}
	c2 = fgetc ( filein );
	if ( c2 == EOF ) {
		return ERROR;
	}

	if ( c1 != 'B' || c2 != 'M' ) {
		return ERROR;
	}

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}

	retval = read_u_short_int ( &u_short_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}

	retval = read_u_short_int ( &u_short_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}
	/*
	   The bitmap header is 40 bytes long.
	   32 bytes unsigned Size;            Size of this header, in bytes.
	   32 bytes Width;                    Image width, in pixels.
	   32 bytes Height;                   Image height, in pixels.  (Pos/Neg, origin at bottom, top)
	   16 bytes Planes;                   Number of color planes (always 1).
	   16 bytes BitsPerPixel;             1 to 24.  1, 4, 8 and 24 legal.  16 and 32 on Win95.
	   32 bytes unsigned Compression;     0, uncompressed; 1, 8 bit RLE; 2, 4 bit RLE; 3, bitfields.
	   32 bytes unsigned SizeOfBitmap;    Size of bitmap in bytes. (0 if uncompressed).
	   32 bytes HorzResolution;           Pixels per meter. (Can be zero)
	   32 bytes VertResolution;           Pixels per meter. (Can be zero)
	   32 bytes unsigned ColorsUsed;      Number of colors in palette.  (Can be zero).
	   32 bytes unsigned ColorsImportant. Minimum number of important colors. (Can be zero).
	   */
	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}
	*xsize = ( int ) u_long_int_val;

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}
	*ysize = ( int ) u_long_int_val;

	retval = read_u_short_int ( &u_short_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}


	retval = read_u_short_int ( &u_short_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}
	*bsize = (int) u_short_int_val;

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}
	*psize = ( int ) u_long_int_val;

	retval = read_u_long_int ( &u_long_int_val, filein );
	if ( retval == ERROR ) {
		return ERROR;
	}


	return SUCCESS;
}

static int csc_image_bmp_read_palette ( FILE *filein, int psize ) {
	int  c;
	int  i;
	int  j;

	for ( i = 0; i < psize; i++ ) {
		for ( j = 0; j < 4; j++ ) {
			c = fgetc ( filein );
			if ( c == EOF ) {
				return ERROR;
			}
		}
	}

	return SUCCESS;
}

int csc_image_bmp_write ( const char *fileout_name, csc_image_bmp pic ) {
	FILE *fileout;
	int   result;
	int xsize = pic.width;
	int ysize = pic.height;
	int bsize = pic.bpp;
	unsigned char *rarray = pic.red;
	unsigned char *garray = pic.green;
	unsigned char *barray = pic.blue;

	/* Open the output file. */
	fileout = fopen ( fileout_name, "wb" );

	if ( fileout == NULL ) {
		csc_error_message("csc_image_bmp_write: Could not open the output filei %s.\n", fileout_name );
		return ERROR;
	}

	/*  Write the header. */
	result = csc_image_bmp_write_header ( fileout, xsize, ysize, bsize );

	if ( result == ERROR ) {
		csc_error_message("csc_image_bmp_write_header failed\n"); 
		return ERROR;
	}
	/*  Write the palette if neccessary  */
	if (bsize == 8)
	{
		result = csc_image_bmp_write_palette ( fileout);

		if ( result == ERROR ) {
			csc_error_message("csc_image_bmp_write_palette failed.\n"); 
			return ERROR;
		}
	}
	/*  Write the data. */
	result = csc_image_bmp_write_data ( fileout, xsize, ysize, bsize, rarray, garray, barray );

	if ( result == ERROR ) {
		csc_error_message("csc_image_bmp_write_data failed.\n"); 
		return ERROR;
	}
	/* Close the file. */
	fclose ( fileout );

	return SUCCESS;
}

static int csc_image_bmp_write_data ( FILE *fileout, int xsize, int ysize, int bsize,
					unsigned char *rarray, unsigned char *garray, unsigned char *barray ) {

	int  i;
	unsigned char *indexb;
	unsigned char *indexg;
	unsigned char *indexr;
	int  j;
	int numbyte;

	numbyte = 0;
	for (j = ysize-1; j>=0; j-- ) {
		/* Reverse the write order so the bitmap will be the
		   the same as data array */

		indexr = rarray + xsize*j*sizeof(unsigned char);
		indexg = garray + xsize*j*sizeof(unsigned char);
		indexb = barray + xsize*j*sizeof(unsigned char);
		for ( i = 0; i < xsize; i++ ) {

			if (bsize == 24)
			{
				fputc ( *indexb, fileout );
				fputc ( *indexg, fileout );
				fputc ( *indexr, fileout );
				indexb = indexb + 1;
				indexg = indexg + 1;
				indexr = indexr + 1;
				numbyte+=3;
			}
			else
			{
				fputc ( *indexr, fileout );
				indexr = indexr + 1;
				numbyte++;
			}
		}
		//alligne
		while (numbyte %4)
		{
			fputc(0,fileout);
			numbyte++;
		}
	}

	return SUCCESS;
}

static int csc_image_bmp_write_palette( FILE *fileout) {
	int       i;
	int       j;
	unsigned char c;

	for (i=0;i<256;i++)
	{
		c = (unsigned char)i;
		for(j=0;j<3;j++)
			fputc ( c, fileout );
		fputc(0,fileout);

	}
	return SUCCESS;
}

static int csc_image_bmp_write_header ( FILE *fileout, int xsize, int ysize, int bsize ) {
	int                 i;
	unsigned long int   u_long_int_val;
	unsigned short int  u_short_int_val;
	int xsize_aligned;

	/*
	   Header, 14 bytes.
	   16 bytes FileType;        Magic number: "BM",
	   32 bytes FileSize;        Size of file in bytes,
	   16 bytes Reserved1;       Always 0,
	   16 bytes Reserved2;       Always 0,
	   32 bytes BitmapOffset.    Starting position of image data, in bytes.
	   */
	fputc ( 'B', fileout );
	fputc ( 'M', fileout );


	if (bsize == 8)
	{
		xsize_aligned = xsize;
		while (xsize_aligned %4)
			xsize_aligned++;
		u_long_int_val =  xsize_aligned * ysize + 54 + 256*4;
	}
	else
	{
		xsize_aligned = xsize;
		while (xsize_aligned %4)
			xsize_aligned++;
		u_long_int_val = xsize_aligned * ysize + 54;
	}

	write_u_long_int ( u_long_int_val, fileout );

	u_short_int_val = 0;
	write_u_short_int ( u_short_int_val, fileout );

	u_short_int_val = 0;
	write_u_short_int ( u_short_int_val, fileout );


	if (bsize == 8)
	{
		u_long_int_val = 1078;
	}
	else
	{
		u_long_int_val = 54;
	}

	write_u_long_int ( u_long_int_val, fileout );
	/*
	   The bitmap header is 40 bytes long.
	   32 bytes unsigned Size;            Size of this header, in bytes.
	   32 bytes Width;                    Image width, in pixels.
	   32 bytes Height;                   Image height, in pixels.  (Pos/Neg, origin at bottom, top)
	   16 bytes Planes;                   Number of color planes (always 1).
	   16 bytes BitsPerPixel;             1 to 24.  1, 4, 8 and 24 legal.  16 and 32 on Win95.
	   32 bytes unsigned Compression;     0, uncompressed; 1, 8 bit RLE; 2, 4 bit RLE; 3, bitfields.
	   32 bytes unsigned SizeOfBitmap;    Size of bitmap in bytes. (0 if uncompressed).
	   32 bytes HorzResolution;           Pixels per meter. (Can be zero)
	   32 bytes VertResolution;           Pixels per meter. (Can be zero)
	   32 bytes unsigned ColorsUsed;      Number of colors in palette.  (Can be zero).
	   32 bytes unsigned ColorsImportant. Minimum number of important colors. (Can be zero).
	   */
	u_long_int_val = 40;
	write_u_long_int ( u_long_int_val, fileout );

	write_u_long_int ( xsize, fileout );

	write_u_long_int ( ysize, fileout );

	u_short_int_val = 1;
	write_u_short_int ( u_short_int_val, fileout );

	u_short_int_val = bsize;
	write_u_short_int ( u_short_int_val, fileout );

	u_long_int_val = 0;
	write_u_long_int ( u_long_int_val, fileout ); //compression

	u_long_int_val = (bsize/8)*xsize*ysize;
	write_u_long_int ( u_long_int_val, fileout );

	for ( i = 2; i < 4; i++ ) {
		u_long_int_val = 0;
		write_u_long_int ( u_long_int_val, fileout );
	}

	if (bsize == 8)
		u_long_int_val = 256;		//Number of palette colors
	else
		u_long_int_val = 0;
	write_u_long_int ( u_long_int_val, fileout );

	u_long_int_val = 0;
	write_u_long_int ( u_long_int_val, fileout );

	return SUCCESS;
}


static int read_u_long_int ( unsigned long int *u_long_int_val, FILE *filein ) {
	int                 retval;
	unsigned short int  u_short_int_val_hi;
	unsigned short int  u_short_int_val_lo;

	if ( byte_swap == TRUE ) {
		retval = read_u_short_int ( &u_short_int_val_lo, filein );
		if ( retval == ERROR ) {
			return ERROR;
		}
		retval = read_u_short_int ( &u_short_int_val_hi, filein );
		if ( retval == ERROR ) {
			return ERROR;
		}
	}
	else {
		retval = read_u_short_int ( &u_short_int_val_hi, filein );
		if ( retval == ERROR ) {
			return ERROR;
		}
		retval = read_u_short_int ( &u_short_int_val_lo, filein );
		if ( retval == ERROR ) {
			return ERROR;
		}
	}

	*u_long_int_val = ( u_short_int_val_hi << 16 ) | u_short_int_val_lo;

	return SUCCESS;
}

static int read_u_short_int ( unsigned short int *u_short_int_val, FILE *filein ) {
	int chi;
	int clo;

	if ( byte_swap == TRUE ) {
		clo = fgetc ( filein );
		if ( clo == EOF ) {
			return ERROR;
		}
		chi = fgetc ( filein );
		if ( chi == EOF ) {
			return ERROR;
		}
	}
	else {
		chi = fgetc ( filein );
		if ( chi == EOF ) {
			return ERROR;
		}
		clo = fgetc ( filein );
		if ( clo == EOF ) {
			return ERROR;
		}
	}

	*u_short_int_val = ( chi << 8 ) | clo;

	return SUCCESS;
}

static int write_u_long_int ( unsigned long int u_long_int_val, FILE *fileout ) {
	unsigned short int  u_short_int_val_hi;
	unsigned short int  u_short_int_val_lo;

	u_short_int_val_hi = ( unsigned short ) ( u_long_int_val / 65536 );
	u_short_int_val_lo = ( unsigned short ) ( u_long_int_val % 65536 );

	if ( byte_swap == TRUE ) {
		write_u_short_int ( u_short_int_val_lo, fileout );
		write_u_short_int ( u_short_int_val_hi, fileout );
	}
	else {
		write_u_short_int ( u_short_int_val_hi, fileout );
		write_u_short_int ( u_short_int_val_lo, fileout );
	}

	return 4;
}


static int write_u_short_int ( unsigned short int u_short_int_val, FILE *fileout ) {
	unsigned char chi;
	unsigned char clo;

	chi = ( unsigned char ) ( u_short_int_val / 256 );
	clo = ( unsigned char ) ( u_short_int_val % 256 );

	if ( byte_swap == TRUE ) {
		fputc ( clo, fileout );
		fputc ( chi, fileout );
	}
	else {

		fputc ( chi, fileout );
		fputc ( clo, fileout );
	}

	return 2;
}


// #define BMP_SETVALUE(arr, i,j, w)  (arr)[ ((Y_SIZE)-((y)+1))*(X_SIZE)+(x)]=(w);
void csc_image_bmp_set_pixel ( csc_image_bmp pic, int x, int y, csc_image_color r, csc_image_color g, csc_image_color b)
{
	if (x < pic.width && y < pic.height) {
		int X_SIZE;
		r = r %256;
		g = g %256;
		b = b %256;

		X_SIZE = pic.width;


		pic.red[y*(X_SIZE)+(x)] = r;
		pic.green[y*(X_SIZE)+(x)] = g;
		pic.blue[y*(X_SIZE)+(x)] = b;
	}

}

void csc_image_bmp_fill_rect(csc_image_bmp pic, int x, int y, int width, int height, csc_image_color r, csc_image_color g, csc_image_color b){
	int i, j;
	for ( i = x; i < x+width; i++){
		for (j = y; j < y+height; j++){
			if (i < pic.width && j < pic.height)
				csc_image_bmp_set_pixel (pic, i, j, r, g, b);

		}
	}

}

int csc_image_bmp_init ( csc_image_bmp *pic, int width, int height )
{
	int i;
	pic->red = (unsigned char *) malloc (sizeof(unsigned char) * height * width);
	pic->green = (unsigned char *) malloc (sizeof(unsigned char) * height * width);
	pic->blue = (unsigned char *) malloc (sizeof(unsigned char) * height * width);
	if ( pic->blue == NULL || pic->red == NULL || pic->green == NULL ) return -1;
	for ( i = 0; i < height*width; i++){
		pic->red[i] = 255;
		pic->green[i] = 255;
		pic->blue[i] = 255;
	}

	pic->width = width;
	pic->height = height;
	pic->bpp = 24;
	return 0;
}

void csc_image_bmp_clear( csc_image_bmp  *pic){
	if ( pic->red != NULL) 	free ( pic->red);
	if ( pic->green != NULL)  free ( pic->green);
	if ( pic->blue != NULL)  free ( pic->blue);
	pic->red=NULL;
	pic->green =NULL;
	pic->blue = NULL;
}



