/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2017
 */

#ifdef __linux__ 
	#define __GNU_SOURCE
	#define _GNU_SOURCE
#endif
#include "flexiblas.h"
#include <errno.h>
#include <stddef.h>
#include <string.h>
#include <strings.h>
#include "cscutils/strutils.h"


#define GET_BLAS_OFFSET(BLAS_NAME) (((ssize_t) &(current_backend->blas. BLAS_NAME)) - (ssize_t) current_backend) 
#define GET_EXTBLAS_OFFSET(BLAS_NAME) (((ssize_t) &(current_backend->extblas. BLAS_NAME)) - (ssize_t) current_backend) 
#define GET_LAPACK_OFFSET(BLAS_NAME) (((ssize_t) &(current_backend->lapack. BLAS_NAME)) - (ssize_t) current_backend) 

__attribute__((unused)) 
static void print_profile(FILE * output, const char*name, struct flexiblas_blasfn *fn)
{
#ifdef FLEXIBLAS_CBLAS 
	char cblas_name[64];
#endif
	fprintf(output,"%16s \t %11.7e \t %8lu\n",name,fn->timings[0],(unsigned long) fn->calls[0]);
#ifdef FLEXIBLAS_CBLAS
	snprintf(cblas_name, 63, "cblas_%s", name);
	fprintf(output,"%16s \t %11.7e \t %8lu \t %s\n",cblas_name, fn->timings[1], (unsigned long) fn->calls[1], (fn->call_cblas == NULL && fn->calls[1]>0)?"redirected to BLAS":"");
#endif

}

static void print_profile_offset(FILE * output, const char*name, ssize_t offset)
{
    double timings = 0; 
    size_t calls = 0;  
    struct flexiblas_blasfn * fn; 
    size_t i; 
#ifdef FLEXIBLAS_CBLAS 
	char cblas_name[64];
    double ctimings = 0; 
    size_t ccalls = 0; 
#endif
    for (i = 0; i < nloaded_backends; i++) {
        fn = (struct flexiblas_blasfn *) ( ((void*)loaded_backends[i]) + offset); 
        timings += fn->timings[0]; 
        calls   += fn->calls[0]; 
#ifdef FLEXIBLAS_CBLAS
        ctimings += fn->timings[1]; 
        ccalls   += fn->calls[1]; 
#endif

    }
	fprintf(output,"%16s \t %11.7e \t %8lu\n",name,timings,(unsigned long) calls);
#ifdef FLEXIBLAS_CBLAS
	snprintf(cblas_name, 63, "cblas_%s", name);
	fprintf(output,"%16s \t %11.7e \t %8lu \n",cblas_name, ctimings, (unsigned long) ccalls); 
#endif

}

static void print_profile_offset_lapack(FILE * output, const char*name, ssize_t offset)
{
    double timings = 0; 
    size_t calls = 0;  
    struct flexiblas_blasfn * fn; 
    size_t i; 
    for (i = 0; i < nloaded_backends; i++) {
        fn = (struct flexiblas_blasfn *) ( ((void*)loaded_backends[i]) + offset); 
        timings += fn->timings[0]; 
        calls   += fn->calls[0]; 
    }
	fprintf(output,"%16s \t %11.7e \t %8lu\n",name,timings,(unsigned long) calls);

}

HIDDEN void  flexiblas_print_profile() {
	FILE *output = NULL; 
	int  on_screen = 0;

	if (__flexiblas_profile_file == NULL) {
		output = stderr; 
		on_screen =1;
	} else {
		if ( strcmp(__flexiblas_profile_file, "stdout") == 0){
			output = stdout;
			on_screen = 1;
		} else if ( strcmp(__flexiblas_profile_file, "stderr") == 0 ){
			output = stderr;
			on_screen = 1;
		} else {
			output = fopen(__flexiblas_profile_file,"w"); 
			if (!output){
				int err = errno; 
				fprintf(stderr, "Opening %s for profile output failed. Use stderr instead. (Reason: %s)\n",__flexiblas_profile_file, strerror(err));
				output = stderr; 
				on_screen = 1;
			} 
		}
	}
	if (__flexiblas_profile_file != NULL) 
		free(__flexiblas_profile_file);

    /*-----------------------------------------------------------------------------
     *  Print the output 
     *-----------------------------------------------------------------------------*/
    if (on_screen) {
		fprintf(output, "\n");
		fprintf(output, "*******************************************************************************\n");
		fprintf(output, "* FlexiBLAS Profiling                                                         *\n");
		fprintf(output, "*******************************************************************************\n");
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* Single Precission BLAS calls.                                               *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
	}
	fprintf(output,"#Function\t\t Runtime in s\t     Calls \n"); 

	print_profile_offset(output,"sasum", GET_BLAS_OFFSET( sasum ) );
	print_profile_offset(output,"saxpy", GET_BLAS_OFFSET( saxpy ) );
	print_profile_offset(output,"scabs1",GET_BLAS_OFFSET( scabs1 ) );
	print_profile_offset(output,"scopy", GET_BLAS_OFFSET( scopy ) );
	print_profile_offset(output,"sdot",  GET_BLAS_OFFSET( sdot ) );
	print_profile_offset(output,"sdsdot",GET_BLAS_OFFSET( sdsdot ) );
	print_profile_offset(output,"sgbmv", GET_BLAS_OFFSET( sgbmv ) );
	print_profile_offset(output,"sgemm", GET_BLAS_OFFSET( sgemm ) );
	print_profile_offset(output,"sgemv", GET_BLAS_OFFSET( sgemv ) );
	print_profile_offset(output,"sger",  GET_BLAS_OFFSET( sger ) );
	print_profile_offset(output,"snrm2", GET_BLAS_OFFSET( snrm2 ) );
	print_profile_offset(output,"srot",  GET_BLAS_OFFSET( srot ) );
	print_profile_offset(output,"srotg", GET_BLAS_OFFSET( srotg ) );
	print_profile_offset(output,"srotm", GET_BLAS_OFFSET( srotm ) );
	print_profile_offset(output,"srotmg",GET_BLAS_OFFSET( srotmg ) );
	print_profile_offset(output,"ssbmv", GET_BLAS_OFFSET( ssbmv ) );
	print_profile_offset(output,"sscal", GET_BLAS_OFFSET( sscal ) );
	print_profile_offset(output,"sspmv", GET_BLAS_OFFSET( sspmv ) );
	print_profile_offset(output,"sspr2", GET_BLAS_OFFSET( sspr2 ) );
	print_profile_offset(output,"sspr",  GET_BLAS_OFFSET( sspr ) );
	print_profile_offset(output,"sswap", GET_BLAS_OFFSET( sswap ) );
	print_profile_offset(output,"ssymm", GET_BLAS_OFFSET( ssymm ) );
	print_profile_offset(output,"ssymv", GET_BLAS_OFFSET( ssymv ) );
	print_profile_offset(output,"ssyr2", GET_BLAS_OFFSET( ssyr2 ) );
	print_profile_offset(output,"ssyr2k",GET_BLAS_OFFSET( ssyr2k ) );
	print_profile_offset(output,"ssyr",  GET_BLAS_OFFSET( ssyr ) );
	print_profile_offset(output,"ssyrk", GET_BLAS_OFFSET( ssyrk ) );
	print_profile_offset(output,"stbmv", GET_BLAS_OFFSET( stbmv ) );
	print_profile_offset(output,"stbsv", GET_BLAS_OFFSET( stbsv ) );
	print_profile_offset(output,"stpmv", GET_BLAS_OFFSET( stpmv ) );
	print_profile_offset(output,"stpsv", GET_BLAS_OFFSET( stpsv ) );
	print_profile_offset(output,"strmm", GET_BLAS_OFFSET( strmm ) );
	print_profile_offset(output,"strmv", GET_BLAS_OFFSET( strmv ) );
	print_profile_offset(output,"strsm", GET_BLAS_OFFSET( strsm ) );
	print_profile_offset(output,"strsv", GET_BLAS_OFFSET( strsv ) );



	if ( on_screen ) {
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* Double Precission BLAS calls.                                               *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output,"Function \t\t Runtime in s\t     Calls \n");
	}
	print_profile_offset(output,"dasum", GET_BLAS_OFFSET( dasum ) );
	print_profile_offset(output,"daxpy", GET_BLAS_OFFSET( daxpy ) );
	print_profile_offset(output,"dcabs1",GET_BLAS_OFFSET( dcabs1 ) );
	print_profile_offset(output,"dcopy", GET_BLAS_OFFSET( dcopy ) );
	print_profile_offset(output,"ddot",  GET_BLAS_OFFSET( ddot ) );
	print_profile_offset(output,"dgbmv", GET_BLAS_OFFSET( dgbmv ) );
	print_profile_offset(output,"dgemm", GET_BLAS_OFFSET( dgemm ) );
	print_profile_offset(output,"dgemv", GET_BLAS_OFFSET( dgemv ) );
	print_profile_offset(output,"dger",  GET_BLAS_OFFSET( dger ) );
	print_profile_offset(output,"dnrm2", GET_BLAS_OFFSET( dnrm2 ) );
	print_profile_offset(output,"drot",  GET_BLAS_OFFSET( drot ) );
	print_profile_offset(output,"drotg", GET_BLAS_OFFSET( drotg ) );
	print_profile_offset(output,"drotm", GET_BLAS_OFFSET( drotm ) );
	print_profile_offset(output,"drotmg",GET_BLAS_OFFSET( drotmg ) );
	print_profile_offset(output,"dsbmv", GET_BLAS_OFFSET( dsbmv ) );
	print_profile_offset(output,"dscal", GET_BLAS_OFFSET( dscal ) );
	print_profile_offset(output,"dsdot", GET_BLAS_OFFSET( dsdot ) );
	print_profile_offset(output,"dspmv", GET_BLAS_OFFSET( dspmv ) );
	print_profile_offset(output,"dspr2", GET_BLAS_OFFSET( dspr2 ) );
	print_profile_offset(output,"dspr",  GET_BLAS_OFFSET( dspr ) );
	print_profile_offset(output,"dswap", GET_BLAS_OFFSET( dswap ) );
	print_profile_offset(output,"dsymm", GET_BLAS_OFFSET( dsymm ) );
	print_profile_offset(output,"dsymv", GET_BLAS_OFFSET( dsymv ) );
	print_profile_offset(output,"dsyr2", GET_BLAS_OFFSET( dsyr2 ) );
	print_profile_offset(output,"dsyr2k",GET_BLAS_OFFSET( dsyr2k ) );
	print_profile_offset(output,"dsyr",  GET_BLAS_OFFSET( dsyr ) );
	print_profile_offset(output,"dsyrk", GET_BLAS_OFFSET( dsyrk ) );
	print_profile_offset(output,"dtbmv", GET_BLAS_OFFSET( dtbmv ) );
	print_profile_offset(output,"dtbsv", GET_BLAS_OFFSET( dtbsv ) );
	print_profile_offset(output,"dtpmv", GET_BLAS_OFFSET( dtpmv ) );
	print_profile_offset(output,"dtpsv", GET_BLAS_OFFSET( dtpsv ) );
	print_profile_offset(output,"dtrmm", GET_BLAS_OFFSET( dtrmm ) );
	print_profile_offset(output,"dtrmv", GET_BLAS_OFFSET( dtrmv ) );
	print_profile_offset(output,"dtrsm", GET_BLAS_OFFSET( dtrsm ) );
	print_profile_offset(output,"dtrsv", GET_BLAS_OFFSET( dtrsv ) );

	if ( on_screen ) {	
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* Complex Single Precission BLAS calls.                                       *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 
	}

	print_profile_offset(output, "scasum",  GET_BLAS_OFFSET( scasum ));
	print_profile_offset(output, "scnrm2",  GET_BLAS_OFFSET( scnrm2 ));


	print_profile_offset(output, "caxpy",  GET_BLAS_OFFSET( caxpy ));
	print_profile_offset(output, "ccopy",  GET_BLAS_OFFSET( ccopy ));
	print_profile_offset(output, "cdotc",  GET_BLAS_OFFSET( cdotc ));
	print_profile_offset(output, "cdotu",  GET_BLAS_OFFSET( cdotu ));
	print_profile_offset(output, "cgbmv",  GET_BLAS_OFFSET( cgbmv ));
	print_profile_offset(output, "cgemm",  GET_BLAS_OFFSET( cgemm ));
	print_profile_offset(output, "cgemv",  GET_BLAS_OFFSET( cgemv ));
	print_profile_offset(output, "cgerc",  GET_BLAS_OFFSET( cgerc ));
	print_profile_offset(output, "cgeru",  GET_BLAS_OFFSET( cgeru ));
	print_profile_offset(output, "chbmv",  GET_BLAS_OFFSET( chbmv ));
	print_profile_offset(output, "chemm",  GET_BLAS_OFFSET( chemm ));
	print_profile_offset(output, "chemv",  GET_BLAS_OFFSET( chemv ));
	print_profile_offset(output, "cher",   GET_BLAS_OFFSET( cher ));
	print_profile_offset(output, "cher2",  GET_BLAS_OFFSET( cher2 ));
	print_profile_offset(output, "cher2k", GET_BLAS_OFFSET( cher2k ));
	print_profile_offset(output, "cherk",  GET_BLAS_OFFSET( cherk ));
	print_profile_offset(output, "chpmv",  GET_BLAS_OFFSET( chpmv ));
	print_profile_offset(output, "chpr",   GET_BLAS_OFFSET( chpr ));
	print_profile_offset(output, "chpr2",  GET_BLAS_OFFSET( chpr2 ));
	print_profile_offset(output, "crotg",  GET_BLAS_OFFSET( crotg ));
	print_profile_offset(output, "csrot",  GET_BLAS_OFFSET( csrot ));
	print_profile_offset(output, "cscal",  GET_BLAS_OFFSET( cscal ));
	print_profile_offset(output, "csscal", GET_BLAS_OFFSET( csscal ));
	print_profile_offset(output, "cswap",  GET_BLAS_OFFSET( cswap ));
	print_profile_offset(output, "csymm",  GET_BLAS_OFFSET( csymm ));
	print_profile_offset(output, "csyr2k", GET_BLAS_OFFSET( csyr2k ));
	print_profile_offset(output, "csyrk",  GET_BLAS_OFFSET( csyrk ));
	print_profile_offset(output, "ctbmv",  GET_BLAS_OFFSET( ctbmv ));
	print_profile_offset(output, "ctbsv",  GET_BLAS_OFFSET( ctbsv ));
	print_profile_offset(output, "ctpmv",  GET_BLAS_OFFSET( ctpmv ));
	print_profile_offset(output, "ctpsv",  GET_BLAS_OFFSET( ctpsv ));
	print_profile_offset(output, "ctrmm",  GET_BLAS_OFFSET( ctrmm ));
	print_profile_offset(output, "ctrmv",  GET_BLAS_OFFSET( ctrmv ));
	print_profile_offset(output, "ctrsm",  GET_BLAS_OFFSET( ctrsm ));
	print_profile_offset(output, "ctrsv",  GET_BLAS_OFFSET( ctrsv ));



	if ( on_screen ) {
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* Complex Double Precission BLAS calls.                                       *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output,"Function \t\t Runtime in s\t     Calls \n");
	}
	print_profile_offset(output, "dzasum",  GET_BLAS_OFFSET( dzasum ));
	print_profile_offset(output, "dznrm2",  GET_BLAS_OFFSET( dznrm2 ));

	print_profile_offset(output,"zaxpy", GET_BLAS_OFFSET( zaxpy ) );
	print_profile_offset(output,"zcopy", GET_BLAS_OFFSET( zcopy ) );
	print_profile_offset(output,"zdotc", GET_BLAS_OFFSET( zdotc ) );
	print_profile_offset(output,"zdotu", GET_BLAS_OFFSET( zdotu ) );
	print_profile_offset(output,"zdrot", GET_BLAS_OFFSET( zdrot ) );
	print_profile_offset(output,"zdscal", GET_BLAS_OFFSET( zdscal ) );
	print_profile_offset(output,"zgbmv", GET_BLAS_OFFSET( zgbmv ) );
	print_profile_offset(output,"zgemm", GET_BLAS_OFFSET( zgemm ) );
	print_profile_offset(output,"zgemv", GET_BLAS_OFFSET( zgemv ) );
	print_profile_offset(output,"zgerc", GET_BLAS_OFFSET( zgerc ) );
	print_profile_offset(output,"zgeru", GET_BLAS_OFFSET( zgeru ) );
	print_profile_offset(output,"zhbmv", GET_BLAS_OFFSET( zhbmv ) );
	print_profile_offset(output,"zhemm", GET_BLAS_OFFSET( zhemm ) );
	print_profile_offset(output,"zhemv", GET_BLAS_OFFSET( zhemv ) );
	print_profile_offset(output,"zher2", GET_BLAS_OFFSET( zher2 ) );
	print_profile_offset(output,"zher2k", GET_BLAS_OFFSET( zher2k ) );
	print_profile_offset(output,"zher", GET_BLAS_OFFSET( zher ) );
	print_profile_offset(output,"zherk", GET_BLAS_OFFSET( zherk ) );
	print_profile_offset(output,"zhpmv", GET_BLAS_OFFSET( zhpmv ) );
	print_profile_offset(output,"zhpr2", GET_BLAS_OFFSET( zhpr2 ) );
	print_profile_offset(output,"zhpr", GET_BLAS_OFFSET( zhpr ) );
	print_profile_offset(output,"zrotg", GET_BLAS_OFFSET( zrotg ) );
	print_profile_offset(output,"zscal", GET_BLAS_OFFSET( zscal ) );
	print_profile_offset(output,"zswap", GET_BLAS_OFFSET( zswap ) );
	print_profile_offset(output,"zsymm", GET_BLAS_OFFSET( zsymm ) );
	print_profile_offset(output,"zsyr2k", GET_BLAS_OFFSET( zsyr2k ) );
	print_profile_offset(output,"zsyrk", GET_BLAS_OFFSET( zsyrk ) );
	print_profile_offset(output,"ztbmv", GET_BLAS_OFFSET( ztbmv ) );
	print_profile_offset(output,"ztbsv", GET_BLAS_OFFSET( ztbsv ) );
	print_profile_offset(output,"ztpmv", GET_BLAS_OFFSET( ztpmv ) );
	print_profile_offset(output,"ztpsv", GET_BLAS_OFFSET( ztpsv ) );
	print_profile_offset(output,"ztrmm", GET_BLAS_OFFSET( ztrmm ) );
	print_profile_offset(output,"ztrmv", GET_BLAS_OFFSET( ztrmv ) );
	print_profile_offset(output,"ztrsm", GET_BLAS_OFFSET( ztrsm ) );
	print_profile_offset(output,"ztrsv", GET_BLAS_OFFSET( ztrsv ) );

	/* BLAS Extension  */
#ifdef EXTBLAS_ENABLED
	if ( on_screen ) {
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* BLAS Extension calls.                                                       *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 
	}
    print_profile_offset(output, "saxpby",       GET_EXTBLAS_OFFSET( saxpby ) );
    print_profile_offset(output, "daxpby",       GET_EXTBLAS_OFFSET( daxpby ) );
    print_profile_offset(output, "caxpby",       GET_EXTBLAS_OFFSET( caxpby ) );
    print_profile_offset(output, "zaxpby",       GET_EXTBLAS_OFFSET( zaxpby ) );
    print_profile_offset(output, "somatcopy",       GET_EXTBLAS_OFFSET( somatcopy ) );
    print_profile_offset(output, "domatcopy",       GET_EXTBLAS_OFFSET( domatcopy ) );
    print_profile_offset(output, "comatcopy",       GET_EXTBLAS_OFFSET( comatcopy ) );
    print_profile_offset(output, "zomatcopy",       GET_EXTBLAS_OFFSET( zomatcopy ) );

    print_profile_offset(output, "simatcopy",       GET_EXTBLAS_OFFSET( simatcopy ) );
    print_profile_offset(output, "dimatcopy",       GET_EXTBLAS_OFFSET( dimatcopy ) );
    print_profile_offset(output, "cimatcopy",       GET_EXTBLAS_OFFSET( cimatcopy ) );
    print_profile_offset(output, "zimatcopy",       GET_EXTBLAS_OFFSET( zimatcopy ) );
#endif 
	if ( on_screen ){
		fprintf(output, "*******************************************************************************\n"); 
	}
    print_profile_offset(output, "xerbla", (((ssize_t) &(current_backend->xerbla)) - (ssize_t) current_backend));

	if ( on_screen ) {
		fprintf(output, "*******************************************************************************\n"); 
	}

#ifdef FLEXIBLAS_LAPACK 
	if ( on_screen ) {
		fprintf(output, "\n");
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output, "* LAPACK calls.                                                               *\n"); 
		fprintf(output, "*-----------------------------------------------------------------------------*\n"); 
		fprintf(output,"Function \t\t Runtime in s\t     Calls \n"); 
	}

#ifdef FLEXIBLAS_LAPACK_3_7_0
#include "lapack_interface/profile_3_7_0.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_7_0_WODPRC
#include "lapack_interface/profile_3_7_0-wodprc.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_6_1
#include "lapack_interface/profile_3_6_1.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_6_1_WODPRC
#include "lapack_interface/profile_3_6_1-wodprc.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_6_0
#include "lapack_interface/profile_3_6_0.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_6_0_WODPRC
#include "lapack_interface/profile_3_6_0-wodprc.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_5_0
#include "lapack_interface/profile_3_5_0.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_4_2
#include "lapack_interface/profile_3_4_2.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_4_1
#include "lapack_interface/profile_3_4_1.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_4_0
#include "lapack_interface/profile_3_4_0.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_3_1
#include "lapack_interface/profile_3_3_1.c"
#endif 
#ifdef FLEXIBLAS_LAPACK_3_3_0
#include "lapack_interface/profile_3_3_0.c"
#endif 
	if ( on_screen ) {
		fprintf(output, "*******************************************************************************\n"); 
	}
#endif 


	if ( output != stderr && output != stdout) fclose(output); 
	return; 
}

