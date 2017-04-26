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
 * Copyright (C) Martin Koehler , 2015
 */


#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <complex.h> 
#include <math.h>

#include "flexiblas.h"


/*-----------------------------------------------------------------------------
 *  SCABS / DCABS
 *-----------------------------------------------------------------------------*/

double dcabs1_(double complex *Z){
#ifdef __i386__
	double register volatile ret = 0.0; 
#else
	double ret = 0.0;
#endif 
	double te = 0, ts =  0;
	if (__flexiblas_profile) {
		ts = flexiblas_wtime(); 
	}
	ret= fabs(creal(*Z)) + fabs(cimag(*Z)); 

	if ( __flexiblas_profile ) {
		te = flexiblas_wtime(); 
		current_backend->blas.dcabs1.calls[0]++; 
		current_backend->blas.dcabs1.timings[0] += (te - ts); 
	}
	return ret; 
}

double dcabs1(double complex *Z) __attribute__((alias("dcabs1_"))); 
double dcabs132_(double complex *Z) __attribute__((alias("dcabs1_"))); 
double dcabs164_(double complex *Z) __attribute__((alias("dcabs1_"))); 

float scabs1_(float complex *Z){
#ifdef __i386__
	float register volatile ret = 0.0; 
#else
	float ret = 0.0;
#endif 
	double te = 0, ts =  0;
	float complex z = *Z;
	if (__flexiblas_profile) {
		ts = flexiblas_wtime(); 
	}

	// printf("SCABS %20g %20g\n", crealf(*Z), cimagf(*Z));
	ret= fabsf(crealf(z)) + fabsf(cimagf(z)); 

	if ( __flexiblas_profile ) {
		te = flexiblas_wtime(); 
		current_backend->blas.scabs1.calls[0]++; 
		current_backend->blas.scabs1.timings[0] += (te - ts); 
	}
	return ret; 
}

float scabs1(float complex *Z) __attribute__((alias("scabs1_"))); 
float scabs132_(float complex *Z) __attribute__((alias("scabs1_"))); 
float scabs164_(float complex *Z) __attribute__((alias("scabs1_"))); 


#ifdef EXTBLAS_ENABLED
#include "extblas.h"
#define INT_SELECT(F4,F8) ((handle->info.backend_integer_size == 4) ? ((void*)F4):((void*)F8)) 

int __flexiblas_load_extblas(flexiblas_backend_t * handle, int *loaded, int *failed) 
{
	int ifailed = *failed; 


	/*-----------------------------------------------------------------------------
	 *  Single 
	 *-----------------------------------------------------------------------------*/
	/* Load AXPBY  */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.saxpby), "saxpby") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas SAXPBY loaded.\n");
		}
		handle->extblas.saxpby.call_fblas = INT_SELECT(fsaxpby32_, fsaxpby64_); 
		handle->extblas.saxpby.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.saxpby), "saxpby") != 0 ){
			handle->extblas.saxpby.call_cblas = NULL; 
		}
	}

	/* Load OMATCOPY */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.somatcopy), "somatcopy") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas SOMATCOPY loaded.\n");
		}
		handle->extblas.somatcopy.call_fblas = INT_SELECT(fsomatcopy32_, fsomatcopy64_); 
		handle->extblas.somatcopy.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.somatcopy), "somatcopy") != 0 ){
			handle->extblas.somatcopy.call_cblas = NULL; 
		}
	}


	/* Load IMATCOPY */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.simatcopy), "simatcopy") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas SIMATCOPY loaded.\n");
		}
		handle->extblas.simatcopy.call_fblas = INT_SELECT(fsimatcopy32_, fsimatcopy64_); 
		handle->extblas.simatcopy.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.simatcopy), "simatcopy") != 0 ){
			handle->extblas.simatcopy.call_cblas = NULL; 
		}
	}

    /* Load SGEADD */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.sgeadd), "sgeadd") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas SGEADD loaded.\n");
		}
		handle->extblas.sgeadd.call_fblas = INT_SELECT(fsgeadd32_, fsgeadd64_); 
		handle->extblas.sgeadd.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.sgeadd), "sgeadd") != 0 ){
			handle->extblas.sgeadd.call_cblas = NULL; 
		}
	}

	/*-----------------------------------------------------------------------------
	 *  Double 
	 *-----------------------------------------------------------------------------*/
	/* Load AXPBY  */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.daxpby), "daxpby") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas DAXPBY loaded.\n");
		}
		handle->extblas.daxpby.call_fblas = INT_SELECT(fdaxpby32_, fdaxpby64_); 
		handle->extblas.daxpby.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.daxpby), "daxpby") != 0 ){
			handle->extblas.daxpby.call_cblas = NULL; 
		}
	}

	/* Load OMATCOPY  */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.domatcopy), "domatcopy") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas DOMATCOPY loaded.\n");
		}
		handle->extblas.domatcopy.call_fblas = INT_SELECT(fdomatcopy32_, fdomatcopy64_); 
		handle->extblas.domatcopy.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.domatcopy), "domatcopy") != 0 ){
			handle->extblas.domatcopy.call_cblas = NULL; 
		}
	}


	/* Load IMATCOPY  */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.dimatcopy), "dimatcopy") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas DIMATCOPY loaded.\n");
		}
		handle->extblas.dimatcopy.call_fblas = INT_SELECT(fdimatcopy32_, fdimatcopy64_); 
		handle->extblas.dimatcopy.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.dimatcopy), "dimatcopy") != 0 ){
			handle->extblas.dimatcopy.call_cblas = NULL; 
		}
	}

    /* Load DGEADD */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.dgeadd), "dgeadd") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas DGEADD loaded.\n");
		}
		handle->extblas.dgeadd.call_fblas = INT_SELECT(fdgeadd32_, fdgeadd64_); 
		handle->extblas.dgeadd.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.dgeadd), "dgeadd") != 0 ){
			handle->extblas.dgeadd.call_cblas = NULL; 
		}
	}

	/*-----------------------------------------------------------------------------
	 *  Complex 
	 *-----------------------------------------------------------------------------*/
	/* Load AXPBY  */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.caxpby), "caxpby") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas CAXPBY loaded.\n");
		}
		handle->extblas.caxpby.call_fblas = INT_SELECT(fcaxpby32_, fcaxpby64_); 
		handle->extblas.caxpby.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.caxpby), "caxpby") != 0 ){
			handle->extblas.caxpby.call_cblas = NULL; 
		}
	}

	/* Load OMATCOPY */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.comatcopy), "comatcopy") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas COMATCOPY loaded.\n");
		}
		handle->extblas.comatcopy.call_fblas = INT_SELECT(fcomatcopy32_, fcomatcopy64_); 
		handle->extblas.comatcopy.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.comatcopy), "comatcopy") != 0 ){
			handle->extblas.comatcopy.call_cblas = NULL; 
		}
	}



	/* Load IMATCOPY */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.cimatcopy), "cimatcopy") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas CIMATCOPY loaded.\n");
		}
		handle->extblas.cimatcopy.call_fblas = INT_SELECT(fcimatcopy32_, fcimatcopy64_); 
		handle->extblas.cimatcopy.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.cimatcopy), "cimatcopy") != 0 ){
			handle->extblas.cimatcopy.call_cblas = NULL; 
		}
	}

    /* Load CGEADD */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.cgeadd), "cgeadd") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas CGEADD loaded.\n");
		}
		handle->extblas.cgeadd.call_fblas = INT_SELECT(fcgeadd32_, fcgeadd64_); 
		handle->extblas.cgeadd.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.cgeadd), "cgeadd") != 0 ){
			handle->extblas.cgeadd.call_cblas = NULL; 
		}
	}

    
	/*-----------------------------------------------------------------------------
	 *  Complex 16
	 *-----------------------------------------------------------------------------*/
	/* AXPBY   */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.zaxpby), "zaxpby") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas ZAXPBY loaded.\n");
		}
		handle->extblas.zaxpby.call_fblas = INT_SELECT(fzaxpby32_, fzaxpby64_); 
		handle->extblas.zaxpby.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.zaxpby), "zaxpby") != 0 ){
			handle->extblas.zaxpby.call_cblas = NULL; 
		}
	}
	
	/* Load OMATCOPY  */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.zomatcopy), "zomatcopy") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas ZOMATCOPY loaded.\n");
		}
		handle->extblas.zomatcopy.call_fblas = INT_SELECT(fzomatcopy32_, fzomatcopy64_); 
		handle->extblas.zomatcopy.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.zomatcopy), "zomatcopy") != 0 ){
			handle->extblas.zomatcopy.call_cblas = NULL; 
		}
	}


	/* Load IMATCOPY */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.zimatcopy), "zimatcopy") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas ZIMATCOPY loaded.\n");
		}
		handle->extblas.zimatcopy.call_fblas = INT_SELECT(fzimatcopy32_, fzimatcopy64_); 
		handle->extblas.zimatcopy.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.zimatcopy), "zimatcopy") != 0 ){
			handle->extblas.zimatcopy.call_cblas = NULL; 
		}
	}

    /* Load ZGEADD */
	if ( __flexiblas_load_fortran_function(handle->library_handle, &(handle->extblas.zgeadd), "zgeadd") != 0 ) {
		if ( __flexiblas_verbose > 0 ) {
			fprintf(stderr,PRINT_PREFIX "flexiblas ZGEADD loaded.\n");
		}
		handle->extblas.zgeadd.call_fblas = INT_SELECT(fzgeadd32_, fzgeadd64_); 
		handle->extblas.zgeadd.call_cblas = NULL; 
	} else {
		if ( __flexiblas_load_cblas_function(handle->library_handle, &(handle->extblas.zgeadd), "zgeadd") != 0 ){
			handle->extblas.zgeadd.call_cblas = NULL; 
		}
	}

	
	if (ifailed != *failed)
		return 1; 
	else 
		return 0; 
}

#else 

/* Dummy if we do not use EXT BLAS  */
int __flexiblas_load_extblas(void * handle, int *loaded, int *failed) 
{
	return 0; 
}

#endif 




