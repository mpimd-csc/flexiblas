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

#include "fortran_mangle.h"
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


#ifdef EXTBLAS_ENABLED
#include "extblas.h"

int __flexiblas_load_extblas(flexiblas_backend_t * handle, int *failed) 
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
		handle->extblas.saxpby.call_fblas = FC_GLOBAL(fsaxpby,FSAXPBY); 
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
		handle->extblas.somatcopy.call_fblas = FC_GLOBAL(fsomatcopy,FSOMATCOPY); 
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
		handle->extblas.simatcopy.call_fblas = FC_GLOBAL(fsimatcopy,FSIMATCOPY);  
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
		handle->extblas.sgeadd.call_fblas = FC_GLOBAL(fsgeadd,FSGEADD); 
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
		handle->extblas.daxpby.call_fblas = FC_GLOBAL(fdaxpby,FDAXPBY); 
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
		handle->extblas.domatcopy.call_fblas = FC_GLOBAL(fdomatcopy,FDOMATCOPY); 
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
		handle->extblas.dimatcopy.call_fblas = FC_GLOBAL(fdimatcopy,FDIMATCOPY);  
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
		handle->extblas.dgeadd.call_fblas = FC_GLOBAL(fdgeadd,FGEADD); 
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
		handle->extblas.caxpby.call_fblas = FC_GLOBAL(fcaxpby,FCAXPBY); 
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
		handle->extblas.comatcopy.call_fblas = FC_GLOBAL(fcomatcopy,FCOMATCOPY);  
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
		handle->extblas.cimatcopy.call_fblas = FC_GLOBAL(fcimatcopy,FCIMATCOPY); 
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
		handle->extblas.cgeadd.call_fblas = FC_GLOBAL(fcgeadd,FCGEADD);  
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
		handle->extblas.zaxpby.call_fblas =  FC_GLOBAL(fdaxpby,FDAXPBY);
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
		handle->extblas.zomatcopy.call_fblas = FC_GLOBAL(fzomatcopy,FZOMATCOPY);  
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
		handle->extblas.zimatcopy.call_fblas =  FC_GLOBAL(fzimatcopy,FZIMATCOPY); 
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
		handle->extblas.zgeadd.call_fblas = FC_GLOBAL(fzgeadd,FZGEADD); 
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
int __flexiblas_load_extblas(void * handle, int *failed) 
{
	return 0; 
}

#endif 




