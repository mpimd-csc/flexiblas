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
 * Copyright (C) Martin Koehler, 2015
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "flexiblas_api.h"
#include "flexiblas_fortran_mangle.h"

/*-----------------------------------------------------------------------------
 *  Include the right header files 
 *-----------------------------------------------------------------------------*/
#ifdef BLAS_INTERFACE_INTEL 
#include "blas_intel.h"
#include "extblas_intel.h"
#else 
#include "blas_gnu.h"
#include "extblas_gnu.h"
#endif 
#ifdef FLEXIBLAS_CBLAS
#include "cblas.h" 
#endif 

#ifdef INTEGER8
#define Int int64_t
#else 
#define Int int
#endif 


int main ( int argc, char **argv ) {
	double test1[]={1,2,3,4,5,6,7,8,9,10}; 
	Int N = 10;
	Int one = 1; 
	double ret = 0, ret2 = 0;
    int major, minor, patch; 
    char fb_name[128]; 
    int ids[1024]; 
    int i; 

    

    flexiblas_get_version(&major, &minor, &patch); 
    printf("FlexiBLAS Version %d.%d.%d\n\n", major, minor, patch);

    printf("Loaded Backends:\n");
    flexiblas_print_loaded_backends(stdout);
    printf("\n");

    printf("Available Backends:\n");
    flexiblas_print_avail_backends(stdout); 
    printf("\n");
    
    printf("Current loaded backend:\n");
    flexiblas_print_current_backend(stdout);
    printf("\n");
   	ret = FC_GLOBAL(dasum,DASUM)(&N, test1, &one); 
    printf("dasum_(%20s)      = %lg\n\n", "DEFAULT", ret );         


    printf("Try the other backends.\n");
    int nbackends = flexiblas_list(NULL, 0 , 0); 
    for (i = 0; i < nbackends; i++) {
        flexiblas_list(fb_name, 128, i); 
        printf("Load %s.\n", fb_name);
        ids[i] = flexiblas_load_backend(fb_name); 
        printf("Switch to %s - %d\n", fb_name, ids[i]);
        flexiblas_switch(ids[i]); 
        printf("Current loaded backend:\n");
        flexiblas_print_current_backend(stdout);
        printf("\n");

    	ret = FC_GLOBAL(dasum,DASUM)(&N, test1, &one); 
	    printf("dasum_(%20s)      = %lg\n\n", fb_name, ret );         
    }
    printf("loaded backends:\n");
    flexiblas_print_loaded_backends(stdout); 
    printf("\n");

    ids[i] = flexiblas_load_backend_library("../contributed/netlib-blas/libflexiblas_netlib.so"); 
    printf("Netlib %d\n", ids[i]);
    flexiblas_switch(ids[i]); 
   
    printf("Current loaded backend:\n");
    flexiblas_print_current_backend(stdout);
    printf("\n");

    
    
    printf("finally loaded backends:\n");
    flexiblas_print_loaded_backends(stdout); 
    printf("\n");

#ifdef FLEXIBLAS_CBLAS 
	ret2 = cblas_dasum(N, test1, one); 
	printf("cblas_dasum(test) = %lg\n", ret2 ); 
#endif
	return 0;
}

