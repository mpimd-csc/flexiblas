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



#ifndef FLEXIBLAS_API_H
#define FLEXIBLAS_API_H

#ifdef __cplusplus
extern "C" { 
#endif
    // #include "flexiblas_structures.h"    

    #define FLEXIBLAS_DEFAULT_BLAS 0 
    /*-----------------------------------------------------------------------------
     *  External API of FlexiBLAS
     *-----------------------------------------------------------------------------*/
    
    /* Print Information  */
    extern void flexiblas_get_version(int *major, int *minor, int *patch);  
    extern void flexiblas_print_loaded_backends(FILE *fp); 
    extern void flexiblas_print_avail_backends(FILE *fp); 
    extern void flexiblas_print_current_backend(FILE* fp); 
    
    /* Handle Backends  */
    extern int flexiblas_list(char *name, size_t len, int pos); 
    extern int flexiblas_list_loaded(char *name, size_t len, int pos); 
    extern int flexiblas_load_backend(const char * name ); 
    extern int flexiblas_load_backend_library(const char *libname); 
    extern int flexiblas_switch(int id); 
    extern int flexiblas_current_backend(char *name, size_t len);

    /* Set number of threads  */
    extern void flexiblas_set_num_threads(int num); 
    extern void flexiblas_set_num_threads_(int* num); 
    extern void openblas_set_num_threads(int num); 
    extern void openblas_set_num_threads_(int* num); 
    extern void mkl_set_num_threads(int num); 
    extern void mkl_set_num_threads_(int* num); 
    extern void blas_set_num_threads(int num); 
    extern void blas_set_num_threads_(int* num); 
    extern void acmlsetnumthreads(int num); 
    extern void acmlsetnumthreads_(int* num); 

    /* Get number of threads  */
    extern int flexiblas_get_num_threads(); 
    extern int flexiblas_get_num_threads_(); 
    extern int openblas_get_num_threads(); 
    extern int openblas_get_num_threads_(); 
    extern int mkl_get_num_threads(); 
    extern int mkl_get_num_threads_(); 
    extern int blas_get_num_threads(); 
    extern int blas_get_num_threads_(); 
    extern int acmlgetnumthreads(); 
    extern int acmlgetnumthreads_(); 


#ifdef __cplusplus
};
#endif 

#endif /* end of include guard: FLEXIBLAS_API_H */
