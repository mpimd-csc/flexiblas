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
 *  Set the number of threads from C
 *-----------------------------------------------------------------------------*/
void flexiblas_set_num_threads(int num)
{
    flexiblas_set_num_threads_function_t fn;
    DPRINTF(2, "Set number of threads: %d  C-fn: %lx F77-fn: %lx\n", num,
            (unsigned long) current_backend->set_num_threads_function[0],
            (unsigned long) current_backend->set_num_threads_function[1]);
    if ( current_backend->set_num_threads_function[0] == NULL
         && current_backend->set_num_threads_function[1] != NULL ) {
        Int nx = num;
        flexiblas_set_num_threads_(&nx);
        return;
    }
    fn = current_backend->set_num_threads_function[0];
    if ( fn == NULL) return;
    fn (num);
    return;
}

#ifndef __APPLE__
void openblas_set_num_threads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
void mkl_set_num_threads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
void acmlsetnumthreads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
void blas_set_num_threads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
#else
void openblas_set_num_threads(int num) { flexiblas_set_num_threads(num); }
void mkl_set_num_threads(int num) { flexiblas_set_num_threads(num); }
void acmlsetnumthreads(int num) { flexiblas_set_num_threads(num); }
void blas_set_num_threads(int num)  { flexiblas_set_num_threads(num); }

#endif
/* BLIS Interface */
void bli_thread_set_num_threads(Int num) {
    int _num = num;
    flexiblas_set_num_threads(_num);
    return;
}

/*-----------------------------------------------------------------------------
 *  Get the current number of threads from C
 *-----------------------------------------------------------------------------*/
int flexiblas_get_num_threads()
{
    flexiblas_get_num_threads_function_t fn;
    DPRINTF(2, "Get number of threads:  C-fn: %lx F77-fn: %lx\n",
            (unsigned long) current_backend->get_num_threads_function[0],
            (unsigned long) current_backend->get_num_threads_function[1]);
    if ( current_backend->get_num_threads_function[0] == NULL
         && current_backend->get_num_threads_function[1] != NULL ) {
        return flexiblas_get_num_threads_();
    }
    fn = current_backend->get_num_threads_function[0];
    if ( fn == NULL) return 1;
    return fn ();
}

#ifndef __APPLE__
int  openblas_get_num_threads() __attribute__((weak,alias("flexiblas_get_num_threads")));
int  mkl_get_num_threads() __attribute__((weak,alias("flexiblas_get_num_threads")));
int  acmlgetnumthreads() __attribute__((weak,alias("flexiblas_get_num_threads")));
int  blas_get_num_threads() __attribute__((weak,alias("flexiblas_get_num_threads")));
#else
int  openblas_get_num_threads() { return flexiblas_get_num_threads(); }
int  mkl_get_num_threads()	{ return flexiblas_get_num_threads(); }
int  acmlgetnumthreads() 	{ return flexiblas_get_num_threads(); }
int  blas_get_num_threads() 	{ return flexiblas_get_num_threads(); }
#endif

/*  Blis Interface  */
Int bli_thread_get_num_threads() {
    Int _num;
    _num = (Int) flexiblas_get_num_threads();
    return _num;
}

/*-----------------------------------------------------------------------------
 *  Set the number of threads from Fortran
 *-----------------------------------------------------------------------------*/
void flexiblas_set_num_threads_(Int* num)
{
    Int num_threads;
    flexiblas_set_num_threads_function_t fn;
    void (*fn2) (Int *);
    DPRINTF(2, "Set number of threads: %d  C-fn: %lx F77-fn: %lx\n", *num,
            (unsigned long) current_backend->set_num_threads_function[0],
            (unsigned long) current_backend->set_num_threads_function[1]);

    if ( current_backend->set_num_threads_function[1] == NULL
         && current_backend->set_num_threads_function[0] != NULL ) {
        flexiblas_set_num_threads(*num);
        return;
    }
    fn = current_backend->set_num_threads_function[1];
    num_threads = *num;
    if (fn == NULL) return;
    fn2 =(void*)fn;
    fn2 (&num_threads);
    return;
}

#ifndef __APPLE__
void openblas_set_num_threads_(Int *num) __attribute__((weak, alias("flexiblas_set_num_threads_")));
void mkl_set_num_threads_(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
void acmlsetnumthreads_(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
void blas_set_num_threads_(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
#else
void openblas_set_num_threads_(Int *num) { flexiblas_set_num_threads_(num); }
void mkl_set_num_threads_(Int *num)      { flexiblas_set_num_threads_(num); }
void acmlsetnumthreads_(Int *num)        { flexiblas_set_num_threads_(num); }
void blas_set_num_threads_(Int *num)     { flexiblas_set_num_threads_(num); }

#endif

/*-----------------------------------------------------------------------------
 *  Get number of threads from fortran
 *-----------------------------------------------------------------------------*/
Int flexiblas_get_num_threads_()
{
    flexiblas_get_num_threads_function_t fn;
    DPRINTF(2, "Get number of threads: C-fn: %lx F77-fn: %lx\n",
            (unsigned long) current_backend->get_num_threads_function[0],
            (unsigned long) current_backend->get_num_threads_function[1]);

    if ( current_backend->get_num_threads_function[1] == NULL
         && current_backend->get_num_threads_function[0] != NULL ) {
        return flexiblas_get_num_threads();
    }
    fn = current_backend->get_num_threads_function[1];
    if (fn == NULL) return 1;
    return fn ();
}

#ifndef __APPLE__
Int  openblas_get_num_threads_() __attribute__((weak, alias("flexiblas_get_num_threads_")));
Int  mkl_get_num_threads_() __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  acmlgetnumthreads_() __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  blas_get_num_threads_() __attribute__((weak,alias("flexiblas_get_num_threads_")));
#else
Int  openblas_get_num_threads_() { return flexiblas_get_num_threads_(); }
Int  mkl_get_num_threads_()      { return flexiblas_get_num_threads_(); }
Int  acmlgetnumthreads_()        { return flexiblas_get_num_threads_(); }
Int  blas_get_num_threads_()     { return flexiblas_get_num_threads_(); }

#endif


/*-----------------------------------------------------------------------------
 *  Search for the set number of threads function
 *-----------------------------------------------------------------------------*/
void __flexiblas_load_set_num_threads(flexiblas_backend_t * backend)
{
    void *ptr = NULL;
    void *ptr2 = NULL;
    char fn_name[128];
    char fn2_name[130];
    int i = 0;

    for (i = 0; i < 5; i++) {
        if (i == 0 )
            strncpy(fn_name, "hook_set_num_threads",127);
        else if ( i == 1) {
            strncpy(fn_name, "MKL_Set_Num_Threads",127);
            strncpy(fn2_name, "mkl_set_num_threads_",127);
        } else if ( i == 2)
            strncpy(fn_name, "openblas_set_num_threads",127);
        else if ( i == 3)
            strncpy(fn_name, "acmlsetnumthreads",127);
        else if ( i == 4 )
            strncpy(fn_name, "bli_thread_set_num_threads", 127);
        fn_name[127] = '\0';
        if ( i != 1 ) {
            snprintf(fn2_name, 130, "%s_", fn_name);
        }
    	ptr  = dlsym(backend->library_handle, fn_name);
        ptr2  = dlsym(backend->library_handle, fn2_name);

        if (ptr != NULL || ptr2 != NULL)
            break;
    }
    backend->set_num_threads_function[0] = (flexiblas_set_num_threads_function_t ) ptr;
    backend->set_num_threads_function[1] = (flexiblas_set_num_threads_function_t ) ptr2;

    if ( ptr ) {
        DPRINTF(1, "Set thread number function found ( func_name = %s ) at 0x%lx\n", fn_name,  (unsigned long)ptr);
    }
    if ( ptr2 ) {
        DPRINTF(1, "Set thread number function found ( func_name = %s ) at 0x%lx\n", fn2_name, (unsigned long)ptr2);
    }

    return;
}


/*-----------------------------------------------------------------------------
 *  Search for the get number of threads function
 *-----------------------------------------------------------------------------*/
void __flexiblas_load_get_num_threads(flexiblas_backend_t * backend)
{
    void *ptr = NULL;
    void *ptr2 = NULL;
    char fn_name[128];
    char fn2_name[130];
    int i = 0;

    for (i = 0; i < 5; i++) {
        if (i == 0 )
            strncpy(fn_name, "hook_get_num_threads",127);
        else if ( i == 1) {
            strncpy(fn_name, "MKL_Get_Max_Threads",127);
            strncpy(fn2_name, "mkl_get_max_threads_",127);
        } else if ( i == 2)
            strncpy(fn_name, "openblas_get_num_threads",127);
        else if ( i == 3)
            strncpy(fn_name, "acmlgetnumthreads",127);
        else if ( i == 4 )
            strncpy(fn_name, "bli_thread_get_num_threads",127);

        fn_name[127] = '\0';
        if ( i != 1 )
            snprintf(fn2_name, 130, "%s_", fn_name);

       	ptr  = dlsym(backend->library_handle, fn_name);
        ptr2  = dlsym(backend->library_handle, fn2_name);

        if (ptr != NULL || ptr2 != NULL)
            break;
    }
    backend->get_num_threads_function[0] = (flexiblas_get_num_threads_function_t ) ptr;
    backend->get_num_threads_function[1] = (flexiblas_get_num_threads_function_t ) ptr2;

    if ( ptr ) {
        DPRINTF(1, "Get thread number function ( func_name = %s )  at 0x%lx\n", fn_name, (unsigned long)ptr);
    }
    if ( ptr2 ) {
        DPRINTF(1, "Get thread number function ( func_name = %s )  at 0x%lx\n", fn2_name, (unsigned long)ptr2);
    }

    return;
}


