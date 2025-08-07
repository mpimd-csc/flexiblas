//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2025 Martin Koehler

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
   */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <complex.h>

#include "flexiblas.h"

#ifdef mkl_set_num_threads
#undef mkl_set_num_threads
#endif

#ifdef mkl_get_num_threads
#undef mkl_get_num_threads
#endif

#ifdef mkl_get_max_threads
#undef mkl_get_max_threads
#endif


/*-----------------------------------------------------------------------------
 *  Set the number of threads from C
 *-----------------------------------------------------------------------------*/
void flexiblas_set_num_threads(int num)
{
    flexiblas_set_num_threads_function_t fn;
    DPRINTF(2, "C-Interface: Set number of threads: %d  C-fn: %lx F77-fn: %lx\n", num,
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
void MKL_Set_Num_Threads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
void acmlsetnumthreads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
void blas_set_num_threads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
void nvpl_blas_set_num_threads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
void nvpl_lapack_set_num_threads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
void armpl_set_num_threads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
void armpl_omp_set_num_threads(int num) __attribute__((weak,alias("flexiblas_set_num_threads")));
#else
void openblas_set_num_threads(int num) { flexiblas_set_num_threads(num); }
void MKL_Set_Num_Threads(int num) { flexiblas_set_num_threads(num); }
void acmlsetnumthreads(int num) { flexiblas_set_num_threads(num); }
void blas_set_num_threads(int num)  { flexiblas_set_num_threads(num); }
void nvpl_blas_set_num_threads(int num)  { flexiblas_set_num_threads(num); }
void nvpl_lapack_set_num_threads(int num)  { flexiblas_set_num_threads(num); }
void armpl_set_num_threads(int num) { flexiblas_set_num_threads(num); }
void armpl_omp_set_num_threads(int num) { flexiblas_set_num_threads(num); }
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
int flexiblas_get_num_threads(void)
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
int  openblas_get_num_threads(void) __attribute__((weak,alias("flexiblas_get_num_threads")));
int  MKL_Get_Num_Threads(void) __attribute__((weak,alias("flexiblas_get_num_threads")));
int  MKL_Get_Max_Threads(void) __attribute__((weak,alias("flexiblas_get_num_threads")));
int  acmlgetnumthreads(void) __attribute__((weak,alias("flexiblas_get_num_threads")));
int  blas_get_num_threads(void) __attribute__((weak,alias("flexiblas_get_num_threads")));
int  nvpl_blas_get_max_threads(void) __attribute__((weak,alias("flexiblas_get_num_threads")));
int  nvpl_lapack_get_max_threads(void) __attribute__((weak,alias("flexiblas_get_num_threads")));
int  armpl_get_num_threads(void) __attribute__((weak,alias("flexiblas_get_num_threads")));
int  armpl_omp_get_num_threads(void) __attribute__((weak,alias("flexiblas_get_num_threads")));
#else
int  openblas_get_num_threads(void) { return flexiblas_get_num_threads(); }
int  MKL_Get_Num_Threads(void)	{ return flexiblas_get_num_threads(); }
int  MKL_Get_Max_Threads(void)	{ return flexiblas_get_num_threads(); }
int  acmlgetnumthreads(void) 	{ return flexiblas_get_num_threads(); }
int  blas_get_num_threads(void) 	{ return flexiblas_get_num_threads(); }
int  nvpl_blas_get_max_threads(void) 	{ return flexiblas_get_num_threads(); }
int  nvpl_lapack_get_max_threads(void) 	{ return flexiblas_get_num_threads(); }
int  armpl_get_num_threads(void) { return flexiblas_get_num_threads(); }
int  armpl_omp_get_num_threads(void) { return flexiblas_get_num_threads(); }
#endif

/*  BLIS Interface  */
Int bli_thread_get_num_threads(void) {
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
    DPRINTF(2, "Fortran Interface - Set number of threads: %d  C-fn: %lx F77-fn: %lx\n", *num,
            (unsigned long) current_backend->set_num_threads_function[0],
            (unsigned long) current_backend->set_num_threads_function[1]);

    if ( current_backend->set_num_threads_function[1] == NULL
            && current_backend->set_num_threads_function[0] != NULL ) {
        flexiblas_set_num_threads(*num);
        return;
    }
    * (void **) &fn = *( void **) &current_backend->set_num_threads_function[1];
    num_threads = *num;
    if (fn == NULL) return;
    *(void **) & fn2 = *(void**) &fn;
    fn2 (&num_threads);
    return;
}

#ifndef __APPLE__
void openblas_set_num_threads_(Int *num) __attribute__((weak, alias("flexiblas_set_num_threads_")));
void mkl_set_num_threads(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
void mkl_set_num_threads_(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
void MKL_SET_NUM_THREADS(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
void acmlsetnumthreads_(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
void blas_set_num_threads_(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
void armpl_set_num_threads_(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
void armpl_omp_set_num_threads_(Int *num) __attribute__((weak,alias("flexiblas_set_num_threads_")));
#else
void openblas_set_num_threads_(Int *num) { flexiblas_set_num_threads_(num); }
void mkl_set_num_threads(Int *num)      { flexiblas_set_num_threads_(num); }
void mkl_set_num_threads_(Int *num)      { flexiblas_set_num_threads_(num); }
void MKL_SET_NUM_THREADS(Int *num)      { flexiblas_set_num_threads_(num); }
void acmlsetnumthreads_(Int *num)        { flexiblas_set_num_threads_(num); }
void blas_set_num_threads_(Int *num)     { flexiblas_set_num_threads_(num); }
void armpl_set_num_threads_(Int *num)    { flexiblas_set_num_threads_(num); }
void armpl_omp_set_num_threads_(Int *num) { flexiblas_set_num_threads_(num); }
#endif

void nvpl_blas_set_num_threads_(int32_t* num)
{
    Int n = *num;
    flexiblas_set_num_threads_(&n);
}

void nvpl_lapack_set_num_threads_(int32_t* num)
{
    Int n = *num;
    flexiblas_set_num_threads_(&n);
}


/*-----------------------------------------------------------------------------
 *  Get number of threads from fortran
 *-----------------------------------------------------------------------------*/
Int flexiblas_get_num_threads_(void)
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
Int  openblas_get_num_threads_(void) __attribute__((weak, alias("flexiblas_get_num_threads_")));
Int  mkl_get_num_threads_(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  mkl_get_num_threads(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  MKL_GET_NUM_THREADS(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  mkl_get_max_threads_(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  mkl_get_max_threads(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  MKL_GET_MAX_THREADS(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  acmlgetnumthreads_(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  blas_get_num_threads_(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  armpl_get_num_threads_(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
Int  armpl_omp_get_num_threads_(void) __attribute__((weak,alias("flexiblas_get_num_threads_")));
#else
Int  openblas_get_num_threads_(void) { return flexiblas_get_num_threads_(); }
Int  mkl_get_num_threads_(void)      { return flexiblas_get_num_threads_(); }
Int  mkl_get_num_threads(void)      { return flexiblas_get_num_threads_(); }
Int  MKL_GET_NUM_THREADS(void)      { return flexiblas_get_num_threads_(); }
Int  mkl_get_max_threads_(void)      { return flexiblas_get_num_threads_(); }
Int  mkl_get_max_threads(void)      { return flexiblas_get_num_threads_(); }
Int  MKL_GET_MAX_THREADS(void)      { return flexiblas_get_num_threads_(); }
Int  acmlgetnumthreads_(void)        { return flexiblas_get_num_threads_(); }
Int  blas_get_num_threads_(void)     { return flexiblas_get_num_threads_(); }
Int  armpl_get_num_threads_(void)    { return flexiblas_get_num_threads_(); }
Int  armpl_omp_get_num_threads_(void) { return flexiblas_get_num_threads_(); }
#endif

int32_t  nvpl_blas_get_max_threads_(void) 	{ return (int32_t) flexiblas_get_num_threads_(); }
int32_t  nvpl_lapack_get_max_threads_(void) 	{ return (int32_t) flexiblas_get_num_threads_(); }

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
    int fn2_empty = 1;

    for (i = 0; i < 8; i++) {
        fn2_empty = 1;
        if (i == 0 )
            strncpy(fn_name, "hook_set_num_threads",127);
        else if ( i == 1) {
            strncpy(fn_name, "MKL_Set_Num_Threads",127);
            strncpy(fn2_name, "mkl_set_num_threads_",127);
            fn2_empty = 0;
        } else if ( i == 2) {
            strncpy(fn_name, "openblas_set_num_threads",127);
        }
        else if ( i == 3)
            strncpy(fn_name, "acmlsetnumthreads",127);
        else if ( i == 4 )
            strncpy(fn_name, "bli_thread_set_num_threads", 127);
        else if ( i == 5 )
            strncpy(fn_name, "flexiblas_backend_set_num_threads", 127);
        else if ( i == 6 ) {
            strncpy(fn_name, "armpl_omp_set_num_threads", 127);
        } else if ( i == 7 ) {
            strncpy(fn_name, "armpl_set_num_threads", 127);
        }
        fn_name[127] = '\0';
        if ( fn2_empty ) {
            snprintf(fn2_name, 130, "%s_", fn_name);
        }
        ptr = __flexiblas_dlsym(backend->library_handle, fn_name);
        ptr2 = __flexiblas_dlsym(backend->library_handle, fn2_name);

        if (ptr != NULL || ptr2 != NULL)
            break;
    }
    *(void **) &backend->set_num_threads_function[0] = *(void **) &ptr;
    *(void **) &backend->set_num_threads_function[1] = *(void **) &ptr2;

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

    for (i = 0; i < 8; i++) {
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
        else if ( i == 5 )
            strncpy(fn_name, "flexiblas_backend_ge_num_threads", 127);
        else if ( i == 6 ) {
            strncpy(fn_name, "armpl_omp_get_num_threads",127);
        } else if ( i == 7 ) {
            strncpy(fn_name, "armpl_get_num_threads",127);
        }

        fn_name[127] = '\0';
        if ( i != 1 )
            snprintf(fn2_name, 130, "%s_", fn_name);

        ptr = __flexiblas_dlsym(backend->library_handle, fn_name);
        ptr2 = __flexiblas_dlsym(backend->library_handle, fn2_name);

        if (ptr != NULL || ptr2 != NULL)
            break;
    }
    *(void **) &backend->get_num_threads_function[0] = ptr;
    *(void **) &backend->get_num_threads_function[1] = ptr2;

    if ( ptr ) {
        DPRINTF(1, "Get thread number function ( func_name = %s )  at 0x%lx\n", fn_name, (unsigned long)ptr);
    }
    if ( ptr2 ) {
        DPRINTF(1, "Get thread number function ( func_name = %s )  at 0x%lx\n", fn2_name, (unsigned long)ptr2);
    }

    return;
}

