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
 * Copyright (C) Martin Koehler, 2013-2020
 */

#include <sys/types.h>

#ifndef FLEXIBLAS_API_H
#define FLEXIBLAS_API_H

#ifndef Int
#define Int int
#endif

#ifdef __cplusplus
extern "C" {
#endif
    // #include "flexiblas_structures.h"

    /*-----------------------------------------------------------------------------
     *  External API of FlexiBLAS
     *-----------------------------------------------------------------------------*/

    /* Print Information  */
    extern int flexiblas_avail(void);
    extern int flexiblas_get_color_output(void);
    extern void flexiblas_set_color_output(int s);
    extern void flexiblas_get_version(int *major, int *minor, int *patch);
    extern void flexiblas_print_loaded_backends(FILE *fp);
    extern void flexiblas_print_avail_backends(FILE *fp);
    extern void flexiblas_print_current_backend(FILE* fp);

    /* Handle Backends  */
    extern ssize_t flexiblas_list(char *name, const size_t len, const ssize_t pos);
    extern ssize_t flexiblas_list_loaded(char *name, const size_t len, const ssize_t pos);
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
    extern void bli_thread_set_num_threads(Int num);

    /* Get number of threads  */
    extern int flexiblas_get_num_threads(void);
    extern Int flexiblas_get_num_threads_(void);
    extern int openblas_get_num_threads(void);
    extern Int openblas_get_num_threads_(void);
    extern int mkl_get_num_threads(void);
    extern Int mkl_get_num_threads_(void);
    extern int blas_get_num_threads(void);
    extern Int blas_get_num_threads_(void);
    extern int acmlgetnumthreads(void);
    extern Int acmlgetnumthreads_(void);
    extern Int bli_thread_get_num_threads(void);

#ifdef __cplusplus
};
#endif

#endif /* end of include guard: FLEXIBLAS_API_H */

