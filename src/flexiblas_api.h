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

