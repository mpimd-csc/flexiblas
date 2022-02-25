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



#include <stdio.h>
#include <stdlib.h>

#include <dlfcn.h>
#ifndef RTLD_NEXT
# define RTLD_NEXT  ((void *) -1l)
#endif
#ifndef RTLD_DEFAULT
#define  RTLD_DEFAULT   ((void *) 0)
#endif

#include "flexiblas_api.h"



int flexiblas_avail()
{
    int (*fnptr) ();
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_avail");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_avail");
    void *ptr_self = &flexiblas_avail;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return 0;
    }
    return fnptr();
}

int flexiblas_get_color_output() {
    int (*fnptr)();
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_get_color_output");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_get_color_output");
    void *ptr_self = &flexiblas_get_color_output;

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return 0;
    }
    return fnptr();
}

void flexiblas_set_color_output(int s) {
    void (*fnptr)(int);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_set_color_output");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_set_color_output");
    void *ptr_self = &flexiblas_get_color_output;

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return ;
    }
    fnptr(s);
    return;
}

void flexiblas_get_version(int *major, int *minor, int *patch)
{
    void (*fnptr) (int *, int*, int *);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_get_version");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_get_version");
    void *ptr_self = &flexiblas_get_version;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        *major = -1;
        *minor = -1;
        *patch = -1;
        return ;
    }
    fnptr(major, minor, patch);
    return;
}

void flexiblas_print_loaded_backends(FILE *fp)
{
    void (*fnptr) (FILE *);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_print_loaded_backends");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_print_loaded_backends");
    void *ptr_self = &flexiblas_print_loaded_backends;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return ;
    }
    fnptr(fp);
    return;

}

void flexiblas_print_avail_backends(FILE *fp)
{
    void (*fnptr) (FILE *);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_print_avail_backends");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_print_avail_backends");
    void *ptr_self = &flexiblas_print_avail_backends;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return ;
    }
    fnptr(fp);
    return;

}

void flexiblas_print_current_backend(FILE* fp)
{
    void (*fnptr) (FILE *);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_print_current_backend");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_print_current_backend");
    void *ptr_self = &flexiblas_print_current_backend;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return ;
    }
    fnptr(fp);
    return;

}

/* Handle Backends  */
ssize_t flexiblas_list(char *name, const size_t len, const ssize_t pos)
{
    ssize_t (*fnptr) (char *, size_t, ssize_t);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_list");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_list");
    void *ptr_self = &flexiblas_list;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return -1;
    }
    return fnptr(name, len, pos);
}

ssize_t flexiblas_list_loaded(char *name, size_t len, ssize_t pos)
{
    ssize_t (*fnptr) (char *, size_t, ssize_t);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_list_loaded");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_list_loaded");
    void *ptr_self = &flexiblas_list_loaded;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return -1;
    }
    return fnptr(name, len, pos);

}

int flexiblas_load_backend(const char * name )
{
    int (*fnptr) (const char *);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_load_backend");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_load_backend");
    void *ptr_self = &flexiblas_load_backend;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return -1;
    }
    return fnptr(name);
}

int flexiblas_load_backend_library(const char *libname)
{
    int (*fnptr) (const char *);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_load_backend_library");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_load_backend_library");
    void *ptr_self = &flexiblas_load_backend_library;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return -1;
    }
    return fnptr(libname);

}

int flexiblas_switch(int id)
{
    int (*fnptr) (int);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_switch");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_switch");
    void *ptr_self = &flexiblas_switch;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return -1;
    }
    return fnptr(id);

}

int flexiblas_current_backend(char *name, size_t len)
{
    int (*fnptr) (char *, size_t);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_current_backend");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_current_backend");
    void *ptr_self = &flexiblas_current_backend;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return -1;
    }
    return fnptr(name,len);

}

/* Set number of threads  */
void flexiblas_set_num_threads(int num)
{
    void (*fnptr) (int);
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_set_num_threads");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_set_num_threads");
    void *ptr_self = &flexiblas_set_num_threads;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return;
    }
    fnptr(num);
    return;

}

void flexiblas_set_num_threads_(FLEXIBLAS_API_INT* num)
{
    flexiblas_set_num_threads(*num);
}

void openblas_set_num_threads(int num)
{
    flexiblas_set_num_threads(num);
}

void openblas_set_num_threads_(FLEXIBLAS_API_INT* num)
{
    flexiblas_set_num_threads(*num);
}

void mkl_set_num_threads(int num)
{
    flexiblas_set_num_threads(num);
}

void mkl_set_num_threads_(FLEXIBLAS_API_INT* num)
{
    flexiblas_set_num_threads(*num);
}

void blas_set_num_threads(int num)
{
    flexiblas_set_num_threads(num);
}

void blas_set_num_threads_(FLEXIBLAS_API_INT* num)
{
    flexiblas_set_num_threads(*num);

}

void acmlsetnumthreads(int num)
{
    flexiblas_set_num_threads(num);
}

void acmlsetnumthreads_(FLEXIBLAS_API_INT* num)
{
    flexiblas_set_num_threads(*num);
}

void bli_thread_set_num_threads(FLEXIBLAS_API_INT num)
{
    flexiblas_set_num_threads(num);
}


/* Get number of threads  */
int flexiblas_get_num_threads()
{
    int (*fnptr) ();
    void *ptr_next    = dlsym(RTLD_NEXT, "flexiblas_get_num_threads");
    void *ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_get_num_threads");
    void *ptr_self = &flexiblas_get_num_threads;

    // printf("next: %lx \t default: %lx \t self: %lx\n", ptr_next, ptr_default, ptr_self);

    /*  -lflexiblas_api -lflexiblas */
    if ( ptr_next != NULL && ptr_next != ptr_self) {
        fnptr = ptr_next;
    }
    /*   -lflexiblas -lflexiblas_api */
    else if ( ptr_default != NULL && ptr_default != ptr_self) {
        fnptr = ptr_default;
    }
    else {
        return 1;
    }
    return fnptr();


}

FLEXIBLAS_API_INT flexiblas_get_num_threads_()
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

int openblas_get_num_threads()
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT openblas_get_num_threads_()
{
    return flexiblas_get_num_threads();
}

int mkl_get_num_threads()
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT mkl_get_num_threads_()
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

int blas_get_num_threads()
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT blas_get_num_threads_()
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

int acmlgetnumthreads()
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT acmlgetnumthreads_()
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}


FLEXIBLAS_API_INT bli_thread_get_num_threads()
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}



