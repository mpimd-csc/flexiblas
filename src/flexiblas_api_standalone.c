//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software: you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program. If not, see <https://www.gnu.org/licenses/>.
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

typedef int (*flexiblas_avail_t) (void);

int flexiblas_avail(void)
{
    int (*fnptr) (void);
    flexiblas_avail_t  ptr_next, ptr_default, ptr_self;
    *(void **) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_avail");
    *(void **) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_avail");
    ptr_self    = &flexiblas_avail;

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

typedef int (*get_color_function_t) ( void );

int flexiblas_get_color_output(void) {
    int (*fnptr)(void);
    get_color_function_t ptr_next, ptr_default, ptr_self;
    *(void **) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_get_color_output");
    *(void **) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_get_color_output");
    ptr_self = &flexiblas_get_color_output;

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

typedef void (*set_color_output_t) (int);

void flexiblas_set_color_output(int s) {
    void (*fnptr)(int);
    set_color_output_t ptr_next, ptr_default, ptr_self;
    *(void **) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_set_color_output");
    *(void **) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_set_color_output");
    ptr_self = &flexiblas_set_color_output;

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

typedef void (*get_version_t)( int *, int *, int *);

void flexiblas_get_version(int *major, int *minor, int *patch)
{
    void (*fnptr) (int *, int*, int *);
    get_version_t ptr_next, ptr_default, ptr_self;
    *(void**) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_get_version");
    *(void**) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_get_version");
    ptr_self = &flexiblas_get_version;

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

typedef void (*print_loaded_backends_t) (FILE *);

void flexiblas_print_loaded_backends(FILE *fp)
{
    void (*fnptr) (FILE *);
    print_loaded_backends_t ptr_next, ptr_default, ptr_self;

    *(void**) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_print_loaded_backends");
    *(void**) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_print_loaded_backends");
    ptr_self = &flexiblas_print_loaded_backends;

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

typedef void (*print_avail_backend_t)(FILE *);

void flexiblas_print_avail_backends(FILE *fp)
{
    void (*fnptr) (FILE *);
    print_avail_backend_t ptr_next, ptr_default, ptr_self;

    *(void **) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_print_avail_backends");
    *(void **) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_print_avail_backends");
    ptr_self = &flexiblas_print_avail_backends;

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

typedef void (*print_current_backend_t) (FILE *);


void flexiblas_print_current_backend(FILE* fp)
{
    void (*fnptr) (FILE *);
    print_current_backend_t ptr_next, ptr_default, ptr_self;
    *(void**) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_print_current_backend");
    *(void**) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_print_current_backend");
    ptr_self = &flexiblas_print_current_backend;

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
typedef ssize_t (*list_t) (char *, size_t, ssize_t);


ssize_t flexiblas_list(char *name, const size_t len, const ssize_t pos)
{
    ssize_t (*fnptr) (char *, size_t, ssize_t);
    list_t ptr_next, ptr_default, ptr_self;
    *(void**) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_list");
    *(void**) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_list");
    ptr_self = &flexiblas_list;

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
    list_t ptr_next, ptr_default, ptr_self;
    *(void **) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_list_loaded");
    *(void **) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_list_loaded");
    ptr_self = &flexiblas_list_loaded;

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

typedef int (*load_backend_t) (const char *);

int flexiblas_load_backend(const char * name )
{
    int (*fnptr) (const char *);
    load_backend_t ptr_next, ptr_default, ptr_self;
    *(void**) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_load_backend");
    *(void**) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_load_backend");
    ptr_self = &flexiblas_load_backend;

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
    load_backend_t ptr_next, ptr_default, ptr_self;
    *(void **) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_load_backend_library");
    *(void **) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_load_backend_library");
    ptr_self = &flexiblas_load_backend_library;

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

typedef int (*switch_t) (int);
int flexiblas_switch(int id)
{
    int (*fnptr) (int);
    switch_t ptr_next, ptr_default, ptr_self;

    *(void **) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_switch");
    *(void **) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_switch");
    ptr_self = &flexiblas_switch;

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

typedef int(*current_backend_t)(char *, size_t);

int flexiblas_current_backend(char *name, size_t len)
{
    int (*fnptr) (char *, size_t);
    current_backend_t ptr_next, ptr_default, ptr_self;
    *(void **) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_current_backend");
    *(void **) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_current_backend");
    ptr_self = &flexiblas_current_backend;

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
typedef void (*set_num_threads_t) (int);
void flexiblas_set_num_threads(int num)
{
    void (*fnptr) (int);
    set_num_threads_t ptr_next, ptr_default, ptr_self;
    *(void**) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_set_num_threads");
    *(void**) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_set_num_threads");
    ptr_self = &flexiblas_set_num_threads;

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
typedef int (*get_num_threads_t) (void);
int flexiblas_get_num_threads(void)
{
    int (*fnptr) (void);
    get_num_threads_t ptr_next, ptr_default, ptr_self;
    *(void**) &ptr_next    = dlsym(RTLD_NEXT, "flexiblas_get_num_threads");
    *(void**) &ptr_default = dlsym(RTLD_DEFAULT, "flexiblas_get_num_threads");
    ptr_self = &flexiblas_get_num_threads;

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

FLEXIBLAS_API_INT flexiblas_get_num_threads_(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

int openblas_get_num_threads(void)
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT openblas_get_num_threads_(void)
{
    return flexiblas_get_num_threads();
}

int mkl_get_num_threads(void)
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT mkl_get_num_threads_(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

int blas_get_num_threads(void)
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT blas_get_num_threads_(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

int acmlgetnumthreads(void)
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT acmlgetnumthreads_(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}


FLEXIBLAS_API_INT bli_thread_get_num_threads(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}



