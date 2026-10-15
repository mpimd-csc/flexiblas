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
#include <stdlib.h>
#include <unistd.h>

#include <dlfcn.h>
#ifndef RTLD_NEXT
# define RTLD_NEXT  ((void *) -1l)
#endif
#ifndef RTLD_DEFAULT
#define  RTLD_DEFAULT   ((void *) 0)
#endif

#ifndef HIDDEN
#define HIDDEN 
#endif 

#include "flexiblas_api.h"
#include "helper.h" 

#define FLEXIBLAS_LOAD_FUNC(name, rettype, args, retgood, retbad) \
  typedef rettype (*name ## _t) args; \
  rettype name args \
  { \
      rettype (*fnptr) args; \
      name ## _t  ptr_next, ptr_default, ptr_self; \
      *(void **) &ptr_next    = __flexiblas_dlsym(RTLD_NEXT, #name); \
      *(void **) &ptr_default = __flexiblas_dlsym(RTLD_DEFAULT, #name); \
      ptr_self    = &name; \
      /* printf("next: %p \t default: %p \t self: %p\n", ptr_next, ptr_default, ptr_self); */ \
      /*  -lflexiblas_api -lflexiblas */ \
      if ( ptr_next != NULL && ptr_next != ptr_self) { \
          fnptr = ptr_next; \
      } \
      /*   -lflexiblas -lflexiblas_api */ \
      else if ( ptr_default != NULL && ptr_default != ptr_self) { \
          fnptr = ptr_default; \
      } \
      else { \
          retbad; \
      } \
      retgood; \
  }

FLEXIBLAS_LOAD_FUNC(flexiblas_avail, int,
  (void), return fnptr(), return 0)
FLEXIBLAS_LOAD_FUNC(flexiblas_get_color_output, int,
  (void), return fnptr(), return 0)
FLEXIBLAS_LOAD_FUNC(flexiblas_set_color_output, void,
  (int s), fnptr(s);return, return)
FLEXIBLAS_LOAD_FUNC(flexiblas_get_version, void,
  (int *major, int *minor, int *patch),
  fnptr(major, minor, patch); return,
  *major = -1; *minor = -1; *patch = -1; return)
FLEXIBLAS_LOAD_FUNC(flexiblas_print_loaded_backends, void,
  (FILE *fp), fnptr(fp);return, return)
FLEXIBLAS_LOAD_FUNC(flexiblas_print_avail_backends, void,
  (FILE *fp), fnptr(fp);return, return)
FLEXIBLAS_LOAD_FUNC(flexiblas_print_current_backend, void,
  (FILE *fp), fnptr(fp);return, return)

/* Handle Backends  */
FLEXIBLAS_LOAD_FUNC(flexiblas_list, ssize_t,
  (char *name, const size_t len, const ssize_t pos),
  return fnptr(name, len, pos), return -1)
FLEXIBLAS_LOAD_FUNC(flexiblas_list_loaded, ssize_t,
  (char *name, const size_t len, const ssize_t pos),
  return fnptr(name, len, pos), return -1)
FLEXIBLAS_LOAD_FUNC(flexiblas_load_backend, int,
  (const char *name), return fnptr(name), return -1)
FLEXIBLAS_LOAD_FUNC(flexiblas_load_backend_library, int,
  (const char *libname), return fnptr(libname), return -1)
FLEXIBLAS_LOAD_FUNC(flexiblas_switch, int,
  (int id), return fnptr(id), return -1)
FLEXIBLAS_LOAD_FUNC(flexiblas_current_backend, int,
  (char *name, size_t len), return fnptr(name,len), return -1)

/* Set number of threads  */
FLEXIBLAS_LOAD_FUNC(flexiblas_set_num_threads, void,
  (int num), fnptr(num);return, return)

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

void MKL_Set_Num_Threads(int num)
{
    flexiblas_set_num_threads(num);
}

void mkl_set_num_threads_(FLEXIBLAS_API_INT* num)
{
    flexiblas_set_num_threads(*num);
}

#ifdef mkl_set_num_threads
#undef mkl_set_num_threads
#endif
void mkl_set_num_threads(FLEXIBLAS_API_INT* num)
{
    flexiblas_set_num_threads(*num);
}

void MKL_SET_NUM_THREADS(FLEXIBLAS_API_INT* num)
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

void armpl_set_num_threads(int num)
{
    flexiblas_set_num_threads(num);
}

void armpl_omp_set_num_threads(int num)
{
    flexiblas_set_num_threads(num);
}

void armpl_set_num_threads_(FLEXIBLAS_API_INT* num)
{
    flexiblas_set_num_threads(*num);
}

void armpl_omp_set_num_threads_(FLEXIBLAS_API_INT* num)
{
    flexiblas_set_num_threads(*num);
}


/* Get number of threads  */
FLEXIBLAS_LOAD_FUNC(flexiblas_get_num_threads, int,
  (void), return fnptr(), return 1)

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

#ifdef mkl_get_num_threads
#undef mkl_get_num_threads
#endif
int MKL_Get_Num_Threads(void)
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT mkl_get_num_threads(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT mkl_get_num_threads_(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT MKL_GET_NUM_THREADS(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

#ifdef mkl_get_max_threads
#undef mkl_get_max_threads
#endif
int MKL_Get_Max_Threads(void)
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT mkl_get_max_threads(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT mkl_get_max_threads_(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT MKL_GET_MAX_THREADS(void)
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

int armpl_get_num_threads(void)
{
    return flexiblas_get_num_threads();
}

int armpl_omp_get_num_threads(void)
{
    return flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT armpl_get_num_threads_(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}

FLEXIBLAS_API_INT armpl_omp_get_num_threads_(void)
{
    return (FLEXIBLAS_API_INT) flexiblas_get_num_threads();
}
