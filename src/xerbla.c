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


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <complex.h>
#include <stdarg.h>

#include "flexiblas.h"
#include "flexiblas_fortran_char_len.h"

#ifndef __WIN32__
#  include <dlfcn.h>
#  ifndef RTLD_DEFAULT
#    define RTLD_DEFAULT   ((void *) 0)
#  endif
#else
#  include <psapi.h>
#endif


#ifdef __WIN32__
// FIXME: This is partly repeated from flexiblas_api_standalone.c
void get_default_symbol(const char *symbol_name, FARPROC *global) {
    // Emulate the symbol lookup for dlsym(RTLD_DEFAULT, "...").
    HMODULE hMods[1024];
    HANDLE hProcess = GetCurrentProcess();
    DWORD cbNeeded;
    FARPROC procAddress = NULL;
    int global_index = 0;
    global = NULL;

    if (EnumProcessModules(hProcess, hMods, sizeof(hMods), &cbNeeded)) {
        for (int i = 0; i < (cbNeeded / sizeof(HMODULE)); i++) {
            procAddress = GetProcAddress(hMods[i], symbol_name);
            if (procAddress != NULL) {
                *global = procAddress;
                global_index = i;
                break;
            }
        }
    }

    return;
}
#endif


// static int user_xerbla = 0;
void flexiblas_internal_xerbla(char *SNAME, Int *Info, flexiblas_fortran_charlen_t len);
#ifdef FLEXIBLAS_ABI_IBM
#ifdef __ELF__
#pragma weak xerbla_
#pragma weak xerbla
void xerbla_(char *, Int *, flexiblas_fortran_charlen_t) __attribute__ (( alias ("flexiblas_internal_xerbla")));
void xerbla(char *, Int *, flexiblas_fortran_charlen_t) __attribute__ (( alias ("flexiblas_internal_xerbla")));
void XERBLA(char *, Int *, flexiblas_fortran_charlen_t) __attribute__ (( alias ("flexiblas_internal_xerbla")));
#else
void xerbla_(char *SNAME, Int *Info, flexiblas_fortran_charlen_t) {
    flexiblas_internal_xerbla(SNAME, Info, len);
}
void xerbla(char *SNAME, Int *Info, flexiblas_fortran_charlen_t len) {
    flexiblas_internal_xerbla(SNAME, Info, len);
}
void XERBLA(char *SNAME, Int *Info, flexiblas_fortran_charlen_t len) {
    flexiblas_internal_xerbla(SNAME, Info, len);
}
#endif

#else
#if defined(__ELF__) || ((defined (__PGI) || defined(__NVCOMPILER)) && (defined(__linux__)  || defined(__unix__)))
void xerbla_(char *, Int *, flexiblas_fortran_charlen_t) __attribute__ ((weak, alias ("flexiblas_internal_xerbla")));
void xerbla (char *, Int *, flexiblas_fortran_charlen_t) __attribute__ ((weak, alias ("flexiblas_internal_xerbla")));
void XERBLA (char *, Int *, flexiblas_fortran_charlen_t) __attribute__ ((weak, alias ("flexiblas_internal_xerbla")));

#else
#pragma weak xerbla_
#pragma weak xerbla
#pragma weak XERBLA
void xerbla_(char *SNAME, Int *Info, flexiblas_fortran_charlen_t len) {
    flexiblas_internal_xerbla(SNAME, Info, len);
}
void xerbla(char *SNAME, Int *Info, flexiblas_fortran_charlen_t len) {
    flexiblas_internal_xerbla(SNAME, Info, len);
}
void XERBLA(char *SNAME, Int *Info, flexiblas_fortran_charlen_t len) {
    flexiblas_internal_xerbla(SNAME, Info, len);
}
#endif
#endif


int __flexiblas_setup_xerbla(flexiblas_backend_t *backend)
{
    /* Check if the user supplied a XERBLA function  */
    {
        int user_xerbla = 0;
#ifdef __WIN32__
        void *xerbla_symbol1 = GetProcAddress(backend->library_handle, "xerbla_");
        void *xerbla_symbol2;
        get_default_symbol("xerbla_", (FARPROC *) &xerbla_symbol2);
#else
        void *xerbla_symbol1 = dlsym(backend->library_handle,"xerbla_");
        void *xerbla_symbol2 = dlsym(RTLD_DEFAULT,"xerbla_");
#endif
        void (*flexiblas_internal) (char *, Int *, flexiblas_fortran_charlen_t);
        void *internal;
        flexiblas_internal = flexiblas_internal_xerbla;
        *(void **) &internal = *((void**) & flexiblas_internal);
        DPRINTF(1, "Available XERBLA ( backend: 0x%lx, user defined: 0x%lx, FlexiBLAS: 0x%lx )\n",
                (unsigned long)((void*)xerbla_symbol1),
                (unsigned long)((void*)xerbla_symbol2),
                (unsigned long)((void*)internal));

        if (internal == xerbla_symbol2) {
            user_xerbla = 0;
        } else {
            user_xerbla = 1;
        }

        if ( user_xerbla == 0 ){
            DPRINTF(1,"Use XERBLA of the BLAS backend.\n");
            backend->xerbla = xerbla_symbol1;
        } else {
            DPRINTF(1,"Use XERBLA supplied by the user.\n");
            backend->xerbla = xerbla_symbol2;
        }
    }
    return 0;
}

void flexiblas_internal_xerbla(char *SNAME, Int *Info, flexiblas_fortran_charlen_t len)  {
    void (*fn) (char *SNAME, Int *info, flexiblas_fortran_charlen_t len)  ;
    *(void**) &fn = current_backend->xerbla;

    if ( fn == NULL ) {
        int _info = (int) *Info;
        char * ptr = malloc ( sizeof(char) * (len + 1));
        strncpy(ptr, SNAME, len);
        ptr[len] = '\0';
        fprintf(stderr,"XERBLA: Parameter %d was incorrect on entry to %s\n", _info , ptr);
        free(ptr);
        return;
    }
    fn (SNAME, Info, len);
    return;
}



/*
 * CBLAS related stuff
 */

#ifdef FLEXIBLAS_CBLAS
int RowMajorStrg = 0;


#define CBLAS_INT int32_t

#if defined(__ELF__) || ((defined (__PGI) || defined(__NVCOMPILER)) && (defined(__linux__)  || defined(__unix__))) || defined(__MINGW32__)
void internal_cblas_xerbla(CBLAS_INT info, const char *rout, const char *form, ...);
void cblas_xerbla(CBLAS_INT info, const char *, const char *, ...) __attribute__ ((weak, alias ("internal_cblas_xerbla")));
void internal_cblas_xerbla(CBLAS_INT _info, const char *rout, const char *form, ...)
#else
void internal_cblas_xerbla(CBLAS_INT _info, const char *rout, const char *form, ...)
#endif
{
    extern int RowMajorStrg;
    char empty[1] = "";
    va_list argptr;

    va_start(argptr, form);
    Int info = _info; 

    if (RowMajorStrg)
    {
        if (strstr(rout,"gemm") != 0)
        {
            if      (info == 5 ) info =  4;
            else if (info == 4 ) info =  5;
            else if (info == 11) info =  9;
            else if (info == 9 ) info = 11;
        }
        else if (strstr(rout,"symm") != 0 || strstr(rout,"hemm") != 0)
        {
            if      (info == 5 ) info =  4;
            else if (info == 4 ) info =  5;
        }
        else if (strstr(rout,"trmm") != 0 || strstr(rout,"trsm") != 0)
        {
            if      (info == 7 ) info =  6;
            else if (info == 6 ) info =  7;
        }
        else if (strstr(rout,"gemv") != 0)
        {
            if      (info == 4)  info = 3;
            else if (info == 3)  info = 4;
        }
        else if (strstr(rout,"gbmv") != 0)
        {
            if      (info == 4)  info = 3;
            else if (info == 3)  info = 4;
            else if (info == 6)  info = 5;
            else if (info == 5)  info = 6;
        }
        else if (strstr(rout,"ger") != 0)
        {
            if      (info == 3) info = 2;
            else if (info == 2) info = 3;
            else if (info == 8) info = 6;
            else if (info == 6) info = 8;
        }
        else if ( (strstr(rout,"her2") != 0 || strstr(rout,"hpr2") != 0)
                && strstr(rout,"her2k") == 0 )
        {
            if      (info == 8) info = 6;
            else if (info == 6) info = 8;
        }
    }

    if (info)
        fprintf(stderr, "Parameter %d to routine %s was incorrect\n", (int) info, rout);
    vfprintf(stderr, form, argptr);
    va_end(argptr);
    if (info) {
        if ( !info) {
            FC_GLOBAL(xerbla,XERBLA)(empty, &info, 0);
        }
    }
}


#ifdef __ELF__
CBLAS_INT  internal_cblas_errprn(CBLAS_INT ierr, CBLAS_INT info, const char *form, ...);
CBLAS_INT  cblas_errprn(CBLAS_INT ierr, CBLAS_INT info, const char *, ...) __attribute__ ((weak, alias ("internal_cblas_errprn")));
CBLAS_INT  internal_cblas_errprn(CBLAS_INT ierr, CBLAS_INT info, const char *form, ...)
#else
CBLAS_INT cblas_errprn(CBLAS_INT ierr, CBLAS_INT info,const char *form, ...)
#endif
{

    va_list argptr;

    va_start(argptr, form);
#ifdef GCCWIN
    vprintf(form, argptr);
#else
    vfprintf(stderr, form, argptr);
#endif
    va_end(argptr);
    if ( ierr < info )
        return ierr;
    else
        return info;
}
#if (defined(__APPLE__) || defined(__WIN32__))
/* This routine is designed for MacOS */
void internal_cblas_xerbla(int info, const char *rout, const char *form, ...);
void cblas_xerbla(int info, const char *rout, const char *form, ...)
{
    // printf("Hier in xerbla.c (0x%lx) backend = 0x%lx \n", (unsigned long)(void*) & cblas_xerbla, (unsigned long) current_backend->xerbla.cblas_function);
    if ( current_backend->cblas_xerbla != NULL) {
        va_list ap;
        void (*fn) ( int, const char*, const char*, ...);
        size_t a1, a2, a3, a4, a5;

        fn = current_backend->cblas_xerbla;
        va_start(ap, form);
        fn(info, rout, form, a1, a2, a3, a4, a5);
        va_end(ap);
    } else {
        va_list ap;
        void (*fn) ( int, const char*, const char*, ...);
        size_t a1, a2, a3, a4, a5;

        fn = current_backend->cblas_xerbla;
        va_start(ap, form);
        internal_cblas_xerbla(info, rout, form, a1, a2, a3, a4, a5);
        va_end(ap);
    }
}
#endif

int __flexiblas_setup_cblas_xerbla(flexiblas_backend_t *backend)
{
#ifndef __WIN32__
    /* Check if the user supplied a XERBLA function  */
    {
        int user_xerbla = 0;
        void *xerbla_symbol1 = dlsym(backend->library_handle,"cblas_xerbla");
        void *xerbla_symbol2 = dlsym(RTLD_DEFAULT,"cblas_xerbla");
#ifndef __APPLE__
        void (*flexiblas_internal)(int, const char *, const char *, ...);
        flexiblas_internal = internal_cblas_xerbla;
        void *internal;
        *(void **) &internal = *((void**) & flexiblas_internal);
        DPRINTF(1, "Available CBLAS_XERBLA ( backend: 0x%lx, user defined: 0x%lx, FlexiBLAS: 0x%lx )\n",
                (unsigned long)((void*)xerbla_symbol1),
                (unsigned long)((void*)xerbla_symbol2),
                (unsigned long)((void*)internal));
#else
        void (*flexiblas_internal) (int, const char *, const char *, ...);
        void *internal;
        *(void**) &internal = (void*) &cblas_xerbla;
        DPRINTF(1, "Available CBLAS_XERBLA ( backend: 0x%lx, user defined: 0x%lx, FlexiBLAS: 0x%lx)\n",
                (unsigned long)((void*)xerbla_symbol1),
                (unsigned long)((void*)xerbla_symbol2),
                (unsigned long)((void*)internal));

#endif
        if (internal == xerbla_symbol2) {
            user_xerbla = 0;
        } else {
            user_xerbla = 1;
        }

        if ( user_xerbla == 0 ){
            DPRINTF(1,"Use XERBLA of the BLAS backend.\n");
            backend->cblas_xerbla = xerbla_symbol1;
        } else {
            DPRINTF(1, "Use XERBLA supplied by the user.\n");
            backend->cblas_xerbla = xerbla_symbol2;
        }
    }
#endif
    return 0;
}

#endif


