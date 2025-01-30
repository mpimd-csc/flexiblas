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



#include "flexiblas.h"
#include <errno.h>
#include <complex.h>


/*-----------------------------------------------------------------------------
 *  Load CBLAS
 *-----------------------------------------------------------------------------*/
HIDDEN int __flexiblas_load_cblas_function( void * handle , struct flexiblas_blasfn * fn, const char *name)
{
#ifdef FLEXIBLAS_CBLAS
    void *ptr_csymbol = NULL;
    char cname[40];

    /* Quick return  */
    if ( handle == NULL ) {
        return 1;
    }


    snprintf(cname, 39, "cblas_%s", name);
    DPRINTF(3, "Look up: %18s", cname);
    ptr_csymbol = dlsym(handle, cname);

    fn -> cblas_real = ptr_csymbol;
    fn -> cblas_function = ptr_csymbol;

    if ( __flexiblas_verbose > 2) {
        fprintf(stderr, " %s.\n",(fn->cblas_function == NULL)?"failed":"success");
    }

    if (fn->cblas_function == NULL) {
        return 1;
    } else {
        return 0;
    }
#else
    fn->cblas_real = NULL;
    fn->cblas_function = NULL;
    return 0;
#endif
}

/*-----------------------------------------------------------------------------
 *  Fortran Loader
 *-----------------------------------------------------------------------------*/
HIDDEN void * __flexiblas_lookup_fortran_function(void * handle, const char *name)
{
    void *ptr_fsymbol = NULL;
    char fname[40];
    int run = 0;

    if (handle == NULL)
    {
        return NULL;
    }

    DPRINTF(3, "Look up (Fortran Symbol): ");
    for (run = 0; run < 2 ; run++) {
        if (run == 0) {
            snprintf(fname, 39, "%s_", name);
        } else if ( run == 1 ){
            snprintf(fname, 39, "%s", name);
        } else {
            break;
        }
        if ( __flexiblas_verbose > 2) {
            fprintf(stderr, "%10s ", fname);
        }

#ifdef __WIN32__
        ptr_fsymbol = GetProcAddress(handle, fname);
#else
        ptr_fsymbol = dlsym(handle, fname);
#endif
        if (ptr_fsymbol!=NULL) {
            break;
        }
    }
    if ( ptr_fsymbol ) {
        if ( __flexiblas_verbose > 2) {
            fprintf(stderr, " at = 0x%lx  in = 0x%lx\n", (unsigned long)  ptr_fsymbol, (unsigned long) handle );
        }
    } else {
        if ( __flexiblas_verbose > 2) {
            fprintf(stderr, " failed.\n");
        }

    }

    return ptr_fsymbol;
}

HIDDEN int __flexiblas_load_fortran_function( void * handle , struct flexiblas_blasfn * fn, const char *name)
{
    /* Quick return  */
    if ( handle == NULL ) {
        fn ->f77_blas_function = NULL;
        return 1;
    }

    fn -> f77_blas_function = __flexiblas_lookup_fortran_function(handle, name);

    if (fn->f77_blas_function == NULL) {
        return 1;
    } else {
        return 0;
    }
}

#define FUNCTION_POINTER_ASSIGN(DST, SRC) *(void **)&(DST) = (void *)(SRC)

HIDDEN flexiblas_complex_interface_t __flexiblas_get_complex_interface(void *handle)
{
    if (handle == NULL) {
        return FLEXIBLAS_COMPLEX_NONE_INTERFACE;
    }
#if defined(__i386__) || defined (__i686__)
    return FLEXIBLAS_COMPLEX_NONE_INTERFACE;
#else
    void *zdotc_ptr = __flexiblas_lookup_fortran_function(handle, "zdotc");
    if ( zdotc_ptr == NULL) {
        DPRINTF(2, "Could not check complex return value interface. ZDOTC not found.\n");
        return FLEXIBLAS_COMPLEX_NONE_INTERFACE;
    }

#ifdef FLEXIBLAS_INTEGER8
    double complex (*zdotc_gnu)(                  int64_t *, double complex *, int64_t *, double complex *, int64_t *);
    void           (*zdotc_intel)(double complex *, int64_t *, double complex *, int64_t *, double complex *, int64_t *);
    int64_t        zero = 0;
#else
    double complex (*zdotc_gnu)(                  int32_t *, double complex *, int32_t *, double complex *, int32_t *);
    void           (*zdotc_intel)(double complex *, int32_t *, double complex *, int32_t *, double complex *, int32_t *);
    int32_t        zero = 1;
#endif

    FUNCTION_POINTER_ASSIGN(zdotc_gnu, zdotc_ptr);
    FUNCTION_POINTER_ASSIGN(zdotc_intel, zdotc_ptr);

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    double complex retval = 0.0 + 0.0*I;
    double complex zeroc = 0.0 + 0.0*I;
    /** retval = zdotc_gnu(&zero, &zeroc, &zero, &zeroc, &zero); */

    zdotc_intel(&retval, &zero, &zeroc, &zero, &zeroc, &zero);
    if (creal(retval) == 0.0 && cimag(retval) == 0.0) {
        return FLEXIBLAS_COMPLEX_INTEL_INTERFACE;
    }

    retval = 0.0 + 1.0*I;
    retval = zdotc_gnu(&zero, &zeroc, &zero, &zeroc, &zero);
    if (creal(retval) == 0.0 && cimag(retval) == 0.0) {
        return FLEXIBLAS_COMPLEX_GNU_INTERFACE;
    }
#else
    /*
     * ZDOTC will return zero if all inputs are zero. Especially it overwrites existing data.
     * Ifi the the retval arguments gets zero, we are using the "Intel" calling convention.
     */

    double complex retval = 0.0 + 1.0*I;
    double complex zeroc = 0.0 + 0.0*I;

    zdotc_intel(&retval, &zero, &zeroc, &zero, &zeroc, &zero);
    if (creal(retval) == 0.0 && cimag(retval) == 0.0) {
        return FLEXIBLAS_COMPLEX_INTEL_INTERFACE;
    }

    retval = 0.0 + 1.0*I;
    retval = zdotc_gnu(&zero, &zeroc, &zero, &zeroc, &zero);
    if (creal(retval) == 0.0 && cimag(retval) == 0.0) {
        return FLEXIBLAS_COMPLEX_GNU_INTERFACE;
    }
#endif
    return FLEXIBLAS_COMPLEX_NONE_INTERFACE;

#endif
}

HIDDEN flexiblas_interface_t __flexiblas_get_interface(void *handle)
{
    if (handle == NULL) {
        return FLEXIBLAS_INTERFACE_NONE;
    }

#if defined(__i386__) || defined(__i686__)
    return FLEXIBLAS_INTERFACE_LP64;
#else
    int64_t (*isamax_function)(int64_t *, float *, int64_t *);
    void *isamax_ptr = __flexiblas_lookup_fortran_function(handle, "isamax");
    if ( isamax_ptr == NULL) {
        DPRINTF(2, "Could not check lp64/ilp64 interface. ISAMAX not found. \n");
        return FLEXIBLAS_INTERFACE_NONE;
    }

    FUNCTION_POINTER_ASSIGN(isamax_function, isamax_ptr);

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    float X[3] = {1.0f, 100.0f, 1.0f};
    int64_t n = 3;
    int64_t incx = 1;

    int64_t idx = isamax_function(&n, X, &incx);

    // This means the `isamax()` implementation saw `N == 0`, n(63:32) interpreted as 32bit value.
    if (idx == 0) {
        return FLEXIBLAS_INTERFACE_LP64;
    }

    // This means the `isamax()` implementation saw `N == 3`, n(63:0) interpreted as 64bit value.
    if (idx == 2) {
        return FLEXIBLAS_INTERFACE_ILP64;
    }
#else

    float X[3] = {1.0f, 100.0f, 1.0f};
    int64_t n = 0xffffffff00000003;
    int64_t incx = 1;

    /* It seems that big endian does not influence the alignment in the return value */
    int64_t idx = isamax_function(&n, X, &incx);
    idx = idx & 0xffffffff;

    // This means the `isamax()` implementation saw `N < 0`, n interpreted as 64bit value.
    if (idx == 0) {
        return FLEXIBLAS_INTERFACE_ILP64;
    }
    // This means the `isamax()` implementation saw `N == 3`, n interpreted as 32bit value.
    if (idx == 2) {
        return FLEXIBLAS_INTERFACE_LP64;
    }

#endif
    return FLEXIBLAS_INTERFACE_NONE;
#endif
}

