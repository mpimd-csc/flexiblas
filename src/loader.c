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

#include "flexiblas_fortran_mangle.h"
#include <stdarg.h>

/*-----------------------------------------------------------------------------
 *  Load CBLAS
 *-----------------------------------------------------------------------------*/
HIDDEN void * __flexiblas_lookup_cblas_function( void * handle , ...)
{
#ifdef FLEXIBLAS_CBLAS
    va_list args;
    void *ptr_csymbol = NULL;
    char *name = NULL;
    /* Quick return  */
    if ( handle == NULL ) {
        return NULL;
    }

    va_start(args, handle);

    dlerror();
    while ( (name = va_arg(args, char*)) != NULL) {
        DPRINTF(3, "Look up (C Symbol): %21s", name);
        ptr_csymbol = __flexiblas_dlsym(handle, cname);

        if ( __flexiblas_verbose > 2) {
            fprintf(stderr, " %s at = 0x%lx in = 0x%lx.\n",(ptr_csymbol == NULL)?"failed":"success", (unsigned long) ptr_csymbol, (unsigned long) handle);
        }
        if ( ptr_csymbol != NULL)
            break;

    }
    va_end(args);
    return ptr_csymbol;
#else
    return NULL;
#endif
}

/*-----------------------------------------------------------------------------
 *  Fortran Loader
 *-----------------------------------------------------------------------------*/
HIDDEN void * __flexiblas_lookup_fortran_function(void * handle, ...)
{
    void *ptr_fsymbol = NULL;
    char fname[40];
    int run = 0;
    va_list args;
    char *name;

    if (handle == NULL)
    {
        return NULL;
    }

    va_start(args, handle);

    while ( (name = va_arg(args, char*)) != NULL) {
        DPRINTF(3, "Look up (Fortran Symbol): ");
        for (run = 0; run < 3 ; run++) {
            if (run == 0) {
                snprintf(fname, 39, "%s_", name);
            } else if ( run == 1 ){
                snprintf(fname, 39, "%s", name);
                for (char * p = &fname[0]; *p; ++p) { *p = tolower(*p); }
            } else if ( run == 2 ){
                snprintf(fname, 39, "%s", name);
                for (char * p = &fname[0]; *p; ++p) { *p = toupper(*p); }
            } else {
                break;
            }

            if ( __flexiblas_verbose > 2) {
                fprintf(stderr, "%18s", fname);
            }

            ptr_fsymbol = __flexiblas_dlsym(handle, fname);

        if (ptr_fsymbol!=NULL) {

            if (ptr_fsymbol!=NULL) {
                break;
            }
        }
        if ( ptr_fsymbol )
            break;
    }
    va_end(args);
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



#define FUNCTION_POINTER_ASSIGN(DST, SRC) *(void **)&(DST) = (void *)(SRC)

HIDDEN flexiblas_complex_interface_t __flexiblas_get_complex_interface(void *handle)
{
    if (handle == NULL) {
        return FLEXIBLAS_COMPLEX_NONE_INTERFACE;
    }
#if defined(__i386__) || defined (__i686__)
    return FLEXIBLAS_COMPLEX_NONE_INTERFACE;
#else
    void *zdotc_ptr = __flexiblas_lookup_fortran_function(handle, "zdotc", NULL);
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


HIDDEN int __flexiblas_get_f2c_float_return(void *handle)
{
    if (handle == NULL) {
        return 0;
    }
    void *sdot_ptr = __flexiblas_lookup_fortran_function(handle, "sdot");
    if ( sdot_ptr == NULL) {
        DPRINTF(2, "Could not check for defect of functions with real return value. SDOT not found.\n");
        return 0;
    }

    float retval = 0.0;
    float x[2] = {1.0, 2.0};
    float y[2] = {3.0, 4.0};
#ifdef FLEXIBLAS_INTEGER8
    int64_t n = 2;
    int64_t one = 1;
#else
    int32_t n = 2;
    int32_t one = 1;
#endif
#ifdef FLEXIBLAS_INTEGER8
    float (*sdot_run)(int64_t *, float *, int64_t *, float *, int64_t *);
#else
    float (*sdot_run)(int32_t *, float *, int32_t *, float *, int32_t *);
#endif
	FUNCTION_POINTER_ASSIGN(sdot_run, sdot_ptr);

    retval = sdot_run(&n, x, &one, y, &one);

    if (retval == 11.0)
        return 0;
    else
        return 1;
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
    void *isamax_ptr = __flexiblas_lookup_fortran_function(handle, "isamax", NULL);
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


/*
 * Load a hook function
 */
#ifdef FLEXIBLAS_HOOK_API
HIDDEN int __flexiblas_load_fortran_hook_function( void * handle , struct flexiblas_hook_fn *ptr, const char *name)
{
    char fname[40];
    void * ptr_hsymbol = NULL;
    int run = 0;

    if ( handle == NULL) return 0;

    /* Load Hook if available */
    DPRINTF(3, "Look up hook: ");
    for (run = 0; run < 3 ; run++) {
        if (run == 0) {
            snprintf(fname, 39, "hook_%s", name);
        } else if ( run == 1 ){
            snprintf(fname, 39, "hook_%s_", name);
        } else if ( run == 2 ){
            snprintf(fname, 39, "hook_%s__", name);
        } else {
            break;
        }
        if ( __flexiblas_verbose > 2) {
            fprintf(stderr, "%s ", fname);
        }

#ifdef __WIN32__
        ptr_hsymbol = (void *) GetProcAddress(handle, fname);
#else
        ptr_hsymbol = dlsym(handle, fname);
#endif

        if (ptr_hsymbol!=NULL) {
            break;
        }
    }
    if ( __flexiblas_verbose > 2) {
        fprintf(stderr, "%s\n", (ptr_hsymbol==NULL)?("not found."):("found."));
    }
    if ( ptr_hsymbol ) {
        int p = ptr->nhook;
        ptr->hook_function[p] = ptr_hsymbol;
        ptr->nhook++;
        if ( ptr->nhook >= FLEXIBLAS_MAX_HOOKS) {
            DPRINTF_WARN(0, "Maximum number of installable hooks reached for %s. This may cause problems.\n");
            ptr->nhook = FLEXIBLAS_MAX_HOOKS-1;
        }
    }

    return 0;
}

HIDDEN int __flexiblas_load_cblas_hook_function( void * handle , struct flexiblas_hook_fn *ptr, const char *name)
{
    char fname[40];
    void * ptr_hsymbol = NULL;
    int run = 0;

    if ( handle == NULL) return 0;

    /* Load Hook if available */
    DPRINTF(3, "Look up hook: ");
    for (run = 0; run < 1 ; run++) {
        if (run == 0) {
            snprintf(fname, 39, "hook_%s", name);
        } else {
            break;
        }
        if ( __flexiblas_verbose > 2) {
            fprintf(stderr, "%s ", fname);
        }

        ptr_hsymbol = dlsym(handle, fname);

        if (ptr_hsymbol!=NULL) {
            break;
        }
    }
    if ( __flexiblas_verbose > 2) {
        fprintf(stderr, "%s\n", (ptr_hsymbol==NULL)?("not found."):("found."));
    }
    if ( ptr_hsymbol ) {
        int p = ptr->nhook;
        ptr->hook_function[p] = ptr_hsymbol;
        ptr->nhook++;
        if ( ptr->nhook >= FLEXIBLAS_MAX_HOOKS) {
            DPRINTF_WARN(0, "Maximum number of installable hooks reached for %s. This may cause problems.\n");
            ptr->nhook = FLEXIBLAS_MAX_HOOKS-1;
        }
    }

    return 0;
}


#endif
