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



#ifndef FLEXIBLAS_HELPER_H

#define FLEXIBLAS_HELPER_H

#ifdef __cplusplus
extern "C" {
#endif

    HIDDEN extern int __flexiblas_verbose;

    void flexiblas_print_error(const char *prefix, const char *path, const int line, const char *fmt, ... );
    void flexiblas_print_warning(const char *prefix, const char *fmt, ... );
    void flexiblas_print_info(const char *prefix, const char *fmt, ... );

    HIDDEN void * __flexiblas_dlopen( const char *libname, int flags, char **soname );
    HIDDEN int __flexiblas_dl_symbol_exist( const char *libname, const char *symbol_name );
    HIDDEN void * __flexiblas_dlsym(void *lib, const char *fname);
    HIDDEN void __flexiblas_dlclose(void *lib);


#define	DPRINTF( level, ... )	do { if ( __flexiblas_verbose >= (level)) {flexiblas_print_info( "flexiblas", __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#define	DPRINTFP( level, prefix, ... )	do { if ( __flexiblas_verbose >= (level)) {flexiblas_print_info( prefix, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"

#define	DPRINTF_WARN( level, ... )	do { if ( __flexiblas_verbose >= (level)) {flexiblas_print_warning( "flexiblas", __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#define	DPRINTFP_WARN( level, prefix, ... )	do { if ( __flexiblas_verbose >= (level)) {flexiblas_print_warning( prefix, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"

#ifdef DEBUG
#define	DPRINTF_ERROR( level, ... )	    do { if ( __flexiblas_verbose >= (level)) {flexiblas_print_error("flexiblas", __FILE__,__LINE__, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#define	DPRINTFP_ERROR( level, prefix, ... )	do { if ( __flexiblas_verbose >= (level)) {flexiblas_print_error(prefix, __FILE__,__LINE__, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#else
#define	DPRINTF_ERROR( level, ... )	    do { if ( __flexiblas_verbose >= (level)) {flexiblas_print_error("flexiblas", NULL,0, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"
#define	DPRINTFP_ERROR( level, prefix, ... )	do { if ( __flexiblas_verbose >= (level)) {flexiblas_print_error(prefix, NULL,0, __VA_ARGS__); } } while(0) // ... represents the "text" and optionally the "args"

#endif

#ifdef __cplusplus
};
#endif

#endif /* end of include guard: FLEXIBLAS_HELPER_H */
