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
