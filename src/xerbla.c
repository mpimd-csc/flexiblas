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
 * Copyright (C) Martin Koehler, 2013-2023
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
#ifndef __WIN32__
#include <dlfcn.h>
#ifndef RTLD_DEFAULT
# define RTLD_DEFAULT   ((void *) 0)
#endif
#endif

// static int user_xerbla = 0;
void flexiblas_internal_xerbla(char *SNAME, Int *Info, Int len);
#ifdef FLEXIBLAS_ABI_IBM
#ifdef __ELF__
#pragma weak xerbla_
#pragma weak xerbla
void xerbla_(char *, Int *, Int) __attribute__ (( alias ("flexiblas_internal_xerbla")));
void xerbla(char *, Int *, Int) __attribute__ (( alias ("flexiblas_internal_xerbla")));
#else
void xerbla_(char *SNAME, Int *Info, Int len) {
	flexiblas_internal_xerbla(SNAME, Info, len);
}
void xerbla(char *SNAME, Int *Info, Int len) {
	flexiblas_internal_xerbla(SNAME, Info, len);
}
#endif

#else
#ifdef __ELF__
void xerbla_(char *, Int *, Int) __attribute__ ((weak, alias ("flexiblas_internal_xerbla")));
void xerbla(char *, Int *, Int) __attribute__ ((weak, alias ("flexiblas_internal_xerbla")));
#else
#pragma weak xerbla_
#pragma weak xerbla
void xerbla_(char *SNAME, Int *Info, Int len) {
	flexiblas_internal_xerbla(SNAME, Info, len);
}
void xerbla(char *SNAME, Int *Info, Int len) {
	flexiblas_internal_xerbla(SNAME, Info, len);
}
#endif
#endif


int __flexiblas_setup_xerbla(flexiblas_backend_t *backend)
{
#ifndef __WIN32__
	/* Check if the user supplied a XERBLA function  */
	{
        int user_xerbla = 0;
		void *xerbla_symbol1 = dlsym(backend->library_handle,"xerbla_");
		void *xerbla_symbol2 = dlsym(RTLD_DEFAULT,"xerbla_");
	    void (*flexiblas_internal) (char *, Int *, Int);
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
			backend->xerbla.f77_blas_function = xerbla_symbol1;
		} else {
			DPRINTF(1,"Use XERBLA supplied by the user.\n");
			backend->xerbla.f77_blas_function = xerbla_symbol2;
		}
	}
#endif
	return 0;
}

void flexiblas_internal_xerbla(char *SNAME, Int *Info, Int len)  {
	void (*fn) (char *SNAME, Int *info, Int len)  ;
	*(void**) &fn = current_backend->xerbla.f77_blas_function;

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

#ifndef __APPLE__
extern void internal_cblas_xerbla(int info, const char *rout, const char *form, ...);
#else
/* This routine is designed for MacOS */
void internal_cblas_xerbla(int info, const char *rout, const char *form, ...);
void cblas_xerbla(int info, const char *rout, const char *form, ...)
{
	// printf("Hier in xerbla.c (0x%lx) backend = 0x%lx \n", (unsigned long)(void*) & cblas_xerbla, (unsigned long) current_backend->xerbla.cblas_function);
	if ( current_backend->xerbla.cblas_function != NULL) {
		va_list ap;
		void (*fn) ( int, const char*, const char*, ...);
		size_t a1, a2, a3, a4, a5;

		fn = current_backend->xerbla.cblas_function;
		va_start(ap, form);
		fn(info, rout, form, a1, a2, a3, a4, a5);
		va_end(ap);
	} else {
		va_list ap;
		void (*fn) ( int, const char*, const char*, ...);
		size_t a1, a2, a3, a4, a5;

		fn = current_backend->xerbla.cblas_function;
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
			backend->xerbla.cblas_function = xerbla_symbol1;
		} else {
			DPRINTF(1, "Use XERBLA supplied by the user.\n");
			backend->xerbla.cblas_function = xerbla_symbol2;
		}
	}
#endif
	return 0;
}

#endif


