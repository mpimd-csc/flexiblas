/* $Id: flexiblas.c 3739 2013-10-01 07:48:40Z komart $ */ 
/* 
 Copyright (C) 2013  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef FLEXIBLAS_H
#define FLEXIBLAS_H

#define FLEXIBLAS_VERSION "1.0.0" 
#define FLEXIBLAS_YEARS "2013, 2014" 

#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dlfcn.h>
#include <errno.h>
#include <ctype.h>
#include <assert.h>
#ifdef FLEXIBLAS_PROFILE
#include <sys/time.h>
#define FLEXIBLAS_RC "flexiblasrc-profile" 
#else 
#define FLEXIBLAS_RC "flexiblasrc" 
#endif 
#include "f77blas_interface.h"
#include "hooks.h"
#include "hashtable.h" 


#ifndef CMAKE_INSTALL_FULL_SYSCONFDIR 
#define CMAKE_INSTALL_FULL_SYSCONFDIR "/usr/local/etc" 
#endif 

#ifdef FLEXIBLAS_PROFILE
void flexiblas_print_profile(); 
#endif 

void*  __flexiblas_library = NULL; 
int __flexiblas_initialized = 0; 
struct flexiblas_info __flexiblas_current_blas; 
int __flexiblas_verbose; 

// static char * blas_default_map = NULL; 

#ifndef RTLD_DEEPBIND 
#define RTLD_DEEPBIND 0 
#endif 

#ifdef __APPLE__
	#define  SO_EXTENSION ".dylib"
#else 
	#define SO_EXTENSION ".so" 
#endif


#endif /* end of include guard: FLEXIBLAS_H */
