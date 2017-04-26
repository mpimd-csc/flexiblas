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

#define FLEXIBLAS_VERSION "1.2.0" 
#define FLEXIBLAS_YEARS "2014" 

#include <stdlib.h>
#include <stdio.h> 
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#ifndef __WIN32__ 
#include <dlfcn.h>
#endif 

#include <errno.h>
#include <ctype.h>
#include <assert.h>
#include <sys/time.h>
#define FLEXIBLAS_RC "flexiblasrc" 

#include "f77blas_interface.h"
#include "hooks.h"
#include "hashtable.h" 


#ifndef CMAKE_INSTALL_FULL_SYSCONFDIR 
#define CMAKE_INSTALL_FULL_SYSCONFDIR "/usr/local/etc" 
#endif 



void flexiblas_print_profile(); 

extern void*  __flexiblas_library; 
extern int __flexiblas_initialized; 
struct flexiblas_info __flexiblas_current_blas; 
int __flexiblas_verbose; 

// static char * blas_default_map = NULL; 

#ifndef RTLD_DEEPBIND 
#define RTLD_DEEPBIND 0 
#endif 

#define FLEXIBLAS_ENV_SO_EXTENSION 0x01
#define FLEXIBLAS_ENV_HOMEDIR      0x02
#define FLEXIBLAS_ENV_GLOBAL_RC    0x03
#define FLEXIBLAS_ENV_USER_RC 	   0x04

extern int __flexiblas_count_additional_paths;
extern char **  __flexiblas_additional_paths;


void __flexiblas_print_copyright(int prefix);
char * __flexiblas_getenv(int what);
void __flexiblas_insert_fallback_blas(hashtable table);
void __flexiblas_read_config_file(FILE * fp, hashtable table, char **default_map );
void __flexiblas_load_config ( const char * path, hashtable table , char **default_map ); 
void __flexiblas_add_path(const char * path );
void __flexiblas_free_paths();
void * __flexiblas_dlopen( const char *libname, int flags );

#endif /* end of include guard: FLEXIBLAS_H */
