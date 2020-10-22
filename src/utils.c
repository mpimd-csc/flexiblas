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
 * Copyright (C) Martin Koehler, 2013-2020
 */




#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "flexiblas.h"
#include "helper.h"
#define MAX_BUFFER_SIZE (4096 * 8)

#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif 
#include <unistd.h>
#ifdef HAVE_SYSMACRO_H
#include <sys/sysmacros.h>
#endif 

#include "cscutils/strutils.h"

HIDDEN int __flexiblas_file_exist(const char *path) {
    if ( path == NULL ) return 0;
	if( access( path, F_OK ) == 0  ) {
		return -1;
	} else {
		return 0;
	}
}

HIDDEN int __flexiblas_directory_exists(const char * path)
{
    struct stat statbuffer;
    if ( path == NULL) return 0;
    if ( stat(path, &statbuffer) == -1) return 0;
    if ( S_ISDIR(statbuffer.st_mode)) return 1;
    return 0;
}

HIDDEN int __flexiblas_str_endwith(const char * haystack, const char *needle )
{
     if (!haystack || !needle)
        return 0;
    size_t lenhaystack = strlen(haystack);
    size_t lenneedle = strlen(needle);
    if (lenneedle >  lenhaystack)
        return 0;
    return strncmp(haystack + lenhaystack - lenneedle, needle, lenneedle) == 0;
}

HIDDEN char *__flexiblas_getenv(int what) {
	static char container[MAX_BUFFER_SIZE];
	container[0] = '\0';
	switch (what) {
		case FLEXIBLAS_ENV_SO_EXTENSION:
			#ifdef __APPLE__
			snprintf(container, MAX_BUFFER_SIZE, ".dylib");
			#else
			#ifdef __WIN32__
			snprintf(container, MAX_BUFFER_SIZE, ".dll");
			#else
			snprintf(container, MAX_BUFFER_SIZE, ".so");
			#endif
			#endif
			break;
		case FLEXIBLAS_ENV_HOMEDIR:
			#ifdef __WIN32__
			snprintf(container,MAX_BUFFER_SIZE,"%s\\%s\\",getenv("HOMEDRIVE"),getenv("HOMEPATH"));
			#else
			snprintf(container,MAX_BUFFER_SIZE,"%s",getenv("HOME"));
			#endif
			break;
		case FLEXIBLAS_ENV_GLOBAL_RC:
			#ifdef __WIN32__
			snprintf(container,MAX_BUFFER_SIZE,"%s\\%s", getenv("SYSTEMROOT"), FLEXIBLAS_RC);
			#else
			snprintf(container,MAX_BUFFER_SIZE,"%s/%s",CMAKE_INSTALL_FULL_SYSCONFDIR,FLEXIBLAS_RC);
			#endif
			break;
        case FLEXIBLAS_ENV_GLOBAL_RC_DIR:
            #ifdef __WIN32__
            #warning NOT IMPLEMENTED
            #else
			snprintf(container,MAX_BUFFER_SIZE,"%s/%s/",CMAKE_INSTALL_FULL_SYSCONFDIR,FLEXIBLAS_RC_DIR);
            #endif
            break;
        case FLEXIBLAS_ENV_USER_RC:
			#ifdef __WIN32__
			snprintf(container,MAX_BUFFER_SIZE,"%s\\%s",getenv("APPDATA"), FLEXIBLAS_RC);
			#else
			snprintf(container,MAX_BUFFER_SIZE,"%s/.%s", getenv("HOME"), FLEXIBLAS_RC);
			#endif
			break;
        case FLEXIBLAS_ENV_HOST_RC:
    		#ifdef __WIN32__
            #error Not implemented
			#else
            {
                char hostname[MAX_BUFFER_SIZE-20];
                gethostname(hostname, MAX_BUFFER_SIZE-20);
    			snprintf(container,MAX_BUFFER_SIZE,"%s/.%s.%s", getenv("HOME"), FLEXIBLAS_RC, hostname);
                csc_str_remove_char(container, '"');
                csc_str_remove_char(container, '\"');

            }
			#endif
			break;


		default:
			return NULL;
	}
	return strdup(container);
}

HIDDEN void __flexiblas_print_copyright (int prefix) {
	if (prefix){
        fprintf(stderr, "<%s>\n", PRINT_PREFIX );
		fprintf(stderr, "<" PRINT_PREFIX "> FlexiBLAS, version " FLEXIBLAS_VERSION "\n");
		fprintf(stderr, "<" PRINT_PREFIX "> Copyright (C) " FLEXIBLAS_YEARS " Martin Koehler and others.\n");
		fprintf(stderr, "<" PRINT_PREFIX "> This is free software; see the source code for copying conditions.\n");
		fprintf(stderr, "<" PRINT_PREFIX "> There is ABSOLUTELY NO WARRANTY; not even for MERCHANTABILITY or\n");
		fprintf(stderr, "<" PRINT_PREFIX "> FITNESS FOR A PARTICULAR PURPOSE.\n");
		fprintf(stderr, "<" PRINT_PREFIX "> \n");
	} else {
		printf("FlexiBLAS, version " FLEXIBLAS_VERSION "\n");
		printf("Copyright (C) " FLEXIBLAS_YEARS " Martin Koehler and others.\n");
		printf("This is free software; see the source code for copying conditions.\n");
		printf("There is ABSOLUTELY NO WARRANTY; not even for MERCHANTABILITY or\n");
		printf("FITNESS FOR A PARTICULAR PURPOSE.\n");
		printf("\n");
	}
	return;
}




/*-----------------------------------------------------------------------------
 *  Other Stuff
 *-----------------------------------------------------------------------------*/
HIDDEN int __flexiblas_insert_fallback_blas(flexiblas_mgmt_t *config)
{
    int ret = 0;
	char *SO_EXTENSION = __flexiblas_getenv(FLEXIBLAS_ENV_SO_EXTENSION);
	size_t len=strlen(FALLBACK_NAME)+strlen(SO_EXTENSION)+2;
	char *tmp = (char *) calloc(len,sizeof(char));
	char *tmp2;

	snprintf(tmp,len, "%s%s", FALLBACK_NAME,SO_EXTENSION);
	len = strlen("libflexiblas_netlib")+strlen(SO_EXTENSION)+2;
	tmp2 = (char*) calloc(len,sizeof(char));
	snprintf(tmp2,len, "libflexiblas_netlib%s", SO_EXTENSION);

	if ( flexiblas_mgmt_blas_add(config, FLEXIBLAS_GLOBAL, "NETLIB", tmp2, NULL)){
	    DPRINTF(0,"Can not insert Netlib BLAS library.\n");
        ret ++;
	}
	if (  flexiblas_mgmt_blas_add(config, FLEXIBLAS_GLOBAL, "__FALLBACK__", tmp2, NULL)) {
		DPRINTF(0,"Can not insert Netlib BLAS library as fallback.\n");
        ret++;
	}

	free(tmp);
	free(tmp2);
	free(SO_EXTENSION);
	return ret;
}

