/* 
 Copyright (C) 2013, 2014, 2015, 2016  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "flexiblas.h"

#define MAX_BUFFER_SIZE (4096 * 8)

HIDDEN int __flexiblas_file_exist(const char *path) {
	if( access( path, F_OK ) != -1 ) {
		return -1; 
	} else {
		return 0; 
	}
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
                char hostname[MAX_BUFFER_SIZE];
                gethostname(hostname, MAX_BUFFER_SIZE);
    			snprintf(container,MAX_BUFFER_SIZE,"%s/.%s.%s", getenv("HOME"), FLEXIBLAS_RC, hostname);
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
		fprintf(stderr, PRINT_PREFIX "FlexiBLAS, version " FLEXIBLAS_VERSION "\n");
		fprintf(stderr, PRINT_PREFIX "Copyright (C) " FLEXIBLAS_YEARS " Martin Koehler and others.\n");
		fprintf(stderr, PRINT_PREFIX "This is free software; see the source code for copying conditions.\n");
		fprintf(stderr, PRINT_PREFIX "There is ABSOLUTELY NO WARRANTY; not even for MERCHANTABILITY or\n");
		fprintf(stderr, PRINT_PREFIX "FITNESS FOR A PARTICULAR PURPOSE.\n");
		fprintf(stderr, PRINT_PREFIX "\n"); 
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
 *  Path management
 *-----------------------------------------------------------------------------*/
HIDDEN void __flexiblas_add_path(const char * path ) {
	__flexiblas_count_additional_paths++;
	__flexiblas_additional_paths = (char **) realloc( __flexiblas_additional_paths,
			sizeof(char *) * __flexiblas_count_additional_paths);
	DPRINTF(2,"Add additional search path %s\n", path); 
	__flexiblas_additional_paths[__flexiblas_count_additional_paths-1] = strdup(path); 
}

HIDDEN void __flexiblas_free_paths() {
	int i = 0;
	for ( i = 0; i < __flexiblas_count_additional_paths; i++) {
		free(__flexiblas_additional_paths[i]);
	}
	if ( __flexiblas_additional_paths != NULL) free(__flexiblas_additional_paths);
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

