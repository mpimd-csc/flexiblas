/* $Id$ */
/* 
 Copyright (C) 2013, 2014  Martin Köhler, koehlerm@mpi-magdeburg.mpg.de

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

int __flexiblas_file_exist(const char *path) {
	if( access( path, F_OK ) != -1 ) {
		return -1; 
	} else {
		return 0; 
	}
}

char *__flexiblas_getenv(int what) {
	char container[MAX_BUFFER_SIZE];
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
		default:
			return NULL;
	}
	return strdup(container);
}

void __flexiblas_print_copyright (int prefix) {
	if (prefix){
		fprintf(stderr, PRINT_PREFIX "flexiblas, version " FLEXIBLAS_VERSION "\n");
		fprintf(stderr, PRINT_PREFIX "Copyright (C) " FLEXIBLAS_YEARS " Martin Koehler and others.\n");
		fprintf(stderr, PRINT_PREFIX "This is free software; see the source code for copying conditions.\n");
		fprintf(stderr, PRINT_PREFIX "There is ABSOLUTELY NO WARRANTY; not even for MERCHANTABILITY or\n");
		fprintf(stderr, PRINT_PREFIX "FITNESS FOR A PARTICULAR PURPOSE.\n");
		fprintf(stderr, PRINT_PREFIX "\n"); 
	} else {
		printf("flexiblas, version " FLEXIBLAS_VERSION "\n");
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
void __flexiblas_add_path(const char * path ) {
	__flexiblas_count_additional_paths++;
	__flexiblas_additional_paths = (char **) realloc( __flexiblas_additional_paths,
			sizeof(char *) * __flexiblas_count_additional_paths);
	DPRINTF(2,"Add additional search path %s\n", path); 
	__flexiblas_additional_paths[__flexiblas_count_additional_paths-1] = strdup(path); 
}

void __flexiblas_free_paths() {
	int i = 0;
	for ( i = 0; i < __flexiblas_count_additional_paths; i++) {
		free(__flexiblas_additional_paths[i]);
	}
	if ( __flexiblas_additional_paths != NULL) free(__flexiblas_additional_paths);
}



/*-----------------------------------------------------------------------------
 *  Other Stuff 
 *-----------------------------------------------------------------------------*/
void __flexiblas_insert_fallback_blas(csc_ini_file_t *config)
{
	char *SO_EXTENSION = __flexiblas_getenv(FLEXIBLAS_ENV_SO_EXTENSION);
	size_t len=strlen(FALLBACK_NAME)+strlen(SO_EXTENSION)+2;
	char *tmp = (char *) calloc(len,sizeof(char));
	char *tmp2;
	
	snprintf(tmp,len, "%s%s", FALLBACK_NAME,SO_EXTENSION);
	len = strlen("libblas_netlib")+strlen(SO_EXTENSION)+2;
	tmp2 = (char*) calloc(len,sizeof(char));
	snprintf(tmp2,len, "libblas_netlib%s", SO_EXTENSION);

	if ( csc_ini_setstring(config,"NETLIB", "library", tmp2)!=CSC_INI_SUCCESS){
		DPRINTF(0,"Can not insert Netlib  blas library.\n");
		abort(); 
	}
	if ( csc_ini_setstring(config,"__FALLBACK__", "library", tmp2) != CSC_INI_SUCCESS) {
		DPRINTF(0,"Can not insert Netlib  blas library.\n");
		abort(); 
	}

	free(tmp);
	free(tmp2);
	free(SO_EXTENSION);
	return; 
}

void __flexiblas_read_config_file(const char *filename, csc_ini_file_t *config, char **default_map ) {
	char *dm = NULL;
	csc_ini_section_t *sec; 
	csc_ini_iterator_t iter; 
	csc_ini_kvstore_t *kv; 
	int verbose = 0 ;
	int profile = 0; 

	if (__flexiblas_file_exist(filename) == 0 ) {
		DPRINTF(1,"Cannot open %s. Skipped.\n", filename);
		return; 
	}

	if ( csc_ini_load(filename, config, CSC_INI_LOAD_SECTION_UPPERCASE) != CSC_INI_SUCCESS) {
		DPRINTF(0, COLOR_RED "Failed to load %s. The file might be in the old format.\n" COLOR_RESET, filename); 
	}

	/* Get the default BLAS  */
	if ( csc_ini_getstring(config, CSC_INI_DEFAULT_SECTION,"default", &dm) == CSC_INI_SUCCESS){
		if ( *default_map == NULL ) {
			*default_map = strdup(dm); 
		} else {
			free(*default_map); 
			*default_map = strdup(dm); 
		}
	}

	if ( csc_ini_getinteger(config, CSC_INI_DEFAULT_SECTION,"verbose", &verbose) == CSC_INI_SUCCESS){
		__flexiblas_verbose = (__flexiblas_verbose > verbose ) ? (__flexiblas_verbose ) : (verbose); 
	}
	if ( csc_ini_getinteger(config, CSC_INI_DEFAULT_SECTION,"profile", &profile) == CSC_INI_SUCCESS){
		__flexiblas_profile = (__flexiblas_profile || profile ) ? 1 : 0; 
	}


	/* Get additional PATHS  */
	sec = csc_ini_getsection(config, CSC_INI_DEFAULT_SECTION); 
	if ( sec == NULL ) {
		DPRINTF(2, "No nameless section in configfile %s\n", filename); 
		return; 
	} 
	iter = NULL; 
	while ( (kv = csc_ini_kvstore_iterator(sec, &iter)) != NULL ) {
		char *key = csc_ini_getkey(kv); 
		if ( key[0] == 'p' && key[1]=='a' && key[2]=='t' && key[3] == 'h'){
			__flexiblas_add_path(csc_ini_getvalue(kv)); 
		}
	}
	
}


