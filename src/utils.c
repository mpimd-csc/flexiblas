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
#include "string_tools.h"

#define MAX_BUFFER_SIZE (4096 * 8)
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
int __flexiblas_count_additional_paths = 0;
char **  __flexiblas_additional_paths = NULL ;

void __flexiblas_add_path(const char * path ) {
	__flexiblas_count_additional_paths++;
	__flexiblas_additional_paths = (char **) realloc( __flexiblas_additional_paths,
			sizeof(char *) * __flexiblas_count_additional_paths);
	__flexiblas_additional_paths[__flexiblas_count_additional_paths-1] = (char *) calloc( strlen (path) +1, sizeof(char) );
	strncpy(__flexiblas_additional_paths[__flexiblas_count_additional_paths-1], path, strlen(path)+1);
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
void __flexiblas_insert_fallback_blas(hashtable table)
{
	char *SO_EXTENSION = __flexiblas_getenv(FLEXIBLAS_ENV_SO_EXTENSION);
	size_t len=strlen(FALLBACK_NAME)+strlen(SO_EXTENSION)+2;
	char *tmp = (char *) calloc(len,sizeof(char));
	char *tmp2;
	snprintf(tmp,len, "%s%s", FALLBACK_NAME,SO_EXTENSION);
	len = strlen("libblas_netlib")+strlen(SO_EXTENSION)+2;
	tmp2 = (char*) calloc(len,sizeof(char));
	snprintf(tmp2,len, "libblas_netlib%s", SO_EXTENSION);
	kv_pair *  default_pair = __flexiblas_kv_new_pair("netlib", tmp2); 
	kv_pair *  fallback_pair = __flexiblas_kv_new_pair("fallback", tmp);
	free(tmp);
	free(tmp2);
	free(SO_EXTENSION); 
	if ( flexiblas_hashtable_insert(table, default_pair) == 0 ) {
		fprintf(stderr, PRINT_PREFIX "Can not insert Netlib  blas library.\n");
		abort();
	}
	if ( flexiblas_hashtable_insert(table, fallback_pair) == 0 ) {
		fprintf(stderr, PRINT_PREFIX "Can not insert Netlib  blas library.\n");
		abort();
	}

}

void __flexiblas_read_config_file(FILE * fp, hashtable table, char **default_map ) {
	char * line =NULL; 
	char *k,*v,*k2,*v2; 
	char *save; 
	kv_pair * pair; 
	size_t len = 0 ; 
	char * SO_EXTENSION = __flexiblas_getenv(FLEXIBLAS_ENV_SO_EXTENSION);
	while (!feof(fp)) {
	        if( _getline(&line,&len, fp) < 0 ) 
			break; 
		if ( line == NULL) { 
			continue; 
		}
#ifdef __WIN32__
		k = strtok_s(line, "|,;\r\n", &save);
#else 
		k = strtok_r(line, "|,;\n", &save);
#endif
		if ( k == NULL ) { 
			if ( line ) free(line); 
			line = NULL;
			continue; 
		}
#ifdef __WIN32__
		v = strtok_s(NULL, "|,;\r\n", &save); 
#else 
		v = strtok_r(NULL, "|,;\n", &save); 
#endif
		if (v== NULL) {
			if ( line ) free(line); 
			line = NULL; 
			continue; 
		}
		k2 = trim ( k ); 
		v2 = trim ( v ); 
		if (strcasecmp(k2,"default") == 0 ) {
			/* Set the default entry */
			if ( __flexiblas_verbose ) fprintf(stderr, PRINT_PREFIX "Set config file default BLAS to: %s\n",v2);
			if (*default_map != NULL ) free(*default_map); 
			*default_map = strdup(v2); 
		} else if ( strcasecmp(k2, "path") == 0 ) {
			/* Add a new search path  */
			__flexiblas_add_path(v2);
		} else {
			/* Add a Library entry  */
			if (strstr(v2, SO_EXTENSION) == NULL ) {
				char *vtmp = calloc(sizeof(char), strlen(v2) + strlen(SO_EXTENSION) + 3);
				if ( vtmp == NULL) abort(); 
				if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Library name without extension. Add %s to filename\n", SO_EXTENSION);
				snprintf(vtmp, strlen(v2)+strlen(SO_EXTENSION) +3, "%s%s", v2, SO_EXTENSION); 
				pair = __flexiblas_kv_new_pair(k2, vtmp); 
				free(vtmp); 
			} else {
				pair = __flexiblas_kv_new_pair(k2,v2); 
			}
			if ( flexiblas_hashtable_insert(table, (data) pair) == 0 ) {
				flexiblas_hashtable_remove(table, k ); 
				if (flexiblas_hashtable_insert(table, (data) pair) == 0) {
					fprintf(stderr, PRINT_PREFIX " Can not insert BLAS entry ( %s, %s) in Hashtable\n", k,v);
					abort();
				}
			}
		}
		free(line); 
		line = NULL; 
	}
	if ( line != NULL ) free(line); 
	if ( SO_EXTENSION ) free(SO_EXTENSION);
	return; 
}

void __flexiblas_load_config ( const char * path, hashtable table , char **default_map ) {
	FILE *configfile; 
	if (path == NULL) return; 
	
	if ( (configfile = fopen(path, "r"))) {	
		if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Read %s\n",path);
		__flexiblas_read_config_file(configfile, table, default_map ); 
		fclose(configfile); 
	} else {
		if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Cannot open %s. Skipped.\n", path);
	}
}


