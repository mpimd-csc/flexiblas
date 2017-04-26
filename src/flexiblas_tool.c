/* $Id$ */
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
#include "flexiblas.h"
#include "string_tools.h"
#ifdef __WIN32__
#define strtok_r strtok_s
#endif 

static void init_default_search_path() {
	char searchpath[] = FLEXIBLAS_DEFAULT_LIB_PATH;
	char *path;
	char *r; 

	path = strtok_r(searchpath,":", &r);
	while ( path != NULL ) {
		__flexiblas_add_path(path);	
		path = strtok_r(NULL, ":",&r);
	}
}


void print_usage(const char *prgmname) {
	printf("The flexiblas tool helps to set the user default BLAS backend for\n");
	printf("FlexiBLAS. The tool modifies the ~/.flexiblasrc files and sets the\n");
	printf("approitate default entry in it.\n");
	printf("\n");
	printf("Usage: \n");
	printf(" %12s help          \t Print this information.\n", prgmname);
	printf(" %12s list          \t List all configured BLAS libraries.\n", prgmname);
	printf(" %12s set BLASNAME  \t Sets the default BLAS backend in ~/.flexiblasrc\n", prgmname);
	printf(" %12s unset         \t Removes the default setting in ~/.flexiblasrc\n", prgmname);
	printf("\n");
	return; 
}

void flexiblas_hashtable_display(hashtable s) {
    Int ix;
    hashtable_entry *e;
    Int maxlen = 0; 
    Int clen; 
    
    for( ix = 0; ix < s->size; ix++) {
        for( e = s->hashtable[ix]; e; e = e->next) {
		clen = strlen((s->name)(e->obj)); 
		if ( clen > maxlen) maxlen = clen; 
	}
    }
    maxlen += 5; 
    for( ix = 0; ix < s->size; ix++) {
        for( e = s->hashtable[ix]; e; e = e->next) {
		kv_pair * pair  = (kv_pair *) e->obj; 
		printf("%*s \t %s\n", (int) maxlen, pair->key, pair->value);
	}
    }


}

void list_blas() {
	hashtable blas_libary_map_system; 
	hashtable blas_libary_map_user; 
	char * default_map_system = NULL; 
	char * default_map_user   = NULL; 
	int i = 0;

	printf("Available FlexiBLAS backends:\n");
	printf("-----------------------------\n\n");
	/*-----------------------------------------------------------------------------
	 *  System-Wide BLAS Libraries 
	 *-----------------------------------------------------------------------------*/
	printf("System BLAS backends:\n");
	blas_libary_map_system = flexiblas_hashtable_create(__flexiblas_kv_pair_getkey, __flexiblas_kv_pair_free, 31, __flexiblas_kv_hash); 
	if ( blas_libary_map_system == NULL) {
		printf("Can not create hash-table for BLAS entires.\n");
		abort(); 
	}
	__flexiblas_insert_fallback_blas(blas_libary_map_system);
	{ /* Load System config */
		char * system_config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
		__flexiblas_load_config(system_config_file,blas_libary_map_system, &default_map_system); 
		free(system_config_file);
	}
	flexiblas_hashtable_display(blas_libary_map_system); 

	printf("\nUser BLAS backends:\n"); 
	blas_libary_map_user = flexiblas_hashtable_create(__flexiblas_kv_pair_getkey, __flexiblas_kv_pair_free, 31, __flexiblas_kv_hash); 
	if ( blas_libary_map_user == NULL) {
		printf("Can not create hash-table for BLAS entires.\n");
		abort(); 
	}
	/* __flexiblas_insert_fallback_blas(blas_libary_map_user); */ 
	
	{ /* Load User Config */
		char * user_config_file = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
		__flexiblas_load_config(user_config_file, blas_libary_map_user, &default_map_user);
		free(user_config_file);
	}
	flexiblas_hashtable_display(blas_libary_map_user); 

	init_default_search_path();

	printf("\nAdditional BLAS paths:\n");
	for ( i = 0; i < __flexiblas_count_additional_paths; i++){
		printf(" - %s\n", __flexiblas_additional_paths[i]);
	}

	if ( default_map_system == NULL && default_map_user == NULL ) {
		printf("\nDefault BLAS backend: netlib  (generic default).\n"); 
	} else if ( default_map_user == NULL ) {
		printf("\nDefault BLAS backend: %s  (system default).\n", default_map_system); 
	} else {
		printf("\nDefault BLAS backend: %s  (user default).\n", default_map_user); 
	}
	flexiblas_hashtable_freeall(blas_libary_map_user); 
	flexiblas_hashtable_freeall(blas_libary_map_system); 
	free(default_map_user); 
	free(default_map_system); 
	__flexiblas_free_paths();
	return; 
}

void set_blas(char *blas ) {
	char *rcfilename; 
	char *homedir = getenv("HOME"); 
	FILE *rcfile; 
	char *tr_blas = trim(blas); 
	hashtable blas_libary_map; 
	char* default_map = NULL; 

	blas_libary_map = flexiblas_hashtable_create(__flexiblas_kv_pair_getkey, __flexiblas_kv_pair_free, 31, __flexiblas_kv_hash); 
	if ( blas_libary_map == NULL) {
		printf("Can not create hash-table for BLAS entires.\n");
		abort(); 
	}
	__flexiblas_insert_fallback_blas(blas_libary_map);
	{ /* Load System config */
		char * system_config_file  = __flexiblas_getenv(FLEXIBLAS_ENV_GLOBAL_RC);
		__flexiblas_load_config(system_config_file,blas_libary_map, &default_map); 
		free(system_config_file);
	}
	{ /* Load User Config */
		char * user_config_file = __flexiblas_getenv(FLEXIBLAS_ENV_USER_RC);
		__flexiblas_load_config(user_config_file, blas_libary_map, &default_map);
		free(user_config_file);
	}

	if ( default_map != NULL && flexiblas_hashtable_find(blas_libary_map, default_map) == NULL ) {
		printf("Current default backend does not exist: %s\nIt will be replaced.", default_map);
	}
	if ( default_map != NULL && strcasecmp(default_map, tr_blas) == 0) {
		 printf("Default BLAS backend already set to: %s\n", tr_blas);
		 free(default_map); 
		 flexiblas_hashtable_freeall(blas_libary_map); 
		 return; 
	}
	if ( flexiblas_hashtable_find(blas_libary_map, tr_blas) == NULL ) {
		printf("Desired BLAS backend \"%s\" does not exists. Cancel.\n", tr_blas);
		free(default_map); 
		flexiblas_hashtable_freeall(blas_libary_map); 
		return; 
	}
	free(default_map); 
	flexiblas_hashtable_freeall(blas_libary_map); 
	__flexiblas_free_paths();
	
	rcfilename = calloc(sizeof(char), strlen(homedir) + 40); 
	if ( !rcfilename ) abort(); 
	snprintf(rcfilename, strlen(homedir)+40,"%s/.%s", homedir, FLEXIBLAS_RC); 
	if (!(rcfile=fopen(rcfilename,"r"))) {
		// new rcfile 
		rcfile = fopen(rcfilename,"w"); 
		if (!rcfile) {
			fprintf(stderr, "Can not open %s for writing. Exit.\n", rcfilename);
			free(rcfilename); 
			abort(); 
		}
		fprintf(rcfile, "default | %s\n", tr_blas);
		fclose(rcfile); 
	} else {
		char **lines = NULL; 
		char *line = NULL; 
		size_t nlines = 0; 
		char *k,*k2; 
		int def = 0; 
		int i = 0; 
		size_t len;
		char * save; 
		while (!feof(rcfile)) {
		        if( _getline(&line,&len, rcfile) < 0 ) 
				break; 
			if ( line == NULL) 
				continue; 
			nlines++; 
			lines = realloc(lines, sizeof(char *) * nlines); 
			lines[nlines-1] = strdup(line); 
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
			k2 = trim ( k ); 
			if (strcasecmp(k2,"default") == 0 ) {
				if (def == 0) {
					lines[nlines-1] = realloc(lines[nlines-1], sizeof(char) *( strlen(tr_blas) + 15)); 
					snprintf(lines[nlines-1], strlen(tr_blas) + 14, "default | %s\n", tr_blas); 
					def = 1; 
				}
			} 
			free(line); 
			line = NULL; 
		}
		if ( line != NULL ) free(line); 
		fclose(rcfile); 
		
		if (def == 0) {
			nlines++; 
			lines = realloc(lines, sizeof(char *) * nlines); 
			lines[nlines-1] = calloc(sizeof(char), strlen(tr_blas) + 15); 
			snprintf(lines[nlines-1], strlen(tr_blas) + 14, "default | %s\n", tr_blas); 
			def = 1; 
		}
		rcfile = fopen(rcfilename, "w"); 
		if (!rcfile) {
			fprintf(stderr, "Can not open %s for writing. Exit.\n", rcfilename);
			free(rcfilename); 
			abort(); 
		}
	
		for (i = 0; i < nlines; i++) {
			if ( lines[i] != NULL ) fprintf(rcfile, "%s", lines[i]);
			free(lines[i]); 
		}
		free(lines); 
		fclose(rcfile); 
	}
	free(rcfilename); 
	return; 
}

void unset_blas( ) {
	char *rcfilename; 
	char *homedir = getenv("HOME"); 
	FILE *rcfile; 

	rcfilename = calloc(sizeof(char), strlen(homedir) + 20); 
	if ( !rcfilename ) abort(); 
	snprintf(rcfilename, strlen(homedir)+20,"%s/.flexiblasrc", homedir); 
	if ((rcfile=fopen(rcfilename,"r"))) {
		char **lines = NULL; 
		char *line = NULL; 
		size_t nlines = 0; 
		char *k,*k2; 
		int i = 0; 
		size_t len;
		char * save; 
		while (!feof(rcfile)) {
		        if( _getline(&line,&len, rcfile) < 0 ) 
				break; 
			if ( line == NULL) 
				continue; 
			nlines++; 
			lines = realloc(lines, sizeof(char *) * nlines); 
			lines[nlines-1] = strdup(line); 
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
			k2 = trim ( k ); 
			if (strcasecmp(k2,"default") == 0 ) {
				free(lines[nlines-1]);
				lines[nlines-1]= NULL; 
			} 
			free(line); 
			line = NULL; 
		}
		if ( line != NULL ) free(line); 
		fclose(rcfile); 
		
		rcfile = fopen(rcfilename, "w"); 
		if (!rcfile) {
			fprintf(stderr, "Can not open %s for writing. Exit.\n", rcfilename);
			free(rcfilename); 
			abort(); 
		}
	
		for (i = 0; i < nlines; i++) {
			if ( lines[i] != NULL ) fprintf(rcfile, "%s", lines[i]);
			free(lines[i]); 
		}
		free(lines); 
		fclose(rcfile); 
	}
	free(rcfilename); 
	return; 
}


int main(int argc, char **argv)
{
	
	if ( argc == 1 ) {
		__flexiblas_print_copyright(0); 
		print_usage(argv[0]); 
		return 0; 
	}

	if ( argc < 2 || argc > 3 ) {
		printf("Wrong number of arguments. Exit. \n");
		return -1; 
	}
	if (strcasecmp(argv[1], "help") == 0) {
		__flexiblas_print_copyright(0); 
		print_usage(argv[0]); 
		return 0; 
	}
	if (strcasecmp(argv[1], "list") == 0) {
		list_blas(); 
		return 0; 
	}
	if (strcasecmp(argv[1], "unset") == 0 ) {
		unset_blas(); 
		return 0; 
	}
	if (strcasecmp(argv[1], "set" ) == 0 ) {
		if ( argc != 3 ) {
			printf("set needs one argument. Exit.\n");
			return -1; 
		}
		set_blas(argv[2]); 
		return 0; 
	}
	printf("Unknown command. Exit.\n");
	return -1; 
}

