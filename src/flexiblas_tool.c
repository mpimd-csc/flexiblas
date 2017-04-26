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
#include "flexiblas_common.h"

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
		printf("%*s \t %s\n", maxlen, pair->key, pair->value);
	}
    }


}

void list_blas() {
	hashtable blas_libary_map_system; 
	hashtable blas_libary_map_user; 
	char * default_map_system = NULL; 
	char * default_map_user   = NULL; 

	printf("Available FlexiBLAS backends:\n");
	printf("-----------------------------\n\n");
	/*-----------------------------------------------------------------------------
	 *  System-Wide BLAS Libraries 
	 *-----------------------------------------------------------------------------*/
	printf("System BLAS backends:\n");
	blas_libary_map_system = flexiblas_hashtable_create(kv_pair_getkey, kv_pair_free, 31, kv_hash); 
	if ( blas_libary_map_system == NULL) {
		printf("Can not create hash-table for BLAS entires.\n");
		abort(); 
	}
	load_systemconfig(blas_libary_map_system, &default_map_system); 
	flexiblas_hashtable_display(blas_libary_map_system); 

	printf("\nUser BLAS backends:\n"); 
	blas_libary_map_user = flexiblas_hashtable_create(kv_pair_getkey, kv_pair_free, 31, kv_hash); 
	if ( blas_libary_map_user == NULL) {
		printf("Can not create hash-table for BLAS entires.\n");
		abort(); 
	}
	load_userconfig(blas_libary_map_user, &default_map_user); 
	flexiblas_hashtable_display(blas_libary_map_user); 

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
	return; 
}

void set_blas(char *blas ) {
	char *rcfilename; 
	char *homedir = getenv("HOME"); 
	FILE *rcfile; 
	char *tr_blas = trim(blas); 
	hashtable blas_libary_map; 
	char* default_map = NULL; 

	blas_libary_map = flexiblas_hashtable_create(kv_pair_getkey, kv_pair_free, 31, kv_hash); 
	if ( blas_libary_map == NULL) {
		printf("Can not create hash-table for BLAS entires.\n");
		abort(); 
	}
	load_systemconfig(blas_libary_map, &default_map); 
	load_userconfig(blas_libary_map,&default_map); 

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
			
			k = strtok_r(line, "|,;\n", &save);
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
			
			k = strtok_r(line, "|,;\n", &save);
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
		print_copyright(0); 
		print_usage(argv[0]); 
		return 0; 
	}

	if ( argc < 2 || argc > 3 ) {
		printf("Wrong number of arguments. Exit. \n");
		return -1; 
	}
	if (strcasecmp(argv[1], "help") == 0) {
		print_copyright(0); 
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

