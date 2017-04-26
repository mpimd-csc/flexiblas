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


#ifndef FLEXIBLAS_COMMON_H
#define FLEXIBLAS_COMMON_H

/*-----------------------------------------------------------------------------
 *  Implementation <-> Library mapping table 
 *-----------------------------------------------------------------------------*/
typedef struct {
	char *key; 
	char *value; 
} kv_pair;

static char* kv_pair_getkey(data kv) {
	return ((kv_pair *) kv)->key; 
}
static void kv_pair_free(data kv) {
	kv_pair * kvi = (kv_pair *) kv; 
	if ( kvi->key) free(kvi->key); 
	if ( kvi->value) free(kvi->value); 
	free(kvi); 
}
static Int kv_hash(char * name, Int size) {
	Int ret = 0, i, len ; 
	len = strlen(name); 
	for (i = 0;  i < len ; i++) {
		ret = (ret+tolower(name[i])) % size; 
	}
	return ret; 
}
static kv_pair * kv_new_pair(const char *key, const char *value){
	kv_pair * pair = calloc ( sizeof(kv_pair), 1);
	if ( pair == NULL ) return NULL; 
	pair->key = strdup(key); 
	pair->value = strdup(value); 
	return pair; 
}

/*-----------------------------------------------------------------------------
 *  Additional File IO 
 *-----------------------------------------------------------------------------*/
#define MIN_CHUNK 256
static int getstr ( char **lineptr, size_t *n, FILE *stream, int terminator, int offset, int limit) 
{
	int nchars_avail;		/*  Allocated but unused chars in *LINEPTR.  */
	char *read_pos;		/*  Where we're reading into *LINEPTR. */
	int ret;
	size_t ni =0; 

	if (!lineptr || !n || !stream)   {
		return -1; 
	}
	
	if (!*lineptr)  {
		ni = MIN_CHUNK;
		*lineptr = calloc(sizeof(char), ni); 
		*n = ni; 
	}

	ni = *n ; 
	nchars_avail = ni - offset;
	read_pos = *lineptr + offset;
	for (;;) {
		int save_errno;
		int c;
		if (limit == 0) break;
		else {
			c = getc (stream);
			/*  If limit is negative, then we shouldn't pay attention to it, so decrement only if positive. */
			if (limit > 0) limit--;
		}
		save_errno = errno;
									  
		/*  We always want at least one char left in the buffer, since we
		 *  always (unless we get an error while reading the first char)
		 *  NULL-terminate the line buffer.  */
		assert((*lineptr + *n) == (read_pos + nchars_avail));
		if (nchars_avail < 2) {
			if (ni > MIN_CHUNK)    ni *= 2;
			else ni += MIN_CHUNK;
			*n = ni; 
			nchars_avail = ni + *lineptr - read_pos;
			*lineptr = (char *) realloc (*lineptr, sizeof(char) * (ni) );
			if (!*lineptr) {
				return -1; 
			}
			read_pos = ni - nchars_avail + *lineptr;
			assert((*lineptr + ni) == (read_pos + nchars_avail));
		}
		if (ferror (stream)) {
			/* Might like to return partial line, but there is no
			 * place for us to store errno.  And we don't want to just
			 * lose errno.  */
			errno = save_errno;
			return -1; 
		}
		if (c == EOF) { 
			/*  Return partial line, if any.  */
			if (read_pos == *lineptr)
				return -1;
			else
				break;
		}
		*read_pos++ = c;
		nchars_avail--;
		if (c == terminator)
			/*  Return the line.  */
			break;
	}
	/*  Done - NUL terminate and return the number of chars read.  */
	*read_pos = '\0';
	ret = read_pos - (*lineptr + offset);
	return ret;
}

static int _getline (char **line, size_t *nbytes, FILE *stream) {
	return getstr(line, nbytes, stream, '\n',0, 4096); 
}

static char *trim ( char * in) {
	char * out = in; 
	int i; 
	while ( (*out !='\0') && ( *out == ' ' || *out == '\t')) out++; 
	for (i = strlen(out); i > 0; i--) {
		if ( out[i-1] == ' ' || out[i-1]=='\t' || out[i-1]=='\n') {
			out[i-1]='\0'; 
		} else {
			break; 
		}
	}
	return out; 
}

static void read_config_file(FILE * fp, hashtable table, char **default_map ) {
	char * line =NULL; 
	char *k,*v,*k2,*v2; 
	char *save; 
	kv_pair * pair; 
	size_t len = 0 ; 
	while (!feof(fp)) {
	        if( _getline(&line,&len, fp) < 0 ) 
			break; 
		if ( line == NULL) { 
			continue; 
		}
		k = strtok_r(line, "|,;\n", &save);
		if ( k == NULL ) { 
			if ( line ) free(line); 
			line = NULL;
			continue; 
		}
		v = strtok_r(NULL, "|,;\n", &save); 
		if (v== NULL) {
			if ( line ) free(line); 
			line = NULL; 
			continue; 
		}
		k2 = trim ( k ); 
		v2 = trim ( v ); 
		if (strcasecmp(k2,"default") == 0 ) {
			if ( __flexiblas_verbose ) fprintf(stderr, PRINT_PREFIX "Set config file default BLAS to: %s\n",v2);
			if (*default_map != NULL ) free(*default_map); 
			*default_map = strdup(v2); 
		} else {
			if (strstr(v2, SO_EXTENSION) == NULL ) {
				char *vtmp = calloc(sizeof(char), strlen(v2) + strlen(SO_EXTENSION) + 3);
				if ( vtmp == NULL) abort(); 
				if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Library name without extension. Add " SO_EXTENSION " to filename\n");
				snprintf(vtmp, strlen(v2)+strlen(SO_EXTENSION) +3, "%s%s", v2, SO_EXTENSION); 
				pair = kv_new_pair(k2, vtmp); 
				free(vtmp); 
			} else {
				pair = kv_new_pair(k2,v2); 
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
	return; 
}

static void load_systemconfig ( hashtable table, char **default_map ) {
	FILE *configfile; 

	// Insert default pair 
	{
		kv_pair *  default_pair = kv_new_pair("netlib", "libblas_netlib" SO_EXTENSION); 
		if ( flexiblas_hashtable_insert(table, default_pair) == 0 ) {
			fprintf(stderr, PRINT_PREFIX "Can not insert default blas library.\n");
		}
	}
	/* // load etc/flexiblasrc
	if ( strcmp( "/etc/" FLEXIBLAS_RC, SYSCONFDIR "/" FLEXIBLAS_RC) != 0)  {
		if ( (configfile = fopen("/etc/" FLEXIBLAS_RC, "r"))) {	
			if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Read /etc/" FLEXIBLAS_RC "\n");
			read_config_file(configfile, table, default_map);  
			fclose(configfile); 
		} else {
			if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Cannot open /etc/" FLEXIBLAS_RC ". Skipped.\n");
		}
	} */
	// load CMAKE_INSTALL_PREFIX/etc/flexiblasrc 
	if ( (configfile = fopen(CMAKE_INSTALL_FULL_SYSCONFDIR "/" FLEXIBLAS_RC, "r"))) {	
		if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Read " CMAKE_INSTALL_FULL_SYSCONFDIR "/" FLEXIBLAS_RC "\n");
		read_config_file(configfile, table, default_map); 
		fclose(configfile); 
	} else {
		if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Cannot open " CMAKE_INSTALL_FULL_SYSCONFDIR "/" FLEXIBLAS_RC ". Skipped.\n");
	}
	return; 
} 

static void load_userconfig ( hashtable table , char **default_map ) {
	char *tmp; 
	char *homedir = getenv("HOME"); 
	FILE *configfile; 

	// load home/.flexiblasrc 
	tmp = calloc(sizeof(char), strlen(homedir) + 40); 
	if ( !tmp ) abort(); 
	snprintf(tmp, strlen(homedir)+40,"%s/.%s", homedir, FLEXIBLAS_RC); 
	if ( (configfile = fopen(tmp, "r"))) {	
		if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Read %s\n",tmp);
		read_config_file(configfile, table, default_map ); 
		fclose(configfile); 
	} else {
		if (__flexiblas_verbose) fprintf(stderr, PRINT_PREFIX "Cannot open %s. Skipped.\n", tmp);
	}
	free(tmp); 
}

static void print_copyright (int prefix) {
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



#endif /* end of include guard: FLEXIBLAS_COMMON_H */
