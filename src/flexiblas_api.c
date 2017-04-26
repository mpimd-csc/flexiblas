/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2015
 */



#include "flexiblas.h"
#include "flexiblas_api.h"

#include <errno.h>




/*-----------------------------------------------------------------------------
 *  Return the version of the Flexiblas library
 *-----------------------------------------------------------------------------*/
void flexiblas_get_version(int *major, int *minor, int *patch) 
{
    if ( major != NULL ) *major = FLEXIBLAS_VERSION_MAJOR;
    if ( minor != NULL ) *minor = FLEXIBLAS_VERSION_MINOR;
    if ( patch != NULL ) *patch = FLEXIBLAS_VERSION_PATCH;
    return; 
}



/*-----------------------------------------------------------------------------
 *  Print all loaded Backends 
 *-----------------------------------------------------------------------------*/
void flexiblas_print_loaded_backends(FILE *fp) 
{
    int i; 
    for (i = 0; i < nloaded_backends; i++) {
        fprintf(fp, "%2d - %s\n", i, loaded_backends[i]->name);
    }
    return; 
}


/*-----------------------------------------------------------------------------
 *  Print all available Backends 
 *-----------------------------------------------------------------------------*/
void flexiblas_print_avail_backends(FILE *fp) 
{
	csc_ini_iterator_t iter = NULL;
	csc_ini_iterator_t kv_iter = NULL;
	csc_ini_section_t  *sec;
	csc_ini_kvstore_t  *kv;

	while ((sec = csc_ini_section_iterator(&__flexiblas_config , &iter)) != NULL ){
        if ( csc_ini_getsectionname(sec) == NULL) continue; 
		fprintf(fp, "%20s - ", csc_ini_getsectionname(sec));
		kv_iter = NULL;
		while ( (kv = csc_ini_kvstore_iterator(sec, &kv_iter)) != NULL) {
            if (strcmp(csc_ini_getkey(kv), "library") == 0 ) {
                fprintf(fp, "%s\n", csc_ini_getvalue(kv));
            }
		}
	}
    return; 
}


/*-----------------------------------------------------------------------------
 *  Print Current Backend 
 *-----------------------------------------------------------------------------*/
void flexiblas_print_current_backend(FILE* fp) 
{
    fprintf(fp, "%s\n", current_backend->name);
}


/*-----------------------------------------------------------------------------
 *  Return current backend
 *-----------------------------------------------------------------------------*/
int flexiblas_current_backend(char *name, size_t len)
{
    strncpy(name, current_backend->name, len);
    name[len-1] = '\0';
    return strlen(name);
}

/*-----------------------------------------------------------------------------
 *  Switch BLAS 
 *-----------------------------------------------------------------------------*/
int flexiblas_switch(int id) 
{
    if ( id < 0 ) return -1; 
    if ( id >= nloaded_backends) return -1; 
    
    current_backend = loaded_backends[id]; 
    return 0; 
}


/*-----------------------------------------------------------------------------
 *  List all available backends 
 *-----------------------------------------------------------------------------*/
int flexiblas_list(char *name, size_t len, int pos) 
{
	csc_ini_iterator_t iter = NULL;
	csc_ini_section_t  *sec;

    /* Return number of backends */
    if (name == NULL) {
        int count = 0; 
    	while ((sec = csc_ini_section_iterator(&__flexiblas_config , &iter)) != NULL ){
            if ( csc_ini_getsectionname(sec) == NULL) continue; 
            count++; 
        }
        return count; 
    }
 
    if ( pos == -1 ) {
        char *dm = NULL; 
        /* Return the default name  */
        if ( csc_ini_getstring(&__flexiblas_config, CSC_INI_DEFAULT_SECTION,"default", &dm) == CSC_INI_SUCCESS){
            strncpy(name, dm, len); 
            name[len-1] = '\0'; 
            len = strlen(name);            
        } else {
            strncpy(name, "", len); 
            name[len-1] = '\0'; 
            len = strlen(name);            
        }
    } else {
        /* Return the BLAS-Name */
        int count = 0 ; 
     	while ((sec = csc_ini_section_iterator(&__flexiblas_config , &iter)) != NULL ){
            if ( csc_ini_getsectionname(sec) == NULL) continue; 
            if ( count == pos ) {
                strncpy(name, csc_ini_getsectionname(sec), len); 
                name[len-1] = '\0'; 
                len = strlen(name);         
                break; 
            }
            count ++; 
        }
        if ( count > pos ) 
            len = -1; 
    }
    return len; 
}

/*-----------------------------------------------------------------------------
 *  List all available backends 
 *-----------------------------------------------------------------------------*/
int flexiblas_list_loaded(char *name, size_t len, int pos) 
{
    /* Return number of backends */
    if (name == NULL) {
        return nloaded_backends; 
    }
 
    if (pos < 0 ) 
        return -1; 
    if (pos >= nloaded_backends) 
        return -1; 

    strncpy(name, loaded_backends[pos]->name, len); 
    name[len-1]='\0'; 
    len = strlen(name); 
    return len; 
}

