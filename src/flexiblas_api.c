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
 * Copyright (C) Martin Koehler, 2013-2020
 */



#include "flexiblas.h"
#include "flexiblas_api.h"
#include "flexiblas_mgmt.h"
#include <errno.h>


/*-----------------------------------------------------------------------------
 *  Check if flexiblas is available. True in case if built-in is used 
 *-----------------------------------------------------------------------------*/
int flexiblas_avail()
{
    return 1; 
}



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
    size_t i; 
    for (i = 0; i < nloaded_backends; i++) {
        fprintf(fp, "%2zu - %s\n", i, loaded_backends[i]->name);
    }
    return; 
}


/*-----------------------------------------------------------------------------
 *  Print all available Backends 
 *-----------------------------------------------------------------------------*/
void flexiblas_print_avail_backends(FILE *fp) 
{
    void *iter_helper = NULL;
    char blas_name[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    char library_name[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    char comment[FLEXIBLAS_MGMT_MAX_BUFFER_LEN];
    int i = 0;
    flexiblas_mgmt_location_t loc;

    for (i = 0; i < 3; i++) {
        if (i == 0) 
            loc = FLEXIBLAS_HOST;
        else if (i == 1) 
            loc = FLEXIBLAS_USER;
        else if (i == 2) 
            loc = FLEXIBLAS_GLOBAL;

        iter_helper = NULL;
        while ( flexiblas_mgmt_list_blas(__flexiblas_mgmt, loc,  blas_name, library_name, comment, &iter_helper) > 0){
	    	fprintf(fp, "%20s - %s\n", blas_name, library_name);
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
    size_t iid;
    if ( id < 0 ) return -1; 
    iid = (size_t) id;
    if ( iid >= nloaded_backends) return -1; 
    
    current_backend = loaded_backends[iid]; 
    return 0; 
}


/*-----------------------------------------------------------------------------
 *  List all available backends 
 *-----------------------------------------------------------------------------*/
ssize_t flexiblas_list(char *name, const size_t len, const ssize_t pos) 
{
    /* Return number of backends */
    if (name == NULL) {
        return __flexiblas_mgmt->nblas_names;
    }
     
    if (pos == -1) {

        /* Return the default name  */
        flexiblas_mgmt_location_t loc;
        char dm[FLEXIBLAS_MGMT_MAX_BUFFER_LEN]; 
        flexiblas_mgmt_get_active_default(__flexiblas_mgmt, &loc, dm);
        strncpy(name, dm, len);
        name[len-1] = '\0'; 
        return (ssize_t)strlen(name);
    } else {
        if ((size_t)pos >=  __flexiblas_mgmt->nblas_names) {
            name[0] = '\0'; 
            return -1;
        }

        strncpy(name, __flexiblas_mgmt->blas_names[pos], len);
        name[len-1] = '\0';
        return (ssize_t)strlen(name);
    }
}

/*----------------------------------------------------------------------------
 *  List all available backends 
 *----------------------------------------------------------------------------*/
ssize_t flexiblas_list_loaded(char *name, const size_t len, const ssize_t pos) 
{
    /* Return number of backends */
    if (name == NULL) {
        return (ssize_t)nloaded_backends; 
    }

    if (pos == -1) {
        return -1;
    } else {
        if ((size_t)pos >= nloaded_backends) {
           return -1;
        }

        strncpy(name, loaded_backends[pos]->name, len); 
        name[len-1]='\0'; 
        return (ssize_t)strlen(name);
    }
}

