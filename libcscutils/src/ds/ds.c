/* 
* CSCUTILS - A collection of various software routines uses in CSC projects
* Copyright (C) 2015 Martin Koehler
* 
* This library is free software; you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published
* by the Free Software Foundation; either version 2.1 of the License, or
* (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public License
* along with this library; if not, see <http://www.gnu.org/licenses/>.
* 
*/ 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cscutils/ds.h"
#include "cscutils/error_message.h" 

#ifdef HAVE_PTHREAD
#define MUTEX_LOCK pthread_mutex_lock(&(ds->mutex))
#define MUTEX_UNLOCK pthread_mutex_unlock(&(ds->mutex))
#else 
#define MUTEX_UNLOCK 
#define MUTEX_LOCK
#endif 


/** common initfunction */
int csc_ds_common_init(csc_ds_t * ds) 
{
    #ifdef HAVE_PTHREAD
    pthread_mutex_init(&(ds->mutex), NULL);
    #else 
    ds->mutex = (void *) 0;
    #endif
    return CSC_SUCCESS;
}

int csc_ds_insert(csc_ds_t *ds, csc_ds_object_t obj) 
{
    int ret = 0;
    if ( ds == NULL) return CSC_ERROR; 
    if ( ds->insert == NULL) return CSC_ERROR; 
    MUTEX_LOCK;
    ret = ds->insert(ds, obj); 
    MUTEX_UNLOCK;
    return ret;
}

int csc_ds_insert_at( csc_ds_t *ds, csc_ds_object_t obj, int pos)
{
    int ret ;
    if ( !ds) return CSC_ERROR;
    MUTEX_LOCK;
    if ( ds->insert_at ) {
        ret = ds->insert_at(ds, obj, pos);
    } else {
        ret = ds->insert(ds, obj);
    }
    MUTEX_UNLOCK;
    return ret;
}


csc_ds_object_t csc_ds_find(csc_ds_t *ds, const void *key)
{
    csc_ds_object_t ret = NULL;
    if (ds == NULL ) return NULL; 
    if (ds->find == NULL) return NULL; 
    MUTEX_LOCK;
    ret = ds->find(ds, key); 
    MUTEX_UNLOCK;
    return ret;
}

int csc_ds_remove(csc_ds_t *ds, const void *key)
{
    int ret ;
    if ( ds == NULL) return CSC_ERROR; 
    if ( ds->remove == NULL) return CSC_ERROR; 
    MUTEX_LOCK;
    ret = ds->remove(ds, key); 
    MUTEX_UNLOCK;
    return ret;
}

void csc_ds_dump(FILE *out, csc_ds_t *ds)
{
    if ( ds == NULL) return; 
    if ( ds->dump == NULL) return; 
    MUTEX_LOCK;
    ds->dump(out, ds); 
    MUTEX_UNLOCK;
    return; 
}

void csc_ds_free(csc_ds_t *ds)
{
    if ( ds == NULL) return; 
    if ( ds->remove_all == NULL) return; 

    MUTEX_LOCK;
    ds->remove_all(ds); 
    MUTEX_UNLOCK;
#ifdef HAVE_PTHREAD 
    pthread_mutex_destroy(&(ds->mutex));
#endif 
    free(ds);
    ds = NULL;  
    return; 
}


