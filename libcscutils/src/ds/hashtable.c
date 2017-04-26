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

typedef struct _hashtable_entry   {
    struct _hashtable_entry *next; 
    csc_ds_object_t obj;             
} hashtable_entry;

typedef struct __hashtable{
    csc_ds_object_string name; 
    csc_ds_object_free freeobj; 
    csc_ds_object_hash hash;
    size_t size; 
    hashtable_entry **hashtable; 
} hashtable_t;

static int hashtable_insert(csc_ds_t *ds, csc_ds_object_t obj) 
{
    size_t ix; 
    int cmp;
    char *name;
    hashtable_t *ht; 
    hashtable_entry **pe, *neu;

    ht = (hashtable_t* ) ds->data; 
    if ( ht == NULL) return CSC_ERROR; 
    if ( ht->hashtable == NULL) return CSC_ERROR; 
    if ( ht->hash == NULL ) return CSC_ERROR; 

    name = ht->name(obj); 
    ix = ht->hash(name, ht->size);
    
    for( pe = ht->hashtable + ix; *pe; pe = &((*pe)->next))
    {
        cmp = strcmp( name, (ht->name)((*pe)->obj));
        if( cmp == 0){
            return CSC_ERROR;
        } else if( cmp < 0)
            break;
    }
    neu = (hashtable_entry *)malloc( sizeof( hashtable_entry));
    neu->next = *pe;
    neu->obj = obj;
    *pe = neu;
    return CSC_SUCCESS;
}

static csc_ds_object_t hashtable_find(csc_ds_t *ds, const void *_key) 
{
    hashtable_t *ht; 
    size_t index;
    hashtable_entry *e;
    const char * key = (const char *) _key;

    ht = (hashtable_t* ) ds->data; 
    if ( ht == NULL) return NULL; 
    if ( ht->hashtable == NULL) return NULL; 
    if ( ht->hash == NULL ) return NULL; 

    index = ht->hash( key, ht->size);
    for( e = ht->hashtable[index]; e; e = e->next)
    {
        if( !strcmp( key, (ht->name)(e->obj)))
            return e->obj;
    }
    return NULL;
}

static int hashtable_remove(csc_ds_t *ds, const void *_key ) 
{
    size_t ix; 
    int cmp;
    hashtable_entry **pe;
    hashtable_entry *e;
    hashtable_t *ht; 
    const char * key = (const char * ) _key;

    ht = (hashtable_t* ) ds->data; 
    if ( ht == NULL) return CSC_ERROR; 
    if ( ht->hashtable == NULL) return CSC_ERROR; 
    if ( ht->hash == NULL ) return CSC_ERROR; 

    ix = ht->hash( key, ht->size);
    
    for( pe = ht->hashtable + ix; *pe; pe = &((*pe)->next)) {
        cmp = strcmp( key, (ht->name)((*pe)->obj));
        if( cmp == 0 ) {
            e = *pe;
            *pe = ((*pe)->next);
            (ht->freeobj)(e->obj);
            free(e);
            return CSC_SUCCESS; 
        }
  	}
    return CSC_ERROR; 
}

static void hashtable_remove_all(csc_ds_t *ds) 
{
    size_t ix; 
    hashtable_entry *e;
    hashtable_t *ht; 
    
    ht = (hashtable_t* ) ds->data; 
    if ( ht == NULL) return;  
    if ( ht->hashtable == NULL) return; 

    for( ix = 0; ix < ht->size; ix++) {
        while( (e = ht->hashtable[ix] )) {
            ht->hashtable[ix] = e->next;
            ht->freeobj( e->obj);
            free(e);

        }
    }
    free( ht->hashtable);
    free( ht);
    ht = NULL; 
    ds->data = NULL; 

}

static void hashtable_dump(FILE *out, csc_ds_t *ds)
{
    hashtable_t *ht; 
    size_t index;
    hashtable_entry *e;

    ht = (hashtable_t* ) ds->data; 
    if ( ht == NULL) return; 
    if ( ht->hashtable == NULL) return; 
    
    for( index = 0; index < ht->size; index++) {
	    printf( "%6lu: ", (unsigned long) index); 
        for( e = ht->hashtable[index]; e; e = e->next) {
            printf( " %s%c", (ht->name)(e->obj), e->next ? ',' : '.');
        }
        printf( "\n");
    }
}

/*-----------------------------------------------------------------------------
 *  Setup a hashtable 
 *-----------------------------------------------------------------------------*/
csc_ds_t * csc_ds_hashtable(size_t len, csc_ds_object_string getname, csc_ds_object_hash gethash, csc_ds_object_free free_obj)
{
    csc_ds_t * ds = NULL;
    hashtable_t *ht; 

    /* Allocate */
    ds = (csc_ds_t *) malloc ( sizeof(csc_ds_t) ) ; 
    if ( ds == NULL ) return NULL; 
    memset(ds, 0, sizeof(csc_ds_t)); 

    ds->type = CSC_DS_HASHTABLE; 
    ds->data = NULL; 

    /* Set the function  */
    ds->insert = hashtable_insert; 
    ds->find   = hashtable_find; 
    ds->remove = hashtable_remove; 
    ds->remove_all = hashtable_remove_all; 
    ds->dump   = hashtable_dump; 

    /* Build the table  */
    ht = (hashtable_t* ) malloc(sizeof(hashtable_t)); 
    if ( ht == NULL) {
        free(ds); 
        return NULL; 
    }
    ht->name = getname; 
    ht->freeobj = free_obj; 
    ht->hash = gethash; 
    ht->size = len; 
    ht->hashtable = ( hashtable_entry **) calloc(len, sizeof(hashtable_entry *)); 
    if ( ht->hashtable == NULL) {
        free(ht); 
        free(ds); 
        return NULL; 
    }
    ds->data = (void *) ht; 
    if ( csc_ds_common_init(ds)) {
        free(ht);
        free(ds);
        return NULL;
    }
    return ds; 
}

