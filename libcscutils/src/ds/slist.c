/* 
* CSCUTILS - A collection of various software routines uses in CSC projects
* Copyright (C) 2017 Martin Koehler
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


typedef struct _entry_t {
    struct _entry_t *pnext;
    csc_ds_object_t data;
} entry_t;

typedef struct _single_linked_list_t {
    csc_ds_object_free freeobj; 
    csc_ds_object_compare_t compare;
    size_t size; 
    entry_t *head; 
    entry_t *last;
} single_linked_list_t;

static int slist_insert(csc_ds_t *ds, csc_ds_object_t obj) 
{
    entry_t *elem;
    single_linked_list_t * list = (single_linked_list_t *) ds->data;
    if (! list) return CSC_ERROR;
 
    /* Insert first  */
    if (! list->head ) {
        list->head = (entry_t *) malloc(sizeof(entry_t) * (1));
        if ( !list->head) {
            return CSC_ERROR;
        }
        list->head->data = obj;
        list->head->pnext = NULL;
        list->last = list->head;
        list->size = 1;
        return CSC_SUCCESS;
    }

    /* Insert Last / Append */
    elem = (entry_t *) malloc(sizeof(entry_t) * (1));
    if (!elem) {
        return CSC_ERROR;
    }
    elem->data = obj;
    elem->pnext = NULL;
    list->last->pnext = elem;
    list->last=elem;
    list->size++;
    return CSC_SUCCESS;
}

static int slist_insert_at(csc_ds_t *ds, csc_ds_object_t obj, int pos ) 
{
    entry_t *elem;
    single_linked_list_t * list = (single_linked_list_t *) ds->data;
    if (! list) return CSC_ERROR;

    if ( pos > (long) list->size) {
        csc_error_message("Insert at postion %d is not possible (list length %d)\n", pos, (int) list->size);
        return CSC_ERROR;
    }
    if ( pos < -1 ) {
        csc_error_message("Invalid position. pos = %d\n", pos);
        return CSC_ERROR;
    }

    if ( pos == 0 || list->head == NULL ) {
        elem = (entry_t *) malloc(sizeof(entry_t) * (1));
        if ( !elem) {
            return CSC_ERROR;
        }
        elem->data = obj;
        elem->pnext = list->head;
        list->head = elem;
        list->size++;
        if ( list->size == 1)
            list->last = list->head;
        return CSC_SUCCESS;

    }
    if ( pos == -1 ) {
        return slist_insert(ds, obj);
    }
    /*  insert at position  */
    int c_pos = 0;
    entry_t *iter = list->head;
    entry_t *last = NULL;

    for (c_pos = 0; c_pos < pos; c_pos++) {
        last = iter;
        iter = iter->pnext;
    }
    if (last == NULL) {
        csc_error_message("The last iteration entry is NULL.");
        return CSC_ERROR;
    }

    elem = (entry_t *) malloc(sizeof(entry_t) * (1));
    elem->data = obj;
    elem->pnext = iter;
    last->pnext = elem;
    if ( iter == NULL ) {
        list->last = elem;
    }
    list->size++;
    return CSC_SUCCESS;
}

static csc_ds_object_t slist_find(csc_ds_t *ds, const void *key) 
{
    single_linked_list_t * list = (single_linked_list_t *) ds->data;
    entry_t *elem;
    if (! list) return NULL;
    if (list->size == 0 || list->head==NULL) {
        return NULL;
    }

    elem = list->head;
    while(elem) {
        if ( list->compare(elem->data, key) == 0) 
            return elem->data;
        elem = elem->pnext;
    }
    return elem;    
}

static int slist_remove(csc_ds_t *ds, const void *key) 
{
    single_linked_list_t * list = (single_linked_list_t *) ds->data;
    entry_t *elem, *last;
    if (! list) return CSC_ERROR;
    if (list->size == 0 || list->head==NULL) {
        return CSC_ERROR;
    }

    last = NULL;
    elem = list->head;
    while(elem) {
        if ( list->compare(elem->data, key) == 0) {
            break;
        }
        last = elem;
        elem = elem->pnext;
    }
    if ( last == NULL && elem!=NULL) {
        /*  Remove first */
        list->head = elem->pnext;
        if ( list->freeobj) list->freeobj(elem->data);
        free(elem);
        list->size--;
        return CSC_SUCCESS;
    } else if ( elem != NULL) {
        last->pnext = elem->pnext;
        if ( list->freeobj) list->freeobj(elem->data);
        free(elem);
        if (last->pnext==NULL) 
            list->last = last;
        list->size--;
        return CSC_SUCCESS;
    } else {
        return CSC_ERROR;
    }
}

static void slist_remove_all(csc_ds_t *ds) 
{
    single_linked_list_t * list = (single_linked_list_t *) ds->data;
    entry_t *elem;
    if (! list) return;
    if (list->size == 0 || list->head==NULL) {
        return;
    }
    
    while (list->head) {
        elem = list->head;
        list->head = list->head->pnext;
        if ( list->freeobj) list->freeobj(elem->data);
        free(elem);
    }
    list->head = NULL;
    list->last = NULL;
    list->size = 0;
    free(list);
    ds->data = NULL;
    return;
}


static void slist_dump(FILE *out, csc_ds_t *ds) 
{
    entry_t *elem;
    size_t p = 0;
    single_linked_list_t * list = (single_linked_list_t *) ds->data;
    if (! list) return;
    elem = list->head;
    while(elem) {
        fprintf(out, "list [%5lu] = %lu\n", p, (size_t) elem->data);
        p++;
        elem = elem->pnext;
    }
    return;
}

csc_ds_t * csc_ds_slist(csc_ds_object_compare_t compare, csc_ds_object_free free_obj)
{
    csc_ds_t * ds = NULL;
    single_linked_list_t *list; 

    if (!compare) {
        csc_error_message("Missing compare function.\n");
        return NULL;
    }

    /* Allocate */
    ds = (csc_ds_t *) malloc ( sizeof(csc_ds_t) ) ; 
    if ( ds == NULL ) return NULL; 
    memset(ds, 0, sizeof(csc_ds_t)); 

    ds->type = CSC_DS_SLIST; 
    ds->data = NULL; 

    /* Set the function  */
    ds->insert = slist_insert;
    ds->insert_at = slist_insert_at; 
    ds->find   = slist_find; 
    ds->remove = slist_remove; 
    ds->remove_all = slist_remove_all; 
    ds->dump   = slist_dump; 

    /* Build the table  */
    list = (single_linked_list_t* ) malloc(sizeof(single_linked_list_t)); 
    if ( list == NULL) {
        free(ds); 
        return NULL; 
    }
    list->compare = compare;
    list->freeobj = free_obj;
    list->size = 0;
    list->head = NULL;
    list->last = NULL;
    ds->data = (void *) list; 
    if ( csc_ds_common_init(ds)) {
        free(ds);
        return NULL;
    }

    return ds; 
}


