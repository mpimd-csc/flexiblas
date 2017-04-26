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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hashtable.h"


hashtable flexiblas_hashtable_create( data_getkey nf, data_free rf, Int size, data_hash hf)
{
    hashtable s;
    
    s = (hashtable) malloc( sizeof( hashtable_t));
    if ( s == NULL ) return NULL;
    s->name = nf;
    s->freigabe = rf;
    s->size = size;
    s->hash = hf;
    s->hashtable = (hashtable_entry **)calloc( size, sizeof( hashtable_entry *));
    return s;
}

int flexiblas_hashtable_insert( hashtable s, data obj)
{
    Int ix;
    int cmp;
    char *name;
    hashtable_entry **pe, *neu;
    
    name = (s->name)( obj); 
    if (s==NULL) {
    	printf("Hashtable points to NULL\n");
	return 0;
    }
    if (s->hash ==NULL) {
	    printf("hash function is NULL\n");
	    return 0;
	}
    ix = s->hash(name, s->size);
    
    for( pe = s->hashtable + ix; *pe; pe = &((*pe)->next))
        {
        cmp = strcasecmp( name, (s->name)((*pe)->obj));
        if( cmp == 0){
            return 0;
	}
        else if( cmp < 0)
            break;
        }
    neu = (hashtable_entry *)malloc( sizeof( hashtable_entry));
    neu->next = *pe;
    neu->obj = obj;
    *pe = neu;
    return 1;
}

data flexiblas_hashtable_find( hashtable s, char *name)
{
    Int index;
    hashtable_entry *e;
    
    index = s->hash( name, s->size);
    for( e = s->hashtable[index]; e; e = e->next)
        {
        if( !strcasecmp( name, (s->name)(e->obj)))
            return e->obj;
        }
    return NULL;
}

void flexiblas_hashtable_show( hashtable s){
    Int ix;
    hashtable_entry *e;
    
    for( ix = 0; ix < s->size; ix++)
        {
	printf( "%4d:", ix);
        for( e = s->hashtable[ix]; e; e = e->next)
            printf( " %s%c", (s->name)(e->obj), e->next ? ',' : '.');
        printf( "\n");
        }
   }

void flexiblas_hashtable_freeall( hashtable s)
    {
    Int ix;
    hashtable_entry *e;
    
    for( ix = 0; ix < s->size; ix++)
        {
        while( (e = s->hashtable[ix] ))
            {
            s->hashtable[ix] = e->next;
            s->freigabe( e->obj);
            free( e);
            }
        }
    free( s->hashtable);
    free( s);
    }

int flexiblas_hashtable_remove( hashtable s, char *name){
    Int ix;
    int cmp;
    hashtable_entry **pe;
    hashtable_entry *e;
    
    ix = s->hash( name, s->size);
    
    
    for( pe = s->hashtable + ix; *pe; pe = &((*pe)->next))
        {
        cmp = strcasecmp( name, (s->name)((*pe)->obj));
        if( !cmp)
            {
            e = *pe;
            *pe = ((*pe)->next);
            (s->freigabe)(e->obj);
            free(e);
            return 1;
            }
        else if( cmp < 0)
            break;
        }
    return 0;
}

