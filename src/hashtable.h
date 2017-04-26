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


#ifndef FLEXIBLASHASHTABLE_H
#define FLEXIBLASHASHTABLE_H


#ifdef __cplusplus
extern "C" {
#endif

	// Necessary for Int  
	#include "hooks.h"

	typedef void* data ;
	typedef char *(*data_getkey)( data obj);
	typedef void (*data_free)( data obj);
	typedef Int (*data_hash)( char *name,  Int size);

	typedef struct __htb_entry   {
	    struct __htb_entry *next;
	    data *obj;
	    } hashtable_entry;

	typedef struct __hashtable{
	    data_getkey name;
	    data_free freigabe;
	    data_hash hash;
	    Int size;
	    hashtable_entry **hashtable;
	    } hashtable_t;
	typedef hashtable_t * hashtable;

	extern hashtable flexiblas_hashtable_create( data_getkey kf, data_free rf, Int size, data_hash hf);
	extern int flexiblas_hashtable_insert( hashtable ht, data obj);
	extern data flexiblas_hashtable_find( hashtable ht, char *name);
	extern void flexiblas_hashtable_freeall( hashtable ht);
	extern void flexiblas_hashtable_show( hashtable ht);
	extern int flexiblas_hashtable_remove( hashtable ht, char *name);

	typedef struct {
		char *key; 
		char *value; 
	} kv_pair;
	kv_pair *__flexiblas_kv_new_pair(const char *key, const char *value);
	Int __flexiblas_kv_hash(char * name, Int size);
	void __flexiblas_kv_pair_free(data kv);
	char* __flexiblas_kv_pair_getkey(data kv);

#ifdef __cplusplus
}
#endif
#endif
