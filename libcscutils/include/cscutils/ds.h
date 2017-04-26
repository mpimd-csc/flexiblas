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

#ifndef CSC_DS_H

#define CSC_DS_H

#include "cscutils/error_message.h"

#if defined(_PTHREAD_H) || defined(HAVE_PTHREAD) 
 #include <pthread.h>
 typedef pthread_mutex_t csc_ds_mutex_t;
#else 
 typedef void * csc_ds_mutex_t;
#endif 

#ifdef __cplusplus
extern "C" { 
#endif
	/**
	 @file libcscutils/include/cscutils/ds.h
	  @defgroup ds Data Structures: Basic data structures for managing data. 

	  This part of the library contains some functions to manage various standard 
      data structures like hashtables.
      
      @attention This part of the library depends on the \ref error_message module.

	  @addtogroup ds
	  @{ 
	*/


    /**
     * @brief Representation of a data item inside a data structure.
     *
     * The csc_ds_object type definition represents an object inside a data 
     * structure. In order to be used with any data types we use a void pointer 
     * a the moment. 
     **/
    typedef void * csc_ds_object_t;  

    /** @brief Function prototype to get a string representation of an item. 
     *
     * The csc_ds_object_string type definition declares the function prototype 
     * to get a string representation of a data item. The return value must be 
     * a non malloced pointer to the name of the object.
     **/
    typedef char *(*csc_ds_object_string)( csc_ds_object_t obj);

    /**
     * @brief Function prototype to define a free function for the items. 
     *
     * The csc_ds_object_free function type definition declares the prototype of 
     * a clean up function for an item. It should remove an item from memory and 
     * frees all data. 
     *
     **/
    typedef void (*csc_ds_object_free)(csc_ds_object_t obj); 

    /**
     * @brief Function prototype of a hash function. 
     *
     * The csc_ds_object_hash type definition declares a function prototype to 
     * compute a hash of a given string with resprect to a hash table of size len.
     *
     */
    typedef size_t (*csc_ds_object_hash)(const char *key, size_t len); 

    /** 
     * @brief Function prototype for a compare function.
     * 
     * The csc_ds_object_compare_t type defines a function taking two 
     * pointers to objects and compare them. It has to return -1, 0, or 1 
     * if the second argument is less than, equal, or greater than the first one. 
     */
    typedef int (*csc_ds_object_compare_t) (const void *a, const void *b);

    /** 
     * @brief Enumerator to identify the type of the data structure. 
     *
     * The csc_ds_type_t enumerator defines the different supported data structures. 
     * At the moment this is only the hash table. 
     */
    typedef enum {
        /** The data structure represents a hash table. */
        CSC_DS_HASHTABLE = 0,
        /** The data structure represents a single linked list. */
        CSC_DS_SLIST = 1
    } csc_ds_type_t;

    /**
     * @brief Definition of a arbitrary data structure. 
     *
     * The csc_ds_t type definition defines an arbitrary data structure with 
     * its basic operations.
     *
     * @sa csc_ds_hashtable
     * @sa csc_ds_slist
     *
     */
    typedef struct _csc_ds_t {
        /** Type of the data structure. */
        csc_ds_type_t type; 
        /** Pointer to the data contained in the data structure. */
        void *data; 
        /** Insert function to insert new objects.  */
        int (*insert)(struct _csc_ds_t *ds, csc_ds_object_t obj); 
        /** InsertAt function to insert new objects at given positions. */
        int (*insert_at)(struct _csc_ds_t *ds, csc_ds_object_t obj, int pos);
        /** Search function to find elements given by its key. */
        csc_ds_object_t (*find)(struct _csc_ds_t *ds, const void *key); 
        /** Remove function to delete an element given by its key. */ 
        int (*remove)(struct _csc_ds_t *ds, const void *key); 
        /** Cleanup function to remove all elements from the data structure.  */
        void (*remove_all)(struct _csc_ds_t *ds); 
        /** Dump the data structure to a FILE. This is used for debbuging purpose only. */
        void (*dump)(FILE *out, struct _csc_ds_t *ds); 
        /** Internal Mutex */
        csc_ds_mutex_t mutex;
    } csc_ds_t;


    /** 
     * @brief Insert an item into a data structure. 
     * @param[in,out]  ds Data structure where to insert the item. 
     * @param[in]     obj Item to insert into the data structure. 
     * @return zero on success or a non zero error code otherwise 
     *
     * The csc_ds_insert function inserts an item obj into the data structure 
     * addressed by ds. If the underlying object is a list. The insert function 
     * performs an append operation. 
     *
     */
    int csc_ds_insert(csc_ds_t *ds, csc_ds_object_t obj);

    /**
     * @brief Insert an item at a given position 
     * @param[in,out] ds Data structure where to insert the item.
     * @param[in]     obj object to insert into the data structure. 
     * @param[in]     pos postion to insert. 
     * @return zero on success or a non zeor error code otherwise. 
     *
     * The csc_ds_insert_at function inserts an item into a data structure 
     * at a given position if this is supported by the underlying structure. 
     * If this is not supported a standard \ref csc_ds_insert is used. 
     *
     * Special values for \ref pos are: 
     * \li 0 At the begin of the struture. 
     * \li -1 At the end of the structure. 
     *
     */
    int csc_ds_insert_at( csc_ds_t *ds, csc_ds_object_t obj, int pos);

    
    /** 
     * @brief Search for an item identified by a key inside a data structure. 
     * @param[in] ds    Data structure where to search for the item. 
     * @param[in] key   Key of the item to search for. 
     * @return If the item was found the item is returned otherwise NULL is returned. 
     *
     * The csc_ds_find function search for an item with the given key. If the item was found 
     * in the data structure addressed by ds the item is returned. If the item was not found 
     * in the data structure NULL is returned. 
     */
    csc_ds_object_t csc_ds_find(csc_ds_t *ds, const void *key); 


    /** 
     * @brief Search and remove an item from a data structure.  
     * @param[in,out]  ds   Data structure where to delete the item from. 
     * @param[in]      key  Key of the item to delete. 
     * @return zero if the key was found and successfully removed or a non zero value in case of an error. 
     *
     * The csc_ds_remove function searches for a given key and removes the item belonging to the key. If 
     * the item was found and removed zero is returned. If the item did not exists in the data structure
     * a non zero error value is returned. 
     */
    int csc_ds_remove(csc_ds_t *ds, const void *key); 

    
    /** 
     * @brief Removes the whole data structure from memory.  
     * @param[in] ds    Data structure to remove. 
     *
     * The csc_ds_free function removes the whole data structure from memory. 
     */
    void csc_ds_free(csc_ds_t *ds); 


    /** 
     * @brief Dump a human readable representation of the data structure to a FILE.
     * @param[in]   out FILE pointer for the output. 
     * @param[in]   ds  Data structure which to dump in to the FILE.
     *
     * The csc_ds_dump function dumps the data structure to a FILE (or stdout). The primary 
     * usage of this function is for debbuging purpose. 
     */
    void csc_ds_dump(FILE *out, csc_ds_t *ds); 
    
    /**
     * @brief Common init function for the data structure system. 
     * @param[in,out] ds   Data structure to initialize. 
     * @return zero on success or a non zero error value. 
     *
     * The csc_ds_common_init function finalizes the initialization 
     * procedure of a data structure. It should only be called by functions
     * like \ref csc_ds_slist or \ref csc_ds_hashtable.
     * */
    int csc_ds_common_init(csc_ds_t * ds);

    /** 
     * @brief Create a data structure representing a hash table. 
     * @param[in]   len     Length of the hash table. 
     * @param[in]   getname Function returning the string representation of an item. 
     * @param[in]   gethash Function to the hash value of an item. 
     * @param[in]   freeobj Function to remove an item from memory. 
     * @return The pointer to the data structure or NULL in case of an error. 
     *
     * The csc_ds_hashtable function create an easy hash table based on a linked list 
     * if collisions appear. Beside the getname and the freeobj function it needs a 
     * hash function to calculate the positions in the table. 
     */
    csc_ds_t * csc_ds_hashtable(size_t len, csc_ds_object_string getname, csc_ds_object_hash gethash, csc_ds_object_free free_obj); 


    /**
     * @brief Create a single linked list.
     * @param[in] compare   Define a compare function to search for elements.
     * @param[in] free_obj  Function to remove an entry from memory.
     * @return  The pointer to the data structure or NULL in case of an error. 
     *
     * The csc_ds_slist function creates a single linked list.
     */
    csc_ds_t * csc_ds_slist(csc_ds_object_compare_t compare, csc_ds_object_free free_obj);


    /** @}   */


#ifdef __cplusplus
}; 
#endif 

#endif /* end of include guard: CSC_DS_H */

