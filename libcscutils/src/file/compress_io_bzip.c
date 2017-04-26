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

#if _FILE_OFFSET_BITS == 64
#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#ifndef NO_BZ2
#include <bzlib.h>

/* Local include */
#include "compress_handler.h"


#include "cscutils/io.h"
#include "cscutils/error_message.h"


static int bzip2_open (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler * handler);
static int bzip2_close(void **data);
static size_t bzip2_read(void *data, void *buf, size_t len);
static size_t bzip2_write(void *data, void *buf, size_t len);

// Internal structure to represent a bzip2 compressed file.
typedef struct {
	BZFILE *fp;
	char * path;
	csc_io_mode_t mode;	
}_bzip2_file;



/* Registers the bzip2 compression library. */ 
_compressed_io_handler io_register_bzip2(){
    _compressed_io_handler handler; 
    strcpy(handler.extension, ".bz2"); 
    handler.type = CSC_IO_FILE_BZIP2; 
    strcpy(handler.magic, "BZh"); 
    handler.open = bzip2_open; 
    handler.close= bzip2_close; 
    handler.write= bzip2_write; 
    handler.read = bzip2_read; 
    return handler;
}



/* Local function which opens a bzip2 compressed file. */ 
static int bzip2_open (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler * handler) {
	BZFILE *fp;
	_bzip2_file *f;
	int err;
	char _mode[4];
	if ( mode == CSC_IO_FILE_WRITE ) {
		strcpy(_mode, "w");
	} else if ( mode == CSC_IO_FILE_READ ) {
		strcpy(_mode, "r");
	} else {
		csc_error_message("Wrong mode argument. This must either be CSC_IO_FILE_READ or CSC_IO_FILE_WRITE\n");
		return -1;
	}

	if ( !(fp = BZ2_bzopen(path, _mode))) {
		err = errno;
		csc_warn_message("opening file: %s failed errno: %03d - %s \n", path, err, strerror(err));
		return -1;
	}

	f = (_bzip2_file *) malloc(sizeof(_bzip2_file));
	f->fp = fp;
	f->path = strdup(path);
	f->mode = mode;
	*data = (void *) f;
	return 0;
}



/* Local function to close and free a bzip2 file */ 
static int bzip2_close(void **data) {
	_bzip2_file *f;

	if ( data == NULL ) {
		csc_warn_message("Error: data == NULL\n");
		return -1;
	}
	if ( *data == NULL ) {
		csc_warn_message("Error: *data == NULL\n");
		return -1;
	}

	f = (_bzip2_file*) *data;
	BZ2_bzclose(f->fp);
	if (f->path) 
		free(f->path);
	free(f);
	*data = NULL;
	return 0;

}


/* Function to read a block from a bzip2 compressed file */ 
static size_t bzip2_read(void *data, void *buf, size_t len) {
	_bzip2_file *f;
	if ( data == NULL ) return -1;
	if ( buf  == NULL)  return -1;
	f = (_bzip2_file*) data;
	return BZ2_bzread(f->fp,buf,(unsigned int) len);

}

/* Local function to write a block to a bzip2 compressed file. */ 
static size_t bzip2_write(void *data, void *buf, size_t len) {
	_bzip2_file *f;
	if ( data == NULL) return -1;
	if ( buf == NULL ) return -1;
	f = (_bzip2_file*) data;
	return BZ2_bzwrite(f->fp,buf,(unsigned int)len);
}

#endif /* NO_BZ2 */
