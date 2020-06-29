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
#include "cscutils/cscutils_config.h"

#ifdef CSC_HAVE_ZLIB
#include <zlib.h>

#include "compress_handler.h"
#include "cscutils/io.h"
#include "cscutils/error_message.h"

static int gzip_open (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler * handler);
static int gzip_close(void **data);
static size_t gzip_read(void *data, void *buf, size_t len);
static size_t gzip_write(void *data, void *buf, size_t len);

/* Internal structure to represent a gzip compressed file. */
typedef struct {
    gzFile fp;
    char * path;
    csc_io_mode_t mode;
}_gzip_file;


/* Registers the gzip compression library. */
_compressed_io_handler io_register_gzip(){
    _compressed_io_handler handler;
    strcpy(handler.extension, ".gz");
    handler.type = CSC_IO_FILE_GZIP;
    strcpy(handler.magic,  "\x1F\x8B\x08");
    handler.open = gzip_open;
    handler.close= gzip_close;
    handler.write= gzip_write;
    handler.read = gzip_read;
    return handler;
}

/* Local function which opens a gzip compressed file. */
static int gzip_open (void **data, const char * path, const csc_io_mode_t  mode, _compressed_io_handler * handler)
{
    gzFile fp;
    _gzip_file *f;
    int err;
    (void) handler;
    char _mode[4];
    if ( mode == CSC_IO_FILE_WRITE ) {
        strcpy(_mode, "w");
    } else if ( mode == CSC_IO_FILE_READ ) {
        strcpy(_mode, "r");
    } else {
        csc_error_message("Wrong mode argument. This must either be CSC_IO_FILE_READ or CSC_IO_FILE_WRITE\n");
        return -1;
    }
    if ( !(fp = gzopen(path, _mode))) {
        err = errno;
        csc_warn_message("opening file: %s failed, errno: %03d - %s\n", path, err, strerror(err));
        return -1;
    }

    f = (_gzip_file *) malloc(sizeof(_gzip_file));
    f->fp = fp;
    f->path = strdup(path);
    f->mode = mode;
    *data = (void *) f;
    return 0;
}

/* Local function to close and free a gzip file */
static int gzip_close(void **data){
    _gzip_file *f;

    if ( data == NULL ) {
        csc_warn_message("Error: data == NULL\n");
        return -1;
    }
    if ( *data == NULL ) {
        csc_warn_message("Error: *data == NULL\n");
        return -1;
    }
    f = (_gzip_file*) *data;
    gzclose(f->fp);
    if ( f->path )
        free(f->path);
    free(f);
    *data = NULL;
    return 0;
}

/* Local function to read a block from a gzip compressed file */
static size_t gzip_read(void *data, void *buf, size_t len){
    _gzip_file *f;
    if ( data == NULL) return -1;
    if ( buf == NULL) return -1;
    f = (_gzip_file*) data;
    return gzread(f->fp,buf,(unsigned int)len);

}

/* Local function to write a block to a gzip compressed file. */
static size_t gzip_write(void *data, void *buf, size_t len){
    _gzip_file *f;
    if ( data == NULL) return -1;
    if ( buf == NULL) return -1;
    f = (_gzip_file*) data;
    return gzwrite(f->fp,buf,(unsigned int)len);
}

#endif /* NO_GZ  */
