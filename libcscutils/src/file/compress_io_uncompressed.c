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

/* Erst ab glibc >= 2.10  */
#ifdef CSC_HAVE_MMAP_IO
#define MMAP_IO
#endif

#if defined(__GNUC__)
#define ATTR_UNUSED __attribute__((unused))
#else
#define ATTR_UNUSED
#endif

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

/*-----------------------------------------------------------------------------
 * Memory Mapped IO under Linux
 *-----------------------------------------------------------------------------*/
#ifdef MMAP_IO
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#endif


#include "compress_handler.h"
#include "cscutils/io.h"
#include "cscutils/error_message.h"

static int uncompressed_open  (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler * handler);
static int uncompressed_open2 (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler * handler);
static int uncompressed_close(void **data);
static size_t uncompressed_read(void *data, void *buf, size_t len);
static size_t uncompressed_write(void *data, void *buf, size_t len);


#ifdef MMAP_IO
static int uncompressed_mmap_open (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler *handler);
static int uncompressed_mmap_close(void **data);
static size_t uncompressed_mmap_read(void *data, void *buf, size_t len);
static size_t uncompressed_mmap_write(void *data, void *buf, size_t len);
#endif


/* Internal structure to represent an uncompressed file. */
typedef struct {
    FILE *fp;
    char * path;
    csc_io_mode_t mode;
}_uncompressed_file;

// Internal structure to represent an uncompressed file.
typedef struct {
    int fd;
    size_t size;
    size_t pos;
    char * file;
    char * path;
    csc_io_mode_t mode;
    int rw;
}_uncompressed_mmap_file;

/* Registers the access to uncompressed files. */
_compressed_io_handler io_register_uncompressed(void){
    _compressed_io_handler handler;
    strcpy(handler.extension, "");
    handler.type = CSC_IO_FILE_UNCOMPRESSED;
    strcpy(handler.magic,"");
#ifndef MMAP_IO
    handler.open = uncompressed_open;
    handler.close = uncompressed_close;
    handler.write = uncompressed_write;
    handler.read  = uncompressed_read;
#else
    handler.open  = uncompressed_mmap_open;
    handler.close = uncompressed_mmap_close;
    handler.write = uncompressed_mmap_write;
    handler.read  = uncompressed_mmap_read;

#endif
    return handler;
}

_compressed_io_handler io_register_no_gzip(void){
    _compressed_io_handler handler;
    strcpy(handler.extension, ".gz");
    handler.type = CSC_IO_FILE_UNCOMPRESSED;
    strcpy(handler.magic,  "\x1F\x8B\x08");
    handler.open = uncompressed_open2;
    handler.close = uncompressed_close;
    handler.write = uncompressed_write;
    handler.read  = NULL;

    return handler;
}

_compressed_io_handler io_register_no_bzip2(void){
    _compressed_io_handler handler;
    strcpy(handler.extension, ".bz2");
    handler.type = CSC_IO_FILE_UNCOMPRESSED;
    strcpy(handler.magic, "BZh");
    handler.open = uncompressed_open2;
    handler.close = uncompressed_close;
    handler.write = uncompressed_write;
    handler.read  = NULL;
    return handler;
}

_compressed_io_handler io_register_no_xz(void){
    _compressed_io_handler handler;
    const char HEADER_MAGIC[6] = { (char) 0xFD, '7', 'z', 'X', 'Z', 0x00 };
    strcpy(handler.extension, ".xz");
    handler.type = CSC_IO_FILE_UNCOMPRESSED;
    strcpy(handler.magic,(char *) (uintptr_t) (const void*) HEADER_MAGIC);
    handler.open = uncompressed_open2;
    handler.close = uncompressed_close;
    handler.write = uncompressed_write;
    handler.read  = NULL;
    return handler;
}

/* Local function which opens an uncompressed file. */
ATTR_UNUSED static int uncompressed_open (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler * handler) {
    FILE *fp;
    _uncompressed_file *f;
    int err;
    char _mode[4];
    (void) handler;
    if ( mode == CSC_IO_FILE_WRITE ) {
        strcpy(_mode, "w");
    } else if ( mode == CSC_IO_FILE_READ ) {
        strcpy(_mode, "r");
    } else {
        csc_error_message("Wrong mode argument. This must either be CSC_IO_FILE_READ or CSC_IO_FILE_WRITE\n");
        return -1;
    }

    if ( !(fp = fopen(path, _mode))) {
        err = errno;
        csc_warn_message("opening file: %s failed, errno: %03d - %s\n", path, err, strerror(err));
        return -1;
    }

    f = (_uncompressed_file * ) malloc( sizeof(_uncompressed_file));
    f->fp = fp;
    f->path = strdup(path);
    f->mode = mode;
    *data = (void *) f;
    return 0;
}

/* Local function which opens an uncompressed file. */
static int uncompressed_open2 (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler * handler){
    FILE *fp;
    _uncompressed_file *f;
    int err;
    char *pos;
    char *internal_fn;

    char _mode[4];
    if ( mode == CSC_IO_FILE_WRITE ) {
        strcpy(_mode, "w");
    } else if ( mode == CSC_IO_FILE_READ ) {
        strcpy(_mode, "r");
    } else {
        csc_error_message("Wrong mode argument. This must either be CSC_IO_FILE_READ or CSC_IO_FILE_WRITE\n");
        return -1;
    }
    /*-----------------------------------------------------------------------------
     *  dectect extension of the archiver
     *-----------------------------------------------------------------------------*/
    internal_fn = strdup(path);
    if (strlen(handler->extension) >  0 ) {
        pos = strstr(internal_fn, handler->extension);
    } else {
        pos = NULL;
    }
    if ( pos != NULL ) {
        *pos = '\0';
        csc_warn_message("Support for \"%s\" compression not available, truncate to \"%s\".", handler->extension, internal_fn);
    }


    if ( !(fp = fopen(internal_fn, _mode))) {
        free(internal_fn);
        err = errno;
        csc_warn_message("opening file: %s failed, errno: %03d - %s\n", path, err, strerror(err));

        return -1;
    }

    f = (_uncompressed_file * ) malloc( sizeof(_uncompressed_file));
    f->fp = fp;
    f->path = strdup(path);
    f->mode = mode;
    *data = (void *) f;
    free(internal_fn);
    return 0;
}


/* Local function to close and free an uncompressed file */
static int uncompressed_close(void **data) {
    _uncompressed_file *f;

    if ( data == NULL ) {
        csc_warn_message("Error: data == NULL\n");
        return -1;
    }
    if ( *data == NULL ) {
        csc_warn_message("Error: *data == NULL\n");
        return -1;
    }

    f = (_uncompressed_file*) *data;
    fclose(f->fp);
    free(f->path);
    free(f);
    *data = NULL;
    return 0;
}


/* Local function to read a block from an uncompressed file */
ATTR_UNUSED static size_t uncompressed_read(void *data, void *buf, size_t len) {
    _uncompressed_file *f;
    if ( data == NULL) return -1;
    if ( buf == NULL) return -1;
    f = (_uncompressed_file*) data;
    return fread(buf,1,len,f->fp);
}

/* Local function to write a block to an uncompressed file. */
static size_t uncompressed_write(void *data, void *buf, size_t len){
    _uncompressed_file *f;
    if ( data == NULL) return -1;
    if ( buf == NULL) return -1;
    f = (_uncompressed_file*) data;
    return (size_t) fwrite(buf,1,len,f->fp);
}

#ifdef MMAP_IO
/* Local function which opens an uncompressed file using memory mapped IO. */
static int uncompressed_mmap_open (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler *handler ){
    _uncompressed_mmap_file *f;
    int err;
    int rw = 0;

    char _mode[4];
    if ( mode == CSC_IO_FILE_WRITE ) {
        strcpy(_mode, "w");
    } else if ( mode == CSC_IO_FILE_READ ) {
        strcpy(_mode, "r");
    } else {
        csc_error_message("Wrong mode argument. This must either be CSC_IO_FILE_READ or CSC_IO_FILE_WRITE\n");
        return -1;
    }

    if ( strcmp(_mode,"w") == 0 ) rw = 1;

    f = (_uncompressed_mmap_file *) malloc(sizeof(_uncompressed_mmap_file) * (1));
    f->path = strdup(path);
    f->mode = mode;
    f->rw = rw;
    if ( rw ) {
        f->fd = open(path, O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
        if ( f->fd < 0) {
            err = errno;
            csc_error_message("opening file: %s failed\nerrno: %03d - %s\n", path, err, strerror(err));
            free(f);
            return -1;
        }
        f->size = 0;
        f->pos  = 0;
    } else {
        struct stat attr;
        f->fd = open (path, O_RDWR);
        if ( f->fd < 0) {
            err = errno;
            csc_error_message("opening file: %s failed\nerrno: %03d - %s\n", path, err, strerror(err));
            free(f);
            return -1;
        }

        f->pos = 0;
        if ( fstat(f->fd, &attr) ){
            err = errno;
            csc_error_message ("fstat on file: %s failed\nerrno: %03d - %s\n", path, err, strerror(err));
            free(f);
            return -1;
        }
        f->size = attr.st_size;
        f->file = mmap (NULL, f->size, PROT_READ, MAP_SHARED,f->fd,0);
        if ( f->file == MAP_FAILED) {
            err = errno;
            csc_error_message("fstat on file: %s failed\nerrno: %03d - %s\n", path, err, strerror(err));
            free(f);
            return -1;
        }
    }
    *data = (void *) f;
    return 0;
}

/* Local function to close and free an uncompressed file. ( memory mapped IO variant) */
static int uncompressed_mmap_close(void **data) {
    _uncompressed_mmap_file *f;

    if (data == NULL) {
        return -1;
    }
    if (*data == NULL) {
        return -1;
    }
    f = (_uncompressed_mmap_file*) *data;
    if ( f -> rw ) {
        // msync(f->file, f->size, MS_SYNC);
        close(f->fd);
    } else {
        if ( munmap(f->file, f->size) ) {
            return -1;
        }
        close (f->fd);
    }
    free(f->path);
    free(f);
    *data = NULL;
    return 0;
}

/* Local function to read a block from an uncompressed file using memory mapped IO */
static size_t uncompressed_mmap_read(void *data, void *buf, size_t len){
    size_t w;
    _uncompressed_mmap_file *f;
    if ( data == NULL) return -1;
    if ( buf == NULL ) return -1;
    f = (_uncompressed_mmap_file*) data;

    if ( f->pos>=f->size) return EOF;

    if ( f->pos + len > f->size )
        w = f->size - f->pos;
    else w = len ;
    memcpy(buf,f->file+f->pos, w * sizeof(char));
    f->pos += w;
    return w;
}

/* Local function to write a block to an uncompressed file using memory mapped IO */
static size_t uncompressed_mmap_write(void *data, void *buf, size_t len){
    int err  = 0;
    _uncompressed_mmap_file *f;
    if ( data == NULL) return -1;
    if ( buf == NULL ) return -1;
    f = (_uncompressed_mmap_file*) data;
    f->size += len;
    if (lseek(f->fd, f->size-1, SEEK_SET)< 0 ) {
        csc_error_message("Error lseek\n");
        return -1;
    }
    if (write(f->fd,"",1) != 1) {
        csc_error_message("Error writing last byte of the file");
        return -1;
    }
    f->file = mmap (0, f->size,PROT_WRITE|PROT_READ, MAP_SHARED, f->fd, 0);
    if ( f->file == MAP_FAILED) {
        err = errno;
        csc_error_message("errno: %03d - %s\n Failed to map file", err, strerror(err));
        return -1;
    }
    memcpy(f->file+f->pos, buf, sizeof(char) * len);
    // msync(f->file, f->size, MS_SYNC);
    if ( munmap(f->file, f->size)) return -1;
    f->pos += len;
    return len;
}

#endif
