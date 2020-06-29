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

#if _FILE_OFFSET_BITS == 64 && !defined(_LARGEFILE64_SOURCE)
#define _LARGEFILE64_SOURCE
#endif

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "cscutils/io.h"
#include "compress_handler.h"
#include "cscutils/error_message.h"


#define MIN(A,B) (((A)<(B))?(A):(B))
#define MAX(A,B) (((A)>(B))?(A):(B))

extern void io_init();




/* Detect the compression type of a file  */
csc_io_compress_type_t  csc_io_is_compressed (const char *path)
{
    FILE *fp;
    char head[10];
    size_t read;
    int i;

    /*-----------------------------------------------------------------------------
     *  open the file
     *-----------------------------------------------------------------------------*/
    fp = fopen (path, "r");
    if (!fp) {
        for (i = 0 ; i < _compressed_io_handler_len;  i++) {
            if ( strlen(compress_methods[i].extension)  > 0  && strstr(path, compress_methods[i].extension) != NULL){
                return compress_methods[i].type;
            }
        }
        return CSC_IO_FILE_UNCOMPRESSED;
    } else {
        /*-----------------------------------------------------------------------------
         *  read the header
         *-----------------------------------------------------------------------------*/
        read=fread(head, sizeof ( char ) ,  10 , fp);
        fclose(fp);

        for ( i = 0; i < _compressed_io_handler_len; i++){
            if ( strlen(compress_methods[i].magic) > 0 && strncmp(head, compress_methods[i].magic,MIN(strlen(compress_methods[i].magic),read)) == 0 ) {
                return compress_methods[i].type;
            }
        }
        return CSC_IO_FILE_UNCOMPRESSED;
    }
    return CSC_IO_FILE_ERROR;
}

/* Get the compression handler  */
static _compressed_io_handler * get_compress_handler(const char *path) {
    FILE *fp;
    int err = 0;
    char head[10];
    size_t read;
    int i;
    _compressed_io_handler *handler = NULL;

    /*-----------------------------------------------------------------------------
     *  open the file
     *-----------------------------------------------------------------------------*/
    fp = fopen (path, "r");
    if (!fp) {
        err = errno;
        csc_error_message("opening file: %s failed, errno: %03d - %s\n", path, err, strerror(err));
        return NULL;
    }

    /*-----------------------------------------------------------------------------
     *  read the header
     *-----------------------------------------------------------------------------*/
    read=fread(head, sizeof ( char ) ,  10 , fp);
    fclose(fp);

    for ( i = 0; i < _compressed_io_handler_len; i++){

        if ( strlen(compress_methods[i].magic) > 0  && strncmp(head, compress_methods[i].magic,MIN(strlen(compress_methods[i].magic),read)) == 0 ) {
            handler =  &(compress_methods[i]);
            break;
        }
    }
    if ( handler == NULL ) {
        handler = &compress_fallback;
    }
    return handler;
}


/* Open a file   */
csc_io_file_t* csc_io_open(const char * path, csc_io_mode_t mode)
{
    int rw = 0;
    int ret = 0;
    int i = 0;
    csc_io_file_t  *f;

    if ( path == NULL) {
        csc_error_message("path points to NULL\n");
    }


    if ( mode != CSC_IO_FILE_READ && mode != CSC_IO_FILE_WRITE ) {
        csc_error_message("The mode parameter must either be CSC_IO_FILE_WRITE or CSC_IO_FILE_READ.");
        return NULL;
    }

    csc_io_init ();

    /*-----------------------------------------------------------------------------
     *  detect write mode
     *-----------------------------------------------------------------------------*/
    if ( mode == CSC_IO_FILE_WRITE ) {
        rw = 1;
    }

    f = (csc_io_file_t *) malloc ( sizeof(csc_io_file_t));
    if ( f == NULL ) {
        csc_error_message("Allocation of the mess_file object failed.\n");
        return NULL;
    }
    /*-----------------------------------------------------------------------------
     *  Open the file
     *-----------------------------------------------------------------------------*/
    if ( rw == 0) {
        _compressed_io_handler *handler;
        f->pos = 0;
        f->avail = 0;
        f->data  = NULL;
        f->mode = CSC_IO_FILE_READ;
        f->eof = 0;
        handler = get_compress_handler(path);
        if ( !handler ) {
            csc_error_message("Can not determine file IO hanlder. \n");
            free(f);
            return NULL;
        }

        if ( handler->read == NULL) {
            csc_error_message("File \"%s\" cannot be opened for reading. The \"%s\" archive extension is not supported.\n", path, handler->extension);
            free(f);
            return NULL;
        }
        f->handler =(void *) handler;
        ret = handler->open(&(f->data), path, mode, handler);
        if ( ret != 0) {
            csc_error_message("Opening %s failed.\n", path);
            free(f);
            return NULL;
        }
    } else {
        _compressed_io_handler *handler = NULL;
        _compressed_io_handler *fallback = NULL;
        for (i = 0 ; i < _compressed_io_handler_len;  i++) {
            if ( strlen(compress_methods[i].extension)  > 0  && strstr(path, compress_methods[i].extension) != NULL){
                handler = &(compress_methods[i]);
                break;
            }
            if ( strlen(compress_methods[i].extension) == 0) {
                fallback = &(compress_methods[i]);
            }
        }
        if ( handler == NULL) handler= fallback;
        if ( handler == NULL) {
            csc_error_message("No suitable IO handler for %s found.\n", path);
            free(f);
            return NULL;
        }
        if ( handler->write == NULL ) {
            csc_error_message("The select archive format \"%s\" does not support writing.\n");
            free(f);
            return NULL;
        }

        f->mode=CSC_IO_FILE_WRITE;
        f->pos = 0;
        f->avail = 0;
        f->data  = NULL;
        f->eof = 0;
        f->handler =(void *) handler;
        ret = handler->open(&(f->data), path, mode, handler);
        if ( ret != 0) {
            csc_error_message("Opening %s failed.\n", path);

            free(f);
            return NULL;
        }
    }
    return f;
}

/* Flush the buffer cache   */
int csc_io_flush (csc_io_file_t  *f )
{
    size_t w =0;
    void *htmp;
    struct _compressed_io_handler_t * handler;

    if (!f) return -1;
    if (f->mode != CSC_IO_FILE_WRITE) return -1;

    htmp = (void *) (f->handler);
    handler = (struct _compressed_io_handler_t *) htmp;

    if (f->pos == 0 ) return 0;
    w = handler->write(f->data, f->buffer, f->pos);
    if ( w != f->pos ) {
        return -1;
    }
    f->pos = 0;
    return 0;
}

/* Close the file */
int csc_io_close (csc_io_file_t  *f )
{
    _compressed_io_handler *h;
    if ( f == NULL ) {
        csc_error_message("File f points to NULLs\n");
        return -1;
    }

    if ( f->mode == CSC_IO_FILE_WRITE) {
        csc_io_flush(f);
    }
    h = (_compressed_io_handler *) f->handler;
    h->close(&(f->data));
    free(f);
    return 0;

}

/* Check for EOF  */
int csc_io_eof(csc_io_file_t *f){
    if (f == NULL )
        return 1;
    if (f->eof)
        return 1;
    return 0;
}

/* Read a character from a file    */
int     csc_io_getc (csc_io_file_t  * f )
{
    void *htmp;
    struct _compressed_io_handler_t * handler;

    if (!f) return EOF;
    if (f->mode != CSC_IO_FILE_READ) return -1;

    htmp = (void *) (f->handler);
    handler = (struct _compressed_io_handler_t *) htmp;

    if ( handler->read == NULL ) {
        csc_error_message("The used io-handler does not provide any read function.\n");
        return -1;
    }

    if ( f -> pos == f->avail ) {
        f->avail = handler->read(f->data, f->buffer, CSC_IO_FILE_BUFFER_SIZE);
        if ( f->avail <= 0) {
            f->eof = 1;
            return EOF;
        }

        /* if ( f -> avail < 0 ) {
           return -1;
           } */
        f->pos = 0;
    }
    if ( f -> pos < f->avail) {
        return (int) f->buffer[f->pos++];
    }
    return EOF;
}

/* Read a string from the file */
char*   csc_io_gets( char *buf, int len, csc_io_file_t * f)
{
    char *buf_sic = buf;
    int c;

    if ( f == NULL ) {
        csc_error_message("File f points to NULLs\n");
        return NULL;
    }
    if ( buf == NULL) {
        csc_error_message("Buffer buf points to NULL\n");
        return NULL;
    }

    if ( len <= 0) return NULL;
    memset(buf, 0, len);
    while ( --len > 0 ) {
        c = csc_io_getc(f);
        if ( c == EOF ) {
            f->eof = 1;
            return buf_sic;
        }
        if ( c == '\n' || c=='\r') {
            if ( c == '\r') c = csc_io_getc(f);
            *buf = '\0';
            return buf_sic;
        }
        *buf = (char) c;
        buf++;
    }
    return buf_sic;

}

#define CHUNK_SIZE 128
/* Replacement for getline */
ssize_t csc_io_getline( char **buf, size_t *len, csc_io_file_t *file)
{
    size_t idx = 0;
    int c;

    if ( buf  == NULL || len == NULL || file == NULL)  {
        csc_error_message("Either buf, len or file points to NULL");
        return -1;
    }

    if (*buf == NULL)
    {
        *buf = malloc (CHUNK_SIZE);
        if (*buf == NULL) {
            return -1;
        }
        *len = CHUNK_SIZE;
    }

    while ((c = csc_io_getc (file)) != EOF)
    {
        if (idx >= *len)
        {
            *buf = realloc (*buf, *len + CHUNK_SIZE);
            if (*buf == NULL) {
                return -1;
            }
            *len += CHUNK_SIZE;
        }

        (*buf)[idx++] = c;

        if (c == '\n')
            break;
    }

    if (idx >= *len)
    {
        *buf = realloc (*buf, *len + 1);
        if (*buf == NULL) {
            return -1;
        }
        *len += 1;
    }
    /* Null terminate the buffer.  */
    (*buf)[idx++] = 0;

    return (c == EOF && (idx - 1) == 0) ? (-1) : (ssize_t)(idx - 1);
}


#ifdef CSC_HAVE_VSSCANF
/* Fscanf replacement */
int     csc_io_scanf(csc_io_file_t * f, const char *fmt, ...)
{
    int ret = 0 ;
    char *buf = NULL;
    size_t len = 0 ;
    ssize_t iret = 0;
    va_list args;

    iret = csc_io_getline(&buf, &len, f);
    if ( iret < 0 ) {
        if (buf != NULL)
            free(buf);
        return -1;
    }
    va_start(args,fmt);
    ret = vsscanf(buf, fmt, args);
    va_end(args);

    free(buf);
    return ret;
}
#endif


/* Binary Read   */
size_t  csc_io_read(void *ptr, size_t selem, size_t nelem, csc_io_file_t *f)
{
    void *htmp;
    struct _compressed_io_handler_t * handler;
    size_t nbytes = selem * nelem;
    size_t remain_buffer, pos_read = 0;
    char *_ptr = ptr;

    if (!f) return 0;
    if ( selem == 0 ) return 0;
    if ( nelem == 0 ) return 0;
    if (f->mode != CSC_IO_FILE_READ) return 0;

    htmp = (void *) (f->handler);
    handler = (struct _compressed_io_handler_t *) htmp;

    if ( handler->read == NULL ) {
        csc_error_message("The used io-handler does not provide any read function.\n");
        return -1;
    }

    remain_buffer = f->avail-f->pos;

    if ( remain_buffer > nbytes ) {
        memcpy(_ptr, &(f->buffer[f->pos]), nbytes);
        f->pos += nbytes;
        pos_read += nbytes;
    } else {
        while (nbytes > 0 ) {
            size_t len;
            remain_buffer = f->avail-f->pos;
            len = MIN(remain_buffer, nbytes);
            memcpy(&_ptr[pos_read], &(f->buffer[f->pos]), len);
            pos_read += len;
            nbytes -= len;
            f->pos += len;
            if ( f -> pos == f->avail ) {
                f->avail = handler->read(f->data, f->buffer, CSC_IO_FILE_BUFFER_SIZE);
                if ( f->avail <= 0) {
                    f->eof = 1;
                }
                f->pos = 0;
            }
        }

    }
    return (pos_read/selem);

}

/* Write a character */
int csc_io_putc ( int c, csc_io_file_t *f  )
{
    void *htmp;
    struct _compressed_io_handler_t * handler;

    size_t w = 0;
    if (!f) return EOF;
    if (f->mode != CSC_IO_FILE_WRITE) return EOF;

    htmp = (void *) (f->handler);
    handler = (struct _compressed_io_handler_t *) htmp;

    if ( handler->write == NULL ) {
        csc_error_message("The used io-handler does not provide any write function.\n");
        return -1;
    }

    f->buffer[f->pos++] = (char) c;
    if ( f -> pos == CSC_IO_FILE_BUFFER_SIZE ) {
        f->pos = 0;
        w = handler->write(f->data, f->buffer, CSC_IO_FILE_BUFFER_SIZE);
        if ( w != CSC_IO_FILE_BUFFER_SIZE ) {
            return EOF;
        }
    }
    return c;
}

/* Write a string  */
int csc_io_puts ( const char *buf, csc_io_file_t * f)
{
    _compressed_io_handler * h;
    size_t len, free_in_buffer;
    size_t to_write;
    size_t w;
    size_t written = 0;

    if ( f == NULL ) return -1;
    if ( buf == NULL) return -1;
    if ( f->mode != CSC_IO_FILE_WRITE )  return -1;


    h = ( _compressed_io_handler *) f->handler;
    if ( h->write == NULL ) {
        csc_error_message("The used io-handler does not provide any write function.\n");
        return -1;
    }


    len = strlen(buf);

    free_in_buffer = CSC_IO_FILE_BUFFER_SIZE - f->pos;
    if ( len < free_in_buffer ) {
        memcpy(&f->buffer[f->pos],buf, len);
        f->pos += len;
        written = len;
    } else {
        while (len > 0 ) {
            free_in_buffer = CSC_IO_FILE_BUFFER_SIZE - f->pos;
            to_write = MIN (free_in_buffer, len);
            memcpy(&f->buffer[f->pos],buf, to_write );
            f->pos += to_write;
            if ( f -> pos == CSC_IO_FILE_BUFFER_SIZE ) {
                f->pos = 0;
                w = h->write(f->data, f->buffer, CSC_IO_FILE_BUFFER_SIZE);
                if ( w != CSC_IO_FILE_BUFFER_SIZE ) {
                    csc_error_message("Writing Buffer to File failed. Only %d of %d bytes written.\n", w, CSC_IO_FILE_BUFFER_SIZE);
                    return -1;
                }
            }
            buf += to_write;
            len -= to_write;
            written += to_write;
        }
    }
    return written;
}

/* Formated printing   */
#ifdef CSC_HAVE_VSNPRINTF
static char * make_message(const char *fmt, va_list ap)
{
    int n;
    int size = 100;     /* Guess we need no more than 100 bytes. */
    char *p, *np;

    if ((p = malloc(size)) == NULL)
        return NULL;

    while (1) {

        /* Try to print in the allocated space. */
        // va_start(ap, fmt);
        n = vsnprintf(p, size, fmt, ap);
        // va_end(ap);

        /* If that worked, return the string. */
        if (n > -1 && n < size)
            return p;

        /* Else try again with more space. */

        if (n > -1)    /* glibc 2.1 */
            size = n+1; /* precisely what is needed */
        else           /* glibc 2.0 */
            size *= 2;  /* twice the old size */

        if ((np = realloc (p, size)) == NULL) {
            free(p);
            return NULL;
        } else {
            p = np;
        }
    }
    return NULL;
}

int csc_io_printf( csc_io_file_t * f, const char * fmt, ...)
{
    va_list args;
    char *str;
    int ret = 0;

    if ( f == NULL) return -1;
    if ( fmt == NULL) return -1;

    va_start(args, fmt);
    str = make_message(fmt, args);
    va_end(args);
    if ( str == NULL )
        return -1;
    ret = csc_io_puts(str,f );
    free(str);
    return ret;
}


#endif

/* Binary Write */
size_t csc_io_write( void * ptr, size_t selem, size_t nelem, csc_io_file_t *f)
{
    _compressed_io_handler * h;
    size_t len, free_in_buffer;
    size_t to_write;
    size_t w;
    size_t written = 0;
    char *buf = ptr;

    if ( f == NULL ) return 0;
    if ( buf == NULL) return 0;
    if ( f->mode != CSC_IO_FILE_WRITE )  return 0;
    if ( selem == 0 ) return 0;


    h = ( _compressed_io_handler *) f->handler;
    if ( h->write == NULL ) {
        csc_error_message("The used io-handler does not provide any write function.\n");
        return 0;
    }
    len = selem * nelem;

    free_in_buffer = CSC_IO_FILE_BUFFER_SIZE - f->pos;
    if ( len < free_in_buffer ) {
        memcpy(&f->buffer[f->pos],buf, len);
        f->pos += len;
        written = len;
    } else {
        while (len > 0 ) {
            free_in_buffer = CSC_IO_FILE_BUFFER_SIZE - f->pos;
            to_write = MIN (free_in_buffer, len);
            memcpy(&f->buffer[f->pos],buf, to_write );
            f->pos += to_write;
            if ( f -> pos == CSC_IO_FILE_BUFFER_SIZE ) {
                f->pos = 0;
                w = h->write(f->data, f->buffer, CSC_IO_FILE_BUFFER_SIZE);
                if ( w != CSC_IO_FILE_BUFFER_SIZE ) {
                    csc_error_message("Writing Buffer to File failed. Only %d of %d bytes written.\n", w, CSC_IO_FILE_BUFFER_SIZE);
                    return -1;
                }
            }
            buf += to_write;
            len -= to_write;
            written += to_write;
        }
    }
    return written/selem;

}


