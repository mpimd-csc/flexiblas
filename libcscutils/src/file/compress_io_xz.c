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
#include <stdbool.h>
#include "cscutils/cscutils_config.h"

#ifdef CSC_HAVE_LZMA
#include <lzma.h>

#include "cscutils/io.h"
#include "compress_handler.h"
#include "cscutils/error_message.h"

#define TRUE -1
#define FALSE 0

#define XZ_PRESET 8
#define XZ_BUFFERSIZE 4096
// #define XZ_BUFFERSIZE 2041

#define MAX(A,B) (((A)>(B))?(A):(B));
#define MIN(A,B) (((A)<(B))?(A):(B));

static int xz_open (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler * handler);
static int xz_close(void **data);
static size_t xz_read(void *data, void *buf, size_t len);
static size_t xz_write(void *data, void *buf, size_t len);



/* Internal structure to represent an uncompressed file. */
typedef struct {
    FILE *fp;
    char * path;
    csc_io_mode_t mode;
    int readonly;
    lzma_stream strm;
    uint8_t inbuf[XZ_BUFFERSIZE];
    uint8_t outbuf[XZ_BUFFERSIZE];
    uint8_t readbuf[XZ_BUFFERSIZE];
    size_t readpos;
    int eof;
    lzma_action action;
}_xz_file;

/* Registers the access to uncompressed files. */
_compressed_io_handler io_register_xz(void){
    _compressed_io_handler handler;
    const char HEADER_MAGIC[6] = { (char) 0xFD, '7', 'z', 'X', 'Z', 0x00 };

    strcpy(handler.extension, ".xz");
    handler.type = CSC_IO_FILE_XZ;
    strcpy(handler.magic,(char *)(uintptr_t) (const void*)  HEADER_MAGIC);
    handler.open  = xz_open;
    handler.close = xz_close;
    handler.write = xz_write;
    handler.read  = xz_read;
    return handler;
}

static int init_encoder(lzma_stream *strm, uint32_t preset)
{
    lzma_ret ret = lzma_easy_encoder(strm, preset, LZMA_CHECK_CRC32);
    // Return successfully if the initialization went fine.
    if (ret == LZMA_OK)
        return TRUE;

    const char *msg;
    switch (ret) {
        case LZMA_MEM_ERROR:
            msg = "Memory allocation failed";
            break;

        case LZMA_OPTIONS_ERROR:
            msg = "Specified preset is not supported";
            break;

        case LZMA_UNSUPPORTED_CHECK:
            msg = "Specified integrity check is not supported";
            break;

        default:
            msg = "Unknown error, possibly a bug";
            break;
    }
    csc_error_message("Error initializing the encoder: %s (error code %u)\n", msg, ret);
    return FALSE;
}

static bool init_decoder(lzma_stream *strm)
{
    lzma_ret ret = lzma_stream_decoder( strm, UINT64_MAX, 0x00 | LZMA_CONCATENATED );

    // Return successfully if the initialization went fine.
    if (ret == LZMA_OK)
        return TRUE;

    const char *msg;
    switch (ret) {
        case LZMA_MEM_ERROR:
            msg = "Memory allocation failed";
            break;

        case LZMA_OPTIONS_ERROR:
            msg = "Unsupported decompressor flags";
            break;

        default:
            msg = "Unknown error, possibly a bug";
            break;
    }

    csc_error_message("Error initializing the decoder: %s (error code %u)\n",msg, ret);
    return FALSE;
}




/* Local function which opens an uncompressed file. */
static int xz_open (void **data, const char * path, const csc_io_mode_t mode, _compressed_io_handler * handler) {
    _xz_file *f;
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
    /* Initlize the data structure  */
    f = (_xz_file * ) malloc( sizeof(_xz_file));
    memset(&(f->strm), 0, sizeof(f->strm));
    f->path = strdup(path);
    f->mode = mode;
    *data = (void *) f;
    if ( (strstr(_mode, "w")) || (strstr(_mode, "+")) || (strstr(_mode, "a") )) {
        // MSG_INFO ("open %s for writing\n", path);
        f->readonly = 0;
    } else {
        f->readonly = 1;
    }

    /* Init LZMA  */
    if ( f->readonly == 0 ) {
        /* Open for writing */
        f->fp =fopen(f->path, "w");
        if ( !(f->fp)) {
            err = errno;
            csc_warn_message("opening file: %s failed, errno: %03d - %s\n", path, err, strerror(err));
            free(f);
            return -1;
        }
        if ( !init_encoder(&(f->strm), XZ_PRESET) ) {
            csc_warn_message("Can not initialize lzma encoder\n");
            free(f);
            return -1;
        }
        f->strm.next_in = NULL;
        f->strm.avail_in = 0;
        f->strm.next_out = f->outbuf;
        f->strm.avail_out = sizeof(f->outbuf);
        f->action = LZMA_RUN;
    } else {
        /* Open for READING */
        f->fp =fopen(f->path, "r");
        if ( !(f->fp)) {
            err = errno;
            csc_warn_message("opening file: %s failed, errno: %03d - %s\n", path, err, strerror(err));
            free(f);
            return -1;
        }
        if ( !init_decoder(&(f->strm)) ) {
            csc_warn_message("Can not initialize lzma decoder\n");
            free(f);
            return -1;
        }
        f->strm.next_in = NULL;
        f->strm.avail_in = 0;
        f->strm.next_out = f->outbuf;
        f->strm.avail_out = sizeof(f->outbuf);
        f->readpos = 0;
        f->eof = 0;
        f->action = LZMA_RUN;
    }
    return 0;
}


/* Local function to close and free an uncompressed file */
static int xz_close(void **data) {
    _xz_file *f;
    // lzma_action action = LZMA_FINISH;
    lzma_ret ret;

    if ( data == NULL ) {
        csc_warn_message("Error: data == NULL\n");

        return -1;
    }
    if ( *data == NULL ) {
        csc_warn_message("Error: *data == NULL\n");
        return -1;
    }

    f = (_xz_file*) *data;

    if ( f->readonly == 0 ) {
        f->action = LZMA_FINISH;
        ret = LZMA_OK;
        while ( ret != LZMA_STREAM_END ) {
            /* Finish the encoding   */
            ret = lzma_code(&(f->strm), f->action);

            // If the output buffer is full or if the compression finished
            // successfully, write the data from the output bufffer to
            // the output file.
            if (f->strm.avail_out == 0 || ret == LZMA_STREAM_END) {
                // When lzma_code() has returned LZMA_STREAM_END,
                // the output buffer is likely to be only partially
                // full. Calculate how much new data there is to
                // be written to the output file.
                size_t write_size = sizeof(f->outbuf) - f->strm.avail_out;

                if (fwrite(f->outbuf, 1, write_size, f->fp) != write_size) {
                    csc_warn_message("Write error: %s\n", strerror(errno));
                    return 0;
                }

                // Reset next_out and avail_out.
                f->strm.next_out = f->outbuf;
                f->strm.avail_out = sizeof(f->outbuf);

            }
            if ( ret != LZMA_STREAM_END && ret != LZMA_OK) {
                csc_warn_message("LZMA Encode error\n");
                break;
            }
        }


        lzma_end(&(f->strm));
    } else {
        lzma_end(&(f->strm));
    }
    printf("close xz\n");
    fflush(f->fp);
    fclose(f->fp);
    f->fp = NULL;
    free(f->path);
    free(f);
    *data = NULL;
    return 0;
}


/* Local function to read a block from an uncompressed file */
static size_t xz_read(void *data, void *buf, size_t len) {
    _xz_file *f;
    // lzma_action action = LZMA_RUN;
    lzma_ret ret;
    size_t pos = 0;
    uint8_t *ibuf = (uint8_t*) buf;
    size_t read_entries = 0;

    if ( data == NULL) return 0;
    if ( buf == NULL) return 0;
    f = (_xz_file*) data;
    if ( len == 0) return 0;

    if ( f->readpos == 0 && f->eof ) return 0;


    if ( f->readpos > 0 ) {
        size_t remain_to_copy = XZ_BUFFERSIZE - f->readpos;
        if ( remain_to_copy > len ) {
            memcpy(buf, &(f->readbuf[f->readpos]), sizeof(uint8_t) * len);
            f->readpos += len;
            return len;
        } else if ( remain_to_copy == len ) {
            memcpy(buf, &(f->readbuf[f->readpos]), sizeof(uint8_t) * len);
            f->readpos = 0;
            return len;
        } else {
            memcpy(buf, &(f->readbuf[f->readpos]), sizeof(uint8_t) * remain_to_copy);
            len = len - remain_to_copy;
            pos = remain_to_copy;
            read_entries = remain_to_copy;
            f->readpos = 0;
        }
    }

    f->strm.next_out = f->outbuf;
    f->strm.avail_out = sizeof(f->outbuf);

    while (! f->eof) {
        if (f->strm.avail_in == 0 && !feof(f->fp)) {
            f->strm.next_in = f->inbuf;
            f->strm.avail_in = fread(f->inbuf, 1, sizeof(f->inbuf),f->fp);

            if (ferror(f->fp)) {
                csc_error_message("Read error: %s\n",strerror(errno));
                return 0;
            }

            // Once the end of the input file has been reached,
            // we need to tell lzma_code() that no more input
            // will be coming. As said before, this isn't required
            // if the LZMA_CONATENATED flag isn't used when
            // initializing the decoder.
            if (feof(f->fp)){
                f->action = LZMA_FINISH;
            }
        }

        ret = lzma_code(&(f->strm), f->action);


        if (f->strm.avail_out == 0 || ret == LZMA_STREAM_END) {
            size_t write_size = sizeof(f->outbuf) - f->strm.avail_out;

            memcpy(f->readbuf, f->outbuf, sizeof(uint8_t) * write_size);

            if ( len > 0 ) {
                size_t towrite = MIN(len, write_size);
                memcpy(&ibuf[pos], f->readbuf, sizeof(uint8_t) * towrite);
                len = len - towrite;
                if ( len == 0 ) {
                    f->readpos = towrite;
                    read_entries += towrite;
                    break;
                } else {
                    pos += towrite;
                    read_entries += towrite;
                }
            }

            f->strm.next_out = f->outbuf;
            f->strm.avail_out = sizeof(f->outbuf);

            if ( ret == LZMA_STREAM_END) {
                f->eof = 1;
            }
        }

        if (ret != LZMA_OK && ret != LZMA_STREAM_END) {
            const char *msg;
            switch (ret) {
                case LZMA_MEM_ERROR:
                    msg = "Memory allocation failed";
                    break;

                case LZMA_FORMAT_ERROR:
                    // .xz magic bytes weren't found.
                    msg = "The input is not in the .xz format";
                    break;

                case LZMA_OPTIONS_ERROR:
                    msg = "Unsupported compression options";
                    break;

                case LZMA_DATA_ERROR:
                    msg = "Compressed file is corrupt";
                    break;

                case LZMA_BUF_ERROR:
                    msg = "Compressed file is truncated or otherwise corrupt";
                    break;
                case LZMA_MEMLIMIT_ERROR:
                    msg = "The memory limit for decompression is too small";
                    break;

                default:
                    msg = "Unknown error, possibly a bug";
                    break;
            }

            csc_error_message("Decoder error: %s (error code %u)\n",msg, ret);
            return 0;
        }
    }
    if ( f->readpos == XZ_BUFFERSIZE) {
        f->readpos = 0;
    }

    return read_entries;
}

/* Local function to write a block to an uncompressed file. */
static size_t xz_write(void *data, void *buf, size_t len){
    _xz_file *f;
    // lzma_action action = LZMA_RUN;
    lzma_ret ret;
    size_t pos = 0;
    char *cbuf;

    if ( data == NULL) return -1;
    if ( buf == NULL) return -1;
    f = (_xz_file*) data;

    while (true) {
        // Fill the input buffer if it is empty.
        if (f->strm.avail_in == 0 && pos != len) {
            size_t tocopy = MIN(XZ_BUFFERSIZE, (len-pos));
            cbuf = (char *) buf;
            memcpy(f->inbuf, cbuf+pos, tocopy);
            f->strm.next_in = f->inbuf;
            f->strm.avail_in = tocopy;
            pos = pos + tocopy;
        } else if ( pos >= len && f->strm.avail_in == 0 ) {
            return len;
        }

        ret = lzma_code(&(f->strm), f->action);

        if (f->strm.avail_out == 0 || ret == LZMA_STREAM_END) {
            size_t write_size = sizeof(f->outbuf) - f->strm.avail_out;

            if (fwrite(f->outbuf, 1, write_size, f->fp) != write_size) {
                csc_warn_message("Write error: %s\n", strerror(errno));
                return 0;
            }

            // Reset next_out and avail_out.
            f->strm.next_out = f->outbuf;
            f->strm.avail_out = sizeof(f->outbuf);
        }

        // Normally the return value of lzma_code() will be LZMA_OK
        // until everything has been encoded.
        if (ret != LZMA_OK) {
            const char *msg = "" ;

            if (ret == LZMA_STREAM_END)
                return true;
            switch (ret) {
                case LZMA_MEM_ERROR:
                    msg = "Memory allocation failed";
                    break;

                case LZMA_DATA_ERROR:
                    msg = "File size limits exceeded";
                    break;

                default:
                    break;
            }

            csc_warn_message("Encoder error: %s (error code %u)\n",msg, ret);
            return 0;
        }
    }
    return 0;
}
#endif /* NO_XZ */
