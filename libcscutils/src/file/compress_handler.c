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
#include <ctype.h>
#include <errno.h>

#include "cscutils/io.h"
#include "compress_handler.h"

/*-----------------------------------------------------------------------------
 *  Enable compression backends.
 *-----------------------------------------------------------------------------*/
#ifdef CSC_HAVE_ZLIB
#define USE_GZIP
#endif

#ifdef CSC_HAVE_BZIP2
#define USE_BZ2
#endif

#ifdef CSC_HAVE_LZMA
#define USE_LZMA
#endif


/*-----------------------------------------------------------------------------
 *  include compression specific headers
 *-----------------------------------------------------------------------------*/

#ifdef  USE_GZIP
#include <zlib.h>
extern _compressed_io_handler io_register_gzip(void);
#else
extern _compressed_io_handler io_register_no_gzip(void);
#endif
#ifdef  USE_BZ2
#include <bzlib.h>
extern _compressed_io_handler io_register_bzip2(void);
#else
extern _compressed_io_handler io_register_no_bzip2(void);
#endif
#ifdef USE_LZMA
extern _compressed_io_handler io_register_xz(void);
#else
extern _compressed_io_handler io_register_no_xz(void);
#endif

extern _compressed_io_handler io_register_uncompressed(void);
typedef _compressed_io_handler (*compress_handler_init)(void);


/*-----------------------------------------------------------------------------
 *  static Array containing the register functions for the I/0 handlers.
 *-----------------------------------------------------------------------------*/
static const compress_handler_init handler[] = {
    io_register_uncompressed
#ifdef USE_GZIP
        , io_register_gzip
#else
        , io_register_no_gzip
#endif
#ifdef USE_BZ2
        , io_register_bzip2
#else
        , io_register_no_bzip2
#endif
#ifdef USE_LZMA
        , io_register_xz
#else
        , io_register_no_xz
#endif
};


/*-----------------------------------------------------------------------------
 *  Global array with the IO handlers
 *-----------------------------------------------------------------------------*/
#define LEN_COMPRESS_METHODS  sizeof(handler)/sizeof(compress_handler_init)
_compressed_io_handler compress_methods[LEN_COMPRESS_METHODS];
int _compressed_io_handler_len = LEN_COMPRESS_METHODS;
_compressed_io_handler compress_fallback;

static int is_initialized = 0;


/*-----------------------------------------------------------------------------
 *  Setup the available IO handlers.
 *-----------------------------------------------------------------------------*/
#ifdef CSC_HAVE_ATTR_CONSTRUCTOR
void csc_io_init (void)  __attribute__((constructor));
#endif

void csc_io_init (void)
{
    if (! is_initialized ){
        int i = 0 ;
        for ( i = 0; i < _compressed_io_handler_len; i++){
            compress_handler_init h = handler[i];
            compress_methods[i] = h();
        }
        compress_fallback = io_register_uncompressed();
        is_initialized = 1;
    }
}



