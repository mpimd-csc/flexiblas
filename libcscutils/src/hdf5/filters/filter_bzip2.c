/*
 * LIBCSCUTILS - FILTER HDF5 BZIP2
 * Copyright (C) Martin Koehler, 2018
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
#include <hdf5.h>
#include "filter_bzip2.h"
#include "cscutils/error_message.h"
#include "cscutils/cscutils_config.h"

#ifdef CSC_HAVE_BZIP2

#include <bzlib.h>
static size_t H5Z_filter_bzip2(unsigned int flags, size_t cd_nelmts,
        const unsigned int cd_values[], size_t nbytes,
        size_t *buf_size, void **buf);


const H5Z_class2_t H5Z_BZIP2[1] = {
    {
        H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
        (H5Z_filter_t)H5Z_FILTER_BZIP2,         /* Filter id number             */
        1,              /* encoder_present flag (set to true) */
        1,              /* decoder_present flag (set to true) */
        "bzip2",                  /* Filter name for debugging    */
        NULL,                       /* The "can apply" callback     */
        NULL,                       /* The "set local" callback     */
        (H5Z_func_t)H5Z_filter_bzip2,         /* The actual filter function   */
    }
};

static size_t H5Z_filter_bzip2(unsigned int flags, size_t cd_nelmts,
        const unsigned int cd_values[], size_t nbytes,
        size_t *buf_size, void **buf)
{
    char *outbuf = NULL;
    size_t outbuflen, outdatalen;
    int ret;

    if (flags & H5Z_FLAG_REVERSE) {

        /** Decompress data.
         **
         ** This process is troublesome since the size of uncompressed data
         ** is unknown, so the low-level interface must be used.
         ** Data is decompressed to the output buffer (which is sized
         ** for the average case); if it gets full, its size is doubled
         ** and decompression continues.  This avoids repeatedly trying to
         ** decompress the whole block, which could be really inefficient.
         **/

        bz_stream stream;
        char *newbuf = NULL;
        size_t newbuflen;

        /* Prepare the output buffer. */
        outbuflen = nbytes * 3 + 1;  /* average bzip2 compression ratio is 3:1 */
        outbuf = malloc(outbuflen);
        if (outbuf == NULL) {
            fprintf(stderr, "memory allocation failed for bzip2 decompression\n");
            goto cleanupAndFail;
        }

        /* Use standard malloc()/free() for internal memory handling. */
        stream.bzalloc = NULL;
        stream.bzfree = NULL;
        stream.opaque = NULL;

        /* Start decompression. */
        ret = BZ2_bzDecompressInit(&stream, 0, 0);
        if (ret != BZ_OK) {
            fprintf(stderr, "bzip2 decompression start failed with error %d\n", ret);
            goto cleanupAndFail;
        }

        /* Feed data to the decompression process and get decompressed data. */
        stream.next_out = outbuf;
        stream.avail_out = outbuflen;
        stream.next_in = *buf;
        stream.avail_in = nbytes;
        do {
            ret = BZ2_bzDecompress(&stream);
            if (ret < 0) {
                fprintf(stderr, "BUG: bzip2 decompression failed with error %d\n", ret);
                goto cleanupAndFail;
            }

            if (ret != BZ_STREAM_END && stream.avail_out == 0) {
                /* Grow the output buffer. */
                newbuflen = outbuflen * 2;
                newbuf = realloc(outbuf, newbuflen);
                if (newbuf == NULL) {
                    fprintf(stderr, "memory allocation failed for bzip2 decompression\n");
                    goto cleanupAndFail;
                }
                stream.next_out = newbuf + outbuflen;  /* half the new buffer behind */
                stream.avail_out = outbuflen;  /* half the new buffer ahead */
                outbuf = newbuf;
                outbuflen = newbuflen;
            }
        } while (ret != BZ_STREAM_END);

        /* End compression. */
        outdatalen = stream.total_out_lo32;
        ret = BZ2_bzDecompressEnd(&stream);
        if (ret != BZ_OK) {
            fprintf(stderr, "bzip2 compression end failed with error %d\n", ret);
            goto cleanupAndFail;
        }

    } else {

        /** Compress data.
         **
         ** This is quite simple, since the size of compressed data in the worst
         ** case is known and it is not much bigger than the size of uncompressed
         ** data.  This allows us to use the simplified one-shot interface to
         ** compression.
         **/

        unsigned int odatalen;  /* maybe not the same size as outdatalen */
        int blockSize100k = 9;

        /* Get compression block size if present. */
        if (cd_nelmts > 0) {
            blockSize100k = cd_values[0];
            if (blockSize100k < 1 || blockSize100k > 9) {
                fprintf(stderr, "invalid compression block size: %d\n", blockSize100k);
                goto cleanupAndFail;
            }
        }

        /* Prepare the output buffer. */
        outbuflen = nbytes + nbytes / 100 + 600;  /* worst case (bzip2 docs) */
        outbuf = malloc(outbuflen);
        if (outbuf == NULL) {
            fprintf(stderr, "memory allocation failed for bzip2 compression\n");
            goto cleanupAndFail;
        }

        /* Compress data. */
        odatalen = outbuflen;
        ret = BZ2_bzBuffToBuffCompress(outbuf, &odatalen, *buf, nbytes,
                blockSize100k, 0, 0);
        outdatalen = odatalen;
        if (ret != BZ_OK) {
            fprintf(stderr, "bzip2 compression failed with error %d\n", ret);
            goto cleanupAndFail;
        }
    }

    /* Always replace the input buffer with the output buffer. */
    free(*buf);
    *buf = outbuf;
    *buf_size = outbuflen;
    return outdatalen;

cleanupAndFail:
    if (outbuf)
        free(outbuf);
    return 0;
}

int csc_hdf5_register_bzip2()
{
    herr_t status;
    status = H5Zregister(H5Z_BZIP2);
    if ( status < 0 )
        return -1;
    else
        return 0;
}



#else

/* Filter not available  */
int csc_hdf5_register_bzip2()
{
    return -1;
}
#endif
