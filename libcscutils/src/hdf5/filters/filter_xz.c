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
#include <stdlib.h>
#include <stdio.h>
#include <hdf5.h>
#include "filter_xz.h"
#include "cscutils/error_message.h"
#include "cscutils/cscutils_config.h"
#include <omp.h>

#ifdef CSC_HAVE_LZMA

#define XZ_PRESET 3
#include <lzma.h>


static size_t H5Z_filter_xz(unsigned int flags, size_t cd_nelmts,
        const unsigned int cd_values[], size_t nbytes,
        size_t *buf_size, void **buf);


const H5Z_class2_t H5Z_XZ[1] = {
    {
        H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
        (H5Z_filter_t)H5Z_FILTER_XZ,         /* Filter id number             */
        1,              /* encoder_present flag (set to true) */
        1,              /* decoder_present flag (set to true) */
        "xz",                  /* Filter name for debugging    */
        NULL,                       /* The "can apply" callback     */
        NULL,                       /* The "set local" callback     */
        (H5Z_func_t)H5Z_filter_xz,         /* The actual filter function   */
    }
};

#define SINGLE_THREAD
#ifdef SINGLE_THREAD
static int init_encoder(lzma_stream *strm, uint32_t preset)
{
    lzma_ret ret = lzma_easy_encoder(strm, preset, LZMA_CHECK_CRC64);
    // Return successfully if the initialization went fine.
    if (ret == LZMA_OK)
        return 1;

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
    return 0;
}
#else

static bool
init_encoder(lzma_stream *strm, uint32_t preset)
{
    lzma_mt mt = {
        .flags = 0,
        .block_size = 0,
        .timeout = 0,
        .preset = preset,
        .filters = NULL,
        .check = LZMA_CHECK_CRC64,
    };
    // mt.threads = lzma_cputhreads();
    mt.threads = omp_get_max_threads();
    printf("XZ threads: %d\n", mt.threads);
    if (mt.threads == 0)
        mt.threads = 1;
    const uint32_t threads_max = 16;
    if (mt.threads > threads_max)
        mt.threads = threads_max;
    lzma_ret ret = lzma_stream_encoder_mt(strm, &mt);

    if (ret == LZMA_OK)
        return true;

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
    return 0;
}



#endif

static int init_decoder(lzma_stream *strm)
{
    lzma_ret ret = lzma_stream_decoder( strm, UINT64_MAX, 0x00 | LZMA_CONCATENATED );

    // Return successfully if the initialization went fine.
    if (ret == LZMA_OK)
        return 1;

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
    return 0;
}


static size_t H5Z_filter_xz(unsigned int flags, size_t cd_nelmts,
        const unsigned int cd_values[], size_t nbytes,
        size_t *buf_size, void **buf)
{
    char *outbuf = NULL;
    size_t outbuflen, outdatalen;
    int ret;

    (void) (cd_values);
    (void) (cd_nelmts);

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
        int err = 0;
        lzma_action action = LZMA_RUN;
        lzma_stream strm = LZMA_STREAM_INIT;
        if ( !(ret = init_decoder(&strm)) ) {
            csc_error_message("xz decompression start failed with error %d\n", ret);
            goto cleanupAndFail;

        }

        /* Prepare the output buffer. */
        outbuflen = nbytes * 4 + 1;  /* average xz compression ratio is 4:1 */
        outbuf = malloc(outbuflen);
        if (outbuf == NULL) {
            csc_error_message("memory allocation failed for xz decompression\n");
            goto cleanupAndFail;
        }


        /* Feed data to the decompression process and get decompressed data. */
        strm.next_in  = (unsigned char *) *buf ;
        strm.avail_in = *buf_size;
        strm.next_out = (unsigned char *) outbuf;
        strm.avail_out = outbuflen;

        /* printf("bufsize = %lu\n", *buf_size); */
        /* printf("outbuflen = %lu\n", outbuflen); */

		while (1) {
            // Fill the input buffer if it is empty.
            if (strm.avail_in == 0)  {
                action = LZMA_FINISH;
            }

            lzma_ret retu = lzma_code(&strm, action);

            if (strm.avail_out == 0 || retu == LZMA_STREAM_END) {
                if ( ! (retu == LZMA_STREAM_END)){
                    size_t s = outbuflen;
                    outbuflen = outbuflen + nbytes;
                    outbuf = realloc(outbuf, outbuflen);
                    strm.next_out = (unsigned char *) outbuf + s;
                    strm.avail_out = nbytes;
                } else {
                    outdatalen = outbuflen - strm.avail_out;
                }
            }

    		if (retu != LZMA_OK) {
                // Once everything has been encoded successfully, the
                // return value of lzma_code() will be LZMA_STREAM_END.
                //
                // It is important to check for LZMA_STREAM_END. Do not
                // assume that getting ret != LZMA_OK would mean that
                // everything has gone well.
                if (retu == LZMA_STREAM_END)
                    break;

                err = 1;
                // It's not LZMA_OK nor LZMA_STREAM_END,
                // so it must be an error code. See lzma/base.h
                // (src/liblzma/api/lzma/base.h in the source package
                // or e.g. /usr/include/lzma/base.h depending on the
                // install prefix) for the list and documentation of
                // possible values. Most values listen in lzma_ret
                // enumeration aren't possible in this example.
                switch (retu) {
                    case LZMA_MEM_ERROR:
                        csc_error_message("Memory allocation failed for xz\n");
                        break;
                    case LZMA_DATA_ERROR:
                        csc_error_message("File size limits exceeded for xz\n");
                        break;
                    default:
                        csc_error_message("Unknown error, possibly a bug in xz\n");
                        break;
    			}
			    break;
		    }
        }
        /* End compression. */
        outdatalen = strm.total_out;

        lzma_end(&strm);
        if ( err ) {
            goto cleanupAndFail;
        }

        /* printf("written = %lu\n", outdatalen); */

    } else {

        /** Compress data.
         **
         ** This is quite simple, since the size of compressed data in the worst
         ** case is known and it is not much bigger than the size of uncompressed
         ** data.  This allows us to use the simplified one-shot interface to
         ** compression.
         **/

        int err = 0;

        lzma_stream strm = LZMA_STREAM_INIT;
        if ( ! (ret =  init_encoder(&strm, XZ_PRESET)) ) {
            csc_error_message("xz decompression start failed with error %d\n", ret);
            goto cleanupAndFail;

        }
        /* Prepare the output buffer. */
        outbuflen = nbytes + nbytes / 100 + 2000;
        outbuf = malloc(outbuflen);
        if (outbuf == NULL) {
            csc_error_message("memory allocation failed for xz compression\n");
            goto cleanupAndFail;
        }
        lzma_action action = LZMA_RUN;

        strm.next_in  = (unsigned char *) *buf ;
        strm.avail_in = *buf_size;
        strm.next_out = (unsigned char *) outbuf;
        strm.avail_out = outbuflen;

        /* printf("bufsize = %lu\n", *buf_size); */
        /* printf("outbuflen = %lu\n", outbuflen); */

		while (1) {
            if (strm.avail_in == 0)  {
                action = LZMA_FINISH;
            }

            lzma_ret retu = lzma_code(&strm, action);

            if (strm.avail_out == 0 || retu == LZMA_STREAM_END) {
                if ( ! (retu == LZMA_STREAM_END)){
                    size_t s = outbuflen;
                    outbuflen = outbuflen + nbytes;
                    outbuf = realloc(outbuf, outbuflen);
                    strm.next_out = (unsigned char *) outbuf + s;
                    strm.avail_out = nbytes;
                } else {
                    outdatalen = outbuflen - strm.avail_out;
                }
            }

    		if (retu != LZMA_OK) {
                if (retu == LZMA_STREAM_END)
                    break;

                err = 1;
                // It's not LZMA_OK nor LZMA_STREAM_END,
                // so it must be an error code. See lzma/base.h
                // (src/liblzma/api/lzma/base.h in the source package
                // or e.g. /usr/include/lzma/base.h depending on the
                // install prefix) for the list and documentation of
                // possible values. Most values listen in lzma_ret
                // enumeration aren't possible in this example.
                switch (retu) {
                    case LZMA_MEM_ERROR:
                        csc_error_message("Memory allocation failed for xz\n");
                        break;
                    case LZMA_DATA_ERROR:
                        csc_error_message("File size limits exceeded for xz\n");
                        break;
                    default:
                        csc_error_message("Unknown error, possibly a bug in xz\n");
                        break;
    			}
			    break;
		    }
        }

        outdatalen = strm.total_out;
        lzma_end(&strm);

        if ( err ) {
            goto cleanupAndFail;
        }
               /* printf("written = %lu\n", outdatalen); */
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

int csc_hdf5_register_xz(void)
{
    herr_t status;
    status = H5Zregister(H5Z_XZ);
    if ( status < 0 )
        return -1;
    else
        return 0;
}



#else

/* Filter not available  */
int csc_hdf5_register_xz(void)
{
    return -1;
}
#endif
