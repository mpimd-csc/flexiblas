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
#include "filter_zstd.h"
#include "cscutils/error_message.h"
#include "cscutils/cscutils_config.h"

#ifdef CSC_HAVE_ZSTD

#include <zstd.h>

static size_t H5Z_filter_zstd(unsigned int flags, size_t cd_nelmts,
        const unsigned int cd_values[], size_t nbytes,
        size_t *buf_size, void **buf);


const H5Z_class2_t H5Z_ZSTD[1] = {
    {
        H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
        (H5Z_filter_t)H5Z_FILTER_ZSTD,         /* Filter id number             */
        1,              /* encoder_present flag (set to true) */
        1,              /* decoder_present flag (set to true) */
        "zstd",                  /* Filter name for debugging    */
        NULL,                       /* The "can apply" callback     */
        NULL,                       /* The "set local" callback     */
        (H5Z_func_t)H5Z_filter_zstd,         /* The actual filter function   */
    }
};


static size_t H5Z_filter_zstd(unsigned int flags, size_t cd_nelmts,
		const unsigned int cd_values[], size_t nbytes,
		size_t *buf_size, void **buf)
{
	void *outbuf = NULL;    /* Pointer to new output buffer */
	void *inbuf = NULL;    /* Pointer to input buffer */
	inbuf = *buf;

	size_t ret_value;
	size_t origSize = nbytes;     /* Number of bytes for output (compressed) buffer */

	if (flags & H5Z_FLAG_REVERSE)
	{
		size_t decompSize = ZSTD_getDecompressedSize(*buf, origSize);
		if (NULL == (outbuf = malloc(decompSize)))
			goto error;

		decompSize = ZSTD_decompress(outbuf, decompSize, inbuf, origSize);

		free(*buf);
		*buf = outbuf;
		outbuf = NULL;
		ret_value = (size_t)decompSize;
	}
	else
	{
		int aggression = 4;
		if (cd_nelmts > 0)
			aggression = (int)cd_values[0];
		if (aggression > 22)
			aggression = 22;

		size_t compSize = ZSTD_compressBound(origSize);
		if (NULL == (outbuf = malloc(compSize)))
			goto error;

		compSize = ZSTD_compress(outbuf, compSize, inbuf, origSize, aggression);

		free(*buf);
		*buf = outbuf;
		*buf_size = compSize;
		outbuf = NULL;
		ret_value = compSize;
	}
	if (outbuf != NULL)
		free(outbuf);
	return ret_value;

error:
	return 0;

}

int csc_hdf5_register_zstd(void)
{
    herr_t status;
    status = H5Zregister(H5Z_ZSTD);
    if ( status < 0 )
        return -1;
    else
        return 0;
}



#else

/* Filter not available  */
int csc_hdf5_register_zstd(void)
{
    return -1;
}
#endif
