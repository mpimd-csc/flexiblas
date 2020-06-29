/*
 * CSCUTILS - A collection of various software routines uses in CSC projects
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

#include "cscutils/io.h"
#include "cscutils/mtx.h"
#include "cscutils/error_message.h"


int csc_mtx_write_double_dense(char * filename, int m, int n, double *A, int lda)
{
    csc_io_file_t *file;
    size_t _m, _n, k, l, _lda;

    file = csc_io_open(filename, CSC_IO_FILE_WRITE);
    if ( file == NULL) {
        csc_error_message("Failed to open %s for writing.\n", filename);
        return -1;
    }

    csc_io_printf(file, "%%%%MatrixMarket matrix array real general\n");
    csc_io_printf(file, "%d %d\n", m, n);

    _m = m;
    _n = n;
    _lda = lda;

    for ( l = 0; l < _n; l++) {
        for (k = 0; k < _m; k++) {
            csc_io_printf(file, "%20.17e\n", A[_lda*l+k]);
        }
    }
    csc_io_close(file);
    return 0;
}

