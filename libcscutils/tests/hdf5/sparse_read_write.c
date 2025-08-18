//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
#include <string.h>
#include <unistd.h>
// #define H5_NO_DEPRECATED_SYMBOLS
#include "cscutils/hdf.h"

int read_write_sparse(int matlab, int octave)
{
    hid_t h5file;
    csc_hdf5_options_t opts = CSC_HDF5_OPTIONS_INIT;

    double values[10]={1,2,3,4,5,6,7,8,9,10};
    float  fvalues[10]={1,2,3,4,5,6,7,8,9,10};
    double complex valuesc[10]={1+1*I,2,3,4,5,6,7,8,9,10};
    float  complex fvaluesc[10]={1-1*I,2,3,4,5,6,7,8,9,10};

    size_t rows, cols, nnz;
    int rrowptr[10], rcolptr[6];
    unsigned int rurowptr[10], rucolptr[6];
    long rlrowptr[10], rlcolptr[6];
    unsigned long rulrowptr[10], rulcolptr[6];
    double rvalues[10];
    float rfvalues[10];
    double complex rvaluesc[10];
    float  complex rfvaluesc[10];

    int rowptr[10] = {0,1 ,1,2, 2,3, 3,4, 0,4};
    int colptr[6] = {0, 2, 4, 6, 8, 10};
    unsigned int urowptr[10] = {0,1 ,1,2, 2,3, 3,4, 0, 4};
    unsigned int ucolptr[6] = {0, 2, 4, 6, 8, 10};
    long lrowptr[10] = {0,1 ,1,2, 2,3, 3,4, 0,4};
    long lcolptr[6] = {0, 2, 4, 6, 8, 10};
    unsigned long ulrowptr[10] = {0,1 ,1,2, 2,3, 3,4, 0, 4};
    unsigned long ulcolptr[6] = {0, 2, 4, 6, 8, 10};
    int cnt_err = 0;

    // Write Octave Format
    opts.matlab_attributes = matlab;
    opts.octave_style = octave;

#define CHECK_CALL(x) do { if ( x ) { \
        csc_error_message("%s:%d: Call %s failed.", __FILE__, __LINE__, #x); cnt_err++; \
    } } while (0)

    if ( matlab ) {
       h5file = csc_hdf5_open_matlab("test_sparse.h5", "rw");
    } else {
        h5file = csc_hdf5_open("test_sparse.h5", "rw");
    }
    if (h5file < 0 ) {
        csc_error_message("Failed to open test_sparse.h5");
        return -1;
    }

    CHECK_CALL(csc_hdf5_write_double_sparse_matrix_int(h5file, "sparse_real_int", 5, 5, 10, rowptr, colptr, values, &opts));
    CHECK_CALL(csc_hdf5_write_double_sparse_matrix_uint(h5file, "sparse_real_uint", 5, 5, 10, urowptr, ucolptr, values, &opts));
    CHECK_CALL(csc_hdf5_write_double_sparse_matrix_long(h5file, "sparse_real_long", 5, 5, 10, lrowptr, lcolptr, values, &opts));
    CHECK_CALL(csc_hdf5_write_double_sparse_matrix_ulong(h5file, "sparse_real_ulong", 5, 5, 10, ulrowptr, ulcolptr, values, &opts));
    CHECK_CALL(csc_hdf5_write_float_sparse_matrix_int(h5file,   "sparse_float_real_int", 5, 5, 10, rowptr, colptr, fvalues, &opts));
    CHECK_CALL(csc_hdf5_write_float_sparse_matrix_uint(h5file,  "sparse_float_real_uint", 5, 5, 10, urowptr, ucolptr, fvalues, &opts));
    CHECK_CALL(csc_hdf5_write_float_sparse_matrix_long(h5file,  "sparse_float_real_long", 5, 5, 10, lrowptr, lcolptr, fvalues, &opts));
    CHECK_CALL(csc_hdf5_write_float_sparse_matrix_ulong(h5file, "sparse_float_real_ulong", 5, 5, 10, ulrowptr, ulcolptr, fvalues, &opts));

    CHECK_CALL(csc_hdf5_write_double_complex_sparse_matrix_int(h5file, "sparse_complex_int", 5, 5, 10, rowptr, colptr, valuesc, &opts));
    CHECK_CALL(csc_hdf5_write_double_complex_sparse_matrix_uint(h5file, "sparse_complex_uint", 5, 5, 10, urowptr, ucolptr, valuesc, &opts));
    CHECK_CALL(csc_hdf5_write_double_complex_sparse_matrix_long(h5file, "sparse_complex_long", 5, 5, 10, lrowptr, lcolptr, valuesc, &opts));
    CHECK_CALL(csc_hdf5_write_double_complex_sparse_matrix_ulong(h5file, "sparse_complex_ulong", 5, 5, 10, ulrowptr, ulcolptr, valuesc, &opts));
    CHECK_CALL(csc_hdf5_write_float_complex_sparse_matrix_int(h5file,   "sparse_float_complex_int", 5, 5, 10, rowptr, colptr, fvaluesc, &opts));
    CHECK_CALL(csc_hdf5_write_float_complex_sparse_matrix_uint(h5file,  "sparse_float_complex_uint", 5, 5, 10, urowptr, ucolptr, fvaluesc, &opts));
    CHECK_CALL(csc_hdf5_write_float_complex_sparse_matrix_long(h5file,  "sparse_float_complex_long", 5, 5, 10, lrowptr, lcolptr, fvaluesc, &opts));
    CHECK_CALL(csc_hdf5_write_float_complex_sparse_matrix_ulong(h5file, "sparse_float_complex_ulong", 5, 5, 10, ulrowptr, ulcolptr, fvaluesc, &opts));



    csc_hdf5_close(h5file);

#define ASSERT( assertation ) if ( !(assertation)) { csc_error_message("%s:%5d: Assertation %s failed.", __FILE__, __LINE__, #assertation); cnt_err++; }
#define ASSERT_VECTOR( v1, v2, len) do { size_t k = 0; \
        for(k = 0; k < len; k++) { if ( v1[k] != v2[k] ) { csc_error_message("%s:%d: Assertation %s[%d] == %s[%d] failed.", __FILE__, __LINE__, #v1, k, #v2, k); cnt_err++;} v1[k] = -1;}; \
    } while(0)
#define CHECK_SIZE(set) do { size_t r=-1,c=-1,n=-1; \
    if (csc_hdf5_sparse_size(h5file, set, &r,&c,&n)) { csc_error_message("%s:%d: csc_hdf5_sparse_size failed.",__FILE__, __LINE__); cnt_err++;} \
    ASSERT(r ==5); \
    ASSERT(c ==5); \
    ASSERT(n ==10); } while(0)

    if ( matlab ) {
       h5file = csc_hdf5_open_matlab("test_sparse.h5", "r");
    } else {
        h5file = csc_hdf5_open("test_sparse.h5", "r");
    }
    if (h5file < 0 ) {
        csc_error_message("Failed to open test_sparse.h5");
        return -1;
    }

    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_real_int"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_real_uint"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_real_long"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_real_ulong"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_float_real_int"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_float_real_uint"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_float_real_long"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_float_real_ulong"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_complex_int"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_complex_uint"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_complex_long"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_complex_ulong"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_float_complex_int"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_float_complex_uint"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_float_complex_long"));
    ASSERT(csc_hdf5_is_sparse(h5file, "sparse_float_complex_ulong"));

    CHECK_SIZE( "sparse_real_int");
    CHECK_SIZE( "sparse_real_uint");
    CHECK_SIZE( "sparse_real_long");
    CHECK_SIZE( "sparse_real_ulong");
    CHECK_SIZE( "sparse_float_real_int");
    CHECK_SIZE( "sparse_float_real_uint");
    CHECK_SIZE( "sparse_float_real_long");
    CHECK_SIZE( "sparse_float_real_ulong");
    CHECK_SIZE( "sparse_complex_int");
    CHECK_SIZE( "sparse_complex_uint");
    CHECK_SIZE( "sparse_complex_long");
    CHECK_SIZE( "sparse_complex_ulong");
    CHECK_SIZE( "sparse_float_complex_int");
    CHECK_SIZE( "sparse_float_complex_uint");
    CHECK_SIZE( "sparse_float_complex_long");
    CHECK_SIZE( "sparse_float_complex_ulong");

    CHECK_CALL(csc_hdf5_read_double_sparse_matrix_int(h5file, "sparse_real_int", &rows, &cols, &nnz, rrowptr, rcolptr, rvalues, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rrowptr, rowptr, nnz); ASSERT_VECTOR(rcolptr, colptr, cols+1); ASSERT_VECTOR(rvalues, values, nnz);

    CHECK_CALL(csc_hdf5_read_double_sparse_matrix_uint(h5file, "sparse_real_uint", &rows, &cols, &nnz, rurowptr, rucolptr, rvalues, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rurowptr, urowptr, nnz); ASSERT_VECTOR(rucolptr, ucolptr, cols+1); ASSERT_VECTOR(rvalues, values, nnz);

    CHECK_CALL(csc_hdf5_read_double_sparse_matrix_long(h5file, "sparse_real_long", &rows, &cols, &nnz, rlrowptr, rlcolptr, rvalues, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rlrowptr, rowptr, nnz); ASSERT_VECTOR(rlcolptr, colptr, cols+1); ASSERT_VECTOR(rvalues, values, nnz);

    CHECK_CALL(csc_hdf5_read_double_sparse_matrix_ulong(h5file, "sparse_real_ulong", &rows, &cols, &nnz, rulrowptr, rulcolptr, rvalues, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rulrowptr, urowptr, nnz); ASSERT_VECTOR(rulcolptr, ucolptr, cols+1); ASSERT_VECTOR(rvalues, values, nnz);

    CHECK_CALL(csc_hdf5_read_float_sparse_matrix_int(h5file, "sparse_float_real_int", &rows, &cols, &nnz, rrowptr, rcolptr, rfvalues, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rrowptr, rowptr, nnz); ASSERT_VECTOR(rcolptr, colptr, cols+1); ASSERT_VECTOR(rfvalues, fvalues, nnz);

    CHECK_CALL(csc_hdf5_read_float_sparse_matrix_uint(h5file, "sparse_float_real_uint", &rows, &cols, &nnz, rurowptr, rucolptr, rfvalues, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rurowptr, urowptr, nnz); ASSERT_VECTOR(rucolptr, ucolptr, cols+1); ASSERT_VECTOR(rfvalues, fvalues, nnz);

    CHECK_CALL(csc_hdf5_read_float_sparse_matrix_long(h5file, "sparse_float_real_long", &rows, &cols, &nnz, rlrowptr, rlcolptr, rfvalues, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rlrowptr, rowptr, nnz); ASSERT_VECTOR(rlcolptr, colptr, cols+1); ASSERT_VECTOR(rfvalues, fvalues, nnz);

    CHECK_CALL(csc_hdf5_read_float_sparse_matrix_ulong(h5file, "sparse_float_real_ulong", &rows, &cols, &nnz, rulrowptr, rulcolptr, rfvalues, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rulrowptr, urowptr, nnz); ASSERT_VECTOR(rulcolptr, ucolptr, cols+1); ASSERT_VECTOR(rfvalues, fvalues, nnz);

    CHECK_CALL(csc_hdf5_read_double_complex_sparse_matrix_int(h5file, "sparse_complex_int", &rows, &cols, &nnz, rrowptr, rcolptr, rvaluesc, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rrowptr, rowptr, nnz); ASSERT_VECTOR(rcolptr, colptr, cols+1); ASSERT_VECTOR(rvaluesc, valuesc, nnz);

    CHECK_CALL(csc_hdf5_read_double_complex_sparse_matrix_uint(h5file, "sparse_complex_uint", &rows, &cols, &nnz, rurowptr, rucolptr, rvaluesc, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rurowptr, urowptr, nnz); ASSERT_VECTOR(rucolptr, ucolptr, cols+1); ASSERT_VECTOR(rvaluesc, valuesc, nnz);

    CHECK_CALL(csc_hdf5_read_double_complex_sparse_matrix_long(h5file, "sparse_complex_long", &rows, &cols, &nnz, rlrowptr, rlcolptr, rvaluesc, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rlrowptr, rowptr, nnz); ASSERT_VECTOR(rlcolptr, colptr, cols+1); ASSERT_VECTOR(rvaluesc, valuesc, nnz);

    CHECK_CALL(csc_hdf5_read_double_complex_sparse_matrix_ulong(h5file, "sparse_complex_ulong", &rows, &cols, &nnz, rulrowptr, rulcolptr, rvaluesc, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rulrowptr, urowptr, nnz); ASSERT_VECTOR(rulcolptr, ucolptr, cols+1); ASSERT_VECTOR(rvaluesc, valuesc, nnz);

    CHECK_CALL(csc_hdf5_read_float_complex_sparse_matrix_int(h5file, "sparse_float_complex_int", &rows, &cols, &nnz, rrowptr, rcolptr, rfvaluesc, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rrowptr, rowptr, nnz); ASSERT_VECTOR(rcolptr, colptr, cols+1); ASSERT_VECTOR(rfvaluesc, fvaluesc, nnz);

    CHECK_CALL(csc_hdf5_read_float_complex_sparse_matrix_uint(h5file, "sparse_float_complex_uint", &rows, &cols, &nnz, rurowptr, rucolptr, rfvaluesc, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rurowptr, urowptr, nnz); ASSERT_VECTOR(rucolptr, ucolptr, cols+1); ASSERT_VECTOR(rfvaluesc, fvaluesc, nnz);

    CHECK_CALL(csc_hdf5_read_float_complex_sparse_matrix_long(h5file, "sparse_float_complex_long", &rows, &cols, &nnz, rlrowptr, rlcolptr, rfvaluesc, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rlrowptr, rowptr, nnz); ASSERT_VECTOR(rlcolptr, colptr, cols+1); ASSERT_VECTOR(rfvaluesc, fvaluesc, nnz);

    CHECK_CALL(csc_hdf5_read_float_complex_sparse_matrix_ulong(h5file, "sparse_float_complex_ulong", &rows, &cols, &nnz, rulrowptr, rulcolptr, rfvaluesc, NULL));
    ASSERT(rows == 5); ASSERT(cols == 5); ASSERT(nnz == 10);
    ASSERT_VECTOR(rulrowptr, urowptr, nnz); ASSERT_VECTOR(rulcolptr, ucolptr, cols+1); ASSERT_VECTOR(rfvaluesc, fvaluesc, nnz);

    csc_hdf5_close(h5file);

#undef ASSERT_VECTOR
#undef ASSERT
#undef CHECK_SIZE
    return cnt_err;
}


int main(int argc, char **argv)
{
    int matlab, octave;
    if ( argc != 3) {
        printf("usage: %s matlab octave\n", argv[0]);
        return -1;
    }
    matlab = atoi(argv[1]);
    octave = atoi(argv[2]);

    return read_write_sparse(matlab, octave);
}


