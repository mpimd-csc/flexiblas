/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2015
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
#include <string.h>

// #define H5_NO_DEPRECATED_SYMBOLS
#include "cscutils/hdf.h"



int main(int argc, char **argv)
{
    hid_t h5file;
    char *dset;
    int ret= 0;

    if ( argc != 3 ) {
        printf("%s hdf5.h5 dset\n", argv[0]);
        exit(1);
    }

    dset = strdup(argv[2]);

    h5file = csc_hdf5_open(argv[1], "r");

#if 0

    size_t rows, cols;
    ret = csc_hdf5_matrix_size(h5file, dset, &rows, &cols);
    printf("ret = %d\n", ret);
    printf("rows = %lu\n", rows);
    printf("cols = %lu\n", cols);

    csc_hdf5_field_t field = 0;
    csc_hdf5_matrix_get_datatype(h5file, dset, &field);
    printf("field %d\n", field );

    if (csc_hdf5_attribute_exist(h5file, dset, "MATLAB_class")) {
        char *tmp;
        size_t len = 0;
        printf("MATLAB_class exists\n");

        csc_hdf5_attribute_read_string(h5file, dset, "MATLAB_class", &tmp, &len);

        printf("attr: %s\n", tmp);
        free(tmp);
    }

    if ( field == CSC_HDF5_DOUBLE  || field == CSC_HDF5_FLOAT) {
        double * data = malloc(sizeof(double) * rows * cols);
        if ( csc_hdf5_read_double_matrix(h5file, dset, rows, cols, data, rows, NULL )) {
            fprintf(stderr, "Failed to read %s from %s\n", dset, argv[1]);
            exit(1);
        }
        size_t i,j;
        for (j = 0; j < rows; j++) {
            for (i = 0; i < cols; i++) {
                printf("%5lg ", data[i*rows+j]);
            }
            printf("\n");
        }
        free(data);
    }

    if ( field == CSC_HDF5_DOUBLE_COMPLEX  || field == CSC_HDF5_FLOAT_COMPLEX) {
        double complex * data = malloc(sizeof(double complex) * rows * cols);
        if ( csc_hdf5_read_double_complex_matrix(h5file, dset, rows, cols, data, rows, NULL )) {
            fprintf(stderr, "Failed to read %s from %s\n", dset, argv[1]);
            exit(1);
        }
        size_t i,j;
        for (j = 0; j < rows; j++) {
            for (i = 0; i < cols; i++) {
                printf("%5lg+%5lgi ", creal(data[i*rows+j]), cimag(data[i*rows+j]));
            }
            printf("\n");
        }
        free(data);
    }

#else
    size_t rows, cols, nnz;
    if ( csc_hdf5_sparse_size(h5file, dset, &rows, &cols, &nnz)) {
        fprintf(stderr, "Failed to read size properties from %s.\n",dset);
    }
    printf("rows = %lu\n", rows);
    printf("cols = %lu\n", cols);
    printf("nnz  = %lu\n", nnz);

    csc_hdf5_field_t field = 0;
    if (csc_hdf5_sparse_get_datatype(h5file, dset, &field)){
        fprintf(stderr, "Failed to read field from %s\n", dset);
    }
    if ( field == CSC_HDF5_DOUBLE ){
        printf("Double Precision / Real Matrix\n");
    } else if ( field == CSC_HDF5_DOUBLE_COMPLEX ) {
        printf("Complex Matrix.\n");
    } else {
        printf("Unsupported for Sparse.\n");
    }
#endif
    csc_hdf5_close(h5file);

    free(dset);
    return 0;
}
