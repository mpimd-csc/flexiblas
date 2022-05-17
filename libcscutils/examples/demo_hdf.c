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
#include <unistd.h>
#include "cscutils/hdf.h"


void writevector()
{
    hid_t h5file;
    hid_t vgroup;

    double v1[8] = {0,1,2,3,4,5,6,7};
    double complex v2[8] = {0,1,2,3,4,5,6,7+I};
    int v3[5] = {1,11,111,1111,11111};

    // h5file = H5Fopen("test.h5", H5F_ACC_RDWR, H5P_DEFAULT);

    h5file = csc_hdf5_open("test.h5", "rw" );   // H5Fcreate("test.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if ( h5file < 0 ) {
        printf("Failed to open file \n");
    }
    vgroup = csc_hdf5_group_open(h5file,"vectortest");
    // h5file = H5Fopen("test.h5", H5F_ACC_RDWR, H5P_DEFAULT);
    csc_hdf5_vector_write(CSC_HDF5_REAL, vgroup, "real_vector", 8, v1);
    csc_hdf5_vector_write(CSC_HDF5_COMPLEX, vgroup, "complex_vector", 8, v2);
    csc_hdf5_vector_write(CSC_HDF5_INTEGER32, vgroup, "integer_vector", 5, v3);

    csc_hdf5_group_close(vgroup);
    csc_hdf5_close(h5file);

}

void readsparse()
{
    hid_t h5file;
    hid_t vgroup;
    size_t rows, cols, nnz;
    size_t i;


    double values[10];
    int rowptr[10];
    int colptr[10];
    csc_hdf5_content_t content;
    csc_hdf5_field_t field;



    h5file = csc_hdf5_open("test.h5", "rw");
    vgroup = csc_hdf5_group_open(h5file,"sparsetest");
    if ( vgroup < 0 ) {
        printf("Hmpf\n");
    }
    // h5file = H5Fopen("test.h5", H5F_ACC_RDWR, H5P_DEFAULT);
    csc_hdf5_sparse_read(&content, &field, CSC_HDF5_INTEGER32, vgroup, "sparse_real_int", &rows, &cols, &nnz, rowptr, colptr, values);
    for (i = 0; i < nnz; i++) {
        printf("[ %3d, %3d ] = %lg\n", rowptr[i], colptr[i],values[i]);
    }
    csc_hdf5_group_close(vgroup);
    csc_hdf5_close(h5file);

}


void readvector()
{
    hid_t h5file;
    hid_t vgroup;
    csc_hdf5_options_t opts;
    double v1[10];
    double complex v2[10];
    long v3[5];
    int l1, l2, i, l3;

    h5file = csc_hdf5_open("test.h5", "rw");
    vgroup = csc_hdf5_group_open(h5file, "vectortest");


    printf("Length of real_vector: %d\n", l1 = (int) csc_hdf5_vector_len(vgroup, "real_vector"));
    printf("Length of complex_vector: %d\n", l2 = (int) csc_hdf5_vector_len(vgroup, "complex_vector"));
    printf("Length of integer_vector: %d\n", l3 = (int) csc_hdf5_vector_len(vgroup, "integer_vector"));

    printf("is_real(v1)   : %d\n", csc_hdf5_is_real(vgroup, "real_vector"));
    printf("is_complex(v1): %d\n", csc_hdf5_is_complex(vgroup, "real_vector"));
    printf("is_real(v2)   : %d\n", csc_hdf5_is_real(vgroup, "complex_vector"));
    printf("is_complex(v2): %d\n", csc_hdf5_is_complex(vgroup, "complex_vector"));

    csc_hdf5_read_double_vector(vgroup, "real_vector", l1, v1, &opts);
    csc_hdf5_read_double_complex_vector(vgroup, "complex_vector", l2, v2, &opts);

    for (i = 0; i <l1 ; i++) {
        printf("v1 [%2d] = %g\n", i, v1[i]);
    }
    for (i = 0; i <l2 ; i++) {
        printf("v2 [%2d] = %g %g\n", i, creal(v2[i]), cimag(v2[i]));
    }

    csc_hdf5_read_long_vector(vgroup, "integer_vector", l3, v3, &opts);

    for (i = 0; i <l3 ; i++) {
        printf("v3 [%2d] = %ld\n", i, v3[i]);
    }

    csc_hdf5_group_close(vgroup);
    csc_hdf5_close(h5file);
}

void writematrix()
{
    hid_t h5file;
    hid_t vgroup;

    double v1[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
    double complex v2[16] = {0-I,1,2,3,4,5,6,7+I, 8, 9, 10, 11, 12, 13, 14, 15 };

    // h5file = H5Fcreate("test.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    h5file = csc_hdf5_open2("test.h5", "rw", csc_hdf5_matlab_header(NULL));
    vgroup = csc_hdf5_group_open(h5file, "matrixtest");

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
    if ( csc_hdf5_write_matrix(h5file, "A", 4, 2, v1, 8, NULL)) {
        printf("Write failed.\n");
    }
    /* if ( csc_hdf5_write_matrix(h5file, "writetest/real/A", 4, 2, v1, 8, NULL)) { */
    /*     printf("Write failed.\n"); */
    /* } */
    /* if ( csc_hdf5_write_matrix(h5file, "writetest/complex/A", 4, 2, v2, 8, NULL)) { */
    /*     printf("Write failed.\n"); */
    /* } */
    if ( csc_hdf5_write_matrix(h5file, "B", 4, 2, v2, 8, NULL)) {
        printf("Write failed.\n");
    }
#else
    if ( csc_hdf5_write_double_matrix(h5file, "A", 4, 2, v1, 8, NULL)) {
        printf("Write failed.\n");
    }
    if ( csc_hdf5_write_double_matrix(h5file, "writetest/real/A", 4, 2, v1, 8, NULL)) {
        printf("Write failed.\n");
    }
    if ( csc_hdf5_write_double_complex_matrix(h5file, "writetest/complex/A", 4, 2, v2, 8,NULL)) {
        printf("Write failed.\n");
    }
    if ( csc_hdf5_write_double_complex_matrix(h5file, "B", 4, 2, v2, 8,NULL)) {
        printf("Write failed.\n");
    }


#endif

    csc_hdf5_write_string(h5file, "string", "test string");
    csc_hdf5_write_int(h5file, "integer", 1);
    csc_hdf5_write_uint(h5file, "unsigned integer", 123);
    csc_hdf5_write_long(h5file, "long", 4711);
    csc_hdf5_write_ulong(h5file, "unsigned long", 815);
    csc_hdf5_write_float(h5file, "float", 3.14);
    csc_hdf5_write_double(h5file, "double", 2.7);

    {
        int r;
        csc_hdf5_read_int(h5file, "integer", &r);
        printf("r = %d\n", r);
    }




    csc_hdf5_matrix_write_real(vgroup, "real_matrix", 4,2,8, v1);
    csc_hdf5_matrix_write_complex(vgroup, "complex_matrix", 4,2,8, v2);

    /* printf("Is matrix: %d\n", csc_hdf5_is_matrix(vgroup, "real_matrix")); */
    /* printf("Is matrix: %d\n", csc_hdf5_is_matrix(vgroup, "complex_matrix")); */


    /* csc_hdf5_group_close(vgroup); */
    csc_hdf5_close(h5file);

    csc_hdf5_options_t opts = CSC_HDF5_OPTIONS_INIT;
    /* opts.octave_style = 1; */

    h5file = csc_hdf5_open("test_octave.h5", "rw");
    csc_hdf5_write_string(h5file, "string", "test string");
    csc_hdf5_write_int(h5file, "integer", 1);
    csc_hdf5_write_uint(h5file, "unsigned integer", 123);
    csc_hdf5_write_long(h5file, "long", 4711);
    csc_hdf5_write_ulong(h5file, "unsigned long", 815);
    csc_hdf5_write_float(h5file, "float", 3.14);
    csc_hdf5_write_double(h5file, "double", 2.7);



    if ( csc_hdf5_write_double_matrix(h5file, "A", 4, 2, v1, 8, &opts)) {
        printf("Write failed.\n");
    }

    csc_hdf5_close(h5file);

}

void readmatrix()
{
    hid_t h5file;
    hid_t vgroup;
    size_t rows, cols;

    double v1[16];
    double complex v2[16];

    h5file = csc_hdf5_open("test.h5", "rw");
    vgroup = csc_hdf5_group_open(h5file, "matrixtest");


    printf("Is matrix: %d\n", csc_hdf5_is_matrix(vgroup, "real_matrix"));
    printf("Is matrix: %d\n", csc_hdf5_is_matrix(vgroup, "complex_matrix"));

    csc_hdf5_matrix_size(vgroup, "real_matrix", &rows, &cols);
    printf("%s is of size (%d, %d)\n", "real_matrix", (int) rows, (int) cols);

    /* Read rows == ld  */
    csc_hdf5_matrix_read_real(vgroup, "real_matrix", 4,2,4, v1);
    size_t i, j;
    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            printf("%4.2lg \t", v1[i+j*4]);
        }
        printf("\n");
    }
    /* Read rows != LD  */
    csc_hdf5_matrix_read_real(vgroup, "real_matrix", 4,2,8, v1);
    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            printf("%4.2lg \t", v1[i+j*8]);
        }
        printf("\n");
    }

    // csc_hdf5_matrix_write_complex(vgroup, "complex_matrix", 8,2,8, v2);

    csc_hdf5_matrix_size(vgroup, "complex_matrix", &rows, &cols);
    printf("%s is of size (%d, %d)\n", "complex_matrix", (int) rows, (int) cols);

    /* Read rows == ld  */
    csc_hdf5_matrix_read_complex(vgroup, "complex_matrix", 4,2,4, v2);
    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            printf("%4.2lg + %4.2lg \t", creal(v2[i+j*4]), cimag(v2[i+j*4]));
        }
        printf("\n");
    }
    /* Read rows != LD  */
    csc_hdf5_matrix_read_complex(vgroup, "complex_matrix", 4,2,8, v2);
    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            printf("%4.2lg + %4.2lg \t", creal(v2[i+j*4]), cimag(v2[i+j*4]));
        }
        printf("\n");
    }

    csc_hdf5_group_close(vgroup);
    csc_hdf5_close(h5file);
}


int main(int argc, char **argv)
{
    (void) argc;
    (void) argv;

    csc_hdf5_set_compression(CSC_HDF5_COMPRESS_DEFLATE);
    writevector();
    readvector();
    unlink("test_sparse.h5");
    writematrix();
    readmatrix();
    return 0;
}
