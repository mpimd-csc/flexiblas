/*
 * CSCUTILS - A collection of various software routines uses in CSC projects
 * Copyright (C) 2015, 2016 Martin Koehler
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
#include <math.h>
#include <complex.h>
#include <string.h>
#define H5_NO_DEPRECATED_SYMBOLS
#include <hdf5.h>
#include <hdf5_hl.h>

#include "cscutils/hdf.h"

#include "csc_hdf5_common.h"

typedef struct _complex_t {
    double re;
    double im;
} complex_t;


/* Create a complex double precision type   */
hid_t csc_hdf5_complex_type(hid_t root)
{
    hid_t complex_type = 0;
    hid_t file_id = 0;
    herr_t err;

    file_id = H5Iget_file_id(root);
    if ( file_id < 0 ) {
        csc_error_message("Get file ID failed.\n");
        return -1;
    }

    if (H5Lexists(file_id, "CSC_COMPLEX_TYPE", H5P_DEFAULT) > 0) {
        complex_type = H5Topen2(file_id, "CSC_COMPLEX_TYPE", H5P_DEFAULT);
        H5Idec_ref(file_id);

        if ( complex_type < 0) {
            return -1;
        }
    } else {
        complex_type = H5Tcreate(H5T_COMPOUND, sizeof(complex_t));
        if ( complex_type < 0 ) {
            csc_error_message("Failed to create the HDF5 compound.\n");
            H5Idec_ref(file_id);

            return -1;
        }

        err = H5Tinsert(complex_type, "real", HOFFSET(complex_t, re), H5T_NATIVE_DOUBLE);
        if ( err < 0 ) {
            csc_error_message("Failed to insert the real part of the complex type.\n");
            H5Idec_ref(file_id);

            return -1;
        }

        err = H5Tinsert(complex_type, "imag", HOFFSET(complex_t, im), H5T_NATIVE_DOUBLE);

        if ( err < 0 ) {
            csc_error_message("Failed to insert the imaginary part of the complex type.\n");
            H5Idec_ref(file_id);

            return -1;
        }

        err = H5Tcommit2 (file_id, "CSC_COMPLEX_TYPE", complex_type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
        if ( err < 0 ) {
            csc_error_message("Failed to commit CSC_COMPLEX_TYPE type.\n");
            H5Tclose(complex_type);
            H5Idec_ref(file_id);

            return -1;
        }
        H5Idec_ref(file_id);

    }
    return complex_type;
}


/* Create a enumeration for the content  */
herr_t csc_hdf5_content_type(hid_t root, hid_t *cnt, hid_t *cnt_file)
{
    hid_t content_type = 0;
    hid_t content_type_file = 0 ;
    hid_t file_id = 0;
    herr_t err;
    int i;
    csc_hdf5_content_t val;
    char *names[]  = {
        "CSC_HDF5_VECTOR",
        "CSC_HDF5_MATRIX",
        "CSC_HDF5_SPARSE_COORDINATE",
        "CSC_HDF5_SPARSE_CSC",
        "CSC_HDF5_SPARSE_CSR" };


    content_type = H5Tenum_create(H5T_NATIVE_INT);
    if ( content_type < 0 ) {
        csc_error_message("Failed to create the HDF5 enum.\n");
        return -1;
    }
    content_type_file = H5Tenum_create(H5T_STD_I32LE);
    if ( content_type_file < 0 ) {
        csc_error_message("Failed to create the HDF5 enum.\n");
        H5Tclose(content_type);
        return -1;
    }

    for (i = (int) CSC_HDF5_VECTOR; i <= (int) CSC_HDF5_SPARSE_CSR; i++) {
        /*
         * Insert enumerated value for memtype.
         */
        val = (csc_hdf5_content_t) i;
        err = H5Tenum_insert (content_type, names[i], &val);
        /*
         * Insert enumerated value for filetype.  We must first convert
         * the numerical value val to the base type of the destination.
         */
        err = H5Tconvert (H5T_NATIVE_INT, H5T_STD_I32LE, 1, &val, NULL, H5P_DEFAULT);
        err = H5Tenum_insert (content_type_file, names[i], &val);
    }


    file_id = H5Iget_file_id(root);
    if ( file_id < 0 ) {
        csc_error_message("Get file ID failed.\n");
        H5Tclose(content_type);
        H5Tclose(content_type_file);
        return -1;
    }
    if (H5Lexists(file_id, "CSC_CONTENT_ENUM", H5P_DEFAULT) == 0) {
        err = H5Tcommit2 (file_id, "CSC_CONTENT_ENUM", content_type_file, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
        if ( err < 0 ) {
            csc_error_message("Failed to commit CSC_CONTENT_ENUM type.\n");
            H5Tclose(content_type);
            H5Tclose(content_type_file);
            H5Idec_ref(file_id);

            return -1;
        }
    }

    H5Idec_ref(file_id);
    *cnt = content_type;
    *cnt_file = content_type_file;
    return 0;
}

/* Create a enumeration for the field  */
herr_t csc_hdf5_field_type(hid_t root, hid_t *ft, hid_t *ft_file)
{
    hid_t field_type = 0;
    hid_t field_type_file = 0;
    hid_t file_id = 0;
    herr_t err;
    int i;
    csc_hdf5_field_t val;
    char *names[]  = {
        "CSC_HDF5_REAL",
        "CSC_HDF5_COMPLEX",
        "CSC_HDF5_INTEGER8",
        "CSC_HDF5_INTEGER16",
        "CSC_HDF5_INTEGER32",
        "CSC_HDF5_INTEGER64",
        "CSC_HDF5_REAL_SINGLE",
        "CSC_HDF5_COMPLEX_SINGLE" };

    field_type = H5Tenum_create(H5T_NATIVE_INT);
    if ( field_type < 0 ) {
        csc_error_message("Failed to create the HDF5 enum.\n");
        return -1;
    }

    field_type_file = H5Tenum_create(H5T_STD_I32LE);
    if ( field_type_file < 0 ) {
        H5Tclose(field_type);
        csc_error_message("Failed to create the HDF5 enum.\n");
        return -1;
    }

    for (i = (int) CSC_HDF5_REAL; i <= (int) CSC_HDF5_COMPLEX_SINGLE; i++) {
        /*
         * Insert enumerated value for memtype.
         */
        val = (csc_hdf5_field_t) i;
        err = H5Tenum_insert (field_type, names[i], &val);
        /*
         * Insert enumerated value for filetype.  We must first convert
         * the numerical value val to the base type of the destination.
         */
        err = H5Tconvert (H5T_NATIVE_INT, H5T_STD_I32LE, 1, &val, NULL, H5P_DEFAULT);
        err = H5Tenum_insert (field_type_file, names[i], &val);
    }


    file_id = H5Iget_file_id(root);
    if ( file_id < 0 ) {
        csc_error_message("Get file ID failed.\n");
        return -1;
    }

    if (H5Lexists(file_id, "CSC_FIELD_ENUM", H5P_DEFAULT) == 0) {
        err = H5Tcommit2 (file_id, "CSC_FIELD_ENUM", field_type_file, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
        if ( err < 0 ) {
            csc_error_message("Failed to commit CSC_FIELD_ENUM type.\n");
            H5Tclose(field_type);
            H5Tclose(field_type_file);
            H5Idec_ref(file_id);

            return -1;
        }

    }

    H5Idec_ref(file_id);
    *ft = field_type;
    *ft_file = field_type_file;
    return 0;
}

