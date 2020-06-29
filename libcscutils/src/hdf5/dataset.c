/*
 * LIBCSCUTILS: HDF5 Interface
 * Copyright (C) Martin Koehler, 2020
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

#include <hdf5.h>

#include "cscutils/hdf.h"
#include "csc_hdf5_common.h"

int csc_hdf5_dataset_exist(hid_t root, const char *dset_name)
{
    int ret = 0;
    char *saveptr = NULL;
    char *tok = NULL;
    char *dset_name_dup = NULL;
    char *tmp = NULL;
    int depth = 0;
    int k;
    hid_t gid = 0;
    herr_t err = 0;
    H5O_info_t info;
    size_t len;

    /* Split the dset_name into group and data set part.   */
    len = strlen(dset_name);
    dset_name_dup = strdup(dset_name);
    tmp = strdup(dset_name);

    tok = strtok_r(dset_name_dup, "/", &saveptr);
    depth = 0;
    while (tok) {
        tok = strtok_r(NULL, "/", &saveptr);
        depth ++;
    }

    /* Check the path.  */
    strncpy(dset_name_dup, dset_name, len+1);
    saveptr = NULL;
    tok = strtok_r(dset_name_dup, "/", &saveptr);
    tmp[0] = '\0';
    for (k = 0; k < depth-1; k++) {
        if ( k > 0 ) strcat(tmp,"/");
        strcat(tmp, tok);
        if ( ! csc_hdf5_group_exist(root, tmp)) {
            ret = 0;
            goto end;
        }
        tok = strtok_r(NULL, "/", &saveptr);
    }

    if ( depth > 1) {
        gid = H5Gopen2(root, tmp, H5P_DEFAULT);
        if ( gid < 0 ) {
            ret = 0;
            goto end;
        }
    } else {
        gid = root;
    }

    /* Remove existing */
    if ( H5Lexists(gid, tok, H5P_DEFAULT) > 0 ) {
        err = H5Oget_info_by_name(gid, tok, &info, H5P_DEFAULT);
        if ( err < 0 ) {
            ret = 0;
            goto end;
        }

        if ( info.type == H5O_TYPE_DATASET ) {
            ret = 1;
        }
    } else {
        ret = 0;
    }


end:
    if ( dset_name_dup ) free(dset_name_dup);
    if ( tmp ) free(tmp);
    if ( gid != root && gid > 0 ) H5Gclose(gid);
    return ret;
}

int csc_hdf5_write_string(hid_t root, const char * dset_name, const char *str)
{
    int ret = 0;
    size_t len = 0;
    herr_t err;
    hid_t space_id = 0;
    hid_t dtype = 0;
    hid_t dataset = 0;


    if ( csc_hdf5_group_path_create(root, dset_name, 0)) {
        csc_error_message("Failed to create path to %s\n", dset_name);
        ret = -1;
        goto end;
    }


    /* Remove existing */
    if ( H5Lexists(root, dset_name, H5P_DEFAULT) > 0 ) {
        if ( H5Ldelete(root, dset_name, H5P_DEFAULT) < 0 ) {
            csc_error_message("Failed to remove previous entry %s\n", dset_name);
            ret = -1;
            goto end;
        }
    }

    /*   */
    space_id = H5Screate(H5S_SCALAR);
    if ( space_id < 0 ) {
        csc_error_message("Failed to create dataspace for %s\n", dset_name);
        ret = -1;
        goto end;
    }

    /*  String type  */
    len = strlen(str) + 1;
    dtype = H5Tcopy(H5T_C_S1);
    if ( dtype < 0 ) {
        ret = -1 ;
        goto end;
    }
    if ( H5Tset_size( dtype, (size_t) len ) < 0 ) {
        ret = -1;
        goto end;
    }
    if ( H5Tset_strpad( dtype, H5T_STR_NULLTERM ) < 0 ) {
        ret = -1;
        goto end;
    }

    dataset = H5Dcreate2(root, dset_name, dtype, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if ( dataset < 0 ) {
        csc_error_message("Failed to create the dataset for %s\n", dset_name);
        ret = -1;
    }

    err = H5Dwrite(dataset, dtype, space_id, space_id, H5P_DEFAULT, str);
    if ( err < 0 ) {
        csc_error_message("Failed to write %s\n", dset_name);
        ret = -1;
        goto end;
    }

    ret = 0;

end:
    if ( space_id > 0 ) H5Sclose(space_id);
    if ( dtype > 0 ) H5Tclose(dtype);
    if ( dataset > 0 ) H5Dclose(dataset);
    return ret;

}

int csc_hdf5_read_string(hid_t root, const char * dset_name, char **str, size_t *len)
{
    int ret = 0;
    hid_t dtype = 0;
    hid_t dataset = 0;
    hsize_t dlen = 0;

    dataset = H5Dopen2(root, dset_name, H5P_DEFAULT);
    if ( dataset < 0 ) {
        ret = -1;
        goto end;
    }


    dlen = H5Dget_storage_size(dataset) + 1;
    if ( *str == NULL) * len = 0;

    if ( *len == 0 ) {
        *len = dlen;
        *str = calloc(dlen, sizeof(char));
    } else if ( *len < dlen ) {
        *len = dlen;
        *str = realloc(*str, sizeof(char) * dlen);
    } else {
        *len = dlen;
    }

    dtype = H5Dget_type(dataset);
    if ( dtype < 0 ) {
        ret = -1;
        goto end;
    }

    if ( H5Dread(dataset, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, *str) < 0 ) {
        csc_error_message("Read string from %s failed.", dset_name);
        ret = -1;
        goto end;
    }



end:
    if ( dtype > 0 ) H5Tclose(dtype);
    if ( dataset > 0 ) H5Dclose(dataset);
    return ret;

}

int csc_hdf5_write_scalar(hid_t root, const char * dset_name, hid_t dtype, void *val)
{
    int ret = 0;
    herr_t err;
    hid_t space_id = 0;
    hid_t dataset = 0;


    if ( csc_hdf5_group_path_create(root, dset_name, 0)) {
        csc_error_message("Failed to create path to %s\n", dset_name);
        ret = -1;
        goto end;
    }


    /* Remove existing */
    if ( H5Lexists(root, dset_name, H5P_DEFAULT) > 0 ) {
        if ( H5Ldelete(root, dset_name, H5P_DEFAULT) < 0 ) {
            csc_error_message("Failed to remove previous entry %s\n", dset_name);
            ret = -1;
            goto end;
        }
    }

    /*   */
    space_id = H5Screate(H5S_SCALAR);
    if ( space_id < 0 ) {
        csc_error_message("Failed to create dataspace for %s\n", dset_name);
        ret = -1;
        goto end;
    }


    dataset = H5Dcreate2(root, dset_name, dtype, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if ( dataset < 0 ) {
        csc_error_message("Failed to create the dataset for %s\n", dset_name);
        ret = -1;
    }

    err = H5Dwrite(dataset, dtype, space_id, space_id, H5P_DEFAULT, val);
    if ( err < 0 ) {
        csc_error_message("Failed to write %s\n", dset_name);
        ret = -1;
        goto end;
    }

    ret = 0;

end:
    if ( space_id > 0 ) H5Sclose(space_id);
    if ( dataset > 0 ) H5Dclose(dataset);
    return ret;
}

int csc_hdf5_write_int(hid_t root, const char * dset_name, int val)
{
    return csc_hdf5_write_scalar(root, dset_name, H5T_NATIVE_INT, (void *) &val);
}

int csc_hdf5_write_uint(hid_t root, const char * dset_name, unsigned int val)
{
    return csc_hdf5_write_scalar(root, dset_name, H5T_NATIVE_UINT, (void *) &val);
}

int csc_hdf5_write_long(hid_t root, const char * dset_name, long val)
{
    return csc_hdf5_write_scalar(root, dset_name, H5T_NATIVE_LONG, (void *) &val);
}

int csc_hdf5_write_ulong(hid_t root, const char * dset_name, unsigned long val)
{
    return csc_hdf5_write_scalar(root, dset_name, H5T_NATIVE_ULONG, (void *) &val);
}

int csc_hdf5_write_float(hid_t root, const char * dset_name, float val)
{
    return csc_hdf5_write_scalar(root, dset_name, H5T_NATIVE_FLOAT, (void *) &val);
}

int csc_hdf5_write_double(hid_t root, const char * dset_name, double val)
{
    return csc_hdf5_write_scalar(root, dset_name, H5T_NATIVE_DOUBLE, (void *) &val);
}

int csc_hdf5_read_scalar(hid_t root, const char * dset_name, hid_t dtype, void *val)
{
    int ret = 0;
    hid_t dataset = 0;

    dataset = H5Dopen2(root, dset_name, H5P_DEFAULT);
    if ( dataset < 0 ) {
        ret = -1;
        goto end;
    }


    if ( H5Dread(dataset, dtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, val) < 0 ) {
        ret = -1;
        goto end;
    }



end:
    if ( dataset > 0 ) H5Dclose(dataset);
    return ret;
}


int csc_hdf5_read_int(hid_t root, const char * dset_name, int *val)
{
    return csc_hdf5_read_scalar(root, dset_name, H5T_NATIVE_INT, val);
}

int csc_hdf5_read_uint(hid_t root, const char * dset_name, unsigned int *val)
{
    return csc_hdf5_read_scalar(root, dset_name, H5T_NATIVE_UINT, val);
}

int csc_hdf5_read_long(hid_t root, const char * dset_name, long *val)
{
    return csc_hdf5_read_scalar(root, dset_name, H5T_NATIVE_LONG, val);
}

int csc_hdf5_read_ulong(hid_t root, const char * dset_name, unsigned long *val)
{
    return csc_hdf5_read_scalar(root, dset_name, H5T_NATIVE_ULONG, val);
}

int csc_hdf5_read_float(hid_t root, const char * dset_name, float *val)
{
    return csc_hdf5_read_scalar(root, dset_name, H5T_NATIVE_FLOAT, val);
}

int csc_hdf5_read_double(hid_t root, const char * dset_name, double *val)
{
    return csc_hdf5_read_scalar(root, dset_name, H5T_NATIVE_DOUBLE, val);
}



