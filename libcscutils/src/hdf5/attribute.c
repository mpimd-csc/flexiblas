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

/*
 *
 * Write Functions.
 *
 */
static int csc_hdf5_attribute_write_mem(hid_t root, const char* dset_name, const char * aname, hid_t dtype, void * aval)
{
    hid_t obj_id = 0;
    hid_t attr_id = 0;
    hid_t attr_space = 0;
    int ret = 0;
    herr_t err;

    if (! dset_name ) {
        ret = -1;
        goto end;
    }

    if ( !aname ) {
        ret = -1;
        goto end;
    }

    obj_id = H5Oopen(root, dset_name, H5P_DEFAULT);
    if ( obj_id < 0 ) {
        csc_error_message("Failed to open %s to write attribute %s\n", dset_name, aname);
        ret = -1;
        goto end;
    }

    attr_space = H5Screate(H5S_SCALAR);
    if ( attr_space < 0 ) {
        csc_error_message("Failed to create dataspace for the attribute %s\n", aname);
        ret = -1;
        goto end;
    }

    if ( csc_hdf5_attribute_exist(root, dset_name, aname) ) {
        if (H5Adelete(obj_id, aname) < 0 ) {
            ret = -1;
            goto end;
        }
    }

    attr_id = H5Acreate2(obj_id, aname, dtype, attr_space, H5P_DEFAULT, H5P_DEFAULT);
    if ( attr_id < 0 ) {
        csc_error_message("Failed to create attribute %s\n", aname);
        ret = -1;
        goto end;
    }

    err = H5Awrite(attr_id, dtype, aval);
    if ( err < 0 ) {
        ret = -1;
        goto end;
    }
    ret = 0;

end:
    if ( attr_space > 0 ) H5Sclose(attr_space);
    if ( attr_id > 0 ) H5Aclose(attr_id);
    if ( obj_id > 0 ) H5Oclose(obj_id);
    if ( ret ) {
        csc_error_message("Failed to write attribute %s to %s\n", aname, dset_name);
    }

    return ret;

}

int csc_hdf5_attribute_write_char(hid_t root, const char* dset_name, const char * aname, char aval)
{
    return csc_hdf5_attribute_write_mem(root, dset_name, aname, H5T_NATIVE_CHAR, (void *) &aval);
}

int csc_hdf5_attribute_write_uchar(hid_t root, const char* dset_name, const char * aname, unsigned char aval)
{
    return csc_hdf5_attribute_write_mem(root, dset_name, aname, H5T_NATIVE_UCHAR, (void *) &aval);
}


int csc_hdf5_attribute_write_int(hid_t root, const char* dset_name, const char * aname, int aval)
{
    return csc_hdf5_attribute_write_mem(root, dset_name, aname, H5T_NATIVE_INT, (void *) &aval);
}

int csc_hdf5_attribute_write_uint(hid_t root, const char* dset_name, const char * aname, unsigned int aval)
{
    return csc_hdf5_attribute_write_mem(root, dset_name, aname, H5T_NATIVE_UINT, (void *) &aval);
}

int csc_hdf5_attribute_write_long(hid_t root, const char* dset_name, const char * aname, long aval)
{
    return csc_hdf5_attribute_write_mem(root, dset_name, aname, H5T_NATIVE_LONG, (void *) &aval);
}

int csc_hdf5_attribute_write_ulong(hid_t root, const char* dset_name, const char * aname, unsigned long aval)
{
    return csc_hdf5_attribute_write_mem(root, dset_name, aname, H5T_NATIVE_ULONG, (void *) &aval);
}

int csc_hdf5_attribute_write_float(hid_t root, const char* dset_name, const char * aname, float aval)
{
    return csc_hdf5_attribute_write_mem(root, dset_name, aname, H5T_NATIVE_FLOAT, (void *) &aval);
}

int csc_hdf5_attribute_write_double(hid_t root, const char* dset_name, const char * aname, double aval)
{
    return csc_hdf5_attribute_write_mem(root, dset_name, aname, H5T_NATIVE_DOUBLE, (void *) &aval);
}

int csc_hdf5_attribute_write_string(hid_t root, const char* dset_name, const char * aname, char * str)
{
    hid_t obj_id = 0;
    hid_t attr_id = 0;
    hid_t attr_space = 0;
    hid_t attr_type = 0;
    size_t len = 0;
    int ret = 0;
    herr_t err;

    if (! dset_name ) {
        ret = -1;
        goto end;
    }

    if ( !aname ) {
        ret = -1;
        goto end;
    }

    obj_id = H5Oopen(root, dset_name, H5P_DEFAULT);
    if ( obj_id < 0 ) {
        csc_error_message("Failed to open %s to write attribute %s\n", dset_name, aname);
        ret = -1;
        goto end;
    }

    attr_space = H5Screate(H5S_SCALAR);
    if ( attr_space < 0 ) {
        csc_error_message("Failed to create dataspace for the attribute %s\n", aname);
        ret = -1;
        goto end;
    }

    if ( csc_hdf5_attribute_exist(root, dset_name, aname) ) {
        if (H5Adelete(obj_id, aname) < 0 ) {
            ret = -1;
            goto end;
        }
    }

    /*  String type  */
    len = strlen(str) + 1;
    attr_type = H5Tcopy(H5T_C_S1);
    if ( attr_type < 0 ) {
        ret = -1 ;
        goto end;
    }
    if ( H5Tset_size( attr_type, (size_t) len ) < 0 ) {
        ret = -1;
        goto end;
    }
    if ( H5Tset_strpad( attr_type, H5T_STR_NULLTERM ) < 0 ) {
        ret = -1;
        goto end;
    }


    attr_id = H5Acreate2(obj_id, aname, attr_type, attr_space, H5P_DEFAULT, H5P_DEFAULT);
    if ( attr_id < 0 ) {
        csc_error_message("Failed to create attribute %s\n", aname);
        ret = -1;
        goto end;
    }

    err = H5Awrite(attr_id, attr_type, str);
    if ( err < 0 ) {
        ret = -1;
        goto end;
    }
    ret = 0;

end:
    if ( attr_type > 0 )  H5Tclose(attr_type);
    if ( attr_space > 0 ) H5Sclose(attr_space);
    if ( attr_id > 0 ) H5Aclose(attr_id);
    if ( obj_id > 0 ) H5Oclose(obj_id);
    /* if ( ret ) { */
    /*     csc_error_message("Failed to write attribute %s to %s\n", aname, dset_name); */
    /* } */
    return ret;
}



/*
 *
 * Read Functions.
 *
 */

static int csc_hdf5_attribute_read_mem(hid_t root, const char * dset_name,  const char * aname, hid_t dtype, void * retval)
{
    hid_t obj_id = 0;
    hid_t attr_id = 0;
    int ret = -1;

    if (!dset_name) {
        ret = -1;
        goto end;
    }
    if (!aname) {
        ret = -1;
        goto end;
    }

    obj_id = H5Oopen(root, dset_name, H5P_DEFAULT);
    if ( obj_id < 0 ) {
        ret = -1;
        goto end;
    }

    if ( H5Aexists(obj_id, aname) <= 0 ) {
        ret = -1;
        goto end;
    }

    attr_id = H5Aopen(obj_id, aname, H5P_DEFAULT);
    if ( attr_id < 0 ) {
        ret = -1;
        goto end;
    }

    if (H5Aread(attr_id, dtype, retval) < 0 ) {
        ret = -1;
        goto end;
    }
    ret = 0;

end:
    if ( obj_id > 0 ) H5Oclose(obj_id);
    if ( attr_id > 0 ) H5Aclose(attr_id);
    /* if ( ret ) { */
    /*     csc_error_message("Failed to read attribute %s from %s\n", aname, dset_name); */
    /* } */
    return ret;

}

int csc_hdf5_attribute_read_char(hid_t root, const char * dset_name,  const char * aname, char * retval)
{
    return csc_hdf5_attribute_read_mem(root, dset_name, aname, H5T_NATIVE_CHAR, retval);
}

int csc_hdf5_attribute_read_uchar(hid_t root, const char * dset_name,  const char * aname, unsigned char * retval)
{
    return csc_hdf5_attribute_read_mem(root, dset_name, aname, H5T_NATIVE_UCHAR, retval);
}


int csc_hdf5_attribute_read_int(hid_t root, const char * dset_name,  const char * aname, int * retval)
{
    return csc_hdf5_attribute_read_mem(root, dset_name, aname, H5T_NATIVE_INT, retval);
}

int csc_hdf5_attribute_read_uint(hid_t root, const char * dset_name,  const char * aname, unsigned int * retval)
{
    return csc_hdf5_attribute_read_mem(root, dset_name, aname, H5T_NATIVE_UINT, retval);
}

int csc_hdf5_attribute_read_long(hid_t root, const char * dset_name,  const char * aname, long * retval)
{
    return csc_hdf5_attribute_read_mem(root, dset_name, aname, H5T_NATIVE_LONG, retval);
}

int csc_hdf5_attribute_read_ulong(hid_t root, const char * dset_name,  const char * aname, unsigned long * retval)
{
    return csc_hdf5_attribute_read_mem(root, dset_name, aname, H5T_NATIVE_ULONG, retval);
}

int csc_hdf5_attribute_read_float(hid_t root, const char * dset_name,  const char * aname, float * retval)
{
    return csc_hdf5_attribute_read_mem(root, dset_name, aname, H5T_NATIVE_FLOAT, retval);
}

int csc_hdf5_attribute_read_double(hid_t root, const char * dset_name,  const char * aname, double * retval)
{
    return csc_hdf5_attribute_read_mem(root, dset_name, aname, H5T_NATIVE_DOUBLE, retval);
}

int csc_hdf5_attribute_read_string(hid_t root, const char* dset_name, const char * aname, char ** str, size_t *len)
{
    hid_t obj_id = 0;
    hid_t attr_id = 0;
    hid_t attr_type = 0;
    size_t attr_len = 0;
    int ret = -1;

    if (!dset_name) {
        ret = -1;
        goto end;
    }
    if (!aname) {
        ret = -1;
        goto end;
    }

    obj_id = H5Oopen(root, dset_name, H5P_DEFAULT);
    if ( obj_id < 0 ) {
        ret = -1;
        goto end;
    }

    if ( H5Aexists(obj_id, aname) <= 0 ) {
        ret = -1;
        goto end;
    }

    attr_id = H5Aopen(obj_id, aname, H5P_DEFAULT);
    if ( attr_id < 0 ) {
        ret = -1;
        goto end;
    }

    attr_len = H5Aget_storage_size(attr_id) + 1;
    if ( *len == 0 ) {
        *len = attr_len;
        *str = calloc(attr_len, sizeof(char));
    } else if ( *len < attr_len ) {
        *len = attr_len;
        *str = realloc(*str, sizeof(char) * attr_len);
    } else {
        *len = attr_len;
    }

    attr_type = H5Aget_type(attr_id);
    if ( attr_type < 0 ) {
        ret = -1;
        goto end;
    }

    if ( H5Aread(attr_id, attr_type, *str) < 0 ) {
        ret = -1;
        goto end;
    }



end:
    if ( obj_id > 0 ) H5Oclose(obj_id);
    if ( attr_id > 0 ) H5Aclose(attr_id);
    if ( attr_type > 0 ) H5Tclose(attr_type);
    return ret;

}


#if 0
herr_t attr_info( hid_t location_id/*in*/, const char *attr_name/*in*/, const H5A_info_t *ainfo/*in*/, void *op_data/*in,out*/)
{
    printf("attr_name: %s\n", attr_name);
}

int csc_hdf5_attribute_show(hid_t root, const char * dset_name)
{
    hid_t dataset;
    herr_t err;
    hsize_t n = 0;

    dataset = H5Oopen(root, dset_name, H5P_DEFAULT);
    if ( dataset < 0) {
        csc_error_message("Failed to open object: %s", dset_name);
        return 0;
    }

    do {
        printf("n: %d\n", (int) n );
        err = H5Aiterate2(dataset, H5_INDEX_NAME, H5_ITER_INC, &n, attr_info, NULL);
        if ( err < 0 ) break;
    } while(err > 0 );

    H5Oclose(dataset);
    return 0;
}
#endif

/* Check functions  */
/* Check if a data set has a given attribute.   */
int csc_hdf5_attribute_exist(hid_t root, const char *dset_name, const char *attr_name)
{
    hid_t dataset;
    htri_t ae;

    dataset = H5Oopen(root, dset_name, H5P_DEFAULT);
    if ( dataset < 0) {
        /* csc_error_message("Failed to open object: %s", dset_name); */
        return 0;
    }


    ae = H5Aexists(dataset, attr_name);
    if ( ae > 0 ) {
        H5Oclose(dataset);
        return 1;
    } else if ( ae == 0) {
        H5Oclose(dataset);
        return 0;
    } else {
        csc_error_message("csc_hdf5_attribute_exist: H5Aexists failed.");
    }
    H5Oclose(dataset);
    return 0;
}


