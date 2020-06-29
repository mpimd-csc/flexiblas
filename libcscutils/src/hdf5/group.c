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

int csc_hdf5_group_exist(hid_t base, const char *path)
{
	hid_t last = base, next;
    herr_t err;
    H5O_info_t info;
    int ret = 0;
	char *pch;
	char *pathc;
    char *saveptr;
	pathc = strdup(path);
	pch = strtok_r(pathc, "/", &saveptr);
	while (pch != NULL) {
		int exists = H5Lexists(last, pch, H5P_DEFAULT);
		if (!exists) {
            ret = 0;
            goto end;
		}

        exists = H5Oexists_by_name(last, pch, H5P_DEFAULT);
        if ( !exists) {
            ret = 0;
            goto end;
        }

        err = H5Oget_info_by_name(last, pch, &info, H5P_DEFAULT);
        if ( err < 0 ) {
            ret = 0;
            goto end;
        }

        if ( info.type != H5O_TYPE_GROUP ) {
            ret = 0;
            goto end;
        }

    	next = H5Gopen(last, pch, H5P_DEFAULT);
		if (last != base) H5Gclose(last);
		last = next;
		pch = strtok_r(NULL, "/", &saveptr);
	}
    ret = 1;
end:
	if (last != base) H5Gclose(last);
    free(pathc);
	return ret;
}

int csc_hdf5_group_path_create(hid_t root, const char * path, int last)
{
    char *saveptr = NULL;
    char *tok = NULL;
    char *dset_name_dup = NULL;
    char *tmp = NULL;
    size_t len;
    int depth;
    int ret = 0;
    hid_t gid;
    int k;


    len = strlen(path);

    /* Split the dset_name into group and data set part.   */
    dset_name_dup = strdup(path);
    tmp = strdup(path);

    tok = strtok_r(dset_name_dup, "/", &saveptr);
    depth = 0;
    while (tok) {
        tok = strtok_r(NULL, "/", &saveptr);
        depth ++;
    }

    /* create the path if necessary.  */
    strncpy(dset_name_dup, path, len+1);
    saveptr = NULL;
    tok = strtok_r(dset_name_dup, "/", &saveptr);
    tmp[0] = '\0';

    if ( ! last ) depth--;

    for (k = 0; k < depth; k++) {
        if ( k > 0 ) strcat(tmp,"/");
        strcat(tmp, tok);

        if ( csc_hdf5_dataset_exist(root, tmp) ) {
            if ( H5Ldelete(root, tmp, H5P_DEFAULT) < 0 ) {
                csc_error_message("Failed to remove previous entry %s\n", path);
                ret = -1;
                goto end;
            }
        }
        if ( ! csc_hdf5_group_exist(root, tmp)) {
            htri_t le = H5Lexists(root, tmp, H5P_DEFAULT);
            if ( le > 0 ) {
                if ( H5Ldelete(root, tmp, H5P_DEFAULT) < 0 ) {
                    csc_error_message("Failed to remove previous entry %s\n", path);
                    ret = -1;
                    goto end;
                }
            } else if ( le < 0 ) {
                csc_error_message("H5Lexists failed.\n");
            }
            gid = H5Gcreate(root, tmp, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            if ( gid < 0 ) {
                csc_error_message("Failed to create group %s\n", tmp);
                ret = -1;
                goto end;
            }
            H5Gclose(gid);
        }
        tok = strtok_r(NULL, "/", &saveptr);
    }

    ret = 0;
end:
    if ( dset_name_dup ) free(dset_name_dup);
    if ( tmp )  free(tmp);
    return ret;
}
