/*
 * LIBCSCUTILS -- BZIP2 - HDF5 Helper
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
#ifndef CSC_FILTER_HDF5_BZIP2

#define CSC_FILTER_HDF5_BZIP2

#ifdef __cplusplus
extern "C" {
#endif

    /* Filter number, see https://support.hdfgroup.org/services/contributions.html  */
    #define H5Z_FILTER_BZIP2 305

    int csc_hdf5_register_bzip2();

#ifdef __cplusplus
};
#endif



#endif /* end of include guard: CSC_FILTER_HDF5_BZIP2 */






