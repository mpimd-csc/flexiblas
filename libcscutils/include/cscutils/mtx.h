/*
* CSCUTILS - A collection of various software routines uses in CSC projects
* Copyright (C) 2018 Martin Koehler
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

#ifndef CSC_MTX_H

#define CSC_MTX_H
#include "cscutils/cscutils_config.h"

#ifdef __cplusplus
extern "C" {
#endif

    #include <stdio.h>
    #include <stdlib.h>
    #include <stdint.h>
    #include <stdarg.h>
    #include <sys/stat.h>
    /**
     * @file libcscutils/include/cscutils/mtx.h
     * @defgroup mtx MatrixMarket File Support
     * @brief This part of the library contains routines to write matrix market files.
     *
     * The MatrixMarket Exchange (MTX) format is an easy humanreadable format to store
     * sparse matrices. This module helps to handle these files.
     *
     * \attention This part of the library depends on the \ref error_message module and the \ref io module.
     *
     * @todo support compressed I/O for all functions
     * @todo full featured access to MatrixMarket files.
     *
     * @addtogroup mtx
     * @{
    */

    /**
     * @brief Enumeration to identify the data type of the matrix.
     *
     * The csc_mtx_datatype_t enumeration is used to identify the data type of the
     * values used to store the matrix.
     */
    typedef enum {
        CSC_MTX_INTEGER = 0,    /**< The matrix uses integer values. */
        CSC_MTX_REAL    = 1,    /**< The matrix uses real values. */
        CSC_MTX_COMPLEX = 2,    /**< The matrix uses complex values. */
        CSC_MTX_PATTERN = 3     /**< The matrix contains only the sparse pattern and stores no values. */
    } csc_mtx_datatype_t;

    /**
     * @brief Enumeration to identify the structure of the matrix.
     *
     * The csc_mtx_symmetry_t enumeration is used to identify the structure of the stored matrix.
     */
    typedef enum {
        CSC_MTX_GENERAL = 0,            /**< The stored matrix has no special structure. It is a generic one. */
        CSC_MTX_SYMMETRIC = 1,          /**< The stored matrix is symmatric. Only one triangle is stored in the file. */
        CSC_MTX_SKEWSYMMETRIC = 2,      /**< The stored matrix is skew-symmatric. Only one triangle is stored in the file. */
        CSC_MTX_HERMITIAN = 3           /**< The stored matrix is hermitian-symmatric. Only one triangle is stored in the file. */
    } csc_mtx_symmetry_t;

    /**
     * @brief Enumeration to identify the storage type of a matrix.
     *
     * The csc_mtx_storetype_t enumeration is used to distinguish between sparse and dense matrices
     * stored in a file.
     */
    typedef enum {
        CSC_MTX_COORDINATE = 0,         /**< The matrix is sparse and stored in the coordinate format. */
        CSC_MTX_ARRAY = 1               /**< The matrix is dense and stored in the array format. */
    } csc_mtx_storetype_t;

    /**
     * @brief Write a Double Precision Matrix to a Matrix Market File.
     * @param[in] filename      Filename
     * @param[in] m             Number of Rows
     * @param[in] n             Number of Columns
     * @param[in] A             Matrix to write
     * @param[in] lda           Leading dimension of the matrix
     * @return zero on success, -1 otherwise.
     *
     * The csc_mtx_write_double_dense function write a double precision matrix to a mtx file.
     * The matrix is stored with general structure and using the array format.
     *
     */
    int csc_mtx_write_double_dense(char * filename, int m, int n, double *A, int lda);

    /**
     * @brief Read the size from an array-storage MatrixMarket file.
     * @param[in]   filename        Filename
     * @param[out]  M               Number of rows
     * @param[out]  N               Number of columns
     * @return zero on success or a negative value in case of an error.
     *
     * The csc_mtx_read_dense_size function reads the size of an array from
     * a MatrixMarket file. It checks whether the matrix is stored as an array
     * and contains real values. If this is not the case an error is returned.
     *
     * @attention The function does not support to read from compressed files yet.
     *
     */
    int csc_mtx_read_dense_size(const char *filename, int64_t *M, int64_t*N);

    /**
     * @brief Read a dense matrix form a MatrixMarket file.
     * @param[in]   filename    Filename
     * @param[in]   M           Number of rows
     * @param[in]   N           Number of columns
     * @param[out]  A           Pointer to a preallocated array to store the read matrix
     * @param[in]   lda         Leading dimension of the matrix.
     * @return zero on success or a negative value in case of an error.
     *
     * The csc_mtx_read_double_dense function reads a real values dense matrix from
     * a MatrixMarket file into a column-major stored matrix. If the matrix is not
     * stored in the array format or does not contain real values an error is returned.
     * The size of the matrix can be obtained previously with the help of \ref csc_mtx_read_dense_size.
     *
     * @see csc_mtx_read_dense_size
     * @attention The function deos not support to read from compressed files yet.
     *
     */
    int csc_mtx_read_double_dense(const char *filename, int64_t M, int64_t N, double *A, int64_t lda);


    /**
     * @}
     */


#ifdef __cplusplus
};
#endif



#endif /* end of include guard: CSC_IO_H */



