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
#include <stdint.h>
#include <string.h>
#include "cscutils/io.h"
#include "cscutils/mtx.h"
#include "cscutils/error_message.h"
#include "cscutils/strutils.h"

#define MM_MAX_LINE_LENGTH  1024
#define MM_MAX_TOKEN_LENGTH 128

#define MatrixMarketBanner "%%MatrixMarket"
#define MM_MTX_STR		"matrix"
#define MM_ARRAY_STR	"array"
#define MM_DENSE_STR	"array"
#define MM_COORDINATE_STR "coordinate"
#define MM_SPARSE_STR	"coordinate"
#define MM_COMPLEX_STR	"complex"
#define MM_REAL_STR		"real"
#define MM_INT_STR		"integer"
#define MM_GENERAL_STR  "general"
#define MM_SYMM_STR		"symmetric"
#define MM_HERM_STR		"hermitian"
#define MM_SKEW_STR		"skew-symmetric"
#define MM_PATTERN_STR  "pattern"


static int mm_read_banner(FILE *f, csc_mtx_datatype_t *dt, csc_mtx_symmetry_t *symt, csc_mtx_storetype_t *st)
{
    char line[MM_MAX_LINE_LENGTH];
    char banner[MM_MAX_TOKEN_LENGTH];
    char mtx[MM_MAX_TOKEN_LENGTH];
    char crd[MM_MAX_TOKEN_LENGTH];
    char data_type[MM_MAX_TOKEN_LENGTH];
    char storage_scheme[MM_MAX_TOKEN_LENGTH];


    if (fgets(line, MM_MAX_LINE_LENGTH, f) == NULL) {
        csc_error_message("failed to get line.\n");
        return -1;
    }

    if (sscanf(line, "%s %s %s %s %s", banner, mtx, crd, data_type,
        storage_scheme) != 5) {
        csc_error_message("Failed to parse header.");
        return -1;
    }

    csc_strlowercase(mtx);
    csc_strlowercase(crd);
    csc_strlowercase(data_type);
    csc_strlowercase(storage_scheme);

    /* check for banner */
    if (strncmp(banner, MatrixMarketBanner, strlen(MatrixMarketBanner)) != 0) {
        csc_error_message("Matrix Market Banner wrong.\n");
        return -1;
    }

    /* first field should be "mtx" */
    if (strcmp(mtx, MM_MTX_STR) != 0){
        csc_error_message("MTX field wrong mtx = %s\n", mtx);
        return  -1;
    }

    if (strcmp(crd, MM_SPARSE_STR) == 0)
        *st = CSC_MTX_COORDINATE;
    else if (strcmp(crd, MM_DENSE_STR) == 0)
        *st = CSC_MTX_ARRAY;
    else
        return -1;


    /* third field */

    if (strcmp(data_type, MM_REAL_STR) == 0)
        *dt =CSC_MTX_REAL;
    else if (strcmp(data_type, MM_COMPLEX_STR) == 0)
        *dt =CSC_MTX_COMPLEX;
    else if (strcmp(data_type, MM_PATTERN_STR) == 0)
        *dt = CSC_MTX_PATTERN;
    else if (strcmp(data_type, MM_INT_STR) == 0)
        *dt = CSC_MTX_INTEGER;
    else
        return -1;


    /* fourth field */

    if (strcmp(storage_scheme, MM_GENERAL_STR) == 0)
        *symt = CSC_MTX_GENERAL;
    else if (strcmp(storage_scheme, MM_SYMM_STR) == 0)
        *symt = CSC_MTX_SYMMETRIC;
    else if (strcmp(storage_scheme, MM_HERM_STR) == 0)
        *symt = CSC_MTX_HERMITIAN;
    else if (strcmp(storage_scheme, MM_SKEW_STR) == 0)
        *symt = CSC_MTX_SKEWSYMMETRIC;
    else
        return -1;


    return 0;
}


#if 0
static int mm_read_mtx_crd_size(FILE *f, int64_t *M, int64_t *N, int64_t *nz )
{
    char line[MM_MAX_LINE_LENGTH];
    int num_items_read;
    long Mx, Nx, nzx;

    /* set return null parameter values, in case we exit with errors */
    *M = *N = *nz = 0;

    /* now continue scanning until you reach the end-of-comments */
    do
    {
        if (fgets(line,MM_MAX_LINE_LENGTH,f) == NULL)
            return -1;
    }while (line[0] == '%');

    /* line[] is either blank or has M,N, nz */
    if (sscanf(line, "%ld %ld %ld", &Mx, &Nx, &nzx) == 3){
        *M =Mx;
        *N =Nx;
        *nz = nzx;
        return 0;
    }
    do
    {
        num_items_read = fscanf(f, "%ld %ld %ld", &Mx, &Nx, &nzx);
        if (num_items_read == EOF) return -1;
    }
    while (num_items_read != 3);

    *M =Mx;
    *N =Nx;
    *nz = nzx;


    return 0;
}
#endif


static int mm_read_mtx_array_size(FILE *f, int64_t *M, int64_t *N)
{
    char line[MM_MAX_LINE_LENGTH];
    int num_items_read;
    long Mx, Nx;
    /* set return null parameter values, in case we exit with errors */
    *M = *N = 0;

    /* now continue scanning until you reach the end-of-comments */
    do
    {
        if (fgets(line,MM_MAX_LINE_LENGTH,f) == NULL)
            return -1;
    }while (line[0] == '%');

    /* line[] is either blank or has M,N, nz */
    if (sscanf(line, "%ld %ld", &Mx, &Nx) == 2) {
        *M = Mx;
        *N = Nx;
        return 0;
    }
    do
    {
        num_items_read = fscanf(f, "%ld %ld", &Mx, &Nx);
        if (num_items_read == EOF) return -1;
    }
    while (num_items_read != 2);

    *M = Mx;
    *N = Nx;
    return 0;
}

int csc_mtx_read_dense_size(const char *filename, int64_t *M, int64_t*N)
{
    FILE *fp;
    csc_mtx_datatype_t dt;
    csc_mtx_symmetry_t symt;
    csc_mtx_storetype_t st;
    int64_t lm, ln;

    fp = fopen(filename, "r");
    if (!fp) {
        csc_error_message("Failed to open %s\n", filename);
        return -1;
    }
    mm_read_banner(fp, &dt, &symt, &st);
    if ( st != CSC_MTX_ARRAY ){
        csc_error_message("Failed to read Matrix Market header (filename = %s).\n", filename);
        fclose(fp);
        return -1;
    }
    if ( dt != CSC_MTX_REAL ) {
        csc_error_message("Matrix in %s is not real\n", filename);
        fclose(fp);
        return -1;
    }
    if ( symt != CSC_MTX_GENERAL ){
        csc_error_message("Matrix in %s is not general.\n", filename);
        fclose(fp);
        return -1;
    }

    if ( mm_read_mtx_array_size(fp, &lm, &ln)) {
        csc_error_message("Failed to read size from %s\n", filename);
        fclose(fp);
        return -1;
    }
    *M =lm;
    *N =ln;
    fclose(fp);
    return 0;
}

#define dA(K,L) A[K*lda+L]
int csc_mtx_read_double_dense(const char *filename, int64_t M, int64_t N, double *A, int64_t lda)
{
    FILE *fp;

    csc_mtx_datatype_t dt;
    csc_mtx_symmetry_t symt;
    csc_mtx_storetype_t st;
    int64_t lm, ln;
    int64_t k, l;
    double r;

    fp = fopen(filename, "r");
    if (!fp) {
        return -1;
    }
    mm_read_banner(fp, &dt, &symt, &st);
    if ( st != CSC_MTX_ARRAY ){
        fclose(fp);
        return -1;
    }
    if ( dt != CSC_MTX_REAL ) {
        fclose(fp);
        return -1;
    }
    if ( symt != CSC_MTX_GENERAL ){
        fclose(fp);
        return -1;
    }

    if ( mm_read_mtx_array_size(fp, &lm, &ln)) {
        fclose(fp);
        return -1;
    }

    if (lm != M || ln != N) {
        fclose(fp);
        return -1;
    }

    for (l = 0; l < N; l++) {
        for (k = 0; k < M; k++) {
            if ( fscanf(fp, "%lg", &r) != 1) {
                fclose(fp);
                return -1;
            }
            dA(k,l) = r;
        }
    }
    fclose(fp);
    return 0;
}

