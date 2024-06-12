//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2024 Martin Koehler

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with this program. If not, see <https://www.gnu.org/licenses/>.
   */




#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <math.h>
#include <complex.h>

#include "flexiblas.h"

#ifdef EXTBLAS_ENABLED
#endif



HIDDEN int __flexiblas_load_cblas(flexiblas_backend_t *backend)
{
    void * cblas_in_blis = dlsym(backend->library_handle, "bli_info_get_enable_cblas");
    if ( cblas_in_blis ) {
        DPRINTF_WARN(1, "The desired BLAS library is BLIS. We do not load their CBLAS wrapper since it might alter the behavior of your programs.");
        return 0;
    }
    /*-----------------------------------------------------------------------------
     *  Integer and XERBLA routine
     *-----------------------------------------------------------------------------*/
    LOAD_CBLAS(backend,blas.icamax,icamax);
    LOAD_CBLAS(backend,blas.idamax,idamax);
    LOAD_CBLAS(backend,blas.isamax,isamax);
    LOAD_CBLAS(backend,blas.izamax,izamax);
    // LOAD_CBLAS(backend,xerbla,xerbla);

    /*-----------------------------------------------------------------------------
     *  Single Precision Routines
     *-----------------------------------------------------------------------------*/
    LOAD_CBLAS(backend,blas.sasum,sasum );
    LOAD_CBLAS(backend,blas.saxpy,saxpy);
    LOAD_CBLAS(backend,blas.scasum,scasum);
    LOAD_CBLAS(backend,blas.scnrm2,scnrm2);
    LOAD_CBLAS(backend,blas.scopy,scopy);
    LOAD_CBLAS(backend,blas.sdot,sdot);
    LOAD_CBLAS(backend,blas.sdsdot,sdsdot);
    LOAD_CBLAS(backend,blas.sgbmv,sgbmv);
    LOAD_CBLAS(backend,blas.sgemm,sgemm);
    LOAD_CBLAS(backend,blas.sgemv,sgemv);
    LOAD_CBLAS(backend,blas.sger,sger);
    LOAD_CBLAS(backend,blas.snrm2,snrm2);
    LOAD_CBLAS(backend,blas.srot,srot);
    LOAD_CBLAS(backend,blas.srotg,srotg);
    LOAD_CBLAS(backend,blas.srotm,srotm);
    LOAD_CBLAS(backend,blas.srotmg,srotmg);
    LOAD_CBLAS(backend,blas.ssbmv,ssbmv);
    LOAD_CBLAS(backend,blas.sscal,sscal);
    LOAD_CBLAS(backend,blas.sspmv,sspmv);
    LOAD_CBLAS(backend,blas.sspr2,sspr2);
    LOAD_CBLAS(backend,blas.sspr,sspr);
    LOAD_CBLAS(backend,blas.sswap,sswap);
    LOAD_CBLAS(backend,blas.ssymm,ssymm);
    LOAD_CBLAS(backend,blas.ssymv,ssymv);
    LOAD_CBLAS(backend,blas.ssyr2,ssyr2);
    LOAD_CBLAS(backend,blas.ssyr2k,ssyr2k);
    LOAD_CBLAS(backend,blas.ssyr,ssyr);
    LOAD_CBLAS(backend,blas.ssyrk,ssyrk);
    LOAD_CBLAS(backend,blas.stbmv,stbmv);
    LOAD_CBLAS(backend,blas.stbsv,stbsv);
    LOAD_CBLAS(backend,blas.stpmv,stpmv);
    LOAD_CBLAS(backend,blas.stpsv,stpsv);
    LOAD_CBLAS(backend,blas.strmm,strmm);
    LOAD_CBLAS(backend,blas.strmv,strmv);
    LOAD_CBLAS(backend,blas.strsm,strsm);
    LOAD_CBLAS(backend,blas.strsv,strsv);


    /*-----------------------------------------------------------------------------
     *  Double Precision Routines
     *-----------------------------------------------------------------------------*/
    LOAD_CBLAS(backend,blas.dasum,dasum);
    LOAD_CBLAS(backend,blas.daxpy,daxpy);
    LOAD_CBLAS(backend,blas.dcopy,dcopy);
    LOAD_CBLAS(backend,blas.ddot,ddot);
    LOAD_CBLAS(backend,blas.dgbmv,dgbmv);
    LOAD_CBLAS(backend,blas.dgemm,dgemm);
    LOAD_CBLAS(backend,blas.dgemv,dgemv);
    LOAD_CBLAS(backend,blas.dger,dger);
    LOAD_CBLAS(backend,blas.dnrm2,dnrm2);
    LOAD_CBLAS(backend,blas.drot,drot);
    LOAD_CBLAS(backend,blas.drotg,drotg);
    LOAD_CBLAS(backend,blas.drotm,drotm);
    LOAD_CBLAS(backend,blas.drotmg,drotmg);
    LOAD_CBLAS(backend,blas.dsbmv,dsbmv);
    LOAD_CBLAS(backend,blas.dscal,dscal);
    LOAD_CBLAS(backend,blas.dsdot,dsdot);
    LOAD_CBLAS(backend,blas.dspmv,dspmv);
    LOAD_CBLAS(backend,blas.dspr2,dspr2);
    LOAD_CBLAS(backend,blas.dspr,dspr);
    LOAD_CBLAS(backend,blas.dswap,dswap);
    LOAD_CBLAS(backend,blas.dsymm,dsymm);
    LOAD_CBLAS(backend,blas.dsymv,dsymv);
    LOAD_CBLAS(backend,blas.dsyr2,dsyr2);
    LOAD_CBLAS(backend,blas.dsyr2k,dsyr2k);
    LOAD_CBLAS(backend,blas.dsyr,dsyr);
    LOAD_CBLAS(backend,blas.dsyrk,dsyrk);
    LOAD_CBLAS(backend,blas.dtbmv,dtbmv);
    LOAD_CBLAS(backend,blas.dtbsv,dtbsv);
    LOAD_CBLAS(backend,blas.dtpmv,dtpmv);
    LOAD_CBLAS(backend,blas.dtpsv,dtpsv);
    LOAD_CBLAS(backend,blas.dtrmm,dtrmm);
    LOAD_CBLAS(backend,blas.dtrmv,dtrmv);
    LOAD_CBLAS(backend,blas.dtrsm,dtrsm);
    LOAD_CBLAS(backend,blas.dtrsv,dtrsv);
    LOAD_CBLAS(backend,blas.dzasum,dzasum);
    LOAD_CBLAS(backend,blas.dznrm2,dznrm2);


    /*-----------------------------------------------------------------------------
     *  Complex Routines
     *-----------------------------------------------------------------------------*/
    LOAD_CBLAS(backend,blas.caxpy,caxpy);
    LOAD_CBLAS(backend,blas.ccopy,ccopy);
    LOAD_CBLAS(backend,blas.cdotc,cdotc_sub);
    LOAD_CBLAS(backend,blas.cdotu,cdotu_sub);
    LOAD_CBLAS(backend,blas.cgbmv,cgbmv);
    LOAD_CBLAS(backend,blas.cgemm,cgemm);
    LOAD_CBLAS(backend,blas.cgemv,cgemv);
    LOAD_CBLAS(backend,blas.cgerc,cgerc);
    LOAD_CBLAS(backend,blas.cgeru,cgeru);
    LOAD_CBLAS(backend,blas.chbmv,chbmv);
    LOAD_CBLAS(backend,blas.chemm,chemm);
    LOAD_CBLAS(backend,blas.chemv,chemv);
    LOAD_CBLAS(backend,blas.cher2,cher2);
    LOAD_CBLAS(backend,blas.cher2k,cher2k);
    LOAD_CBLAS(backend,blas.cher,cher);
    LOAD_CBLAS(backend,blas.cherk,cherk);
    LOAD_CBLAS(backend,blas.chpmv,chpmv);
    LOAD_CBLAS(backend,blas.chpr2,chpr2);
    LOAD_CBLAS(backend,blas.chpr,chpr);
    LOAD_CBLAS(backend,blas.crotg,crotg);
    LOAD_CBLAS(backend,blas.cscal,cscal);
    LOAD_CBLAS(backend,blas.csrot,csrot);
    LOAD_CBLAS(backend,blas.csscal,csscal);
    LOAD_CBLAS(backend,blas.cswap,cswap);
    LOAD_CBLAS(backend,blas.csymm,csymm);
    LOAD_CBLAS(backend,blas.csyr2k,csyr2k);
    LOAD_CBLAS(backend,blas.csyrk,csyrk);
    LOAD_CBLAS(backend,blas.ctbmv,ctbmv);
    LOAD_CBLAS(backend,blas.ctbsv,ctbsv);
    LOAD_CBLAS(backend,blas.ctpmv,ctpmv);
    LOAD_CBLAS(backend,blas.ctpsv,ctpsv);
    LOAD_CBLAS(backend,blas.ctrmm,ctrmm);
    LOAD_CBLAS(backend,blas.ctrmv,ctrmv);
    LOAD_CBLAS(backend,blas.ctrsm,ctrsm);
    LOAD_CBLAS(backend,blas.ctrsv,ctrsv);
    LOAD_CBLAS(backend,blas.crotg,crotg);
    LOAD_CBLAS(backend,blas.csrot,csrot);


    /*-----------------------------------------------------------------------------
     *  Complex*16 Routines
     *-----------------------------------------------------------------------------*/
    LOAD_CBLAS(backend,blas.zaxpy,zaxpy);
    LOAD_CBLAS(backend,blas.zcopy,zcopy);
    LOAD_CBLAS(backend,blas.zdotc,zdotc_sub);
    LOAD_CBLAS(backend,blas.zdotu,zdotu_sub);
    LOAD_CBLAS(backend,blas.zdrot,zdrot);
    LOAD_CBLAS(backend,blas.zdscal,zdscal);
    LOAD_CBLAS(backend,blas.zgbmv,zgbmv);
    LOAD_CBLAS(backend,blas.zgemm,zgemm);
    LOAD_CBLAS(backend,blas.zgemv,zgemv);
    LOAD_CBLAS(backend,blas.zgerc,zgerc);
    LOAD_CBLAS(backend,blas.zgeru,zgeru);
    LOAD_CBLAS(backend,blas.zhbmv,zhbmv);
    LOAD_CBLAS(backend,blas.zhemm,zhemm);
    LOAD_CBLAS(backend,blas.zhemv,zhemv);
    LOAD_CBLAS(backend,blas.zher2,zher2);
    LOAD_CBLAS(backend,blas.zher2k,zher2k);
    LOAD_CBLAS(backend,blas.zher,zher);
    LOAD_CBLAS(backend,blas.zherk,zherk);
    LOAD_CBLAS(backend,blas.zhpmv,zhpmv);
    LOAD_CBLAS(backend,blas.zhpr2,zhpr2);
    LOAD_CBLAS(backend,blas.zhpr,zhpr);
    LOAD_CBLAS(backend,blas.zrotg,zrotg);
    LOAD_CBLAS(backend,blas.zscal,zscal);
    LOAD_CBLAS(backend,blas.zswap,zswap);
    LOAD_CBLAS(backend,blas.zsymm,zsymm);
    LOAD_CBLAS(backend,blas.zsyr2k,zsyr2k);
    LOAD_CBLAS(backend,blas.zsyrk,zsyrk);
    LOAD_CBLAS(backend,blas.ztbmv,ztbmv);
    LOAD_CBLAS(backend,blas.ztbsv,ztbsv);
    LOAD_CBLAS(backend,blas.ztpmv,ztpmv);
    LOAD_CBLAS(backend,blas.ztpsv,ztpsv);
    LOAD_CBLAS(backend,blas.ztrmm,ztrmm);
    LOAD_CBLAS(backend,blas.ztrmv,ztrmv);
    LOAD_CBLAS(backend,blas.ztrsm,ztrsm);
    LOAD_CBLAS(backend,blas.ztrsv,ztrsv);
    LOAD_CBLAS(backend,blas.zrotg,zrotg);
    LOAD_CBLAS(backend,blas.zdrot,zdrot);


    return 0;
}

