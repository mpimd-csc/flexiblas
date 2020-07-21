
/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Linking FlexiBLAS statically or dynamically with other modules is making a
 * combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
 * General Public License cover the whole combination.
 *
 * As a special exception, the copyright holders of FlexiBLAS give you permission
 * to combine FlexiBLAS program with free software programs or libraries that are
 * released under the GNU LGPL and with independent modules that communicate with
 * FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
 * BLAS/LAPACK reference implementation. You may copy and distribute such a system
 * following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
 * code concerned, provided that you include the source code of that other code
 * when and as the GNU GPL requires distribution of source code and provided that
 * you do not modify the BLAS/LAPACK interface.
 *
 * Note that people who make modified versions of FlexiBLAS are not obligated to
 * grant this special exception for their modified versions; it is their choice
 * whether to do so. The GNU General Public License gives permission to release a
 * modified version without this exception; this exception also makes it possible
 * to release a modified version which carries forward this exception. If you
 * modify the BLAS/LAPACK interface, this exception does not apply to your
 * modified version of FlexiBLAS, and you must remove this exception when you
 * distribute your modified version.
 *
 * This exception is an additional permission under section 7 of the GNU General
 * Public License, version 3 (“GPLv3”)
 *
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2013-2020
 */




#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <dlfcn.h>
#include "flexiblas_backend.h"

#include "profile_hook.h"

extern void flexiblas_chain_caxpy (Int * n, float complex* ca, float complex* cx, Int * incx, float complex* cy, Int * incy);
void hook_caxpy(Int * n, float complex* ca, float complex* cx, Int * incx, float complex* cy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_caxpy((void*) n, (void*) ca, (void*) cx, (void*) incx, (void*) cy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->caxpy.timings[0] += (helpTimeStop - helpTime);
    data->caxpy.calls[0]++;

    return ;
}



extern void flexiblas_chain_ccopy (Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy);
void hook_ccopy(Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ccopy((void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->ccopy.timings[0] += (helpTimeStop - helpTime);
    data->ccopy.calls[0]++;

    return ;
}



extern float complex flexiblas_chain_cdotc (void *retvalue, Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy);
float complex hook_cdotc(Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy)
{
    float complex v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cdotc( (void*) &v , (void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->cdotc.timings[0] += (helpTimeStop - helpTime);
    data->cdotc.calls[0]++;

    return v;
}



extern float complex flexiblas_chain_cdotu (void *retvalue, Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy);
float complex hook_cdotu(Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy)
{
    float complex v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cdotu( (void*) &v , (void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->cdotu.timings[0] += (helpTimeStop - helpTime);
    data->cdotu.calls[0]++;

    return v;
}



extern void flexiblas_chain_cgbmv (char* trans, Int * m, Int * n, Int * kl, Int * ku, float complex* alpha, float complex* a, Int * lda, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy);
void hook_cgbmv(char* trans, Int * m, Int * n, Int * kl, Int * ku, float complex* alpha, float complex* a, Int * lda, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cgbmv((void*) trans, (void*) m, (void*) n, (void*) kl, (void*) ku, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->cgbmv.timings[0] += (helpTimeStop - helpTime);
    data->cgbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_cgemm (char* transa, char* transb, Int * m, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float complex* beta, float complex* c, Int * ldc);
void hook_cgemm(char* transa, char* transb, Int * m, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float complex* beta, float complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cgemm((void*) transa, (void*) transb, (void*) m, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->cgemm.timings[0] += (helpTimeStop - helpTime);
    data->cgemm.calls[0]++;

    return ;
}



extern void flexiblas_chain_cgemv (char* trans, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy);
void hook_cgemv(char* trans, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cgemv((void*) trans, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->cgemv.timings[0] += (helpTimeStop - helpTime);
    data->cgemv.calls[0]++;

    return ;
}



extern void flexiblas_chain_cgerc (Int * m, Int * n, float complex* alpha, float complex* x, Int * incx, float complex* y, Int * incy, float complex* a, Int * lda);
void hook_cgerc(Int * m, Int * n, float complex* alpha, float complex* x, Int * incx, float complex* y, Int * incy, float complex* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cgerc((void*) m, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->cgerc.timings[0] += (helpTimeStop - helpTime);
    data->cgerc.calls[0]++;

    return ;
}



extern void flexiblas_chain_cgeru (Int * m, Int * n, float complex* alpha, float complex* x, Int * incx, float complex* y, Int * incy, float complex* a, Int * lda);
void hook_cgeru(Int * m, Int * n, float complex* alpha, float complex* x, Int * incx, float complex* y, Int * incy, float complex* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cgeru((void*) m, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->cgeru.timings[0] += (helpTimeStop - helpTime);
    data->cgeru.calls[0]++;

    return ;
}



extern void flexiblas_chain_chbmv (char* uplo, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy);
void hook_chbmv(char* uplo, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_chbmv((void*) uplo, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->chbmv.timings[0] += (helpTimeStop - helpTime);
    data->chbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_chemm (char* side, char* uplo, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float complex* beta, float complex* c, Int * ldc);
void hook_chemm(char* side, char* uplo, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float complex* beta, float complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_chemm((void*) side, (void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->chemm.timings[0] += (helpTimeStop - helpTime);
    data->chemm.calls[0]++;

    return ;
}



extern void flexiblas_chain_chemv (char* uplo, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy);
void hook_chemv(char* uplo, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_chemv((void*) uplo, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->chemv.timings[0] += (helpTimeStop - helpTime);
    data->chemv.calls[0]++;

    return ;
}



extern void flexiblas_chain_cher (char* uplo, Int * n, float* alpha, float complex* x, Int * incx, float complex* a, Int * lda);
void hook_cher(char* uplo, Int * n, float* alpha, float complex* x, Int * incx, float complex* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cher((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->cher.timings[0] += (helpTimeStop - helpTime);
    data->cher.calls[0]++;

    return ;
}



extern void flexiblas_chain_cher2 (char* uplo, Int * n, float complex* alpha, float complex* x, Int * incx, float complex* y, Int * incy, float complex* a, Int * lda);
void hook_cher2(char* uplo, Int * n, float complex* alpha, float complex* x, Int * incx, float complex* y, Int * incy, float complex* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cher2((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->cher2.timings[0] += (helpTimeStop - helpTime);
    data->cher2.calls[0]++;

    return ;
}



extern void flexiblas_chain_cher2k (char* uplo, char* trans, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float* beta, float complex* c, Int * ldc);
void hook_cher2k(char* uplo, char* trans, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float* beta, float complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cher2k((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->cher2k.timings[0] += (helpTimeStop - helpTime);
    data->cher2k.calls[0]++;

    return ;
}



extern void flexiblas_chain_cherk (char* uplo, char* trans, Int * n, Int * k, float* alpha, float complex* a, Int * lda, float* beta, float complex* c, Int * ldc);
void hook_cherk(char* uplo, char* trans, Int * n, Int * k, float* alpha, float complex* a, Int * lda, float* beta, float complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cherk((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->cherk.timings[0] += (helpTimeStop - helpTime);
    data->cherk.calls[0]++;

    return ;
}



extern void flexiblas_chain_chpmv (char* uplo, Int * n, float complex* alpha, float complex* ap, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy);
void hook_chpmv(char* uplo, Int * n, float complex* alpha, float complex* ap, float complex* x, Int * incx, float complex* beta, float complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_chpmv((void*) uplo, (void*) n, (void*) alpha, (void*) ap, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->chpmv.timings[0] += (helpTimeStop - helpTime);
    data->chpmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_chpr (char* uplo, Int * n, float* alpha, float complex* x, Int * incx, float complex* ap);
void hook_chpr(char* uplo, Int * n, float* alpha, float complex* x, Int * incx, float complex* ap)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_chpr((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap);

    helpTimeStop = flexiblas_wtime();

    data->chpr.timings[0] += (helpTimeStop - helpTime);
    data->chpr.calls[0]++;

    return ;
}



extern void flexiblas_chain_chpr2 (char* uplo, Int * n, float complex* alpha, float complex* x, Int * incx, float complex* y, Int * incy, float complex* ap);
void hook_chpr2(char* uplo, Int * n, float complex* alpha, float complex* x, Int * incx, float complex* y, Int * incy, float complex* ap)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_chpr2((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) ap);

    helpTimeStop = flexiblas_wtime();

    data->chpr2.timings[0] += (helpTimeStop - helpTime);
    data->chpr2.calls[0]++;

    return ;
}



extern void flexiblas_chain_crotg (float complex* ca, float complex* cb, float* c, float complex* s);
void hook_crotg(float complex* ca, float complex* cb, float* c, float complex* s)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_crotg((void*) ca, (void*) cb, (void*) c, (void*) s);

    helpTimeStop = flexiblas_wtime();

    data->crotg.timings[0] += (helpTimeStop - helpTime);
    data->crotg.calls[0]++;

    return ;
}



extern void flexiblas_chain_cscal (Int * n, float complex* ca, float complex* cx, Int * incx);
void hook_cscal(Int * n, float complex* ca, float complex* cx, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cscal((void*) n, (void*) ca, (void*) cx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->cscal.timings[0] += (helpTimeStop - helpTime);
    data->cscal.calls[0]++;

    return ;
}



extern void flexiblas_chain_csrot (Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy, float* c, float* s);
void hook_csrot(Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy, float* c, float* s)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_csrot((void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy, (void*) c, (void*) s);

    helpTimeStop = flexiblas_wtime();

    data->csrot.timings[0] += (helpTimeStop - helpTime);
    data->csrot.calls[0]++;

    return ;
}



extern void flexiblas_chain_csscal (Int * n, float* sa, float complex* cx, Int * incx);
void hook_csscal(Int * n, float* sa, float complex* cx, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_csscal((void*) n, (void*) sa, (void*) cx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->csscal.timings[0] += (helpTimeStop - helpTime);
    data->csscal.calls[0]++;

    return ;
}



extern void flexiblas_chain_cswap (Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy);
void hook_cswap(Int * n, float complex* cx, Int * incx, float complex* cy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cswap((void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->cswap.timings[0] += (helpTimeStop - helpTime);
    data->cswap.calls[0]++;

    return ;
}



extern void flexiblas_chain_csymm (char* side, char* uplo, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float complex* beta, float complex* c, Int * ldc);
void hook_csymm(char* side, char* uplo, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float complex* beta, float complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_csymm((void*) side, (void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->csymm.timings[0] += (helpTimeStop - helpTime);
    data->csymm.calls[0]++;

    return ;
}



extern void flexiblas_chain_csyr2k (char* uplo, char* trans, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float complex* beta, float complex* c, Int * ldc);
void hook_csyr2k(char* uplo, char* trans, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb, float complex* beta, float complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_csyr2k((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->csyr2k.timings[0] += (helpTimeStop - helpTime);
    data->csyr2k.calls[0]++;

    return ;
}



extern void flexiblas_chain_csyrk (char* uplo, char* trans, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* beta, float complex* c, Int * ldc);
void hook_csyrk(char* uplo, char* trans, Int * n, Int * k, float complex* alpha, float complex* a, Int * lda, float complex* beta, float complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_csyrk((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->csyrk.timings[0] += (helpTimeStop - helpTime);
    data->csyrk.calls[0]++;

    return ;
}



extern void flexiblas_chain_ctbmv (char* uplo, char* trans, char* diag, Int * n, Int * k, float complex* a, Int * lda, float complex* x, Int * incx);
void hook_ctbmv(char* uplo, char* trans, char* diag, Int * n, Int * k, float complex* a, Int * lda, float complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ctbmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ctbmv.timings[0] += (helpTimeStop - helpTime);
    data->ctbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ctbsv (char* uplo, char* trans, char* diag, Int * n, Int * k, float complex* a, Int * lda, float complex* x, Int * incx);
void hook_ctbsv(char* uplo, char* trans, char* diag, Int * n, Int * k, float complex* a, Int * lda, float complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ctbsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ctbsv.timings[0] += (helpTimeStop - helpTime);
    data->ctbsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ctpmv (char* uplo, char* trans, char* diag, Int * n, float complex* ap, float complex* x, Int * incx);
void hook_ctpmv(char* uplo, char* trans, char* diag, Int * n, float complex* ap, float complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ctpmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) ap, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ctpmv.timings[0] += (helpTimeStop - helpTime);
    data->ctpmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ctpsv (char* uplo, char* trans, char* diag, Int * n, float complex* ap, float complex* x, Int * incx);
void hook_ctpsv(char* uplo, char* trans, char* diag, Int * n, float complex* ap, float complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ctpsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) ap, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ctpsv.timings[0] += (helpTimeStop - helpTime);
    data->ctpsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ctrmm (char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb);
void hook_ctrmm(char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ctrmm((void*) side, (void*) uplo, (void*) transa, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->ctrmm.timings[0] += (helpTimeStop - helpTime);
    data->ctrmm.calls[0]++;

    return ;
}



extern void flexiblas_chain_ctrmv (char* uplo, char* trans, char* diag, Int * n, float complex* a, Int * lda, float complex* x, Int * incx);
void hook_ctrmv(char* uplo, char* trans, char* diag, Int * n, float complex* a, Int * lda, float complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ctrmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ctrmv.timings[0] += (helpTimeStop - helpTime);
    data->ctrmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ctrsm (char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb);
void hook_ctrsm(char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ctrsm((void*) side, (void*) uplo, (void*) transa, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->ctrsm.timings[0] += (helpTimeStop - helpTime);
    data->ctrsm.calls[0]++;

    return ;
}



extern void flexiblas_chain_ctrsv (char* uplo, char* trans, char* diag, Int * n, float complex* a, Int * lda, float complex* x, Int * incx);
void hook_ctrsv(char* uplo, char* trans, char* diag, Int * n, float complex* a, Int * lda, float complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ctrsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ctrsv.timings[0] += (helpTimeStop - helpTime);
    data->ctrsv.calls[0]++;

    return ;
}



extern double flexiblas_chain_dasum (Int * n, double* dx, Int * incx);
double hook_dasum(Int * n, double* dx, Int * incx)
{
    double v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_dasum((void*) n, (void*) dx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dasum.timings[0] += (helpTimeStop - helpTime);
    data->dasum.calls[0]++;

    return v;
}



extern void flexiblas_chain_daxpy (Int * n, double* da, double* dx, Int * incx, double* dy, Int * incy);
void hook_daxpy(Int * n, double* da, double* dx, Int * incx, double* dy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_daxpy((void*) n, (void*) da, (void*) dx, (void*) incx, (void*) dy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->daxpy.timings[0] += (helpTimeStop - helpTime);
    data->daxpy.calls[0]++;

    return ;
}



extern void flexiblas_chain_dcopy (Int * n, double* dx, Int * incx, double* dy, Int * incy);
void hook_dcopy(Int * n, double* dx, Int * incx, double* dy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dcopy((void*) n, (void*) dx, (void*) incx, (void*) dy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->dcopy.timings[0] += (helpTimeStop - helpTime);
    data->dcopy.calls[0]++;

    return ;
}



extern double flexiblas_chain_ddot (Int * n, double* dx, Int * incx, double* dy, Int * incy);
double hook_ddot(Int * n, double* dx, Int * incx, double* dy, Int * incy)
{
    double v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_ddot((void*) n, (void*) dx, (void*) incx, (void*) dy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->ddot.timings[0] += (helpTimeStop - helpTime);
    data->ddot.calls[0]++;

    return v;
}



extern void flexiblas_chain_dgbmv (char* trans, Int * m, Int * n, Int * kl, Int * ku, double* alpha, double* a, Int * lda, double* x, Int * incx, double* beta, double* y, Int * incy);
void hook_dgbmv(char* trans, Int * m, Int * n, Int * kl, Int * ku, double* alpha, double* a, Int * lda, double* x, Int * incx, double* beta, double* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dgbmv((void*) trans, (void*) m, (void*) n, (void*) kl, (void*) ku, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->dgbmv.timings[0] += (helpTimeStop - helpTime);
    data->dgbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dgemm (char* transa, char* transb, Int * m, Int * n, Int * k, double* alpha, double* a, Int * lda, double* b, Int * ldb, double* beta, double* c, Int * ldc);
void hook_dgemm(char* transa, char* transb, Int * m, Int * n, Int * k, double* alpha, double* a, Int * lda, double* b, Int * ldb, double* beta, double* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dgemm((void*) transa, (void*) transb, (void*) m, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->dgemm.timings[0] += (helpTimeStop - helpTime);
    data->dgemm.calls[0]++;

    return ;
}



extern void flexiblas_chain_dgemv (char* trans, Int * m, Int * n, double* alpha, double* a, Int * lda, double* x, Int * incx, double* beta, double* y, Int * incy);
void hook_dgemv(char* trans, Int * m, Int * n, double* alpha, double* a, Int * lda, double* x, Int * incx, double* beta, double* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dgemv((void*) trans, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->dgemv.timings[0] += (helpTimeStop - helpTime);
    data->dgemv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dger (Int * m, Int * n, double* alpha, double* x, Int * incx, double* y, Int * incy, double* a, Int * lda);
void hook_dger(Int * m, Int * n, double* alpha, double* x, Int * incx, double* y, Int * incy, double* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dger((void*) m, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->dger.timings[0] += (helpTimeStop - helpTime);
    data->dger.calls[0]++;

    return ;
}



extern double flexiblas_chain_dnrm2 (Int * n, double* x, Int * incx);
double hook_dnrm2(Int * n, double* x, Int * incx)
{
    double v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_dnrm2((void*) n, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dnrm2.timings[0] += (helpTimeStop - helpTime);
    data->dnrm2.calls[0]++;

    return v;
}



extern void flexiblas_chain_drot (Int * n, double* dx, Int * incx, double* dy, Int * incy, double* c, double* s);
void hook_drot(Int * n, double* dx, Int * incx, double* dy, Int * incy, double* c, double* s)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_drot((void*) n, (void*) dx, (void*) incx, (void*) dy, (void*) incy, (void*) c, (void*) s);

    helpTimeStop = flexiblas_wtime();

    data->drot.timings[0] += (helpTimeStop - helpTime);
    data->drot.calls[0]++;

    return ;
}



extern void flexiblas_chain_drotg (double* da, double* db, double* c, double* s);
void hook_drotg(double* da, double* db, double* c, double* s)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_drotg((void*) da, (void*) db, (void*) c, (void*) s);

    helpTimeStop = flexiblas_wtime();

    data->drotg.timings[0] += (helpTimeStop - helpTime);
    data->drotg.calls[0]++;

    return ;
}



extern void flexiblas_chain_drotm (Int * n, double* dx, Int * incx, double* dy, Int * incy, double* dparam);
void hook_drotm(Int * n, double* dx, Int * incx, double* dy, Int * incy, double* dparam)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_drotm((void*) n, (void*) dx, (void*) incx, (void*) dy, (void*) incy, (void*) dparam);

    helpTimeStop = flexiblas_wtime();

    data->drotm.timings[0] += (helpTimeStop - helpTime);
    data->drotm.calls[0]++;

    return ;
}



extern void flexiblas_chain_drotmg (double* dd1, double* dd2, double* dx1, double* dy1, double* dparam);
void hook_drotmg(double* dd1, double* dd2, double* dx1, double* dy1, double* dparam)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_drotmg((void*) dd1, (void*) dd2, (void*) dx1, (void*) dy1, (void*) dparam);

    helpTimeStop = flexiblas_wtime();

    data->drotmg.timings[0] += (helpTimeStop - helpTime);
    data->drotmg.calls[0]++;

    return ;
}



extern void flexiblas_chain_dsbmv (char* uplo, Int * n, Int * k, double* alpha, double* a, Int * lda, double* x, Int * incx, double* beta, double* y, Int * incy);
void hook_dsbmv(char* uplo, Int * n, Int * k, double* alpha, double* a, Int * lda, double* x, Int * incx, double* beta, double* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dsbmv((void*) uplo, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->dsbmv.timings[0] += (helpTimeStop - helpTime);
    data->dsbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dscal (Int * n, double* da, double* dx, Int * incx);
void hook_dscal(Int * n, double* da, double* dx, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dscal((void*) n, (void*) da, (void*) dx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dscal.timings[0] += (helpTimeStop - helpTime);
    data->dscal.calls[0]++;

    return ;
}



extern double flexiblas_chain_dsdot (Int * n, float* sx, Int * incx, float* sy, Int * incy);
double hook_dsdot(Int * n, float* sx, Int * incx, float* sy, Int * incy)
{
    double v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_dsdot((void*) n, (void*) sx, (void*) incx, (void*) sy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->dsdot.timings[0] += (helpTimeStop - helpTime);
    data->dsdot.calls[0]++;

    return v;
}



extern void flexiblas_chain_dspmv (char* uplo, Int * n, double* alpha, double* ap, double* x, Int * incx, double* beta, double* y, Int * incy);
void hook_dspmv(char* uplo, Int * n, double* alpha, double* ap, double* x, Int * incx, double* beta, double* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dspmv((void*) uplo, (void*) n, (void*) alpha, (void*) ap, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->dspmv.timings[0] += (helpTimeStop - helpTime);
    data->dspmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dspr (char* uplo, Int * n, double* alpha, double* x, Int * incx, double* ap);
void hook_dspr(char* uplo, Int * n, double* alpha, double* x, Int * incx, double* ap)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dspr((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap);

    helpTimeStop = flexiblas_wtime();

    data->dspr.timings[0] += (helpTimeStop - helpTime);
    data->dspr.calls[0]++;

    return ;
}



extern void flexiblas_chain_dspr2 (char* uplo, Int * n, double* alpha, double* x, Int * incx, double* y, Int * incy, double* ap);
void hook_dspr2(char* uplo, Int * n, double* alpha, double* x, Int * incx, double* y, Int * incy, double* ap)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dspr2((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) ap);

    helpTimeStop = flexiblas_wtime();

    data->dspr2.timings[0] += (helpTimeStop - helpTime);
    data->dspr2.calls[0]++;

    return ;
}



extern void flexiblas_chain_dswap (Int * n, double* dx, Int * incx, double* dy, Int * incy);
void hook_dswap(Int * n, double* dx, Int * incx, double* dy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dswap((void*) n, (void*) dx, (void*) incx, (void*) dy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->dswap.timings[0] += (helpTimeStop - helpTime);
    data->dswap.calls[0]++;

    return ;
}



extern void flexiblas_chain_dsymm (char* side, char* uplo, Int * m, Int * n, double* alpha, double* a, Int * lda, double* b, Int * ldb, double* beta, double* c, Int * ldc);
void hook_dsymm(char* side, char* uplo, Int * m, Int * n, double* alpha, double* a, Int * lda, double* b, Int * ldb, double* beta, double* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dsymm((void*) side, (void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->dsymm.timings[0] += (helpTimeStop - helpTime);
    data->dsymm.calls[0]++;

    return ;
}



extern void flexiblas_chain_dsymv (char* uplo, Int * n, double* alpha, double* a, Int * lda, double* x, Int * incx, double* beta, double* y, Int * incy);
void hook_dsymv(char* uplo, Int * n, double* alpha, double* a, Int * lda, double* x, Int * incx, double* beta, double* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dsymv((void*) uplo, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->dsymv.timings[0] += (helpTimeStop - helpTime);
    data->dsymv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dsyr (char* uplo, Int * n, double* alpha, double* x, Int * incx, double* a, Int * lda);
void hook_dsyr(char* uplo, Int * n, double* alpha, double* x, Int * incx, double* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dsyr((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->dsyr.timings[0] += (helpTimeStop - helpTime);
    data->dsyr.calls[0]++;

    return ;
}



extern void flexiblas_chain_dsyr2 (char* uplo, Int * n, double* alpha, double* x, Int * incx, double* y, Int * incy, double* a, Int * lda);
void hook_dsyr2(char* uplo, Int * n, double* alpha, double* x, Int * incx, double* y, Int * incy, double* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dsyr2((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->dsyr2.timings[0] += (helpTimeStop - helpTime);
    data->dsyr2.calls[0]++;

    return ;
}



extern void flexiblas_chain_dsyr2k (char* uplo, char* trans, Int * n, Int * k, double* alpha, double* a, Int * lda, double* b, Int * ldb, double* beta, double* c, Int * ldc);
void hook_dsyr2k(char* uplo, char* trans, Int * n, Int * k, double* alpha, double* a, Int * lda, double* b, Int * ldb, double* beta, double* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dsyr2k((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->dsyr2k.timings[0] += (helpTimeStop - helpTime);
    data->dsyr2k.calls[0]++;

    return ;
}



extern void flexiblas_chain_dsyrk (char* uplo, char* trans, Int * n, Int * k, double* alpha, double* a, Int * lda, double* beta, double* c, Int * ldc);
void hook_dsyrk(char* uplo, char* trans, Int * n, Int * k, double* alpha, double* a, Int * lda, double* beta, double* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dsyrk((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->dsyrk.timings[0] += (helpTimeStop - helpTime);
    data->dsyrk.calls[0]++;

    return ;
}



extern void flexiblas_chain_dtbmv (char* uplo, char* trans, char* diag, Int * n, Int * k, double* a, Int * lda, double* x, Int * incx);
void hook_dtbmv(char* uplo, char* trans, char* diag, Int * n, Int * k, double* a, Int * lda, double* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dtbmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dtbmv.timings[0] += (helpTimeStop - helpTime);
    data->dtbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dtbsv (char* uplo, char* trans, char* diag, Int * n, Int * k, double* a, Int * lda, double* x, Int * incx);
void hook_dtbsv(char* uplo, char* trans, char* diag, Int * n, Int * k, double* a, Int * lda, double* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dtbsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dtbsv.timings[0] += (helpTimeStop - helpTime);
    data->dtbsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dtpmv (char* uplo, char* trans, char* diag, Int * n, double* ap, double* x, Int * incx);
void hook_dtpmv(char* uplo, char* trans, char* diag, Int * n, double* ap, double* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dtpmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) ap, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dtpmv.timings[0] += (helpTimeStop - helpTime);
    data->dtpmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dtpsv (char* uplo, char* trans, char* diag, Int * n, double* ap, double* x, Int * incx);
void hook_dtpsv(char* uplo, char* trans, char* diag, Int * n, double* ap, double* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dtpsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) ap, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dtpsv.timings[0] += (helpTimeStop - helpTime);
    data->dtpsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dtrmm (char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, double* alpha, double* a, Int * lda, double* b, Int * ldb);
void hook_dtrmm(char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, double* alpha, double* a, Int * lda, double* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dtrmm((void*) side, (void*) uplo, (void*) transa, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->dtrmm.timings[0] += (helpTimeStop - helpTime);
    data->dtrmm.calls[0]++;

    return ;
}



extern void flexiblas_chain_dtrmv (char* uplo, char* trans, char* diag, Int * n, double* a, Int * lda, double* x, Int * incx);
void hook_dtrmv(char* uplo, char* trans, char* diag, Int * n, double* a, Int * lda, double* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dtrmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dtrmv.timings[0] += (helpTimeStop - helpTime);
    data->dtrmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_dtrsm (char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, double* alpha, double* a, Int * lda, double* b, Int * ldb);
void hook_dtrsm(char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, double* alpha, double* a, Int * lda, double* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dtrsm((void*) side, (void*) uplo, (void*) transa, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->dtrsm.timings[0] += (helpTimeStop - helpTime);
    data->dtrsm.calls[0]++;

    return ;
}



extern void flexiblas_chain_dtrsv (char* uplo, char* trans, char* diag, Int * n, double* a, Int * lda, double* x, Int * incx);
void hook_dtrsv(char* uplo, char* trans, char* diag, Int * n, double* a, Int * lda, double* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dtrsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dtrsv.timings[0] += (helpTimeStop - helpTime);
    data->dtrsv.calls[0]++;

    return ;
}



extern double flexiblas_chain_dzasum (Int * n, double complex* zx, Int * incx);
double hook_dzasum(Int * n, double complex* zx, Int * incx)
{
    double v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_dzasum((void*) n, (void*) zx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dzasum.timings[0] += (helpTimeStop - helpTime);
    data->dzasum.calls[0]++;

    return v;
}



extern double flexiblas_chain_dznrm2 (Int * n, double complex* x, Int * incx);
double hook_dznrm2(Int * n, double complex* x, Int * incx)
{
    double v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_dznrm2((void*) n, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->dznrm2.timings[0] += (helpTimeStop - helpTime);
    data->dznrm2.calls[0]++;

    return v;
}



extern int flexiblas_chain_icamax (Int * n, float complex* cx, Int * incx);
int hook_icamax(Int * n, float complex* cx, Int * incx)
{
    int v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_icamax((void*) n, (void*) cx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->icamax.timings[0] += (helpTimeStop - helpTime);
    data->icamax.calls[0]++;

    return v;
}



extern int flexiblas_chain_idamax (Int * n, double* dx, Int * incx);
int hook_idamax(Int * n, double* dx, Int * incx)
{
    int v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_idamax((void*) n, (void*) dx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->idamax.timings[0] += (helpTimeStop - helpTime);
    data->idamax.calls[0]++;

    return v;
}



extern int flexiblas_chain_isamax (Int * n, float* sx, Int * incx);
int hook_isamax(Int * n, float* sx, Int * incx)
{
    int v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_isamax((void*) n, (void*) sx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->isamax.timings[0] += (helpTimeStop - helpTime);
    data->isamax.calls[0]++;

    return v;
}



extern int flexiblas_chain_izamax (Int * n, double complex* zx, Int * incx);
int hook_izamax(Int * n, double complex* zx, Int * incx)
{
    int v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_izamax((void*) n, (void*) zx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->izamax.timings[0] += (helpTimeStop - helpTime);
    data->izamax.calls[0]++;

    return v;
}



extern float flexiblas_chain_sasum (Int * n, float* sx, Int * incx);
float hook_sasum(Int * n, float* sx, Int * incx)
{
    float v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_sasum((void*) n, (void*) sx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->sasum.timings[0] += (helpTimeStop - helpTime);
    data->sasum.calls[0]++;

    return v;
}



extern void flexiblas_chain_saxpy (Int * n, float* sa, float* sx, Int * incx, float* sy, Int * incy);
void hook_saxpy(Int * n, float* sa, float* sx, Int * incx, float* sy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_saxpy((void*) n, (void*) sa, (void*) sx, (void*) incx, (void*) sy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->saxpy.timings[0] += (helpTimeStop - helpTime);
    data->saxpy.calls[0]++;

    return ;
}



extern float flexiblas_chain_scasum (Int * n, float complex* cx, Int * incx);
float hook_scasum(Int * n, float complex* cx, Int * incx)
{
    float v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_scasum((void*) n, (void*) cx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->scasum.timings[0] += (helpTimeStop - helpTime);
    data->scasum.calls[0]++;

    return v;
}



extern float flexiblas_chain_scnrm2 (Int * n, float complex* x, Int * incx);
float hook_scnrm2(Int * n, float complex* x, Int * incx)
{
    float v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_scnrm2((void*) n, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->scnrm2.timings[0] += (helpTimeStop - helpTime);
    data->scnrm2.calls[0]++;

    return v;
}



extern void flexiblas_chain_scopy (Int * n, float* sx, Int * incx, float* sy, Int * incy);
void hook_scopy(Int * n, float* sx, Int * incx, float* sy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_scopy((void*) n, (void*) sx, (void*) incx, (void*) sy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->scopy.timings[0] += (helpTimeStop - helpTime);
    data->scopy.calls[0]++;

    return ;
}



extern float flexiblas_chain_sdot (Int * n, float* sx, Int * incx, float* sy, Int * incy);
float hook_sdot(Int * n, float* sx, Int * incx, float* sy, Int * incy)
{
    float v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_sdot((void*) n, (void*) sx, (void*) incx, (void*) sy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->sdot.timings[0] += (helpTimeStop - helpTime);
    data->sdot.calls[0]++;

    return v;
}



extern float flexiblas_chain_sdsdot (Int * n, float* sb, float* sx, Int * incx, float* sy, Int * incy);
float hook_sdsdot(Int * n, float* sb, float* sx, Int * incx, float* sy, Int * incy)
{
    float v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_sdsdot((void*) n, (void*) sb, (void*) sx, (void*) incx, (void*) sy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->sdsdot.timings[0] += (helpTimeStop - helpTime);
    data->sdsdot.calls[0]++;

    return v;
}



extern void flexiblas_chain_sgbmv (char* trans, Int * m, Int * n, Int * kl, Int * ku, float* alpha, float* a, Int * lda, float* x, Int * incx, float* beta, float* y, Int * incy);
void hook_sgbmv(char* trans, Int * m, Int * n, Int * kl, Int * ku, float* alpha, float* a, Int * lda, float* x, Int * incx, float* beta, float* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sgbmv((void*) trans, (void*) m, (void*) n, (void*) kl, (void*) ku, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->sgbmv.timings[0] += (helpTimeStop - helpTime);
    data->sgbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_sgemm (char* transa, char* transb, Int * m, Int * n, Int * k, float* alpha, float* a, Int * lda, float* b, Int * ldb, float* beta, float* c, Int * ldc);
void hook_sgemm(char* transa, char* transb, Int * m, Int * n, Int * k, float* alpha, float* a, Int * lda, float* b, Int * ldb, float* beta, float* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sgemm((void*) transa, (void*) transb, (void*) m, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->sgemm.timings[0] += (helpTimeStop - helpTime);
    data->sgemm.calls[0]++;

    return ;
}



extern void flexiblas_chain_sgemv (char* trans, Int * m, Int * n, float* alpha, float* a, Int * lda, float* x, Int * incx, float* beta, float* y, Int * incy);
void hook_sgemv(char* trans, Int * m, Int * n, float* alpha, float* a, Int * lda, float* x, Int * incx, float* beta, float* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sgemv((void*) trans, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->sgemv.timings[0] += (helpTimeStop - helpTime);
    data->sgemv.calls[0]++;

    return ;
}



extern void flexiblas_chain_sger (Int * m, Int * n, float* alpha, float* x, Int * incx, float* y, Int * incy, float* a, Int * lda);
void hook_sger(Int * m, Int * n, float* alpha, float* x, Int * incx, float* y, Int * incy, float* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sger((void*) m, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->sger.timings[0] += (helpTimeStop - helpTime);
    data->sger.calls[0]++;

    return ;
}



extern float flexiblas_chain_snrm2 (Int * n, float* x, Int * incx);
float hook_snrm2(Int * n, float* x, Int * incx)
{
    float v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    v = flexiblas_chain_snrm2((void*) n, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->snrm2.timings[0] += (helpTimeStop - helpTime);
    data->snrm2.calls[0]++;

    return v;
}



extern void flexiblas_chain_srot (Int * n, float* sx, Int * incx, float* sy, Int * incy, float* c, float* s);
void hook_srot(Int * n, float* sx, Int * incx, float* sy, Int * incy, float* c, float* s)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_srot((void*) n, (void*) sx, (void*) incx, (void*) sy, (void*) incy, (void*) c, (void*) s);

    helpTimeStop = flexiblas_wtime();

    data->srot.timings[0] += (helpTimeStop - helpTime);
    data->srot.calls[0]++;

    return ;
}



extern void flexiblas_chain_srotg (float* sa, float* sb, float* c, float* s);
void hook_srotg(float* sa, float* sb, float* c, float* s)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_srotg((void*) sa, (void*) sb, (void*) c, (void*) s);

    helpTimeStop = flexiblas_wtime();

    data->srotg.timings[0] += (helpTimeStop - helpTime);
    data->srotg.calls[0]++;

    return ;
}



extern void flexiblas_chain_srotm (Int * n, float* sx, Int * incx, float* sy, Int * incy, float* sparam);
void hook_srotm(Int * n, float* sx, Int * incx, float* sy, Int * incy, float* sparam)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_srotm((void*) n, (void*) sx, (void*) incx, (void*) sy, (void*) incy, (void*) sparam);

    helpTimeStop = flexiblas_wtime();

    data->srotm.timings[0] += (helpTimeStop - helpTime);
    data->srotm.calls[0]++;

    return ;
}



extern void flexiblas_chain_srotmg (float* sd1, float* sd2, float* sx1, float* sy1, float* sparam);
void hook_srotmg(float* sd1, float* sd2, float* sx1, float* sy1, float* sparam)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_srotmg((void*) sd1, (void*) sd2, (void*) sx1, (void*) sy1, (void*) sparam);

    helpTimeStop = flexiblas_wtime();

    data->srotmg.timings[0] += (helpTimeStop - helpTime);
    data->srotmg.calls[0]++;

    return ;
}



extern void flexiblas_chain_ssbmv (char* uplo, Int * n, Int * k, float* alpha, float* a, Int * lda, float* x, Int * incx, float* beta, float* y, Int * incy);
void hook_ssbmv(char* uplo, Int * n, Int * k, float* alpha, float* a, Int * lda, float* x, Int * incx, float* beta, float* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ssbmv((void*) uplo, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->ssbmv.timings[0] += (helpTimeStop - helpTime);
    data->ssbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_sscal (Int * n, float* sa, float* sx, Int * incx);
void hook_sscal(Int * n, float* sa, float* sx, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sscal((void*) n, (void*) sa, (void*) sx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->sscal.timings[0] += (helpTimeStop - helpTime);
    data->sscal.calls[0]++;

    return ;
}



extern void flexiblas_chain_sspmv (char* uplo, Int * n, float* alpha, float* ap, float* x, Int * incx, float* beta, float* y, Int * incy);
void hook_sspmv(char* uplo, Int * n, float* alpha, float* ap, float* x, Int * incx, float* beta, float* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sspmv((void*) uplo, (void*) n, (void*) alpha, (void*) ap, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->sspmv.timings[0] += (helpTimeStop - helpTime);
    data->sspmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_sspr (char* uplo, Int * n, float* alpha, float* x, Int * incx, float* ap);
void hook_sspr(char* uplo, Int * n, float* alpha, float* x, Int * incx, float* ap)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sspr((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap);

    helpTimeStop = flexiblas_wtime();

    data->sspr.timings[0] += (helpTimeStop - helpTime);
    data->sspr.calls[0]++;

    return ;
}



extern void flexiblas_chain_sspr2 (char* uplo, Int * n, float* alpha, float* x, Int * incx, float* y, Int * incy, float* ap);
void hook_sspr2(char* uplo, Int * n, float* alpha, float* x, Int * incx, float* y, Int * incy, float* ap)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sspr2((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) ap);

    helpTimeStop = flexiblas_wtime();

    data->sspr2.timings[0] += (helpTimeStop - helpTime);
    data->sspr2.calls[0]++;

    return ;
}



extern void flexiblas_chain_sswap (Int * n, float* sx, Int * incx, float* sy, Int * incy);
void hook_sswap(Int * n, float* sx, Int * incx, float* sy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sswap((void*) n, (void*) sx, (void*) incx, (void*) sy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->sswap.timings[0] += (helpTimeStop - helpTime);
    data->sswap.calls[0]++;

    return ;
}



extern void flexiblas_chain_ssymm (char* side, char* uplo, Int * m, Int * n, float* alpha, float* a, Int * lda, float* b, Int * ldb, float* beta, float* c, Int * ldc);
void hook_ssymm(char* side, char* uplo, Int * m, Int * n, float* alpha, float* a, Int * lda, float* b, Int * ldb, float* beta, float* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ssymm((void*) side, (void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->ssymm.timings[0] += (helpTimeStop - helpTime);
    data->ssymm.calls[0]++;

    return ;
}



extern void flexiblas_chain_ssymv (char* uplo, Int * n, float* alpha, float* a, Int * lda, float* x, Int * incx, float* beta, float* y, Int * incy);
void hook_ssymv(char* uplo, Int * n, float* alpha, float* a, Int * lda, float* x, Int * incx, float* beta, float* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ssymv((void*) uplo, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->ssymv.timings[0] += (helpTimeStop - helpTime);
    data->ssymv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ssyr (char* uplo, Int * n, float* alpha, float* x, Int * incx, float* a, Int * lda);
void hook_ssyr(char* uplo, Int * n, float* alpha, float* x, Int * incx, float* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ssyr((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->ssyr.timings[0] += (helpTimeStop - helpTime);
    data->ssyr.calls[0]++;

    return ;
}



extern void flexiblas_chain_ssyr2 (char* uplo, Int * n, float* alpha, float* x, Int * incx, float* y, Int * incy, float* a, Int * lda);
void hook_ssyr2(char* uplo, Int * n, float* alpha, float* x, Int * incx, float* y, Int * incy, float* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ssyr2((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->ssyr2.timings[0] += (helpTimeStop - helpTime);
    data->ssyr2.calls[0]++;

    return ;
}



extern void flexiblas_chain_ssyr2k (char* uplo, char* trans, Int * n, Int * k, float* alpha, float* a, Int * lda, float* b, Int * ldb, float* beta, float* c, Int * ldc);
void hook_ssyr2k(char* uplo, char* trans, Int * n, Int * k, float* alpha, float* a, Int * lda, float* b, Int * ldb, float* beta, float* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ssyr2k((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->ssyr2k.timings[0] += (helpTimeStop - helpTime);
    data->ssyr2k.calls[0]++;

    return ;
}



extern void flexiblas_chain_ssyrk (char* uplo, char* trans, Int * n, Int * k, float* alpha, float* a, Int * lda, float* beta, float* c, Int * ldc);
void hook_ssyrk(char* uplo, char* trans, Int * n, Int * k, float* alpha, float* a, Int * lda, float* beta, float* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ssyrk((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->ssyrk.timings[0] += (helpTimeStop - helpTime);
    data->ssyrk.calls[0]++;

    return ;
}



extern void flexiblas_chain_stbmv (char* uplo, char* trans, char* diag, Int * n, Int * k, float* a, Int * lda, float* x, Int * incx);
void hook_stbmv(char* uplo, char* trans, char* diag, Int * n, Int * k, float* a, Int * lda, float* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_stbmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->stbmv.timings[0] += (helpTimeStop - helpTime);
    data->stbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_stbsv (char* uplo, char* trans, char* diag, Int * n, Int * k, float* a, Int * lda, float* x, Int * incx);
void hook_stbsv(char* uplo, char* trans, char* diag, Int * n, Int * k, float* a, Int * lda, float* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_stbsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->stbsv.timings[0] += (helpTimeStop - helpTime);
    data->stbsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_stpmv (char* uplo, char* trans, char* diag, Int * n, float* ap, float* x, Int * incx);
void hook_stpmv(char* uplo, char* trans, char* diag, Int * n, float* ap, float* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_stpmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) ap, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->stpmv.timings[0] += (helpTimeStop - helpTime);
    data->stpmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_stpsv (char* uplo, char* trans, char* diag, Int * n, float* ap, float* x, Int * incx);
void hook_stpsv(char* uplo, char* trans, char* diag, Int * n, float* ap, float* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_stpsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) ap, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->stpsv.timings[0] += (helpTimeStop - helpTime);
    data->stpsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_strmm (char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, float* alpha, float* a, Int * lda, float* b, Int * ldb);
void hook_strmm(char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, float* alpha, float* a, Int * lda, float* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_strmm((void*) side, (void*) uplo, (void*) transa, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->strmm.timings[0] += (helpTimeStop - helpTime);
    data->strmm.calls[0]++;

    return ;
}



extern void flexiblas_chain_strmv (char* uplo, char* trans, char* diag, Int * n, float* a, Int * lda, float* x, Int * incx);
void hook_strmv(char* uplo, char* trans, char* diag, Int * n, float* a, Int * lda, float* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_strmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->strmv.timings[0] += (helpTimeStop - helpTime);
    data->strmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_strsm (char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, float* alpha, float* a, Int * lda, float* b, Int * ldb);
void hook_strsm(char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, float* alpha, float* a, Int * lda, float* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_strsm((void*) side, (void*) uplo, (void*) transa, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->strsm.timings[0] += (helpTimeStop - helpTime);
    data->strsm.calls[0]++;

    return ;
}



extern void flexiblas_chain_strsv (char* uplo, char* trans, char* diag, Int * n, float* a, Int * lda, float* x, Int * incx);
void hook_strsv(char* uplo, char* trans, char* diag, Int * n, float* a, Int * lda, float* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_strsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->strsv.timings[0] += (helpTimeStop - helpTime);
    data->strsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_zaxpy (Int * n, double complex* za, double complex* zx, Int * incx, double complex* zy, Int * incy);
void hook_zaxpy(Int * n, double complex* za, double complex* zx, Int * incx, double complex* zy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zaxpy((void*) n, (void*) za, (void*) zx, (void*) incx, (void*) zy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zaxpy.timings[0] += (helpTimeStop - helpTime);
    data->zaxpy.calls[0]++;

    return ;
}



extern void flexiblas_chain_zcopy (Int * n, double complex* zx, Int * incx, double complex* zy, Int * incy);
void hook_zcopy(Int * n, double complex* zx, Int * incx, double complex* zy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zcopy((void*) n, (void*) zx, (void*) incx, (void*) zy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zcopy.timings[0] += (helpTimeStop - helpTime);
    data->zcopy.calls[0]++;

    return ;
}



extern double complex flexiblas_chain_zdotc (void *retvalue, Int * n, double complex* zx, Int * incx, double complex* zy, Int * incy);
double complex hook_zdotc(Int * n, double complex* zx, Int * incx, double complex* zy, Int * incy)
{
    double complex v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zdotc( (void*) &v , (void*) n, (void*) zx, (void*) incx, (void*) zy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zdotc.timings[0] += (helpTimeStop - helpTime);
    data->zdotc.calls[0]++;

    return v;
}



extern double complex flexiblas_chain_zdotu (void *retvalue, Int * n, double complex* zx, Int * incx, double complex* zy, Int * incy);
double complex hook_zdotu(Int * n, double complex* zx, Int * incx, double complex* zy, Int * incy)
{
    double complex v;
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zdotu( (void*) &v , (void*) n, (void*) zx, (void*) incx, (void*) zy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zdotu.timings[0] += (helpTimeStop - helpTime);
    data->zdotu.calls[0]++;

    return v;
}



extern void flexiblas_chain_zdrot (Int * n, double complex* cx, Int * incx, double complex* cy, Int * incy, double* c, double* s);
void hook_zdrot(Int * n, double complex* cx, Int * incx, double complex* cy, Int * incy, double* c, double* s)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zdrot((void*) n, (void*) cx, (void*) incx, (void*) cy, (void*) incy, (void*) c, (void*) s);

    helpTimeStop = flexiblas_wtime();

    data->zdrot.timings[0] += (helpTimeStop - helpTime);
    data->zdrot.calls[0]++;

    return ;
}



extern void flexiblas_chain_zdscal (Int * n, double* da, double complex* zx, Int * incx);
void hook_zdscal(Int * n, double* da, double complex* zx, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zdscal((void*) n, (void*) da, (void*) zx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->zdscal.timings[0] += (helpTimeStop - helpTime);
    data->zdscal.calls[0]++;

    return ;
}



extern void flexiblas_chain_zgbmv (char* trans, Int * m, Int * n, Int * kl, Int * ku, double complex* alpha, double complex* a, Int * lda, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy);
void hook_zgbmv(char* trans, Int * m, Int * n, Int * kl, Int * ku, double complex* alpha, double complex* a, Int * lda, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zgbmv((void*) trans, (void*) m, (void*) n, (void*) kl, (void*) ku, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zgbmv.timings[0] += (helpTimeStop - helpTime);
    data->zgbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_zgemm (char* transa, char* transb, Int * m, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double complex* beta, double complex* c, Int * ldc);
void hook_zgemm(char* transa, char* transb, Int * m, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double complex* beta, double complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zgemm((void*) transa, (void*) transb, (void*) m, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->zgemm.timings[0] += (helpTimeStop - helpTime);
    data->zgemm.calls[0]++;

    return ;
}



extern void flexiblas_chain_zgemv (char* trans, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy);
void hook_zgemv(char* trans, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zgemv((void*) trans, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zgemv.timings[0] += (helpTimeStop - helpTime);
    data->zgemv.calls[0]++;

    return ;
}



extern void flexiblas_chain_zgerc (Int * m, Int * n, double complex* alpha, double complex* x, Int * incx, double complex* y, Int * incy, double complex* a, Int * lda);
void hook_zgerc(Int * m, Int * n, double complex* alpha, double complex* x, Int * incx, double complex* y, Int * incy, double complex* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zgerc((void*) m, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->zgerc.timings[0] += (helpTimeStop - helpTime);
    data->zgerc.calls[0]++;

    return ;
}



extern void flexiblas_chain_zgeru (Int * m, Int * n, double complex* alpha, double complex* x, Int * incx, double complex* y, Int * incy, double complex* a, Int * lda);
void hook_zgeru(Int * m, Int * n, double complex* alpha, double complex* x, Int * incx, double complex* y, Int * incy, double complex* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zgeru((void*) m, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->zgeru.timings[0] += (helpTimeStop - helpTime);
    data->zgeru.calls[0]++;

    return ;
}



extern void flexiblas_chain_zhbmv (char* uplo, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy);
void hook_zhbmv(char* uplo, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zhbmv((void*) uplo, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zhbmv.timings[0] += (helpTimeStop - helpTime);
    data->zhbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_zhemm (char* side, char* uplo, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double complex* beta, double complex* c, Int * ldc);
void hook_zhemm(char* side, char* uplo, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double complex* beta, double complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zhemm((void*) side, (void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->zhemm.timings[0] += (helpTimeStop - helpTime);
    data->zhemm.calls[0]++;

    return ;
}



extern void flexiblas_chain_zhemv (char* uplo, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy);
void hook_zhemv(char* uplo, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zhemv((void*) uplo, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zhemv.timings[0] += (helpTimeStop - helpTime);
    data->zhemv.calls[0]++;

    return ;
}



extern void flexiblas_chain_zher (char* uplo, Int * n, double* alpha, double complex* x, Int * incx, double complex* a, Int * lda);
void hook_zher(char* uplo, Int * n, double* alpha, double complex* x, Int * incx, double complex* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zher((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->zher.timings[0] += (helpTimeStop - helpTime);
    data->zher.calls[0]++;

    return ;
}



extern void flexiblas_chain_zher2 (char* uplo, Int * n, double complex* alpha, double complex* x, Int * incx, double complex* y, Int * incy, double complex* a, Int * lda);
void hook_zher2(char* uplo, Int * n, double complex* alpha, double complex* x, Int * incx, double complex* y, Int * incy, double complex* a, Int * lda)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zher2((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) a, (void*) lda);

    helpTimeStop = flexiblas_wtime();

    data->zher2.timings[0] += (helpTimeStop - helpTime);
    data->zher2.calls[0]++;

    return ;
}



extern void flexiblas_chain_zher2k (char* uplo, char* trans, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double* beta, double complex* c, Int * ldc);
void hook_zher2k(char* uplo, char* trans, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double* beta, double complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zher2k((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->zher2k.timings[0] += (helpTimeStop - helpTime);
    data->zher2k.calls[0]++;

    return ;
}



extern void flexiblas_chain_zherk (char* uplo, char* trans, Int * n, Int * k, double* alpha, double complex* a, Int * lda, double* beta, double complex* c, Int * ldc);
void hook_zherk(char* uplo, char* trans, Int * n, Int * k, double* alpha, double complex* a, Int * lda, double* beta, double complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zherk((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->zherk.timings[0] += (helpTimeStop - helpTime);
    data->zherk.calls[0]++;

    return ;
}



extern void flexiblas_chain_zhpmv (char* uplo, Int * n, double complex* alpha, double complex* ap, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy);
void hook_zhpmv(char* uplo, Int * n, double complex* alpha, double complex* ap, double complex* x, Int * incx, double complex* beta, double complex* y, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zhpmv((void*) uplo, (void*) n, (void*) alpha, (void*) ap, (void*) x, (void*) incx, (void*) beta, (void*) y, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zhpmv.timings[0] += (helpTimeStop - helpTime);
    data->zhpmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_zhpr (char* uplo, Int * n, double* alpha, double complex* x, Int * incx, double complex* ap);
void hook_zhpr(char* uplo, Int * n, double* alpha, double complex* x, Int * incx, double complex* ap)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zhpr((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) ap);

    helpTimeStop = flexiblas_wtime();

    data->zhpr.timings[0] += (helpTimeStop - helpTime);
    data->zhpr.calls[0]++;

    return ;
}



extern void flexiblas_chain_zhpr2 (char* uplo, Int * n, double complex* alpha, double complex* x, Int * incx, double complex* y, Int * incy, double complex* ap);
void hook_zhpr2(char* uplo, Int * n, double complex* alpha, double complex* x, Int * incx, double complex* y, Int * incy, double complex* ap)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zhpr2((void*) uplo, (void*) n, (void*) alpha, (void*) x, (void*) incx, (void*) y, (void*) incy, (void*) ap);

    helpTimeStop = flexiblas_wtime();

    data->zhpr2.timings[0] += (helpTimeStop - helpTime);
    data->zhpr2.calls[0]++;

    return ;
}



extern void flexiblas_chain_zrotg (double complex* ca, double complex* cb, double* c, double complex* s);
void hook_zrotg(double complex* ca, double complex* cb, double* c, double complex* s)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zrotg((void*) ca, (void*) cb, (void*) c, (void*) s);

    helpTimeStop = flexiblas_wtime();

    data->zrotg.timings[0] += (helpTimeStop - helpTime);
    data->zrotg.calls[0]++;

    return ;
}



extern void flexiblas_chain_zscal (Int * n, double complex* za, double complex* zx, Int * incx);
void hook_zscal(Int * n, double complex* za, double complex* zx, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zscal((void*) n, (void*) za, (void*) zx, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->zscal.timings[0] += (helpTimeStop - helpTime);
    data->zscal.calls[0]++;

    return ;
}



extern void flexiblas_chain_zswap (Int * n, double complex* zx, Int * incx, double complex* zy, Int * incy);
void hook_zswap(Int * n, double complex* zx, Int * incx, double complex* zy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zswap((void*) n, (void*) zx, (void*) incx, (void*) zy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zswap.timings[0] += (helpTimeStop - helpTime);
    data->zswap.calls[0]++;

    return ;
}



extern void flexiblas_chain_zsymm (char* side, char* uplo, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double complex* beta, double complex* c, Int * ldc);
void hook_zsymm(char* side, char* uplo, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double complex* beta, double complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zsymm((void*) side, (void*) uplo, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->zsymm.timings[0] += (helpTimeStop - helpTime);
    data->zsymm.calls[0]++;

    return ;
}



extern void flexiblas_chain_zsyr2k (char* uplo, char* trans, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double complex* beta, double complex* c, Int * ldc);
void hook_zsyr2k(char* uplo, char* trans, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb, double complex* beta, double complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zsyr2k((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->zsyr2k.timings[0] += (helpTimeStop - helpTime);
    data->zsyr2k.calls[0]++;

    return ;
}



extern void flexiblas_chain_zsyrk (char* uplo, char* trans, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* beta, double complex* c, Int * ldc);
void hook_zsyrk(char* uplo, char* trans, Int * n, Int * k, double complex* alpha, double complex* a, Int * lda, double complex* beta, double complex* c, Int * ldc)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zsyrk((void*) uplo, (void*) trans, (void*) n, (void*) k, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) c, (void*) ldc);

    helpTimeStop = flexiblas_wtime();

    data->zsyrk.timings[0] += (helpTimeStop - helpTime);
    data->zsyrk.calls[0]++;

    return ;
}



extern void flexiblas_chain_ztbmv (char* uplo, char* trans, char* diag, Int * n, Int * k, double complex* a, Int * lda, double complex* x, Int * incx);
void hook_ztbmv(char* uplo, char* trans, char* diag, Int * n, Int * k, double complex* a, Int * lda, double complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ztbmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ztbmv.timings[0] += (helpTimeStop - helpTime);
    data->ztbmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ztbsv (char* uplo, char* trans, char* diag, Int * n, Int * k, double complex* a, Int * lda, double complex* x, Int * incx);
void hook_ztbsv(char* uplo, char* trans, char* diag, Int * n, Int * k, double complex* a, Int * lda, double complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ztbsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) k, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ztbsv.timings[0] += (helpTimeStop - helpTime);
    data->ztbsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ztpmv (char* uplo, char* trans, char* diag, Int * n, double complex* ap, double complex* x, Int * incx);
void hook_ztpmv(char* uplo, char* trans, char* diag, Int * n, double complex* ap, double complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ztpmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) ap, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ztpmv.timings[0] += (helpTimeStop - helpTime);
    data->ztpmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ztpsv (char* uplo, char* trans, char* diag, Int * n, double complex* ap, double complex* x, Int * incx);
void hook_ztpsv(char* uplo, char* trans, char* diag, Int * n, double complex* ap, double complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ztpsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) ap, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ztpsv.timings[0] += (helpTimeStop - helpTime);
    data->ztpsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ztrmm (char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb);
void hook_ztrmm(char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ztrmm((void*) side, (void*) uplo, (void*) transa, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->ztrmm.timings[0] += (helpTimeStop - helpTime);
    data->ztrmm.calls[0]++;

    return ;
}



extern void flexiblas_chain_ztrmv (char* uplo, char* trans, char* diag, Int * n, double complex* a, Int * lda, double complex* x, Int * incx);
void hook_ztrmv(char* uplo, char* trans, char* diag, Int * n, double complex* a, Int * lda, double complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ztrmv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ztrmv.timings[0] += (helpTimeStop - helpTime);
    data->ztrmv.calls[0]++;

    return ;
}



extern void flexiblas_chain_ztrsm (char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb);
void hook_ztrsm(char* side, char* uplo, char* transa, char* diag, Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ztrsm((void*) side, (void*) uplo, (void*) transa, (void*) diag, (void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->ztrsm.timings[0] += (helpTimeStop - helpTime);
    data->ztrsm.calls[0]++;

    return ;
}



extern void flexiblas_chain_ztrsv (char* uplo, char* trans, char* diag, Int * n, double complex* a, Int * lda, double complex* x, Int * incx);
void hook_ztrsv(char* uplo, char* trans, char* diag, Int * n, double complex* a, Int * lda, double complex* x, Int * incx)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_ztrsv((void*) uplo, (void*) trans, (void*) diag, (void*) n, (void*) a, (void*) lda, (void*) x, (void*) incx);

    helpTimeStop = flexiblas_wtime();

    data->ztrsv.timings[0] += (helpTimeStop - helpTime);
    data->ztrsv.calls[0]++;

    return ;
}



extern void flexiblas_chain_caxpby (Int * n, float complex* ca, float complex* cx, Int * incx, float complex* cb, float complex* cy, Int * incy);
void hook_caxpby(Int * n, float complex* ca, float complex* cx, Int * incx, float complex* cb, float complex* cy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_caxpby((void*) n, (void*) ca, (void*) cx, (void*) incx, (void*) cb, (void*) cy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->caxpby.timings[0] += (helpTimeStop - helpTime);
    data->caxpby.calls[0]++;

    return ;
}



extern void flexiblas_chain_daxpby (Int * n, double* da, double* dx, Int * incx, double* db, double* dy, Int * incy);
void hook_daxpby(Int * n, double* da, double* dx, Int * incx, double* db, double* dy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_daxpby((void*) n, (void*) da, (void*) dx, (void*) incx, (void*) db, (void*) dy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->daxpby.timings[0] += (helpTimeStop - helpTime);
    data->daxpby.calls[0]++;

    return ;
}



extern void flexiblas_chain_zaxpby (Int * n, double complex* za, double complex* zx, Int * incx, double complex* zb, double complex* zy, Int * incy);
void hook_zaxpby(Int * n, double complex* za, double complex* zx, Int * incx, double complex* zb, double complex* zy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zaxpby((void*) n, (void*) za, (void*) zx, (void*) incx, (void*) zb, (void*) zy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->zaxpby.timings[0] += (helpTimeStop - helpTime);
    data->zaxpby.calls[0]++;

    return ;
}



extern void flexiblas_chain_saxpby (Int * n, float* sa, float* sx, Int * incx, float* sb, float* sy, Int * incy);
void hook_saxpby(Int * n, float* sa, float* sx, Int * incx, float* sb, float* sy, Int * incy)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_saxpby((void*) n, (void*) sa, (void*) sx, (void*) incx, (void*) sb, (void*) sy, (void*) incy);

    helpTimeStop = flexiblas_wtime();

    data->saxpby.timings[0] += (helpTimeStop - helpTime);
    data->saxpby.calls[0]++;

    return ;
}



extern void flexiblas_chain_comatcopy (char* order, char* trans, Int * rows, Int * cols, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb);
void hook_comatcopy(char* order, char* trans, Int * rows, Int * cols, float complex* alpha, float complex* a, Int * lda, float complex* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_comatcopy((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->comatcopy.timings[0] += (helpTimeStop - helpTime);
    data->comatcopy.calls[0]++;

    return ;
}



extern void flexiblas_chain_zomatcopy (char* order, char* trans, Int * rows, Int * cols, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb);
void hook_zomatcopy(char* order, char* trans, Int * rows, Int * cols, double complex* alpha, double complex* a, Int * lda, double complex* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zomatcopy((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->zomatcopy.timings[0] += (helpTimeStop - helpTime);
    data->zomatcopy.calls[0]++;

    return ;
}



extern void flexiblas_chain_domatcopy (char* order, char* trans, Int * rows, Int * cols, double* alpha, double* a, Int * lda, double* b, Int * ldb);
void hook_domatcopy(char* order, char* trans, Int * rows, Int * cols, double* alpha, double* a, Int * lda, double* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_domatcopy((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->domatcopy.timings[0] += (helpTimeStop - helpTime);
    data->domatcopy.calls[0]++;

    return ;
}



extern void flexiblas_chain_somatcopy (char* order, char* trans, Int * rows, Int * cols, float* alpha, float* a, Int * lda, float* b, Int * ldb);
void hook_somatcopy(char* order, char* trans, Int * rows, Int * cols, float* alpha, float* a, Int * lda, float* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_somatcopy((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->somatcopy.timings[0] += (helpTimeStop - helpTime);
    data->somatcopy.calls[0]++;

    return ;
}



extern void flexiblas_chain_cimatcopy (char* order, char* trans, Int * rows, Int * cols, float complex* alpha, float complex* a, Int * lda, Int * ldb);
void hook_cimatcopy(char* order, char* trans, Int * rows, Int * cols, float complex* alpha, float complex* a, Int * lda, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cimatcopy((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->cimatcopy.timings[0] += (helpTimeStop - helpTime);
    data->cimatcopy.calls[0]++;

    return ;
}



extern void flexiblas_chain_zimatcopy (char* order, char* trans, Int * rows, Int * cols, double complex* alpha, double complex* a, Int * lda, Int * ldb);
void hook_zimatcopy(char* order, char* trans, Int * rows, Int * cols, double complex* alpha, double complex* a, Int * lda, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zimatcopy((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->zimatcopy.timings[0] += (helpTimeStop - helpTime);
    data->zimatcopy.calls[0]++;

    return ;
}



extern void flexiblas_chain_dimatcopy (char* order, char* trans, Int * rows, Int * cols, double* alpha, double* a, Int * lda, Int * ldb);
void hook_dimatcopy(char* order, char* trans, Int * rows, Int * cols, double* alpha, double* a, Int * lda, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dimatcopy((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->dimatcopy.timings[0] += (helpTimeStop - helpTime);
    data->dimatcopy.calls[0]++;

    return ;
}



extern void flexiblas_chain_simatcopy (char* order, char* trans, Int * rows, Int * cols, float* alpha, float* a, Int * lda, Int * ldb);
void hook_simatcopy(char* order, char* trans, Int * rows, Int * cols, float* alpha, float* a, Int * lda, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_simatcopy((void*) order, (void*) trans, (void*) rows, (void*) cols, (void*) alpha, (void*) a, (void*) lda, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->simatcopy.timings[0] += (helpTimeStop - helpTime);
    data->simatcopy.calls[0]++;

    return ;
}



extern void flexiblas_chain_sgeadd (Int * m, Int * n, float* alpha, float* a, Int * lda, float* beta, float* b, Int * ldb);
void hook_sgeadd(Int * m, Int * n, float* alpha, float* a, Int * lda, float* beta, float* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_sgeadd((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->sgeadd.timings[0] += (helpTimeStop - helpTime);
    data->sgeadd.calls[0]++;

    return ;
}



extern void flexiblas_chain_dgeadd (Int * m, Int * n, double* alpha, double* a, Int * lda, double* beta, double* b, Int * ldb);
void hook_dgeadd(Int * m, Int * n, double* alpha, double* a, Int * lda, double* beta, double* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_dgeadd((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->dgeadd.timings[0] += (helpTimeStop - helpTime);
    data->dgeadd.calls[0]++;

    return ;
}



extern void flexiblas_chain_cgeadd (Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* beta, float complex* b, Int * ldb);
void hook_cgeadd(Int * m, Int * n, float complex* alpha, float complex* a, Int * lda, float complex* beta, float complex* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_cgeadd((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->cgeadd.timings[0] += (helpTimeStop - helpTime);
    data->cgeadd.calls[0]++;

    return ;
}



extern void flexiblas_chain_zgeadd (Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* beta, double complex* b, Int * ldb);
void hook_zgeadd(Int * m, Int * n, double complex* alpha, double complex* a, Int * lda, double complex* beta, double complex* b, Int * ldb)
{
    
    double helpTime;
    double helpTimeStop;

    helpTime = flexiblas_wtime();

    flexiblas_chain_zgeadd((void*) m, (void*) n, (void*) alpha, (void*) a, (void*) lda, (void*) beta, (void*) b, (void*) ldb);

    helpTimeStop = flexiblas_wtime();

    data->zgeadd.timings[0] += (helpTimeStop - helpTime);
    data->zgeadd.calls[0]++;

    return ;
}


