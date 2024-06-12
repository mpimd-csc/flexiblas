! SPDX-License-Identifier: LGPL-3.0-or-later
!
! This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
! Copyright (C) 2013-2024 Martin Koehler
!
! This program is free software: you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by the Free
! Software Foundation, either version 3 of the License, or (at your option)
! any later version.
!
! This program is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
! FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
! more details.
!
! You should have received a copy of the GNU General Public License along
! with this program. If not, see <https://www.gnu.org/licenses/>.



SUBROUTINE ZAXPBY(N,ZA,ZX,INCX,ZB,ZY,INCY)
    !     .. Scalar Arguments ..
    DOUBLE COMPLEX ZA,ZB
    INTEGER INCX,INCY,N
    !     ..
    !     .. Array Arguments ..
    DOUBLE COMPLEX ZX(*),ZY(*)
    !     ..
    !
    !  Purpose
    !  =======
    !
    !     ZAXPY constant times a vector plus constant times a vector.
    !
    !  Further Details
    !  ===============
    !
    !     jack dongarra, linpack, 3/11/78.
    !     Martin Koehler, FlexiBLAS 9/5/2014
    !     modified 12/3/93, array(1) declarations changed to array(*)
    !     modified 09/05/2014, axpy -> axpby
    !
    !  =====================================================================
    !
    !     .. Local Scalars ..
    INTEGER I,IX,IY
    !     ..
    !     .. External Functions ..
    DOUBLE PRECISION DCABS1
    EXTERNAL DCABS1
    !     ..
    IF (N.LE.0) RETURN
    IF (DCABS1(ZA).EQ.0.0d0 .AND. DCABS1(ZB).EQ.0.0d0) RETURN
    IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
        !
        !        code for both increments equal to 1
        !
        DO I = 1,N
        ZY(I) = ZB * ZY(I) + ZA*ZX(I)
        END DO
    ELSE
        !
        !        code for unequal increments or equal increments
        !          not equal to 1
        !
        IX = 1
        IY = 1
        IF (INCX.LT.0) IX = (-N+1)*INCX + 1
        IF (INCY.LT.0) IY = (-N+1)*INCY + 1
        DO I = 1,N
        ZY(IY) = ZB * ZY(IY) + ZA*ZX(IX)
        IX = IX + INCX
        IY = IY + INCY
        END DO
    END IF
    !
    RETURN
END SUBROUTINE


