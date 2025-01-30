!   SPDX-License-Identifier: LGPL-3.0-or-later
!
! This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
! Copyright (C) 2013-2024 Martin Koehler
!
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3 of the License, or (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this program; if not, write to the Free Software Foundation,
! Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
!



SUBROUTINE CAXPBY(N,CA,CX,INCX,CB,CY,INCY)
    IMPLICIT NONE
    !     .. Scalar Arguments ..
    COMPLEX CA,CB
    INTEGER INCX,INCY,N
    !     ..
    !     .. Array Arguments ..
    COMPLEX CX(*),CY(*)
    !     ..
    !
    !  Purpose
    !  =======
    !
    !     CAXPBY constant times a vector plus a constant times a vector.
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
    INTEGER I,IX,IY, IONE
    PARAMETER (IONE = 1)
    !     ..
    !     .. External Functions ..
    REAL SCABS1
    EXTERNAL SCABS1
    !     ..
    IF (N.LE.0) RETURN
    IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
        !
        !        code for both increments equal to 1
        !
        DO I = 1,N
        CY(I) = CB*CY(I) + CA*CX(I)
        END DO
    ELSE
        !
        !        code for unequal increments or equal increments
        !          not equal to 1
        !
        IX = 1
        IY = 1
        IF (INCX.LT.0) IX = (-N+IONE)*INCX + IONE
        IF (INCY.LT.0) IY = (-N+IONE)*INCY + IONE
        DO I = 1,N
        CY(IY) = CB*CY(IY) + CA*CX(IX)
        IX = IX + INCX
        IY = IY + INCY
        END DO
    END IF
    !
    RETURN
END SUBROUTINE


