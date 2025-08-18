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



SUBROUTINE DGEADD(M, N, ALPHA, A, LDA, BETA, B, LDB)
    ! B:= alpha * A + beta *B
    IMPLICIT NONE
    INTEGER M, N, LDA, LDB
    DOUBLE PRECISION ALPHA, BETA
    DOUBLE PRECISION A(LDA,*), B(LDB, *)

    ! Locals
    INTEGER COL
    EXTERNAL DAXPBY, XERBLA
    INTRINSIC MAX

    IF (M .LE. 0 ) RETURN
    IF (N .LE. 0 ) RETURN
    IF (MAX(1,LDA) .LT. M) THEN
        CALL XERBLA("DGEADD", 5)
    ENDIF
    IF (MAX(1,LDB) .LT. M) THEN
        CALL XERBLA("DGEADD", 8)
    ENDIF

    DO COL = 1, N
    B(1:M, COL) = BETA * B(1:M, COL) + ALPHA * A(1:M, COL)
    END DO

END SUBROUTINE

