
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



SUBROUTINE CGEADD(M, N, ALPHA, A, LDA, BETA, B, LDB)
    ! B:= alpha * A + beta *B
    IMPLICIT NONE
    INTEGER M, N, LDA, LDB
    COMPLEX ALPHA, BETA
    COMPLEX A(LDA,*), B(LDB, *)

    ! Locals
    INTEGER COL
    EXTERNAL CAXPBY, XERBLA
    INTRINSIC MAX

    IF (M .LE. 0 ) RETURN
    IF (N .LE. 0 ) RETURN
    IF (MAX(1,LDA) .LT. M) THEN
        CALL XERBLA("CGEADD", 5)
    ENDIF
    IF (MAX(1,LDB) .LT. M) THEN
        CALL XERBLA("CGEADD", 8)
    ENDIF

    DO COL = 1, N
    B(1:M, COL) = BETA * B(1:M, COL) + ALPHA * A(1:M, COL)
    END DO

END SUBROUTINE

