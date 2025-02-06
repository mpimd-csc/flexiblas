! SPDX-License-Identifier: LGPL-3.0-or-later
!    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
!    Copyright (C) 2013-2024 Martin Koehler
!
!    This program is free software; you can redistribute it and/or
!    modify it under the terms of the GNU Lesser General Public
!    License as published by the Free Software Foundation; either
!    version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    Lesser General Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public License
!    along with this program; if not, write to the Free Software Foundation,
!    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
PROGRAM TEST
    IMPLICIT NONE
    DOUBLE COMPLEX A(4), B(4)
    INTEGER ONE, N
    DOUBLE COMPLEX R

    EXTERNAL ZDOTC
    INTRINSIC DCMPLX
    DOUBLE COMPLEX ZDOTC

    A(1) = DCMPLX(1d0,1d0)
    A(2) = DCMPLX(2d0,1d0)
    A(3) = DCMPLX(3d0,0)
    A(4) = DCMPLX(4d0,0)
    B(1) = DCMPLX(1d0,0)
    B(2) = DCMPLX(1d0,0)
    B(3) = DCMPLX(1d0,0)
    B(4) = DCMPLX(1d0,0)

    N = 4
    ONE = 1

    R = ZDOTC(N, A, ONE, B, ONE)

    WRITE(*,*) "A^HB = ",  R

END PROGRAM
