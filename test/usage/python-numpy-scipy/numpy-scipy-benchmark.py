#!/usr/bin/env python
# -*- coding: UTF-8 -*-
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Linking FlexiBLAS statically or dynamically with other modules is making a
# combined work based on FlexiBLAS. Thus, the terms and conditions of the GNU
# General Public License cover the whole combination.
#
# As a special exception, the copyright holders of FlexiBLAS give you permission
# to combine FlexiBLAS program with free software programs or libraries that are
# released under the GNU LGPL and with independent modules that communicate with
# FlexiBLAS solely through the BLAS/LAPACK interface as provided by the
# BLAS/LAPACK reference implementation. You may copy and distribute such a system
# following the terms of the GNU GPL for FlexiBLAS and the licenses of the other
# code concerned, provided that you include the source code of that other code
# when and as the GNU GPL requires distribution of source code and provided that
# you do not modify the BLAS/LAPACK interface.
#
# Note that people who make modified versions of FlexiBLAS are not obligated to
# grant this special exception for their modified versions; it is their choice
# whether to do so. The GNU General Public License gives permission to release a
# modified version without this exception; this exception also makes it possible
# to release a modified version which carries forward this exception. If you
# modify the BLAS/LAPACK interface, this exception does not apply to your
# modified version of FlexiBLAS, and you must remove this exception when you
# distribute your modified version.
#
# This exception is an additional permission under section 7 of the GNU General
# Public License, version 3 (“GPLv3”)
#
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.
#
# Copyright (C) Martin Koehler, 2013-2020
#

# Roughly based on: http://stackoverflow.com/questions/11443302/compiling-numpy-with-openblas-integration

import numpy as np
from scipy import linalg
from time import time

# Let's take the randomness out of random numbers (for reproducibility)
np.random.seed(0)

size = 4096
A, B = np.random.random((size, size)), np.random.random((size, size))
C, D = np.random.random((size * 128,)), np.random.random((size * 128,))
E = np.random.random((int(size / 2), int(size / 4)))
F = np.random.random((int(size / 2), int(size / 2)))
F = np.dot(F, F.T)
G = np.random.random((int(size / 2), int(size / 2)))

# Matrix multiplication
N = 2
t = time()
for i in range(N):
    np.dot(A, B)
delta = time() - t
print('Dotted two %dx%d matrices in %0.2f s.' % (size, size, delta / N))
del A, B

# Vector multiplication
N = 5000
t = time()
for i in range(N):
    np.dot(C, D)
delta = time() - t
print('Dotted two vectors of length %d in %0.2f ms.' % (size * 128, 1e3 * delta / N))
del C, D

# Singular Value Decomposition (SVD)
N = 2
t = time()
for i in range(N):
    np.linalg.svd(E, full_matrices = False)
delta = time() - t
print("SVD of a %dx%d matrix in %0.2f s." % (size / 2, size / 4, delta / N))

N = 2
t = time()
for i in range(N):
    linalg.svd(E, full_matrices = False)
delta = time() - t
print("SVD (SciPy) of a %dx%d matrix in %0.2f s." % (size / 2, size / 4, delta / N))
del E

# Cholesky Decomposition
N = 2
t = time()
for i in range(N):
    np.linalg.cholesky(F)
delta = time() - t
print("Cholesky decomposition of a %dx%d matrix in %0.2f s." % (size / 2, size / 2, delta / N))

N = 2
t = time()
for i in range(N):
    linalg.cholesky(F)
delta = time() - t
print("Cholesky decomposition (SciPy) of a %dx%d matrix in %0.2f s." % (size / 2, size / 2, delta / N))

# Eigendecomposition
t = time()
for i in range(N):
    np.linalg.eig(G)
delta = time() - t
print("Eigendecomposition of a %dx%d matrix in %0.2f s." % (size / 2, size / 2, delta / N))

t = time()
for i in range(N):
    linalg.eig(G)
delta = time() - t
print("Eigendecomposition (SciPy) of a %dx%d matrix in %0.2f s." % (size / 2, size / 2, delta / N))

print('')
print('This was obtained using the following Numpy configuration:')
np.__config__.show()

