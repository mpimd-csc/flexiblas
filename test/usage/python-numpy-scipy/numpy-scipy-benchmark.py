#!/usr/bin/env python
# -*- coding: UTF-8 -*-
#
# SPDX-License-Identifier: LGPL-3.0-or-later
#
# This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
# Copyright (C) 2013-2024 Martin Koehler
#
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <https://www.gnu.org/licenses/>.
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

