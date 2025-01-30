##   SPDX-License-Identifier: LGPL-3.0-or-later
##
## This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
## Copyright (C) 2013-2024 Martin Koehler
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public
## License as published by the Free Software Foundation; either
## version 3 of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this program; if not, write to the Free Software Foundation,
## Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
##

max_threads = 16;
n =  2000;
runs = 10;
A = rand(n,n);
B = rand(n,n);
for i = 1:max_threads
  fprintf('Set number of threads to: %d\n', i);
  flexiblas_set_num_threads(i)
  tic
  for j=1:runs
    C=A*B;
  end
  t = toc;
  t = t / runs;
  fprintf('--> Time: %g\n', t);
  fflush(stdout)
end



