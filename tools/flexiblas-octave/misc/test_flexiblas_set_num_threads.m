## Copyright (C) 2015, 2016, 2017 Martin Köhler
## 
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.


## Author: Martin Köhler
## Created: 2015-08-21


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



