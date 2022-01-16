## Copyright (C) 2015-2020 Martin Koehler
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

## -*- texinfo -*- 
## @deftypefn {[@var{peak}, @var{name}] =} flexiblas_gemm_benchmark (@var{n}, @var{runs}, @var{threads}, @var{ignore_list})
## The flexiblas_gemm_benchmark function benchmarks all available BLAS
## backends (except of NETLIB and __FALLBACK__) using the matrix-matrix product.
## It ## returns the name of the backend and the corresponding peakperformance
## in GFlops.
## The @var{ignore_list} parameter is a cell array used to override the default
## ignore list. 
## @end deftypefn

## Author: Martin Koehler <koehlerm@mpi-magdeburg.mpg.de>
## Created: 2020-06-23

function [peak, name] = flexiblas_gemm_benchmark (n, runs, threads, ignore_list)

A = rand(n);
B = rand(n);
C = zeros(n);

if (nargin < 4 )
    ignore_list = cell(2,1);
    ignore_list{1}='NETLIB';
    ignore_list{2}='__FALLBACK__';
end


backend_names = flexiblas_list();
nbackends = size(backend_names,1);

timings = zeros(nbackends,1);
performance = zeros(nbackends,1);

%% Load backends
backends = zeros(nbackends,1);
jj = 1;
for i = 1:nbackends
    skip = false;
 for j = 1: length(ignore_list)
     s1 = deblank(backend_names(i,:));
     s2 = ignore_list{j};
     eq =strcmp(s1, s2);
     if ( eq > 0 ) 
         skip = true;
     end
 end
 if skip
     continue;
 end
 backends(jj) =  flexiblas_load_backend(backend_names(i,:));
 backend_names2(jj,:) = backend_names(i,:);
 if ( backends(jj) < 0 )
   fprintf(1, 'Failed to load %s\n', backend_names(i,:));
   error('Failed.');
 end
 jj = jj + 1;
end
nbackends = jj -1;

%% Benchmark 
for i = 1:nbackends
  flexiblas_switch(backends(i));
  flexiblas_set_num_threads(threads);
  
  % Warmup 
  x = rand(100)*rand(100);
  clear x;

  tic;
  for r = 1:runs
    C = A*B;
  end
  timings(i) = toc/runs;
  performance(i) = (2*(n/1000.0)^3)/timings(i);
end 

fprintf('\nFinal Results\n');
fprintf('Time \t GFlops \n');
for i = 1:nbackends
  fprintf('%10g\t %10g \t%s\n', timings(i), performance(i),backend_names2(i,:));
end 
[peak, idx] = max(performance);
name = backend_names(idx,:);

endfunction
