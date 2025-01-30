 --   SPDX-License-Identifier: LGPL-3.0-or-later
 --
 -- This file is part of libcscutils, a set of helper function.
 -- Copyright (C) 2013-2024 Martin Koehler
 --
 -- This program is free software; you can redistribute it and/or
 -- modify it under the terms of the GNU Lesser General Public
 -- License as published by the Free Software Foundation; either
 -- version 3 of the License, or (at your option) any later version.
 --
 -- This program is distributed in the hope that it will be useful,
 -- but WITHOUT ANY WARRANTY; without even the implied warranty of
 -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 -- Lesser General Public License for more details.
 --
 -- You should have received a copy of the GNU Lesser General Public License
 -- along with this program; if not, write to the Free Software Foundation,
 -- Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 --




print(testint)
print(testnumber)
print(teststring)

function arg0ret0()
	print("Inside function arg0ret0\n");
	print(teststring);
end


function arg0reti()
	return 12
end

function arg0retii()
	return 1,2
end

function argireti(m)
	return m*2;
end

function argiireti(m, n)
	return m * n;
end

function argiiireti(m, n, k)
	return m * n * k;
end

function argsiireti(s, m, n)
	print(s)
	return m*n
end


