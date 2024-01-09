# -*- coding: utf-8 -*-
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


def dt_translator(fortran_type):
    D = {   "integer": "int",
            "real":    "float",
            "double precision": "double",
            "complex8": "float complex",
            "complex16": "double complex",
            "double complex": "double complex",
            "character": "char",
            "logical": "int"
            }
    if fortran_type["typespec"] == "complex":
        if "kindselector" in  fortran_type:
            kind = fortran_type["kindselector"].get("*", 8)
        else:
            kind = 8
        datatype="complex"+str(kind)
    else:
        datatype = fortran_type["typespec"]
    # print (fortran_type, datatype)
    return D[datatype]

def is_complex(t):
    cpx=set(["complex", "complex8", "complex16", "double complex"])
    if t in cpx:
        return True
    else:
        return False

class FortranFunction(object):
    """docstring for FortranFunction"""
    def __init__(self, func):
        self._name = func["name"]
        self._args = func["args"]
        self._fvars = func["vars"]
        self._cvars = {}
        self._array_args = {}
        self._returntype =""
        self._charaterargs = list()
        if func["block"] == "subroutine":
            self._subroutine = True
            self._function   = False
        elif func["block"] == "function":
            self._subroutine = False
            self._function   = True
        else:
            raise Exception("Block is wether SUBROUTINE nor a FUNCTION")

        self.__init2()

    def __init2(self):
        # Transfer the Datatypes to C
        for i in self._args:
            self._cvars[i] = dt_translator(self._fvars[i])
        for i in self._args:
            var = self._fvars[i]
            if "dimension" in var:
                self._array_args[i] = var["dimension"]
            if "charselector" in var:
                self._charaterargs.append("len_"+i)

        if self._subroutine:
            self._returntype = "void"
        else:
            self._returntype = dt_translator(self._fvars[self._name])
    def return_type(self):
        return self._returntype
    def function_name(self):
        return self._name

    def arg_list(self, void = False ):
        first = True
        s = ""
        if len(self._args) == 0:
            return "void"
        for i in self._args:
            if not void:
                if self._cvars[i] == "int":
                    cdt = "Int "
                else:
                    cdt = self._cvars[i]
            else:
                cdt = "void"
            if first:
                s  += cdt + "* " + i
                first = False
            else:
                s += ", " + cdt + "* " + i
        for i in self._charaterargs:
            s += ", int " + i

        return s
    def declare_return_type(self):
        if self._returntype == "void":
            return ""
        else:
            return self._returntype + " v;"
    def chain_complex(self):
        if self._function:
            if is_complex(self._fvars[self._name]["typespec"]):
                return "void *retvalue, "
        return ""

    def function_call(self):
        s = ""
        if self._function:
            if is_complex(self._fvars[self._name]["typespec"]):
                first = True
                s += "flexiblas_chain_"+self.function_name()+"( (void*) &v "
                for i in self._args:
                    s += ", "
                    s += "(void*) " + i
                for i in self._charaterargs:
                    s += ", (int) " + i
                s += ")"
            else:
                s += "v = flexiblas_chain_"+self.function_name()+"("
                first = True
                for i in self._args:
                    if not first:
                        s += ", "
                    first = False
                    s += "(void*) " + i
                for i in self._charaterargs:
                    s += ", (int) " + i
                s += ")"
        else:
            s += "flexiblas_chain_"+self.function_name()+"("
            first = True
            for i in self._args:
                if not first:
                    s += ", "
                first = False
                s += "(void*) " + i
            for i in self._charaterargs:
                    s += ", (int) " + i
            s += ")"
        return s
    def return_value(self):
        if self._function:
            return "v"
        else:
            return ""

