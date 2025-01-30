# -*- coding: utf-8 -*-
#   SPDX-License-Identifier: LGPL-3.0-or-later
#
# This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
# Copyright (C) 2013-2024 Martin Koehler
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#




import os, sys, json

from collections import OrderedDict
from datetime import datetime

def dt_translator(fortran_type):
    D = {   "integer": "int",
            "real":    "float",
            "double precision": "double",
            "complex8": "float complex",
            "complex16": "double complex",
            "double complex": "double complex",
            "character": "char",
            "logical": "blaslogical"
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

class WrapperConfig(object):
    def __init__(self, int32 = True, int64 = False, intel_interface = False):
        self.int32 = int32
        self.int64 = int64
        self.intel = intel_interface

class FortranFunction(object):
    """docstring for FortranFunction"""
    def __init__(self, func):
        self._name = func["name"]
        self._args = func["args"]
        self._fvars = func["vars"]
        self._cvars = {}
        self._array_args = {}
        self._returntype =""
        self._character_args = list()
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
            if var["typespec"] == "character":
                self._character_args.append("len_"+i)

        if self._subroutine:
            self._returntype = "void"
        else:
            self._returntype = dt_translator(self._fvars[self._name])


    def funcname(self, intwidth = 0, upper_case = False):
        if upper_case:
            if intwidth == 64 :
                s = self._name.upper() + "64"
            elif intwidth == 32:
                s = self._name.upper() + "32"
            else:
                s = self._name.upper()
        else:
            if intwidth == 64 :
                s = self._name.lower() + "64"
            elif intwidth == 32:
                s = self._name.lower() + "32"
            else:
                s = self._name.lower()
        return s

    def funcnamex(self, intwidth = 0, number=""):
        if intwidth == 64 :
            s = self._name.lower() + "64"
        elif intwidth == 32:
            s = self._name.lower() + "32"
        else:
            s = self._name.lower()
        if "_" in self._name:
            s = "FC_GLOBAL"+number+"_("+s+","+s.upper()+")"
        else:
            s = "FC_GLOBAL"+number+"("+s+","+s.upper()+")"
        return s


    def typename(self):
        return "call_" + self._name.lower()

    def enum(self):
        return "F_"+self._name.upper()

    def _inttype(self, intwidth = 0):
        if intwidth == 0:
            int_type ="blasint"
        elif intwidth == 32:
            int_type = "int32_t"
        elif intwidth == 64:
            int_type = "int64_t"
        return int_type

    def _callsequence(self, intwidth = 0, void = False, call = False, typecast = False ):
        first = True
        s = ""
        if len(self._args) == 0:
            if call:
                return ""
            else:
                return "void"
        for i in self._args:
            if not void:
                if self._cvars[i] == "int":
                    cdt = self._inttype(intwidth)
                else:
                    cdt = self._cvars[i]
            else:
                cdt = "void"
            if first:
                if typecast:
                    s  += "("+cdt + "*) " + i
                else:
                    s  += cdt + "* " + i
                first = False
            else:
                if typecast:
                    s += ", (" + cdt + "*) " + i
                else:
                    s += ", " + cdt + "* " + i
        for i in self._character_args:
            if typecast:
                s +=  ", (flexiblas_fortran_charlen_t) " + i
            else:
                s += ", flexiblas_fortran_charlen_t " + i
        return s

    def _callfunction(self, transform_int_name = True, extraspace="", intwidth = 0 ):
        s = ""
        if self._function:
            if is_complex(self._fvars[self._name]["typespec"]):
                s += "if(current_backend->info.intel_interface == 0 ) {\n"
                s += "    ret = fn("
                first = True
                for i in self._args:
                    if not first:
                        s += ", "
                    first = False
                    if self._cvars[i] == "int" and transform_int_name:
                        s += "(void*) _p{name:s}".format(name = i)
                    else:
                        s += "(void*) " + i
                for i in self._character_args:
                    s+= ", ( flexiblas_fortran_charlen_t ) " + i
                s += ");\n"
                s += "} else {\n"
                s += "    fn_intel( &ret"
                for i in self._args:
                    s += ", "
                    if self._cvars[i] == "int" and transform_int_name:
                        s += "(void*) _p{name:s}".format(name = i)
                    else:
                        s += "(void*) " + i
                s += ");\n"
                s += "}\n"
            else:
                s += "ret = fn("
                first = True
                for i in self._args:
                    if not first:
                        s += ", "
                    first = False
                    if self._cvars[i] == "int" and transform_int_name:
                        s += "(void*) _p{name:s}".format(name = i)
                    else:
                        s += "(void*) " + i
                for i in self._character_args:
                    s+= ", ( flexiblas_fortran_charlen_t ) " + i
                s += ");\n"
        else:
            s += "fn("
            first = True
            for i in self._args:
                if not first:
                    s += ", "
                first = False
                if self._cvars[i] == "int" and transform_int_name:
                    s += "(void*) _p{name:s}".format(name = i)
                else:
                    s += "(void*) " + i
            for i in self._character_args:
                s+= ", ( flexiblas_fortran_charlen_t ) " + i
            s += ");\n"
        rs = ""
        for l in s.splitlines():
            rs += "    " + extraspace + l + "\n"

        return rs

    def _callhook(self, transform_int_name = False, extraspace="", intwidth = 0 ):
        s = ""
        if self._function:
            if is_complex(self._fvars[self._name]["typespec"]):
                s += extraspace + "fn_hook( &ret"
                for i in self._args:
                    s += ", "
                    if self._cvars[i] == "int" and transform_int_name:
                        s += "(void*) _p{name:s}".format(name = i)
                    else:
                        s += "(void*) " + i
                s += ");"
            else:
                s += extraspace + "ret = fn_hook("
                first = True
                for i in self._args:
                    if not first:
                        s += ", "
                    first = False
                    if self._cvars[i] == "int" and transform_int_name:
                        s += "(void*) _p{name:s}".format(name = i)
                    else:
                        s += "(void*) " + i
                for i in self._character_args:
                    s+= ", ( flexiblas_fortran_charlen_t )" + i
                s += ");"
        else:
            s += extraspace+ "fn_hook("
            first = True
            for i in self._args:
                if not first:
                    s += ", "
                first = False
                if self._cvars[i] == "int" and transform_int_name:
                    s += "(void*) _p{name:s}".format(name = i)
                else:
                    s += "(void*) " + i
            for i in self._character_args:
                s+= ", ( flexiblas_fortran_charlen_t ) " + i
            s += ");"
        return s


    def structure_declare(self):
        s = "struct flexiblas_blasfn {name:s};" .format(name = self.funcname())
        return s
    def structure_instance(self):
        s = "struct flexiblas_blasfn flexiblas_{name:s} = HOOK_INIT;" .format(name = self.funcname())
        return s

    def c_typedef(self, intwidth = 0, intel_interface = False):
        if intel_interface:
            if self._function and is_complex(self._fvars[self._name]["typespec"]):
                s = "typedef void (*" + self.typename() + ")( " + self._returntype + "* returnvalue, "
            else:
                s = "typedef " + self._returntype + " (* " + self.typename() + ")("
        else:
            s = "typedef " + self._returntype + " (* " + self.typename() + ")("

        s += self._callsequence(intwidth = intwidth)
        s += ");"
        return s

    def c_header(self, intwidth = 0, suffix="_", end = ";", intel_interface= False):
        if intel_interface:
            if self._function and is_complex(self._fvars[self._name]["typespec"]):
                s = "void " + self.funcname(intwidth)+suffix+"( " + self._returntype + "* returnvalue, "
            else:
                s = self._returntype + " " + self.funcname(intwidth) + suffix + "("
        else:
            s = self._returntype + " " + self.funcname(intwidth)+suffix + "("

        s += self._callsequence(intwidth = intwidth)
        s += ")"+end
        return s

    def c_headerx(self, intwidth = 0, suffix="_", end = ";", intel_interface= False, number = ""):
        rt = self._returntype
        if rt == "int":
            rt = "blasint"
        if intel_interface:
            if self._function and is_complex(self._fvars[self._name]["typespec"]):
                s = "void " + self.funcnamex(intwidth, number)+"( " + self._returntype + "* returnvalue, "
            else:
                s = rt + " " + self.funcnamex(intwidth, number) + "("
        else:
            s = rt + " " + self.funcnamex(intwidth, number) + "("

        s += self._callsequence(intwidth = intwidth)
        s += ")"+end
        return s

    def c_header_real(self, suffix="_", end = ";", intel_interface= False, call = False, xname="real"):
        if self._function:
            if intel_interface and is_complex(self._fvars[self._name]["typespec"]):
                if call:
                    s = ""
                else:
                    s = "void "
                s += "flexiblas_"+xname+"_" + self.funcname(0)+suffix+"( "
                if call:
                    s+="("
                s += self._returntype + "*"
                if call:
                    s+=")"
                s +="returnvalue, "
            else:
                if self._returntype == "int":
                    if call:
                        s = "return "
                    else:
                        s = "blasint "
                    s += "flexiblas_"+xname+"_" + self.funcname(0) +suffix+ "("
                else:
                    if call:
                        s = "return "
                    else:
                        s = self._returntype + " "
                    s += "flexiblas_"+xname+"_" + self.funcname(0) +suffix+ "("
        else:
            if call:
                s = ""
            else:
                s = "void "
            s += "flexiblas_"+xname+"_" + self.funcname(0) +suffix+ "("
        s += self._callsequence(intwidth = 0,  void = True, call = call, typecast = call)
        s += ")"+end
        return s

    def c_header_chain(self, suffix="_", end = ";", intel_interface= False):
        if self._function:
            if intel_interface and is_complex(self._fvars[self._name]["typespec"]):
                s = "void flexiblas_chain_" + self.funcname(0)+suffix+"( " + self._returntype + "* returnvalue, "
            else:
                if self._returntype == "int":
                    s = "blasint flexiblas_chain_" + self.funcname(0) +suffix+ "("
                else:
                    s = self._returntype + " flexiblas_chain_" + self.funcname(0) +suffix+ "("
        else:
            s = "void flexiblas_chain_" + self.funcname(0) +suffix+ "("
        s += self._callsequence(intwidth = 0,  void = True)
        s += ")"+end
        return s


    def wrapper_real(self, intwidth = 0, suffix = "_", intel_interface = False, part ="blas"):
        #Adjust Datatypes
        int_type = self._inttype(intwidth)
        return_type = self._returntype
        if return_type == "int":
            return_type = int_type
        s = """
{header_intel:s}
{{\n""".format( header_intel = self.c_header_real (suffix=suffix, intel_interface=True, end=""))

        s += "    {rettype:s} (*fn) ({args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))
        if self._function and is_complex(self._fvars[self._name]["typespec"]):
            s += "    void (*fn_intel) ({rettype:s} *ret, {args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))
        if self._function:
            s += "    {returntype:s} ret;\n".format(returntype = return_type)
        s += "\n"
        # Get the function
        s += "    *(void **) & fn = current_backend->{part:s}.{funcname:s}.f77_blas_function;\n".format(funcname = self._name.lower(),part=part)

        if self._function and is_complex(self._fvars[self._name]["typespec"]):
            s+= "    *(void **) & fn_intel = *(void **) &fn;\n"
        s += "\n"


        # Call the function
        s += self._callfunction(transform_int_name=False, intwidth=intwidth )
        s += "\n"

        if self._function:
            if is_complex(self._fvars[self._name]["typespec"]):
                s += """
    *(({returntype:s} *)returnvalue) = ret;
    return;\n""".format(returntype= return_type)
            else:
                s += "    return ret;\n"
        else:
            s += "    return;\n"
        s += "}\n"

        # Add alias
        if ( suffix == "_"):
             s += "#ifndef __APPLE__\n"
             s += self.c_header_real(suffix="", intel_interface=True, end="")
             s += " __attribute__((alias(\"flexiblas_real_{name:s}_\")));\n".format(name=self.funcname(0))
             s += "#else\n"
             s += self.c_header_real(suffix="", intel_interface=True, end="") + "{"
             s += self.c_header_real(suffix="_", intel_interface=True, end = ";", call = True) + "}\n"
             s += "#endif\n"
        s +="\n"

        return s

    def wrapper_chain(self, intwidth = 0, suffix = "_", intel_interface = False, part ="blas"):
        #Adjust Datatypes
        int_type = self._inttype(intwidth)
        return_type = self._returntype
        if return_type == "int":
            return_type = int_type
        s = """
{header_intel:s}
{{\n""".format( header_intel = self.c_header_chain (suffix=suffix, intel_interface=True, end=""))

        s += "    {rettype:s} (*fn) ({args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))
        if self._function and is_complex(self._fvars[self._name]["typespec"]):
            s += "    void (*fn_intel) ({rettype:s} *ret, {args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))
            s += "    void (*fn_hook) ({rettype:s} *ret, {args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))
        else:
            s += "    {rettype:s} (*fn_hook) ({args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))

        if self._function:
            s += "    {returntype:s} ret;\n".format(returntype = return_type)
        s += "\n"
        # Get the function
        s += "    *(void **) &fn      = current_backend->{part:s}.{funcname:s}.f77_blas_function;\n".format(funcname = self._name.lower(),part=part)
        if self._function and is_complex(self._fvars[self._name]["typespec"]):
            s+= "    *(void **) & fn_intel = *(void **) &fn;\n"

        # Check the hook
        s += """
    hook_pos_{funcname:s} ++;
    if( hook_pos_{funcname:s} < __flexiblas_hooks->{funcname:s}.nhook) {{
        *(void **) &fn_hook = __flexiblas_hooks->{funcname:s}.f77_hook_function[hook_pos_{funcname:s}];
        {call_hook:s}
    }} else {{
        hook_pos_{funcname:s} = 0;\n""".format(funcname = self._name.lower(),
                                               call_hook = self._callhook())

        # Call the function
        s += self._callfunction(transform_int_name=False, intwidth=intwidth, extraspace="    " )
        s += "    }\n"

        if self._function:
            if is_complex(self._fvars[self._name]["typespec"]):
                s += """
    *(({returntype:s} *)returnvalue) = ret;
    return;
""".format(returntype= return_type)
            else:
                s += "    return ret;\n"
        else:
            s += "    return;\n"
        s += "}\n"

        # Add alias
        if ( suffix == "_"):
             s += "#ifndef __APPLE__\n"
             s += self.c_header_real(suffix="", intel_interface=True, end="", xname = "chain")
             s += " __attribute__((alias(\"flexiblas_chain_{name:s}_\")));\n".format(name=self.funcname(0))
             s += "#else\n"
             s += self.c_header_real(suffix="", intel_interface=True, end="", xname = "chain") + "{"
             s += self.c_header_real(suffix="_", intel_interface=True, end = ";", call = True, xname = "chain") + "}\n"
             s += "#endif\n"
        s +="\n"

        return s


    def wrapper(self, intwidth = 0, suffix = "_", intel_interface = False, part ="blas", full_interface= False ):
        #Adjust Datatypes
        int_type = self._inttype(intwidth)
        return_type = self._returntype
        if return_type == "int":
            return_type = int_type
        s = """
static TLS_STORE uint8_t hook_pos_{funcname:s} = 0;
#ifdef FLEXIBLAS_ABI_INTEL
{header_intel:s}
#else
{header_gnu:s}
#endif
{{\n""".format( header_intel = self.c_headerx(intwidth = intwidth, suffix=suffix, intel_interface=True, end=""),
                           header_gnu = self.c_headerx(intwidth = intwidth, suffix=suffix, intel_interface=False, end=""),
                funcname = self.funcname())
        s += "    {rettype:s} (*fn) ({args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))
        if self._function and is_complex(self._fvars[self._name]["typespec"]):
            s += "    void (*fn_intel) ({rettype:s} *ret, {args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))
            s += "    void (*fn_hook) ({rettype:s} *ret, {args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))
        else:
            s += "    {rettype:s} (*fn_hook) ({args:s});\n".format(rettype = return_type, args = self._callsequence(intwidth, void=True))


        if self._function:
            s += "    {returntype:s} ret;\n".format(returntype = return_type)
        if full_interface:
            fmt = ""
            if intwidth == 0:
                fmt = "    int32_t _{name:s}32; int64_t _{name:s}64; void* _p{name:s};\n"
            elif intwidth == 32:
                fmt = "    int64_t _{name:s}64; void* _p{name:s};\n"
            elif intwidth == 64:
                fmt = "    int32_t _{name:s}32; void* _p{name:s};\n"

            for i in self._args:
                if self._cvars[i] == "int":
                    s += fmt.format(name = i)
        s += """
    if ( current_backend->post_init != 0 ) {
        __flexiblas_backend_init(current_backend);
        current_backend->post_init = 0;
    }\n"""
        # s += "\tfn = flexiblas_{funcname:s}.call_fblas; \n".format(funcname = self._name.lower())
        s += "    *(void **) & fn = current_backend->{part:s}.{funcname:s}.f77_blas_function;\n".format(funcname = self._name.lower(),part=part)
        s += "    *(void **) & fn_hook = __flexiblas_hooks->{funcname:s}.f77_hook_function[0];\n".format(funcname = self._name.lower(),part=part)

        # s += "\tif ( fn == NULL ) { \n"
        # s += "\t\tfprintf(stderr, PRINT_PREFIX \"{funcname:s}_ not hooked, abort\\n\"); \n".format(funcname=self._name.lower())
        # s += "\t\tabort(); \n"
        # s += "\t}\n"
        if self._function and is_complex(self._fvars[self._name]["typespec"]):
            s+= "    *(void **) & fn_intel = *(void **) &fn;\n"


        # Adjust Int sizes
        if full_interface:
            s += "    if (current_backend->info.backend_integer_size == sizeof(%s) ) {\n" % (int_type)
            s += "        if ( __flexiblas_profile ) {\n"
            s += "            ts = flexiblas_wtime();\n"
            s += "            " + self._callfunction(transform_int_name = False,extraspace="", intwidth = intwidth )
            s += "            current_backend->{part:s}.{name:s}.timings[0] += (flexiblas_wtime() -ts);\n".format(name = self._name.lower(), part = part)
            s += "            current_backend->{part:s}.{name:s}.calls[0]++;\n".format(name = self._name.lower(),part=part)
            s += "        } else {\n"
            s += ("            " + self._callfunction(transform_int_name = False, extraspace ="\t", intwidth = intwidth))
            s += "        }\n"
            if self._function:
                if intel_interface and is_complex(self._fvars[self._name]["typespec"]):
                    s += "        *returnvalue = ret;\n"
                    s += "        return;\n"
                else:
                    s += "        return ret;\n"
            else:
                s += "        return;\n"
            s += "    }\n"

            # 32 bit ints
            if intwidth != 32:
                s += "    else if ( current_backend->info.backend_integer_size == 4) {\n"
                s += "        if ( sizeof({inttype:s}) > 4 ) {{\n".format(inttype=self._inttype(intwidth))
                for i in self._args:
                    if self._cvars[i] == "int":
                        s += "            if ( *{vname:s} > __INT32_MAX__) {{\n".format(vname =i)
                        s += "                fprintf(stderr,\"*** On entry of {funcname:s} the parameter {vname:s} is out of int32_t range (> %d). This might cause trouble. ***\\n\", __INT32_MAX__);\n".format(vname=i, funcname=self.funcname(intwidth))
                        s += "            }\n"
                s += "        }\n"
                for i in self._args:
                    if self._cvars[i] == "int":
                        s += "         _{name:s}32 = ({intt:s}) *{name:s};\n\t\t_p{name:s} = &_{name:s}32;\n".format(name = i,intt="int32_t")
                s +="     }\n"
            if intwidth != 64:
                s += "    else if (current_backend->info.backend_integer_size == 8) {\n"
                for i in self._args:
                    if self._cvars[i] == "int":
                        s += "         _{name:s}64 = ({intt:s})*{name:s};\n\t\t_p{name:s} = &_{name:s}64;\n".format(name = i,intt="int64_t")
                s += "     }\n"
            s += "     else {\n"
            s += "         fprintf(stderr, PRINT_PREFIX \"{funcname:s} - can not convert integer types\");\n\t\tabort();\n".format(funcname=self.funcname(intwidth))
            s += "     }\n\n"

            # Call the function
            s += "     if ( __flexiblas_profile ) {\n"
            s += "         ts = flexiblas_wtime();\n"
            s += self._callfunction()
            s += "         current_backend->{part:s}.{name:s}.timings[0] += (flexiblas_wtime() -ts);\n".format(name = self._name.lower(), part = part)
            s += "         current_backend->{part:s}.{name:s}.calls[0]++;\n".format(name = self._name.lower(),part=part)
            s += "     } else {\n"
            s += self._callfunction()
            s += "     }\n"
            if self._function:
                if intel_interface and is_complex(self._fvars[self._name]["typespec"]):
                    s += "    *returnvalue = ret;\n"
                    s += "    return;\n"
                else:
                    s += "    return ret;\n"
            else:
                s += "     return;\n"

        else:
            # Short Interface
            s += "    if ( fn_hook == NULL ) {\n";
            s += self._callfunction(transform_int_name = False, extraspace ="    ", intwidth = intwidth)
            if self._function:
                if self._function and is_complex(self._fvars[self._name]["typespec"]):
                    s += """
#ifdef FLEXIBLAS_ABI_INTEL
        *returnvalue = ret;
        return;
#else
        return ret;
#endif
"""
                else:
                    s += "        return ret;\n"

            else:
                s += "        return;\n"
            s+="    } else {\n";
            s+="        hook_pos_{funcname:s} = 0;\n".format(funcname = self.funcname())
            if self._function and is_complex(self._fvars[self._name]["typespec"]):
                s+="        fn_hook(&ret"
                for i in self._args:
                    s += ", "
                    s += "(void*) " + i
                for i in self._character_args:
                    s+= ", ( flexiblas_fortran_charlen_t ) " + i

                s += ");\n"
                s += """
#ifdef FLEXIBLAS_ABI_INTEL
        *returnvalue = ret;
        return;
#else
        return ret;
#endif
"""
            elif self._function:
                f=False;
                s+="        ret = fn_hook("
                for i in self._args:
                    if f:
                        s += ", "
                    f = True
                    s += "(void*) " + i
                for i in self._character_args:
                    s+= ", ( flexiblas_fortran_charlen_t ) " + i

                s += ");\n"
                s += "        return ret;\n";
            else:
                f=False;
                s+="        fn_hook("
                for i in self._args:
                    if f:
                        s += ", "
                    f = True
                    s += "(void*) " + i
                for i in self._character_args:
                    s+= ", ( flexiblas_fortran_charlen_t ) " + i
                s += ");\n"
                s += "        return;\n";

            s+="    }\n";
        s += "}\n"


        # Add alias
        if ( suffix == "_"):
            s += "#ifndef __APPLE__\n"
            s += self.c_headerx(intwidth = intwidth, suffix="", intel_interface=intel_interface, end="", number = "2")
            s += " __attribute__((alias(MTS({name:s}))));\n".format(name=self.funcnamex(intwidth=intwidth))
            s += self.c_headerx(intwidth = intwidth, suffix="", intel_interface=intel_interface, end="", number = "3")
            s += " __attribute__((alias(MTS({name:s}))));\n".format(name=self.funcnamex(intwidth=intwidth))
            s += "#else\n"
            s += self.c_headerx(intwidth = intwidth, suffix="", intel_interface=intel_interface, end="", number = "2")
            if self._function:
                if intel_interface and is_complex(self._fvars[self._name]["typespec"]):
                    s += "{ "+ self.funcnamex(intwidth = intwidth) + "( (void*) returnvalue, "+self._callsequence(intwidth = intwidth, void=True, typecast = True)+"); }\n"
                else:
                    s += "{ return "+ self.funcnamex(intwidth = intwidth) + "("+self._callsequence(intwidth = intwidth, void=True, typecast = True)+"); }\n"
            else:
                s += "{ "+ self.funcnamex(intwidth = intwidth) + "("+self._callsequence(intwidth = intwidth, void=True, typecast = True)+"); }\n"
            s += self.c_headerx(intwidth = intwidth, suffix="", intel_interface=intel_interface, end="", number = "3")
            if self._function:
                if intel_interface and is_complex(self._fvars[self._name]["typespec"]):
                    s += "{ "+ self.funcnamex(intwidth = intwidth) + "( (void*) returnvalue, "+self._callsequence(intwidth = intwidth, void=True, typecast = True)+"); }\n"
                else:
                    s += "{ return "+ self.funcnamex(intwidth = intwidth) + "("+self._callsequence(intwidth = intwidth, void=True, typecast = True)+"); }\n"
            else:
                s += "{ "+ self.funcnamex(intwidth = intwidth) + "("+self._callsequence(intwidth = intwidth, void=True, typecast = True)+"); }\n"
            s += "#endif\n"

#        if ( suffix == "_"):
#
#            s += "#ifdef FLEXIBLAS_ABI_IBM\n"
#            s += self.c_headerx(intwidth = intwidth, suffix="_", intel_interface=intel_interface, end="")
#            s += " __attribute__((alias(MTS({name:s}))));\n".format(name=self.funcnamex(intwidth=intwidth))
#            s += "#else\n"
#            s += "#ifndef __APPLE__\n"
#            s += self.c_headerx(intwidth = intwidth, suffix="", intel_interface=intel_interface, end="")
#            s += " __attribute__((alias(MTS({name:s}))));\n".format(name=self.funcnamex(intwidth=intwidth))
#            s += "#else\n"
#            s += self.c_headerx(intwidth = intwidth, suffix="", intel_interface=intel_interface, end="")
#            if self._function:
#                if intel_interface and is_complex(self._fvars[self._name]["typespec"]):
#                    s += "{ "+ self.funcnamex(intwidth = intwidth) + "( (void*) returnvalue, "+self._callsequence(intwidth = intwidth, void=True, call=True, typecast = True)+"); }\n"
#                else:
#                    s += "{ return "+ self.funcnamex(intwidth = intwidth) + "("+self._callsequence(intwidth = intwidth, void=True, call = True, typecast = True)+"); }\n"
#            else:
#                s += "{ "+ self.funcnamex(intwidth = intwidth) + "("+self._callsequence(intwidth = intwidth, void=True, call=True, typecast = True)+"); }\n"
#            s += "#endif\n"
#            s += "#endif\n"
        s +="\n"

        return s

    def __str__(self):
        s = ""
        if self._subroutine:
            s =  "Subroutine: " + self._name + "\n"
        else:
            s =  "Function: " + self._name + "\n"
        s += " Args: " + str(self._args) + "\n"
        s += " DArgs: " + str(self._cvars) + "\n"
        s += " ArrayArgs: " + str(self._array_args) + "\n"
        return s



class Wrapper(object):
    """docstring for Wrapper"""
    def __init__(self, wrapper_config=WrapperConfig()):
        self.config =  wrapper_config
        self.functions = OrderedDict()

    def add_functions(self, name, function):
        if name in self.functions:
            print("Function already known: " + name)
            return
            raise Exception("Function already known")
        self.functions[name] = function

    def _header_fence_start(self, fn, headername):
        fn.write("#ifndef " + headername + "\n")
        fn.write("#define " + headername + "\n")
        fn.write("\n#include <stdint.h>\n")
        fn.write("\n#include \"flexiblas_config.h\"\n")
        fn.write("\n#include \"flexiblas_fortran_mangle.h\"\n")
        fn.write("\n#include \"flexiblas_fortran_char_len.h\"\n")
        fn.write("#include <complex.h>\n\n")
        fn.write("#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n")

    def _blas_defines(self, fn):
        fn.write("#if defined(USE_BLAS_32) && defined(USE_BLAS_64)\n")
        fn.write("#error Either USE_BLAS_32 or USE_BLAS_64 must be defined!\n#endif\n\n")

        # if self.config.int32:
        #     fn.write("#ifdef USE_BLAS_32\n")
        #     fn.write("#define blasint int32_t\n")
        #     for i in self.functions:
        #         fn.write("#define {:8s} {:8s}\n".format(i.upper(), i+"32_"))
        #     fn.write("#endif\n\n")
        # if self.config.int64:
        #     fn.write("#ifdef USE_BLAS_64\n")
        #     fn.write("#define blasint int64_t\n")
        #     for i in self.functions:
        #         fn.write("#define {:8s} {:8s}\n".format(i.upper(), i+"64_"))
        #     fn.write("#endif\n\n")

        fn.write("#ifndef blasint\n")
        fn.write("#define blasint int\n")
        for i in self.functions:
            fn.write("#define {:8s} {:s}\n".format(i.upper(), i+"_"))
        fn.write("#endif\n")
        fn.write("\n")

    def _header_fence_end(self,fn):
        fn.write("\n#ifdef __cplusplus\n}\n#endif\n#endif\n")
    def _gpl_tag(self):
        s = """//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
    Copyright (C) 2013-2025 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
""".format(date =  datetime.now().ctime())
        return s

    def write_header_file(self, filename, headername):
        fn = open(filename, "w")
        self._header_fence_start(fn, headername)
        self._blas_defines(fn)

        for i,f in self.functions.items():
            fn.write("    " + f.c_headerx(intel_interface = self.config.intel) + "\n")
            if self.config.int32:
                fn.write("    " + f.c_header(intwidth = 32, intel_interface = self.config.intel ) + "\n")
            if self.config.int64:
                fn.write("    " + f.c_header(intwidth = 64, intel_interface = self.config.intel) + "\n")
            fn.write("\n")
        self._header_fence_end(fn)
        fn.close()

    def write_header_file_real(self, filename, headername):
        fn = open(filename, "w")
        fn.write(self._gpl_tag()+"\n")
        self._header_fence_start(fn, headername)

        for i,f in self.functions.items():
            fn.write("    " + f.c_header_real(intel_interface = True) + "\n")
            fn.write("    " + f.c_header_real(intel_interface = True, suffix = "") + "\n")
            fn.write("    " + f.c_header_chain(intel_interface = True) + "\n")
            fn.write("    " + f.c_header_chain(intel_interface = True, suffix = "") + "\n")


        self._header_fence_end(fn)
        fn.close()

    def write_typedefs(self, filename, headername):
        fn = open(filename, "w")
        self._header_fence_start(fn, headername)

        for i,f in self.functions.items():
            fn.write(f.c_typedef(intel_interface = self.config.intel) + "\n")
        self._header_fence_end(fn)
        fn.close()

    def write_structure_declares(self, filename, headername, major, minor, patch, extra):
        fn = open(filename, "w")
        self._header_fence_start(fn, headername)

        fn.write("#define FLEXIBLAS_LAPACK_MAJOR " + str(major) + "\n")
        fn.write("#define FLEXIBLAS_LAPACK_MINOR " + str(minor) + "\n")
        fn.write("#define FLEXIBLAS_LAPACK_PATCH " + str(patch) + "\n")
        fn.write("#define FLEXIBLAS_LAPACK_EXTRA \"" + extra + "\"\n\n")
        fn.write("    typedef struct _flexiblas_lapack_backend {\n");
        for i,f in self.functions.items():
            fn.write("        " + f.structure_declare()+"\n")
        fn.write("    } flexiblas_lapack_backend_t;\n")
        self._header_fence_end(fn)
        fn.close()

    def write_wrapper_file(self, filename, what="blas", skip_loader = False, skip_struct = False, skip_wrapper = False, pt ="blas", loader = "LOAD_FBLAS"):
        fn = open(filename, "w")
        fn.write(self._gpl_tag())
        fn.write("\n#include <stdio.h>\n")
        fn.write("#include <stdlib.h>\n")
        fn.write("#include <stdint.h>\n")
        fn.write("#include <complex.h>\n\n")
        fn.write("#include \"flexiblas_config.h\"\n\n")
        fn.write("#include \"flexiblas_fortran_mangle.h\"\n\n")
        fn.write("#include \"flexiblas.h\"\n\n")
        fn.write("#include \"flexiblas_fortran_char_len.h\"\n\n")

        # declare structures
        # if skip_struct == False:
        #     for i,f in self.functions.items():
        #         fn.write(f.structure_instance() + "\n")
        #     fn.write("\n")



        if skip_loader == False:
            fn.write("HIDDEN int __flexiblas_load_{what:s} ( flexiblas_backend_t *handle, int *loaded, int *failed )  {{\n".format(what=what))
            fn.write("    int _ifailed = *failed;\n")
            for i,f in self.functions.items():
                s ="    {loaderx:s}(handle,{part:s}.{name:s},{name:s});\n".format(loaderx=loader, name = f.funcname(), part=pt)
                fn.write(s)
            fn.write("    if (_ifailed != (*failed))\n        return 1;\n    else\n        return 0;\n")
            fn.write("}\n\n")
        if skip_wrapper == False:
            for i,f in self.functions.items():
                fn.write(f.wrapper(intel_interface = self.config.intel, part=pt) + "\n")
                fn.write("\n\n/* Real Implementation for Hooks */\n\n");
                fn.write(f.wrapper_real(intel_interface = self.config.intel, part=pt) + "\n")
                fn.write("\n\n/* Chainloader for Hooks */\n\n");
                fn.write(f.wrapper_chain(intel_interface = self.config.intel, part=pt) + "\n")
                fn.write("\n")
        fn.close()

    def write_wrapper_file_real(self, filename, what="blas", pt ="blas"):
        fn = open(filename, "w")
        fn.write(self._gpl_tag())
        fn.write("\n#include <stdio.h>\n")
        fn.write("#include <stdlib.h>\n")
        fn.write("#include <stdint.h>\n")
        fn.write("#include <complex.h>\n\n")
        fn.write("#include \"flexiblas_fortran_mangle.h\"\n\n")
        fn.write("#include \"flexiblas.h\"\n\n")
        fn.write("#include \"flexiblas_fortran_char_len.h\"\n\n")

        for i,f in self.functions.items():
            fn.write(f.wrapper_real(intel_interface = self.config.intel, part=pt) + "\n")
            fn.write("\n")
        fn.close()

    def function_enum(self, td_name):
        s = "typedef enum { \n"
        for i in self.functions:
            s += "\t F_" + i.upper() + ",\n"
        s += "\t F_NONE\n } " + td_name + ";"
        return s






