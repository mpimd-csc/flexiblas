//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of libcscutils, a set of helper function.
    Copyright (C) 2013-2024 Martin Koehler

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

#include "cscutils/error_message.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
    (void)argc;
    (void)argv;

    csc_print_message("Hello, World!");
    csc_info_message("This is an information message.");
    csc_warn_message("This is a warning message.");
    csc_error_message("This is an error message.");

    csc_log_prefix("demo");
    /* Example of structured logfmt logging */
    csc_log_message(CSC_LOG_LEVEL_INFO, "Downloading Matrix 21%% (123 KiB / 585 KiB) @ 123 KiB/s");
    csc_log_message(CSC_LOG_LEVEL_INFO, "Downloading Matrix 21%% (123 KiB / 585 KiB) @ 123 KiB/s matrix=HB/lund_a");
    csc_log_debug_message(1, "Waiting to obtain lock lockname=foo lockdesc=bar");
    csc_log_info_message("Downloading Matrix 21%% (123 KiB / 585 KiB) @ 123 KiB/s");
    csc_log_warn_message("This is a warning message");
    csc_log_error_message("This is an error message");

    return 0;
}
