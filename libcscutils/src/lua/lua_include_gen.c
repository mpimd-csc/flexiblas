/*
 * LIBCSCUTILS: LUA Include Gen
 * Copyright (C) Martin Koehler, 2020
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, see <http://www.gnu.org/licenses/>.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <time.h>

#include "cscutils/posix_impl.h"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <getopt.h>

#define LUA_DEFAULT_GUARD "LUA_CODE_H"
#define LUA_DEFAULT_VAR   "luacode"
#define LUA_DEFAULT_GET   "getluacode"


static void printusage(const char *prgname)
{
    printf("libcscutils - LUA Include File Generator\n");
    printf("----------------------------------------\n");
    printf("\n");
    printf("Usage: %s [options]\n", prgname);
    printf("\n");
    printf("options:\n");
    printf("    -i file, --input=file            Set the input file.\n");
    printf("    -o file, --output=file           Set the output file.\n");
    printf("    -g GUARDNAME, --guard=GUARDNAME  Name of the include guard.\n");
    printf("                                     Default: %s\n", LUA_DEFAULT_GUARD);
    printf("    -c codevar, --code=codevar       Name of the code variable.\n");
    printf("                                     Default: %s\n", LUA_DEFAULT_VAR);
    printf("    -f funcname, --function=funcname Name of the get function.\n");
    printf("                                     Default: %s\n", LUA_DEFAULT_GET);
    printf("    -h, --help                       This help text.\n");
    printf("\n");
    printf("Copyright (C) 2020 M.Koehler\n");
    printf("This program comes with ABSOLUTELY NO WARRANTY;\n");
    printf("This is free software, and you are welcome to redistribute it\n");
    printf("under certain conditions.\n");
}

int main(int argc, char **argv)
{
    int choice;
    char *inputfile = NULL;
    char *outputfile = NULL;
    char *guard = NULL;
    char *varname = NULL;
    char *funcname = NULL;

    while (1)
    {
        static struct option long_options[] =
        {
            /* Use flags like so:
            {"verbose",    no_argument,    &verbose_flag, 'V'}*/
            /* Argument styles: no_argument, required_argument, optional_argument */
            {"input",   required_argument, 0, 'i'},
            {"output",  required_argument, 0, 'o'},
            {"guard",   required_argument, 0, 'g'},
            {"code",    required_argument, 0, 'c'},
            {"function", required_argument, 0, 'f'},
            {"help",    no_argument,    0,    'h'},

            {0,0,0,0}
        };

        int option_index = 0;

        /* Argument parameters:
            no_argument: " "
            required_argument: ":"
            optional_argument: "::" */

        choice = getopt_long( argc, argv, "i:o:g:c:f:h",
                    long_options, &option_index);

        if (choice == -1)
            break;

        switch( choice )
        {
            case 'i':
                if (inputfile) free(inputfile);
                inputfile = strdup(optarg);
                break;
            case 'o':
                if (outputfile) free(outputfile);
                outputfile = strdup(optarg);
                break;
            case 'g':
                if (guard) free(guard);
                guard = strdup(optarg);
                break;
            case 'c':
                if (varname) free(varname);
                varname = strdup(optarg);
                break;
            case 'f':
                if (funcname) free(funcname);
                funcname = strdup(optarg);
                break;

            case 'h':
                printusage(argv[0]);
                if (inputfile) free(inputfile);
                if (outputfile) free(outputfile);
                if (guard) free(guard);
                if (varname) free(varname);
                return EXIT_SUCCESS;
                break;
            case '?':

                break;

            default:
                fprintf(stderr, "getopt_long returned with an unknown error.\n");
                return EXIT_FAILURE;
        }
    }

    int err = 0;
    FILE *ifp = NULL;
    FILE *ofp = NULL;
    char timetext[100];
    time_t now = time(NULL);
    struct tm *t = localtime(&now);
    strftime(timetext, sizeof(timetext)-1, "%m/%d/%Y %H:%M", t);

    if(!guard) guard = strdup(LUA_DEFAULT_GUARD);
    if(!varname) varname = strdup(LUA_DEFAULT_VAR);
    if(!funcname) funcname = strdup(LUA_DEFAULT_GET);



    ifp = fopen(inputfile, "r");
    if (!ifp){
        fprintf(stderr, "Failed to open %s.\n", inputfile);
        err = 1;
        goto final;
    }

    ofp = fopen(outputfile, "w");
    if (!ofp) {
        fprintf(stderr, "Failed to open %s\n", outputfile);
        err = 1;
        goto final;
    }


    fprintf(ofp, "/* This file is automatically generated from %s  */\n", inputfile);
    fprintf(ofp, "/* Date: %s  */\n\n", timetext);
    fprintf(ofp, "static const char * %s =\n", varname);

    while (!feof(ifp)) {
        char *line = NULL;
        size_t line_len = 0;
        ssize_t k;
        ssize_t ret;

        ret = csc_getline(&line, &line_len, ifp);

        if ( ret < 0 && !feof(ifp)) {
            fprintf(stderr, "Failed to read line from input.\n");
            if ( line ) free(line);
            err = 1;
            goto final;
        }

        fputc('"', ofp);
        for (k = 0; k < ret; k++) {
            switch(line[k]) {
                case '\n':
                case '\r':
                    break;
                case '\\':
                    fputc('\\',ofp);
                    fputc('\\',ofp);
                    break;
                case '"':
                    fputc('\\', ofp);
                    fputc('"', ofp);
                    break;
                default:
                    fputc(line[k], ofp);
            }
        }
        fputc('\\', ofp);
        fputc('n', ofp);
        fputc('"', ofp);
        fputc('\n',ofp);

        if ( line )  free(line);
    }
    fprintf(ofp, ";\n");
    fprintf(ofp, "\nconst char * %s ( ) { return %s; }\n\n", funcname, varname );





final:
    if ( ifp ) fclose(ifp);
    if ( ofp ) fclose(ofp);
    if ( inputfile ) free(inputfile);
    if ( outputfile ) free(outputfile);
    if ( guard )    free(guard);
    if ( varname )  free(varname);
    if ( funcname ) free(funcname);

    return err;
}
