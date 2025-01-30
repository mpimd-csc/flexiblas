#!/usr/bin/env sh
ROOT="$(pwd)"
VIMRC="${ROOT}/tools/code_generators/vimrc"
VIMCMD="${ROOT}/tools/code_generators/indentation.vim"
for d in "examples/" "include/" "src/"
do
    find "${d}" -maxdepth 1 -type f -name "*.c" -exec vim -u "${VIMRC}" -s "${VIMCMD}" "{}" \;
    find "${d}" -maxdepth 1 -type f -name "*.h" -exec vim -u "${VIMRC}" -s "${VIMCMD}" "{}" \;
done

