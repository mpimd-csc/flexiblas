#!/usr/bin/env sh
ROOT="$(pwd)"
VIMRC="${ROOT}/tools/code_generators/vimrc"
VIMCMD="${ROOT}/tools/code_generators/indentation.vim"
for d in "examples/" "include/" "src/"
do
    find "${d}" -type f -exec vim -u "${VIMRC}" -s "${VIMCMD}" "{}" \;
done

