#!/usr/bin/env sh

if [ $# -ne 2 ]; then
    echo "usage: $0 symbols_input.txt symbols_output.txt"
    exit 1
fi

export SI="$1"
export SO="$2"

if [ -z "${MKL_LINK}" ]; then
    export MKL_LINK="-m64  -L${MKLROOT}/lib -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl"
fi

rm -f "$SO"
touch "$SO"


while read SYM;
do
TMPFILE=`mktemp --suffix .c`
TMPFILE2=`mktemp --suffix .out`

 cat > ${TMPFILE} << END_FILE
extern void $SYM();
int main(int argc, char** argv) {
    $SYM();
    return 0;
}
END_FILE

    echo -n "Check $SYM..."
    export OK=0
    gcc -o ${TMPFILE2} ${TMPFILE} $MKL_LINK && export OK=1

    if [ $OK == "1" ]; then
        echo "OK"
        echo "$SYM" >> $SO
    else
        echo "FAIL"
    fi
    rm -f ${TMPFILE} ${TMPFILE2}
done < "$SI"
