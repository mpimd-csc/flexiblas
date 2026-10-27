export D=$(date +"%Y-%m-%d")
sed -i -e "s,^release-date: .*\$,release-date: $D,g" CODE
sed -i -e "s,^date-released: .*\$,date-released: $D,g" CITATION.cff
sed -i -e "s,^Date: .*\$,Date: $D,g" tools/flexiblas-octave/DESCRIPTION

