 while read version ; do  podman run -v $(pwd):/workspace --rm -t intel /bin/bash /workspace/mkl_symbol.sh $version ; done < mkl_version_list.txt
