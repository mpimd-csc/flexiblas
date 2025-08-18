ifndef OUTFILE
OUTFILE=libcscutils.a
endif

ifdef INC
include $(INC)
endif

CC?=gcc
CFLAGS?= -O2 -Wall -fPIC -DNO_BZ2 -DNO_XZ



.SUFFIXES: .c .o
.PHONY: clean distclean

CFLAGS+=-I$(shell pwd)/include

SRCS=  common/error_message.c\
       common/backtrace.c\
       common/counter.c \
       common/string.c \
       common/table.c \
       common/table_comment.c \
       common/table_latex.c \
       ini/inifile.c\
       file/dir.c\
       file/io.c\
       file/compress_handler.c\
       file/compress_io_uncompressed.c\
       file/compress_io_gzip.c\
       threading/worker_queue.c\
       hdf5/common.c \
       hdf5/datatypes.c \
       hdf5/matrix.c \
       hdf5/sparse_matrix.c \
       hdf5/vector.c

OBJS=$(addprefix objs/,$(SRCS:%.c=%.o))

$(OUTFILE): objs $(OBJS)
  ar r $(OUTFILE) $(OBJS)

objs/%.o: src/%.c
  mkdir -p $(dir $@)
  $(CC) -fopenmp $(CFLAGS) -c -o $@ $<
objs:
  mkdir objs

clean:
  rm -rf objs

distclean: clean
  rm -f $(OUTFILE)


