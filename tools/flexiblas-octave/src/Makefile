MKOCTFILE ?= mkoctfile

F77_FLAG = $(shell mkoctfile -p F77_INTEGER8_FLAG)

ifeq ($(F77_FLAG),)
FLEXIBLAS_LD := $(shell pkg-config --libs flexiblas_api)
FLEXIBLAS_CF := $(shell pkg-config --cflags flexiblas_api)
else
FLEXIBLAS_LD := $(shell pkg-config --libs flexiblas64_api)
FLEXIBLAS_CF := $(shell pkg-config --cflags flexiblas64_api)
endif

LDFLAGS := $(shell $(MKOCTFILE) -p LDFLAGS) $(FLEXIBLAS_LD)
CXXFLAGS := $(shell $(MKOCTFILE) -p CXXFLAGS) $(FLEXIBLAS_CF) -Wall --verbose

AR := $(shell $(MKOCTFILE) -p AR)

all: 	flexiblas_avail.oct \
	flexiblas_set_num_threads.oct \
	flexiblas_get_num_threads.oct \
	flexiblas_version.oct \
	flexiblas_list.oct \
	flexiblas_list_loaded.oct \
	flexiblas_load_backend.oct \
	flexiblas_load_backend_library.oct\
	flexiblas_switch.oct\
	flexiblas_current_backend.oct

%.oct: %.cc
	$(MKOCTFILE) $(CXXFLAGS) $(LDFLAGS) $<



clean:
	rm -rf *.o

cleanall: clean
	rm -rf *.oct

.PHONY: clean cleanall

