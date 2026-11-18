# libcscutils

`libcscutils` is a comprehensive collection of utility libraries designed to simplify common software engineering tasks. It provides a set of helper functions and data structures that are useful for a wide range of C and Fortran applications.

## Dependencies

### Required
- **C11 Compiler**: A compiler supporting the C11 standard.
- **POSIX Threads**: Required for threading support on Linux/Unix systems.

### Optional
- **ZLIB, BZip2, LZMA**: For transparent compression support in IO modules.
- **ZSTD**: For HDF5 compressed IO support.
- **HDF5**: For reading and writing HDF5 files.
- **CPUFREQ**: For advanced system information (e.g., CPU frequency).
- **Lua**: For embedding the Lua scripting language.

## Modules

### IniFile
Reading and writing of key-value based configuration files (INI format).
- **Use Case**: Simplifying the management of application settings and configuration through human-readable INI files, supporting both reading and writing.
- **Example**: [`examples/demo_inifile.c`](./examples/demo_inifile.c)

### IO
A wrapper around libc IO routines that includes transparent compression support (ZLIB, BZIP2, LZMA).
- **Use Case**: Providing a high-performance wrapper around libc IO routines with transparent support for ZLIB, BZIP2, and LZMA compression to optimize disk space.
- **Example**: [`examples/demo_io.c`](./examples/demo_io.c)

### Threading
Support for multi-threaded applications, featuring a background worker thread pool and locked queues.
- **Use Case**: Facilitating concurrent execution in applications by providing a robust background worker thread pool and locked queues for safe task offloading.
- **Example**: [`examples/demo_background_worker.c`](./examples/demo_background_worker.c)

### Error Message
Flexible replacements for `fprintf` and `printf` to handle error, warning, and information messages consistently.
- **Use Case**: Establishing a consistent logging and error-reporting mechanism across the project using flexible replacements for printf and fprintf.
- **Example**: [`examples/hello_world.c`](./examples/hello_world.c)

### HDF5
A simplified wrapper around the HDF5 library for easy reading and writing of matrices, vectors, and basic data types.
- **Use Case**: Simplifying the interaction with HDF5 for the storage and retrieval of large scientific datasets (matrices, vectors) in a portable, hierarchical format.
- **Example**: [`examples/demo_hdf.c`](./examples/demo_hdf.c), [`examples/demo_hdf_f.f90`](./examples/demo_hdf_f.f90)

### Data Structures (DS)
A collection of basic data structures including singly linked lists, hash tables, and maps.
- **Use Case**: Providing efficient, general-purpose data structures like singly linked lists, hash tables, and maps for organizing complex datasets.
- **Example**: [`examples/demo_slist.c`](./examples/demo_slist.c), [`examples/demo_hashtable.c`](./examples/demo_hashtable.c), [`examples/demo_map.c`](./examples/demo_map.c)

### SysInfo
Utilities for retrieving detailed system information, including memory usage, CPU details (cores, architecture), and OS information.
- **Use Case**: Retrieving detailed system and hardware information (CPU cores, architecture, memory usage, OS) for environment logging and performance tuning.
- **Example**: [`examples/demo_sysinfo.c`](./examples/demo_sysinfo.c)

### Lua
An interface for embedding the Lua scripting language into C/C++ and Fortran applications.
- **Use Case**: Embedding the Lua scripting language to enable dynamic extension and customization of application behavior without requiring recompilation.
- **Example**: [`examples/demo_lua.c`](./examples/demo_lua.c), [`examples/demo_lua.lua`](./examples/demo_lua.lua), [`examples/demo_lua_f.f90`](./examples/demo_lua_f.f90)

### Table
Utilities for managing and manipulating tabular data and matrices.
- **Use Case**: Handling tabular data and matrices efficiently.
- **Example**: [`examples/demo_table.c`](./examples/demo_table.c), [`examples/demo_table_fortran.f90`](./examples/demo_table_fortran.f90)

### Image
Basic support for image formats, specifically BMP.
- **Use Case**: Providing basic utilities for reading and writing BMP images.

### EventTracer
A tool for recording and writing events for performance analysis and tracing.
- **Use Case**: Recording and analyzing application events to profile execution and identify performance bottlenecks.

## Examples
A variety of examples are provided in the `examples/` directory to demonstrate the usage of each module. You can find them here: [examples](./examples)

## Documentation
All modules are documented using doxygen. In order to generate the whole
documentation check out the repository and run

```
  mkdir build && cd build
  cmake ../
  make doc
  firefox doc/explore-html/index.html
```

## CMake
At the moment, CMAKE 3.12 is the oldest supported version.

## Formatting the Code
The repository provides a clang-format file to execute this run:
```shell
git ls-files '*.c' '*.h' '*.cpp' | grep -v "^contrib" | xargs clang-format -i
```
