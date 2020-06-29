INCLUDE(CheckCSourceCompiles)
MESSAGE(STATUS "Check for Thread Local Storage")

CHECK_C_SOURCE_COMPILES("
__thread int tls;

int main(void) {
    return 0;
}" HAVE_THREAD_LOCAL)

CHECK_C_SOURCE_COMPILES("
_Thread_local int tls;

int main(void) {
    return 0;
}" HAVE__THREAD_LOCAL)

CHECK_C_SOURCE_COMPILES("
__thread int tls;

int main(void) {
    return 0;
    }" HAVE_GNU_THREAD_LOCAL)

