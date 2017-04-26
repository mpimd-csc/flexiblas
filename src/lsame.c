#include <ctype.h>

#if defined(__alpha__) || defined(__sparc64__) || defined(__x86_64__) || defined(__ia64__)
typedef int ftnlen;
typedef int logical;
#else
typedef long int logical;
typedef long int ftnlen;
#endif


logical lsame_(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len)
{
	logical ret_val;
	/* Local variables */
	char a, b; 

	a = tolower(ca[0]); 
	b = tolower(cb[0]); 

	return (a==b); 

    return ret_val;
} 

logical lsame(char *ca, char *cb, ftnlen ca_len, ftnlen cb_len){
	return lsame_(ca, cb, ca_len,cb_len); 
}

