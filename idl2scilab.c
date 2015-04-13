/****************************************************************
 * Interface pour pouvoir utiliser idl2matlab dans matlab
 * grace a la fonction mex 
 ***************************************************************/


#include "mex.h"  /* Pour pouvoir compiler avec la fonction Mex de Matlab */
#include "matrix.h"
#include <stdlib.h>

#define printf mexPrintf
#define malloc mxMalloc
#define realloc mxRealloc
#define free mxFree
#define main idl2matlab
#define exit(x) { printf("Error : %d\n",x); \
mexErrMsgTxt("Error in idl2scilab");}

#include "main.c"

void mexFunction(
                 int nlhs,       mxArray *plhs[],
                 int nrhs, const mxArray *prhs[]  )
{
    int carg;
    char *varg[4];
    char * InputString;
    int buflen;
    int status;
    int i;

    carg = nrhs + 1;

    for (i = 0; i < nrhs; i++)
    {
	if (mxIsChar(prhs[i]) != 1)
	{
	    mexPrintf("Error : argument %i\n", i+1);
	    mexErrMsgTxt("Input should be strings");
	}
	
	buflen      = (mxGetM(prhs[i])*mxGetN(prhs[i]))+1;
	varg[i+1] = (char*)mxMalloc(buflen+64);
	if (varg[i+1] == NULL)
	{
	    mexPrintf("Error : argument %i. Size %i\n", i, buflen);
	    mexErrMsgTxt("Can not allocate memory for input string\n");
	}
	status      = mxGetString(prhs[i], varg[i+1], buflen);
	if (status != 0)
	{
	    mexPrintf("Error : argument %i. Status %i\n", i, status);
	    mexErrMsgTxt("Can not get input parameter\n");
	}
    }


    main(carg,varg);
    
    
    return;
}
