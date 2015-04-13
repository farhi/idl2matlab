/*-----------------------------program texmex.c-----------------------------*/

#include <mex.h>	/* include MEX library for Matlab */

#include <signal.h>
#include <unistd.h>

#define printf mexPrintf	/* Addapt looktxt.c code to Mex syntax */
#define malloc mxMalloc
#define realloc mxRealloc
/* #define free mxFree  */
#define free NoOp 

#define main setSignal /*signal2*/		/* change stand-alone source to a Matlab usablelibrary */

#define argc carg
#define argv varg
#define TEXMEX

/*char* strEvalString;*/  	      	/* action to execute */
char strEvalString[256];
unsigned iSec;      	      	/* signal delay in seconds*/

int NoOp(char *pointer)
{
  return 0;
}

/***********************************************/
/* Cette fonction traite le timer en evaluant  */
/* une chaine de caracteres                    */
/***********************************************/
void traite_alarme(int sig)
{
  mexEvalString(strEvalString);
}

/***********************************************/
/* Cette fonction initialise le timer sur une  */
/* frequence donnee en secondes                */
/***********************************************/

int setSignal(unsigned iSec, char* strAction)
{
  unsigned ret; 
  signal(SIGALRM, traite_alarme);
  ret = alarm(iSec);
  if (strAction != NULL)
  {
    strcpy(strEvalString, strAction);
  }
  return 0;
}

/***********************************************/
/* Interface entre Matlab et IDL2Matlab        */
/*                                             */
/***********************************************/

void mexFunction(int nlhs, mxArray *plhs[], 
                        int nrhs, const mxArray *prhs[])
{
  char *InputTokens, *InputString, *strAction;
  char EndFlag = 0;
  int  status;
  int  buflen;
  double *pMainField;
  long MainField;

  buflen = 1024;

  iSec = 5;
  strAction = NULL;

  /*  check strEvalString is Empty or Not */
  if (strlen(strEvalString) == 0)
  {  
    strcpy(strEvalString, "widget_signal(0)");
  }

  /* check in parameters */

  if (nrhs > 2)
  {
    mexErrMsgTxt("timer : Too many input arguments (2 max).");
  }
  if (nrhs < 1)
  {
    mexErrMsgTxt("timer : one or two input arguments required.");
  }

  /* allocate memory */
  if (nrhs == 1) /* one argument only */
  {
    buflen      = (mxGetM(prhs[0])*mxGetN(prhs[0]))+1;
    InputString = (char*)mxMalloc(buflen+64);
    if (InputString == NULL)
    {
      mexErrMsgTxt("timer/mex : can not allocate memory for input string\n");
    }

    status      = mxGetString(prhs[0], InputString, buflen);
    if (status != 0)
    {
      mexErrMsgTxt("timer/mex : can not get input parameter\n");
    }
    
    iSec = atoi(InputString);
  } 
  
  if (nrhs == 2)
  {
    buflen      = (mxGetM(prhs[0])*mxGetN(prhs[0]))+1;
    InputString = (char*)mxMalloc(buflen+64);
    if (InputString == NULL)
    {
      mexErrMsgTxt("timer/mex : can not allocate memory for input string\n");
    }

    status      = mxGetString(prhs[0], InputString, buflen);
    if (status != 0)
    {
      mexErrMsgTxt("timer/mex : can not get input parameter\n");
    }
    
    iSec = atoi(InputString);
  
    buflen      = (mxGetM(prhs[1])*mxGetN(prhs[1]))+1;
    strAction = (char*)mxMalloc(buflen+64);
    if (strAction == NULL)
    {
      mexErrMsgTxt("timer/mex : can not allocate memory for input string\n");
    }
    status      = mxGetString(prhs[1], strAction, buflen);
    if (status != 0)
    {
      mexErrMsgTxt("timer/mex : can not get input parameter\n");
    }
   
  }

  MainField = setSignal(iSec, strAction);/* call main routine */
  

  /* set output parameters */
  plhs[0]=mxCreateDoubleMatrix(1,1,mxREAL);
  pMainField = mxGetPr(plhs[0]);
  *pMainField = (double)MainField;
    
  /* pour idl2matlab 
  malloc result (char*)
  if (strlen(result))
  plhs[1] = mxCreateString(result);
  */
    
  if (MainField != -1)
  {

    
  }
  
  mxFree(InputString);
  mxFree(strAction);
}
