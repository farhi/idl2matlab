pro arne2,first,last

XICUTE,"W1=RDRUN("+STRING(first)+")"
XICUTE,"W2=W1"
XICUTE,"PRINT,REFORM(N2(0,0,*),2)"
FOR i=first+1,last DO BEGIN
  XICUTE,"N=N2"
  XICUTE,"W1=RDRUN("+STRING(i)+")"
  XICUTE,"W2=W2+W1"
  XICUTE,"N2=N+N1"
  XICUTE,"PRINT,REFORM(N2(0,0,*),2)"
ENDFOR
XICUTE,"W3=sumscan(2)"
XICUTE,"W3=W3/("+STRING(last-first+1.)+")"
XICUTE,"E3=E3/("+STRING(last-first+1.)+")"
XICUTE,"N3=N3/("+STRING(last-first+1.)+")"
XICUTE,"W2=W2/("+STRING(last-first+1.)+")"
XICUTE,"E2=E2/("+STRING(last-first+1.)+")"
XICUTE,"N2=N2/("+STRING(last-first+1.)+")"
END
