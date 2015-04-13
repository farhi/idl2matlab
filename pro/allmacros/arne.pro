pro arne,first,last

XICUTE,"W1=RDRUN("+STRING(first)+")"
XICUTE,"W2=sumscan(1)"
XICUTE,"W3=W2"

FOR i=first+1,last DO BEGIN
  XICUTE,"W1=RDRUN("+STRING(i)+")"
  XICUTE,"W2=sumscan(1)"
  XICUTE,"W3=W3+W2"
ENDFOR
XICUTE,"W3=W3/("+STRING(last-first+1.)+")"
XICUTE,"E3=E3/("+STRING(last-first+1.)+")"
XICUTE,"N3=N3/("+STRING(last-first+1.)+")"
END
