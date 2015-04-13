PRO holland,filename

@lamp.cbk
OPENR,unit,/get,filename
i=0
numors=' '
subnum=' '
WHILE NOT EOF(unit) DO BEGIN
READF, unit, numors
READF, unit, subnum

PRINT,"W1=num2merg(["+numors+"],/sdev,min=10,bin=.3,sub=["+subnum+"],/nopr)"
PRINT,"d20reg,w1,'dhm"+STRCOMPRESS(i,/REM)+"',par=indgen(39)"

XICUTE,"W1=num2merg(["+numors+"],/sdev,min=10,bin=.3,sub=["+subnum+"],/nopr)"
XICUTE,"d20reg,w1,'dhm"+STRCOMPRESS(i,/REM)+"',par=indgen(39)"
i=i+1
ENDWHILE

FREE_LUN,unit

END
