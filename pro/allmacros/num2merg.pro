Function num2merg,num1,num2,$
                    nosum=nosum,nointerpolation=noint,threshold=thresh,$
                    sdev=sdev,noprint=noprint,bad=bad,mult=bad_mult,bin=bin,$
                    norm=norm,min_count=min_count,subnum=subnum
                    

;+
; Merge twotheta scan of n steps (PSD cell width must be 0.1 deg!)
; W1 = mergd20 (W2,/nosum)
; Function
; Parameters
;   Number of workspace to be merged 
; Keywords
;   NOSUM : no sum ...
; Output
;   Workspace containing merged diagram
;-

IF N_ELEMENTS(num1) GT 1 OR N_PARAMS() EQ 1 THEN BEGIN
  numors=num1 
ENDIF ELSE BEGIN
  numors=LINDGEN(ABS(num2-num1)+1)+min([num1,num2])
ENDELSE
d=1
IF NOT KEYWORD_SET(subnum) THEN subnum=INTARR(N_ELEMENTS(numors))+9999  
FOR i=0, N_ELEMENTS(numors)-1 DO BEGIN
  W1=RDRUN(numors(i),datp=d)
  IF i LT N_ELEMENTS(subnum) THEN BEGIN
    W1  =W1  (*,  0:((N_ELEMENTS(W1(0,*))<subnum(i))-1))
    d.e =d.e (*,  0:((N_ELEMENTS(W1(0,*))<subnum(i))-1))
    d.x =d.x (*,  0:((N_ELEMENTS(W1(0,*))<subnum(i))-1))
    d.pv=d.pv(*,  0:((N_ELEMENTS(W1(0,*))<subnum(i))-1))
    d.n =d.n (*,*,0:((N_ELEMENTS(W1(0,*))<subnum(i))-1))
    d.y =d.y (    0:((N_ELEMENTS(W1(0,*))<subnum(i))-1))
  ENDIF
  IF i EQ 0 THEN BEGIN
    ;help,w1,d,/stru
    W =W1
    E =d.e
    X =d.x
    N =d.n
    Y =d.y
    PV=d.pv
  ENDIF ELSE BEGIN
    W =[[W],   [W1]]
    X =[[X],   [d.x]]
    E =[[E],   [d.e]]
    PV=[[PV],  [d.pv]]
    Y =[Y,      d.y]
    N =[[[N]],[[d.n]]]
  ENDELSE
ENDFOR
mod_datp,d,'x',x
mod_datp,d,'e',e
mod_datp,d,'n',n
mod_datp,d,'y',y
mod_datp,d,'pv',pv
IF NOT KEYWORD_SET(noprint)   THEN noprint=0
IF NOT KEYWORD_SET(sdev)      THEN sdev=0
IF NOT KEYWORD_SET(min_count) THEN min_count=0
IF NOT KEYWORD_SET(bin)       THEN bin=0
IF NOT KEYWORD_SET(norm)      THEN norm=0
PRINT,N_ELEMENTS(Y),' steps'
W=mergd20(W,datp=d,sdev=sdev,noprint=noprint,bin=bin,norm=norm,min_count=min_count)
GIVE_DATP,d
RETURN,W
END
