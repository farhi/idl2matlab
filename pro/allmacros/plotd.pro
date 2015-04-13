PRO plotd,Win,c,pos,d,thresh,datp=datp,bg=bg,max=max,sum=sum,x=x,$
    noprint=noprint
;+
; procedure plotd is called by def2.pro and defil.pro
; for plotting detector scans,
; for writing detector cell reply functions in *.def files,
; and for calculating the different total counts on particular positions
;
; Written by  Thomas C Hansen,          1998
; Modified by Thomas C Hansen, November 2000
;-
  IF NOT KEYWORD_SET(datp) THEN take_datp,datp 
  IF NOT KEYWORD_SET(bg)   THEN bg='?'
  IF     KEYWORD_SET(max)  THEN yr=[0,max]
  cells=N_ELEMENTS(c)
  points=N_ELEMENTS(Win(0,*))
  TVLCT,[0,255,0,0],[0,0,255,0],[0,0,0,255]
  tmp=0.
  sumtotal=total((Win)>0,1)
  sum3=sumtotal*0.
  sum5=sumtotal*0.
  sum7=sumtotal*0.
  FOR i=0,points-1 DO BEGIN
    maxc=MAX(Win    (*,i),cell)
    sum3(i)=TOTAL(Win((cell-1)>0:(cell+1)<(cells-1),i)>0)
    sum5(i)=TOTAL(Win((cell-2)>0:(cell+2)<(cells-1),i)>0)
    sum7(i)=TOTAL(Win((cell-3)>0:(cell+3)<(cells-1),i)>0)
  ENDFOR
  ; x=datp.p(14)-pos-d+datp.pv(13,*)
  IF NOT KEYWORD_SET(x) THEN x=datp.x
  IF NOT KEYWORD_SET(max) THEN maxi=MAX(sumtotal) ELSE maxi=max
  PLOT,x,sumtotal,yr=[0,maxi],COLOR=0,BACKGROUND=255,$
    TITLE='2th-om:'+STRCOMPRESS(pos)+' bg:'+STRCOMPRESS(bg),$
    SUB=datp.other_tit,XTIT=datp.w_tit
  OPLOT,x,sum3,color=1
  OPLOT,x,sum5,color=2
  OPLOT,x,sum7,color=3
  FOR i=0,cells-1 DO BEGIN
    filnam='C'+STRMID(STRCOMPRESS(c(i)+10000,/RE),1,4)+'.def'
    bid=FINDFILE(filnam,COUNT=counts)
    wfromfile=1
    IF counts GE 1 THEN BEGIN
      OPENR,in,filnam,/GET_LUN
      number=0
      READF,in,number
      xfromfile=FLTARR(number)
      wfromfile=FLTARR(number)
      READF,in,xfromfile,wfromfile
      FREE_LUN,in
    ENDIF
    nozerin=WHERE(wfromfile GT 0,nbnozerin)
    nozer  =WHERE(Win(i,*) GT 0,nbnozer)
    IF nbnozerin GE 1 THEN IF nbnozer GE 1 THEN BEGIN 
      IF NOT KEYWORD_SET(noprint) THEN PRINT,i,c(i),nbnozerin,nbnozer,[TOTAL(wfromfile(nozerin)-$
          TOTAL(wfromfile(nozerin([0,nbnozerin-1])))/2.),$
          wfromfile(nozerin([0,nbnozerin-1])),TOTAL(Win(i,nozer)-$
          TOTAL(Win(i,nozer([0,nbnozer-1])))/2.),$
          REFORM(Win(i,nozer([0,nbnozer-1])),2)]
      ; IF N_ELEMENTS(wfromfile) LE 1 OR TOTAL(wfromfile(nozerin)-$
      ;     TOTAL(wfromfile(nozerin([0,nbnozerin-1])))/2.) LE TOTAL(Win(i,nozer)-$
      ;     TOTAL(Win(i,nozer([0,nbnozer-1])))/2.) THEN BEGIN
      IF NOT KEYWORD_SET(noprint) THEN BEGIN
        IF N_ELEMENTS(wfromfile) LE 1 $
        ;OR TOTAL(wfromfile(nozerin)-TOTAL(wfromfile(nozerin([0,nbnozerin-1])))/2.) LE TOTAL(Win(i,nozer)-TOTAL(Win(i,nozer([0,nbnozer-1])))/2.) $
        OR nbnozer GE nbnozerin THEN BEGIN
          PRINT,filnam
          OPENW,out,filnam,/GET_LUN
          PRINTF,out,nbnozer
          PRINTF,out,x(nozer),REFORM(Win(i,nozer),nbnozer)
          FREE_LUN,out
        ENDIF
      ENDIF
    ENDIF ELSE IF nbnozer GE 1 THEN BEGIN 
      IF NOT KEYWORD_SET(noprint) THEN BEGIN
        PRINT,filnam
        OPENW,out,filnam,/GET_LUN
        PRINTF,out,nbnozer
        PRINTF,out,x(nozer),REFORM(Win(i,nozer),nbnozer)
        FREE_LUN,out
     ENDIF
    ENDIF
    OPLOT,x,Win(i,*),COLOR=c(i) MOD 4  ; ,LINE=c(i) mod 4
    maxw=MAX(Win(i,*),pos)
    pos=x(pos)
    IF maxw ge MAX(Win)/2. THEN BEGIN
      XYOUTS, pos,maxw,charsize=.6, ' '+strcompress(c(i),/re),align=.0,orient=90,color=c(i) mod 4
      IF N_ELEMENTS(thresh) EQ 1600 THEN BEGIN
        XYOUTS, pos,maxw,charsize=.5, strcompress(round(thresh(c(i))),/re)+'    ',align=1,orient=90,color=c(i) mod 4
      ENDIF
    ENDIF
  ENDFOR
  sum=sumtotal
END
