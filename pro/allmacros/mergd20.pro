Function mergd20,wn,nosum=nosum,nointerpolation=noint,threshold=thresh,$
                    sdev=sdev,noprint=noprint,bad=bad,mult=bad_mult,bin=bin,$
                    norm=norm,datp=datp,min_count=min_count
                    

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

COMMON C_LAMP_INFO 

bad_mult=INTARR(1600)
IF NOT KEYWORD_SET(bin) THEN bin=.05 ELSE bin=bin/2.
IF NOT KEYWORD_SET(thresh) THEN thresh=3.
IF N_ELEMENTS(wn) GT 1 THEN BEGIN
  n=two
ENDIF
IF N_ELEMENTS(wn) EQ 1 THEN n=wn
workspace_number=n
IF N_ELEMENTS(n) EQ 1 THEN BEGIN
  IF NOT KEYWORD_SET(datp) THEN take_w,w,w=n ELSE w=wn
  IF NOT KEYWORD_SET(datp) THEN take_datp,d,w=n ELSE d=datp
  e=d.e
  n=d.n
  x=d.x
  scans=N_ELEMENTS(d.x(0,*))
  cells=N_ELEMENTS(d.x(*,0))
  IF N_ELEMENTS(w(0,*)) GT 2 THEN BEGIN
    ;############# Normalisation #############
    norm=TOTAL(d.n(0,0,*))/N_ELEMENTS(d.n(0,0,*))
    time=TOTAL(d.n(0,1,*))/N_ELEMENTS(d.n(0,1,*))
    FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN
      w(*,i)  =  w(*,i)*norm/d.n(0,0,i)
      e(*,i)  =  e(*,i)*norm/d.n(0,0,i)
      n(*,*,i)=n(*,*,i)*norm/d.n(0,0,i)
    ENDFOR
    ;############# 2Theta ####################
    delta=(10.*d.x(0,*)-FLOOR(10.*d.x(0,*)))/10.
    test=ABS(median(delta)-total(delta)/N_ELEMENTS(d.x(0,*)))
    delta=(10.*(d.x(0,*)-.05)-FLOOR(10.*(d.x(0,*)-.05)))/10.
    IF test LT ABS(median(delta+.05)-total(delta+.05)/N_ELEMENTS(d.x(0,*))) THEN BEGIN
      delta=(10.*d.x(0,*)-FLOOR(10.*d.x(0,*)))/10.
      delta=MEDIAN(delta)
      x=FLOOR(10.*d.x)/10.+delta
      print,'+ ','Delta: ',delta
    ENDIF ELSE BEGIN
      delta=MEDIAN(delta+.05)
      delta=((10.*delta)-FLOOR(10.*delta))/10.
      x=FLOOR(10.*(d.x-.05))/10.+delta
      print,'- ','Delta: ',delta
    ENDELSE
    FOR i=0,N_ELEMENTS(w(0,*))-1 DO BEGIN
        IF ABS(d.x(0,i)-x(0,i)) GE bin THEN BEGIN
          IF NOT KEYWORD_SET(noint) THEN BEGIN
            PRINT,'Step',STRCOMPRESS(i),' outside limit, interpolated: ' ,d.x(0,i),x(0,i)
            w(*,i)=INTERPOL(w(*,i),d.x(*,i),x(*,i))
            d.e(*,i)=INTERPOL(d.e(*,i),d.x(*,i),x(*,i))
            d.x(*,i)=x(*,i)
          ENDIF ELSE PRINT,'Step',STRCOMPRESS(i),' outside limit, rejected: ' ,d.x(0,i),x(0,i) 
        ENDIF ;ELSE   PRINT,'Step',STRCOMPRESS(i),' inside limit: ' ,d.x(0,i),x(0,i) 
    ENDFOR  
    xrange=[FLOOR(10.*min(d.x))/10.+delta,CEIL(10.*max(d.x))/10.+delta]
    x=0
    e=0
    y=0
    IF NOT KEYWORD_SET(min_count) THEN BEGIN
      IF KEYWORD_SET(sdev) THEN min_count=2 ELSE min_count=1  
    ENDIF ELSE BEGIN
      IF KEYWORD_SET(sdev) THEN min_count=min_count>2 ELSE min_count=min_count>1
    ENDELSE 
    IF NOT KEYWORD_SET(nosum)  THEN BEGIN
      FOR i=xrange(0),xrange(1),0.1 DO BEGIN
        index=WHERE(d.x GE i-bin AND d.x LT i+bin,count)
        IF count GT 0 THEN BEGIN
          tmp_w = w(index)
          tmp_m = MEDIAN(tmp_w)
          tmp_e = d.e(index)
          tmp_i= WHERE(ABS(tmp_w-tmp_m) LE thresh*tmp_e,count)
          tmp_n= WHERE(ABS(tmp_w-tmp_m) GT thresh*tmp_e,count_n)
          IF count EQ 1 THEN BEGIN
            tmp=min(tmp_w(WHERE(tmp_w GE tmp_m))-tmp_m,tmp_pos)
            tmp=max(tmp_w(WHERE(tmp_w LE tmp_m))-tmp_m,tmp_neg)
            tmp_i= [tmp_pos,tmp_neg]
            count=2
            ;tmp_n= WHERE(index NE tmp_pos AND index NE tmp_neg,count_n)
          ENDIF 
          IF count_n GT 0 THEN BEGIN
            index_n=index(tmp_n)
            new_bad=ROUND(10*(d.x(index_n/scans,index_n/cells)-d.x(0,index_n/cells)))
            bad_mult(new_bad)=bad_mult(new_bad)+1
            IF N_ELEMENTS(bad) LT 1 THEN BEGIN
              bad=new_bad
            ENDIF ELSE BEGIN
              bad=[bad,new_bad]
            ENDELSE    
          ENDIF    
          IF count GT 0 THEN index=index(tmp_i)
        ENDIF
        IF count GE min_count THEN BEGIN
          tmp_0=TOTAL(w(index)^2/d.e(index)^2)
          IF tmp_0 GT 0 THEN BEGIN
            mean=TOTAL(w(index))/count
            x=[x,i]
            y=[y,mean]
            IF count GT 1 THEN BEGIN
              sdev=SQRT(TOTAL((w(index)-mean)^2))/((count-1)>1)
            ENDIF ELSE sdev=mean/SQRT(TOTAL(w(index)^2/d.e(index)^2))
            tmp_0=mean/SQRT(tmp_0>0)
            IF NOT KEYWORD_SET(noprint) THEN BEGIN
              print,i,mean,count,sdev,tmp_0
            ENDIF
            IF KEYWORD_SET(sdev) THEN e=[e,sdev] ELSE BEGIN
              e=[e,tmp_0] 
            ENDELSE
          ENDIF
        ENDIF
      ENDFOR
      x=x[1:N_ELEMENTS(x)-1]
      w=y[1:N_ELEMENTS(y)-1]
      e=e[1:N_ELEMENTS(e)-1]
    ENDIF
  ENDIF ELSE PRINT,'WorkSpace should be a 2theta scan of at least 2 steps'
ENDIF ELSE PRINT,'WorkSpace Numbers missing'
IF KEYWORD_SET(nosum) THEN BEGIN
  index=SORT(d.x)
  w=w(index)
  e=d.e(index)
  x=d.x(index)
ENDIF
bad=bad(sort(bad))
bad=bad(uniq(bad))
bad=bad(sort(bad_mult(bad)))
new_bad=[bad,bad]
nb_bad=N_ELEMENTS(bad)
new_bad(INDGEN(nb_bad)*2)=bad
new_bad(INDGEN(nb_bad)*2+1)=bad_mult(bad)
PRINT,STRCOMPRESS(N_ELEMENTS(where(bad_mult eq max(bad_mult))),/RE),' cells are',STRCOMPRESS(max(bad_mult)),' of',STRCOMPRESS(scans),' times excluded : '
print,STRCOMPRESS(where(bad_mult eq max(bad_mult)))
d.w_tit=d.w_tit+' - merged W'+STRCOMPRESS(workspace_number,/RE)+STRCOMPRESS(ROUND(norm))
mod_datp,d,'e',e
mod_datp,d,'x',x
mod_datp,d,'n',[[norm,time,0]]
IF NOT KEYWORD_SET(datp) THEN give_datp,d ELSE datp=d
RETURN,w
END
