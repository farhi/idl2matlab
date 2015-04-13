function concat,n,n2,cells=cells,noprint=noprint
;
; Macro (started 28/10/96 by TH.HANSEN) 
; Effectuates a concatenating reading of multiple numors
; Adapted to the special case of D20 (already multiple diagrams in one numor).
; Creates a TWO-dimensional array whilst RDAND may create THREE-dimensional arrays
;
;      Modification 19-Jun-97 by Th.Hansen
; Last Modification 27-Aug-97 by Th.Hansen: keyword cells - reads only a part of each diagram
;
IF n_params() ge 1 THEN BEGIN
  IF N_PARAMS() GE 2 THEN n=n+indgen(n2-n+1)
  datp = 0
  w    = 0
  e    = 0
  x    = 0
  pv   = 0
  nn   = 0
  n = long (REFORM (n,n_elements(n)))
  FOR i=0,n_elements(n)-1 DO BEGIN
    wtmp = rdrun(n(i))
    take_datp,datptmp
    IF NOT KEYWORD_SET(noprint) THEN PRINT,STRMID(datptmp.other_tit+' '+datptmp.w_tit,0,79)
    IF n_elements(w) le 1 THEN BEGIN
      IF KEYWORD_SET(cells) THEN w= wtmp(cells,*) ELSE w=wtmp
      wt  = datptmp.w_tit		                ; choose a common title!
      IF i eq 0 THEN ot  = datptmp.other_tit	; choose a common title!
      p   = datptmp.p		                    ; if there are differences in p, no concatenation possible
      pt  = datptmp.par_txt
      pv  = datptmp.pv
      IF KEYWORD_SET(cells) THEN e= datptmp.e(cells,*) ELSE e   = datptmp.e
      nn  = datptmp.n
      IF KEYWORD_SET(cells) THEN x   = datptmp.x(cells,*) ELSE x   = datptmp.x
      y   = datptmp.y		
      z   = datptmp.z
      xt  = datptmp.x_tit
      yt  = datptmp.y_tit
      zt  = datptmp.z_tit
      t   = datptmp.time
;      IF n_elements(x) ne n_elements(wtmp)  THEN BEGIN        ; one-dimensional x-array for not-scanning detector
;        xtmptmp=wtmp*0.0
;        FOR j=0,n_elements(wtmp(0,*))-1 DO xtmptmp(*,j)=x(*)
;	    x=xtmptmp
;      ENDIF
;      IF n_elements(y) ne n_elements(wtmp) THEN BEGIN
;        ytmptmp=wtmp*0.0
;        FOR j=0,n_elements(wtmp(*,0))-1 DO ytmptmp(j,*)=y(*)
;	    y=ytmptmp
;      ENDIF
    ENDIF ELSE BEGIN
      IF KEYWORD_SET(cells) THEN wtmp= wtmp(cells,*) 
      IF KEYWORD_SET(cells) THEN etmp = datptmp.e(cells,*) ELSE etmp = datptmp.e
      IF KEYWORD_SET(cells) THEN xtmp = datptmp.x(cells,*) ELSE xtmp = datptmp.x
      ytmp = datptmp.y
      ntmp = datptmp.n
       ; *** one-dimensional x-array for not-scanning detector has to become two-dimensional if 2theta values change
      tmp=WHERE((x(0,*) EQ xtmp(0,*)) EQ 1,cnt)
      IF cnt NE N_ELEMENTS(x(0,*)) AND n_elements(x) ne n_elements(wtmp)  THEN BEGIN  ; make precedent x-array 2-dim     
        xtmptmp=w*0.0
        FOR j=0,n_elements(w(0,*))-1 DO xtmptmp(*,j)=x(*)
	    x=xtmptmp
      ENDIF
      IF cnt NE N_ELEMENTS(x(0,*)) OR N_ELEMENTS(x(0,*)) GT 1 THEN BEGIN ; make recently read x-array 2-dim
        IF n_elements(xtmp) ne n_elements(wtmp) THEN BEGIN
          xtmptmp=wtmp*0.0
          FOR j=0,n_elements(wtmp(0,*))-1 DO xtmptmp(*,j)=xtmp(*)
	      xtmp=xtmptmp
        ENDIF
      ENDIF
;      IF n_elements(ytmp) ne n_elements(wtmp) THEN BEGIN
;        ytmptmp=wtmp*0.0
;        FOR j=0,n_elements(wtmp(*,0))-1 DO ytmptmp(j,*)=ytmp(*)
;	    ytmp=ytmptmp
;      ENDIF
      IF n_elements(w(*,0)) gt n_elements(wtmp(*,0)) THEN BEGIN     ; different number of PSD-cells - should not happen ...
        PRINT,'WARNING: Different number of PSD-cells for different numors - should not happen ...'
        PRINT,'         Solution: Use option /int for interpolation of bad cells in flag (type flag,/int)!'
        wtmp=[wtmp,fltarr(n_elements(w(*,0))-n_elements(wtmp(*,0)),n_elements(wtmp(0,*)))]
        etmp=[etmp,fltarr(n_elements(w(*,0))-n_elements(wtmp(*,0)),n_elements(wtmp(0,*)))]
        xtmp=[xtmp,fltarr(n_elements(w(*,0))-n_elements(wtmp(*,0)),n_elements(wtmp(0,*)))]
        ytmp=[ytmp,fltarr(n_elements(w(*,0))-n_elements(wtmp(*,0)),n_elements(wtmp(0,*)))]
      ENDIF
      IF n_elements(w(*,0)) lt n_elements(wtmp(*,0)) THEN BEGIN
        PRINT,'WARNING: Different number of PSD-cells for different numors - should not happen ...'
        PRINT,'         Solution: Use option /int for interpolation of bad cells in flag (type flag,/int)!'
        w   =[w,fltarr(n_elements(wtmp(*,0))-n_elements(w(*,0)),n_elements(w(0,*)))]
        e   =[e,fltarr(n_elements(wtmp(*,0))-n_elements(w(*,0)),n_elements(w(0,*)))]
        x   =[x,fltarr(n_elements(wtmp(*,0))-n_elements(w(*,0)),n_elements(w(0,*)))]
        y   =[y,fltarr(n_elements(wtmp(*,0))-n_elements(w(*,0)),n_elements(w(0,*)))]
      ENDIF
;help,w,wtmp
;help,y,ytmp
      w =   [[w] ,[wtmp      ]]
      e   = [[e] ,[etmp      ]]
      IF cnt NE N_ELEMENTS(x(0,*)) OR N_ELEMENTS(x(0,*)) GT 1 THEN x   = [[x] ,[xtmp      ]]
      y   = [y ,ytmp      ]
      pv  = [[pv],[datptmp.pv]]
      nn  = [ [[nn]] , [[ntmp]]       ]
    ENDELSE
  ENDFOR
  tmp=WHERE((pv(*,0) NE pv(*,N_ELEMENTS(pv(0,*))-1)) EQ 1,cnt)
  PRINT,'Possible scan parameters:'
  PRINT,STRMID(STRING(100+tmp),10,2)+' '+pt(tmp)+STRING((pv(tmp,0)))+STRING((pv(tmp,N_ELEMENTS(pv(0,*))-1)))
  mod_datp,datptmp,'w_tit', wt
  mod_datp,datptmp,'other_tit', ot+strcompress(string(round(n(i-1))))
  mod_datp,datptmp,'p', p
  mod_datp,datptmp,'par_txt', pt
  mod_datp,datptmp,'pv', pv
  mod_datp,datptmp,'e', e
  mod_datp,datptmp,'n', nn
  mod_datp,datptmp,'x', x
  mod_datp,datptmp,'y', y
  mod_datp,datptmp,'z', z
  mod_datp,datptmp,'x_tit', xt
  mod_datp,datptmp,'y_tit', yt
  mod_datp,datptmp,'z_tit', zt
  mod_datp,datptmp,'time', t
  PRINT,'Time:',t
  give_datp,datptmp  
ENDIF
return,w
END
