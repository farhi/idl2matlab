FUNCTION hkl,max,F=F,I=I,A=A,B=B,C=C,screw=screw,glide=glide,n=n,d=d,cubic=cubic,si=si
;+
; Si : h=hkl(8,/F,/cub,d=[1,0,0])
;-
IF NOT KEYWORD_SET(max) THEN max=9
IF KEYWORD_SET(si) THEN BEGIN
  F=1
  I=0
  A=0
  B=0
  C=0
  screw=0
  glide=0
  n=0
  d=[1,0,0]
  cubic=1
ENDIF
all=[0,0,0]
FOR h=0,max DO FOR k=0,max DO FOR l=0,max DO all=[[all],[h,k,l]]
all=all(*,2:N_ELEMENTS(all(0,*))-1)
IF KEYWORD_SET(F) THEN BEGIN
  A=1
  B=1
  C=1
ENDIF
IF KEYWORD_SET(I) THEN BEGIN
  new=[0,0,0]
  FOR j=0,N_ELEMENTS(all(0,*))-1 DO IF (TOTAL(all(*,j)) MOD 2) EQ 0 THEN new=[[new],[all(*,j)]]
  all=new(*,1:N_ELEMENTS(new(0,*))-1)
ENDIF
IF KEYWORD_SET(A) THEN BEGIN
  new=[0,0,0]
  FOR j=0,N_ELEMENTS(all(0,*))-1 DO IF (TOTAL(all(1:2,j)) MOD 2) EQ 0 THEN new=[[new],[all(*,j)]]
  all=new(*,1:N_ELEMENTS(new(0,*))-1)
ENDIF
IF KEYWORD_SET(B) THEN BEGIN
  new=[0,0,0]
  FOR j=0,N_ELEMENTS(all(0,*))-1 DO IF (TOTAL(all([0,2],j)) MOD 2) EQ 0 THEN new=[[new],[all(*,j)]]
  all=new(*,1:N_ELEMENTS(new(0,*))-1)
ENDIF
IF KEYWORD_SET(C) THEN BEGIN
  new=[0,0,0]
  FOR j=0,N_ELEMENTS(all(0,*))-1 DO IF (TOTAL(all(0:1,j)) MOD 2) EQ 0 THEN new=[[new],[all(*,j)]]
  all=new(*,1:N_ELEMENTS(new(0,*))-1)
ENDIF
IF KEYWORD_SET(screw) THEN IF N_ELEMENTS(screw) EQ 3 THEN BEGIN
  all=all(*,WHERE(all(0,*) MOD screw(0) EQ 0 OR TOTAL(all([1,2],*),1) NE 0))
  all=all(*,WHERE(all(1,*) MOD screw(1) EQ 0 OR TOTAL(all([0,2],*),1) NE 0))
  all=all(*,WHERE(all(2,*) MOD screw(2) EQ 0 OR TOTAL(all([1,0],*),1) NE 0))
ENDIF
IF KEYWORD_SET(glide) THEN IF N_ELEMENTS(glide) EQ 3 THEN BEGIN
  IF glide(0) THEN all=all(*,WHERE(all(glide(0)-1,*) MOD 2 EQ 0 OR all(0,*) NE 0))
  IF glide(1) THEN all=all(*,WHERE(all(glide(1)-1,*) MOD 2 EQ 0 OR all(1,*) NE 0))
  IF glide(2) THEN all=all(*,WHERE(all(glide(2)-1,*) MOD 2 EQ 0 OR all(2,*) NE 0))
ENDIF
IF KEYWORD_SET(n) THEN IF N_ELEMENTS(n) EQ 3 THEN BEGIN
  IF n(0) THEN all=all(*,WHERE(TOTAL(all([1,2],*),1) MOD 2 EQ 0 OR all(0,*) NE 0))
  IF n(1) THEN all=all(*,WHERE(TOTAL(all([0,2],*),1) MOD 2 EQ 0 OR all(1,*) NE 0))
  IF n(2) THEN all=all(*,WHERE(TOTAL(all([1,0],*),1) MOD 2 EQ 0 OR all(2,*) NE 0))
ENDIF
IF KEYWORD_SET(d) THEN IF N_ELEMENTS(d) EQ 3 THEN BEGIN
  IF d(0) THEN all=all(*,WHERE(TOTAL(all([1,2],*),1) MOD 4 EQ 0 OR all(0,*) NE 0))
  IF d(1) THEN all=all(*,WHERE(TOTAL(all([0,2],*),1) MOD 4 EQ 0 OR all(1,*) NE 0))
  IF d(2) THEN all=all(*,WHERE(TOTAL(all([1,0],*),1) MOD 4 EQ 0 OR all(2,*) NE 0))
ENDIF
h=all
IF KEYWORD_SET(cubic) THEN h=h(*,WHERE(h(0,*) le h(1,*) and h(1,*) le h(2,*)))
h=h(*,sort((max+1)*(max+1)*h(0,*)+(max+1)*h(1,*)+h(2,*)))
RETURN,h
END
