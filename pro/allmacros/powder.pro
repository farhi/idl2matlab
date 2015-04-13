; $Id: gaussfit.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO	pow_funct,X,A,F,PDER,C=C
; 2qtrue = 2qexp.-ZER => 2tsim = 2tcalc + ZER
; Up to now only
; Refinement of intensity and width of each peak, wavelength, zeroshift & background 
 ON_ERROR,2                       ;Return to caller if an error occurs
 aaa=0
 jj=0
 jjj=0
 FOR j=0,N_ELEMENTS(c.f)-1 DO IF c.f(j) GT 0 THEN BEGIN
    aaa=[aaa,c.p(jj)] 
    jj=jj+1
 ENDIF ELSE BEGIN
    aaa=[aaa,a(jjj)] 
    jjj=jjj+1
 ENDELSE  ; free and fixed parameters split up in two arrays
 aaa=aaa(1:N_ELEMENTS(aaa)-1)
 peaks=C.n(3)                                                ; number of peaks
 lambda=aaa(TOTAL(c.n(0:7)) :TOTAL(c.n(0:8))-1)              ; wavelength
 scale =aaa(TOTAL(c.n(0:11)):TOTAL(c.n(0:12))-1)             ; wavelength
 ZER   =aaa(TOTAL(c.n(0:8)) :TOTAL(c.n(0:9))-1)              ; FullProf : 2ttrue = 2texp.-ZER => 2tsim = 2tcalc + ZER
 ZER   =ZER(0)                                               ; only one zeroshift and wavelenght yet
 b     =[aaa(TOTAL(c.n(0:6)):TOTAL(c.n(0:7))-1),0,0,0,0,0,0] ; background polynom
 FOR j=0,N_ELEMENTS(lambda)-1 DO BEGIN
   l=lambda(j)
   s=scale(j<(N_ELEMENTS(scale)-1))
   t=aaa(TOTAL(c.n(0:2)):TOTAL(c.n(0:3))-1)
   t=2*asin(l/2/t)*180/!pi + ZER                          ; peak position  (2theta)
                                                          ; ATTENTION : d > l/2 !! (esp. in fits if l is refinable)
   AA=FLTARR(peaks,3)                                     ; Gaussian parameters
   AA(*,0)=aaa(TOTAL(c.n(0:4)):TOTAL(c.n(0:5))-1)         ; peak intensity (integrated)
   AA(*,1)=t                                              ; peak position  (2theta)
   AA(*,2)=aaa(TOTAL(c.n(0:5)):TOTAL(c.n(0:6))-1)         ; peak width     (FWHM(2theta))
   Z = FLTARR(N_ELEMENTS(X),N_ELEMENTS(a(*,0))) + 10.
  	FOR i=0,N_ELEMENTS(AA(*,0))-1 DO IF AA(i,2) GT 0 THEN Z(*,i)=(X-AA(i,1))/AA(i,2)*2*SQRT(2*ALOG(2)) 	;GET Z
   IF j EQ 0 THEN EZ = FLTARR(N_ELEMENTS(X))
  	FOR i=0,N_ELEMENTS(AA(*,0))-1 DO BEGIN
     EZ=EZ+s*2*SQRT(ALOG(2)/!pi)/AA(i,2)*AA(i,0)*EXP(-(Z(*,i)*(ABS(Z(*,i)) LE 7.))^2/2.)*(ABS(Z(*,i)) LE 7.) ; GAUSSIAN PART IGNORE SMALL TERMS
   ENDFOR
 ENDFOR
	F = EZ + b(0) + b(1)*X + b(2)*X^2 + b(3)*X^3 + b(4)*X^4 + b(5)*X^5;FUNCTIONS.
	IF N_PARAMS(0) LE 3 THEN BEGIN
	  RETURN 
	ENDIF;NEED PARTIAL?
;
; These values  are not yet correct! (still from former GAUSS_FUNC)
;
	PDER = FLTARR(N_ELEMENTS(X),N_ELEMENTS(A)) ;YES, MAKE ARRAY.
 FOR j=0,peaks-1 DO BEGIN ; integrated intensity
	  ;PDER(0,0) = EZ		;COMPUTE PARTIALS
 ENDFOR
 FOR j=peaks,2*peaks-1 DO BEGIN ; FWHM
	  if a(2) ne 0. then PDER(0,1) = A(0) * EZ * Z/A(2)
	  PDER(0,2) = PDER(*,1) * Z
 ENDFOR
 FOR j=2*peaks,2*peaks+c.bg-1 DO BEGIN ; background
	  IF c.bg GE 1 THEN PDER(*,j)   = 1.
	  IF c.bg GE 2 THEN PDER(*,j+1) = X
	  IF c.bg GE 3 THEN PDER(*,j+2) = X^2
	  IF c.bg GE 4 THEN PDER(*,j+3) = X^3
	  IF c.bg GE 5 THEN PDER(*,j+4) = X^4
	  IF c.bg GE 6 THEN PDER(*,j+5) = X^5
 ENDFOR
 j=2*peaks+c.bg   ; wavelength
 j=2*peaks+c.bg+1 ; zeroshift
	RETURN
END

FUNCTION powder,   w,sigmaa=sigmaa,uvw=u,hkl=h,abc=abc,lambda=l,$
                   int=i,bg=b,x=x,fwhm=f,scale=scale,$
                   zeroshift=z,dspacing=d,multiplicity=K,$
                   twotheta=t,iterations=iterations,key=key,$
                   fixedblock=fixedblock,correct=correct,datp=datp_,pseudo=pseudo,true=true 
;+
;uvw=[],1.92,-1.40,0.40579]&i=0&f=0&d=0&h=hkl(/si)&s=1000&a=5.43094&l=1.304947&b=1000&z=-.3494
; w3=powder(y2,in=i,fw=f,/cor,ab=a,la=l,sc=s,bg=b,tw=t,ds=d,m=m,it=3,u=u,hk=h,datp=datp2,/pseudo)
;-
on_error,2                      ;Return to caller if an error occurs
IF NOT KEYWORD_SET(true) THEN true=0
IF NOT KEYWORD_SET(pseudo) THEN pseudo=0
colors,color,pseudo=pseudo,true=true
IF NOT KEYWORD_SET(datp_) THEN TAKE_DATP,datp ELSE datp=datp_
x=datp.x
IF NOT KEYWORD_SET(b_damp) THEN b_damp=.5
lambda=l
l=max(l)
IF KEYWORD_SET(h) THEN BEGIN 
  IF N_ELEMENTS(h) GE 3 THEN d=abc(0)/SQRT(h(0,*)^2+h(1,*)^2+h(2,*)^2)
 ; h=h(*,where(d gt l/2))
  tmp=(where(d gt l/2))
  h=h(*,reverse(tmp(sort(d(tmp)))))
ENDIF
d=d(where(d gt l/2))
d=d(reverse(sort(d)))
d=REFORM(d,N_ELEMENTS(d))
t=2*asin(l/2/d)*180/!pi
l=lambda
t=t(WHERE(t LE datp.x(N_ELEMENTS(datp.x)-1)))
d=d(WHERE(t LE datp.x(N_ELEMENTS(datp.x)-1)))
IF KEYWORD_SET(h) THEN h=h(*,(WHERE(t LE datp.x(N_ELEMENTS(datp.x)-1))))
ind=(UNIQ(t,sort(t)))
K=intarr(N_ELEMENTS(d))
FOR j=0,N_ELEMENTS(d)-1 DO BEGIN
    bid=WHERE(t EQ t(j),count)
    K(j)=K(j)+count
ENDFOR
IF NOT KEYWORD_SET(z)         THEN z=0         ; Zeroshift like FullProf : 2tcalc=2tsim-Z
IF NOT KEYWORD_SET(scale)     THEN scale=1     
IF N_ELEMENTS(scale) EQ 1     THEN BEGIN
  scale=scale(0)
  IF scale LE 0  THEN scale=1  
ENDIF
IF NOT KEYWORD_SET(u)         THEN u=[0,0,.3]  ; 
IF NOT KEYWORD_SET(b)         THEN b=0
IF NOT KEYWORD_SET(f)         THEN f=fwhm(u,t(ind)) ELSE IF N_ELEMENTS(f) EQ N_ELEMENTS(d) THEN f=f(ind) ELSE f=fwhm(u,t(ind))
IF NOT KEYWORD_SET(i)         THEN i=K(ind)         ELSE IF N_ELEMENTS(i) EQ N_ELEMENTS(d) THEN i=i(ind) ELSE i=K(ind)& 
Params     =[REFORM(h(0,*),N_ELEMENTS(h(0,*))),REFORM(h(1,*),N_ELEMENTS(h(1,*))),REFORM(h(2,*),N_ELEMENTS(h(2,*))),d(ind),t(ind),i,f,b,l,z,abc,u,scale]
NbOfParams =[N_ELEMENTS(h(0,*)),N_ELEMENTS(h(0,*)),N_ELEMENTS(h(0,*)),N_ELEMENTS(d(ind)),N_ELEMENTS(t(ind)),N_ELEMENTS(i),N_ELEMENTS(f),N_ELEMENTS(b),N_ELEMENTS(l),N_ELEMENTS(z),N_ELEMENTS(abc),N_ELEMENTS(u),N_ELEMENTS(scale)]
TotalNbOfParams=NbOfParams
NbOfParams([0,1,2,4,10,11])=0
IF NOT KEYWORD_SET (fixedblock) THEN fixedBlock =[1,1,1,1,1,0,0,1,0,0,1,1,1]
IF N_ELEMENTS(fixedblock) NE 13 THEN fixedBlock =[1,1,1,1,1,0,0,1,0,0,1,1,1]
block = (WHERE  (NbOfParams GT 0,count)) 
index=TotalNbOfParams
altindex=NbOfParams
fixed=INTarr(TOTAL(NbOfParams))
FOR j=0,N_ELEMENTS(TOTALNbOfParams)-1 DO index(j)=TOTAL(TotalNbOfParams(0:j))
FOR j=0,N_ELEMENTS(NbOfParams)-1 DO altindex(j)=TOTAL(NbOfParams(0:j))
FOR j=0,N_ELEMENTS(block)-1 DO IF N_ELEMENTS(a) LT 1 THEN a=Params(index(block(j))-NbOfParams(block(j)):index(block(j))-1) ELSE BEGIN
    a=[a,Params(index(block(j))-NbOfParams(block(j)):index(block(j))-1)]  
ENDELSE
FOR j=0,N_ELEMENTS(block)-1 DO BEGIN
    fixed(altindex(block(j))-NbOfParams(block(j)):altindex(block(j))-1) = fixedBlock(block(j))
ENDFOR
comment =   ['h','k','l','dspacings','twotheta','intensities','widths','background','wavelength','zeroshift','unit cell','Cagliotti','scale']
par=0
var=0
old=a
FOR j=0,N_ELEMENTS(fixed)-1 DO IF fixed(j) EQ 0 THEN var=[var,a(j)] ELSE par=[par,a(j)] ; free and fixed parameters split up in two arrays
var =var(1:N_ELEMENTS(var)-1) ; ... and if nothing is to be refined ???
par=par(1:N_ELEMENTS(par)-1)
help,var,par
key={p:par,f:fixed,n:nbOfParams}

pow_funct,x,var,result,c=key

e=sqrt(result)
mod_datp,datp,'e',e
IF NOT KEYWORD_SET(datp_) THEN give_datp,datp
plot,x,w,tit='1st guess',/xstyle,back=color(4),col=color(3)
oplot,x,result,color=color(1)

; free and fixed parameters split up in two arrays => put it together again ...
a=0
jj=0
jjj=0
FOR j=0,N_ELEMENTS(fixed)-1 DO IF fixed(j) GT 0 THEN BEGIN
    a=[a,par(jj)] 
    jj=jj+1
ENDIF ELSE BEGIN
    a=[a,var(jjj)] 
    jjj=jjj+1
ENDELSE  ; free and fixed parameters split up in two arrays
a=a(1:N_ELEMENTS(a)-1)
peaks=nbofparams(3)                                       ; number of peaks
l    =a(TOTAL(nbofparams(0:7)):TOTAL(nbofparams(0:8))-1)  ; wavelength
scale=a(TOTAL(nbofparams(0:11)):TOTAL(nbofparams(0:12))-1); wavelength
Z    =a(TOTAL(nbofparams(0:8)):TOTAL(nbofparams(0:9))-1)  ; FullProf : 2ttrue = 2texp.-ZER => 2tsim = 2tcalc + ZER
Z    =Z(0)                                                ; only one zeroshift and wavelenght yet
t    =2*asin(l(0)/2/d)*180/!pi + Z                        ; peak position  (2theta)
                                                          ; ATTENTION : C.d > l/2 !! (esp. in fits if l is refinable)
b    =[a(TOTAL(nbofparams(0:6)):TOTAL(nbofparams(0:7))-1)]; background polynom
i=FLTARR(N_ELEMENTS(d))
f=i
FOR j=0,N_ELEMENTS(d)-1 DO BEGIN
    i(j)=a(TOTAL(nbofparams(0:4))+WHERE(d(ind) EQ d(j),count))
    f(j)=a(TOTAL(nbofparams(0:5))+WHERE(d(ind) EQ d(j),count))
ENDFOR
i=i/k
print,'  h, k, l,          d,     2theta,       fwhm,  intensity, mult.
for j=0,n_elements(d)-1 do print,FORMAT="(3I3,4F12.3,I3)",h(0,j),h(1,j),h(2,j),d(j),t(j),f(j),i(j),k(j)
print,'1st guess, zeroshift=',+strcompress(z)
FOR j=0,N_ELEMENTS(l)-1 DO print,'lambda='+strcompress(l(j))+', scale=',+strcompress(scale(j<(N_ELEMENTS(scale)-1)))
 
IF NOT KEYWORD_SET (iterations) THEN iterations=0 

FOR iteration=1,iterations DO BEGIN
  ;help,datp,/stru,w,var
  result=powfit(datp.x,w,w/datp.e,var,sigmaa,function_name = "POW_FUNCT",key=key,/noder,itmax=1,chi2=chi2) ;call powfit

  help,var,par

  e=sqrt(result)
  mod_datp,datp,'e',e
  IF NOT KEYWORD_SET(datp_) THEN give_datp,datp
  plot,x,w,tit='iteration no.'+strcompress(iteration)+', chi2='+strcompress(chi2),/xstyle,back=color(4),col=color(3)
  oplot,x,result,color=color(1)

  ; free and fixed parameters split up in two arrays => put it together again ...
  a=0
  jj=0
  jjj=0
  FOR j=0,N_ELEMENTS(fixed)-1 DO IF fixed(j) GT 0 THEN BEGIN
    a=[a,par(jj)] 
    jj=jj+1
  ENDIF ELSE BEGIN
    a=[a,var(jjj)] 
    jjj=jjj+1
  ENDELSE  ; free and fixed parameters split up in two arrays
  a=a(1:N_ELEMENTS(a)-1)
  peaks=nbofparams(3)                                       ; number of peaks
  l    =a(TOTAL(nbofparams(0:7)):TOTAL(nbofparams(0:8))-1)  ; wavelength
  scale=a(TOTAL(nbofparams(0:11)):TOTAL(nbofparams(0:12))-1); scale
  Z    =a(TOTAL(nbofparams(0:8)):TOTAL(nbofparams(0:9))-1)  ; FullProf : 2ttrue = 2texp.-ZER => 2tsim = 2tcalc + ZER
  Z    =Z(0)                                                ; only one zeroshift and wavelenght yet
  t    =2*asin(l(0)/2/d)*180/!pi + Z                        ; peak position  (2theta)
                                                            ; ATTENTION : C.d > l/2 !! (esp. in fits if l is refinable)
  b  =[a(TOTAL(nbofparams(0:6)):TOTAL(nbofparams(0:7))-1)] ; background polynom
  i=FLTARR(N_ELEMENTS(d))
  f=i
  FOR j=0,N_ELEMENTS(d)-1 DO BEGIN
    i(j)=a(TOTAL(nbofparams(0:4))+WHERE(d(ind) EQ d(j),count))
    f(j)=a(TOTAL(nbofparams(0:5))+WHERE(d(ind) EQ d(j),count))
  ENDFOR
  i=i/k
  print,'  h, k, l,          d,     2theta,       fwhm,  intensity, mult.
  for j=0,n_elements(d)-1 do print,FORMAT="(3I3,4F12.3,I3)",h(0,j),h(1,j),h(2,j),d(j),t(j),f(j),i(j),k(j)
  print,'iteration no.'+strcompress(iteration)+', zeroshift=',+strcompress(z)+', chi2='+strcompress(chi2)+', bg=',+strcompress(b)
  FOR j=0,N_ELEMENTS(l)-1 DO print,'lambda='+strcompress(l(j))+', scale=',+strcompress(scale(j<(N_ELEMENTS(scale)-1)))
  IF KEYWORD_SET(correct) THEN BEGIN
    ; Now correct non-realistic values (negative peaks etc.)
    for j=0,nbofparams(5) DO BEGIN
      if a(TOTAL(nbofparams(0:4))+j) LE 0 THEN BEGIN
        a(TOTAL(nbofparams(0:4))+j)=0
        fixed(TOTAL(nbofparams(0:4))+j)=1
        IF nbofparams(3) GT 0 THEN fixed(TOTAL(nbofparams(0:2))+j)=1
        IF nbofparams(4) GT 0 THEN fixed(TOTAL(nbofparams(0:3))+j)=1
        IF nbofparams(6) GT 0 THEN fixed(TOTAL(nbofparams(0:5))+j)=1
      ENDIF
    ENDFOR
    par=0
    var=0
    old=a
    FOR j=0,N_ELEMENTS(fixed)-1 DO IF fixed(j) EQ 0 THEN var=[var,a(j)] ELSE par=[par,a(j)] ; free and fixed parameters split up in two arrays
    var =var(1:N_ELEMENTS(var)-1) ; ... and if nothing is to be refined ???
    par=par(1:N_ELEMENTS(par)-1)
    key={p:par,f:fixed,n:nbOfParams}
    help,var,par

    pow_funct,x,var,result,c=key

    e=sqrt(result)
    mod_datp,datp,'e',e
    IF NOT KEYWORD_SET(datp_) THEN give_datp,datp
    plot,x,w,tit='1st guess',/xstyle,back=color(4),col=color(3)
    oplot,x,result,color=color(1)

    ; free and fixed parameters split up in two arrays => put it together again ...
    a=0
    jj=0
    jjj=0
    FOR j=0,N_ELEMENTS(fixed)-1 DO IF fixed(j) GT 0 THEN BEGIN
        a=[a,par(jj)] 
        jj=jj+1
    ENDIF ELSE BEGIN
        a=[a,var(jjj)] 
        jjj=jjj+1
    ENDELSE  ; free and fixed parameters split up in two arrays
    a=a(1:N_ELEMENTS(a)-1)
    peaks=nbofparams(3)                                       ; number of peaks
    l    =a(TOTAL(nbofparams(0:7)):TOTAL(nbofparams(0:8))-1)  ; wavelength
    scale=a(TOTAL(nbofparams(0:11)):TOTAL(nbofparams(0:12))-1); wavelength
    Z    =a(TOTAL(nbofparams(0:8)):TOTAL(nbofparams(0:9))-1)  ; FullProf : 2ttrue = 2texp.-ZER => 2tsim = 2tcalc + ZER
    Z    =Z(0)                                                ; only one zeroshift and wavelenght yet
    t    =2*asin(l(0)/2/d)*180/!pi + Z                        ; peak position  (2theta)
                                                              ; ATTENTION : C.d > l/2 !! (esp. in fits if l is refinable)
    b  =[a(TOTAL(nbofparams(0:6)):TOTAL(nbofparams(0:7))-1)]  ; background polynom
    i=FLTARR(N_ELEMENTS(d))
    f=i
    FOR j=0,N_ELEMENTS(d)-1 DO BEGIN
        i(j)=a(TOTAL(nbofparams(0:4))+WHERE(d(ind) EQ d(j),count))
        f(j)=a(TOTAL(nbofparams(0:5))+WHERE(d(ind) EQ d(j),count))
    ENDFOR
    i=i/k
    print,'  h, k, l,          d,     2theta,       fwhm,  intensity, mult.
    for j=0,n_elements(d)-1 do print,FORMAT="(3I3,4F12.3,I3)",h(0,j),h(1,j),h(2,j),d(j),t(j),f(j),i(j),k(j)
    print,'corrected guess, zeroshift=',+strcompress(z)+', bg=',+strcompress(b)
    FOR j=0,N_ELEMENTS(l)-1 DO print,'lambda='+strcompress(l(j))+', scale=',+strcompress(scale(j<(N_ELEMENTS(scale)-1)))
    oplot,x,result,col=color(6)
  ENDIF
ENDFOR
MOD_DATP,datp,'x',x
MOD_DATP,datp,'e',e
MOD_DATP,datp,'p',[l,z,scale]
MOD_DATP,datp,'par_txt',['lambda     ', 'zeroshift','scale   ']
IF NOT KEYWORD_SET(datp_) THEN GIVE_DATP,datp
RETURN,result
END

