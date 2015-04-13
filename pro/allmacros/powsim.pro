FUNCTION powsim,uvw=u,hkl=h,abc=a,lambda=l,$
                int=i,bg=b,x=x,fwhm=f,scale=scale,$
                zeroshift=z,dspacing=d,multiplicity=K,$
                twotheta=t

; INPUT :  hkl          h
;          uvw          u
;
; OUTPUT : Multiplicity K
;          dspacing     d
;          Int. calc.   i
;          FWHM         f
;          2theta       t
;

  TAKE_DATP,datp
if not keyword_set(x) THEN x=indgen(1600)/10.

result=fltarr(n_elements(x))
help,h
d=a/SQRT(h(0,*)^2+h(1,*)^2+h(2,*)^2)
h=h(*,where(d gt l/2))
help,h
h=h(*,reverse(sort(d)))
d=d(where(d gt l/2))
d=d(reverse(sort(d)))
;h=h(*,where(d gt l/2))
d=REFORM(d,N_ELEMENTS(d))
t=2*asin(l/2/d)*180/!pi
;t=REFORM(t,N_ELEMENTS(t))

  ind=(UNIQ(t,sort(t)))
  K=intarr(N_ELEMENTS(d))
  FOR j=0,N_ELEMENTS(d)-1 DO BEGIN
    bid=WHERE(t EQ t(j),count)
    K(j)=K(j)+count
  ENDFOR

  IF NOT KEYWORD_SET(z) THEN z=0  ; Zeroshift like FullProf : 2tcalc=2tsim-Z

  f=fwhm(u,t(ind))
  i=K(ind)*scale
  
  c={d:d(ind),bg:N_ELEMENTS(b)}
  a=[i,f,b,l,z]
  pow_funct,x,a,result,c=c
  
  f=fwhm(u,t(ind))
  i=K*scale
  FOR j=0,N_ELEMENTS(d)-1 DO BEGIN
    ;bid=WHERE(t(ind) EQ t(j),count)
    i(j)=a(WHERE(t(ind) EQ t(j),count))
  ENDFOR
  i=i/k

  e=sqrt(result)
  for j=0,n_elements(d)-1 do print,h(0,j),h(1,j),h(2,j),d(j),t(j),i(j),k(j)
  plot,x, result,tit='simulation'
MOD_DATP,datp,'x',x
MOD_DATP,datp,'e',e
GIVE_DATP,datp
RETURN,w
END


