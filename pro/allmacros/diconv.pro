;******* ******   *
function di_rev , c
;******* ******   *
;******* ******   *
if (size(c))(0) eq 2 then return, REVERSE(REVERSE(c,1),2) $
		     else return, REVERSE(c)
end

;** ******   ************** Return s1,s4 margin & s2,s3 vector index range
pro di_mka , c ,s1,s2,s3,s4
;** ******   ************** **********************************************
;** ******   ************** **********************************************


    tm= findgen(n_elements(c))
    i = round  (total(tm*c)/total(c))
    id= where  (c gt 0)   & nc= n_elements(id)
    i = i-id(0)
    s = i-(nc-i-1)
    s1=((-s)>0)+1  & s4=((+s)>0)+1
    s2= id(0)	   & s3=id(nc-1)
end

;******* ******   *****  ** Construcs the vector in all directions of the plane
function di_two , v1,v2 ,fl
;******* ******   *****  ** ***************************************************
;******* ******   *****  ** ***************************************************
common di_com, di_siz,di_sin,di_cos,di_t1,di_t2,di_r1,di_r2,di_th1,di_th2,di_bo

if not fl then return,v1#v2

n1=n_elements(v1) & n2=n_elements(v2)

if n_elements(di_siz) ne 2 then di_siz=[0,0]
if (di_siz(0) ne n1)  or (di_siz(1) ne n2) then begin
    di_siz= [n1,n2]
    di_t1 = fltarr(n1)+1.     & di_t2 =fltarr(n2)+1.
    di_th1= fltarr(n1,n2)     & di_th2=fltarr(n1,n2)

    a	  = !pi/n2 & b=!pi/n1 & m1=(n1-1)/2 & m2=(n2-1)/2 &  di_bo=[-m1,-m2,m1,m2]
    for j = 0,n2-1  do begin di_th1(0:m1-1,j)=!pi  +a*j & di_th1(m1:*,j)=a*j	     & endfor
    for i = 0,n1-1  do begin di_th2(i,0:m2-1)=!pi/2+b*i & di_th2(i,m2:*)=-!pi/2+b*i  & endfor

    di_sin= abs(findgen(n1)-m1) # di_t2	    & di_r1 =di_sin
    di_cos= di_t1 # abs(findgen(n2)-m2)	    & di_r2 =di_cos
    di_sin(m1,*)=1.
    di_cos= ATAN(di_cos/di_sin)		    & di_cos(m1,*)=!pi/2.
    di_sin= SIN (di_cos)^2		    & di_cos=COS(di_cos)^2
    di_sin(m1,m2)=.5			    & di_cos(m1,m2) =.5
endif
r1=v1#di_t2				    & r2=di_t1#v2
r1=POLAR_SURFACE(r1,di_r1,di_th1,sp=[1,1],bo=di_bo)
r2=POLAR_SURFACE(r2,di_r2,di_th2,sp=[1,1],bo=di_bo)

return,r1*di_cos + r2*di_sin
end

;******* *******  *********** Solve linear or bilinear function (coef. of convolution steps)
function di_solv, R,bi,R2,bi2,METHOD=met,GUESS=guess
;******* *******  *********** **************************************************************
;******* *******  *********** **************************************************************
s =size (R)
tb=total(bi) & t1=tb/total(R)
nc=s(1) & mi=(nc-1)/2

   A = dblarr(nc,nc) & B=dblarr(nc)
   if  n_elements(bi) eq 1 then  B (mi)=bi else B=bi
   for i = 0  ,mi   do A(0,i)   =R (mi-i:nc-1)         ;fill each coefficient
   for i =mi+1,nc-1 do A(i-mi,i)=R (0   :nc-1-(i-mi))  ;fill each coefficient

if n_elements(met) ne 1 then met=1

if met eq 1 then begin
	SVDC,A,w,u,v
	CZ = SVSOL(u,w,v,B)
   
endif else if met eq 2 then begin ; un-used methode
	CZ = CRAMER(A,B)

endif else if met eq 3 then begin
	if n_elements(guess) gt 1 then X = guess else  $
	X  = -R & X(mi)=X(mi)+total(R)+total(bi)/total(R)
	CZ = LINBCG(sprsin(A),B,X)

endif else if met eq 4 then begin
	LUDC,A,Index
	CZ = LUSOL(A,Index,B)
   
endif else if met eq 5 then begin AA=A
	if n_elements(guess) gt 1 then X = guess else  $
	X  = -R & X(mi)=X(mi)+total(R)+total(bi)/total(R)
	LUDC,AA,Index
	CZ = LUMPROVE(A,AA,Index,B,X)
endif

if n_elements(R2) gt 1 then begin
   s = size (R2)
   tb=total(bi2) & t2=tb/total(R2)
   n2= s(1) & m2=(n2-1)/2
   A = dblarr(n2,n2) & B=dblarr(n2)
   if  n_elements(bi2) eq 1 then B (m2)=bi2 else B=bi2
   for i = 0  ,m2   do A(0,i)   =R2(m2-i:n2-1)
   for i =m2+1,n2-1 do A(i-m2,i)=R2(0   :n2-1-(i-m2))
   SVDC,A,w,u,v
   CZ2= SVSOL(u,w,v,B)
  ;CZ2= CZ2 + (t2-total(CZ2))/n2

   CZ = DI_TWO(CZ,CZ2,1)                               ;make 2D function
endif
return,CZ
end

;******* *******  *** Just do a convolution
function di_conv, F,C
;******* *******  *** *********************
;******* *******  *** *********************
sf =size(F) & sc =size(C)
nf1=sf(1)   & nc1=sc(1) & mi=(nc1-1)/2

if sf(0) eq 1 then begin
   if  nf1 gt nc1  then return,  CONVOL(	    F	         ,C) $
		   else begin  t=CONVOL([fltarr(mi),F,fltarr(mi)],C)
			return,t(mi:nf1-1+mi) & endelse
endif else begin
   nf2=sf(2) & nc2=sc(2) & ni=(nc2-1)/2
   if (nf1 gt nc1) and (nf2 gt nc2) then return,CONVOL(F   ,C) $
   else begin
	if nf1 le nc1 then nf1=nf1+2*mi else mi=0
	if nf2 le nc2 then nf2=nf2+2*ni else ni=0
	tmp=fltarr(nf1,nf2) & tmp(mi,ni)=F & t =CONVOL(tmp ,C)
	return,t(mi:nf1-1-mi,ni:nf2-1-ni)
   endelse
endelse
end

;******* *******  ************* Derivatives: return all max values
function di_keeb, V ,mi, D ,idx ,shf
;******* *******  ************* **********************************
;******* *******  ************* **********************************
gi =mi/4
s  =size (V)
if s(0) eq 1 then begin
   v1 =shift(V, 1)     & res=    ((V-v1)>0) & res(0)       =0
   v1 =shift(V,-1)     & res=res*((V-v1)>0) & res(s(1)-1)  =0
endif else begin
   v1 =shift(V, 1, 0)  & res=    ((V-v1)>0) & res(0,*)     =0
   v1 =shift(V,-1, 0)  & res=res*((V-v1)>0) & res(s(1)-1,*)=0
   v1 =shift(V, 0,-1)  & res=res*((V-v1)>0) & res(*,0)     =0
   v1 =shift(V, 0, 1)  & res=res*((V-v1)>0) & res(*,s(2)-1)=0
endelse

idx=where(res gt 0)
if  idx(0) eq -1 then bid=max(V,idx)
shf=idx*0

;Gravity center for better peak positions.
;******* ****** *** ****** **** *********
if gi gt 1 then begin ni=n_elements(idx) & l1=s(1)-1 & l2=s(2)-1
	for i=0,ni-1 do begin
	    if s(0) eq 1 then begin
		hf=(idx(i)-gi)>0 & hl=(idx(i)+gi)<l1
		gd=(D(hf:hl))
		gd=(gd-min(gd))
		gx=findgen(hl-hf+1)+hf
;		shf(i)=idx(i)-ROUND((total(gd*gx)/total(gd)+idx(i))/2.)
		shf(i)=idx(i)-ROUND((total(gd*gx)/total(gd)))
		;print,idx(i),total(gd*gx)/total(gd)
	    endif else begin
	        iy= idx(i)/s(2)  & ix=idx(i)-iy*s(2)
		hf=(ix-gi)>0     & hl=(ix+gi) <l1
		vf=(iy-gi)>0     & vl=(iy+gi) <l2
		gd=(D(hf:hl,vf:vl))
		gd=(gd-min(gd))
		gh=total(gd,2)   & gv=total(gd,1)
		gx=findgen(hl-hf+1)+hf & gy=findgen(vl-vf+1)+vf
		tx=ROUND((total(gh*gx)/total(gh)))
		ty=ROUND((total(gv*gy)/total(gv)))
		shf(i)=idx(i)-(tx+ty*s(2))
	    endelse
	endfor
	res(*)=0
endif

res(idx+shf)=V(idx)
idx=idx+shf
return,res
end

;******* ********  *********  ***** Make a 1 or 2 dimensional Gaussian
function di_gauss, h,hw1,hw2 ,y1,y2
;******* ********  *********  ***** **********************************
;******* ********  *********  ***** **********************************
if hw1 gt 0 then begin
   n1= round(hw1*10)+1
   y1= h*exp(-(((findgen(n1)-n1/2)/hw1)^2)/2)
   y1= y1/total(y1) & y1=y1>2e-5 & y1=y1-2e-5 & y1=y1/total(y1)
   y1= y1(where(y1 gt 0))
   y2= 0
endif
if hw2 gt 0 then begin
   n2= round(hw2*10)+1
   y2= h*exp(-(((findgen(n2)-n2/2)/hw2)^2)/2)
   y2= y2/total(y2) & y2=y2>2e-5 & y2=y2-2e-5 & y2=y2/total(y2)
   y2= y2(where(y2 gt 0))
   y3= DI_TWO  (y1,y2,0)
   return,y3/total(y3)
endif
   return,y1
end

;************************************************
function diconv , d ,cv ,pos=idx ,filt=filt ,Xdata=xx ,Xprofil=xcv, stat=stat, get_profil=prof $
		, h_hw=h_hw,v_hw=v_hw,orient=a_deg ,sym=sym, unit=unit, bg=bg, res=cd, cutoff=cut
;******* ******					*
;						*
;** d    = input data				*
;** cv   = optionnal observed resolution profil	*
;** h_hw = optionnal horiz half_width profil	*
;** v_hw = optionnal verti half_width profil	*
;** a_deg= degrees of profil:clockwise rotation *
;** xx   = abcissa of input if not regular      *
;** xcv  = abcissa of input profil if unregular *
;** unit = regular unit cell                    *
;** /sym   make cv symetrical                   *
;** /filt  use FOU function to remove stat.     *
;** /stat  use FOU function  & return.          *
;** /prof  return normalized profil only        *
;** /cut   low cutoff limit (max value factor   *
;                                               *
;** return F(d)					*
;** return idx the positions of bands           *
;** return bg  the calculated background        *
;**                                             *
;** 		D.Richard Apr 1996 (ILL)	*
;************************************************
;** 2D gaussian with a_deg not implemented !!
;** gravity in keeb for 2D data to check !!
;** Errors for d 
;** cv & d same unit !!

common c_conv, cz, fou ,c, kcv, kh_hw, kv_hw, ku, kdeg

;** Adjustment variables:
    LC=2e-4      ;**Minimum value set to 0 in normalised CV (use cut if defined)
    F =1./1000.  ;**Delta for CZ calculation (c+|eps)~CZ=|1 (self adjust)
    LS=5.        ;**Lowest limit factor for second maximum in CZ

sd = size(d)  & xn=sd(1) & if sd(0) eq 2 then yn=sd(2) else yn=1
ncv= n_elements(cv)
w  = 0

;CHECK PARAMETERS resol:-1=bad, 0=given, 1=1Dgauss, 2=2Dgauss
;***** **********
resol=-1
use  = 1

if (n_elements(unit) ne 1)  then if (n_elements(xcv) eq ncv)   then unit=min(abs(shift(xcv,-1)-xcv)) else $
				 if (n_elements(xx)  eq xn )   then unit=min(abs(shift(xx ,-1)-xx) ) else unit=1
if (n_elements(ku)   eq 0)  then ku  =0  &  if (unit ne ku )   then use =0 & ku=unit

if (n_elements(a_deg) ne 1) or (yn eq 1)  then o_deg=0 else o_deg=a_deg
if (n_elements(kdeg)  eq 0) then kdeg=0  &  if o_deg ne kdeg then use =0 & kdeg=o_deg

if keyword_set(h_hw) then    begin
   if h_hw  gt 0 then begin  resol=1 & if n_elements(kh_hw) ne 1 then use=0 else if kh_hw ne h_hw then use=0
      if yn gt 1 then begin  resol=2
	 if not  keyword_set(v_hw) then v_hw=h_hw else $
	 if v_hw le 0 then   v_hw =h_hw
	 if n_elements(kv_hw) ne 1 then use=0 else if kv_hw ne v_hw then use=0
      endif
     ;MAKE PROFIL IF NOT GIVEN.
     ;*********** ** *** ***** 
      if not use then begin
	 if  resol eq 1 then c=DI_GAUSS(1.,h_hw/unit,0        ,y1,0 ) & kh_hw=h_hw	;1D gaussian
	 if  resol eq 2 then c=DI_GAUSS(1.,h_hw/unit,v_hw/unit,y1,y2) & kv_hw=v_hw	;2D gaussian
      endif
      kcv=0 & sym=1
   endif

endif else if ncv ge 3  then  if (size(cv))(0) eq sd(0)  then begin
      resol=0 & kh_hw=0 & kv_hw=0
      sym=keyword_set(sym)
      if ncv ne n_elements(kcv) then use=0 else if total(cv-kcv) ne 0 then use=0
      if not use then begin
     ;CENTER GIVEN RESOLUTION PROFIL.
     ;****** ***** ********** ******
	c = cv>0  & c=c-min(c) & c = c/total(c)
	if n_elements(cut) eq 1 then lc=max(c)*cut
	c=c>lc & c=c-lc & c=c/total(c)
	
	if (size(cv))(0) eq 1 then begin
		if n_elements(xcv) le 1 then if n_elements(xx) gt 1 then xcv=xx
		if n_elements(xcv) eq ncv then begin
			n =(xcv(ncv-1)-xcv(0))/unit +1
			co=INTERPOL(c,xcv,findgen(n)*unit + xcv(0))
			c =co/total(co)
		endif
		DI_MKA,c, s1,s2,s3,s4
		c =[fltarr(s1) , c(s2:s3) , fltarr(s4)]

	endif else begin
		if o_deg ne 0 then begin ct=ROT(c,-o_deg,/interp)
		                 ca=total(ct,2)  & cb=total(ct,1)
		endif else begin ca=total(c ,2)  & cb=total(c ,1) & endelse
		DI_MKA,ca, s1,s2,s3,s4 & sa=s1+s3-s2+s4+1
		DI_MKA,cb, s5,s6,s7,s8 & sb=s5+s7-s6+s8+1
    
		if o_deg ne 0 then begin sm=max([sa,sb])  & s1=s1+(sm-sa)/2 & sa=sm
					 s5=s5+(sm-sb)/2  & sb=sm & endif
		cc=fltarr( sa , sb)
		cc(s1,s5)=c(s2:s3,s6:s7) & c=cc
	endelse
	if sym then c=(DI_REV(c)+c)/2.
	kcv=cv
      endif
endif

nc= n_elements(c) & mi=(nc-1)/2 & xi=mi
if  yn gt 1 then xi=(((size(c))(1))-1)/2
                 yi=(((size(c))(2))-1)/2
                 
IF  resol lt 0        THEN RETURN,w
IF  keyword_set(prof) THEN RETURN,c
IF  nc gt xn*yn/2     THEN RETURN,w

;CZ IS A DECONVOLUTION FUNCTION OF PROFILE+DELTA (R+|f)
;** ** * ************* ******** ** ********************
;(R+|f)~CZ= |1
;*************
    if not use then begin ok=0
    	WHILE not ok do begin
	if yn eq 1 then begin R =c  & R (mi)=R (mi)+f  & CZ=di_solv(R ,1.)
	endif	   else begin R1=y1 & R1(xi)=R1(xi)+f
			      R2=y2 & R2(yi)=R2(yi)+f  & CZ=di_solv(R1,1.,R2,1.)  & endelse
	rcz=CZ & rcz(mi)=0
	if (CZ(mi)/max(rcz))  lt LS then f=f*2. else ok=1
	ENDWHILE
	if o_deg  ne 0  then  CZ=ROT(CZ,o_deg,/interp)
	if o_deg  ne 0  then  C =ROT(C ,o_deg,/interp)

	;FOU IS A FUNCTION USED TO REMOVE STATISTICS
	;*** ** * ******** **** ** ****** **********
   	FOU=-CZ*f
   	FOU(mi)=fou(mi)+1.
   	FOU=FOU/total(FOU)
			;OLD METHODE c~CM=c WHERE CM=c2~CX2 (aproximatively)
			;*** ******* ****** ***** *********  **************
			;	c2= double(c)^2.5 & c2=float(c2/total(c2))
			;	CX2=-c2
			;	CX2(mi)=CX2(mi)+1.
			;    ***C~c2
			;	IF sym then res=di_conv(c  ,c2 ) else res=di_conv(c ,DI_REV(c2) ) ;???
			;	res=res / total(res)
			;    ***Adjust maximum
			;	e  =(c(mi)-res(mi))/total(res*CX2)
			;	CX2=CX2*e & CX2(mi)=CX2(mi)+1.
			;    ***CM =c2~cx2
			;	IF sym then CM =di_conv(c2 ,CX2) else CM=di_conv(c2,DI_REV(CX2))  ;???
			;	CM(mi)=CM(mi)-(total(CM)-1.)
			;    ***Adjuste CM
    endif

;MAKE DATA REGULAR IF NECESSARY (vector only)
;**** **** ******* ** *********
	wok=0
	IF n_elements(xx) eq xn then begin wok=1
		   n=(xx(xn-1)-xx(0))/unit +1  &  xreg=findgen(n)*unit + xx(0)
		   W=INTERPOL(d,xx,xreg)
	ENDIF else W=d

;REMOVE STATISTIC.
;****** *********

	if keyword_set(filt) or keyword_set(stat) then begin
	   W =DI_CONV(W,FOU)
	   Wt=DI_CONV(W,FOU)
	  ;W =W+(W-Wt)
	   if keyword_set(stat) then begin
	   	if wok then RETURN,INTERPOL(W,xreg,xx) else RETURN,W
	   endif
	endif


;!!!!!!!!!!!Test junk for overlaped signals

; w=w+shift(w,10)+shift(w,20)/4+shift(w,-14)/6;+findgen(n_elements(w))/n_elements(w);+shift(w,-15)/2.
;w=[fltarr(24),c,c/2,c*1.5,fltarr(24)] & w=w+findgen(n_elements(w))/n_elements(w)
;if yn eq 1 then begin cp =[fltarr(mi),c,fltarr(mi)]	         & ch =shift(cp,6) & endif
;if yn gt 1 then begin cp = fltarr (4*xi+1,4*yi+1) & cp(xi,yi)=c & ch =shift(cp,7,7) & endif
;if yn eq 1 then w  =cp+ch+(findgen(4*xi+1))/(100.*xi)
;if yn gt 1 then w  =cp+ch+(findgen(4*xi+1,4*yi+1))/(100.*xi*yi)
;!!!!!!!!!!!
;****************************************************************
; (c+|f)~CZ= |1
;  T         =total(CZ)
;  B~c       = D -bg   } sum --> B~c + (D+fB)~CZ/T  = D+B/T
; (D+fB)~CZ/T= B/T +bg }     --> B~(|1/T -fCZ/T-c)  = D~(CZ/T -|1)
;                                B~ a1              = D~ a2
;****************************************************************
; a1 = c~a2
; *********
; a1+c = (CZ/T)~c		EN COURS:ca=di_solv(a1+un,un) , w~cz/t~ca  .......!!!!!
; ***************
; D~(CZ/T) - (D2) =D
; ******************
;(a1+a2)~solv(c+|f) = a2
;
;(B~a1 + B~abs(a1))*F1 = B~(a1>0)*2*F1
;(D~a2 +   XXX    )*F1 = ------------- --> have to find XXX=D~a22 !!!
;
; a22=di_solv(c+un/90,|.25|.5|.25)!
;             c          gunn
; gunn is F(di_solv(c,a11)!


;POSITION OF THE BANDS
;******** ** *** *****
	T  =  abs(total(CZ))
	a2 =  CZ/T      & a2(mi) =a2(mi)-total(a2)
	a1 =-(CZ/T*f+c) & a1(mi) =a1(mi)+1./T
	
	Ro=abs(a1(mi)/total(a2*c)) ;**Error factor
	F1=1./a1(mi) * Ro          ;**Adjustment factor
	Re=abs(1.-Ro)* F1          ;**Lower limit ---------> find better
	D2=di_conv(W ,a2)
	
		bid=60
		solv=w*0.  & solv(bid)=1. & solv(bid+10)=1. & solv(bid+20)=.25 & solv(bid-14)=1./6
		un =c*0.   & un(mi)=1     & D11=di_conv(solv,a1)

	B  = (di_keeb(D2>0,mi*3/2,D2 ,id1,shf)) *F1
	
	D1 =di_conv(solv,a1)
	
stop
	
;ADJUST HIGHT OF THE BANDS
;****** ***** ** *** *****
	for j=1,3 do begin
	  B1=B & print,B(id1)
	  for i=1,5 do begin
		D1 =  di_conv(B1,a1)
	  	df1=B1*0   & df1(id1)=(D2(id1-shf)*Ro - D1 (id1))
	            B1 (id1)= B1(id1)+df1(id1)*F1 & print,B1(id1)
	  endfor

	  ;MISSING BANDS
	  ;******* *****
	  D3=(D2*2-D1)
	  B  = (di_keeb(D3>0,mi,D3 ,id1,shf)) *F1
	  stop
	endfor
		;les bon pics !!! tres mauvais !!
		cc=-c & cc(mi)=cc(mi)+1
		;DC=di_conv(D3,cc)
		;B1(where(DC gt D3))=0
		;B1=B1>0
		;D1=di_conv(B1,a1)

;RST IS BG , CALCULATED BANDS
;*** ** *********************
	rst=di_conv(W+B1*f,cz)+(D1-D2)*t
	rst(0:mi)=W(0:mi)
	rst(xn-mi-1:xn-1)=W(xn-mi-1:xn-1) ; Care bi_dim !!!!!!!!!!!!
	wb =rst-B1
stop
return,rst
end
