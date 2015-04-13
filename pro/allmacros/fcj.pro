PRO SimpleGauss ,x,w,f
    f=(w(0)*2*Sqrt(ALOG(2)/!pi)/w(2) * exp(-(x-w(1))^2/w(2)^2*4*ALOG(2))); +w(3)
End

PRO gauleg,x1,  x2,  x, w, n
   EPS=3.0e-08
	m=(n+1)/2
	xm=0.5*(x2+x1)
	xl=0.5*(x2-x1)
	for i=1,m DO BEGIN 
		z=cos(!PI*(i-0.25)/(n+0.5))
		REPEAT BEGIN
			p1=1.0
			p2=0.0
			for j=1,n DO BEGIN 
				p3=p2
				p2=p1
				p1=((2.0*j-1.0)*z*p2-(j-1.0)*p3)/j
			Endfor
			pp=n*(z*p1-p2)/(z*z-1.0)
			z1=z
			z=z1-p1/pp
		ENDREP UNTIL (abs(z-z1) LE EPS)
		x(i)=xm-xl*z
		x(n+1-i)=xm+xl*z
		w(i)=2.0*xl/((1.0-z*z)*pp*pp)
		w(n+1-i)=w(i)
      ;print,i,x(i),w(i),z,pp
	Endfor
End

PRO fcj,xx,a,y,pder ; FingerCoxJephcoat convoluted with simple Gauss shape
  ;w9=w10 & w=[14286,39.26,0.24162,0.096454,1311.1,0.019359,0.1485,1.5029,-0.0091623,1,0.07,50,40] & fcj,x9,w,w9 & see,w9
  COMMON fit,voigt,nterms,fitflag,key,undo,npeaks,rectangle,bragg,peakpars
  maxindex=N_ELEMENTS(A)-1
  IF voigt EQ 0 THEN peakpars=3 ELSE peakpars=4
  peakpars=peakpars+rectangle
  PDER       =FLTARR(N_ELEMENTS(XX),nterms+3+npeaks*peakpars) 
  k          =peakpars*npeaks
  y          =FLTARR(N_ELEMENTS(xx))
  denominator=y
  NPOINT     =30
  xgauleg   =FLTARR(NPOINT+1)
  wgauleg   =xgauleg
  gauleg,0,1,xgauleg,wgauleg,NPOINT
  tmp_peaks=npeaks
  tmp_terms=nterms
  S=a(k+nterms)
  H=a(k+nterms+1)
  L=a(k+nterms+2)
  npeaks=1
  nterms=0
  index=INDGEN(peakpars)
  FOR i=0,tmp_peaks-1 DO BEGIN
    denominator=denominator*0
    yy=y*0
    ddenominatordpos=yy
    ddenominatordH=yy
    ddenominatordS=yy
    ddenominatordL=yy
    dyydS=yy
    dyydH=yy
    dyydL=yy
    position=a(peakpars*i+1)
    cospos=COS(position*!pi/180)
    tt_min =ACOS(cospos*SQRT(((H+S)/L)^2+1))*180/!pi	
    tt_infl=ACOS(COSpos*SQRT(((H-S)/L)^2+1))*180/!pi
    x_gauleg=tt_min+xgauleg*(position-tt_min)
    w_gauleg=wgauleg*ABS(position-tt_min)
    FOR j=1,NPOINT DO BEGIN
      cosx=COS(x_gauleg(j)*!pi/180)
      h_tmp       =(cosx^2/cospos^2-1)>0
      h_          =L*SQRT(h_tmp)
    	cond1=(((x_gauleg(i) GE tt_min  AND x_gauleg(i) LT  tt_infl) AND (tt_min LE position)) OR ((x_gauleg(i) LE tt_min  AND x_gauleg(i) GT  tt_infl) AND ((tt_min GT position)))) 
    	cond2=(((x_gauleg(i) GE tt_infl AND x_gauleg(i) LE position) AND (tt_min LE position)) OR ((x_gauleg(i) LE tt_infl AND x_gauleg(i) GE position) AND ((tt_min GT position)))) 
		W_          =(H+S-h_)*cond1 + 2*min([H,S])*cond2
      D_tmp       =2*H*h_*cosx ; !!! NOT cospos !!!
      D =L/(D_tmp+(D_tmp EQ 0))*(D_tmp NE 0)*W_ 
      Gauss_Poly,xx-x_gauleg(j)+position,a(index),F,P
      FOR m=0,peakpars-1 DO PDER(*,index(m))=PDER(*,index(m))+D*P*w_gauleg(j)
      yy            =yy            +D*F*w_gauleg(j)
      denominator =denominator+D*  w_gauleg(j)
      dDdH =  D/(W_+(W_ EQ 0))*(W_ NE 0)*(NOT (cond2 AND (H LT S))) ;H
      dDdS = -L/S/S/2/h_/cospos*(cond1 OR (cond2 AND (H LT S)))     ;S
      dDdL =  D/L-W_/2/S/cospos/h_                                  ;L
      ;dDdpos=!PI*W_/360./S*SIN(position*!PI/180.)/cospos^2/(h_/L)*(1-cosx/cospos^2/h_tmp)
      dDdpos=!PI/180.*L^2*cosx*SIN(position*!pi/180.)/h_^2/H/cospos^3*(cond1+W_/h_)
      ;PRINT,xx(0)-x_gauleg(j)+position,P(0,1),D*P(0,1)*w_gauleg(j),dDdpos(0),F(0)*w_gauleg(j)*dDdpos(0)
      ;PDER(*,peakpars*i+1)=PDER(*,peakpars*i+1)+F*w_gauleg(j)*dDdpos
      ddenominatordpos=ddenominatordpos+w_gauleg(j)*dDdpos
      dyydS=         dyydS         +w_gauleg(j)*F*dDdS
      dyydH=         dyydH         +w_gauleg(j)*F*dDdH
      dyydL=         dyydL         +w_gauleg(j)*F*dDdL
      ddenominatordS=ddenominatordS+w_gauleg(j)*dDdS
      ddenominatordH=ddenominatordH+w_gauleg(j)*dDdH
      ddenominatordL=ddenominatordL+w_gauleg(j)*dDdL
      ;PRINT,PDER(0,peakpars*i+1),tmpder(0)
    ENDFOR
    idx=WHERE(denominator GT 0,count)
    ;PRINT,PDER(0,peakpars*i+1),tmpder(0)
    IF count GT 0 THEN BEGIN
      FOR j=0,peakpars-1 DO PDER(idx,index(j))=PDER(idx,index(j))/denominator(idx)
      PDER(*,peakpars*i+1)=PDER(*,peakpars*i+1) - yy(idx)/denominator(idx)^2*ddenominatordpos(idx)
      yy(idx)     =yy(idx)     /denominator(idx)
      PDER(*,k+nterms)  =PDER(*,k+nterms)  + dyydS(idx)/denominator(idx)-yy(idx)/denominator(idx)^2*ddenominatordS(idx) ;S
      PDER(*,k+nterms+1)=PDER(*,k+nterms+1)+ dyydH(idx)/denominator(idx)-yy(idx)/denominator(idx)^2*ddenominatordH(idx) ;H
      PDER(*,k+nterms+2)=PDER(*,k+nterms+2)+ dyydL(idx)/denominator(idx)-yy(idx)/denominator(idx)^2*ddenominatordL(idx) ;L
    ENDIF
    y=y+yy
    index=index+peakpars
  ENDFOR
  npeaks=tmp_peaks
  nterms=tmp_terms
  FOR i=0,nterms-1 DO BEGIN
    y          =y+A(k+i)*XX^i
    PDER(*,k+i)=         XX^i
  ENDFOR
End
