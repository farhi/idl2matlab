
;    ALIGN the Dial
;    ****************

pro  DIAL_ALIGN_MACRO, D,san,trs,len
;**  ********************
;**

	COMMON	dial_plot,gsan,gtrs,len,xp,yp

	CATCH,stat & if stat ne 0 then begin print,!err_string & CATCH,/cancel & return & endif

	iprint =1

	maxiter=5
	
	dpr=180./!pi
	
;   set up instrument before scanning	
	
	foot=len*sin(gsan/dpr)
        sw=foot/2.
	dt=(foot+sw)/2.
	
	
	command='s3w 0'	        & C=DialControl(command,CHECK=0.5)
	command='DET 3400'	& C=DialControl(command,CHECK=0.5)	
	command='DAN 2'	        & C=DialControl(command,CHECK=0.5)
	command='DAN 2'	        & C=DialControl(command,CHECK=0.5)
	command='s2w '+STRING(sw)	        & C=DialControl(command,CHECK=0.5)
	command='s3w '+STRING(sw)	        & C=DialControl(command,CHECK=0.5)
	
	var    =D.scanvar
	c_time =STRING(D.c_time)

	IF (NOT D.started) THEN BEGIN
		D.started=1	& D.niter=0
		D.current(*)=[0,gtrs-dt,gtrs+dt,0.05,.6,.1,1.0,.2,0,0]
		D.current(0)=D.current(1)
		B           =D.current(0)
		IF (iprint GT 0)   THEN PRINT,'Start first scan'
		command=D.initialise	& C=DialControl(command,CHECK=0.5)
		if C lt 0 then print,'Mad is un-reachable !!! sending "'+command+'"''
		command=var+' '+STRING(B)	& C=DialControl(command,CHECK=0.5)
		
		wait,3.
		
		command='co '+c_time+' t nos'	& C=DialControl(command,CHECK=0.5)
		DialWSet
	ENDIF ELSE BEGIN
		B	=D.current(0)
		Bmin	=D.current(1)
		Bmax	=D.current(2)
		dB	=D.current(3)
		B1range	=D.current(4)
		dB1	=D.current(5)
		B2range	=D.current(6)
		dB2	=D.current(7)
		lambda	=D.lambda
		IF (iprint GT 0) THEN BEGIN
			PRINT,'B=',B,' Bmin=',Bmin,' Bmax=',Bmax,' dB=',dB,' var=',var
			FOR i=0,3 DO PRINT,'D.current(',i,')=',D.current(i)
			PRINT,'D.scanvar=', D.scanvar
			PRINT,'Check counting status'
		ENDIF
		D.type='flagus' & status=DialNewValue()
		IF (status EQ 0) THEN BEGIN
;-------------------get monitor counts and plot them------------------
			D.type='data'  & monitor=DialNewValue()
			V=total(monitor)  & D.value=V  & D.error=SQRT(V)
			IF (B EQ Bmin) THEN BEGIN
				 xp=[B,B]  & yp=[V,V]
				 xp=[B]    & yp=[V]
			ENDIF ELSE BEGIN
				xp=[xp,B]  & yp=[yp,V] & ENDELSE
			ep=SQRT(yp)
			DialWSet
			PLOT, xp, yp, PSYM=2, XRANGE=[Bmin,Bmax], YRANGE=[0,MAX(yp)], XTITLE=var, $
				YTITLE='M2 Counts', TITLE='Iteration no. '+STRTRIM(STRING(D.niter+1),2)+' - scan of '+var
			ERRPLOT, xp, yp-ep, yp+ep
;-------------------increment current and start new count-------------
			IF (B LT Bmax) THEN BEGIN
;		    --------continue present scan
				B=B+dB
				command=var+' '+STRING(B)	& C=DialControl(command)
				wait,0.2
				command='co '+c_time+' t nos'	& C=DialControl(command)
			ENDIF ELSE BEGIN
;		    --------end scan, fit minimum, set current and start new scan
				D.niter=D.niter+1
				fit_parabola,xp,float(yp),bestfit,B00	& B0=B00(0) & ymin=B00(1)
				B0=FLOAT(ROUND(B0*100.))/100.
;				DialWSet
				OPLOT, xp, bestfit, PSYM=0, LINESTYLE=0
				ch='Minimum at '+var+' ='+STRING(B0)
				XYOUTS, B0, ymin, ch
				command=var+' '+STRING(B0)	& C=DialControl(command,CHECK=0.5)
				if C lt 0 then print,'Mad is un-reachable !!! sending "'+command+'"'
				IF (var EQ 'TRS') THEN BEGIN
					D.bestB1=B0	& var='B2'
				ENDIF ELSE BEGIN
					D.bestB2=B0	& var='B1'
				ENDELSE
				IF (D.niter EQ 5) THEN BEGIN
					PRINT,'Best currents found: B1=',D.bestB1
					PRINT,'                     B2=',D.bestB2
					PRINT,'finished'
					dialstop
					RETURN
				ENDIF ELSE IF (D.niter EQ 1) THEN BEGIN
					Bmin=3.5	& Bmax=5.9 & dB=0.2
				ENDIF ELSE IF (2*(D.niter/2) EQ D.niter) THEN BEGIN
					Bmin=D.bestB1-B1range	& Bmax=D.bestB1+B1range & dB=dB1
				ENDIF ELSE BEGIN
					Bmin=D.bestB2-B2range	& Bmax=D.bestB2+B2range & dB=dB2
				ENDELSE
				B=Bmin
				wait,3.0
				command=var+' '+STRING(B)	& C=DialControl(command,CHECK=0.5)
				wait,3.0
				command='co '+c_time+' t nos'	& C=DialControl(command,CHECK=0.5)
			ENDELSE
		ENDIF
		D.scanvar=var
		D.current(0)=B
		D.current(1)=Bmin
		D.current(2)=Bmax
		D.current(3)=dB
	ENDELSE

	END

pro fit_parabola, xp,yp, bestfit, xymin
;** ************
	s=SIZE(xp)	& np=s(1)
	dx=xp(1)-xp(0)

	A0=INT_TABULATED(xp,yp)
	A1=INT_TABULATED(xp,yp*xp)
	A2=INT_TABULATED(xp,yp*xp^2)

	x1=xp(0)	& x2=xp(np-1)
	d1=x2-x1
	d2=(x2^2-x1^2)/2.
	d3=(x2^3-x1^3)/3.
	d4=(x2^4-x1^4)/4.
	d5=(x2^5-x1^5)/5.

	A=[[d3,d2,d1],[d4,d3,d2],[d5,d4,d3]]
	B=[A0,A1,A2]
	R=CRAMER(A,B)

	bestfit=R(0)*xp^2+R(1)*xp+R(2)
	x0=-0.5*R(1)/R(0)
	ymin=R(0)*x0^2+R(1)*x0+R(2)

	xymin=[x0,ymin]
	END




;******* ********************************
;******* ********************************
function DIAL_FLIPPER
;******* ********************************
;******* ********************************
;**
;** 
;** Output D is the dial structure returned for DialInit
;********* *********************************************

name     ="flipper"
generic  ="mad"
type     ="data"
onoff    = 0
value    = 0L
error    = 0L
frequency= 2.0
duration = 0.0
history  = 0
plot     = 100

                ;** Other tags used in your procedure DIAL_TEMPLATE1_MACRO:
                ;** ******************************************************
                unit      ="counts"	;The dial string unit
                lowerlim  =0.          ;The dial lower limit value
                upperlim  =0.	;The dial upper limit value

		started   = 0
		current   = FLTARR(10)  ;[,,,initial dB,B1range,dB1,B2range,dB2]
		scanvar   = 'trs'
		initialise='san 0.8'
		c_time    = 5		; counting time per point
		lambda    = 5.0

D={NAME:name, GENERIC:generic  , VALUE:value    , ONOFF:onoff    , FREQUENCY:frequency,$
              DURATION:duration, UNIT:unit      , HISTORY:history, UPPERLIM:upperlim  ,$
              LOWERLIM:lowerlim, PLOT:plot      , TYPE:type      , STARTED:started    ,$
              CURRENT:current  , SCANVAR:scanvar, INITIALISE:initialise,$
              C_TIME:c_time    , LAMBDA:lambda  , NITER:0,BESTB1:0.,BESTB2:0.}
RETURN ,D
END
