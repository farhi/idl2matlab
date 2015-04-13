;-------------------------------------------------------------------------------
;*******************************************************************************
;
	PRO  DIAL_FLIPPER_D7_MACRO, D
;
; 	Scans flipper currents for zpo/xyz setups
;
;						KHA/JRS 22/5/02
;
;-------------------------------------------------------------------------------
;*******************************************************************************
 
	COMMON	flipper_plot, xp, yp

	CATCH,stat
	IF stat ne 0 THEN BEGIN
		PRINT,!err_string
		CATCH, /cancel
		RETURN
	ENDIF

	iprint  = 0			;turns on debugging messages
	maxiter = 5			;number of iterations for each phase
	var    = D.scanvar
	c_time = STRING(D.c_time)

;-------------------------------------------------------------------------------
;*******************************************************************************
; Initialise Dial before each scan phase

start:
	IF NOT D.started THEN BEGIN
		D.started = 1
		D.niter = 0
		IF D.xyz THEN BEGIN
			CASE D.xyziter OF
				0: BEGIN 
				   	D.bxyz = [7.0,0.0,0.0]
				   	D.phase_tit = 'Z scan - '
				   END
				1: BEGIN
					D.bxyz = [-4.5,5.0,0.0]
					D.phase_tit = 'X scan - '
				   END
				2: BEGIN
					D.bxyz = [-4.5,0.0,7.0]
					D.phase_tit = 'Y scan - '
				   END
			ENDCASE
			C = DIALCONTROL('b3 '+STRING(D.bxyz[0]), CHECK=0.5)
			C = DIALCONTROL('b4 '+STRING(D.bxyz[1]), CHECK=0.5)
			C = DIALCONTROL('b5 '+STRING(D.bxyz[2]), CHECK=0.5)
		ENDIF
		D.lambda = DIALNEWVALUE(TYPE = 'wave', /SETVALUE)
		IF (D.lambda LT 4) THEN $
		  D.current(*) = [0.0,0.0,4.0,0.4,1.0,0.2,1.0,0.2] ELSE $
		IF (D.lambda LT 5) THEN $
		  D.current(*) = [0.0,0.5,2.5,0.2,0.6,0.1,1.2,0.2] ELSE $
		  D.current(*) = [0.0,0.5,2.5,0.2,0.6,0.1,1.2,0.2]
		D.current[0] = D.current[1]
		B = D.current[0]
		IF iprint GT 0 THEN PRINT, 'Start first scan'
		C = DIALCONTROL('b2 '+STRING(D.initb2), CHECK=0.5)
		IF C LT 0 THEN PRINT,'Mad is un-reachable'
		C = DIALCONTROL(var+' '+STRING(B))
		C = DIALCONTROL('co '+c_time+' nos')
		DIALWSET

;------------------------------------------------------------------------------
; Update the scan

	ENDIF ELSE BEGIN
		B	= D.current[0]
		Bmin	= D.current[1]
		Bmax	= D.current[2]
		dB	= D.current[3]
		B1range	= D.current[4]
		dB1	= D.current[5]
		B2range	= D.current[6]
		dB2	= D.current[7]
		lambda	= D.lambda
		IF (iprint GT 0) THEN BEGIN
			PRINT,'B=',B,' Bmin=',Bmin,' Bmax=',Bmax,' dB=',dB,' var=',var
			FOR i=0,3 DO PRINT,'D.current[',i,'] =',D.current[i]
			PRINT,'D.scanvar=', D.scanvar
			PRINT,'Check counting status'
		ENDIF

		status = DIALNEWVALUE(TYPE = 'flagus', /SETVALUE)

		IF status EQ 0 THEN BEGIN

;------------------------------------------------------------------------------
;Plot the monitor 2 counts

			monitor = DIALNEWVALUE(TYPE = 'monitor2', /SETVALUE)
			V = monitor[0]
			D.value = V
			D.error = SQRT(V)
			IF B EQ Bmin THEN BEGIN
				 xp = [B,B]   & yp = [V,V]
				 xp = [B]     & yp = [V]
			ENDIF ELSE BEGIN
				 xp = [xp,B]  & yp = [yp,V]
			ENDELSE
			ep = SQRT(yp)
			DIALWSET
			IF NOT D.xyz THEN phase_tit = 'ZPO scan - '
			PLOT, xp, yp, PSYM=2, XRANGE=[Bmin,Bmax], $
			  YRANGE=[0,MAX(yp)], XTITLE=var, $
			  YTITLE='M2 Counts', TITLE=D.phase_tit+'Iteration no. '+$
			  STRTRIM(STRING(D.niter+1),2)+' - scan of '+var
			ERRPLOT, xp, yp-ep, yp+ep

			IF B LT Bmax THEN BEGIN
;------------------------------------------------------------------------------
;Increment current and start new count
				B = B + dB
				C = DIALCONTROL(var+' '+STRING(B))
				C = DIALCONTROL('co '+c_time+' nos')

			ENDIF ELSE BEGIN
;------------------------------------------------------------------------------
;End scan, fit minimum, set current and start new scan

				D.niter = D.niter + 1
				fit_parabola, xp, float(yp), bestfit, B00
				B0 = B00[0]
				ymin = B00[1]
				B0 = FLOAT(ROUND(B0*100.))/100.
				OPLOT, xp, bestfit, PSYM=0, LINESTYLE=0
				XYOUTS, B0, ymin, 'Minimum at '+var+' ='+STRING(B0)
				C = DIALCONTROL(var+' '+STRING(B0),CHECK=0.5)
				IF C LT 0 THEN PRINT,'Mad is un-reachable'
				IF var EQ 'B1' THEN BEGIN
					D.bestB1=B0	& var='B2'
				ENDIF ELSE BEGIN
					D.bestB2=B0	& var='B1'
				ENDELSE
				IF (D.niter EQ 5) AND (D.xyz EQ 0) THEN BEGIN
					PRINT,'Best currents found: B1=',D.bestB1
					PRINT,'                     B2=',D.bestB2
					command='par pczp '+STRING(D.bestB1)+STRING(D.bestB2)
					C = DIALCONTROL(command)
					C = DIALCONTROL('co zpo 10 nos')
					DIALSTOP
					D.started=0
					D.scanvar='B1'
					RETURN
				ENDIF ELSE IF (D.niter EQ 5) AND (D.xyz EQ 1) THEN BEGIN
					CASE D.xyziter OF
						0:	phase = 'z'
						1:	phase = 'x'
						2:	phase = 'y'
					ENDCASE
					PRINT,'Best '+phase+' currents found: B1=',D.bestB1
					PRINT,'                       B2=',D.bestB2
					command = 'par pc'+phase+' '+STRING(D.bestB1)+STRING(D.bestB2)+ $
						  STRING(D.bxyz[0])+STRING(D.bxyz[1])+STRING(D.bxyz[2])
					C = DIALCONTROL(command)
					D.started = 0
					var = 'B1'
					IF D.xyziter EQ 2 THEN BEGIN
						C = DIALCONTROL('co xyz 10 nos')
						DIALSTOP
						D.scanvar = 'B1'
						RETURN
					ENDIF
					D.xyziter = D.xyziter + 1
					GOTO, start
				ENDIF ELSE IF D.niter EQ 1 THEN BEGIN
					Bmin = D.initb2 - 1.8
					Bmax = D.initb2 + 1.8
					dB = 0.3
				ENDIF ELSE IF 2*(D.niter/2) EQ D.niter THEN BEGIN
					Bmin = D.bestB1 - B1range
					Bmax = D.bestB1 + B1range
					dB = dB1
				ENDIF ELSE BEGIN
					Bmin = D.bestB2-B2range
					Bmax = D.bestB2+B2range
					dB = dB2
				ENDELSE
				B = Bmin
				C = DIALCONTROL(var+' '+STRING(B))
				C = DIALCONTROL('co '+c_time+' nos')
			ENDELSE
		ENDIF
		D.scanvar=var
		D.current(0)=B
		D.current(1)=Bmin
		D.current(2)=Bmax
		D.current(3)=dB
	ENDELSE

	END

;-------------------------------------------------------------------------------
;*******************************************************************************

	PRO fit_parabola, xp, yp, bestfit, xymin

	s = SIZE(xp)
	np = s[1]
	dx = xp[1] - xp[0]

	A0 = INT_TABULATED(xp,yp)
	A1 = INT_TABULATED(xp,yp*xp)
	A2 = INT_TABULATED(xp,yp*xp^2)

	x1 = xp[0]
	x2 = xp[np - 1]
	d1 = x2 - x1
	d2 = (x2^2 - x1^2)/2.
	d3 = (x2^3 - x1^3)/3.
	d4 = (x2^4 - x1^4)/4.
	d5 = (x2^5 - x1^5)/5.

	A = [[d3,d2,d1],[d4,d3,d2],[d5,d4,d3]]
	B = [A0,A1,A2]
	R = CRAMER(A,B)

	bestfit = R[0]*xp^2+R[1]*xp+R[2]
	x0 = -0.5*R[1]/R[0]
	ymin = R[0]*x0^2 + R[1]*x0 + R[2]
	xymin = [x0,ymin]
	END

;------------------------------------------------------------------------------
;******************************************************************************
	FUNCTION dial_flipper_d7

	xyz = 0

	name     = "flipper_d7"
	generic  = "mad"
	type     = "monitor"
	onoff    = 0
	value    = 0L
	error    = 0L
	frequency= 1.6
	duration = 0.0
	history  = 0
	plot     = 100

        unit      = "counts"	; The dial string unit
        lowerlim  = 0.          ; The dial lower limit value
        upperlim  = 0.		; The dial upper limit value
	bxyz      = FLTARR(3)
	current   = FLTARR(8)
	phase_tit = 'ZPO - '
	scanvar   = 'B1'
	initb2    = 4.0		; initial value of correction coil
	c_time    = 1.0		; counting time per point
	lambda    = 0.0

	D={NAME:name        , GENERIC:generic, VALUE:value    , ONOFF:onoff        ,$
           DURATION:duration, UNIT:unit      , HISTORY:history, UPPERLIM:upperlim  ,$
           LOWERLIM:lowerlim, PLOT:plot      , TYPE:type      , STARTED:0          ,$
           CURRENT:current  , SCANVAR:scanvar, INITB2:initb2  , FREQUENCY:frequency,$
           C_TIME:c_time    , LAMBDA:lambda  , NITER:0        , BESTB1:0.          ,$
	   BESTB2:0.        , BXYZ:bxyz      , XYZ:xyz	      , XYZITER:0          ,$
	   PHASE_TIT:phase_tit}
	RETURN ,D
	END
