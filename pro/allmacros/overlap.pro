	FUNCTION overlap, x_in, y_in, iprint, oldymin

	COMMON grid, xmin, xmax, ymin, ymax

;	IDL-ed version of area.for for TOF data analysis
;	Calculates amount of overlap area of rectangle with sides
;	x=xmin, x=xmax, y=ymin, y=ymax, and quadrangle with corners 
;	(x1,y1), (x2,y2), (x3,y3), (x4,y4).
;	(bottom and top lines of two quadrangles parallel).
;
;							KHA, 7/12/98
;
; (xmin,ymax)						 (xmax,ymax)
;	     +------------------------------------------+
;	     |						|
;    (x2,y2) |		     (x3,y3)			|
;	  +--|----B---------+				|
;	 /   |		   /				|
;	A    |		  C				|
;     /	     |		/  				|
;   +--------|--D-----+					|
; (x1,y1)    |		(x4,y4)				|
;	     |						|
;	     +------------------------------------------+
; (xmin,ymin)						 (xmax,ymin)
;

	x1=x_in(0)	& x01=x1
	x2=x_in(1)	& x02=x2
	x3=x_in(2)	& x03=x3
	x4=x_in(3)	& x04=x4
	y1=y_in(0)	& y01=y1
	y2=y_in(1)	& y02=y2
	y3=y_in(2)	& y03=y3
	y4=y_in(3)	& y04=y4

	IF (iprint GT 0) THEN PRINT,'Entering overlap:'

;	PRINT,'[Emin,Emax,Emax,Emin]=',y

;*******************************************************************************
;	Plot Q-E rectangle and quadrangle
;
	IF (iprint GT 0) THEN BEGIN
		a=''
		IF (oldymin NE ymin) THEN BEGIN
			PRINT,'Hit return to start new Q-E grid '
			READ, a
			dx=(xmax-xmin)
			dy=(ymax-ymin)
			x=[xmin-dx,xmin-dx,xmax+dx,xmax+dx]
			y=[ymin-dy,ymax+dy,ymin-dy,ymax+dy]
			plot, x, y, psym=1
		ENDIF
		x=[xmin,xmin,xmax,xmax]
		y=[ymin,ymax,ymin,ymax]
		oplot, x, y, psym=2
		x=[x1,x2,x3,x4]
		y=[y1,y2,y3,y4]
		oplot, x, y, psym=4
		PRINT,'Hit return to calculate overlap '
		READ, a
	ENDIF

;*******************************************************************************
;	Error check
;
	OK=1

	IF (y1 NE y4) OR (y2 NE y3) OR (y2 LE y1) THEN BEGIN
	   OK=0
	   IF (iprint GT 0) THEN PRINT,'y1 NE y4 or y2 NE y3 or y2 NE y1'
	   GOTO, finished
	ENDIF
	IF (x2 GE x3) OR (x1 GE x4) THEN BEGIN
	   OK=0
	   IF (iprint GT 0) THEN PRINT,'x2 GT x3 or x1 GE x4'
	   GOTO, finished
	ENDIF

	IF (MAX([x1,x2,x3,x4]) LE xmin) THEN BEGIN
	   OK=0
	   IF (iprint GT 0) THEN PRINT,'MAX([x1,x2,x3,x4]) LE xmin'
	   GOTO, finished
	ENDIF
	IF (MIN([x1,x2,x3,x4]) GE xmax) THEN BEGIN
	   OK=0
	   IF (iprint GT 0) THEN PRINT,'MIN([x1,x2,x3,x4]) GE xmax'
	   GOTO, finished
	ENDIF
	IF (MAX([y1,y2,y3,y4]) LE ymin) THEN BEGIN
	   OK=0
	   IF (iprint GT 0) THEN PRINT,'MAX([y1,y2,y3,y4]) LE ymin'
	   GOTO, finished
	ENDIF
	IF (MIN([y1,y2,y3,y4]) GE ymax) THEN BEGIN
	   OK=0
	   IF (iprint GT 0) THEN PRINT,'MIN([y1,y2,y3,y4]) GE ymax'
	   GOTO, finished
	ENDIF

;*******************************************************************************
;	If necessary, change y-limits of quad to be within rectangle
;
	IF (y1 LT ymin) THEN BEGIN
	   x1=x1+(ymin-y1)*(x2-x1)/(y2-y1)
	   x4=x4+(ymin-y4)*(x3-x4)/(y3-y4)
	   y1=ymin
	   y4=ymin
	ENDIF
	IF (y2 GT ymax) THEN BEGIN
	   x2=x1+(ymax-y1)*(x2-x1)/(y2-y1)
	   x3=x4+(ymax-y4)*(x3-x4)/(y3-y4)
	   y2=ymax
	   y3=ymax
	ENDIF

;*******************************************************************************
;	Find out whether and where quad crosses grid boundaries 
;
	ncross=0
	AcrossXmin=0	& BcrossXmin=0	& CcrossXmin=0	& DcrossXmin=0
	AcrossXmax=0	& BcrossXmax=0	& CcrossXmax=0	& DcrossXmax=0

	IF (x1 NE x2) THEN BEGIN
		yAxmin=y1+(xmin-x1)*(y2-y1)/(x2-x1)	; crossing of xmin
		IF (ymin LT yAxmin) AND (yAxmin LT ymax) AND $
		   (y1 LT yAxmin) AND (yAxmin LT y2) THEN BEGIN
			AcrossXmin=1
			ncross=ncross+1
		ENDIF ELSE AcrossXmin=0
		yAxmax=y1+(xmax-x1)*(y2-y1)/(x2-x1)	; crossing of xmax
		IF (ymin LT yAxmax) AND (yAxmax LT ymax) AND $
		   (y1 LT yAxmax) AND (yAxmax LT y2) THEN BEGIN
			AcrossXmax=1
			ncross=ncross+1
	   	ENDIF ELSE AcrossXmax=0
	ENDIF ELSE BEGIN
		AcrossXmin=0
		AcrossXmax=0
	ENDELSE

	IF (x2 LT xmin) AND (xmin LT x3) THEN BEGIN
		BcrossXmin=1
		ncross=ncross+1
	ENDIF ELSE BcrossXmin=0
	IF (x2 LT xmax) AND (xmax LT x3) THEN BEGIN
		BcrossXmax=1
		ncross=ncross+1
	ENDIF ELSE BcrossXmax=0

	IF (x3 NE x4) THEN BEGIN
		yCxmin=y4+(xmin-x4)*(y3-y4)/(x3-x4)	; crossing of xmin
		IF (ymin LT yCxmin) AND (yCxmin LT ymax) AND $
		   (y4 LT yCxmin) AND (yCxmin LT y3) THEN BEGIN
			CcrossXmin=1
			ncross=ncross+1
		ENDIF ELSE CcrossXmin=0
		yCxmax=y4+(xmax-x4)*(y3-y4)/(x3-x4)	; crossing of xmax
		IF (ymin LT yCxmax) AND (yCxmax LT ymax) AND $
		   (y4 LT yCxmax) AND (yCxmax LT y3) THEN BEGIN
			CcrossXmax=1
			ncross=ncross+1
		ENDIF ELSE CcrossXmax=0
	ENDIF ELSE BEGIN
		CcrossXmin=0
		CcrossXmax=0
	ENDELSE

	IF (x1 LT xmin) AND (xmin LT x4) THEN BEGIN
		DcrossXmin=1
		ncross=ncross+1
	ENDIF ELSE DcrossXmin=0
	IF (x1 LT xmax) AND (xmax LT x4) THEN BEGIN
		DcrossXmax=1
		ncross=ncross+1
	ENDIF ELSE DcrossXmax=0

	IF (x1 EQ xmin) AND (BcrossXmin EQ 1 OR CcrossXmin EQ 1) THEN DcrossXmin=1
	IF (x1 EQ xmax) AND (BcrossXmax EQ 1 OR CcrossXmax EQ 1) THEN DcrossXmax=1
	IF (x2 EQ xmin) AND (CcrossXmin EQ 1 OR DcrossXmin EQ 1) THEN BcrossXmin=1
	IF (x2 EQ xmax) AND (CcrossXmax EQ 1 OR DcrossXmax EQ 1) THEN BcrossXmax=1
	IF (x3 EQ xmin) AND (AcrossXmin EQ 1 OR DcrossXmin EQ 1) THEN BcrossXmin=1
	IF (x3 EQ xmax) AND (AcrossXmax EQ 1 OR DcrossXmax EQ 1) THEN BcrossXmax=1
	IF (x4 EQ xmin) AND (AcrossXmin EQ 1 OR BcrossXmin EQ 1) THEN DcrossXmin=1
	IF (x4 EQ xmax) AND (AcrossXmax EQ 1 OR BcrossXmax EQ 1) THEN DcrossXmax=1

	IF (ncross EQ 0) AND ((x1 LT xmin OR x1 GT xmax) OR $
  			       (y1 LT ymin OR y1 GT ymax)) THEN BEGIN
		OK=0
		IF (iprint GT 0) THEN PRINT,'quadrange does not cross grid',$
					' and (x1,y1) is not inside.
		GOTO, finished
	ENDIF	; i.e. if quad does not cross grid, and (x1,y1) is not inside.


	IF (AcrossXmin EQ 1) THEN BEGIN
	   IF (AcrossXmax EQ 1) THEN BEGIN
	      IF (BcrossXmin EQ 1) THEN BEGIN
		 IF (BcrossXmax EQ 1) THEN BEGIN		; Case 30
		    ic=30
		    x1=xmax	& y1=yAxmax
		    x2=xmin	& y2=yAxmin
		    x3=xmin
		    x4=xmax	& y4=y3
		    IF (CcrossXmin EQ 1) OR (CcrossXmax EQ 1) OR $ 
			(DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		 ENDIF ELSE BEGIN				; Case 31
		    ic=31
		    x4=x3	& y4=y3
		    x1=xmax	& y1=yAxmax
		    x2=xmin	& y2=yAxmin
		    x3=xmin
		    x5=xmax	& y5=yCxmax
		    IF (CcrossXmin EQ 1) OR (CcrossXmax EQ 0) OR $
			(DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		 ENDELSE
	      ENDIF ELSE BEGIN
		 IF (CcrossXmin EQ 1) THEN BEGIN
		    IF (x1 LT xmin) THEN BEGIN			; Case 1
		       ic=1
		       x1=xmin	& y1=yCxmin
		       x2=xmin	& y2=yAxmin
		       x3=xmax	& y3=yAxmax
		       x4=xmax	& y4=yCxmax
		       IF (BcrossXmax EQ 1) OR (CcrossXmax EQ 0) OR $
			  (DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		    ENDIF ELSE BEGIN				; Case 32
		       ic=32
		       x1=xmax	& y1=yAxmax
		       x2=xmin	& y2=yAxmin
		       x3=xmin	& y3=yCxmin
		       x4=xmax	& y4=yCxmax
		       IF (BcrossXmax EQ 1) OR (CcrossXmax EQ 0) OR $
 			  (DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		    ENDELSE
		 ENDIF ELSE IF (CcrossXmax EQ 1) THEN BEGIN	; Case 2
		    ic=2
		    x5=x4	& y5=y4
		    x1=xmin
		    x2=xmin	& y2=yAxmin
		    x3=xmax	& y3=yAxmax
		    x4=xmax	& y4=yCxmax
		    IF (BcrossXmax EQ 1) OR (DcrossXmin EQ 0) OR $
			(DcrossXmax EQ 1) THEN OK=0
		 ENDIF ELSE BEGIN				; Case 3
		    ic=3
		    x1=xmin
		    x2=xmin	& y2=yAxmin
		    x3=xmax	& y3=yAxmax
		    x4=xmax
		    IF (BcrossXmax EQ 1) OR (DcrossXmin EQ 0) OR $
			(DcrossXmax EQ 0) THEN OK=0
		 ENDELSE
	      ENDELSE
	   ENDIF ELSE BEGIN
	      IF (BcrossXmin EQ 1) THEN BEGIN
		 IF (BcrossXmax EQ 1) THEN BEGIN
		    IF (CcrossXmax EQ 1) THEN BEGIN		; Case 18
		       ic=18
		       x6=x4	& y6=y4
		       x2=xmin	& y2=yAxmin
		       x3=xmin
		       x4=xmax	& y4=y3
		       x5=xmax	& y5=yCxmax
		       IF (CcrossXmin EQ 1) OR (DcrossXmin EQ 1) OR $
			  (DcrossXmax EQ 1) THEN OK=0
		    ENDIF ELSE BEGIN				; Case 19
		       ic=19
		       x2=xmin	& y2=yAxmin
		       x3=xmin
		       x4=xmax	& y4=y3
		       x5=xmax	& y5=y1
		       IF (CcrossXmin EQ 1) OR (DcrossXmin EQ 1) OR $
			  (DcrossXmax EQ 0) THEN OK=0
		    ENDELSE
		 ENDIF ELSE BEGIN
		    IF (CcrossXmax EQ 1) THEN BEGIN		; Case 20
		       ic=20
		       x4=x3	& y4=y3
		       x2=xmin	& y2=yAxmin
		       x3=xmin
		       x5=xmax	& y5=yCxmax
		       x6=xmax	& y6=y1
		       IF (CcrossXmin EQ 1) OR (DcrossXmin EQ 1) OR $
			  (DcrossXmax EQ 0) THEN OK=0
		    ENDIF ELSE BEGIN				; Case 21
		       ic=21
		       x5=x4	& y5=y4
		       x4=x3	& y4=y3
		       x2=xmin	& y2=yAxmin
		       x3=xmin
		       IF (CcrossXmin EQ 1) OR (DcrossXmin EQ 1) OR $
			  (DcrossXmax EQ 1) THEN OK=0
		    ENDELSE
		 ENDELSE
	      ENDIF ELSE BEGIN
		 IF (BcrossXmax EQ 1) THEN BEGIN
		    IF (CcrossXmin EQ 1) THEN BEGIN		; Case 4
		       ic=4
		       x3=x2	& y3=y2
		       x1=xmin	& y1=yCxmin
		       x2=xmin	& y2=yAxmin
		       x4=xmax	& y4=y3
		       x5=xmax	& y5=yCxmax
		       IF (CcrossXmax EQ 0) OR (DcrossXmin EQ 1) OR $
			  (DcrossXmax EQ 1) THEN OK=0
		    ENDIF ELSE IF (CcrossXmax EQ 1) THEN BEGIN	; Case 5
		       ic=5
		       x6=x4	& y6=y4
		       x3=x2	& y3=y2
		       x1=xmin
		       x2=xmin	& y2=yAxmin
		       x4=xmax	& y4=y3
		       x5=xmax	& y5=yCxmax
		       IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 1) THEN OK=0
		    ENDIF ELSE BEGIN				; Case 6
		       ic=6
		       x3=x2	& y3=y2
		       x1=xmin
		       x2=xmin	& y2=yAxmin
		       x4=xmax	& y4=y3
		       x5=xmax	& y5=y1
		       IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 0) THEN OK=0
		    ENDELSE
		 ENDIF ELSE BEGIN
		    IF (CcrossXmin EQ 1) THEN BEGIN
		       IF (CcrossXmax EQ 1) THEN BEGIN		; Case 22
			  ic=22
			  x2=xmin	& y2=yAxmin
			  x3=xmin	& y3=yCxmin
			  x4=xmax	& y4=yCxmax
			  x5=xmax	& y5=y1
			  IF (DcrossXmin EQ 1) OR (DcrossXmax EQ 0) THEN OK=0
		       ENDIF ELSE BEGIN
			  IF (x1 LT xmin) THEN BEGIN		; Case 7
			     ic=7
			     x4=x3	& y4=y3
			     x3=x2	& y3=y2
			     x1=xmin	& y1=yCxmin
			     x2=xmin	& y2=yAxmin
			     IF (DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
			  ENDIF ELSE BEGIN			; Case 23
			     ic=23
			     x2=xmin	& y2=yAxmin
			     x3=xmin	& y3=yCxmin
			     IF (DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
			  ENDELSE
		       ENDELSE
		    ENDIF ELSE BEGIN
		       IF (CcrossXmax EQ 1) THEN BEGIN		; Case 8
			  ic=8
			  x4=x3		& y4=y3
			  x3=x2		& y3=y2
			  x1=xmin
			  x2=xmin	& y2=yAxmin
			  x5=xmax	& y5=yCxmax
			  x6=xmax	& y6=y1
			  IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 0) THEN OK=0
		       ENDIF ELSE BEGIN				; Case 9
			  ic=9
			  x5=x4		& y5=y4
			  x4=x3		& y4=y3
			  x3=x2		& y3=y2
			  x1=xmin
			  x2=xmin	& y2=yAxmin
			  IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 1) THEN OK=0
		       ENDELSE
		    ENDELSE
		 ENDELSE
	      ENDELSE
	   ENDELSE
	ENDIF ELSE BEGIN
	   IF (AcrossXmax EQ 1) THEN BEGIN
	      IF (BcrossXmax EQ 1) THEN BEGIN			; Case 33
		 ic=33
		 x1=xmax	& y1=yAxmax
		 x3=xmax
		 IF (BcrossXmin EQ 1) OR (CcrossXmin EQ 1) OR (CcrossXmax EQ 1) $
			OR (DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
	      ENDIF ELSE IF (CcrossXmax EQ 1) THEN BEGIN
		 IF (x1 LT xmax) THEN BEGIN			; Case 24
		    ic=24
		    x2=xmax	& y2=yAxmax
		    x3=xmax	& y3=yCxmax
		    IF (BcrossXmin EQ 1) OR (CcrossXmin EQ 1) OR $
			(DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		 ENDIF ELSE BEGIN				; Case 34
		    ic=34
		    x1=xmax	& y1=yAxmax
		    x4=xmax	& y4=yCxmax
		    IF (BcrossXmin EQ 1) OR (CcrossXmin EQ 1) OR $
			(DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		 ENDELSE
	      ENDIF ELSE BEGIN					; Case 25
		 ic=25
		 x2=xmax	& y2=yAxmax
		 x3=xmax	& y3=y4
		 IF (BcrossXmin EQ 1) OR (CcrossXmin EQ 1) OR $
		    (DcrossXmin EQ 1) OR (DcrossXmax EQ 0) THEN OK=0
	      ENDELSE
	   ENDIF ELSE BEGIN
	      IF (BcrossXmin EQ 1) THEN BEGIN
		 IF (BcrossXmax EQ 1) THEN BEGIN 
		    IF (CcrossXmin EQ 1) THEN BEGIN		; Case 10
		       ic=10
		       x1=xmin	& y1=yCxmin
		       x2=xmin
		       x3=xmax
		       x4=xmax	& y4=yCxmax
		       IF (CcrossXmax EQ 0) OR $
			  (DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		    ENDIF ELSE IF (CcrossXmax EQ 1) THEN BEGIN	; Case 11
		       ic=11
		       x5=x4	& y5=y4
		       x1=xmin
		       x2=xmin
		       x3=xmax
		       x4=xmax	& y4=yCxmax
		       IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 1) THEN OK=0
		    ENDIF ELSE BEGIN				; Case 12
		       ic=12
		       x1=xmin
		       x2=xmin
		       x3=xmax
		       x4=xmax
		       IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 0) THEN OK=0
		    ENDELSE
		 ENDIF ELSE BEGIN
		    IF (CcrossXmin EQ 1) THEN BEGIN		; Case 13
		       ic=13
		       x1=xmin	& y1=yCxmin
		       x2=xmin
		       IF (CcrossXmax EQ 1) OR $
			  (DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		    ENDIF ELSE IF (CcrossXmax EQ 1) THEN BEGIN	; Case 14
		       ic=14
		       x1=xmin
		       x2=xmin
		       x4=xmax	& y4=yCxmax
		       x5=xmax	& y5=y1
		       IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 0) THEN OK=0
		    ENDIF ELSE BEGIN				; Case 15
		       ic=15
		       x1=xmin
		       x2=xmin
		       IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 1) THEN OK=0
		    ENDELSE
		 ENDELSE
	      ENDIF ELSE BEGIN
		 IF (BcrossXmax EQ 1) THEN BEGIN
		    IF (CcrossXmax EQ 1) THEN BEGIN		; Case 26
		       ic=26
		       x5=x4	& y5=y4
		       x3=xmax
		       x4=xmax	& y4=yCxmax
		       IF (CcrossXmin EQ 1) OR $
			  (DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		    ENDIF ELSE BEGIN				; Case 27
		       ic=27
		       x3=xmax
		       x4=xmax
		       IF (CcrossXmin EQ 1) OR $
			  (DcrossXmin EQ 1) OR (DcrossXmax EQ 0) THEN OK=0
		    ENDELSE
		 ENDIF ELSE BEGIN
		    IF (CcrossXmin EQ 1) THEN BEGIN
		       IF (CcrossXmax EQ 1) THEN BEGIN		; Case 16
			  ic=16
			  x1=xmin
			  x2=xmin	& y2=yCxmin
			  x3=xmax	& y3=yCxmax
			  x4=xmax
			  IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 0) THEN OK=0
		       ENDIF ELSE BEGIN				; Case 17
			  ic=17
			  x3=x4		& y3=y4
			  x1=xmin
			  x2=xmin	& y2=yCxmin
			  IF (DcrossXmin EQ 0) OR (DcrossXmax EQ 1) THEN OK=0
		       ENDELSE
		    ENDIF ELSE BEGIN
		       IF (CcrossXmax EQ 1) THEN BEGIN		; Case 28
			  ic=28
			  x4=xmax	& y4=yCxmax
			  x5=xmax	& y5=y1
			  IF (DcrossXmin EQ 1) OR (DcrossXmax EQ 0) THEN OK=0
		       ENDIF ELSE BEGIN				; Case 29
			  ic=29
			  IF (DcrossXmin EQ 1) OR (DcrossXmax EQ 1) THEN OK=0
		       ENDELSE
		    ENDELSE
		 ENDELSE
	      ENDELSE
	   ENDELSE
	ENDELSE

	IF (ic EQ 1) THEN GOTO, quadrangle
	IF (ic EQ 2) THEN GOTO, pentagon
	IF (ic EQ 3) THEN GOTO, quadrangle
	IF (ic EQ 4) THEN GOTO, pentagon
	IF (ic EQ 5) THEN GOTO, hexagon
	IF (ic EQ 6) THEN GOTO, pentagon
	IF (ic EQ 7) THEN GOTO, quadrangle
	IF (ic EQ 8) THEN GOTO, hexagon
	IF (ic EQ 9) THEN GOTO, pentagon
	IF (ic EQ 10) THEN GOTO, quadrangle
	IF (ic EQ 11) THEN GOTO, pentagon
	IF (ic EQ 12) THEN GOTO, quadrangle
	IF (ic EQ 13) THEN GOTO, triangle
	IF (ic EQ 14) THEN GOTO, pentagon
	IF (ic EQ 15) THEN GOTO, quadrangle
	IF (ic EQ 16) THEN GOTO, quadrangle
	IF (ic EQ 17) THEN GOTO, triangle
	IF (ic EQ 18) THEN GOTO, hexagon
	IF (ic EQ 19) THEN GOTO, pentagon
	IF (ic EQ 20) THEN GOTO, hexagon
	IF (ic EQ 21) THEN GOTO, pentagon
	IF (ic EQ 22) THEN GOTO, pentagon
	IF (ic EQ 23) THEN GOTO, quadrangle
	IF (ic EQ 24) THEN GOTO, quadrangle
	IF (ic EQ 25) THEN GOTO, triangle
	IF (ic EQ 26) THEN GOTO, pentagon
	IF (ic EQ 27) THEN GOTO, quadrangle
	IF (ic EQ 28) THEN GOTO, pentagon
	IF (ic EQ 29) THEN GOTO, quadrangle
	IF (ic EQ 30) THEN GOTO, quadrangle
	IF (ic EQ 31) THEN GOTO, pentagon
	IF (ic EQ 32) THEN GOTO, quadrangle
	IF (ic EQ 33) THEN GOTO, triangle
	IF (ic EQ 34) THEN GOTO, quadrangle

	OK=0
	PRINT,' Quadrangle has not fallen into any category! (Area=0)'


triangle:
	a1=x2-x1
	a2=y2-y1
	b1=x3-x1
	b2=y3-y1
	Area=ABS(a1*b2-a2*b1)/2.
	IF (OK EQ 0) THEN PRINT,' Logical problem with triangle, case ',ic
	IF (iprint GT 0) THEN BEGIN
		x=[x1,x2,x3,x1]
		y=[y1,y2,y3,y1]
		oplot, x, y, psym=-1
		PRINT,'Hit return to continue '
		READ, a
	ENDIF
	GOTO, finished

quadrangle:
	a1=x2-x1
	a2=y2-y1
	b1=x3-x1
	b2=y3-y1
	c1=x4-x1
	c2=y4-y1
	Area=ABS(a1*b2-a2*b1)/2.+ABS(b1*c2-b2*c1)/2.
	IF (OK EQ 0) THEN PRINT,' Logical problem with quadrangle, case ',ic
	IF (iprint GT 0) THEN BEGIN
		x=[x1,x2,x3,x4,x1]
		y=[y1,y2,y3,y4,y1]
		oplot, x, y, psym=-1
		PRINT,'Hit return to continue '
		READ, a
	ENDIF
	GOTO, finished

pentagon:
	a1=x2-x1
	a2=y2-y1
	b1=x3-x1
	b2=y3-y1
	c1=x4-x1
	c2=y4-y1
	d1=x5-x1
	d2=y5-y1
	Area=ABS(a1*b2-a2*b1)/2.+ABS(b1*c2-b2*c1)/2.+ABS(c1*d2-c2*d1)/2.
	IF (OK EQ 0) THEN PRINT,' Logical problem with pentagon, case ',ic
	IF (iprint GT 0) THEN BEGIN
		x=[x1,x2,x3,x4,x5,x1]
		y=[y1,y2,y3,y4,y5,y1]
		oplot, x, y, psym=-1
		PRINT,'Hit return to continue '
		READ, a
	ENDIF
	GOTO, finished

hexagon:
	a1=x2-x1
	a2=y2-y1
	b1=x3-x1
	b2=y3-y1
	c1=x4-x1
	c2=y4-y1
	d1=x5-x1
	d2=y5-y1
	e1=x6-x1
	e2=y6-y1
	Area=ABS(a1*b2-a2*b1)/2.+ABS(b1*c2-b2*c1)/2.+ABS(c1*d2-c2*d1)/2. $
		+ABS(d1*e2-d2*e1)/2.
	IF (OK EQ 0) THEN PRINT,' Logical problem with hexagon, case ',ic
	IF (iprint GT 0) THEN BEGIN
		x=[x1,x2,x3,x4,x5,x6,x1]
		y=[y1,y2,y3,y4,y5,y6,y1]
		oplot, x, y, psym=-1
		PRINT,'Hit return to continue '
		READ, a
	ENDIF
	GOTO, finished

finished:
	IF (OK EQ 0) THEN BEGIN
		area=0.
		IF (iprint GT 0) THEN PRINT,'Error in given quadrangle'
	ENDIF

	IF (iprint GT 0) THEN oldymin=ymin

	x1=x01
	x2=x02
	x3=x03
	x4=x04
	y1=y01
	y2=y02
	y3=y03
	y4=y04

;	PRINT,'[Emin,Emax,Emax,Emin]=',y

	RETURN, area
	END
