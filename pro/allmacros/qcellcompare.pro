pro qcellcompare,s,n1,n2,int_ratio=int_ratio,sig_ratio=sig_ratio,plot=plot,$
                     print=print,color=color,truecolors=truecolors,wset=wset,$
                     exclusions=exclusions
;+
; NAME:
;	qcellcompare
;
; PURPOSE:
;	This procedure is the basic routine called from alignloop to calculate relative efficiencies
;
; CATEGORY:
;	Instrument.
;
; CALLING SEQUENCE:
;	qcellcompare,s,n1,n2,int_ratio=int_ratio,sig_ratio=sig_ratio,plot=plot,print=print,color=color,truecolors=truecolors,wset=wset,exclusions=exclusions
;
; INPUTS:
;   s		:	Workspace (2D array, nd * steps elements, as defined in LAMP, normally a 2theta scan over a vanadium)
;	n1		:	detector cell number to be compared with ...
;	n2		:	second detector cell number to be compared to n1
;
; OPTIONAL INPUTS:
;	none
;	
; KEYWORD PARAMETERS:
;	int_ratio	:	intensity ratio of n1 and n2
;	sig_ratio	:	?
;	plot		:	if set, some output will be plotted
;	print		:	if set, some printed output will be created
;	color		:	?
;	truecolors	:	?
;	wset		:	window to be used for plotting output
;	exclusions	:	?
;
; OUTPUTS:
;	none
;
; OPTIONAL OUTPUTS:
;	none
;
; COMMON BLOCKS:
;	none
;
; SIDE EFFECTS:
;	none
;
; RESTRICTIONS:
;	none
;
; PROCEDURE:
; 	methods of error calculation are set according to  "Statistics for nuclear and particle physicists" by L.Lyon that can be found 
; 	in the ILL library (number 5 68)
; 	each calculation is commented 
;
; EXAMPLE:
;	(in LAMP)
;	RDSET,inst='D20',base="Loc Cycle 005" 
;	flag,/nor,/noint,/nobad,/flp,/noeff,/noang
;	w1=RDRUN(19512)
;   ALIGNLOOP(w1,800,x1=x,y1=y,e1=e,s0=s,r0=r)
;
; MODIFICATION HISTORY:
; 	Written by:		Steffen Metzger,	August 1998.
;	Modified by:	Thomas C Hansen,	November 2000 (weighted error calculations)
;
;-
	steps=N_ELEMENTS(s(0,*))
	cells=N_ELEMENTS(s(*,0))
	n1=n1>0
	n1=n1<(cells-1)
	n2=n2>0
	n2=n2<(cells-1)
	nmin=min([n1,n2])
	nmax=max([n1,n2])+steps
	ymin=min(s([n1,n2],*))
	ymax=max(s([n1,n2],*))
	IF KEYWORD_SET(plot) THEN BEGIN
		IF N_ELEMENTS(color) LT 8 THEN colors,color,truecolors=truecolors,/noprint
		IF KEYWORD_SET(wset) THEN wset,0 ELSE window,0,xsi=800,title='Compare cells '+STRCOMPRESS(n1)+' and'+STRCOMPRESS(n2),ysi=400
		cell,s,n1,index=x1,counts=y1,xticklen=1,xgridst=1,xrange=[nmin,nmax<cells],yrange=[ymin,ymax],back=color(4),color=color(0),xstyle=1
		cell,s,n2,index=x2,counts=y2,/oplot,color=color(1),/error
	ENDIF ELSE BEGIN
		x1=indgen(steps)+n1  ; former qcell-call ...
		y1=s(n1,*) 
		x2=indgen(steps)+n2
		y2=s(n2,*)
	ENDELSE
	cmin=max([min(x1),min(x2)])
	cmax=min([max(x1),max(x2)])
	i1=where(x1 ge cmin and x1 le cmax,comcount)
	i2=where(x2 ge cmin and x2 le cmax)
	com=x1(i1)
	celln=BYTARR(cells+steps)
	celln(com)=1
	IF KEYWORD_SET(exclusions) THEN celln(exclusions)=0
	celln=celln(cmin:cmax)
	i1=i1(where(celln EQ 1))
	com=x1(i1)
	i2=i2(where(celln EQ 1))
	ratio=(y1(i1))/(y2(i2))     ;	ratio of cell counting rates  
	e1=SQRT(y1)                 ;	square root of counting rates, (Poisson-distribution)  
	e2=SQRT(y2)
	sigm=ratio*SQRT(((e1(i1))/(y1(i1)))^2+((e2(i2))/(y2(i2)))^2)   ;	error calculatiom in non-linear situation  
	IF KEYWORD_SET(plot) THEN BEGIN
		IF KEYWORD_SET(wset) THEN wset,1 ELSE window,1,title='Ratio cells '+STRCOMPRESS(n1)+' and'+STRCOMPRESS(n2),ysi=380,xsi=800
		plot,com,ratio,back=color(0),yrange=[min(ratio),max(ratio)],xrange=[nmin,nmax<cells],xstyle=1
	 	IF N_ELEMENTS(com) GT 1 THEN oploterr,com,ratio,sigm
	ENDIF
	okay =where(ratio NE 0 AND sigm ne 0,okcount)
	IF okcount GE 1 THEN BEGIN
		int_ratio=(TOTAL(ratio(okay)))/okcount 	;	mean of ratios, after the angles below the beamstop have been excluded, the formula for the 
	                                       		;	weighted mean int_ratio=TOTAL((ratio(okay))/((sigm(okay))^2))/(TOTAL(1/(sigm(okay))^2)) can
	                                       		;	be used  
		sig_ratio=SQRT(TOTAL(((sigm(okay))/okcount)^2))	;	derived from error calculation in a linear situation; for the weighted mean the 
	    	                                           	;	error is sig_ratio=SQRT(1/TOTAL(1/(sigm(okay))^2))
	ENDIF 
	IF KEYWORD_SET(print) THEN BEGIN
		IF okcount GE 1 THEN BEGIN
			PRINT,FORMAT="('Compare',2I5,' - weighted :',2G12.6)",n1,n2,int_ratio,sig_ratio
		ENDIF ELSE PRINT,FORMAT="('Compare',2I5,' - no valid overlap!')",n1,n2
	ENDIF 
END
