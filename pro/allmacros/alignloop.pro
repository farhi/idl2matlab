PRO alignloop,s,n1,truecolors=truecolors,noplot=noplot,x1=x1,y1=y1,e1=e1,r0=r0,s0=s0,wset=wset,$
                   errlim=errlim,errcalcs=errcalcs,limits=limits,exclusions=exclusion
;+
; NAME:
;	alignloop
;
; PURPOSE:
;	This procedure creates the relative efficiencies of a PSD and a reconstituted powder pattern from a detector scan on a vanadium sample
;
; CATEGORY:
;	Instrument.
;
; CALLING SEQUENCE:
;	alignloop,s,n1,truecolors=truecolors,noplot=noplot,x1=x1,y1=y1,e1=e1,r0=r0,s0=s0,wset=wset,errlim=errlim,errcalcs=errcalcs,limits=limits,exclusions=exclusion
;
; INPUTS:
;   s		:	Workspace (2D array, nd * steps elements, as defined in LAMP, normally a 2theta scan over a vanadium)
;	n1		:	Starting detector cell number, will be set to relative efficiency of 1
;
; OPTIONAL INPUTS:
;	none
;	
; KEYWORD PARAMETERS:
;	truecolors	:	?
;	noplot		:	no plotting output, accelerates computing
;	x1			:	?
;	y1			:	?
;	e1			:	?
;	r0			:	?
;	s0			:	?
;	wset		:	window to be used for plotting output
;	errlim		:	not used
;	errcalcs	:	?
;	limits		:	?
;	exclusion	:	?
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
	tmp=where (s eq 0,cnt)
	IF cnt gt 0 THEN s(tmp)=1
	qcompare2allcells,s,n1,int=y1,xx=x1,sig=e1,/nopr;,/plot,/true
	nbofcells=N_ELEMENTS(s(*,0))
	r0=DBLARR(nbofcells)
	IF NOT keyword_SET(plot) THEN plot=0 ELSE wset=1
	IF NOT keyword_SET(exclusions) THEN exclusions=0 
	s0=DBLarr(nbofcells)
	r0(n1)=1L
	list=[n1]
	treated=BYTARR(nbofcells)
	errcalcs=INTARR(nbofcells)
	treated(n1)=1
	seen=treated
	factor=intarr(1600)
	FOR counter=1L,nbofcells-1 DO BEGIN
	  	seen(x1)=1  
	  	cells=where(seen AND NOT treated)
	  	index=cells-x1(0)
	  	minerr=min(ABS(cells-n1),n2) 
	  	n2=x1(index(n2(0)))
	  	list=[list,n2]
	  	treated(n2)=1
	  	qcompare2allcells,s,n2,int=y2,xx=x2,sig=e2,/nopr,exclusions=exclusions
	  	IF min([y1,y2]) le 0 THEN print,"ERROR occured"
	  	xmin=min([x1,x2])
	  	xmax=max([x1,x2])
	  	IF NOT KEYWORD_SET(noplot) THEN BEGIN
	    	colors,color,truecolors=truecolors,/noprint
	    	IF KEYWORD_SET(wset) THEN wset,0 ELSE window,0,xsi=800,title='Compare parts of efficiency corrections ',ysi=400
	    	plot,x1,y1,back=color(4),color=color(0),yrange=[min([y1,y2]),max([y1,y2])<1.2],xstyle=1,xr=[xmin,xmax],title=STRCOMPRESS(n2)  ;black
	    	oplot,where(r0 NE 0),r0(where(r0 NE 0)),color=color(0),psy=7   ;green
	    	oplot,x2,y2,color=color(0) ;blue
	  	ENDIF
		cmin=max([min(x1),min(x2)])
		cmax=min([max(x1),max(x2)])
		i1=where(x1 ge cmin and x1 le cmax AND x1 NE n1,comcount)
		i2=where(x2 ge cmin and x2 le cmax AND x2 NE n1)
		com=x1(i1)
		tmp=where(y1(i1)/y2(i2) EQ 0,count)
		IF count GE 1 THEN BEGIN
			print,'Ratio = 0 : ',com(tmp(0)),', ... ('+STRCOMPRESS(count,/RE)+')'
		ENDIF
		ratio=y1(i1)/y2(i2)                                 		;	ratio of arrays at a certain index
		sigm=ratio*SQRT((e1(i1)/y1(i1))^2+(e2(i2)/y2(i2))^2)		;	error calculation in non-linear situation
		okay=where(ratio NE 0 AND sigm NE 0,okcount)
		;	r0(n2)=(TOTAL(ratio(okay)))/okcount								;	mean of ratios, if angles below beamstop are excluded the formula of the weighted mean 
		r0(n2)=TOTAL((ratio(okay))/(sigm(okay))^2)/TOTAL(1./(sigm(okay))^2)	;	can be used  
		;	s0(n2)=SQRT(TOTAL(((sigm(okay))/okcount)^2))					;	rror calculation in linear situation, for weighted mean 
	  	s0(n2)=SQRT(1/TOTAL((1/sigm(okay))^2))								;	has to be used
		IF NOT KEYWORD_SET(noplot) THEN BEGIN
			IF KEYWORD_SET(wset) THEN wset,1 ELSE window,1,title='Efficiency compared to neighbours',ysi=380,xsi=800
			plot,com,ratio,back=color(4),color=color(0),yst=1,yrange=[r0(n2)*.9,r0(n2)*1.1],xstyle=1,xr=[xmin,xmax],title=STRCOMPRESS(n2)
			oplot,com,ratio+sigm,color=color(0)
			oplot,com,ratio-sigm,color=color(0)
			wset,0
		ENDIF
		IF NOT KEYWORD_SET(noprint) THEN BEGIN
	   		print,FORMAT="(2I5,' : ',2G14.6)",counter,n2,r0(n2),s0(n2)
		ENDIF
		xx=com
		y2(i2)=y2(i2)*r0(n2)                                    	;	array is aligned
		e2(i2)=y2(i2)*SQRT((e2(i2)/y2(i2))^2+(s0(n2)/r0(n2))^2)		;	error calculation in non-linear situation
		factor(com)=factor(com)+1
		y1(i1)= (factor(com)*y1(i1)+y2(i2))/(factor(com)+1)   		;	mean of aligned array and reference array which has the weight of the 
		                                                      		;	number of already aligned arrays
		e1(i1)=SQRT((e1(i1)*factor(com)/(factor(com)+1))^2 + ((e2(i2))/(factor(com)+1))^2)   ;	error calculation in linear situation
		tmp=where(x2 LT min(x1) OR x2 GT max(x1),count)       		;	*** new elements ... ****
		IF count GE 1 THEN BEGIN
			IF min(x2(tmp)) EQ 0 THEN BEGIN
				PRINT,'Cell 0 ...!'
			ENDIF
	 		x1=[x1,x2(tmp)]
	 		y1=[y1,y2(tmp)]
	 		e1=[e1,e2(tmp)]
	 		tmp=sort(x1)
	 		x1=x1(tmp)
	 		y1=y1(tmp)
	 		e1=e1(tmp)
		ENDIF
	ENDFOR
END
