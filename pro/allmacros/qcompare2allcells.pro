PRO qcompare2allcells,s,n,xx=xx,exclusions=exclusions,$
                       int_ratio=int_ratio,sig_ratio=sig_ratio,$
                       noprint=noprint,plot=plot,single=single,truecolors=truecolors
;+
; NAME:
;	qcompare2allcells
;
; PURPOSE:
;	This procedure is the first routine called from alignloop to calculate relative efficiencies
;
; CATEGORY:
;	Instrument.
;
; CALLING SEQUENCE:
;	qcompare2allcells,s,n
;
; INPUTS:
;   s		:	Workspace (2D array, nd * steps elements, as defined in LAMP, normally a 2theta scan over a vanadium)
;	n		:	detector cell number to be compared to all others
;
; OPTIONAL INPUTS:
;	none
;	
; KEYWORD PARAMETERS:
;	xx			:	?
;	exclusions	:	?
;	int_ratio	:	intensity ratio of n1 and n2
;	sig_ratio	:	?
;	noprint		:	if set, no printed output will be created
;	plot		:	if set, some output will be plotted
;	single		:	?
;	truecolors	:	?
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
;	Modified by:	Thomas C Hansen,	November 2000 (documentation)
;
;-
	wset=0
	IF NOT keyword_SET(plot) THEN plot=0 ELSE wset=1
	IF NOT keyword_SET(truecolors) THEN truecolors=0
	IF KEYWORD_SET(single) THEN noprint=0 ELSE single=0
	n=LONG(n)
	steps=N_ELEMENTS(s(0,*))
	cells=N_ELEMENTS(s(*,0))
	nmin=n-(steps-2)>0
	nmax=n+(steps-2)<(cells-1)
	xx=indgen(nmax-nmin+1)+nmin
	int_ratio=DBLARR(nmax-nmin+1)+1
	sig_ratio=int_ratio*DOUBLE(0)
	FOR i=nmin,nmax DO BEGIN 
		IF i NE n THEN BEGIN
			qcellcompare,s,n,i,int_ratio=int,sig_ratio=sig,plot=plot,wset=wset,print=single,true=truecolors,exclusions=exclusions
			IF NOT KEYWORD_SET(noprint) THEN IF NOT KEYWORD_SET(single) THEN PRINT,i,int,sig
 			int_ratio(where(xx eq i))=int
			sig_ratio(where(xx eq i))=sig
		ENDIF
	ENDFOR
END
