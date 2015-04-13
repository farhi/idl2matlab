FUNCTION pn1,in_wk
;
; Read in pn1 xyz files gjk March95
;
@lamp.cbk

ON_IOERROR,misopen
	wwout =11
	
OPENR,in,in_wk,/get_lun
	ix=0
	iy=0
	iz=0
	minx  =1024
	miny  =1024
	wwout =7
	maxx  =-minx
	maxy  =-miny
	wwout =0
	wwout =INTARR(minx,miny)
	lnths =SIZE  (wwout)
	
	if lnths(0) eq 2 then begin
	
	ON_IOERROR,miseof
	
	WHILE (1) DO BEGIN
	
		READF,in,ix,iy,iz
		
		IF (ix lt lnths(1)) and (iy lt lnths(2)) and $
		   (ix ge 0)	    and (iy ge 0)   THEN BEGIN
			wwout(ix,iy)=iz
			if ix lt minx then minx=ix
			if ix gt maxx then maxx=ix
			if iy lt miny then miny=iy
			if iy gt maxy then maxy=iy
		ENDIF
	ENDWHILE
	
	miseof: FREE_LUN,in

;	Trim data to size
	wwout=wwout(minx:maxx , miny:maxy)
	lnths=SIZE(wwout)
	wks  =STRTRIM(STRING(one),2)
	iii=EXECUTE( 'x'+wks+'=INDGEN(lnths(1))+minx' )
	iii=EXECUTE( 'y'+wks+'=INDGEN(lnths(2))+miny' )
	
	x_tit(one)='E - TOTAL (Mev)'
	y_tit(one)='DELTA - E (Mev)'
	w_tit(one)= in_wk
	
	endif else wwout=8

misopen:RETURN, wwout
END
