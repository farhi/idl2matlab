FUNCTION HIST_EQUAL, A, BINSIZE = binsize, MAXV = MAXV, MINV = MINV, $
	TOP = top, HISTOGRAM_ONLY=histogram_only
;
;+
; NAME:
;	HIST_EQUAL
;
; PURPOSE:
;	Return a histogram-equalized image or vector.
;
; CATEGORY:
;	Z1 - Image processing, spatially invariant.
;
; CALLING SEQUENCE:
;	Result = HIST_EQUAL(A [, MINV = minv] [, MAXV = maxv])
;
; INPUTS:
;	A:	The array to be histogram-equalized.
;
; KEYWORD PARAMETERS:
;     BINSIZE:	Size of bin to use.  If this keyword is omitted, the value 1
;		is used.  Ignored for byte type data.  Default = approx 5000
;		bins.
;
;	HISTOGRAM_ONLY: If set, return the cumulative distribution histogram,
;		rather than the histogram equalized array.  MAXV, MINV, and
;		BINSIZE will be set, describing the scaling of the histogram,
;		if not specified.
;	MAXV:	The maximum value to consider.  If this keyword is omitted,
;		the maximum element is used.  Input elements greater than or
;		equal to MAXV are output as 255.
;
;	MINV:	The minimum value to consider.  If this keyword is omitted,
;		the minimum is computed. Input elements less than or equal
;		to MINV are output as zero.
;
;	TOP:	The maximum value to scale the output array. If this keyword 
;		is omitted, 255 is used.
;
; OUTPUTS:
;	A histogram-equalized byte array is returned.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	The output array is always of byte type and is scaled from 0 to TOP. 
;	Floating-point arrays should not have small ranges, (e.g., less than
;	around 255) unless a binsize is specified.
;
; PROCEDURE:
;	The HISTOGRAM function is used to obtain the density distribution of
;	the input array.  The histogram is integrated to obtain the 
;	cumulative density-propability function and finally the lookup 
;	function is used to transform to the output image.
;
;	The first element of the histogram is always zeroed, to remove
;	the background.
; EXAMPLE:
;	Create a sample image using the IDL DIST function and display it by
;	entering:
;
;		IMAGE = DIST(100)
;		TV, DIST
;	
;	Create a histogram-equalized version of the byte array, IMAGE, and
;	display the new version.  Use a minumum input value of 10, a maximum 
;	input value of 200, and limit the top value of the output array to 
;	220.  Enter:
;
;		NEW = HIST_EQUAL(IMAGE, MINV = 10, MAXV = 200, TOP = 220)
;		TV, NEW 
;
; MODIFICATION HISTORY:
;	August, 1982. Written by DMS, RSI.
;	Feb, 1988, Revised for Sun, DMS.
;	Dec, 1994. DMS. Improved handling offloating/double images with
;			large or small ranges.  Default value for MINV is
;			computed, rather than set to 0.
;-
;
	on_error,2                        ;Return to caller if an error occurs
	if n_elements(top) eq 0 then top = 255
	S = SIZE(A)		;Type of var?
	type = s(s(0)+1)
	If type eq 1 then begin	;byte var?
		if n_elements(binsize) le 0 then binsize = 1
		p = histogram(a, binsize = binsize)
		p(0)=0
		up = n_elements(p)-1	;max value
		if n_elements(minv) ne 0 then p(0:minv) = 0  $
		else minv = 0	
		if n_elements(maxv) ne 0 then begin
		    if up gt maxv then p(maxv:*)=0
		endif else maxv = up
		for i=1,up do p(i)=p(i)+p(i-1) ;integrate
		if keyword_set(histogram_only) then return, p
		P=bytscl(p,top=top)
		RETURN,P(A)			;Transform & return.
	   endif else begin
		if n_elements(maxv) le 0 then maxv = max(a, min=minv1)
		if n_elements(minv) le 0 then begin
		  if n_elements(minv1) eq 1 then minv = minv1 else minv = min(a)
		  endif
		if maxv le minv then $
		  message,'Image max is equal to min, or illegal range'
		if n_elements(binsize) eq 0 then begin	;Calc binsize?
		   if type le 3 then $
			binsize = CEIL((long(maxv)-long(minv)) / 5000.) > 1 $
		   else if type le 5 then $	;Floating or double
			binsize = (maxv-minv) / 5000. $
		   else message,'Complex images not allowed'
		   endif			;Binsize
		p = histogram(a, omin=minv, omax = maxv,bin=binsize)
		p(0) = 0
		for i=1L,n_elements(p)-1 do p(i)=p(i)+p(i-1) ;Cumul. integral
		if keyword_set(histogram_only) then return, p
		P=bytscl(p, top = top)
		if binsize eq 1 then begin
			if minv eq 0 then return,p(a) else RETURN,P(A-minv)
		endif else begin
		    if minv eq 0 then return, p(a/binsize) else $
		    return, P((a-minv)/binsize)
		endelse
	endelse
end

