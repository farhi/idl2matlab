;	$Id: hanning.pro,v 1.2 1994/01/17 18:10:21 dave Exp $
function Hanning, N1, N2, Alpha=alpha ;1D or 2D Hanning/Hamming window function
;+
; NAME:
;	HANNING
;
; PURPOSE:
;	Window function for Fourier Transform filtering.  May be used
;		for both the Hanning and Hamming windows.
;
; CATEGORY:
;	Signal, image processing.
;
; CALLING SEQUENCE:
;	Result = HANNING(N1) ;For 1 dimension.
;
;	Result = HANNING(N1, N2) ;For 2 dimensions.
;
; INPUTS:
;	N1:	The number of columns of the result.
;
;	N2:	The number of rows of the result.
;
; Keyword Parameters:
;	Alpha = width parameter of generalized Hamming window.  Alpha
;		must be in the range of 0.5 to 1.0.  If Alpha = 0.5,
;		the default, the function is called the "Hanning" window.
;		If Alpha = 0.54, the result is called the "Hamming" window.
;
; OUTPUTS:
;	Result(i) = 1/2 [1 - COS(2 PI i / (N-1)]
;
;	For two dimensions, the result is the same except that "i" is replaced
;	with "i*j", where i and j are the row and column subscripts.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;	DMS, May, 1987.
;	DMS, Jan, 1994. Added generalized width parameter.
;-

on_error,2                              ;Return to caller if an error occurs
if N_elements(alpha) le 0 then alpha = 0.5
a = 2 * !pi / (N1 -1)			;scale factor
If n_params(0) eq 1 then begin		;1d?
	return, (alpha-1.) * cos(findgen(N1)*a) + alpha
   endif else begin			;2d case
	b = 2 * !pi / (n2-1)		;dim 2 scale fact
	row = (alpha-1.) * cos(findgen(n1)*a) + alpha ;One row
	col = (alpha-1.) * cos(findgen(n2)*b) + alpha ;One column
	RETURN,(row # col)
   endelse
end
