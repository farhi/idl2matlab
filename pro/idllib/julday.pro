; $Id: julday.pro,v 1.2 1993/04/28 14:14:12 dave Exp $

function JULDAY, MONTH, DAY, YEAR
;+
; NAME:
;	JULDAY
;
; PURPOSE:
;	Calculate the Julian Day Number for a given month, day, and year.
;	This is the inverse of the library function CALDAT.
;	See also caldat, the inverse of this function.
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	Result = JULDAY(Month, Day, Year)
;
; INPUTS:
;	MONTH:	Number of the desired month (1 = January, ..., 12 = December).
;
;	DAY:	Number of day of the month.
;
;	YEAR:	Number of the desired year.
;
; OPTIONAL INPUT PARAMETERS:
;	None.
;
; OUTPUTS:
;	JULDAY returns the Julian Day Number (which begins at noon) of the 
;	specified calendar date.
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
; MODIFICATION HISTORY:
;	Translated from "Numerical Recipies in C", by William H. Press,
;	Brian P. Flannery, Saul A. Teukolsky, and William T. Vetterling.
;	Cambridge University Press, 1988 (second printing).
;
;	AB, September, 1988
;-
;
ON_ERROR, 2		; Return to caller if errors

MONTHS = ['JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG', $
	  'SEP','OCT','NOV','DEC']

; Gregorian Calander was adopted on Oct. 15, 1582
GREG = 15L + 31L * (10L + 12L * 1582L)

; Process the input, if all are missing, use todays date.
NP = n_params()
case NP of
	0: begin
	    DATE = systime()
	    L_MONTH = long(where(strupcase(strmid(DATE, 4, 3)) eq MONTHS))
	    L_MONTH = L_MONTH(0) + 1	; Scalarize it...
	    L_DAY = long(strmid(DATE, 8, 2))
	    L_YEAR = long(strmid(DATE, 20, 4))
	    end
	3: begin
	    L_MONTH = LONG(MONTH)
	    L_DAY = LONG(DAY)
	    L_YEAR=LONG(YEAR)
	    if (L_YEAR eq 0) then message, 'There is no year zero.'
	    end
	else: message, 'Wrong number of parameters.'
	endcase



if (L_YEAR lt 0) then L_YEAR = L_YEAR + 1
if (L_MONTH gt 2) then begin
	JY = L_YEAR
	JM = L_MONTH + 1
    endif else begin
	JY = L_YEAR - 1
	JM = L_MONTH + 13
    endelse

JUL = long(365.25 * JY) + long(30.6001 * JM) + L_DAY + 1720995

; Test whether to change to Gregorian Calandar.
if ((L_DAY + 31L * (L_MONTH + 12L * L_YEAR)) ge GREG) then begin
	JA = long(0.01 * JY)
	JUL = JUL + 2 - JA + long(0.25 * JA)
	endif

	return, JUL

end
