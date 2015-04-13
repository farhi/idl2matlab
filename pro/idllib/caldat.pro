; $Id: caldat.pro,v 1.2 1993/04/28 14:14:12 dave Exp $

pro CALDAT, Julian, Month, Day, Year
;+
; NAME:
;	CALDAT
;
; PURPOSE:
;	Return the month, day and year corresponding to a given julian date.
;	This is the inverse of the function JULDAY.
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	CALDAT, Julian, Month, Day, Year
;	See also: julday, the inverse of this function.
;
; INPUTS:
;	JULIAN contains the Julian Day Number (which begins at noon) of the 
;	specified calendar date.  It should be a long integer.
; OUTPUTS:
;	MONTH:	Number of the desired month (1 = January, ..., 12 = December).
;
;	DAY:	Number of day of the month.
;
;	YEAR:	Number of the desired year.
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
;	DMS, July, 1992.
;-
;
ON_ERROR, 2		; Return to caller if errors

IGREG = 2299161L	;Beginning of Gregorian calendar

julian = long(julian)	;Better be long

if julian ge igreg then begin
	jalpha = long(((julian - 1867216) - 0.25d0) / 36524.25)
	ja = julian + 1 + jalpha - long(0.25d0 * jalpha)
endif else ja = julian

jb = ja + 1524
jc = long(6680.0 + ((jb-2439870)-122.1)/365.25)
jd = long(365 * jc + (0.25 * jc))
je = long((jb - jd) / 30.6001)

day = jb - jd - long(30.6001 * je)
month = je -1
if (month gt 12) then month = month - 12
year = jc - 4715
if month gt 2 then year = year - 1
if year le 0 then year = year - 1
end
