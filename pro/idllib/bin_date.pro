; $Id: bin_date.pro,v 1.2 1993/10/04 18:45:20 doug Exp $

function bin_date, ascii_time
;+
; NAME:
;	BIN_DATE
;
; PURPOSE:
;	This function converts a standard form ascii date/time string
;	to a binary string.
;
; CATEGORY:
;	Date/time functions.
;
; CALLING SEQUENCE:
;	Result = BIN_DATE(Asc_time)
;
; INPUTS:
;	Asc_time: the date/time to convert in standard ascii format.
;		  If omitted, use the current date/time.  
;	  	  Standard form is a 24 character string:
;			DOW MON DD HH:MM:SS YYYY
;		  where: DOW = day of week, MON = month, DD=day of month,
;			HH:MM:SS = hour/minute/second, YYYY = year.
;
; OUTPUTS:
;	This function returns a 6 element integer array containing:
; 	Element 0 = year	e.g. 1992
;		1 = month	1-12
;		2 = day		1-31
;		3 = hour	0-23
;		4 = minute	0-59
;		5 = second	0-59
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
; 	Written by:	DMS /RSI, Jul, 1992.
;-


if n_elements(ascii_time) eq 0 then ascii_time = systime(0)	;Current time
m = strmid(ascii_time,4,3)	;Month
m = where(strupcase(m) eq $
 ['JAN','FEB','MAR','APR', 'MAY', 'JUN', 'JUL', 'AUG','SEP','OCT','NOV','DEC'])
return, [ strmid(ascii_time, 20,4), $	;year
	m(0) + 1, $			;Month
	strmid(ascii_time, 8,2), $	;day
	strmid(ascii_time, 11,2), $	;Hour
	strmid(ascii_time, 14,2),$	;minute
	strmid(ascii_time, 17,2)]	;second
end

