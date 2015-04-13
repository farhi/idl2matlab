; $Id: label_date.pro,v 1.2 1993/10/05 17:34:48 doug Exp $
FUNCTION LABEL_DATE, axis, index, x, DATE_FORMAT = format, MONTHS = months
;+
; NAME:
;	LABEL_DATE
;
; PURPOSE:
;	This function labels axes with dates.
;
; CATEGORY:
;	Plotting.
;
; CALLING SEQUENCE:
;	To set up:
;		dummy = LABEL_DATE(DATE_FORMAT='string')
;	To use:
;		PLOT, x, y, XTICKFORMAT='LABEL_DATE'
;
; INPUTS:
;	No explicit user defined inputs. When called from the plotting
;	routines, the input parameters are (Axis, Index, Value)
;
; KEYWORD PARAMETERS:
;	DATE_FORMAT: a format string which may contain the following:
;		       %M for month (3 character abbr)
;		       %N for month (2 digit abbr)
;		       %D for day of month,
;		       %Y for 4 digit year.
;		       %Z for last two digits of year.
;		       %% is %.
;		     Other characters are passed directly thru.
;		     For example, '%M %D, %Y' prints DEC 11, 1993
;		       '%M %2Y' yields DEC 93
;		       '%D-%M' yields 11-DEC
;		       '%D/%N/%Y' yields 11/12/1993
;		       '%M!C%Y' yields DEC on the top line, 1993 on
;		       the bottom (!C is the new line graphic command).
;
;	MONTHS:      The names of the months, a twelve element string array.
;		     If omitted, use Jan, Feb, ..., Dec.
;
; OUTPUTS:
;	The date string to be plotted.
;
; COMMON BLOCKS:
;	LABEL_DATE_COM.
;
; RESTRICTIONS:
;	Only one date axis may be simultaneously active.
;
; PROCEDURE:
;	Straightforward.
;
; EXAMPLE:
;	For example, to plot from Jan 1, 1993, to July 12, 1994:
;	  Start_date = julday(1, 1, 1993)
;	  End_date = julday(7, 12, 1994)
;	  Dummy = LABEL_DATE(DATE_FORMAT='%N/%D')  ;Simple mm/dd
;	  x = findgen(end_date+1 - start_date) + start_date ;Time axis
;	  PLOT, x, sqrt(x), XTICKFORMAT = 'LABEL_DATE', XSTYLE=1
;	  (Plot with X axis style set to exact.)
;	
; MODIFICATION HISTORY:
;	DMS, RSI.	April, 1993.	Written.
;-

COMMON label_date_com, fmt, month_chr

if keyword_set(format) then begin	;Save format string?
	if keyword_set(months) then month_chr = months $
	else month_chr = ['Jan','Feb','Mar', 'Apr', 'May', 'Jun', 'Jul', $
		'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
	fmt = format
	return, 0
	endif

if n_elements(month_chr) ne 12 or n_elements(fmt) le 0 then $
	message,' Not initialized.'

caldat, long(x), month, day, year	;Get the calendar date from julian
n = strlen(fmt)
out = ''

for i=0, n-1 do begin			;Each format character...
    c = strmid(fmt, i, 1)		;The character.
    if c eq '%' then begin
	i = i + 1
	c = strmid(fmt, i, 1)		;The function
	case c of		;format character?
	'M' : out = out + month_chr(month-1)
	'N' : out = out + string(format='(i2.2)', month)
	'D' : out = out + string(format='(i2.2)', day)
	'Y' : out = out + string(format='(i4)', year)
	'Z' : out = out + string(format='(i2.2)', year  mod 100)
	'%' : out = out + '%'
	else : message, 'Illegal character in date format string: '+fmt
	endcase
    endif else out = out + c
endfor
return, out
end
