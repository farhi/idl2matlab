; A Library of time functions

; Mark McCrum


FUNCTION sec2time, secs		;This function converts a time in seconds to
;**********************		;one in days hours minutes and seconds. The
				;return format is [days, hours, mins, secs]
	mins=LONG(secs/60)
	secs=secs MOD 60
	hours=LONG(mins/60)
	mins=mins MOD 60
	days=LONG(hours/24)
	hours=hours MOD 24

	time=[days, hours,mins,secs]
RETURN, time
END

FUNCTION addTime, t1, t2	;Adds two times in the format [days, mins,hours,secs]	
;***********************	;returns in the same format

	ss=t1(3)+t2(3)
	sm=t1(2)+t2(2)
	sh=t1(1)+t2(1)
	sd=t1(0)+t2(0)

	IF ss GE 60 THEN BEGIN 	
			  ss=ss-60  
			  sm=sm+1
			 END
	IF sm GE 60 THEN BEGIN  
			  sm=sm-60
			  sh=sh+1
			 END

	IF sh GE 24 THEN BEGIN 
			  sh=sh-24
			  sd=sd+1
			 END
	RETURN, [sd,sh,sm,ss]
END

FUNCTION timeAfter, start, duration	;start is a time in format returned by the IDL
;********************************** 	 BIN_DATE function. Duration is some time in
					;the format [days,hours,mins,secs]. timeAfter
					;returns what the time will be 'duration' after
				        ; 'start'. Return format is same as for start
					; parameter

	;calculate julian day at start
	julianStart=julday(start(1),start(2),start(0))	
	
	;do the addition using the julian day instead of day of
	;month (makes life much easier)
	timeSum=addTime([julianStart,[start(3:5)]], duration)

	;Now work out the resulting calendar date by converting the julian day
	caldat, timeSum(0), month, day, year

	RETURN, [year, month, day, timeSum(1), timeSum(2), timeSum(3)]

END

FUNCTION now		;Returns the system time and date in BIN_DATE format
;************
	RETURN, bin_date(systime(0))
END 

FUNCTION time2string, time	;Coverts date in BIN_DATE format back to a 
;*************************	;string

	monthYear=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov', $
		   'Dec']
	month=monthYear(time(1)-1)
	text=month+STRING(time(2))+STRING(time(3))+':'+$
			STRING(time(4))+':'+STRING(time(5))
	text=STRCOMPRESS(text)
RETURN, text
END

PRO time
	PRINT, 'time.pro: library loaded'
END
