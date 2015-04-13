;	***************************
	     PRO myhelp_in4, text
;	***************************

;This is effective when the string-array TEXT is more than one element.

	ON_IOERROR, finished

	OPENR, 1, '/home/cs/lambda/macros/IN4/helpin4.txt'

	text=''	& line=''	& sline=''

	WHILE (NOT EOF(1)) DO BEGIN
		READF, 1, line
		l=STRLEN(line)
		IF (l LE 80) THEN BEGIN
			sline=line
		ENDIF
		s=0
		WHILE (l GT 80) DO BEGIN
			l=l-s
			IF (l GT 80) THEN s=RSTRPOS(STRMID(line,0,80),' ') $
					ELSE s=l
			sline=[sline,STRMID(line,0,s)]
			line=STRMID(line,s+1,l)
		ENDWHILE
		text=[text,sline]
	ENDWHILE

finished:

	CLOSE, 1

	RETURN
	END
