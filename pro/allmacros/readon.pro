FUNCTION readon,Win,numor=numor,monitor=monitor,time=time,maxnumor=maxnumor,$
				noplot=noplot,noprint=noprint,$
				minmonitor=minmonitor,minpsd=minpsd,mintime=mintime
;********
;**
;** The call is w6=rdand(62571,62572)&normalize,w6,24e6&w7=readon(w6,mon=24e6)
;** or w2=rdrun(82699)&normalize,w2,1e5&w3=readon(w2,mon=1e5)
;** This function will append all more recent numors to a given workspace
;** starting from the maximum numor (plus one) in the given workspace
;** or an optionally given numor (greater than the maximum numor in the workspace)
;** Last modification 02-May-02 by Th.Hansen: noprint and noplot
;** Last modification 06-May-02 by Th.Hansen: min(psd/monitor/time)

take_datp,datp
Wout=Win
E=datp.e
Y=datp.y
PV=datp.pv
N=datp.n
IF NOT KEYWORD_SET(minpsd) THEN minpsd=0
IF NOT KEYWORD_SET(mintime) THEN mintime=0
IF NOT KEYWORD_SET(minmonitor) THEN minmonitor=0
IF NOT KEYWORD_SET(numor) THEN numor=0 ELSE numor=numor-1
IF NOT KEYWORD_SET(maxnumor) THEN maxnumor=999999
IF NOT KEYWORD_SET(monitor) AND NOT KEYWORD_SET(monitor) THEN BEGIN
	IF datp.p(39) GT 0 THEN monitor=datp.p(39)
ENDIF
numor=(numor>MAX(datp.pv(29,*)))+1
numor=ROUND(numor)
IF NOT KEYWORD_SET(noprint) THEN PRINT,'First numor to append: ',numor
d=1
REPEAT BEGIN
	;PRINT,'Try to read numor',numor
	W=rdrun(numor,datp=d)
	IF N_ELEMENTS(W) GT 1 AND TOTAL(W(*,0)) GT minpsd AND d.n(0) GT minmonitor AND d.n(1) GT mintime THEN BEGIN
		IF NOT KEYWORD_SET(noprint) THEN PRINT,'Appending numor',numor
		IF NOT KEYWORD_SET(noplot) THEN SEE,w1,/below
		IF KEYWORD_SET(monitor) THEN BEGIN
			W=W/d.n(0)*monitor
			d.E=d.E/d.n(0)*monitor
			d.N=d.N/d.n(0)*monitor
		ENDIF ELSE IF KEYWORD_SET(time) THEN BEGIN
			W=W/d.n(1)*time
			d.E=d.E/d.n(1)*time
			d.N=d.N/d.n(1)*time
		ENDIF
		Wout=[[Wout],[W]]
		E=[[E],[d.e]]
		Y=[Y,d.y]
		PV=[[PV],[d.pv]]
		N=[[[N]],[[d.N]]]
	ENDIF ELSE PRINT,'Not appending',numor, TOTAL(W(*,0)), d.n(0:1<(n_elements(d.n)-1))
	numor=numor+1
ENDREP UNTIL N_ELEMENTS(W) LE 1 OR numor GT maxnumor
IF NOT KEYWORD_SET(noprint) THEN PRINT,'Last Numor',numor-2
mod_datp,datp,'e',E
mod_datp,datp,'y',Y
mod_datp,datp,'pv',PV
mod_datp,datp,'n',N
pos=STRPOS(datp.other_tit,'Run ',0)
pos2=STRPOS(datp.other_tit,'*d20',0)
IF NOT KEYWORD_SET(noprint) THEN print,pos
subtitle=datp.other_tit
subtitle=STRMID(subtitle,0,pos+4)$
	+STRCOMPRESS(MIN(ROUND(datp.pv(29,*))),/RE)+':'$
	+STRCOMPRESS(MAX(ROUND(datp.pv(29,*))),/RE)$
	+STRMID(subtitle,pos2,strlen(subtitle)-pos2)
IF NOT KEYWORD_SET(noprint) THEN PRINT,subtitle
datp.other_tit=subtitle
mod_datp,datp,'e',E
give_datp,datp
return, Wout
end
