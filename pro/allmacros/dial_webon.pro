pro dial_webon_macro,D
;** ****************
;**
bid=FINDFILE(D.webpth,count=nn)
IF  nn gt 0 then begin wline=[''] & value=['']
    on_ioerror,misopn
    OPENR,u,D.webpth,/GET_LUN,/DELETE
	on_ioerror,misread
	line=''
	WHILE (not EOF(u)) do begin READF,u,line & wline=[wline,line] & ENDWHILE
	misread:  on_ioerror,misopn
	FREE_LUN ,u
	IF n_elements(wline) gt 2 then if wline(1) eq D.pwd then begin
		wline=wline(2:*)
		XICUTER,' ;--->WebOn '+!stime+' {'+wline(0)+'...}'
		CATCH,stat & if stat ne 0 then begin print,!err_string & return & endif
		FOR i=0,n_elements(wline)-1 do $
			IF wline(i) ne D.pwd then begin line=strtrim(wline(i),2)
			
			   if strpos(line,'ii=barns_i') ge 0 then jj=EXECUTE(line) $
			   else if line gt ' ' then begin XICUTER,(line+'') & value=[value,line]
			        endif
			ENDIF
		XICUTER,' ;---> '
		if n_elements(value) gt 1 then value=value(1:n_elements(value)-1)
		WebDo,'log',value,12
	ENDIF
	DialModValue,value, TAG='VALUE'
    misopn:
endif else if D.value(0) ne '' then DialModValue,[''], TAG='VALUE'
end

function dial_webon
;******* **********
;**
return,{NAME:"webon",PLOT:0,HISTORY:0,VALUE:[''],FREQUENCY:2,WEBPTH:'',PWD:'',WUPDATE:-1}
end
