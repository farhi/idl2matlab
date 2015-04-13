pro tagfile, file, TAG=tag, GET=valg ,SET=vals
;** *******
;**
;** Manu's rich idea .....
;** Procedure which sets and retrieves values from an INI file.
;** File is composed by twin elements such as "tag = value" (no comments at this time).
;** Use: tagfile,"filename" ,TAG=["TOTO","TITI"] ,SET=[valueTOTO,valueTITI]
;**      tagfile,"filename" ,TAG=["toto","titi"] ,GET= returnedStringValues

IF n_elements(tag)      eq 0 then RETURN
IF n_elements(vals)     gt 0 then IF n_elements(vals) ne n_elements(tag) then RETURN
IF n_elements(file)     ne 1 then file="default"
IF strpos(strlowcase(file),'.ini')  lt 0 then file= file+".ini"

ON_IOERROR,misget & u=-1 & line='' & ftag=[''] & fval=[''] & valg=''

;READ FILE.INI
;-------------
OPENR,u,file,/GET_LUN
	WHILE (not eof(u)) do begin
		READF,u,line
		idx=strpos(line,'=')
		IF idx gt 0 then begin
			ftag=[ftag,strtrim(strupcase(strmid(line,0    ,idx)),2)]
			fval=[fval,strtrim(         (strmid(line,idx+1,100)),2)]
		ENDIF
	ENDWHILE
MISGET: IF u gt 0 then FREE_LUN,u else print,!err_string

utag=strupcase(strtrim(tag,2))

;DO SET OR GET
;-------------
FOR i=0,n_elements(utag)-1 do begin miss=1 & jj=n_elements(ftag)

	WHILE (miss) and (jj gt 0) do begin
		jj=jj-1
		IF ftag(jj) eq utag(i) then miss=0
	ENDWHILE
	IF n_elements(vals) gt 0 then begin
		IF not miss then fval(jj)  =string(vals(i))  $
		ELSE	   begin fval=[fval,string(vals(i))]
			         ftag=[ftag,utag(i)] & ENDELSE				 
	ENDIF   ELSE $
		IF jj ge 0 then  valg=[valg,fval(jj)]  ELSE valg=[valg,'']
ENDFOR

nn  = n_elements(ftag)-1
IF nn eq 0  then RETURN

;RETURN IF GET
;-------------
IF n_elements(vals) eq 0 then begin
	IF n_elements(valg) gt 1 then valg=valg(1:n_elements(valg)-1)
	IF n_elements(valg) eq 1 then valg=valg(0)
	RETURN
ENDIF

ON_IOERROR,misput & u=-1

;WRITE FILE.INI IF SET
;---------------------
idx=SORT(ftag) & ftag=ftag(idx) & fval=fval(idx)

OPENW,u,file,/GET_LUN
	FOR i=1,nn do  PRINTF,u,ftag(i)+' = '+fval(i)

MISPUT: IF u gt 0 then FREE_LUN,u else print,!err_string

end
