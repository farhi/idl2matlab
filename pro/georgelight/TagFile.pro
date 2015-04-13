;***************************************************************
;** Project: Light/Lamp/George
;** File:    TagFile.pro
;** Version: 0.1
;** Date:    Jan, 23rd, 2002
;** Author:  D. Richard, E. Farhi
;** Object:  Read/Write configuration files containing lines
;**          [tag] = [value]
;**          Lines starting with '#', ';' or '%' are ignored and
;**          are part of the file header.
;
;** Require: <none>
;
; Procedures:
;   TagFile, file, TAG=utag, GET=valg , SET=vals, HEADER=header
;       can GET or SET (and sort in alpha order) the TAGS given as
;       parameters. Values may be scalar and arrays of type string
;       and numerical (floating).
;       In GET mode, if TAGS is a variable equal to '', then
;       utag/valg will contain all fields
;       The file HEADER can be written or read
;***************************************************************
function TagFile_String, val
;** Recursive function to build string equivalence of values
    if n_elements(val) gt 1 then begin
      Array2Str = '[ ';
      for tmp1=0, n_elements(val)-1 do begin
          if tmp1 eq 0 then $
            Array2Str = Array2Str + TagFile_String(val(tmp1)) $
          else $
            Array2Str = Array2Str + ', ' + TagFile_String(val(tmp1))
      endfor
      Array2Str = Array2Str + ' ]'
    endif else begin
      varsize = size(val)
      vardims = N_ELEMENTS(varsize) - 2
      type = varsize[vardims]
      if type eq 7 then begin
         val = strsplit(val,"'",/extract, /PRESERVE_NULL)
         val = strjoin(val,'"')
         Array2Str = "'"+val+"'"
         ok = 1
      endif else ok = execute('Array2Str = string(val)')
      if not ok then begin
        print, '[TagFile] Can not convert to string'
        help, val
        Array2Str = "'<Could not convert to string>'"
      endif
    endelse
    return, Array2Str
end

pro TagFile, file, TAG=utag, GET=valg , SET=vals, HEADER=header
;** *******
;**
;** Manu's rich idea .....
;** Procedure which sets and retrieves values from an INI file.
;** File is composed by twin elements such as "tag = value" (no comments at this time).
;** Use: tagfile,"filename" ,TAG=["TOTO","TITI"] ,SET=[valueTOTO,valueTITI]
;**      tagfile,"filename" ,TAG=["toto","titi"] ,GET= returnedStringValues

IF n_elements(vals)     gt 0 then $
    IF not((n_elements(utag) eq 1) or (n_elements(utag) eq n_elements(vals))) then begin
        print, '{TagFile}: Wrong number of elements in Values/Tags'
        help, utag, vals
        RETURN
    endif
IF n_elements(file)     ne 1 then file="default"
IF strpos(file,'.')     lt 0 then file= file+".ini"

ON_IOERROR,misget & u=-1 & line='' & ftag=[''] & fval=[''] & valg=''

;READ FILE.INI
;-------------
HeaderInFile = ''
OPENR,u,file,/GET_LUN
  WHILE (not eof(u)) do begin
    READF,u,line
    line = strtrim(line,2)
    FirstChr = strmid(line, 0,1)
    if (FirstChr eq ';') or (FirstChr eq '%') or (FirstChr eq '#') then $
        HeaderInFile = [ HeaderInFile, line] $
    else begin
        idx=strpos(line,'=')
        IF idx gt 0 then begin
          ftag=[ftag,strtrim(strmid(line,0    ,idx(0)),2)]
          fval=[fval,strtrim(strmid(line,idx(0)+1,strlen(line)),2)]
        ENDIF
    ENDELSE
  ENDWHILE
MISGET: IF u gt 0 then FREE_LUN,u else begin
            print,!err_string
            print, '[TagFile] File '+file+' could not be opened for reading. Creating it.
        endelse

if n_elements(utag) eq 0 then utag = ftag
if n_elements(utag) eq 1 and strlen(utag(0)) eq 0 then utag = ftag

;DO SET OR GET
;-------------
FOR i=0,n_elements(utag)-1 do begin miss=1 & jj=n_elements(ftag)

  WHILE (miss) and (jj gt 0) do begin
    jj=jj-1
    IF strtrim(strupcase(ftag(jj)),2) eq strtrim(strupcase(utag(i)),2) then miss=0
  ENDWHILE
  IF n_elements(vals) gt 0 then begin   ; SET
    if n_elements(utag) eq 1 then val = TagFile_String(vals) $
    else  val = TagFile_String(vals(i))
    IF not miss then fval(jj) = val  $
    ELSE    begin
        fval=[fval, val]
        ftag=[ftag, utag(i)]
    ENDELSE
  ENDIF   ELSE $                        ; GET
    IF jj ge 0 then  valg=[valg,fval(jj)]  $
    ELSE valg=[valg,'']
ENDFOR

nn  = n_elements(ftag)-1
IF nn eq 0  then RETURN

;RETURN IF GET
;-------------
IF n_elements(vals) eq 0 then begin
  IF n_elements(valg) gt 1 then valg=valg(1:n_elements(valg)-1)
  IF n_elements(valg) eq 1 then valg=valg(0)
  header = HeaderInFile
  RETURN
ENDIF

ON_IOERROR,misput & u=-1

;WRITE FILE.INI IF SET
;---------------------
idx=SORT(ftag) & ftag=ftag(idx) & fval=fval(idx)

;** default header if not given
if n_elements(header) eq 0 then $
    header =[ '# Configuration file: '+file,$
              '# Date:               '+systime(0)]

OPENW,u,file,/GET_LUN
for i=0,n_elements(header)-1 do printf,u,header(i)
FOR i=1,nn do  PRINTF,u,ftag(i)+' = '+fval(i)

MISPUT: IF u gt 0 then FREE_LUN,u else print,!err_string

end
