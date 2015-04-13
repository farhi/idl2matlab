;************************************************************************
;* rdedf.pro                                             		*
;* Pupose:      rdedf is the function to read and display the ESRF data *
;*		files with LAMP from ILL.				*	
;* Author:      Tobias Karrer                               	    	*
;* Created On:  10/03/97                                        	*
;*----------------------------------------------------------------------*
;* Modifications:                                               	*
;* 30/05/97	changed the way to read the data, because LAMP wasn't	*
;*		able to read modified Images (p. ex. with saxs_add)	*
;* 11/06/97	added the variable 'pref' to specify the prefix of the  *
;*		filename						*
;* 12/06/97	modified the way to get the filename, after doing tests *
;* 02/07/97	removed code to display image number			*
;* 30/07/97	added code to read format and dimension out of header	*
;* 21/06/01	status=11 before the OPENR (D.R.)			*
;* 21/06/01	imgNo =0  only when entering rdedf (D.R.)		*
;* 21/06/01	nofile:if c_unit le 0 then c_file='?' (D.R.)		*
;* 15/11/01	get parameters differently (D.R.)		        *
;************************************************************************

;***************************************************************************************
function rdedf ,inst , path , filename , status , datp
;******* *****
;**

common c_edf	 , pref  ,suf
common c_edf_open, c_unit, c_file, c_imgNo

data  =0

;** if an error occurs, CATCH makes to quit the function, printing the error messages **
CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN, DATA & endif

imgNo=0			;** variable to keep the image number
maxilen=169		;** length of the maximal header in the edf-file
w=0 & s=0 & i=0 & j=0	;** variables to count loops, arrays, etc...
flag=0			;** variable for title - 0:no title, 1:display title
title =''		;** variable to keep the title of the image
head=STRARR(maxilen) 	;** whole header
par =STRARR(maxilen) 	;** value of header params
text=STRARR(maxilen)	;** name of header params
line=''			;** temporary variable for one headerline
formt='?'

;** pref is a common block defined variable and is set with function 'set_pref' ******
IF N_ELEMENTS(pref) EQ 1 THEN BEGIN
				IF STRPOS(pref,'_') EQ -1 THEN prefname=pref+'_' $
						  	  ELSE prefname=pref
			      ENDIF ELSE prefname=''

;** suf is a common block defined variable and is set with function 'set_suf' ******
IF N_ELEMENTS(suf) EQ 1 THEN BEGIN
				IF STRPOS(suf,'_') EQ -1 THEN sufname='_'+suf $
						  	 ELSE sufname=suf 
			     ENDIF ELSE sufname=''

;** if filename is an array, first element = filename, second = image number ******
IF N_ELEMENTS(filename) GT 1 THEN BEGIN imgNo=fix(filename(1))-1
					filename=filename(0) & ENDIF

IF N_ELEMENTS(c_unit)   EQ 0 THEN BEGIN c_unit=0 & c_file=''
					c_imgNo=0 & ENDIF


;** looking for the extension '.edf'  ******
ext=STRPOS(filename,'.edf')

IF ext LT 0 THEN file=prefname+filename+sufname+'.edf' $
ELSE BEGIN hypnorm =STRPOS(filename,'[',ext)		;** '[' given? ******
	   hypshift=STRPOS(filename,'{',ext)		;** '{' given? ******
	   IF (hypnorm LT 0) AND (hypshift LT 0) THEN BEGIN file=filename

	   ENDIF $
	   ELSE BEGIN 		;** get the given image number ******
		file=strmid(filename,0,ext+4)
		ON_IOERROR, mislong	;** on convert error to 'long,
					;** jump to label mislong
		imgNo=long(strmid(filename,ext+5,6))-1
		flag=1
		mislong: IF imgNo LT 0 THEN print,'No valid image number'
	   ENDELSE
ENDELSE

ON_IOERROR,nofile	;** on open error jump to label nofile

status=11		;** display ' Cant open the file or file not found' ******

;** it is checked, if the file out of which to read the image is already open ******  
if (file ne c_file) or (imgNo lt c_imgNo) then begin
	 if c_unit  gt 0 then FREE_LUN,c_unit
	 c_file=file & c_unit=0      & c_imgNo=0
	 OPENR,c_unit,path+file,/GET_LUN
	 endif

ON_IOERROR,erread	;** on read error jump to label erread
status=13		;** display ' Data file incomplete' ******

;** read image data and header values ******
;**
FOR loop=c_imgNo,imgNo DO BEGIN

	j=0
	;** read each single line of the file and save it in head(j), ******
	;** until line contains '}' ******
	REPEAT BEGIN READF,c_unit,line & if j lt maxilen then head(j)=line & j=j+1

	;** looking for 'DataType'  in the header ******
	IF STRPOS(line,'DataType') GT -1 THEN BEGIN
		w=STRPOS(line,'=')
		s=STRPOS(line,';')
		formt=STRCOMPRESS(STRMID(line,w+1,s-w-1), /REMOVE_ALL)
	ENDIF
	;** looking for dimension 1 in the header ******
	IF STRPOS(line,'Dim_1') GT -1 THEN BEGIN
		w=STRPOS(line,'=')
		s=STRPOS(line,';')
		dim1=long(STRMID(line,w+1,s-w-1))
	ENDIF
	;** looking for dimension 2  in the header ******
	IF STRPOS(line,'Dim_2') GT -1 THEN BEGIN
		w=STRPOS(line,'=')
		s=STRPOS(line,';')
		dim2=long(STRMID(line,w+1,s-w-1))
		IF N_ELEMENTS(formt) EQ 1 THEN BEGIN
			CASE formt OF
			'ShortValue':		data=INTARR(dim1,dim2)
			'UnsignedShort':	data=INTARR(dim1,dim2)
			'IntegerValue':		data=LONARR(dim1,dim2) 
			'UnsignedInteger':	data=LONARR(dim1,dim2) 
			'FloatValue':		data=FLTARR(dim1,dim2)
			'DoubleValue':		data=DBLARR(dim1,dim2)
			ELSE:
	    		ENDCASE
		ENDIF
		IF N_ELEMENTS(data) EQ 1 THEN BEGIN
			print, "DataType:",formt," not known, using UnsignedShort as default"
			data=INTARR(dim1,dim2) & formt='UnsignedShort'
		ENDIF
	ENDIF
	;** looking for image number in the header ******
	;** if found, store value in 'titleVal' ******
	IF STRPOS(line,'Image') GT -1 THEN $
		titleVal=STRMID(line,0,STRPOS(line,';'))
	ENDREP UNTIL (StrPos(line,'}') NE -1)

	;** read the image data *******
	READU,c_unit,data  & status=0		;** display 'Successful read' ******
	swap =0
	IF (!version.os_family ne 'unix') and (!version.os_family ne 'mac') then swap=1
	IF swap then CASE formt OF
			'ShortValue':		BYTEORDER,data,/sswap
			'UnsignedShort':	BYTEORDER,data,/sswap
			'IntegerValue':		BYTEORDER,data,/lswap
			'UnsignedInteger':	BYTEORDER,data,/lswap
			ELSE:
	    		ENDCASE
			
	if formt eq 'UnsignedShort' then positive,data

	;** the 'for' loop stores the names and the values of the headerlines ******
	;** in their variables ******
	k=0
	on_ioerror,misflt
	head=strtrim(head,2)
	FOR i=0,(j<maxilen)-1 DO BEGIN
		w=STRPOS(head(i),'=')
		if w gt 0 then begin
		   s=STRPOS(head(i),';')
		   ok=0
		   if s gt w then begin
			text(k)=STRMID(head(i),0,w-1)+' '+STRMID(head(i),s+1,100)
			num=STRMID(head(i),w+1,s-w-1)
			num=float(num) & ok=1 & misflt:
		   endif
		   if (ok) then par(k)=num else text(k)=STRMID(head(i),w+1,100)
		   k=k+1
		endif
	ENDFOR
	text=text(0:k-1)
	par =par (0:k-1)
ENDFOR ;** loop=c_imgNo,imgNo ******

erread:c_imgNo = imgNo+1

;** when flag=1, imagenumber is displayed as title ******
IF flag EQ 1 THEN title=titleVal

;** passing of header to lamp ******
datp={PAR_TXT:text,P:par,W_tit:title,OTHER_tit:file}

nofile:if c_unit le 0 then c_file='?'

RETURN,data
END




