;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	READ_INTERFILE
;
; PURPOSE:
;	Simplistic Interfile (v3.3) reader. Can only read a series
;	of images containing byte,int,long,float or double data where
;	all images have the same height and with.  Result is returned
;	in a 3-D array.
;
; CATEGORY:
;	Input/Output.
;
; CALLING SEQUENCE:
;	READ_INTERFILE, File, Data
;
; INPUTS:
;	File:	Scalar string containing the name of the Interfile
;		to read.  Note: if the Interfile has a header file and
;		a data file, this should be the name of the header
;		file (also called the administrative file).
;	
; OUTPUTS:
;	Data: A 3-D array of data as read from the file.  Assumed to be
;	a series of 2-D images.
;
; RESTRICTIONS:
;	This is a simplistic reader.  It does not get additional
;	keyword information above and beyond what is needed to read
;	in the image data.  If any problems occur reading the file,
;	READ_INTERFILE prints a message and stops.
;
;	If the data is stored in on a bigendian machine and read on
;	a littleendian machine (or vice versa) the order of bytes in
;	each pixel element may be reversed, requiring a call to
;	BYTEORDER
;
; PROCEDURE:
;	Generates keyword table and initializes it on the fly.
;	Read in administrative data.
;	Read in binary data.
;	Clean up keyword processing information.
;
; EXAMPLE:
;	READ_INTERFILE, '0_11.hdr', X
;
; MODIFICATION HISTORY:
; 	Written by:	J. Goldstein, Oct 1993
;
;	12/22/93 JWG,TH		Bug fixes. Added byte swapping for short data
;-

;
;  GetPath
;
FUNCTION GetPath, File, PATH=DoPath, FILE=DoFile

    Idx	= WHERE(!Version.OS EQ [ "vms", "Win32", "MacOS" ])
    Idx	= Idx(0) + 1
		;   Unix, VMS, WIN, MAC
    First	= ([ '/', ']', '\', ':'])(Idx)
    Second	= ([ '',  ':', ':', ''])(Idx)

    FileStart	= RSTRPOS(File, First)+1
    IF FileStart EQ 0 AND Second NE '' THEN $
	FileStart = RSTRPOS(File,Second)+1

    IF KEYWORD_SET(DoFile) THEN $
	RETURN, STRMID(File, FileStart, 1000)
    RETURN, STRMID(File,0,FileStart)
END

;
;  Inter_MakeInfo
;	Create keyword table entries.  Create hetergenous data by
;	EXECUTE'ing initialization strings and storing the results
;	in unrealized base widget UVALUE's.  This may seem confusing.
;	It probably is.
;
PRO Inter_MakeInfo, Info, Filename

    ;	NB. Add hash entry for faster lookup if list gets large.
    ;	E.g. FOR I=0,Info_Size-1 DO Info(i).Hash = TOTAL(BYTE(Info(i).Name))

    Entry = { IFSYM,	$
	Keyword:	"",	$
	Value:		0L,	$	; Use UVALUE to hold current value
	Default:	"",	$	; EXECUTE this to create initial value
	Handler:	"",	$	; Generic keyword processor
	Choices:	0L,	$	; If limited set of choices, these
	ChoiceInit:	"",	$	; are them
	IsArray:	0,	$	; keyword requires indexing?
	Proc:		""	$	; Special per keyword processing
    }

    ;	These are the currently supported keywords.

    DefaultDataFile	= GetPath(Filename, /FILE)

    Info	= [ $
	{ IFSYM, "data starting block",  0L, '0L', "INT", 0L, "", 0, "" }, $
	{ IFSYM, "data offset in bytes", 0L, '0L', "INT", 0L, "", 0, "" }, $
	{ IFSYM, "data compression", 0L, $
		"'none'", "STR", 0L, "'none'", 0, "" }, $
	{ IFSYM, "data encode", 0L, $
		"'none'", "STR", 0L, "'none'", 0, "" }, $
	{ IFSYM, "imagedata byte order", 0L, $
		"'bigendian'", "STR", 0L, $
		"[ 'bigendian','littleendian' ]", 0, "" }, $
	{ IFSYM, "matrix size", 0L, $
		'[0L,0L]', "INT", 0L, "", 1, "Inter_Fixed" }, $
	{ IFSYM, "name of data file", 0L, $
		"'"+DefaultDataFile+"'", "STR", 0L, "", 0, "" }, $
	{ IFSYM, "number format", 0L, $
		"'unsigned integer'", "STR", 0L, $
		"[ 'signed integer','unsigned integer'," + $
		  "'long float', 'short float','bit' ]", 0, "" }, $
	{ IFSYM, "number of bytes per pixel", 0L, '0L',"INT",0L, "", 0, "" }, $
	{ IFSYM, "total number of images", 0L, '0L', "INT", 0L, "", 0, "" } $
    ]

    ;	Run through keywords and create initial values
    ;	and choice values if keyword has a limited set of choices

    FOR I=0,N_ELEMENTS(Info)-1 DO BEGIN

	;	Create value

	Info(I).Value	= WIDGET_BASE()
	Value		= 0
	Result		= EXECUTE("Value = " + Info(I).Default)
	IF Result NE 1 THEN MESSAGE, "Cannot initialize Info structure"
	WIDGET_CONTROL, Info(I).Value, SET_UVALUE=Value

	;	Create choices if they exist

	IF Info(I).ChoiceInit NE "" THEN BEGIN
	    Info(I).Choices	= WIDGET_BASE()
	    Value		= 0
	    Result		= EXECUTE("Value = " + Info(I).ChoiceInit)
	    IF Result NE 1 THEN MESSAGE, "Cannot initialize Info structure"
	    WIDGET_CONTROL, Info(I).Choices, SET_UVALUE=Value
	ENDIF

    ENDFOR
END

;
;  Inter_INT
;	General integer keyword processing routine
;
PRO Inter_INT, KwdInfo, Value, Arr

    Value	= LONG(Value)

    ;	limited # of chioces?  See if user has chosen a valid chioce

    IF KwdInfo.Choices NE 0 THEN BEGIN
	WIDGET_CONTROL, KwdInfo.Choices, GET_UVALUE=Choices
	Dummy	= WHERE(Value EQ Choices, Count)
	IF Count NE 1 THEN $
	    MESSAGE, 'Illegal choice of values for ' + KwdInfo.Keyword
    ENDIF

    IF KwdInfo.Proc THEN BEGIN	;	Special keyword routine?

	CALL_PROCEDURE, KwdInfo.Proc, KwdInfo, Value, Arr

    ENDIF ELSE BEGIN		;	General processing
	IF KwdInfo.IsArray THEN BEGIN
	    WIDGET_CONTROL, KwdInfo.Value, GET_UVALUE=Vals
	    Vals(Arr-1)	= Value
	ENDIF ELSE BEGIN
	    Vals	= Value
	ENDELSE

        WIDGET_CONTROL, KwdInfo.Value, SET_UVALUE=Vals
    ENDELSE
END


;
;  Inter_STR
;	General string keyword processing routine
;
PRO Inter_STR, KwdInfo, Value, Arr

    ;	Hack. I've seen people use Keyword:=
    ;	Perhaps I should just add '' to the list of valid values.
    IF Value EQ '' THEN VALUE = 'none'

    IF KwdInfo.Choices NE 0 THEN BEGIN
	WIDGET_CONTROL, KwdInfo.Choices, GET_UVALUE=Choices
	Value	= STRLOWCASE(Value)
	Dummy	= WHERE(Value EQ Choices, Count)
	IF Count NE 1 THEN $
	    MESSAGE, 'Illegal choice of values for ' + KwdInfo.Keyword
    ENDIF

    IF KwdInfo.Proc THEN BEGIN

	CALL_PROCEDURE, KwdInfo.Proc, KwdInfo, Value, Arr

    ENDIF ELSE BEGIN
	IF KwdInfo.IsArray THEN BEGIN
	    WIDGET_CONTROL, KwdInfo.Value, GET_UVALUE=Vals
	    Vals(Arr-1)	= Value
	ENDIF ELSE BEGIN
	    Vals		= Value
	ENDELSE

        WIDGET_CONTROL, KwdInfo.Value, SET_UVALUE=Vals
    ENDELSE
END


;
;  Inter_Fixed
;	Routine to tell user that this is a simple reader.
;	If the size of a keyword element has been changed
;	and it wasn't 0 before, tell user we give up.
;
;	This is for :matrix size: because we don't handle images
;	of different sizes.
;
PRO Inter_Fixed, KwdInfo, Value, Arr

    WIDGET_CONTROL, KwdInfo.Value, GET_UVALUE=Vals
    IF KwdInfo.IsArray THEN Val = Vals(Arr-1) $
    ELSE Val = Vals

    IF Val EQ 0 THEN BEGIN

	IF KwdInfo.IsArray THEN Vals(Arr-1) = Value $
	ELSE Vals = Value
	WIDGET_CONTROL, KwdInfo.Value, SET_UVALUE=Vals

    ENDIF ELSE IF Value NE Val THEN BEGIN

	MESSAGE, "Support of Interfiles where " + KwdInfo.Keyword + $
		" changes is not currently supported"

    ENDIF
END


;
;  Inter_ReadHdr
;	Wade through administrative data looking for the keywords
;	we understand.  Quietly ignore any keywords not in the Info
;	list. This should provide enough information to either
;	read the data or realize we can't read the data.
;
;	N.B. STRMID(Str,Start, 255) will return the remainder of a
;	string as long as that string is less than 255 characters long --
;	which the Interfile 3.3 spec guarantees
;
PRO Inter_ReadHdr, Unit, Info

    ;	Parse lines until
    Line	= ""
    WHILE NOT EOF(Unit) DO BEGIN
	READF, Unit, Line			; Read in line of text

	;	Remove leading/trailing whitespace (blech)
	BLine		= BYTE(Line)
	Idx		= WHERE(Bline EQ 13b OR Bline EQ 10b, Count)
	IF Count GT 0 THEN Bline(Idx) = 32b
	Line		= STRTRIM(BLine,2)

	IF Line EQ '' THEN GOTO, Continue	; ignore blank lines

	FirstChar	= STRMID(Line,0,1)	; ';' is comment character
	IF FirstChar EQ ";" THEN GOTO, Continue

	;	Find full Keyword
	KeyStart	= FirstChar EQ "!"
	KeyEnd		= STRPOS(Line, ":=")
	Kwd		= STRMID(Line, KeyStart,KeyEnd-KeyStart)

	;	Look for array index
	ArrStart	= STRPOS(Kwd, "[")
	IF ArrStart NE -1 THEN BEGIN
		Arr	= FIX( STRMID(Kwd,ArrStart+1, 255))
		Kwd	= STRMID(Kwd, 0, ArrStart)
	ENDIF ELSE BEGIN
		Arr	= 0
	ENDELSE

	Kwd		= STRLOWCASE(STRTRIM(Kwd,2))

	;	Look for value
	Value		= STRMID(Line,KeyEnd+2,255)
	ValEnd		= STRPOS(Value, ";")
	IF ValEnd NE -1 THEN Value = STRMID(Value,0,ValEnd-1)
	Value		= STRTRIM(Value,2)

	;	We now have keyword, array index and value

	;	Special case for the 'End of Interfile' keyword
	;	It does not set a value like the other keywords.
	IF Kwd EQ "end of interfile" THEN RETURN

	Idx	= WHERE(Kwd EQ Info.Keyword, Count)
	IF Count EQ 0 THEN GOTO, Continue
	KwdInfo	= Info(Idx)

	;	Either we have an array with no subscripting which requires
	;	it or we have an array with a subscript that can't have one

	IF (Arr NE 0) XOR (KwdInfo.IsArray NE 0) THEN BEGIN
	    IF Arr NE 0 THEN $
		MESSAGE, 'Keyword :'+KwdInfo.Keyword +': cannot have subscript'
	    MESSAGE, 'Keyword :' + KwdInfo.Keyword + ': must have subscript'
	ENDIF

	CALL_PROCEDURE, "Inter_" + KwdInfo.Handler, $
		KwdInfo, Value, Arr
    Continue:
    ENDWHILE
END

;
;  GetIFSYM
;	Thin UI to cover keyword finding mechanism.
;	If hash is implemented it goes here
;
FUNCTION GetIFSYM, Name, Info, INDEX=Idx

	; Hash	= FIX(TOTAL(BYTE(Name)))
	; Idxs	= WHERE(Info.Hash EQ Hash, Count)
	; IF Count eq 0 then <Error>
	; Idx	= WHERE(Name EQ Info(Idxs).Keyword, Count)

	Idx	= WHERE(Name EQ Info.Keyword, Count)
	IF Count NE 1 THEN $
		MESSAGE, "Unknown/unsupported keyword '" + Name + "'"
	WIDGET_CONTROL, Info(Idx).Value, GET_UVALUE=Value
	RETURN, Value
END


;
;  Inter_ReadData
;	At this point we have all of the information to read the
;	data: File, Offset, Amount and Type.
;
PRO Inter_ReadData, Info, Data, Path

    ;	Byte/Block offset
    Offset	= GetIFSYM("data offset in bytes", Info)
    IF Offset EQ 0L THEN BEGIN
	Offset	= GetIFSYM("data starting block", Info)
	Offset	= Offset * 2048
    ENDIF

    FileName	= Path + GetIFSYM("name of data file", Info)
    Sz		= LONARR(6)
    Sz(0)	= 3	; 3 dimensions
    Sz(1:2)	= GetIFSYM("matrix size", Info)
    Sz(3)	= GetIFSYM("total number of images", Info)

    ;	Now the tricky one. Data Type:
    ;	Use elements size and number format.
    ;	I hope we blow up if things are wierd (3 bytes/pixel
    ;	or other unsupported conditions)
	
    ElemSize	= GetIFSYM("number of bytes per pixel", Info)
    InterType	= GetIFSYM("number format", Info)

    CASE InterType OF

    'bit':		MESSAGE, "Unsupported Data Type."

    'unsigned integer':	GOTO, IntData
    'signed integer':	BEGIN
    IntData:
	Type	= [ 1, 2, 0, 3 ]	; byte/int/error/long
	Type	= Type(ElemSize-1)
	END

    'short float':	Type	= 4
    'long float':	Type	= 5

    ENDCASE

    Sz(4)	= Type

    OPENR, Unit, FileName, /GET_LUN
    Data = MAKE_ARRAY(SIZE=Sz, /NOZERO)
    POINT_LUN, Unit, Offset
    READU, Unit, Data
    FREE_LUN, Unit

    ;	There are other combinations that require
    ;	byteswapping but this is what was found
    ;	so far.

    Endian	= GetIFSYM("imagedata byte order", Info)

    ;	Short int data. If we are on some machine where the
    ;	endianness is the reverse of that in the file...

    IF Type EQ 2 THEN BEGIN

	;	Determine the endianness of the machine

	LocalEndian	= (BYTE(1, 0, 1))(0)

	;	If the endianness of the machine doesn't
	;	match the endianness of the file then swap

	IF (Endian EQ 'littleendian' AND LocalEndian EQ 0) OR $
	   (Endian EQ 'bigendian' AND LocalEndian EQ 1) THEN BEGIN

	    BYTEORDER, Data, /SSWAP
	ENDIF
    ENDIF
END


PRO Read_Interfile, Filename, Data

    OPENR, Unit, Filename, /GET_LUN
    Inter_MakeInfo, Info, Filename
    Inter_ReadHdr, Unit, Info
    FREE_LUN, Unit
    Inter_ReadData, Info, Data, GetPath(Filename, /PATH)

    ; Release Information structure information
    FOR I=0,N_ELEMENTS(Info)-1 DO BEGIN
	IF Info(I).Value NE 0L THEN WIDGET_CONTROL, Info(I).Value, /DESTROY
	IF Info(I).Choices NE 0L THEN WIDGET_CONTROL, Info(I).Choices, /DESTROY
    ENDFOR
END
