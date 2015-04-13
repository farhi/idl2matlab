; write_sylk.pro


; Writes a single line of sylk cell data to file.

PRO WriteSylkCell, lunOutfile, Data, iRow, iCol

    ON_ERROR, 2

    ; If the data is anything but a string, simply write it to file.
    IF ((SIZE(Data))(1) NE 7) THEN BEGIN
        PRINTF, lunOutfile, "C;X", STRCOMPRESS(STRING(iCol), /REMOVE_ALL), $
            ";Y", STRCOMPRESS(STRING(iRow), /REMOVE_ALL), ";K", $
            STRCOMPRESS(STRING(Data), /REMOVE_ALL)

    ; Otherwise, surround the string in double quotes.
    ENDIF ELSE BEGIN
        PRINTF, lunOutfile, "C;X", STRCOMPRESS(STRING(iCol), /REMOVE_ALL), $
            ";Y", STRCOMPRESS(STRING(iRow), /REMOVE_ALL), ";K", '"', $
            STRCOMPRESS(STRING(Data), /REMOVE_ALL), '"'
    ENDELSE

    RETURN
END


FUNCTION WRITE_SYLK, Outfile, SourceData, STARTROW = iStartRow, STARTCOL = iStartCol

;
;+
; NAME:
;   WRITE_SYLK
;
; PURPOSE:
;   Writes the contents of an IDL variable to a sylk (Symbolic Link) format 
;   spreadsheet data file. 
;
; CATEGORY:
;   Input/Output.
;
; CALLING SEQUENCE:
;   fStatus = WRITE_SYLK(OutFile, SourceData [, STARTROW, STARTCOL])
;
; INPUT:
;   OutFile: Scalar string with the name of the sylk file to write.
;   SourceData: A scalar, a vector, or a 2D array to be written to file.
;
; OUTPUT:
;   fStatus: Boolean flag.  Returns TRUE if function was successful. 
;
; OPTIONAL INPUT PARAMETERS:
;   STARTROW: The starting (0-based) row of spreadsheet cells to which the 
;       routine will write the data.  If not specified, this value defaults 
;       to row 0. 
;   STARTCOL: The starting (0-based) column of spreadsheet cells to which the
;       routine will write the data.  If not specified, this value defaults 
;       to column 0.
;
; SIDE EFFECTS:
;   None.
;
; RESTRICTIONS:
;   This routine *only* writes numerical and string sylk data.  It connot
;   handle spreadsheet and cell formatting information such as cell width, text
;   justification, font type, date, time, and monetary notations, etc.  A 
;   particular sylk data file cannot be appended with data blocks through 
;   subsequent calls.
;
; EXAMPLES:
;   Assume you wished to write the contents of a 2x2 array of floats, 
;   arrfltData, to a sylk data file called "bar.slk" such that, when read into 
;   a spreadsheet, the matrix would appear with it's upper left data at the 
;   cell in the 10th row and the 20th column.  The following call would 
;   accomplish this task:
;   
;       fStatus = WRITE_SYLK("bar.slk", arrflData, STARTROW = 9, STARTCOL = 19)
;
;
; MODIFICATION HISTORY:
;   Written October 1994, AJH
;-
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
    
    ON_ERROR, 2
    ON_IOERROR, CleanUp

    _BAD_ =    0B
    _SCALAR_ = 1B
    _VECTOR_ = 2B
    _MATRIX_ = 3B
    _TABLE_ =  4B

    typeData = _BAD_
    lunOutfile = 0
    fStatus = 0

    ; First check to see if the correct number of positional parameters have
    ; been passed
    IF (N_PARAMS() NE 2) THEN BEGIN
        MESSAGE, "Calling sequence - WRITE_SYLK, Outfile, SourceData, " + $
            "STARTROW, STARTCOL", /CONTINUE
        GOTO, CleanUp
    ENDIF

    ; Check the validity of the file parameter
    IF (N_ELEMENTS(Outfile) EQ 0) THEN BEGIN
        MESSAGE, "Error - A STRING filename must be passed in the Outfile " + $
            "parameter.", /CONTINUE
        GOTO, CleanUp
    ENDIF

    ; Check the validity and type of the SourceData parameter
    IF (N_ELEMENTS(SourceData) NE 0) THEN BEGIN
        sizeSourceData = SIZE(SourceData)
        CASE sizeSourceData(0) OF
            0:      typeData = _SCALAR_
            1:      BEGIN
                        typeData = _VECTOR_
                        IF (sizeSourceData(2) EQ 8) THEN BEGIN
                            typeData = _TABLE_
                        ENDIF
                    END
            2:      BEGIN
                        typeData = _MATRIX_
                        IF (sizeSourceData(3) EQ 8) THEN BEGIN
                            typeData = _BAD_
                        ENDIF
                    END
            ELSE:   typeData = _BAD_
        ENDCASE
    ENDIF

    IF (typeData EQ _BAD_) THEN BEGIN
        MESSAGE, "Error - Either a scalar, a vector, or a 2D ARRAY of " + $
            "scalars must be passed in the SourceData parameter.", /CONTINUE
        GOTO, CleanUp
    ENDIF

    ; Setup keyword default values.
    IF (N_ELEMENTS(iStartRow) EQ 0) THEN BEGIN
        iStartRow = 0
    ENDIF ELSE BEGIN
        iStartRow = (iStartRow > 0)
    ENDELSE

    IF (N_ELEMENTS(iStartCol) EQ 0) THEN BEGIN
        iStartCol = 0
    ENDIF ELSE BEGIN
        iStartCol = (iStartCol > 0)
    ENDELSE

    IF (N_ELEMENTS(fUpdate) EQ 0) THEN fUpdate = 0
    
    ; If Outfile is a filename, open it for reading and get its lun
    IF ((SIZE(Outfile))(1) EQ 7) THEN BEGIN
        OPENW, lunOutfile, Outfile, /GET_LUN, ERROR = fOpenWrite
        IF (fOpenWrite NE 0) THEN BEGIN
            MESSAGE, "Error - File " + STRCOMPRESS(Outfile, /REMOVE_ALL) + $
                " cannot be opened.", /CONTINUE
            GOTO, CleanUp           
        ENDIF
        fstatResult = FSTAT(lunOutfile)
        IF (fstatResult.WRITE EQ 0) THEN BEGIN
            MESSAGE, "Error - File " + STRCOMPRESS(Outfile, /REMOVE_ALL) + $
                " cannot be written to.", /CONTINUE
            GOTO, CleanUp
        ENDIF
    ENDIF

    ; Write the SYLK file creation app id to file.
    PRINTF, lunOutfile, "ID;PIDL"
    
    CASE typeData OF
        _SCALAR_:   WriteSylkCell, lunOutfile, SourceData, iStartRow + 1, $
                        iStartCol + 1   
        _VECTOR_:   BEGIN
                        FOR i = 0, sizeSourceData(1) - 1 DO BEGIN
                            WriteSylkCell, lunOutfile, SourceData(i), $
                                i + iStartRow + 1, iStartCol + 1
                        ENDFOR
                    END
        _MATRIX_:   BEGIN
                        FOR i = 0, sizeSourceData(1) - 1 DO BEGIN
                            FOR j = 0, sizeSourceData(2) - 1 DO BEGIN
                                WriteSylkCell, lunOutfile, SourceData(i, j), $
                                    i + iStartRow + 1, j + iStartCol + 1
                            ENDFOR
                        ENDFOR
                    END
        _TABLE_:    BEGIN
                        nTags = N_TAGS(SourceData)
                        FOR i = 0, sizeSourceData(1) - 1 DO BEGIN
                            FOR j = 0, nTags - 1 DO BEGIN
                                WriteSylkCell, lunOutfile, $
                                    SourceData(i).(j), i + iStartRow + 1, $
                                    j + iStartCol + 1
                            ENDFOR
                        ENDFOR
                    END
    ENDCASE
    
    ; Write the SYLK end-of-data descriptor.
    PRINTF, lunOutfile, "E"
    fStatus = 1
    
    GOTO, CleanUp

    CleanUp: BEGIN
        IF (lunOutfile NE 0) THEN BEGIN
            FREE_LUN, lunOutfile
        ENDIF
    END

    RETURN, fStatus
END
