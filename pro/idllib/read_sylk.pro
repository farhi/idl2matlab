; read_sylk.pro


; Returns TRUE if TestVal is in the range of LoVal through HiVal

FUNCTION ContainsVal, LoVal, TestVal, HiVal

    RETURN, ((LoVal LE TestVal) AND (TestVal LE HiVal))
END


; Scans the passed sylk data line and returns the new cell row.

FUNCTION GetSylkCellRow, szCellLine, iCurRow

    ON_ERROR, 2

    iRow = 0
    posRowData = STRPOS(szCellLine, ";Y")

    IF (posRowData NE -1) THEN BEGIN
    READS, STRMID(szCellLine, posRowData + 2, $
        STRLEN(szCellLine) - posRowData), iRow, FORMAT = "(I)"
    ENDIF
    RETURN, iRow - 1
END


; Scans the current sylk data line and returns the current cell column.

FUNCTION GetSylkCellCol, szCellLine

    ON_ERROR, 2

    iCol = 0
    posColData = STRPOS(szCellLine, ";X")

    IF (posColData NE -1) THEN BEGIN
        READS, STRMID(szCellLine, posColData + 2, $
            STRLEN(szCellLine) - posColData), iCol, FORMAT = "(I)"
    ENDIF
    RETURN, iCol - 1
END


; Reads in the entire sylk file and determines the complete range of cell data 
; therein.

FUNCTION GetSylkCellRange, lunFile, strCellRange

    ON_ERROR, 2

    szFileLine = ""
    iCurRow = 0
    iCurCol = 0
    iFirstRow = 32767
    iFirstCol = 32767
    iLastRow = 0
    iLastCol = 0

    WHILE (NOT EOF(lunFile)) DO BEGIN
        READF, lunFile, szFileLine

        iCurRow = GetSylkCellRow(szFileLine)
        IF (iCurRow NE -1) THEN BEGIN
            iFirstRow = (iCurRow < iFirstRow)
            iLastRow = (iCurRow > iLastRow)
        ENDIF

        iCurCol = GetSylkCellCol(szFileLine)
        IF (iCurCol NE -1) THEN BEGIN
            iFirstCol = (iCurCol < iFirstCol)
            iLastCol = (iCurCol > iLastCol)
        ENDIF
    ENDWHILE

    POINT_LUN, lunFile, 0

    strCellRange.(0) = iFirstRow
    strCellRange.(1) = iFirstCol
    strCellRange.(2) = iLastRow
    strCellRange.(3) = iLastCol

    RETURN, ((iLastRow - iFirstRow GT 0) OR (iLastCol - iFirstCol GT 0))
END
 

; Reads in sylk cell data line, determines the type of data in the cell and
; returns that data.

FUNCTION GetSylkCellContents, szFileLine, fUseLongs, fUseDoubles

    ON_ERROR, 2

    mdefCellContents = 0B
    szCellContents = ""
    posCellContents = STRPOS(szFileLine, ";K")
    
    IF (posCellContents NE -1) THEN BEGIN
        szCellContents = STRMID(szFileLine, posCellContents + 2, $ 
            STRLEN(szFileLine) - posCellContents)

        szCellContents = szCellContents + ";"

        ; Check to see if it's a string or not.  If it is, it will have a '"'
        ; as the first character.
        IF (STRPOS(szCellContents, 34B) EQ 0) THEN BEGIN

            ; Find the end of the cell's text contents, which will be the last
            ; '"' in the string before the next ";", and extract that text from 
            ; the string.
            mdefCellContents = STRMID(szCellContents, 1, $
            STRPOS(szCellContents, ";", 1) - 2)
                
        ; Check to see if it's a floating point value.  If it is, it will have a
        ; "." in the string before the next ";" delimeter.
        ENDIF ELSE BEGIN
            posPeriod = STRPOS(szCellContents, ".")
            IF ((posPeriod GT -1) AND $
                (posPeriod LT STRPOS(szCellContents, ";"))) THEN BEGIN

                ; If the user wishes to use double precision, return a DOUBLE.
                IF (fUseDoubles) THEN BEGIN
                    mdefCellContents = 0.0D
                    READS, szCellContents, mdefCellContents, FORMAT = "(D)"
                
                ; Otherwise, return a FLOAT.
                ENDIF ELSE BEGIN
                    mdefCellContents = 0.0
                    READS, szCellContents, mdefCellContents, FORMAT = "(F)"
                ENDELSE
    
            ; Otherwise, the sylk cell data must be an integer type.
            ENDIF ELSE BEGIN
        
                ; If the user wishes to use long ints, return a LONG.
                IF (fUseLongs) THEN BEGIN
                    mdefCellContents = 0L

                ; Otherwise, return an INT.
                ENDIF ELSE BEGIN
                    mdefCellContents = 0
                ENDELSE

                READS, szCellContents, mdefCellContents, FORMAT = "(I)"

            ENDELSE
        ENDELSE
    ENDIF

    RETURN, mdefCellContents
END


FUNCTION READ_SYLK, Infile, STARTROW = iStartRow, STARTCOL = iStartCol, $
    NROWS = nRows, NCOLS = nCols, ARRAY = fArray, COLMAJOR = fColMajor, $
    USEDOUBLES = fUseDoubles, USELONGS = fUseLongs 
;
;+
; NAME:
;   READ_SYLK
;
; PURPOSE:
;   Reads the contents of a sylk (Symbolic Link) format spreadsheet data file 
;   and returns a cell data range (if present) in the form of an IDL variable.
;
; CATEGORY:
;   Input/Output.
;
; CALLING SEQUENCE:
;   ReturnData = READ_SYLK(InFile [, STARTROW, STARTCOL, NROWS, NCOLS, ARRAY, 
;       COLMAJOR, USEDOUBLES, USELONGS])
;
; INPUT:
;   InFile: Scalar string with the name of the sylk file to read.
;
; OUTPUT:
;   ReturnData: The table (vector of structs) or matrix (2D array) that will
;       contain the spreadsheet cell data.  The size and type of this return
;       variable is set using the optional input parameters (keywords) below.
;
; OPTIONAL INPUT PARAMETERS:
;   STARTROW: The starting (0-based) row of spreadsheet cells to read.  If not
;       specified, this value defaults to the first row of cells found in the 
;       file.
;   STARTCOL: The starting (0-based) column of spreadsheet cells to read.  If 
;       not specified, this value defaults to the first column of cells found 
;       in the file.
;   NROWS: The number of spreadsheet rows to read in.  If not specified, this
;       defaults to all of the cell rows found in the file.
;   NCOLS: The number of spreadsheet columns to read in.  If not specified,
;       this value defaults to all of the cell columns found in the file.
;   ARRAY: Boolean flag.  If TRUE, the data type returned will be an IDL array.
;       Note that the data in the cell range must be of the same type to 
;       successfully return an array.  If this flag is not set, the routine
;       will return a table (vector of structs) instead.  The tags of this
;       struct will be labelled "Col0", "Col1", ..., "ColN" for a row major
;       table and "Row0", "Row1", ..., "RowN" for a column major table.
;   COLMAJOR: Boolean flag.  If TRUE, the range of spreadsheet cell data is
;       transposed and then read into an IDL variable.  This flag should be set 
;       when importing spreadsheet data which has column major organization 
;       (ie., listings by column rather than row).  The default is row major 
;       format.  
;   USEDOUBLES: Boolean flag.  If TRUE, any floating point cell data will be
;       read in and returned as a double precision rather than the default 
;       float type.
;   USELONGS: Boolean flag.  If TRUE, any integer cell data will be read in and
;       returned as a long rather than the default int type.
;
; SIDE EFFECTS:
;   None.
;
; RESTRICTIONS:
;   This routine *only* reads in numerical and string sylk data.  It igonores 
;   all spreadsheet and cell formatting information such as cell width, text 
;   justification, font type, date, time, and monetary notations, etc.  In
;   addition, only attempts to read spreadsheet tables, like-typed cell rows,
;   columns, or subsets thereof will succeed.
;
;
; EXAMPLES:
;   Consider the following row major spreadsheet table with the upper left cell
;   (value = "Index") at location (0, 0) that has been output to the sylk file
;   "foo.slk":
;   
;   Index   Name   Gender  Platform
;     1     Beth     F     Unix
;     2     Kirk     M     VMS
;     3     Mark     M     Windows
;     4     Dave     M     Macintosh
;
;   Note that the data format of the title row (STRING, STRING, STRING, STRING)
;   is inconsistant with the following four rows (INT, STRING, STRING, STRING)
;   in the table.   It is impossible to read all of the table into a single IDL
;   variable, but you could make two calls to READ_SYLK to import all of the
;   data:
;
;       strTitle = READ_SYLK("foo.slk", NROWS = 1)
;       arrstrTable = READ_SYLK("foo.slk", STARTROW = 1)
;
;   The return data are as follows:
;
;       IDL> HELP, strTitle
;       STRTITLE        STRUCT    = -> <Anonymous> Array(1)
;
;       IDL> PRINT, strTitle        
;       { Index Name Gender Platform}
;
;       IDL> HELP, arrstrTable
;       ARRSTRTABLE     STRUCT    = -> <Anonymous> Array(4)
;
;       IDL> PRINT, arrstrTable
;       {       1 Beth F Unix}{       2 Kirk M VMS}{       3 Mark M 
;       Windows}{       4 Dave M Macintosh}
;
;
;   Further, consider the following call from the same sylk file:
;
;       arrszNames = READ_SYLK("foo.slk", /ARRAY, STARTROW = 1, STARTCOL = 1, $
;           NCOLS = 1)
;
;   The return data is now:
;
;       IDL> HELP, arrszNames
;       ARRSZTABLE      STRING    = Array(4)
;
;       IDL> PRINT, arrszNames
;       Beth Kirk Mark Dave 
;
;
;   If the COLMAJOR keyword flag is set the return value differs in type:
;
;       arrszNames = READ_SYLK("foo.slk", /ARRAY, /COLMAJOR, STARTROW = 1, $
;           STARTCOL = 1, NCOLS = 1)
;
;   The return data is now:
;
;       IDL> HELP, arrszNames
;       ARRSZTABLE      STRING    = Array(1, 4)
;
;       IDL> PRINT, arrszNames
;       Beth 
;       Kirk 
;       Mark 
;       Dave 
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
    
    iCurRow = 0
    iCurCol = 0
    strCellRange = {iStartRow:0, iStartCol:0, iEndRow:0, iEndCol:0}
    szFileLine = ""
    htopSylkData = 0L
    lunInfile = 0
    ReturnData = 0

    ; First check to see if the correct number of positional parameters have 
    ; been passed.
    IF (N_PARAMS() NE 1) THEN BEGIN
        MESSAGE, "Calling sequence - ReturnData = READ_SYLK(Infile [, " + $
            "STARTROW, STARTCOL, NROWS, NCOLS, ARRAY, COLMAJOR, " + $
            "USEDOUBLES, USELONGS])", /CONTINUE
        GOTO, CleanUp    
    ENDIF

    ; Check for the validity of the file parameter
    IF (N_ElEMENTS(Infile) EQ 0) THEN BEGIN
        MESSAGE, "Error - A STRING filename must be passed in the Infile " + $
            "parameter.", /CONTINUE
        GOTO, CleanUp
    ENDIF
    
    ; If Infile is a filename, open it for reading and get its lun
    IF ((SIZE(Infile))(1) EQ 7) THEN BEGIN
        OPENR, lunInfile, Infile, /GET_LUN, ERROR = fOpenRead
        IF (fOpenRead NE 0) THEN BEGIN
            MESSAGE, "Error - File " + STRCOMPRESS(Infile, /REMOVE_ALL) + $
                " cannot be opened.", /CONTINUE
            GOTO, CleanUp
        ENDIF
        fstatResult = FSTAT(lunInfile)
        IF (fstatResult.READ EQ 0) THEN BEGIN
            MESSAGE, "Error - File with LUN of " + $
                STRCOMPRESS(STRING(lunInfile), /REMOVE_ALL) + $
                " cannot be read from.", /CONTINUE
            GOTO, CleanUp
        ENDIF
    ENDIF

    ; Get the actual cell range from the file.
    fResult = GetSylkCellRange(lunInfile, strCellRange)
    IF (fResult EQ 0) THEN BEGIN
        MESSAGE, "Error - there is no sylk cell data in the input file " + $
            "specified.", /CONTINUE
        GOTO, CleanUp
    ENDIF

    ; Setup values for cell range based on keywords and actual range.
    IF (N_ELEMENTS(iStartRow) EQ 0) THEN BEGIN
        iStartRow = strCellRange.iStartRow
    ENDIF ELSE BEGIN
        iStartRow = (strCellRange.iStartRow > iStartRow)
    ENDELSE

    IF (N_ELEMENTS(iStartCol) EQ 0) THEN BEGIN
        iStartCol = strCellRange.iStartCol
    ENDIF ELSE BEGIN
        iStartCol = (strCellRange.iStartCol > iStartCol)
    ENDELSE

    IF (N_ELEMENTS(nRows) EQ 0) THEN BEGIN
        nRows = (strCellRange.iEndRow - iStartRow) + 1
    ENDIF ELSE BEGIN
        nRows = (1 > nRows)
        nRows = ((strCellRange.iEndRow - iStartRow) + 1 < nRows)
    ENDELSE

    IF (N_ELEMENTS(nCols) EQ 0) THEN BEGIN
        nCols = (strCellRange.iEndCol - iStartCol) + 1
    ENDIF ELSE BEGIN
        nCols = (1 > nCols)
        nCols = ((strCellRange.iEndCol - iStartCol) + 1 < nCols)
    ENDELSE     

    ; Setup keyword boolean flags.
    IF (N_ELEMENTS(fColMajor) EQ 0) THEN fColMajor = 0
    IF (N_ELEMENTS(fArray) EQ 0) THEN fArray = 0
    IF (N_ELEMENTS(fUseDoubles) EQ 0) THEN fUseDoubles = 0
    IF (N_ELEMENTS(fUseLongs) EQ 0) THEN fUseLongs = 0

    ; Create a 2D array that will hold the IDs for the cell data handles so
    ; they can be easily referenced.
    arrHandles = MAKE_ARRAY(nRows, nCols, /LONG)
    
    ; Create the parent handle that will own the sylk cell data handles.
    htopSylkData = HANDLE_CREATE()

    ; While not yet at the end of the input file, read in sylk data.
    WHILE (NOT EOF(lunInfile)) DO BEGIN

        ; Read in a line of test from the sylk file.
        READF, lunInfile, szFileLine

        ; Set the current row and column indeces.

        iRow = GetSylkCellRow(szFileLine)
        IF (iRow NE -1) THEN BEGIN
            iCurRow = iRow
        ENDIF

        iCol = GetSylkCellCol(szFileLine)
        IF  (iCol NE -1) THEN BEGIN
            iCurCol = iCol
        ENDIF
        
        ; Check to see if the file line contains cell data that is within range
        ; of the sylk cell range desired.
        IF (ContainsVal(iStartRow, iCurRow, iStartRow + nRows - 1) AND $
            ContainsVal(iStartCol, iCurCol, iStartCol + nCols - 1) AND $
            STRPOS(szFileLine, "C;") EQ 0) THEN BEGIN

            ; Create a handle containing the data in the cell and put it's ID
            ; into the array at it's row and column location.
            arrHandles(iCurRow - iStartRow, iCurCol - iStartCol) = $
                HANDLE_CREATE(htopSylkData, VALUE = $
                GetSylkCellContents(szFileLine, fUseLongs, fUseDoubles))            

        ENDIF
    ENDWHILE
    
    IF (fColMajor) THEN BEGIN
        szTagPrefix = "Row"
        arrHandles = TRANSPOSE(arrHandles)
        iMax = nCols
        jMax = nRows
    ENDIF ELSE BEGIN
        szTagPrefix = "Col"
        iMax = nRows
        jMax = nCols
    ENDELSE

    ; Find the first valid handle in the array of handles, and determine
    ; the data type to which it refers.
    fIsValid = 0
    i = -1

    REPEAT BEGIN
        i = i + 1
        j = 0
        REPEAT BEGIN
            IF (arrHandles(i, j) NE 0L) THEN BEGIN
                fIsValid = 1
                HANDLE_VALUE, arrHandles(i, j), value       
                typeValue = (SIZE(value))(1)
            ENDIF ELSE BEGIN
                j = j + 1
            ENDELSE
        END UNTIL ((j EQ jMax - 1) OR (fIsValid))
    END UNTIL ((i EQ iMax - 1) OR (fIsValid))

    ; If there was no valid handle in the array of handles, make the return
    ; data type bytes
    IF (NOT fIsValid) THEN BEGIN
        typeValue = 1
    ENDIF

    ; If the user wants a matrix of the same type of data
    IF (fArray) THEN BEGIN

        ; Create an array of that type.
        ReturnData = MAKE_ARRAY(iMax, jMax, TYPE = typeValue)

        ; Fill the array.
        FOR i = 0, iMax - 1 DO BEGIN
            FOR j = 0, jMax - 1 DO BEGIN
                IF (arrHandles(i, j) NE 0L) THEN BEGIN
                    HANDLE_VALUE, arrHandles(i, j), value
                    IF ((SIZE(value))(1) NE typeValue) THEN BEGIN
                        MESSAGE, "Error - Mixed data types found in cell " + $
                            "range specified; unable to create and return " + $
                            "an array.", /CONTINUE
                        GOTO, CleanUp
                    ENDIF
                ENDIF ELSE BEGIN
                    value = 0B
                ENDELSE 
                ReturnData(i, j) = value
            ENDFOR  
        ENDFOR
    
    ; Otherwise, the user wants a table, so create a vector of structs in the
    ; return variable.
    ENDIF ELSE BEGIN
        szTag = STRCOMPRESS(szTagPrefix + STRING(0), /REMOVE_ALL)
        
        ; Create the first tag in the structure.
        HANDLE_VALUE, arrHandles(i, j), value
        strData = CREATE_STRUCT(szTag, value)

        ; Step through each column in the array of handles and create a tag
        ; and appropriate type in the structure.
        FOR j = 1, jMax - 1 DO BEGIN
            szTag = STRCOMPRESS(szTagPrefix + STRING(j), /REMOVE_ALL)
            IF (arrHandles(0, j) NE 0L) THEN BEGIN
                HANDLE_VALUE, arrHandles(0, j), value
            ENDIF ELSE BEGIN
                value = 0B
            ENDELSE
            strData = CREATE_STRUCT(strData, szTag, value)
        ENDFOR
            
        ; Create the vector of structures.
        ReturnData = MAKE_ARRAY(iMax, VALUE = strData)
            
        ; Load the values into the vector of structures.
        FOR i = 0, iMax - 1 DO BEGIN
            FOR j = 0, jMax - 1 DO BEGIN
                IF (arrHandles(i, j) NE 0L) THEN BEGIN
                    HANDLE_VALUE, arrHandles(i, j), value
                ENDIF ELSE BEGIN
                    value = 0B
                ENDELSE

                ; Compare type of data in array of handles and the type of
                ; data in the structure.
                IF ((size(value))(1) NE (size(ReturnData(i).(j)))(1)) THEN BEGIN
                    IF (fColMajor) THEN BEGIN
                        MESSAGE, "Error - data in cell range specified " + $
                            "is not a column major table.  Unable to " + $
                            "create and return a vector of like-typed " + $
                            "structures.", /CONTINUE
                        GOTO, CleanUp                               
                    ENDIF ELSE BEGIN
                        MESSAGE, "Error - data in cell range specified " + $
                            "is not a row major table.  Unable to " + $
                            "create and return a vector of like-typed " + $
                            "structures.", /CONTINUE
                        GOTO, CleanUp
                    ENDELSE
                ENDIF
                ReturnData(i).(j) = value
            ENDFOR
        ENDFOR
    ENDELSE

    CleanUp: BEGIN
        ; Clean up dynamically allocated memory.
        IF (htopSylkData NE 0L) THEN BEGIN
            HANDLE_FREE, htopSylkData
        ENDIF
        ; Free the file.
        IF (lunInfile NE 0) THEN BEGIN
            FREE_LUN, lunInfile
        ENDIF
    END

    RETURN, ReturnData

END
