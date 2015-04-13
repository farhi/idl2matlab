; $Id: read_wave.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

FUNCTION getword, BIN = BIN

COMMON waverdln, unit

ON_IOERROR, done

IF (KEYWORD_SET(BIN)) THEN BEGIN
  wordlen = 0L
  READU, unit, wordlen
  theword = bytarr(wordlen)
  READU, unit, theword
ENDIF ELSE BEGIN
  filestat = FSTAT(unit)
  current = filestat.cur_ptr
  buffsize = 100 < (filestat.size - current)
  linebuffer = bytarr(100)
  READU, unit, linebuffer
  i = 0
  WHILE (linebuffer(i) EQ 32) OR (linebuffer(i) EQ 10) DO i = i + 1
  wordstart = i
  WHILE (linebuffer(i) NE 32) AND (linebuffer(i) NE 10) DO i = i + 1
  theword = linebuffer(wordstart:i - 1)
  POINT_LUN, unit, current + i
ENDELSE

return, string(theword)

RETURN, STRCOMPRESS(STRING(theword(WHERE(theword NE 10))), /REMOVE_ALL)

done: RETURN, ""

END


FUNCTION getdef, BIN = BIN, BLOCK = BLOCK

COMMON waverdln, unit

ON_IOERROR, done

IF (KEYWORD_SET(BIN)) THEN BEGIN
  IF (KEYWORD_SET(BLOCK)) THEN BEGIN
    def = 0L
    READU, unit, def
    IF (def NE 257L) THEN MESSAGE, "Error, " +$
		"definition expected in binary read (" + def + ")"
  ENDIF
  type = 0L
  READU, unit, type
ENDIF ELSE BEGIN
  IF (KEYWORD_SET(BLOCK)) THEN BEGIN
    def = getword(BIN = BIN)
    IF (def EQ "") THEN GOTO, done
    IF (STRMID(def, 0, 1) EQ "#") THEN BEGIN
      ignore = ""
      READF, unit, ignore
      def = getword(BIN = BIN)
    ENDIF
    IF (def NE "define") THEN $
      MESSAGE, "Error, definition expected in text read (" + def + ")"
  ENDIF
  type = getword(BIN = BIN)
  CASE type OF
    "vdata": 		type = 258L
    "reg_topology": 	type = 259L
    "elem_samp": 	type = 260L
    "irr_topology":	type = 262L
    "count":		type = 263L
    "elems":		type = 264L
    "reg_grid": 	type = 265L
    "grid_samp": 	type = 266L
    "origin": 		type = 267L
    "step":		type = 268L
    "irr_grid":		type = 270L
    "node_vdata":	type = 271L
    "mesh": 		type = 273L
    "mesh_topology":	type = 274L
    "mesh_grid":	type = 275L
    "volume": 		type = 276L
    "volume_mesh":	type = 277L
    "volume_vdata":	type = 278L
    ELSE: BEGIN
	    MESSAGE, type + " was the unrecognized type"
	  END
  ENDCASE
ENDELSE

RETURN, type

done: RETURN, -1L

END


FUNCTION getnum, BIN = BIN

COMMON waverdln, unit

IF (KEYWORD_SET(BIN)) THEN BEGIN
  num = 0L
  READU, unit, num
ENDIF ELSE BEGIN
  num = LONG(getword(BIN = BIN))
ENDELSE
  return, num
END

FUNCTION readarray, len

COMMON waverdln, unit

line = ""
READF, unit, line
endofline = fstat(unit)
current = endofline.cur_ptr

valueline = SHIFT(BYTE(STRCOMPRESS(line)), -1)
spaces = WHERE(valueline EQ 32, numspaces)
IF (numspaces NE 0) THEN BEGIN
  values = INTARR(len)
  last = 0
  FOR i = 0, numspaces - 1 DO BEGIN
    values(i) = STRING(valueline(last:spaces(i) - 1))
    last = spaces(i)
  ENDFOR
ENDIF

return, values
END


PRO read_wave, filename, variables, names, dimensions, $
		MESHNAMES = MESHNAMES
;+
; NAME:
;	READ_WAVE
; PURPOSE:
;	READ a .wave or .bwave file created by the Advanced Data Visualizer
;	into an series of IDL variables.
; CALLING SEQUENCE:
;	READ_WAVE, FILE, VARIABLES, NAMES, DIMENSIONS
; INPUTS:
;	FILE = Scalar string giving the name of the Wavefront file to write.
; KEYWORD PARAMETERS:
;	MESHNAMES = The name of the mesh used in the Wavefront file
;		for each variable.
; OUTPUTS:
;	VARIABLES = Upon return, this variable contains a block of the 
;		variables contained in the wavefront file.  Since each
;		variable in a wavefront file can have more than one field
;		(for instance, a vector variable has 3 fields), the fields
;		of each variable make up the major index into the variable 
;		block.  For instance, if a Wavefront file had one scalar 
;		variable and one vector variable, the scalar would be
;		extracted as follows:
;
;			vector_scalar = variables(0,*,*,*)
;
;		and the vector variable would be extracted as follows:
;
;			vector_variable = variables(1:3,*,*,*)
;
;		To find the dimensions of the returned variable, see the
;		description below regarding DIMENSIONS
;
;	NAMES = Upon return, this variable contains the string names of each
;		variable contained in the file.
;	DIMENSIONS = Upon return, this variable is a long array that describes
;		how many fields in the large returned variable block each
;		variable occupies.  In the above example of one scalar variable
;		followed by a vector variable, the dimension variable would 
;		be:
;			DIMENSIONS = [1,3]
;		So the first field of the returned variable block would be
;		the scalar variable and the following 3 fields would comprise
;		the vector variable.
; RESTRICTIONS:
;	This routine only preserved the structure of the variables if they
;	are regularly grided variables.  
; MODIFICATION HISTORY:
;	Written July 16, 1991, by Steve Richards.
;-
; Copyright (c) 1990, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;

COMMON waverdln, unit

IF (KEYWORD_SET(DEBUG)) THEN DEBUG = 1 ELSE DEBUG = 0

volnum = 0
voldesclist = 0
voldesc = {vldesc,	name:"", $
  			volmeshdesc:"", $
  			voldata:""}
meshnum = 0
meshdesclist = 0
meshdesc = {mshdesc,	name:"", $
			topdesc:"", $
			griddesc:""}
topnum = 0
topdesclist = 0
topdesc = {tpdesc,	name:"", $
			elem_samp:intarr(8)}
gridnum = 0
griddesclist = 0
griddesc = {grddesc,	name:"", $
			grid_samp:intarr(8)}

datafieldnum = 0
datafieldlist = 0
datafield = {dtfld,	name:"", $
			size:0}

variables = 0

OPENR, unit, filename, /GET_LUN

adot = STRPOS(filename, ".")

WHILE (adot NE -1) DO BEGIN
  dot = adot
  adot = STRPOS(filename, ".", adot + 1)
END

ext = STRMID(filename, dot + 1, 100)

IF (ext EQ "bwave") THEN BIN = 1 ELSE BIN = 0

ON_IOERROR, done

curval = getdef(BIN = BIN, /BLOCK)
WHILE (curval NE -1) DO BEGIN				;while new blocks to
  CASE (curval) OF					;the definition type

    258L:BEGIN						;definevdata
	   newnumvars = getnum(BIN = BIN)
  	   newvarnames = strarr(newnumvars)
  	   newvardims = intarr(newnumvars) + 1
  	   FOR i = 0, newnumvars - 1 DO BEGIN		;determine variable
  	     newvarnames(i) = getword(BIN = BIN)	;names and dimensional
  	     parenth = STRPOS(newvarnames(i), "(", 0)	;information if it is
  	     IF(parenth NE -1) THEN BEGIN		;present
  	       newvardims(i) = FIX(STRMID(newvarnames(i), $
  				      parenth + 1, $
  				      10))
	       newvarnames(i) = STRMID(newvarnames(i), $
  				   0, $
  				   parenth)
	       datafieldloc = where(datafieldlist.name EQ newvarnames(i))
	       datafieldlist(datafieldloc).size = $
			datafieldlist(datafieldloc).size * newvardims(i)
  	     ENDIF
  	   ENDFOR
	   newnumdatalines = getnum(BIN = BIN)
  	   newvars = FLTARR(TOTAL(newvardims), newnumdatalines)
	   IF (KEYWORD_SET(BIN)) THEN $
	     READU, unit, newvars $
	   ELSE $
	     READF, unit, newvars
	   varsize = SIZE(variables)
	   IF (varsize(0) EQ 0) THEN BEGIN
	     variables = newvars
	     numvars = newnumvars
	     varnames = newvarnames
	     vardims = newvardims
	   ENDIF ELSE BEGIN
	     newvariables = FLTARR(varsize(1) + $
			TOTAL(newvardims), newnumdatalines)
	     newvariables(0:varsize(1)-1,*) = variables
	     newvariables(varsize(1):*,*) = newvars
	     variables = newvariables
	     numvars = numvars + newnumvars
	     varnames = [varnames, newvarnames]
	     vardims = [vardims, newvardims]
	   ENDELSE
	   END

    259L:BEGIN						;definereg_topology
           topnum = topnum + 1
           IF(KEYWORD_SET(topdesclist)) THEN $
             topdesclist = [topdesclist, topdesc] $
	   ELSE topdesclist = REPLICATE(topdesc, 1)
           topdesclist(topnum - 1).name = getword(BIN = BIN)
	   IF (getdef(BIN = BIN) NE 260L) THEN $	;if not elem_samp
	     MESSAGE, "incorrect format, elem_samp should" + $
			" follow define reg_topology"
	   topdesclist(topnum - 1).elem_samp(0) = getnum(BIN = BIN)
	   topdesclist(topnum - 1).elem_samp(1) = getnum(BIN = BIN)
	   topdesclist(topnum - 1).elem_samp(2) = getnum(BIN = BIN)
	   END

    262L:BEGIN						;define irr_topology
	   gridname = getword(BIN = BIN)
	   countdef = getdef(BIN = BIN)
	   IF (countdef NE 263L) THEN $
		MESSAGE, "incorrect format, count should " +$
			"follow define irr_topology"
	   count = getnum(BIN = BIN)
	   elemsdef = getdef(BIN = BIN)
	   IF (elemsdef NE 264L) THEN $
		MESSAGE, "incorrect format, elems should " +$
			"follow count definition"
	   elemsname = getword(BIN = BIN)
	   elemscount = getnum(BIN = BIN)
	   toss = intarr(4,elemscount)
	   IF (KEYWORD_SET(BIN)) THEN $
	     READU, unit, toss $
	   ELSE $
	     READF, unit, toss
	   toss = 0
	   END

    265L:BEGIN						;define reg_grid
           gridnum = gridnum + 1
           IF(KEYWORD_SET(griddesclist)) THEN $
             griddesclist = [griddesclist,griddesc] $
	   ELSE griddesclist = REPLICATE(griddesc, 1)
           griddesclist(gridnum - 1).name = getword(BIN = BIN)
	   IF (getdef(BIN = BIN) NE 266L) THEN $	;if not grid_samp
	     MESSAGE, "incorrect format, grid_samp should" + $
			" follow define reg_grid"
	   griddesclist(gridnum - 1).grid_samp(0) = getnum(BIN = BIN)
	   griddesclist(gridnum - 1).grid_samp(1) = getnum(BIN = BIN)
	   griddesclist(gridnum - 1).grid_samp(2) = getnum(BIN = BIN)
	   IF (getdef(BIN = BIN) NE 267L) THEN $	;if not origin
	     MESSAGE, "incorrect format, origin" + $
			" follow define grid_samp"
	   orig = fltarr(3)
	   orig(0) = getnum(BIN = BIN)
	   orig(1) = getnum(BIN = BIN)
	   orig(2) = getnum(BIN = BIN)
	   IF (getdef(BIN = BIN) NE 268L) THEN $	;if not step
	     MESSAGE, "incorrect format, step" + $
			" follow define origin"
	   steps = FLTARR(3)
	   steps(0) = getnum(BIN = BIN)
	   steps(1) = getnum(BIN = BIN)
	   steps(2) = getnum(BIN = BIN)
	   END

    270L:BEGIN						;defineirr_grid
	   gridname = getword(BIN = BIN)
	   griddef = getdef(BIN = BIN)
	   IF (griddef NE 271) THEN $
		MESSAGE, "incorrect format, grid_vdata should " +$
			"follow define irr_grid"
	   datafieldnum = datafieldnum + 1
           IF(KEYWORD_SET(datafieldlist)) THEN $
             datafieldlist = [datafieldlist, datafield] $
	   ELSE datafieldlist = REPLICATE(datafield, 1)
           datafieldlist(datafieldnum - 1).name = getword(BIN = BIN)
	   datafieldlist(datafieldnum - 1).size = -1
	   END

    273L:BEGIN						;definemesh
           meshnum = meshnum + 1
           IF(KEYWORD_SET(meshdesclist)) THEN $
             meshdesclist = [meshdesclist, meshdesc] $
           ELSE meshdesclist = REPLICATE(meshdesc, 1)
           meshdesclist(meshnum - 1).name = getword(BIN = BIN)
	   IF (getdef(BIN = BIN) NE 274L) THEN $	;if not mesh_topology
	     MESSAGE, "incorrect format, mesh_topology should" + $
			" follow define mesh"
	   meshdesclist(meshnum - 1).topdesc = getword(BIN = BIN)
	   IF (getdef(BIN = BIN) NE 275L) THEN $	;if not mesh_grid
	     MESSAGE, "incorrect format, mesh_grid" + $
			" follow define mesh_topology"
	   meshdesclist(meshnum - 1).griddesc = getword(BIN = BIN)
	   END

    276L:BEGIN						;definevolume
	     volnum = volnum + 1
             IF(KEYWORD_SET(voldesclist)) THEN $
               voldesclist = [voldesclist,voldesc] $
	     ELSE voldesclist = REPLICATE(voldesc, 1)
             voldesclist(volnum - 1).name = getword(BIN = BIN)
	     IF (getdef(BIN = BIN) NE 277L) THEN $	;if not define vol_mesh
	       MESSAGE, "incorrect format, volume_mesh should" + $
			" follow define volume"
	     voldesclist(volnum - 1).volmeshdesc = getword(BIN = BIN)
	     IF (getdef(BIN = BIN) NE 278L) THEN $     ;if not define vol_dvata
	       MESSAGE, "incorrect format, volume_vdata should" + $
			" follow define volume_mesh"
	     voldesclist(volnum - 1).voldata = getword(BIN = BIN)
	     datafieldnum = datafieldnum + 1
             IF(KEYWORD_SET(datafieldlist)) THEN $
               datafieldlist = [datafieldlist, datafield] $
	     ELSE datafieldlist = REPLICATE(datafield, 1)
             datafieldlist(datafieldnum - 1).name = $
			voldesclist(volnum - 1).name
	     datafieldlist(datafieldnum - 1).size = 1
	   END
    ELSE: print, "no case for type ", curval
  ENDCASE
  curval = getdef(BIN = BIN, /BLOCK)
ENDWHILE

done:

FREE_LUN, unit

nomesh = 0

FOR i = 0, volnum - 1 DO BEGIN
  IF (meshnum EQ 0) OR (gridnum EQ 0) OR (topnum EQ 0) THEN BEGIN
    nomesh = 1
  ENDIF ELSE BEGIN
    meshloc = WHERE(voldesclist(i).volmeshdesc EQ $
 		  meshdesclist.name, meshfound)
    dataloc = WHERE(voldesclist(i).voldata EQ $
  		  varnames, datafound)
    IF ((NOT meshfound) OR (NOT datafound)) THEN nomesh = 1
    toploc = WHERE(meshdesclist(meshloc).topdesc EQ $
  		 topdesclist.name, topfound)
    gridloc = WHERE(meshdesclist(meshloc).griddesc EQ $
  		 griddesclist.name, gridfound)
    IF ((NOT topfound) OR (NOT gridfound))THEN nomesh = 1

    griddims = griddesclist(gridloc).grid_samp( $
  	WHERE(griddesclist(gridloc).grid_samp NE 0))
  ENDELSE
ENDFOR

IF NOT(nomesh) THEN BEGIN
  variables = reform(variables, [TOTAL(vardims), griddims])
  MESHNAMES = meshdesclist.name
ENDIF

names = voldesclist.name

startindex = 0
FOR i = 0, datafieldnum - 1 DO BEGIN
  IF(datafieldlist(i).size GE 1) THEN BEGIN
    IF (KEYWORD_SET(validdata)) THEN BEGIN
      validdata = [validdata, INDGEN(datafieldlist(i).size) + startindex]
      dimensions = [dimensions, datafieldlist(i).size]
    ENDIF ELSE BEGIN
      validdata = INDGEN(datafieldlist(i).size) + startindex
      dimensions = datafieldlist(i).size
    ENDELSE
  ENDIF
  startindex = startindex + ABS(datafieldlist(i).size)
ENDFOR

variables = variables(validdata, *, *, *)

END
