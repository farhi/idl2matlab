; $Id: write_wave.pro,v 1.2 1994/01/03 22:35:39 doug Exp $

PRO write_wave, filename, data, BIN = BIN, NOMESHDEF = NOMESHDEF, $
		DATANAME = DATANAME, $
		MESHNAME = MESHNAME, $
		VECTOR = VECTOR
;+
; NAME:
;	WRITE_WAVE
;
; PURPOSE:
;	Write a three dimensional IDL array to a .wave or .bwave file
;	for use with the Wavefront Visualizer.
;
; CALLING SEQUENCE:
;	WRITE_WAVE, File, Array
;
; INPUTS:
;	File: 	   Scalar string giving the name of the Wavefront file to write.
;	Array:	   3D matrix to be output.
;
; KEYWORD PARAMETERS:
;	BIN:       If the BIN keyword is set, the file will be written in
;		   Binary mode, otherwise, it is written as a text file.
;	DATANAME:  The name of the data inside of the Wavefront file.  If
;		   not specified, the name used is "idldata".
;	MESHNAME:  The name of the mesh used in the Wavefront file.  When
;		   not specified, the name used is "idlmesh".
;	NOMESHDEF: When set, no mesh definition is included.
;	VECTOR:    When set, the variable being written is written as a 
;		   vector
;
; OUTPUTS:
;	FILE contains the array in Wavefront file format. If DATANAME
;	was supplied, the scalar data field in the Wavefront file is given
;	that name. 
;
; RESTRICTIONS:
;	This routine only writes one scalar field for each Wavefront file
;	that it creates.
;
; MODIFICATION HISTORY:
;	Written July 3, 1991, by Steve Richards.
;-
; Copyright (c) 1991, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;


IF NOT(KEYWORD_SET(DATANAME)) THEN DATANAME = "idldata"
IF NOT(KEYWORD_SET(MESHNAME)) THEN MESHNAME = "idlmesh"

s = size(data)
IF (KEYWORD_SET(VECTOR) AND (s(1) EQ 3)) THEN vectwrite = 2 $
ELSE vectwrite = 1

OPENW, unit, filename, /GET_LUN

IF (KEYWORD_SET(BIN)) THEN BEGIN

  WRITEU, unit, 257L					;new block
  WRITEU, unit, 276L					;definevolume
  WRITEU, unit, LONG(STRLEN(DATANAME)), DATANAME
  WRITEU, unit, 277L					;definevolume_mesh
  WRITEU, unit, LONG(STRLEN(MESHNAME)), MESHNAME
  WRITEU, unit, 278L					;definevolume_vdata
  WRITEU, unit, LONG(STRLEN(DATANAME)), DATANAME

  IF (NOT(KEYWORD_SET(NOMESHDEF))) THEN BEGIN
    WRITEU, unit, 257L					;newblock
    WRITEU, unit, 273L					;definemesh
    WRITEU, unit, LONG(STRLEN(MESHNAME)), MESHNAME
    WRITEU, unit, 274L					;definemesh_topology
    WRITEU, unit, 6L, "idltop"
    WRITEU, unit, 275L					;definemesh_grid
    WRITEU, unit, 7L, "idlgrid"
  
    WRITEU, unit, 257L					;newblock
    WRITEU, unit, 259L					;definereg_topology
    WRITEU, unit, 6L, "idltop"
    WRITEU, unit, 260L					;defineelem_samp
    WRITEU, unit, (s(vectwrite:s(0)) - 1)
  
    WRITEU, unit, 257L					;newblock
    WRITEU, unit, 265L					;definereg_grid
    WRITEU, unit, 7L, "idlgrid"
    WRITEU, unit, 266L					;definegrid_samp
    WRITEU, unit, s(vectwrite:s(0))
    WRITEU, unit, 267L					;defineorigin
    WRITEU, unit, LONARR(s(0))
    WRITEU, unit, 268L					;definestep
    WRITEU, unit, (FLTARR(s(0)) + 1.)
  ENDIF

  WRITEU, unit, 257L					;newblock
  WRITEU, unit, 258L					;definevdata
  WRITEU, unit, 1L
  IF (vectwrite EQ 2) THEN $
    DATANAME = DATANAME + "(3)"
  WRITEU, unit, LONG(STRLEN(DATANAME)), DATANAME
  WRITEU, unit, LONG(N_ELEMENTS(data))
  WRITEU, unit, FLOAT(data)

ENDIF ELSE BEGIN					;else write text file

  PRINTF, unit, ""
  PRINTF, unit, ""
  PRINTF, unit, ""

  PRINTF, unit, "define volume " + DATANAME
  PRINTF, unit, "  volume_mesh " + MESHNAME
  PRINTF, unit, "  volume_vdata " + DATANAME
  PRINTF, unit, ""

  IF (NOT(KEYWORD_SET(NOMESHDEF))) THEN BEGIN
    PRINTF, unit, "define mesh " + MESHNAME
    PRINTF, unit, "  mesh_topology idltop"
    PRINTF, unit, "  mesh_grid idlgrid"
    PRINTF, unit, ""
  
    PRINTF, unit, "define reg_topology idltop"
    endofline = STRCOMPRESS(s(1:s(0)) - 1)
    PRINTF, unit, "  elem_samp" + $
    	STRCOMPRESS(REFORM(BYTE(STRING(s(vectwrite:s(0)) - 1)), 36, /OVER))
    PRINTF, unit, ""
    PRINTF, unit, "define reg_grid idlgrid"
    PRINTF, unit, "  grid_samp" + $
    	STRCOMPRESS(REFORM(BYTE(STRING(s(vectwrite:s(0)))), 36, /OVERW))
    PRINTF, unit, "  origin" + $
    	STRCOMPRESS(REFORM(BYTE(STRING(lonarr(3))), 36, /OVERW))
    PRINTF, unit, "  step" + $
    	STRCOMPRESS(REFORM(BYTE(STRING(fltarr(3) + 1.0)), 39, /OVERW))
    PRINTF, unit, ""
  ENDIF

  IF (vectwrite EQ 2) THEN $
    DATANAME = DATANAME + "(3)"
  PRINTF, unit, "define vdata 1 " + DATANAME + STRCOMPRESS(N_ELEMENTS(data))
  PRINTF, unit, ""
  IF (vectwrite EQ 2) THEN $
    PRINTF, unit, data, FORMAT = '(F8.3, F8.3, F8.3)' $
  ELSE PRINTF, unit, data, FORMAT = '(F8.3)'
ENDELSE

FREE_LUN, unit

END

