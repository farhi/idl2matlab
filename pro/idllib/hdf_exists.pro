;
; Copyright (c) 1992, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;
;+
; NAME:
;	HDF_EXISTS
;
; PURPOSE:
;	Test for the existence of the HDF library
;
; CATEGORY:
;	File Formats
;
; CALLING SEQUENCE:
;	Result = HDF_EXISTS()
;
; INPUTS:
;	None.
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	Returns TRUE (1) if the HDF data format library is
;	supported. Returns FALSE(0) if it is not.
;
; EXAMPLE:
;	IF hdf_exists() EQ 0 THEN Fail,"HDF not supported on this machine"
;
; MODIFICATION HISTORY
;	Written by:	Joshua Goldstein,  12/21/92
;
;-

;	A fake function if libraries don't exist
FUNCTION dfp_npals,x
	return,1
END

;
FUNCTION hdf_exists
	exists=0

	x=dfp_npals('.');
	if x LE 0 THEN return,1
	return,0
END
