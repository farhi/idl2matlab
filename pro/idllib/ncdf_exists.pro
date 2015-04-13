; $Id: ncdf_exists.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

;
; Copyright (c) 1992-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;
;+
; NAME:
;	NCDF_EXISTS
;
; PURPOSE:
;	Test for the existence of the NetCDF library
;
; CATEGORY:
;	File Formats
;
; CALLING SEQUENCE:
;	Result = NCDF_EXISTS()
;
; INPUTS:
;	None.
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	Returns TRUE (1) if the NetCDF data format library is supported
;	Returns FALSE(0) if it is not.
;
; COMMON BLOCKS:
;	NCDFTEST
;
; EXAMPLE:
;	IF ncdf_exists() EQ 0 THEN Fail,"Library not supported on this machine"
;
; MODIFICATION HISTORY
;	Written by:	Joshua Goldstein,  12/8/92
;
;-

PRO	ncdf_control, cid, VERBOSE=v
	common	ncdftest, has_ncdf
	has_ncdf	= 0
end

FUNCTION ncdf_exists
	common	ncdftest, has_ncdf
	has_ncdf	= 1
	ncdf_control,0,/verbose
	return, has_ncdf
END
