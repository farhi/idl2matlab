; $Id: resolve_all.pro,v 1.1 1995/02/03 21:54:16 dave Exp $
;
; Copyright (c) 1995, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	RESOLVE_ALL
;
; PURPOSE:
;	Resolve (by compiling) all procedures and functions.
;	This is useful when preparing .sav files containing all the IDL
;	routines required for an application.
; CATEGORY:
;	Programming.
; CALLING SEQUENCE:
;	RESOLVE_ALL
; INPUTS:
;	None.
; KEYWORD PARAMETERS:
;	QUIET = if set, produce no messages.
; OUTPUTS:
;	No explicit outputs.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
; RESTRICTIONS:
;	Will not resolve procedures or functions that are called via
;	CALL_PROCEDURE, CALL_FUNCTION, or EXECUTE.  Only explicit calls
;	are resolved.
;
;	If an unresolved procedure or function is not in the IDL 
;	search path, an error occurs, and no additional routines
;	are resolved.
;
; PROCEDURE:
;	This routine iteratively determines the names of unresolved calls
;	to user-written or library procedures and functions, and then
;	compiles them.  The process stops when there are no unresolved
;	routines.
; EXAMPLE:
;	RESOLVE_ALL.
; MODIFICATION HISTORY:
; 	Written by:	Your name here, Date.
;	DMS, RSI, January, 1995.
;-

PRO resolve_all, QUIET = quiet

if n_elements(quiet) ne 0 then begin
    quiet_save=!quiet
    !quiet = quiet
endif else quiet = 0

repeat begin
    cnt = 0
    a = ROUTINE_NAMES(/PROC, /UNRESOLVED)
    if strlen(a(0)) gt 0 then begin
	cnt = cnt + n_elements(a)
	if quiet eq 0 then print,'Resolving procedures: ', a
	resolve_routine, a
	endif
    a = ROUTINE_NAMES(/FUNC, /UNRESOLVED)
    if strlen(a(0)) gt 0 then begin
	cnt = cnt + n_elements(a)
	if quiet eq 0 then print,'Resolving functions: ', a
	resolve_routine, a, /IS_FUNCTION
	endif
endrep until cnt eq 0

if n_elements(quiet_save) ne 0 then !quiet = quiet_save
end
