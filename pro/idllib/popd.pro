; $Id: popd.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

;+
; NAME:
;	POPD
;
; PURPOSE:
;	Change the current working directory to the directory
;	saved on the top of the directory stack maintained
;	by the PUSHD and POPD User Library procedures. This top entry
;	is then removed.
;
; CALLING SEQUENCE:
;	POPD
;
; SIDE EFFECTS:
;	The top entry of the directory stack is removed.
;
; RESTRICTIONS:
;	Popping a directory from an empty stack causes a warning
;	message to be printed.  The current directory is not changed
;	in this case.
;
; COMMON BLOCKS:
;	DIR_STACK:  Contains the stack.
;
; MODIFICATION HISTORY:
;	17, July, 1989, Written by AB, RSI.
;-
;
;
pro popd

COMMON DIR_STACK, DEPTH, STACK
on_error, 2		; Return to caller on error
if (n_elements(DEPTH) eq 0) then depth = 0
if (DEPTH eq 0) then message, 'Directory stack is empty.'
cd, stack(0)
DEPTH = DEPTH - 1
if (DEPTH eq 0) then STACK = 0 else STACK = STACK(1:*)
end
