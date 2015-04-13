; $Id: skey_sun.pro,v 1.2 1993/06/16 22:26:28 ali Exp $

pro skey_sun, psterm=psterm
;+
; NAME:
;	SKEY_SUN
;
; PURPOSE:
;	Under Unix, the number of function keys, their names, and the
;	escape sequences they send to the host computer vary
;	enough between various keyboards that IDL cannot be
;	written to understand all keyboards. Therefore, it provides
;	a very general routine named DEFINE_KEY that allows the
;	user to specify the names and escape sequences. This
;	routine uses DEFINE_KEY to enter the keys for a Sun keyboard.
;
;	Note: SKEY_SUN is primarily designed to be called by
;	SETUP_KEYS, which attempts to automatically detect the correct
;	keyboard type in use, and define the keys accordingly.
;	Nonetheless, SKEY_SUN can be called as a standalone
;	routine.
;
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	SKEY_SUN
;
; INPUTS:
;	None.
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The definitions for the function keys have been entered, and
;	can be viewed using the command HELP, /KEYS .
;
; MODIFICATION HISTORY:
;	AB, 26 April 1989
;	TJA, July 1990, SETUP_KEYS_SUN created by the "breakup" of SETUP_KEYS
;			into separate files.
;	AB, 21 September 1992,renamed from SETUP_KEYS_SUN to SKEY_SUN to
;		avoid DOS filename limitations.
;	AB, 16 June 1993, The IDL scanner used to treat octal string escapes
;		in a manner similar to the C language, but this ability was
;		removed to make the MS DOS port possible (conflicts with
;		file path specifications). Removed all uses of that here.
;-

CSI=string(27B)+'['
if(keyword_set(psterm)) then $			;the difference between psterm
  define_key, "F1", escape = CSI+'224z'	;and sun is function 1.

; F2-F9
for i=2,9 do define_key,'F'+strtrim(i,2),escape=CSI+strtrim(i+223,2)+'z'

; R1 - R15
for i=1,15 do define_key,'R'+strtrim(i,2),escape=CSI+strtrim(i+207,2)+'z'

; Arrow keys in function code mode, and R13, R15
define_key, 'R8', /PREVIOUS_LINE
define_key, 'R10', /BACK_CHARACTER
define_key, 'R12', /FORWARD_CHARACTER
define_key, 'R13', /BACK_WORD
define_key, 'R14', /NEXT_LINE
define_key, 'R15', /FORWARD_WORD
return ; Sun3 keyboard
end
