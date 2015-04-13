; $Id: skey_mips.pro,v 1.2 1993/06/16 22:26:28 ali Exp $

pro skey_mips,app_keypad=app_keypad,num_keypad=num_keypad
;+
; NAME:
;	SKEY_MIPS
;
; PURPOSE:
;	Under Unix, the number of function keys, their names, and the
;	escape sequences they send to the host computer vary
;	enough between various keyboards that IDL cannot be
;	written to understand all keyboards. Therefore, it provides
;	a very general routine named DEFINE_KEY that allows the
;	user to specify the names and escape sequences. This
;	routine uses DEFINE_KEY to enter the keys for the MIPS RS
;	series keyboard.
;
;	Note: SKEY_MIPS is primarily designed to be called by
;	SETUP_KEYS, which attempts to automatically detect the correct
;	keyboard type in use, and define the keys accordingly.
;	Nonetheless, SKEY_MIPS can be called as a standalone
;	routine.
;
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	SKEY_MIPS
;
; INPUTS:
;	None.
;
; KEYWORD PARAMETERS:
;  APP_KEYPAD:	Defines escape sequences for the group of keys
;		in the numeric keypad, enabling these keys to be programmed
;		within IDL.
;
;  NUM_KEYPAD:	Disables programmability of the numeric keypad.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The definitions for the function keys have been entered, and
;	can be viewed using HELP,/KEYS .
;
;	The <Print Scrn>, <Scroll Lock>, and <Pause> keys are not defined,
;	as they do not generate standard escape sequences.  The same is
;	true for <Home>, <Delete>, and <End> in the grouping of six keys above
;	the arrow keys; however, the other three in the group ARE defined.
;	Keys defined to have labels beginning with a capital "K" belong
;	to the numeric keypad group.  For example, "K9" refers to keypad
;	key "9".  The <Num Lock> key is the only one within the keypad
;	which is not defined.
;
; MODIFICATION HISTORY:
;	TJA , July 1990.  SETUP_KEYS_MIPS created, and call to SETUP_KEYS_MIPS
;			  placed in procedure SETUP_KEYS.
;	AB, 21 September 1992,renamed from SETUP_KEYS_MIPS to SKEY_MIPS to
;		avoid DOS filename limitations.
;	AB, 16 June 1993, The IDL scanner used to treat octal string escapes
;		in a manner similar to the C language, but this ability was
;		removed to make the MS DOS port possible (conflicts with
;		file path specifications). Removed all uses of that here.
;-
if (keyword_set(app_keypad)) then print,string(byte([27,61])) ; Enable keypad
if (keyword_set(num_keypad)) then print,string(byte([27,62])) ; Disable keypad
esc = string(27B)
newl = string(13B)
; Top row of function keys
fvals=[11,12,13,14,15,17,18,19,20,21,23,24]
for i=1,12 do define_key,'F'+strtrim(string(i),2), $
              escape=esc+'['+string(fvals(i-1),'(i2)')+'~'
; Labelled keys above the arrow keys (HOME, DELETE, and END are not definable)
define_key,'INSERT',escape=esc+'[2~'
define_key,'PAGE_UP',escape=esc+'[5~'
define_key,'PAGE_DOWN',escape=esc+'[6~'
; Numeric keypad - This only works if application keypad is enabled!
define_key,'K/',escape=esc+'Oo'
define_key,'K*',escape=esc+'Oj'
define_key,'K-',escape=esc+'Om'
define_key,'K+',escape=esc+'Ok'
define_key,'K_ENTER',escape=esc+'OM'
nkeys='0123456789'
nvals='pqrstuvwxy'
for i=0,9 do define_key,'K'+strmid(nkeys,i,1), $
             escape=esc+'O'+strmid(nvals,i,1)
define_key,'K.',escape=esc+'On'
return ; MIPS RS keyboard
end
