; $Id: skey_hp.pro,v 1.2 1993/06/16 22:26:28 ali Exp $

pro skey_hp,app_keypad=app_keypad,num_keypad=num_keypad
;+
; NAME:
;	SKEY_HP
;
; PURPOSE:
;	Under Unix, the number of function keys, their names, and the
;	escape sequences they send to the host computer vary
;	enough between various keyboards that IDL cannot be
;	written to understand all keyboards. Therefore, it provides
;	a very general routine named DEFINE_KEY that allows the
;	user to specify the names and escape sequences. This
;	routine uses DEFINE_KEY to enter the keys for the HP 9000
;	series 300 keyboard.
;
;	Note: SKEY_HP is primarily designed to be called by
;	SETUP_KEYS, which attempts to automatically detect the correct
;	keyboard type in use, and define the keys accordingly.
;	Nonetheless, SKEY_HP can be called as a standalone
;	routine.
;
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	SKEY_HP
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
;	The upper right-hand group of four keys (at the same height as
;	the function keys) are called "BLANK1" through "BLANK4", since
;	they have no written labels.  Keys defined to have labels beginning
;	with a capital "K" belong to the numeric keypad group.  For example,
;	"K9" refers to keypad key "9".
;
;	Although the HP 9000 series 300 can create both xterm and hpterm
;	windows, IDL supports only user-definable key definitions in xterm
;	windows - hpterm windows use non-standard escape sequences which
;	IDL does not attempt to handle.
;
; MODIFICATION HISTORY:
;	TJA & AB, July 1990, setup_keys_hp created, and call to setup_keys_hp
;		placed in procedure setup_keys
;	AB, 21 September 1992,renamed from SETUP_KEYS_HP to SKEY_HP to
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
fkeys='12345678'
fvals='12345789' ; Not a mistake - no 6!
for i=1,8 do define_key,'F'+strmid(fkeys,(i-1),1), $
             escape=esc+'[1'+strmid(fvals,(i-1),1)+'~'
; Menu key on top row
define_key,'MENU',escape=esc+'[29~'
; Upper righthand "unlabelled" keys (same height as the function keys)
bkeys='1234'
bvals='0134'  ; No 2!
for i=1,4 do define_key,'BLANK'+strmid(bkeys,(i-1),1), $
             escape=esc+'[2'+strmid(bvals,(i-1),1)+'~'
; Labelled keys near the Return key
define_key,'PREV',escape=esc+'[5~'
define_key,'SELECT',escape=esc+'[4~'
define_key,'NEXT',escape=esc+'[6~'
; Numeric keypad - This only works if application keypad is enabled!
define_key,'K*',escape=esc+'Oj'
define_key,'K/',escape=esc+'Oo'
define_key,'K+',escape=esc+'Ok'
define_key,'K-',escape=esc+'Om'
define_key,'K_ENTER',escape=esc+'OM'
nkeys='0123456789'
nvals='pqrstuvwxy'
for i=0,9 do define_key,'K'+strmid(nkeys,i,1), $
             escape=esc+'O'+strmid(nvals,i,1)
define_key,'K,',escape=esc+'Ol'
define_key,'K_TAB',escape=esc+'OI'
define_key,'K.',escape=esc+'On'
return ; HP 9000 Series 300 keyboard (xterm only)
end
