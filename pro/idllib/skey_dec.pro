; $Id: skey_dec.pro,v 1.2 1993/06/16 22:26:28 ali Exp $

pro skey_dec,eightbit=eightbit,app_keypad=app_keypad, num_keypad=num_keypad
;+
; NAME:
;	SKEY_DEC
;
; PURPOSE:
;	Under Unix, the number of function keys, their names, and the
;	escape sequences they send to the host computer vary
;	enough between various keyboards that IDL cannot be
;	written to understand all keyboards. Therefore, it provides
;	a very general routine named DEFINE_KEY that allows the
;	user to specify the names and escape sequences. This
;	routine uses DEFINE_KEY to enter the keys for a DEC
;	VT200-style keyboard.
;
;	Note: SKEY_DEC is primarily designed to be called by
;	SETUP_KEYS, which attempts to automatically detect the correct
;	keyboard type in use, and define the keys accordingly.
;	Nonetheless, SKEY_DEC can be called as a standalone
;	routine.
;
;	This procedure is for Unix systems - NOT VMS.
;
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	SKEY_DEC
;
; INPUTS:
;	None.
;
; KEYWORD PARAMETERS:
;    EIGHTBIT:	When establishing VT200 function key definitions,
;		use the 8-bit versions of the escape codes instead
;		of the default 7-bit.
;
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
;	can be viewed using the command HELP, /KEYS .
;
; MODIFICATION HISTORY:
;	AB, 26 April 1989
;	TJA, July 1990, setup_keys_dec created by the "breakup" of setup_keys
;		into separate files.  Also, keywords added to enable and
;		disable programmability of the numeric keypad.
;	AB, 21 September 1992,renamed from SETUP_KEYS_DEC to SKEY_DEC to
;		avoid DOS filename limitations.
;	AB, 16 June 1993, The IDL scanner used to treat octal string escapes
;		in a manner similar to the C language, but this ability was
;		removed to make the MS DOS port possible (conflicts with
;		file path specifications). Removed all uses of that here.
;-
if (keyword_set(app_keypad)) then print,string(byte([27,61])) ; Enable keypad
if (keyword_set(num_keypad)) then print,string(byte([27,62])) ; Disable keypad
if keyword_set(eightbit) then begin
  esc = string(155B)			; \233
  keypad = string(143B)			; \217
  ; Eight bit arrow keys
  define_key, "UP_ARROW (EIGHT BIT)", esc=esc+'A', /PREVIOUS_LINE
  define_key, "DOWN_ARROW (EIGHT BIT)", esc=esc+'B', /NEXT_LINE
  define_key, "RIGHT_ARROW (EIGHT BIT)", esc=esc+'C', /FORWARD_CHARACTER
  define_key, "LEFT_ARROW (EIGHT BIT)", esc=esc+'D', /BACK_CHARACTER
endif else begin
  esc = string(27B) + '['
  keypad = string(27B) + 'O'
endelse
; Top row function keys
start = [6, 12, 17]
finish = [10, 14, 20]
offset = [11, 12, 14]
for i = 0, 2 do begin
  for j=start(i),finish(i) do begin
    define_key,'F'+strtrim(j,2),escape=esc+strtrim(j+offset(i),2)+'~'
  endfor
endfor
; Help and Do keys
define_key, 'HELP', escape=esc+'28~'
define_key, 'DO', escape=esc+'29~'
; Top two rows of middle keypad (with arrow keys)
names = ['FIND','INSERT-HERE','REMOVE','SELECT','PREV-SCREEN','NEXT-SCREEN']
for i = 1, 6 do define_key, names(i-1), escape=esc+strtrim(i, 2)+'~'
; Application keypad
for i = 49B, 52B do define_key, 'PF'+string(i), escape=keypad+string(i+31B)
for i = 48B, 57B do define_key, string(i), escape=keypad+string(i+64B)
for i = 44B, 46B do define_key, string(i), escape=keypad+strtrim(i+64B)
define_key, 'ENTER', escape=keypad+'M'
return	; DEC VT200 keyboard (Unix)
end
