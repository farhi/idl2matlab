; $Id: skey_sgi.pro,v 1.2 1993/06/16 22:26:28 ali Exp $

PRO skey_sgi
;+
; NAME:		SKEY_SGI
; PURPOSE:	Under Unix, the number of function keys, their names, and the
;		escape sequences they send to the host computer vary
;		enough between various keyboards that IDL cannot be
;		written to understand all keyboards. Therefore, it provides
;		a very general routine named DEFINE_KEY that allows the
;		user to specify the names and escape sequences. This
;		routine uses DEFINE_KEY to enter the keys for a Sun keyboard.
;
;		Note: SKEY_SGI is primarily designed to be called by
;		SETUP_KEYS, which attempts to automatically detect the correct
;		keyboard type in use, and define the keys accordingly.
;		Nonetheless, SKEY_SGI may be called as a standalone
;		routine.
;
; CATEGORY:	Misc.
; CALLING SEQUENCE:
;	SKEY_SGI
; INPUTS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:

;	The definitions for the function keys have been entered, and
;	can be viewed using HELP,/KEYS .
; MODIFICATION HISTORY:
;	AB, 26 April 1989
;	TJA, July 1990, setup_keys_sun created by the "breakup" of setup_keys
;		into separate files.
;	SMR, April 1991, setup_keys_sgi created by modifying setup_keys_sun
;	AB, 21 September 1992,renamed from SETUP_KEYS_SGI to SKEY_SGI to
;		avoid DOS filename limitations.
;	AB, 16 June 1993, The IDL scanner used to treat octal string escapes
;		in a manner similar to the C language, but this ability was
;		removed to make the MS DOS port possible (conflicts with
;		file path specifications). Removed all uses of that here.
;-

; F1-F12
FOR i = 1,12 DO $
  IF (i NE 4) THEN $				;function 4 is used for paste
    define_key, 'F' + STRTRIM(i,2), $		;from the SGI scratch buffer
	ESCAPE = string(27B) + '[' + $
		 STRING(STRTRIM(i,2), format = '(I3.3)') + 'q'

END	; Sgi keyboard




