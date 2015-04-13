; $Id: setup_keys.pro,v 1.2 1994/11/23 05:54:48 ali Exp $

;+
; NAME:
;	SETUP_KEYS
;
; PURPOSE:
;	Set up function keys for use with IDL.
;
;	Under Unix, the number of function keys, their names, and the
;	escape sequences they send to the host computer vary
;	enough between various keyboards that IDL cannot be
;	written to understand all keyboards.  Therefore, it provides
;	a very general routine named DEFINE_KEY that allows the
;	user to specify the names and escape sequences of function keys.
;
;	SETUP_KEYS uses DEFINE_KEY (called via modular procedures) to
;	enter the keys for known keyboards (Sun3, DEC VT200, HP, 
;	Mips, PSTERM, SGI).
;
; CATEGORY:
;	Misc.
;
; CALLING SEQUENCE:
;	SETUP_KEYS
;
; INPUTS:
;	None.
;
; KEYWORD PARAMETERS:
;	NOTE:  If no keyword is specified, SETUP_KEYS uses !VERSION to
;	determine the type of machine running IDL. It assumes that the
;	keyboard involved is of the same type (usually this is correct).
;
;    EIGHTBIT:	When establishing VT200 function key definitions,
;		use the 8-bit versions of the escape codes instead
;		of the default 7-bit.
;
;	SUN:	Establish function key definitions for a Sun3 keyboard.
;
;	VT200:	Establish function key definitions for a DEC VT200 keyboard.
;
;	HP9000:	Establish function key definitions for an HP 9000 series
;		300 keyboard.  Although the HP 9000 series 300 supports both
;		xterm and hpterm windows, IDL supports only user-definable
;		key definitions in xterm windows - hpterm windows use
;		non-standard escape sequences which IDL does not attempt
;		to handle.
;
;	IBM	Establish function key definitions for an IBM keyboard.
;
;	MIPS:	Establish function key definitions for a Mips RS series
;		keyboard.
;
;	PSTERM:	Establish function key definitions for PSTERMS such as those
;		found on the Sun Sparc Station.
;
;	SGI:	Establish function key definitions for SGI keyboards.
;
;  APP_KEYPAD:	Will define escape sequences for the group of keys
;		in the numeric keypad, enabling these keys to be programmed
;		within IDL.
;
;  NUM_KEYPAD:	Will disable programmability of the numeric keypad.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The definitions for the function keys are entered.  The new keys
;	can be viewed using the command HELP, /KEYS.
;
;	For the HP keyboard, the upper right-hand group of four keys (at the
;	same height as the function keys) are called "BLANK1" through
;	"BLANK4", since they have no written labels.  Keys defined to have
;	labels beginning with a capital "K" belong to the numeric keypad
;	group.  For example, "K9" refers to keypad key "9".
;
;	Although the HP 9000 series 300 can create both xterm and hpterm
;	windows, IDL supports only user-definable key definitions in xterm
;	windows - hpterm windows use non-standard escape sequences which
;	IDL does not attempt to handle.
;
; MODIFICATION HISTORY:
;	AB, 26 April 1989
;	TJA, July 1990.	Added key definitions for HP 9000 series 300, as
;			well as Mips RS series; also rearranged code into 
;			separate files.
;
;	SMR, April, 1991.  Added key definitions for SGI and PSTERM
;	AB, 22 November 1994, Added key definitions for IBM.
;-
PRO setup_keys, SUN = SUN, VT200 = VT200, EIGHTBIT = EIGHTBIT, $
	HP9000 = HP9000, MIPS = MIPS, APP_KEYPAD = APP_KEYPAD, $
	NUM_KEYPAD = NUM_KEYPAD, PSTERM = PSTERM, SGI = SGI, $
	IBM = IBM

ON_ERROR,2                              	;Return to caller if an error
						;occurs

IF (!VERSION.OS EQ 'vms') THEN $		;VMS keys are fixed
  MESSAGE, 'VMS key names are "pre-set" by the Screen Management Utility.'

do_sun		= KEYWORD_SET(SUN)
do_psterm	= KEYWORD_SET(PSTERM)
do_dec		= KEYWORD_SET(VT200)
do_hp		= KEYWORD_SET(HP9000)
do_mips		= KEYWORD_SET(MIPS)
do_sgi		= KEYWORD_SET(SGI)
do_ibm		= KEYWORD_SET(IBM)

IF ((NOT do_sun) AND $				;Default to Sun if sunos, HP 
    (NOT do_psterm) AND $			;if hp-ux, mips if mipqs 
    (NOT do_dec) AND $				;arch., else vt200 (DEC)
    (NOT do_hp) AND $
    (NOT do_mips) AND $
    (NOT do_ibm) AND $
    (NOT do_sgi)) THEN BEGIN
  
  IF (!VERSION.OS EQ "sunos") THEN do_sun = 1 $
  ELSE IF (!VERSION.OS EQ "hp-ux") THEN do_hp = 1 $
  ELSE IF (!VERSION.OS EQ "IRIX") THEN do_sgi = 1 $
  ELSE IF (!VERSION.OS EQ "AIX") THEN do_ibm = 1 $
  ELSE IF (STRPOS(!VERSION.ARCH,"mips") NE -1) THEN do_mips = 1 $
  ELSE do_dec = 1
ENDIF

IF (do_psterm) THEN skey_sun, /PSTERM $
ELSE IF (do_sun) THEN skey_sun, PSTERM = PSTERM $
ELSE IF (do_dec) THEN skey_dec, EIGHTBIT = EIGHTBIT, $
			APP_KEYPAD = APP_KEYPAD, $
			NUM_KEYPAD = NUM_KEYPAD $
ELSE IF (do_hp) THEN skey_hp, APP_KEYPAD = APP_KEYPAD, $
			NUM_KEYPAD = NUM_KEYPAD $
ELSE IF (do_sgi) THEN skey_sgi $
ELSE IF (do_ibm) THEN skey_ibm $
ELSE IF (do_mips) THEN skey_mips, APP_KEYPAD = APP_KEYPAD, $
			NUM_KEYPAD = NUM_KEYPAD
END
