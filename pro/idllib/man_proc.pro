; $Id: man_proc.pro,v 1.2 1993/05/17 16:45:04 ali Exp $

;+
; NAME:
;	MAN_PROC
;
; PURPOSE:
;	Provide online documentation for IDL topics.  If the current
;	graphics device supports widgets, a graphical user interface
;	is used.  Otherwise, a more basic version that is a cross
;	between the Unix man pages and VMS online help is used.  The
;	help is organized in a two-level hierarchy.  Level 1 is the
;	global subject, and Level 2 supplies help on subjects within
;	each global subject.
;
; CATEGORY:
;	Help, documentation.
;
; CALLING SEQUENCE:
;	MAN_PROC [, Request]
;
; INPUTS:
;     Request:	A scalar string containing the item for which help is desired.
;		This string can contain one or two (whitespace separated) 
;		words.  The first word is taken as the global topic and the 
;		second as the topic within the scope of the first.  The user
;		is prompted for missing words.
;
; OUTPUTS:
;	The widget version uses a text widget to display the help
;	text.  The basic version sends help text to the standard output.
;
; COMMON BLOCKS:
;	None.
;
; RESTRICTIONS:
;	The help text is derived from the LaTeX files used to produce
;	the reference manual.  However, it is not possible to produce
;	exactly the same output as found in the manual due to the limitations
;	of text-oriented terminals.  Therefore, the text used is considerably
;	abbreviated.  Always check the manual if the online help is
;	insufficient. 
;
; MODIFICATION HISTORY:
;	4 January 1991, AB	Renamed the old MAN_PROC to MP_BASIC and added
;		 		MP_WIDGETS to handle the widget interface.
;	3 September 1992, AB	Switched from the IDLwidgets version
;				(MP_WIDGETS) to the builtin help. This allows
;				help and the IDL> prompt to work simultaneously
;	17 May 1993, AB		Reverted to using MP_WIDGETS for the Sun 3
;				because it is stuck at OpenWindows 2.0 and
;				recent changes to online help will not be
;				ported to that platform.
;-
;

PRO MAN_PROC, REQUEST

  ; If the current graphics device supports widgets, use them. Otherwise,
  ; use the basic version. Sun 3s use the old MP_WIDGETS
  if ((!D.FLAGS and 65536) eq 0) then begin
    MP_BASIC, REQUEST
  endif else begin
    if ((!version.os eq 'sunos') and (!version.arch eq 'mc68020')) then begin
      MP_WIDGETS, REQUEST
    endif else begin
      WIDGET_OLH, REQUEST
    endelse
  endelse

end






















