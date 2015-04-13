; $Id: ljlct.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

pro ljlct
;+
; NAME:
;	LJLCT
;
; PURPOSE:
;	Load standard color tables for LJ-250/252 printer.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	LJLCT
;
; OUTPUTS:
;	No explicit outputs.
;
; SIDE EFFECTS:
;	The color tables are modified if the current device is 'LJ'.
;
; LIMITATIONS:
;	The default color maps used are for the 90dpi color palette.
;	There are only 8 colors colors availible at 180 dpi.
;
; PROCEDURE:
;	If the current device is 'LJ', !D.N_COLORS is used to determine
;	how many bit planes are in use (1 to 4). The standard
;	color map for that number of planes is loaded. These maps are 
;	described in Chapter 7 of the "LJ250/LJ252 Companion Color
;	Printer Programmer Reference Manual", Table 7-5.  That manual gives
;	the values scaled from 1 to 100, LJLCT scales them from 0 to 255.
;
; MODIFICATION HISTORY:
;	AB, 29 July 1990.
;-

if (!d.name eq 'LJ') then begin
  case (!D.N_COLORS) of
  2: TVLCT, [10B, 229B], [10B, 224B], [15B, 217B]
  4: TVLCT, [10B, 135B, 8B, 229B], [10B, 20B, 66B, 224B], [15B, 36B, 56B, 217B]
  8: TVLCT, [10B, 135B, 8B, 227B, 10B, 135B, 5B, 229B], $
	[10B, 20B, 66B, 212B, 10B, 13B, 56B, 224B], $
	[15B, 36B, 56B, 33B, 74B, 64B, 163B, 217B]
  16: TVLCT, [10B, 135B, 8B, 227B, 10B, 135B, 5B, 184B, $
	      31B, 31B, 38B, 110B, 133B, 8B, 227B, 229B], $
	[10B, 20B, 66B, 212B, 10B, 13B, 56B, 105B, $
	 15B, 20B, 41B, 110B, 15B, 25B, 222B, 224B], $
	[15B, 36B, 56B, 33B, 74B, 64B, 163B, 33B, $
	 61B, 25B, 46B, 115B, 48B, 117B, 79B, 217B]
  endcase
endif
end
