; $Id: ps_show_fonts.pro,v 1.3 1993/10/18 16:16:26 doug Exp $

;+
; NAME:
;	PS_SHOW_FONTS
;
; PURPOSE:
;	This procedure displays all the PostScript fonts that IDL knows
;	about, with both the StandardAdobe and ISOLatin1 encodings. Each
;	display takes a separate page, and each character in each font
;	is shown with its character index.
;
; CATEGORY:
;	Misc., PostScript, Fonts.
;
; CALLING SEQUENCE:
;	PS_SHOW_FONTS
;
; INPUTS:
;	None.
;
; KEYWORDS:
;	NOLATIN: If set, do NOT output ISOLatin1 encodings.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A PostScript file is produced, one page per font/mapping combination.
;
; RESTRICTIONS:
;	The output file contains almost 70 pages of output. A PostScript
;	previewer is recommended rather than sending it to a printer.
;
; MODIFICATION HISTORY:
;	12 January 1993, AB, RSI.
;	12 October 1993, Rob Montgomery, NCAR - added 'nolatin' keyword.
;-

PRO PS_SHOFONT, font_kw
; Display the font selected by applying font_kw to the DEVICE
; procedure.

  on_error, 2		; Return to caller if an error occurs

  message,/info,font_kw

  DEVICE,ysize=9, yoffset=1, xsize=7, xoffset=.75, /inch, /COURIER
  xyouts, !d.x_size/2, !d.y_size+1000, align=.5, /dev, font_kw, font=0

  ; Lay the characters and indices out in separate passes to minimize
  ; font switching.

  junk = execute('DEVICE,'+font_kw)
  row = 25
  col = 0
  xstep = 1./10.
  ystep = 1./26.0
  for ch = 1, 255 do begin
    s = string(byte(ch))
    if (s eq '!') then s = '!!'
    xyouts, .075 + col * xstep, row*ystep, /norm, font=0, charsize=2.0, s
    if (col eq 9) then begin
      col = 0
      row = row - 1
    endif else col = col + 1
  endfor

  row = 25
  col = 0
  xstep = 1./10.
  ystep = 1./26.0
  yoff=ystep * .1
  for ch = 1, 255 do begin
    xyouts, .12 + col * xstep, row*ystep-yoff, /norm, font=0, charsize=.5, $
	string(ch,format='(I0)')
    if (col eq 9) then begin
      col = 0
      row = row - 1
    endif else col = col + 1
  endfor

  erase
END







pro ps_show_fonts, nolatin=nolatin
;	Display all of the fonts with and without ISO encodings.

  on_error, 2		; Return to caller if an error occurs
  olddev = !d.name
  set_plot,'ps'

  for i = 0, (1 - keyword_set(nolatin)) do begin

    lat = ',ISOLATIN1=' + strcompress(string(i), /remove_all)

    ps_shofont, '/COURIER'				+ lat
    ps_shofont, '/COURIER,/BOLD'			+ lat
    ps_shofont, '/COURIER,/OBLIQUE'			+ lat
    ps_shofont, '/COURIER,/BOLD,/OBLIQUE'		+ lat
    ps_shofont, '/HELVETICA'				+ lat
    ps_shofont, '/HELVETICA,/BOLD'			+ lat
    ps_shofont, '/HELVETICA,/OBLIQU'			+ lat
    ps_shofont, '/HELVETICA,/BOLD,/OBLIQU'		+ lat
    ps_shofont, '/HELVETICA,/NARROW'			+ lat
    ps_shofont, '/HELVETICA,/NARROW,/BOLD'		+ lat
    ps_shofont, '/HELVETICA,/NARROW,/OBLIQUE'		+ lat
    ps_shofont, '/HELVETICA,/NARROW,/BOLD,/OBLIQUE'	+ lat
    ps_shofont, '/AVANTGARDE,/BOOK'			+ lat
    ps_shofont, '/AVANTGARDE,/BOOK,/OBLIQUE'		+ lat
    ps_shofont, '/AVANTGARDE,/DEMI'			+ lat
    ps_shofont, '/AVANTGARDE,/DEMI,/OBLIQUE'		+ lat
    ps_shofont, '/BKMAN,/DEMI'				+ lat
    ps_shofont, '/BKMAN,/DEMI,/ITALIC'			+ lat
    ps_shofont, '/BKMAN,/LIGHT'				+ lat
    ps_shofont, '/BKMAN,/LIGHT,/ITALIC'			+ lat
    ps_shofont, '/ZAPFCHANCERY,/MEDIUM,/ITALIC'		+ lat
    ps_shofont, '/SCHOOLBOOK'				+ lat
    ps_shofont, '/SCHOOLBOOK,/BOLD'			+ lat
    ps_shofont, '/SCHOOLBOOK,/ITALIC'			+ lat
    ps_shofont, '/SCHOOLBOOK,/BOLD,/ITALIC'		+ lat
    ps_shofont, '/PALATINO'				+ lat
    ps_shofont, '/PALATINO,/BOLD'			+ lat
    ps_shofont, '/PALATINO,/ITALIC'			+ lat
    ps_shofont, '/PALATINO,/BOLD,/ITALIC'		+ lat
    ps_shofont, '/TIMES'				+ lat
    ps_shofont, '/TIMES,/BOLD'				+ lat
    ps_shofont, '/TIMES,/ITALIC'			+ lat
    ps_shofont, '/TIMES,/BOLD,/ITALIC'			+ lat
  endfor

  ps_shofont,'/SYMBOL'
  ps_shofont,'/ZAPFDINGBATS'
  
  DEVICE,/CLOSE
  set_plot,olddev
end
