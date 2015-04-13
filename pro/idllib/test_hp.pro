; $Id: test_hp.pro,v 1.2 1995/02/02 22:34:02 billo Exp $

;+
; NAME:		TEST_HP
; PURPOSE:	Generate HP-GL output to test features of the driver.
; CATEGORY:	Graphics Drivers.
; CALLING SEQUENCE:
;	TEST_HP
; INPUTS:
;	None.
; KEYWORD PARAMETERS:
;	EVASDROP - If this keyword *and* SMART are present, the output
;		is sent directly to the terminal with an evasdrop configuration
;		assummed.
;	SMART - If this keyword is present, it is assummed that the
;		plotter has a sheet feeder and is able to do
;		polygon filling. The HP7550A is an example of such
;		a plotter.
; OUTPUTS:
;	If the SMART keyword is present, a file named hpdemo.1 is created
;	containing 4 pages of plotter output. If SMART and EVASDROP are
;	present, the output goes to the terminal. Otherwise, three files
;	named hpdemo.1, hpdemo.2, and hpdemo.3 are created containing
;	one page of output each.
; RESTRICTIONS:
;	Expects the plotter to have 6 pens.
; SIDE EFFECTS:
;	Output file(s) are created.
; MODIFICATION HISTORY:
;	AB, July, 1989.
;-


pro hp_label_page
; Put a label at (0,0) identifying the page.

  common HPDEV,CURPAGE,SMART,EVASDROP

  on_error,2                    ;Return to caller if an error occurs
  if (smart) then s = 'HP7550A' else s = 'HP7475'
  xyouts, /norm, 0, -.04, 'HP-GL Demo (' + s + '). Page: ' + $
	strcompress(curpage,/remove_all)

end



pro hp_erase
; Some plotters have a sheet feeder, others can be put offline,
; and still others (e.g. HP7475) can only handle a page at a time.
; In order to accomodate the least common denominator, this routine
; is called to set up the next page. It closes the current file
; and opens a new one.

  common HPDEV,CURPAGE,SMART,EVASDROP

  on_error,2                    ;Return to caller if an error occurs
  curpage = curpage + 1

  if (EVASDROP or (SMART and (curpage ne 1))) then erase else begin
    DEVICE,/CLOSE
    DEVICE,filename='hpdemo.' + strcompress(curpage,/remove_all)
  endelse

end



pro hp_polyfill, title		; Polygon filling exercise
;	title - Main title for plot

on_error,2                       ;Return to caller if an error occurs
hp_erase
device,/landscape
plot,[0.0, 1.2],[0.0, 1.2],/nodata, title = title
for i = 0.0, 1.1, .2 do oplot, [0, 1.2 - i], [i, 1.2]
for i = 0.2, 1.1, .2 do oplot, [i, 1.2], [0, 1.2 - i]

; Determine the polygon regions
pregion_x = fltarr(5,12)
pregion_y = fltarr(5,12)
pregionlimit = intarr(12) + 4
pregionlimit(0) = 3
pregionlimit(11) = 3
; zeroth
pregion_x(0:3, 0) = [0.0, 0.2, 0.0, 0.0]
pregion_y(0:3, 0) = [1.0, 1.2, 1.2, 1.0]
; 11th
pregion_x(0:3, 11) = [1.0, 1.2, 1.2, 1.0]
pregion_y(0:3, 11) = [0.0, 0.0, 0.2, 0.0]
; 2 - 6
n = 1
for i = 0.2, 1.1, 0.2 do begin
  pregion_x(0:4, n) = [0., i + .2, i, 0., 0.]
  pregion_y(0:4, n) = [1.0-i, 1.2, 1.2, 1.2-i, 1.0-i]
  n = n + 1
endfor
; 7 - 11
for i = 0.2, 1.1, 0.2 do begin
  pregion_x(0:4, n) = [i, 1.2, 1.2, i-.2, i]
  pregion_y(0:4, n) = [0., 1.2-i, 1.4-i, 0., 0.]
  n = n + 1
endfor


; This vector indicates solid vs. line fill polyfilling
fill = intarr(12) + 1
fill(10:11) = 0

; The line attrbiutes if line filling
orient = [ 0.0, 45.0, 90.0, 0.0, 0.0, 45.0, 90.0, 0.0, 22.5, 45.0, 0.0, 0.0 ]
cross = fltarr(12)
cross(7) = [90.0, 112.5, 135.0]
spacing = fltarr(12) + 0.25
spacing(10:11) = 0.0
lstyle = [ 0, 0, 0, 1, 4, 4, 4, 0, 0, 0, 0, 0 ]
color = [2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 2, 3 ]

; Finally, draw the basic styles
for i = 0, 9 do begin
  polyfill, pregion_x(0:pregionlimit(i),i), pregion_y(0:pregionlimit(i),i), $
	    line_fill = fill(i), orient=orient(i), spacing=spacing(i), $
	    linestyle = lstyle(i), color=color(i)
  if (cross(i) ne 0) then begin
    polyfill, pregion_x(0:pregionlimit(i),i), pregion_y(0:pregionlimit(i),i), $
	      line_fill = fill(i), orient=cross(i), spacing=spacing(i), $
	      linestyle = lstyle(i), color=color(i)
  endif
endfor
for i = 10,11 do begin
  polyfill, pregion_x(0:pregionlimit(i),i), pregion_y(0:pregionlimit(i),i), $
	color=color(i)
endfor

hp_label_page
end


pro hp_lines_and_color
; Line styles and color exercise
on_error,2                    ;Return to caller if an error occurs
hp_erase
device,/landscape
a = findgen(100) - 10
plot, sin(a/5)/exp(a/50),/nodata,title='HP-GL Linestyle And Color Test'
for i = 0, 5 do begin
  a = a + 10
  c = i + 1
  y = .4 - .025 * i
  xyouts, /norm, .6, y, strcompress(string(i),/remove_all), color = c
  plots, /norm, [.62, .9], [y, y], color = c, linestyle = i
  oplot, sin(a/5)/exp(a/50), color = c, linestyle=i
endfor
hp_label_page
end


pro hp_hwtext		; Hardware Text exercise
on_error,2              ;Return to caller if an error occurs
device,/portrait
hp_erase
!p.font=0
xyouts,/norm,.1,.5,'Hardware Text Demonstration',align=.5,orien=90, $
	color=5, size=2.0
plots,/norm,[.05,.05, .125,.125, .05],[0, 1, 1, 0, 0],color=5

plots,/norm,[.5,.5],[0,1]
xyouts,/norm,.5,.7, 'Right Justified Text',align=1.0
xyouts,/norm,.5,.8, 'Centered Text',align=0.5
xyouts,/norm,.2,.3, '2.5 times normal size',size=2.5,orien=30
for i = 1, 4 do begin
  angle = i * 45
  str = '  ' + strcompress(string(angle),/remove_all) + ' degrees rotation'
  xyouts,/norm,.75, .25, str, color=i, orien=angle
  angle = angle + 180
  str = '  ' + strcompress(string(angle),/remove_all) + ' degrees rotation'
  xyouts,/norm,.75, .25, str, color=i, orien=angle
endfor
xyouts,/norm,.5,.9, 'Left Justified Text',align=0.0
!p.font=-1
hp_label_page
end


pro TEST_HP,SMART=KW_SMART,EVASDROP=KW_EVASDROP

common HPDEV,CURPAGE,SMART,EVASDROP
on_error,2                      ;Return to caller if an error occurs
CURPAGE = 0
SMART = (n_elements(KW_SMART) ne 0)
EVASDROP = (n_elements(KW_EVASDROP) ne 0) and SMART

current_d = !D.NAME		; Remember current device
set_plot,'hp'			; Switch to HP-GL
if (SMART) then DEVICE,/EJECT
if (EVASDROP) then DEVICE,/PLOTTER,file='/dev/tty'

hp_lines_and_color
hp_hwtext
hp_polyfill, 'Fill Patterns (Software)'
if (smart) then begin
  DEVICE,/POLYFILL
  hp_polyfill, 'Fill Patterns (Hardware)'
endif

set_plot, current_d		; Return to original device
end



