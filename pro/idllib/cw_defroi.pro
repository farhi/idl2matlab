; $Id: cw_defroi.pro,v 1.4 1995/02/22 15:33:28 dave Exp $
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	CW_DEFROI
;
; PURPOSE:
;   Widget to define a region of interest within a widget drawable. 
; CATEGORY:
;   Regions of interest, graphics.
; CALLING SEQUENCE:
;   Result = CW_DEFROI(draw)
; INPUTS:
;   Draw = id of drawable to draw the region.  The drawable should
; 	have both MOTION and BUTTON events enabled.
; KEYWORD PARAMETERS:
;   IMAGE_SIZE = the size of the underlying array, expressed
;       as a two element vector: [columns, rows].  Default =
;       drawable size / zoom.
;   OFFSET = offset of lower left corner of image within the
;       drawable.  Default = [0,0].
;   ORDER = if set, return inverted subscripts, as if the array
;       were output from top to bottom.
;   RESTORE = Set to restore the drawable to its previous appearance
;       on exit.  Otherwise, the regions remain on the drawable.
;   ZOOM = if the image array was expanded (via REBIN for example)
;       specify this two element vector containing the expansion
;       factor in X and Y.  Default = [1,1].  Must be integer.
; OUTPUTS:
;      Result = subscripts of points within the region[s] defined.
;       If no region was defined, a scalar -1 is returned.
; COMMON BLOCKS:
;       None.
; SIDE EFFECTS:
;       The regions are drawn within the drawable.  Set the RESTORE
;       keyword to undo the damage.  
; RESTRICTIONS:
;   This is a MODAL widget.  No other widget applications will be
;   responsive while this widget is in use.
;
; PROCEDURE:
;   Complicated.
; EXAMPLE:
;   To obtain the average of the counts of a region within a drawable:
;   Assume A = the array of interest, n columns, m rows, and that
;   it is displayed in drawable D, at offset X=20, Y=100, and zoomed
;   with a factor of 2:
;       TV, REBIN(A, M*2, N*2), 20, 100     ;Display the image
;       q = CW_DEFROI(D, ZOOM=[2,2], OFFSET=[20,100], IMAGE_SIZE=[m,n])
;       if q(0) ne -1 then print,'Average = ', total(a(q))/n_elements(q)
;       
; MODIFICATION HISTORY:
;   DMS, RSI, December, 1993.  Written.
;-

pro CW_DEFROI_nmode, s, new
; Set new mode... Save old roi by concatenating it with s.subs.
n = s.npts
if (s.mode ne 1) and (n le 2) then n = 0  ;must have 3 pnts for polygon
WIDGET_CONTROL, s.mode_w, SET_VALUE=0       ;Revert to add mode
s.amode = 0

if n ge 1 then begin       ;Old region to save?
    if s.mode eq 1 then begin   ;Points?
        WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY  ;Get old ROI
        xy = xy(0,0:n-1) + s.image_size(0) * xy(1,0:n-1) ;points to subs
        xy = REFORM(xy, n_elements(xy), /OVERWRITE) ;Make linear
    endif else begin
        CW_DEFROI_DRAW, s, -1, /FILL
        WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY  ;Get old ROI
        xy = polyfillv(xy(0,0:n-1),xy(1,0:n-1),s.image_size(0), s.image_size(1))
    ENDELSE
    WIDGET_CONTROL, s.subs, GET_UVALUE=t, /NO_COPY      ;Prev roi pnts
        ;Concatenate s and xy
    if n_elements(t) le 0 then WIDGET_CONTROL, s.subs, SET_UVALUE=xy, /NO_COPY $
    else WIDGET_CONTROL, s.subs, SET_UVALUE=[t,xy], /NO_COPY
    endif               ;Old region to save

s.mode = new
s.npts = 0
end

PRO CW_DEFROI_DRAW, s, i, FILL = fill
; Draw the outline (or polygon if FILL is set) 
; of the region or the ith segment if i < 0.
; Use the XOR drawing mode.

n = s.npts
if n lt 1 then return
   
WSET, s.win   
DEVICE, SET_GRAPHICS=6          ;Xor drawing mode   
col = 1
while col lt !d.table_size do col = col + col
WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY  ;Get ROI
  xsave = !x.s & ysave = !y.s       ;Set scaling to pixel coords
  p = float([!d.x_size, !d.y_size])
  f = s.offset / p
  q = s.zoom / p
  !x.s = [f(0), q(0)]
  !y.s = [f(1), q(1)]

if s.mode eq 1 then BEGIN       ;Point mode?
    if i lt 0 then begin
        i = 0 & i1 = n-1
    ENDIF else i1 = i
    for j = i, i1 do $
        polyfill, xy(0,j) + [0, .9, .9, 0], xy(1,j) + [0,0,.9,.9], COLOR=col
ENDIF ELSE BEGIN            ;Polygon/circle/rect
    if n ge 2 then begin
        if i lt 0 then plots, COLOR=col, xy(*, 0:n-1)+.5 $ ;All of it?
        else plots, COLOR=col, xy(*, i:i+1)+.5  ;One segment
        IF KEYWORD_SET(FILL) then POLYFILL, xy(*,0:n-1), COLOR=col
    ENDIF
ENDELSE

!x.s = xsave & !y.s = ysave
WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY  ;Set ROI   
DEVICE, SET_GRAPHICS=3          ;Copy mode   
end   
   
   
PRO CW_DEFROI_event, ev, s
; This routine is only called from the CW_DEFROI event loop.
; ev = event structure, s = state structure.


s.button = s.button or ev.press xor ev.release  ;New button state   
n = s.npts
x = (ev.x - s.offset(0)) / s.zoom(0)    ;Pixel coordinates
y = (ev.y - s.offset(1)) / s.zoom(1)   

if s.order then y0 = s.image_size(1)-y-1 else y0 = y
WIDGET_CONTROL, s.pos_w, $
    SET_VALUE=string(x, y0, format='("Position: ",i,", ",i)')

if (x lt 0) or (y lt 0) or $            ;Within region?
    (x ge s.image_size(0)) or (y ge s.image_size(1)) then return
if ev.press ne 0 then s.drag = [x,y]    ;Start of drag operation


if (s.mode eq 2) or (s.mode eq 3) then begin ;Rect or square?
    if s.button ne 0 then begin          ;Drag
        if n gt 0 then CW_DEFROI_draw, s, -1      ;Remove old
        t = s.drag
        if s.mode eq 2 then begin   ;Rectangle
            n = 5
            xy = [[t], [x, t(1)], [x, y], [t(0), y], [t]]
        endif else begin        ;Circle
            n = 30              ;# of points
            a = findgen(n+1) * (2 * !pi/(n-1))
            r = sqrt((float(x)-t(0))^2 + (float(y) - t(1))^2)
            xy = transpose([[t(0) + r * cos(a)], [t(1) + r * sin(a)]])
        endelse
    WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY   ;Restore UVALUE
    s.npts = n
    CW_DEFROI_draw, s, -1
    ENDIF                       ;DRAG
    return
ENDIF                           ;Rect or square


if s.button eq 0 then return        ;Must be point or polygon...
tmode = s.amode                     ;Default mode
if s.button eq 4 then tmode = 1     ;Rt button to remove

if tmode then begin            ;Remove prev point?
    if (ev.press ne 0) and (n gt 0) then begin
        CW_DEFROI_DRAW, s, -1   ;Erase old region
        WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY ;Get ROI array
        d = float(x-xy(0,0:n-1))^2 + float(y-xy(1,0:n-1))^2  ;Dist
        t = min(d, ipnt)        ;Closest...
        if ipnt ne (n-1) then xy(0,ipnt) = xy(*,ipnt+1:*)  ;Collapse
        s.npts = n-1
        WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY  ;Save ROI array
        if n gt 1 then CW_DEFROI_DRAW, s, -1   ;Draw new region
        endif   
    return
    endif               ;Remove mode....   
           
; Here we add a point
WIDGET_CONTROL, s.xy_pnts, GET_UVALUE=xy, /NO_COPY  ;Get ROI array

; Add a point
if n_elements(xy) le 1 then xy = intarr(2,100)   
        ; Remove duplicates...   
if n gt 0 then if x eq xy(0,n-1) and y eq xy(1,n-1) then goto, done0
if s.mode eq 1 then for i=0, n-1 do $       ;Point mode?
    IF x eq xy(0,i) and y eq xy(1,i) then goto, done0    ;No duplicates

if (n+1) ge n_elements(xy)/2 then xy = [[xy], [intarr(2,n)]]  ;Extend array?

xy(0,n) = x     ;New point
xy(1,n) = y
n = n + 1
s.npts = n
WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY   ;Restore UVALUE

if s.mode eq 0 then begin           ;Polygon?
    if n ge 2 then CW_DEFROI_draw, s, n-2           ;Draw the new segment
endif else begin                    ;Point
    CW_DEFROI_draw, s, n-1         ;Draw new point
endelse
return
   
done0: WIDGET_CONTROL, s.xy_pnts, SET_UVALUE=xy, /NO_COPY   
end   
   



function CW_DEFROI, draw, ZOOM = zoom, IMAGE_SIZE = image_size, $   
    OFFSET = offset, RESTORE = restore, ORDER = order

base = widget_base(title='Region of Interest', /COLUMN)   
xy_pnts = WIDGET_TEXT(base, YSIZE=2, /FRAME, UVALUE=0, $
    value=['Add with left button: drag or click', $   
        'Remove with right button'])   
Options = CW_BGROUP(base, /ROW, /NO_RELEASE, /RETURN_NAME,  $
    ['Clear', 'Clear All', 'New', 'Cancel'])   
junk = CW_BGROUP(base, /ROW, /EXCLUSIVE, /NO_REL, /RETURN_NAME, $
    ['Polygon', 'Point', 'Rectangle', 'Circle'], SET_VALUE=0)
mode_w = CW_BGROUP(base, /ROW, LABEL_LEFT = 'Mode:', /EXCLUSIVE, /NO_REL, $   
    /RETURN_NAME, ['Add', 'Remove'], SET_VALUE=0)
junk = CW_BGROUP(base, /ROW, /NO_REL, /RETURN_NAME, ['Done'])
pos_w = WIDGET_TEXT(base, YSIZE=1, XSIZE=18, /FRAME, $
    VALUE='Position:    0,    0')

WIDGET_CONTROL, draw, GET_VALUE=win
WSET, win   
   
if n_elements(zoom) le 0 then zoom = [1,1]
if n_elements(image_size) le 0 then image_size = [!d.x_size, !d.y_size] / zoom
if n_elements(offset) le 0 then offset = [0,0]   
p  = offset + image_size /2   
if (!version.os NE 'MacOS') THEN TVCRS, p(0), p(1), /DEVICE ELSE TVCRS, 1
   
WINDOW, /PIXMAP, /FREE, xs = !d.x_size, ys=!d.y_size  ;Save window
backing = !d.window
DEVICE, copy = [0,0, !d.x_size, !d.y_size, 0, 0, win]  ;Save it

s = { CW_DEFROI_STRUCT, $       ;Structure containing state
    base: base, $       ;Main base widget
    xy_pnts: xy_pnts, $ ;Current roi vertex list
    npts : 0L, $        ;# of points in current roi
    subs : pos_w, $     ;Widget holding prev subscripts
    pos_w : pos_w, $    ;Position text widget
    mode: 0, $          ;major mode
    amode: 0, $         ;0 for add, 1 for remove
    draw: draw, $       ;draw widget id
    win:  win, $        ;draw widget window #
    button: 0, $        ;button state
    image_size : long(image_size), $   ;Image array size
    mode_w: mode_w, $   ;Add/remove button widget
    backing: backing, $ ;Pixmap for backing store
    offset: fix(offset), $   ;offset of array within window
    zoom : fix(zoom), $ ;zoom factor
    order : KEYWORD_SET(order), $  ;Image order
    drag: [0,0]}        ;Beginning of drag motion
   
WIDGET_CONTROL, base, /REALIZE
WSHOW, win
   
WHILE 1 DO BEGIN                ;Internal event loop   
    ev = WIDGET_EVENT([base, draw])
    n = s.npts
    if ev.id eq draw then CW_DEFROI_EVENT, ev, s $
    else case ev.value of   
'Clear All':  BEGIN
    WIDGET_CONTROL, s.subs, GET_UVALUE=t, /NO_COPY  ;Clr list of subscripts
    t = 0
    WSET, win
    DEVICE, copy = [0,0, !d.x_size, !d.y_size, 0, 0, backing]  ;Restore it
    s.npts = 0
    ENDCASE
'Clear':  BEGIN   
    if (n ge 2) or (s.mode eq 1 and n ge 1) then $
        CW_DEFROI_draw, s, -1        ;Erase roi   
    s.npts = 0   
    CW_DEFROI_NMODE, s, s.mode
    ENDCASE
'New' : CW_DEFROI_nmode, s, s.mode      ;Make a new region...
'Cancel':  BEGIN   
    xy = -1
    goto, all_done
    ENDCASE    

;    ['Polygon', 'Point', 'Rectangle', 'Circle'], SET_VALUE=0)
'Polygon': CW_DEFROI_nmode, s, 0
'Point' :  CW_DEFROI_nmode, s, 1
'Rectangle' :  CW_DEFROI_nmode, s, 2
'Circle' :  CW_DEFROI_nmode, s, 3

'Add':  s.amode = 0
'Remove': s.amode = 1
'Done': BEGIN   
    cw_defroi_nmode, s, 0       ;Save old region
    WIDGET_CONTROL, s.subs, GET_UVALUE=t, /NO_COPY  ;List of subscripts
    xy = BYTARR(s.image_size(0), s.image_size(1))  ;Return only unique
    if n_elements(t) gt 0 then xy(t) = 1
    if s.order then xy = reverse(xy,2)  ;Flip it?
    xy = where(temporary(xy))
all_done:
    IF KEYWORD_SET(restore) then begin      ;Undo damage?
        WSET, win
        DEVICE, copy = [0,0, !d.x_size, !d.y_size, 0, 0, backing]  ;Restore it
        ENDIF
    WDELETE, backing
    WIDGET_CONTROL, base, /DESTROY   
    return, xy
    ENDCASE    
ENDCASE   
ENDWHILE            ;Event loop
END   

