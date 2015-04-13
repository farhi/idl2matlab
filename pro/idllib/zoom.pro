; $Id: zoom.pro,v 1.5 1994/06/08 22:37:17 dan Exp $
pro zoom,xsize=xs, ysize=ys, fact = fact, interp = interp, continuous = cont, $
        keep=keep, zoom_window=zoom_win, new_window=new_win
;+
; NAME:	
;	ZOOM
;
; PURPOSE:
;	Display part of an image (or graphics) from the current window
;	enlarged in another window.
;
;	The cursor is used to mark the center of the zoom.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	ZOOM [, FACT = Fact, /INTERP, XSIZE = Xs, YSIZE = Ys, /CONTINUOUS, $
;		/KEEP, ZOOM_WINDOW=Zoom_Win, /NEW_WINDOW ]
;
; INPUTS:
;	All input parameters are passed as keywords.
;
; KEYWORDS:
;	FACT:	Zoom factor.  This parameter must be an integer.  The default
;		zoom factor is 4.
;
;	INTERP:	Set this keyword to use bilinear interpolation, otherwise 
;		pixel replication is used.
;
;	XSIZE:	The X size of the zoom window.  The default is 512.
;
;	YSIZE:	The Y size of the zoom window.  The default is 512.
;
;   CONTINUOUS:	Set this keyword to make the zoom window track the mouse
;		without requiring the user to press the left mouse button.
;		This feature only works well on fast computers.
;
;         KEEP: Keep the zoom window after exiting the procedure.
;
;  ZOOM_WINDOW:	When used with KEEP, returns the index of the zoom window.
;		Otherwise, if KEEP is not set, then -1 is returned.
;
;   NEW_WINDOW:	Normally, if ZOOM is called with /KEEP and then called again,
;		it will use the same window to display the zoomed image.
;		Calling ZOOM with /NEW_WINDOW forces it to create a new window
;		for this purpose.
;
; OUTPUTS:
;	No explicit outputs.   A new window is created if necessary. It
;	is destroyed upon exit if KEEP is not specified.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A window is created/destroyed.
;
;	When ZOOM is reusing a zoom window from a previous call to ZOOM,/KEEP,
;	then the XSIZE and YSIZE parameters are reset to the actual size of the
;	window.
;
; RESTRICTIONS:
;	ZOOM only works with color systems.
;
; PROCEDURE:
;	Straightforward.
; 
; MODIFICATION HISTORY:
;	?
;       William Thompson, March 1992, added common block ZOOM_WINDOW
;		and KEEP keyword.
;	William Thompson, 20 May 1993, added ZOOM_WINDOW and NEW_WINDOW
;		keywords.
;
;-
on_error,2              ;Return to caller if an error occurs
common zoom_window, zoom_w
;
if n_elements(xs) le 0 then xs = 512
if n_elements(ys) le 0 then ys = 512
if n_elements(fact) le 0 then fact=4
if keyword_set(cont) then waitflg = 2 else waitflg = 3
ifact = fact
old_w = !d.window
if keyword_set(new_win) then zoom_w = -1	;Don't use old window (if any)
if n_elements(zoom_w) eq 0 then zoom_w = -1		;No zoom window yet
;
;  If an old window is to be used, then make sure it still exists.  (Added by
;  William Thompson, 20 May 1993.)
;
if zoom_w ge 0 then begin
	device, window_state=win_state
	if not win_state(zoom_w) then zoom_w = -1
endif
;
;  Make sure the parameters xs and ys agree with the size of the window, in
;  case a window is being reused from a previous call to ZOOM,/KEEP.  (Added by
;  William Thompson, 20 May 1993.)
;
IF ZOOM_W GE 0 THEN BEGIN
	OLD_WINDOW = !D.WINDOW
	WSET, ZOOM_W
	XS = !D.X_SIZE
	YS = !D.Y_SIZE
	WSET, OLD_WINDOW
ENDIF
tvcrs,1			;enable cursor
ierase = 0		;erase zoom window flag
print,'Left for zoom center, Middle for new zoom factor, Right to quit'
again:
	tvrdc,x,y,waitflg,/dev	;Wait for change
	case !err of
4:	goto, done
2:	if !d.name eq 'SUN' or !d.name eq 'X' then begin	;Sun view?
		s  = ['New Zoom Factor:',strtrim(indgen(19)+2,2)]
		ifact = wmenu(s, init=ifact-1,title=0)+1
		IF (!Version.Os NE 'MacOS') THEN $
			tvcrs,x,y,/dev $	;Restore cursor
                ELSE tvcrs,1
		ierase = 1
	endif else begin
		Read,'Current factor is',ifact+0,'.  Enter new factor: ',ifact
		if ifact le 0 then begin
			ifact = 4
			print,'Illegal Zoom factor.'
			endif
			ierase = 1	;Clean out previous display
	endelse
else:	begin
	x0 = 0 > (x-xs/(ifact*2)) 	;left edge from center
	y0 = 0 > (y-ys/(ifact*2)) 	;bottom
	nx = xs/ifact			;Size of new image
	ny = ys/ifact
	nx = nx < (!d.x_vsize-x0)
	ny = ny < (!d.y_size-y0)
	x0 = x0 < (!d.x_vsize - nx)
	y0 = y0 < (!d.y_vsize - ny)
	a = tvrd(x0,y0,nx,ny)		;Read image
	if zoom_w lt 0 then begin	;Make new window?
		window,/free,xsize=xs,ysize=ys,title='Zoomed Image'
		zoom_w = !d.window
	endif else begin
		wset,zoom_w
		if ierase then erase		;Erase it?
		ierase = 0
	endelse
	xss = nx * ifact	;Make integer rebin factors
	yss = ny * ifact
	tv,rebin(a,xss,yss,sample=1-keyword_set(interp))
	wset,old_w
	endcase
endcase
goto,again

done:
IF NOT KEYWORD_SET(KEEP) THEN BEGIN
        if zoom_w ge 0 then wdelete,zoom_w              ;Done with window
        ZOOM_W = -1
ENDIF
zoom_win = zoom_w	;Return index of zoom window to user
end

