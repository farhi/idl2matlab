; $Id: zoom_24.pro,v 1.2 1994/06/08 22:37:34 dan Exp $

pro zoom_24, xsize=xs, ysize=ys, fact=fact, right=right
;+
; NAME:	
;	ZOOM_24
;
; PURPOSE:
;	Display part of a 24-bit color image from the current window
;	expanded in another window.  (This procedure was modified from the 
;	8-bit procedure ZOOM).
;
;	The cursor and left mouse button are used to mark the center of the 
;	zoom.  The cursor can be moved into the zoom window to determine the 
;	coordinates (in the original image) and color values of individual 
;	pixels.  In the zoom window, the right mouse button returns you to the
;	mode for selecting a new zoom window or magnification factor
;	from the original image.  The center mouse button is used in the 
;	original picture window to bring up the magnification-factor 
;	selector.  The mouse button erases the zoom window and pixel window
;	and exits.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	ZOOM_24 [, XSIZE = Xsize, YSIZE = Ysize, FACT = Fact, /RIGHT] 
;
; INPUTS:
;	All input parameters are passed as keywords.
;
; KEYWORDS:
;	XSIZE:	The X size of the zoom window.  The default is 512.
;
;	YSIZE:	The Y size of zoom window.  The default is 512.
;
;	FACT:	The zoom enlargement factor.  The default is 4.
;
;	RIGHT:	Position keyword (0 = left screen, 1 = right screen).
;
; OUTPUTS:
;	No explicit outputs. Two new windows are created, and both
;	are destroyed when the procedure is exited.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Two windows are created/destroyed.  An auxiliary image
;	array is created.
;
; RESTRICTIONS:
;	ZOOM_24 only works with 24-bit color systems.
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;	Original: 9-March-1990	W. T. Vetterling
;-

if n_elements(xs) le 0 then xs=512	; Default x-size
if n_elements(ys) le 0 then ys=512	; Default y-size
if n_elements(fact) le 0 then fact=4	; Default magnification

waitflg=3			; Wait for mouse clicks
ifact = fact       		
orig_w = !d.window		; Save original window number
orig_ysiz=!d.y_vsize		; Save original window y-size
xs = xs < !d.x_vsize		; For small originals, don't
ys = ys < !d.y_vsize	  	;   make zoom window too large
tvcrs,1				; Enable cursor
ierase = 0			; Erase zoom window flag

lm=50				; Left margin, pixel window
bm=50                           ; Bottom margin, pixel window
box=60				; Size of color box
barw=20				; Width of color bars
barl=128			; Maximum length of bars
bars=20				; Separation of color bars

lbar=lm+box+30			; Left end of color bars
rbar=lbar+barl			; Right end of color bars

rbox=lm+box			; Right side of box
tbox=bm+box			; Top of box

rtext=lm+300			; Right end of text
btext=bm+120			; Bottom of text
ttext=bm+175			; Top of text

bar1b=bm			; Bar 1 bottom
bar1t=bm+barw                   ; Bar 1 top
bar2b=bar1t+bars                ; Bar 2 bottom
bar2t=bar2b+barw                ; Bar 2 top
bar3b=bar2t+bars                ; Bar 3 bottom
bar3t=bar3b+barw                ; Bar 3 top

red=255L			; Colors for graphics
green=red*256                   ;   in pixel window
blue=green*256
black=0
white=red+green+blue

if keyword_set(right) then xp=!d.x_size+20 else xp=0
; Position expansion windows to left or right of original

window,xpos=xp,ypos=0,xsize=xs,ysize=ys,title='Zoomed Image',/free
zoom_w = !d.window

window,xpos=xp,ypos=ys+30,xsize=rbar+20,ysize=ttext+40,title='Pixel Values',/free
pixel_w = !d.window

!p.color=white          ; Post instructions
xyouts,lm,bm+125,'Make Selection From Original:',/dev
xyouts,lm,bm+100,'   Left Button Selects Region',/dev
xyouts,lm,bm+75,'   Center Button Selects Size',/dev
xyouts,lm,bm+50,'   Right Button Exits',/dev

wset,orig_w

again:
	tvrdc,x,y,waitflg,/dev	;Wait for mousie

	case !err of

4:	goto, done		; Right-hand mouse button quits

2:	if !d.name eq 'SUN' or !d.name eq 'X' then begin
				; Center mouse button brings up
				;   scale for choosing magnification
		s  = ['New Zoom Factor:',strtrim(indgen(19)+2,2)]
		ifact = wmenu(s,init=ifact-1,title=0)+1
                IF (!Version.Os NE 'MacOS') THEN $
                        tvcrs,x,y,/dev $        ;Restore cursor
                ELSE tvcrs,1
		ierase = 1
	endif else begin
		Read,'Current factor is',ifact+0,'.  Enter new factor: ',ifact
		if ifact le 0 then begin
			ifact = 4
			print,'Illegal Zoom factor.'
		endif
		ierase = 1	; Clean out previous display
	endelse

else:	begin	 				; Left-hand mouse button gives 
						;   expanded image
		nx = xs/ifact			; Size of new image
		ny = ys/ifact

		x0 = 0 > (x-nx/2)	 	; Make sure we don't crop
		y0 = 0 > (y-ny/2)	 	;   image on left or bottom
		x0 = x0 < (!d.x_vsize - nx)     ; Make sure we don't crop
		y0 = y0 < (!d.y_vsize - ny)     ;   image on the right or top

		a = tvrd(x0,y0,nx,ny,true=3)	; Read image, pixel interleaved

		wset,pixel_w			; Erase pixel window
		erase

		wset,zoom_w			; Go to zoom window
		if ierase then erase		; Erase it?
		ierase = 0

		xss = nx * ifact		; Make integer rebin factors
		yss = ny * ifact

		b=rebin(a,xss,yss,3,/sample)	; Rebin to correct size
						;   using pixel replication
						;   if necessary
		tv,b,0,0,true=3			; Show the magnified image
	
loop:
		wset,zoom_w			; Go to the zoomed window
		tvrdc,x,y,2,/dev		; Continuously sample the
						;   mouse coordinates
		if (!err eq 4) then goto,another
						; If right-hand button is
						;   pushed, get another sample
						;   from the original
		wset,pixel_w			; Otherwise, go to pixel
						;   window

		xx=string(format='(i3)',(x0+x/ifact))	; Coordinate text
     		yy=string(format='(i3)',(orig_ysiz-y0-1-y/ifact))
		ybar=ys-y-1

		rstr=string(format='(i3)',b(x,ybar,0))
						; Color text
		gstr=string(format='(i3)',b(x,ybar,1))
		bstr=string(format='(i3)',b(x,ybar,2))

		bar1=lbar+b(x,ybar,0)/2
		bar2=lbar+b(x,ybar,1)/2
		bar3=lbar+b(x,ybar,2)/2

		!p.color=black			; Cover old text
		polyfill,[lm,rtext,rtext,lm],[btext,btext,ttext,ttext],/dev
		; Note: This is faster than erase

       		!p.color=white			; Set color to white
		xyouts,lm,bm+155,'Position: ('+xx+','+yy+')',/dev
		xyouts,lm,bm+130,'Value:    ('+rstr+','+gstr+','+bstr+')',/dev

		!p.color=black			; Cover old color bars
		polyfill,[lbar,rbar,rbar,lbar],[bar1b,bar1b,bar3t,bar3t],/dev

		!p.color=red			; Red bar
		polyfill, [lbar,bar1,bar1,lbar],[bar3b,bar3b,bar3t,bar3t],/dev

		!p.color=green			; Green bar
		polyfill, [lbar,bar2,bar2,lbar],[bar2b,bar2b,bar2t,bar2t],/dev

		!p.color=blue			; Blue bar
		polyfill, [lbar,bar3,bar3,lbar],[bar1b,bar1b,bar1t,bar1t],/dev

		!p.color=b(x,ybar,0)+256.0*(b(x,ybar,1)+256.0*b(x,ybar,2))
					;Box showing pixel color
		polyfill, [lm,rbox,rbox,lm],[bm,bm,tbox,tbox],/dev
	
		goto,loop		; Get another pixel value

another:				; Get another section of original
					;   image

		wset,pixel_w            ; Erase pixel window
		erase

		!p.color=white          ; ... and give instructions
		xyouts,lm,bm+125,'Make Selection From Original:',/dev
		xyouts,lm,bm+100,'   Left Button Selects Region',/dev
		xyouts,lm,bm+75,'   Center Button Selects Size',/dev
		xyouts,lm,bm+50,'   Right Button Exits',/dev

		wset,orig_w		; Set to original image
	endelse	
endcase

goto,again				; Return to original state

done:
	if zoom_w ge 0 then wdelete,zoom_w	; Done with windows
	if pixel_w ge 0 then wdelete,pixel_w
	b=0				; Eliminate intermediate image
end















