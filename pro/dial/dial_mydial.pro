pro dial_mydial_macro, d

	if d.init eq 0 then begin
		d.init = 1
		print, d.name + ' ' + 'First call'
		d.frequency=2.
	endif
	
	if (xregistered(d.name) le 0) or (d.fig le 0) then begin
		bas = widget_base(title=d.name)
		device,pseudo=8
		d.fig = widget_draw(bas,xsize=300,ysize=150,colors=-50)
		widget_control, bas, /realize
		widget_control, d.fig, get_value=win & d.fig=win
		xmanager, d.name, bas,/just_reg
	endif
	d.value=d.cnt / 10.
	wset,d.fig
	plot,sin((findgen(50)+d.cnt)),title='Plot of sin(Theta)',$
	  xtitle='Stage UFR-IMA',ytitle='sin(Theta)',background=255,color=0
      	d.cnt=d.cnt+1
end

;********************
function dial_mydial
;********************
;**
    return,{frequency:1.5, duration:0, fig:0, cnt:0, init:0, name:'mydial',value:0.}
    end
    
