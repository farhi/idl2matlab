pro dial_mydial3_macro, d

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
	d.value=d.cnt*2
	wset,d.fig
	shade_surf,d.peaks,ax=70,az=d.cnt+10.,title='Stage UFR-IMA'
      	d.cnt=d.cnt+1
end

;********************
function dial_mydial3
;********************
;**
    return,{frequency:1.5, duration:0, fig:0, cnt:0, init:0, name:'mydial3',value:0.,peaks:dist(30),h:0}
    end
    
