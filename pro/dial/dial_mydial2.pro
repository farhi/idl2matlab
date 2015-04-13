pro dial_mydial2_macro, d

	if d.init eq 0 then begin
		d.init = 1
		print, d.name + ' ' + 'First call'
		dialtag,'mydial',tag='NAME',get=nam
		if nam eq '' then dialinit, 'mydial' else dialstart, 'mydial'
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
	plot,cos((findgen(50)+d.cnt)/5.),title='Plot of cos(Theta)',$
	  xtitle='Stage UFR-IMA',ytitle='cos(Theta)',background=255,color=0
      	d.cnt=d.cnt+1
end

;********************
function dial_mydial2
;********************
;**
    return,{frequency:1.5, duration:0, fig:0, cnt:0, init:0, name:'mydial2',value:0.}
    end
    
