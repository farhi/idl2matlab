pro desstest_event,ev
;** **************
;**
common desstest,win1,win2,labl

widget_control,ev.id,get_uvalue=uv

case uv of
'plot':		begin w=findgen(100) & s=size(w)
		plot,alog10(w+1),sin(w/5),title='sin(findgen(100)/10)',xtitle='alog10(findgen(100)+1)',thick=3
		end
'oploterr':	begin w=findgen(100) & s=size(w)
		plot,sin(w/5),title='sin(findgen(100)/10)',thick=1
		oploterr,sin(w/5),sqrt(sin(w/5)>0)
		end
'surface':	begin
		surface,dist(30),/horizontal
		end
'shade_surf':	begin w=dist(200) & s=size(w)
		shade_surf,w,findgen(s(1))+50,findgen(s(2))+100,ax=75,max_value=max(w)*2/3
		end
'contour':	begin w=dist(40) & s=size(w)
		contour,w,/fill ,alog10((indgen(s(1))+1)/10),alog10((indgen(s(1))+1)/10)
		end
'erase':	begin erase,100
		end
'tvscl':	begin tvscl,dist(80),150,100
		end
'polyfill':	begin x = [30, 100, 100, 30] & y = [30, 30, 100, 100]
		polyfill, x, y, color = 175, /device
		end
'replot':	begin
		w=tvrd() & erase & tv,w
		end
'win1':		begin wset,win1 & widget_control,labl,set_value='Current window is 1'
		end
'win2':		begin wset,win2 & widget_control,labl,set_value='Current window is 2'
		end
'winevent1':	if !d.window eq win1 then begin
		cursor,x,y,0,/data   & widget_control,labl,set_value='Data:   X='+string(x)+' Y='+string(y)
		if ev.type eq 0 then   widget_control,labl,set_value='Button Press'
		if ev.type eq 1 then   widget_control,labl,set_value='Button Release'
		endif
'winevent2':	if !d.window eq win2 then begin
		cursor,x,y,0,/device & widget_control,labl,set_value='Device: X='+string(x)+' Y='+string(y)
		if ev.type eq 0 then   widget_control,labl,set_value='Button Press'
		if ev.type eq 1 then   widget_control,labl,set_value='Button Release'
		endif
else:
endcase
end

pro desstest
;** ********
;**
common desstest,win1,win2,labl

IF xregistered('desstest') then return

base  =widget_base(title='desstest',/column)

row1  =widget_base  (base,/row)
but11 =widget_button(row1,value='plot'      ,uvalue='plot')
but12 =widget_button(row1,value='oploterr'  ,uvalue='oploterr')
but13 =widget_button(row1,value='surface'   ,uvalue='surface')
but14 =widget_button(row1,value='shade_surf',uvalue='shade_surf')
but15 =widget_button(row1,value='contour'   ,uvalue='contour')
but16 =widget_button(row1,value='erase'     ,uvalue='erase')

row2  =widget_base  (base,/row)
but17 =widget_button(row2,value='tvscl'     ,uvalue='tvscl')
but18 =widget_button(row2,value='polyfill'  ,uvalue='polyfill')
but19 =widget_button(row2,value='replot'    ,uvalue='replot')

buex  =widget_base  (base,/row,/exclusive)
but21 =widget_button(buex,value='Use Win#1   ' ,uvalue='win1',/no_release)
but22 =widget_button(buex,value='Use Win#2   ' ,uvalue='win2',/no_release)

labl  =widget_label (base,xsize=600,value=' ')

row3  =widget_base  (base,/row)
draw1 =widget_draw  (row3,xsize=300,ysize=250,/button_event,/motion_event,uvalue='winevent1',retain=2,colors=-10)
draw2 =widget_draw  (row3,xsize=300,ysize=250,/button_event,/motion_event,uvalue='winevent2',retain=2)

widget_control,base,/realize
widget_control,but21,set_button=1
widget_control,draw1,get_value=win1 & widget_control,draw2,get_value=win2 & win2=win2
xmanager,'desstest',base,/no_block
wset,win1
!p.title	='Teste DESS-GI'
!x.title	='This is X'
!y.title	='This is Y'
!z.title	='This is Z'
!p.subtitle	='Sub_title'
!p.charsize	=.6
!x.style	=1
!y.style	=1
!z.style	=1
end
