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
		;errplot,sin(w/5)-0.5,sin(w/5)+0.5
		end
'surface':	begin
		surface,dist(25),/horizontal,ax=15,shades=bytscl(dist(25))
		end
'shade_surf':	begin w=dist(200) & s=size(w)
		shade_surf,w,findgen(s(1))+50,findgen(s(2))+100,ax=75,max_value=max(w)*2/3,shades=bytscl(dist(200))
		end
'live_contour':	begin w=dist(40) & s=size(w)

		live_contour,w
		end
'contour':begin
		 w=dist(40) & s=size(w)
		contour,w,/fill,nlevels=10
		end
'erase':	begin erase,50
		end
'tvscl':	begin tvscl,dist(80),150,100
		end
'image_cont':	begin
		image_cont,dist(50)
		end
'replot':	begin
	    ;if (!D.Name eq 'WIN')then begin w=tvrd(/true) & w=color_Quan(w,1,a,b,c)
		;endif else w=tvrd()
		tvlct,R,G,B,/GET
		w=tvrd()
		erase & tv,w & tvlct,R,G,B
		;w=bytscl(dist(80)) & erase & tv,w
		end
'background': begin
		if !P.background NE 200 then !P.background=200 else !P.background = 0
		end
'ticklen': begin
		IF !P.ticklen NE 1 then!P.ticklen=1 ELSE !P.ticklen=0
		end
'linestyle': begin
		if !P.linestyle EQ 5 then !P.linestyle =0 else !P.linestyle = 5
		end
'loadct': begin
		IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_DROPLIST') $
		THEN BEGIN
     	CASE ev.index OF
       	0: loadct,0
       	1: loadct,1
       	2: loadct,3
       	3: loadct,5
       	4: loadct,7
       	5: loadct,11
       	6: loadct,13
       	7: loadct,18
     	ENDCASE
		ENDIF
		END
		;end
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
device,decomposed=0
base  =widget_base(title='desstest',/column)
row1  =widget_base  (base,/row)
but11 =widget_button(row1,value='plot'      ,uvalue='plot')
but12 =widget_button(row1,value='oploterr'  ,uvalue='oploterr')
but13 =widget_button(row1,value='surface'   ,uvalue='surface')
but14 =widget_button(row1,value='shade_surf',uvalue='shade_surf')
but15 =widget_button(row1,value='live_contour'   ,uvalue='live_contour')
but16 =widget_button(row1,value='erase'     ,uvalue='erase')
but10 =widget_button(row1,value='contour',uvalue='contour')
list1 =widget_droplist(row1,value=['B-W LINEAR','BLUE/WHITE','RED TEMPERATURE','STD GAMMA-II',$
		'RED_PURPLE','BLUE-RED','RAINBOW','Pastels'],uvalue='loadct')

row2  =widget_base  (base,/row)
but17 =widget_button(row2,value='tvscl'     ,uvalue='tvscl')
but18 =widget_button(row2,value='image_cont'  ,uvalue='image_cont')
but19 =widget_button(row2,value='replot'    ,uvalue='replot')
but23 =widget_button(row2,value='!P.background',uvalue='background')
but24 =widget_button(row2,value='!P.ticklen',uvalue='ticklen')
but25 =widget_button(row2,value='!P.linestyle',uvalue='linestyle')


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
!p.title	='Test DESS-GI (variable systeme)'
!x.title	='This is X'
!y.title	='This is Y'
!z.title	='This is Z'
!p.subtitle	='Sub_title'
!p.charsize	=.6
;!x.style	=1
;!y.style	=1
;!z.style	=1
end
