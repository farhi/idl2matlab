PRO resize_window,event
  widget_control,event.top,get_uvalue=pointertodata
  widget_control,pointertodata,get_uvalue=info
  drawwidgetid=widget_info(event.id,/child)
  widget_control,drawwidgetid,draw_xsize=event.x,draw_ysize=event.y
  wset,info.wid
  IF n_elements(info.data(0,*)) gt 1 THEN shade_surf,info.data,info.p.x,info.p.y ELSE plot,info.p.x,info.data
END

PRO FirstWid_Event,event
widget_control,event.top,get_uvalue=info
wset,info.wid
Widget_control,event.id,Get_value=buttonValue
CASE buttonValue OF
  'Shaded Plot':	IF n_elements(info.data(0,*)) gt 1 THEN Shade_surf,info.data,info.p.x,info.p.y
  'Surface Plot':	IF n_elements(info.data(0,*)) gt 1 THEN surface,info.data,info.p.x,info.p.y
  'Contour Plot':	IF n_elements(info.data(0,*)) gt 1 THEN contour,info.data,info.p.x,info.p.y,nlevels=12,/follow,c_colors=indgen(6)*20+80
  'Default Plot':	see,info.data
  'Line Plot':		IF n_elements(info.data(0,*)) le 1 THEN plot,info.p.x,info.data
  'Pretreatment':	pretreat,info.data
  'Calibration':	calib,info.data
  'Single Pattern':	unic,info.data
  'Goodbye':		BEGIN
  			  print,'See you later!'
			  Widget_Control,event.top,/Destroy
			END
ENDCASE
END

FUNCTION scan,w
tlb =		Widget_base  (Column=1,Title='First Widget Program',tlb_frame_attr=1)
topmenu =	widget_button(tlb,    value='Plot Options',/menu)
shadedplot =	widget_button(topmenu,value='Shaded Plot')
surfaceplot =	widget_button(topmenu,value='Surface Plot')
contourplot =	widget_button(topmenu,value='Contour Plot')
defaultplot =	widget_button(topmenu,value='Default Plot')
lineplot =	widget_button(topmenu,value='Line Plot')
pretreat =	widget_button(tlb,    value='Pretreatment')
calib =		widget_button(tlb,    value='Calibration')
unic =		widget_button(tlb,    value='Single Pattern')
byebutton =	widget_button(tlb,    value='Goodbye')
graphicsbase =	widget_base  (tlb_size_events=1,event_pro='Resize_Window',uvalue=tlb,xoffset=50,yoffset=75,group_leader=tlb)
draw=		Widget_Draw  (graphicsbase,xsize=200,ysize=200)
widget_control,tlb,/realize
widget_control,graphicsbase,/realize
widget_control,draw,get_value=windowindex
wset,windowindex
loadct,5
take_datp,datp
IF n_elements(w(0,*)) gt 1 THEN shade_surf,w,datp.x,datp.y ELSE plot,datp.x,w
info={data:w,p:datp,wid:windowindex}
widget_control,tlb,set_uvalue=info
XManager,'FirstWid',tlb,Event='FirstWid_Event'
give_datp,datp
return,w
END
