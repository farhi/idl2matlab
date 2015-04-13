pro view

;program to display an array of any size

;variables

;global variables
common vars,x1,y1,x2,y2,xsize,ysize,grpx,grpy,wid,data,ref1,ref2,xsizez,$
ysizez,grpxz,grpyz,graph,xz,yz,flag_log,bdata,ldata

;SIZE OF ARRAY
xx=51
yy=51



;controls the plotting of the graph
;0 is normal
;1 is log
flag_log=0

;stores the information for the graphics windows
;graph(0)=current graph
;grpah(1)=max number of graphs
a=500.
graph=intarr(a)
graph=long(graph)
graph(0)=2.
graph(1)=a

;x size of graphic array
xsize=xx-1
xsizez=xsize
xz=0
;y size of graphic array
ysize=yy-1
ysizez=ysize
yz=0


;x1 and y1 are the first coors of the zoom area
;x2 and y2 are the second coors of the zoom area
;xz and yz stores the array position for the grp window
x1=0
x2=xsizez-1
y1=0
y2=ysizez-1


;contains the array to be shown
;data=randomn(seed,xsize,ysize,normal=1)
data=indgen(xsize+1,ysize+1)
data=cos(data)
location=where(data lt 0)
data(location)=-data(location)
;openw,3,'test.dat'
;data=fltarr(128,128)
;readf,3,data
data=data*100

;set up the data variable
bdata=data

a=.1
tmp=where(data lt a)
if(tmp ne -1) then begin
  data(tmp)=a
endif
ldata=alog10(data)


;set up data window as a GUI

;creates the base
base=widget_base(title='VIEW',uvalue='base')

;wid stores all the widget address in it
wid=intarr(30)
;creates the draw window widget
wid(0)=widget_draw(base,xsize=500,ysize=500,xoffset=0,yoffset=30,/button_events,uvalue=0,retain=2)
;zoom button
wid(1)=widget_button(base,value=' ZOOM ',xoffset=507,yoffset=0,uvalue=1)
;cancel button
wid(2)=widget_button(base,value='CANCEL',xoffset=507,yoffset=30,uvalue=2)
;quit button
wid(3)=widget_button(base,value=' QUIT ',xoffset=507,yoffset=635,uvalue=3)
;reset zoom button
wid(4)=widget_button(base,value='UNZOOM',xoffset=507,yoffset=60,uvalue=4)
;label saying x from
wid(5)=widget_label(base,value='X FROM',xoffset=0,yoffset=550,uvalue=5)
;label box 
wid(6)=widget_text(base,value=string(x1),xoffset=120,yoffset=550,uvalue=6,/editable,xsize=12)
;label saying to
wid(7)=widget_label(base,value='TO',xoffset=280,yoffset=555,uvalue=7)
;label box
wid(8)=widget_text(base,value=string(x2),xoffset=330,yoffset=550,uvalue=8,/editable,xsize=12)
;label saying y from
wid(9)=widget_label(base,value='Y FROM',xoffset=0,yoffset=605,uvalue=9)
;label box 
wid(10)=widget_text(base,value=string(y1),xoffset=120,yoffset=600,uvalue=10,/editable,xsize=12)
;label saying to
wid(11)=widget_label(base,value='TO',xoffset=280,yoffset=600,uvalue=11)
;label box
wid(12)=widget_text(base,value=string(y2),xoffset=330,yoffset=600,uvalue=12,/editable,xsize=12)
;the x button to plot the axis picture
wid(13)=widget_button(base,value='  X   ',xoffset=507,yoffset=90,uvalue=13)
;the y button to plot the axis picture
wid(14)=widget_button(base,value='  y   ',xoffset=507,yoffset=120,uvalue=14)
;intensity rating
wid(15)=widget_label(base,value='Intensity :',xoffset=0,yoffset=645,uvalue=15)
wid(16)=widget_label(base,value=string(data(0,0)),xoffset=120,yoffset=645,uvalue=16)
;log button
wid(17)=widget_label(base,value=' Axis ',xoffset=507,yoffset=200,uvalue=17)
wid(18)=widget_label(base,value=' Style ',xoffset=507,yoffset=230,uvalue=18)
wid(19)=widget_button(base,value=' LOG  ',xoffset=507,yoffset=260,uvalue=19)
;button to actevate the xpalette function
wid(20)=widget_button(base,value='COLOUR',xoffset=507,yoffset=340,uvalue=20)
;title of graph
wid(21)=widget_label(base,value='Normal Plot Of Intensities',uvalue=21)
;centre of intensity display
wid(22)=widget_label(base,value='Center Of Intensity  X:',xoffset=0,yoffset=680)
wid(23)=widget_label(base,value='000.000',xoffset=300,yoffset=680)
wid(24)=widget_label(base,value='Y:',xoffset=400,yoffset=680)
wid(25)=widget_label(base,value='000.000',xoffset=450,yoffset=680)

;show the widgets on the screen
widget_control,base,/realize

;blank out widgets not used yet
;widget_control,wid(1),sensitive=0
widget_control,wid(2),sensitive=0


;draw the graph
redraw_graph

;set some parameters for the plot
;contains the coordinates of the lower left hand corner
ref1=[120,48]
;contains the coordinates of the upper right hand corner
ref2=[458,467]

;set width and height of graphical display
grpx=ref2(0)-ref1(0)
grpy=ref2(1)-ref1(1)
grpxz=grpx
grpyz=grpy

center2d

;call xmanager to govern events
xmanager,'main',base

end


;---------------------------------------------------------------
;procedures

;this procedure is called when a widget is interacted with
pro main_event,event
common vars

;get infomation on the widget that has been interacted with
widget_control,event.id,get_uvalue=ev,get_value=val

;if the quit button is pressed
if(ev eq 3) then begin
  widget_control,/reset
endif

;if the mouse button is pressed on the graphic display
if(ev eq 0) then begin

  type=event.type
  x=event.x
  y=event.y
  if(type eq 0) then begin
    x1=x
    y1=y
    ;update coordinates for zoom area
    x1=fix(float(xsizez)/float(grpxz)*float(x1-ref1(0)))
    y1=fix(float(ysizez)/float(grpyz)*float(y1-ref1(1)))
   
    ;check to bounds
    if(x1 lt 0) then x1=0
    if(x1 gt xsizez-1) then x1=xsizez-1
    if(y1 lt 0) then y1=0
    if(y1 gt ysizez-1) then y1=ysizez-1
   
    x1=x1+xz
    y1=y1+yz

    ;display the coors
    widget_control,wid(6),set_value=string(x1)
    widget_control,wid(10),set_value=string(y1)
  endif

  if(type eq 1) then begin
    x2=x
    y2=y
    ;active the zoom and cancel buttons and x and y
    widget_control,wid(1),sensitive=1
    ;widget_control,wid(2),sensitive=1
    ;update coordinates for zoom area
    x2=fix(float(xsizez)/float(grpxz)*float(x2-ref1(0)))
    y2=fix(float(ysizez)/float(grpyz)*float(y2-ref1(1)))

    ;check to bounds
    if(x2 lt 0) then x2=0
    if(x2 gt xsizez-1) then x2=xsizez-1
    if(y2 lt 0) then y2=0
    if(y2 gt ysizez-1) then y2=ysizez-1
    
    x2=x2+xz
    y2=y2+yz
 
    ;display the coors
    widget_control,wid(8),set_value=string(x2)
    widget_control,wid(12),set_value=string(y2)  

    ;diplay the intensity
    widget_control,wid(16),set_value=string(bdata(x1,y1))
  
  endif  


   ;error check one
   diff=(x1>x2)-(x1<x2)
   diff1=(y1>y2)-(y1<y2)
  if((diff lt 2) or (diff1 lt 2)) then begin
    widget_control,wid(1),sensitive=0
  endif else begin
    widget_control,wid(1),sensitive=1
  endelse

;print,'mouse coors',x1,x2,y1,y2

endif

;if the zoom button is pressed
if(ev eq 1) then begin
  zoom
endif



;if the UNZOOM button is pressed then reset the zoom level
if(ev eq 4) then begin
  ;dataz=data
  xsizez=xsize
  ysizez=ysize

  xz=0
  yz=0

  x1=0
  x2=xsizez-1
  y1=0
  y2=ysizez-1

  ;set the coors correctly
  widget_control,wid(6),set_value=string(x1)
  widget_control,wid(8),set_value=string(x2)
  widget_control,wid(10),set_value=string(y1) 
  widget_control,wid(12),set_value=string(y2)

  redraw_graph

  center2d
  
endif


;if the x axis button is pressed
if(ev eq 13) then begin
  axis_plot,1
endif

;if the y axis button is pressed
if(ev eq 14) then begin
  axis_plot,2
endif


;if text was entered into the x and y positions
if((ev eq 6)or(ev eq 8)or(ev eq 10)or(ev eq 12)) then begin
  if(ev eq 6) then begin
    x1=fix(val)
    x1=x1(0)
  endif
  if(ev eq 8) then begin
    x2=fix(val)
    x2=x2(0)
  endif
  if(ev eq 10) then begin
    y1=fix(val)
    y1=y1(0)
  endif
  if(ev eq 12) then begin
    y2=fix(val)
    y2=y2(0)
  endif

  ;error checking
  diff=(x1>x2)-(x1<x2)
  diff1=(y1>y2)-(y1<y2)
  if((diff lt 2) or (diff1 lt 2)) then begin
    widget_control,wid(1),sensitive=0
  endif else begin
    widget_control,wid(1),sensitive=1
  endelse
    
endif


;if the log button is pressed
if(ev eq 19) then begin
  flag_log=1-flag_log
  if(flag_log eq 0) then begin
    widget_control,wid(19),set_value=' LOG '
    widget_control,wid(21),set_value='Normal Plot Of Intensities'
    data=bdata
    sx1=x1
    sx2=x2
    sy1=y1
    sy2=y2
    x1=xz
    x2=xz+xsizez
    y1=yz
    y2=yz+ysizez
    zoom
    x1=sx1
    x2=sx2
    y1=sy1
    y2=sy2
  endif
  if(flag_log eq 1) then begin
    widget_control,wid(19),set_value='NORMAL' 
    widget_control,wid(21),set_value='LOG Plot Of Intensities'
    data=ldata
    sx1=x1
    sx2=x2
    sy1=y1
    sy2=y2
    x1=xz
    x2=xz+xsizez
    y1=yz
    y2=yz+ysizez
    zoom
    x1=sx1
    x2=sx2
    y1=sy1
    y2=sy2
  endif
endif

;the button 'colour' was pressed
if(ev eq 20) then begin
  xloadct
endif

end

;redraws the graphical display with new array coors
pro redraw_graph
common vars

;setup up colours for the plot
m1=max(data)
m2=min(data)
m3=total(data)

;set up lighting for contour plot
m=9
l1=m2+indgen(m)*(float(m1-m2)/float(m+1))

;set current graphics window
widget_control,wid(0),get_value=gval
wset,gval

;loadct,5
;tvscl,dist(300)
;stretch,0,70

;tek_color

;draw the plot
;print,'graph stuff:',xz,xz+xsizez,yz,yz+ysizez
contour,data(xz:xz+xsizez,yz:yz+ysizez),levels=l1,c_colors=indgen(10)*25,$
xstyle=5,ystyle=5

dumb=[[0,0],[0,0]]
contour,dumb,/noerase,xrange=[xz,xz+xsizez],yrange=[yz,yz+ysizez],/xstyle,/ystyle

return
end


;draw a plot if either the x or y buttons are pressed
;bop
pro axis_plot,type
common vars

;if type=1 its a y style
;if type=2 its a y style
if(type eq 1) then string='X'
if(type eq 2) then string='Y'

graph(0)=graph(0)+1

if(graph(0) gt graph(1)) then graph(0)=2
graph(graph(0))=widget_base(title='OUTPUT'+string(graph(0)-2)+'         Type: '+string)

;make the draw window
draw=widget_draw(graph(graph(0)),xsize=600,ysize=400,retain=2)
;put a quit button on the axis_plots
quit=widget_button(graph(graph(0)),xoffset=0,yoffset=460,xsize=600,value='QUIT',$
uvalue=1000+graph(0))
peak=widget_label(graph(graph(0)),xoffset=5,yoffset=400,value='',xsize=600,ysize=30)
centerr=widget_label(graph(graph(0)),xoffset=0,yoffset=430,value='',xsize=600,ysize=30)

widget_control,/realize,graph(graph(0))

xmanager,'axis_plot',graph(graph(0))

;--------------------------------------for x plot
;for the x axis
if(type eq 1) then begin
;plot the graph
;print,xsizez,xz,yz,y1
xaxis=indgen(xsizez+1)
xaxis=xaxis+xz

yaxis=fltarr(xsizez+1)

yaxislog=fltarr(xsizez+1)

for u=xz,xz+xsizez do begin
  tmp=0.
  tmplog=0.
  for i=yz,yz+ysizez do begin
    tmp=tmp+data(u,i)
    tmplog=tmplog+bdata(u,i)
  endfor
  tmp=tmp/ysizez
  tmplog=tmplog/ysizez
  yaxis(u-xz)=tmp
  yaxislog(u-xz)=tmplog
endfor    

plot,xaxis,yaxis,/xstyle,/ystyle,$
title='Type: '+strtrim(string,2)+'  x1:'+strtrim(xz,2)+'  y1:'+strtrim(yz,2)+'  x2:'+strtrim(xz+xsizez,2)+'   y2:'+strtrim(yz+ysizez,2),$
xtitle='X Pixel',ytitle='Mean Intensity Over Y Range'

;calculate the centre of mass
err=fltarr(xsizez+1)
tmp=0.
tmp1=0.
for u=xz,xz+xsizez do begin
  tmp=tmp+u*yaxis(u-xz)
  tmp1=tmp1+yaxis(u-xz)
  if(flag_log eq 0) then begin
    err(u-xz)=1/sqrt(yaxis(u-xz))*yaxis(u-xz)/ysizez
  endif else begin
    err(u-xz)=1/sqrt(yaxislog(u-xz))*yaxislog(u-xz)/ysizez
  endelse
endfor
center=tmp/tmp1
;print,'X Center=',center
oplot,[center,center],[min(yaxis),max(yaxis)],thick=3

if(flag_log eq 1) then begin
  below=yaxislog-(err/2)
  above=yaxislog+(err/2)
  below=alog10(below)
  above=alog10(above)
  yaxislog=alog10(yaxislog)
endif

;plot error bars
if(flag_log eq 0) then begin
  err=err/2
  errplot,xaxis,yaxis-err,yaxis+err
endif else begin
  lenabove=above-yaxislog
  lenbelow=yaxislog-below
  errplot,xaxis,yaxis-lenbelow,yaxis+lenabove
endelse

;peak intensity
max=max(yaxis)
location=where(yaxis eq max)
location=location(0)+xz
widget_control,peak,set_value='The Peak Intensity is '+strtrim(string(max),2)+' at '+strtrim(string(location),2)

;display cetre of intensity
widget_control,centerr,set_value='The Centre Of Intensity is at  '+strtrim(string(center),2)

endif


;--------------------------------------------for y plot
;for the y axis
if(type eq 2) then begin
;plot the graph
xaxis=indgen(ysizez+1)
xaxis=xaxis+yz

yaxis=fltarr(ysizez+1)

yaxislog=fltarr(ysizez+1)

for u=yz,yz+ysizez do begin
  tmp=0.
  tmplog=0.
  for i=xz,xz+xsizez do begin
    tmp=tmp+data(i,u)
    tmplog=tmplog+bdata(i,u)
  endfor
  tmp=tmp/xsizez
  tmplog=tmplog/xsizez
  yaxis(u-yz)=tmp
  yaxislog(u-yz)=tmplog
endfor    

plot,xaxis,yaxis,/xstyle,/ystyle,$
title='Type: '+strtrim(string,2)+'  x1:'+strtrim(xz,2)+'  y2:'+strtrim(yz,2)+'  x2:'+strtrim(xz+xsizez,2)+'   y2:'+strtrim(yz+ysizez,2),$
xtitle='Y Pixel',ytitle='Mean Intensity Over X Range'

;calculate the centre of mass
err=fltarr(ysizez+1)
tmp=0.
tmp1=0.
for u=yz,yz+ysizez do begin
  tmp=tmp+u*yaxis(u-yz)
  tmp1=tmp1+yaxis(u-yz)
  if(flag_log eq 0) then begin
    err(u-yz)=1/sqrt(yaxis(u-yz))*yaxis(u-yz)/xsizez
  endif else begin
    err(u-yz)=1/sqrt(yaxislog(u-yz))*yaxislog(u-yz)/xsizez
  endelse
endfor  
center=tmp/tmp1
;print,'Y Center=',center
oplot,[center,center],[min(yaxis),max(yaxis)],thick=3

if(flag_log eq 1) then begin
  below=yaxislog-(err/2)
  above=yaxislog+(err/2)
  below=alog10(below)
  above=alog10(above)
  yaxislog=alog10(yaxislog)
endif

;plot errorbars
if(flag_log eq 0) then begin
  err=err/2
  errplot,xaxis,yaxis-err,yaxis+err
endif else begin
  lenabove=above-yaxislog
  lenbelow=yaxislog-below
  errplot,xaxis,yaxis-lenbelow,yaxis+lenabove
endelse

;peak intensity
max=max(yaxis)
location=where(yaxis eq max)
location=location(0)+yz
widget_control,peak,set_value='The Peak Intensity is '+strtrim(string(max),2)+' at '+strtrim(string(location),2)

;display cetre of intensity
widget_control,centerr,set_value='The Centre Of Intensity is at  '+strtrim(string(center),2)

endif


;xmanager,'axis_plot',graph(graph(0))

return
end

;procedure to deal with events in from the axis_plot window
pro axis_plot_event,event

common vars

widget_control,event.id,get_uvalue=ev

widget_control,graph(ev-1000),/destroy

return
end


; calculates the centre of mass in the 2d contour plot
pro center2d
common vars

;works with data, and the limits are given by xz,yz, and xsizez, ysizez

;find the x mean position
x=0.
print,'zx,zy',xz,yz
for u=0,ysizez do begin
  tmp=0.
  tmp1=0.
  for i=0,xsizez do begin
    tmp=tmp+i*data(i+xz,u+yz)
    tmp1=tmp1+data(i+xz,u+yz)
  endfor
  x=x+(tmp/tmp1)
endfor
x=x/ysizez
;print,'x mean =',x+xz
widget_control,wid(23),set_value=string(x+xz)

;find the y mean position
y=0.
for u=0,xsizez do begin
  tmp=0.
  tmp1=0.
  for i=0,ysizez do begin
    tmp=tmp+i*data(u+xz,i+yz)
    tmp1=tmp1+data(u+xz,i+yz)
  endfor
  y=y+(tmp/tmp1)
endfor
y=y/xsizez
;print,'y mean =',y+yz     
widget_control,wid(25),set_value=string(y+yz)  


;plot the centre of mass

;set the correct graph
widget_control,wid(0),get_value=gval
wset,gval

;convert array coors to screen coors
;if((xsizez eq xsize)and(ysizez eq ysize)) then begin
;  x=float(x)*float(grpxz)/float(xsizez)+float(ref1(0))
;  y=float(y)*float(grpyz)/float(ysizez)+float(ref1(1))

;  xyouts,x/500,y/500,'O',/normal

;endif

return
end



;zoom procedure
pro zoom

  common vars

  ;print,'coors',x1,x2,y1,y2

  ;find new array coordinates
  smallx=(x1<x2)
  bigx=(x1>x2)
  smally=(y1<y2)
  bigy=(y1>y2)
  ;xa=fix(float(xsizez)/float(grpxz)*float(smallx-ref1(0)))
  ;ya=fix(float(ysizez)/float(grpyz)*float(smally-ref1(1)))
  ;xb=fix(float(xsizez)/float(grpxz)*float(bigx-ref1(0)))
  ;yb=fix(float(ysizez)/float(grpyz)*float(bigy-ref1(1)))
  xa=smallx
  xb=bigx
  ya=smally
  yb=bigy

  ;dataz=data(xa:xb,ya:yb)

  xsizez=xb-xa
  ysizez=yb-ya

  xz=smallx
  yz=smally

  redraw_graph

  widget_control,wid(1),sensitive=0
  widget_control,wid(2),sensitive=0
  widget_control,wid(2),sensitive=0

  center2d

  

;print,'zoom corrs',xz,yz,xsizez,ysizez

return
end
