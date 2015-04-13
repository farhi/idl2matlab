;*************************************************************************************************
;*               	         	    Main                                                 *
;*					  Procedure	                                         *
;*						                                                 *
;*************************************************************************************************

pro gres

;-------------------------------------------------
; Define All Global variables via common statement
;-------------------------------------------------
common initialvals,minres,maxres,r1,r2,q1,q2,base
common graph,m,c,info,k,lamda,r,graph,grp,draw,draw2
common vars,d,dis,w,phi,x,phil,bd,bw,bdis,global_flag,slit_chopper_dist,chopper_width
common qs,qmin,qmax,numblocks,currentblock,minq,maxq,sres,sample_angle,qrangeb,wid,perover,widmax,$
banner,slit_lock,qflag,rr1,rr2,ll1,ll2,s2v,s3v,sample_lenght,foot_print,slit_sample_dist,angres,sub,submenu,$
restart_flag,save_string,dr,disr,wr,slit,chopper_to_sample
common slitstuff,slit_separation
common error,errbase,errsnum


;----------------------------------------------------------
;Hitting the NewQ Button In the Data Window brings you here
;----------------------------------------------------------
newpass:


;--------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------
;Variables (Global and in main program only)
;
;d[low limit,high limit]      :Stores the limits of the Distance between the choppers (m) 
;dis[low limit,high limit]    :Stores the limits of the Chopper to sample Distance (m)
;w[low limit,high limit]      :Stores the limits of the Angular Velocity of the Chopper (rad/s)
;lamda[low limit,high limit]  :Stores the limits of the Wavelenght of the beam (amstrongs)
;phi[low limit,high limit]    :Stores the limits of the Chopper Phase Angle (Degrees)
;phil(x)                      :Stores the chopper angle the (x-1)th block (Degrees)
;r                            :Stores the radius of the chopper (m)
;perover                      :Stores the percentage of overlap of the blocks (decimal)
;maxres[low limit,high limit] :Stores the limits of the time resolution for the maximum point
;minres[low limit,high limit] :Stores the limits of the time resolution for the minimum point
;slit_separation              :Stores the value of the slit separation (m)
;slit_chopper_dist            :Stores the value of the distance from s2 to the chopper (m)
;chopper_width                :Stores the value of the effective sample lenght for chopper (m)
;sample_lenght                :Stores the value of the sample lenght (m)
;slit_sample_dist             :Stores the distance from s3 to the sample (m)
;bd, bdis, bw                 :Holds Default values for d,dis,w 
;sres(x,y)                    :sres hold the current value of the time resolution sliders
;      			      :		x=0 indexs the min slider, x=1 indexs the max slider
;			      :         y references the current block. 0=block1,1=block2,2=block3
;sample_angle(x)              :Stores the sample angle for the x'th block. (0=1st block etc) (degrees)
;global_flag                  :Is set to one when newq is pressed in data window otherwise=0
;errbase()                    :Stores the widget references for the message windows
;errsnum                      :Used as an index for errbase, cyles from 0 to 100
;                             :It increments by one everytime a message window appears
;slit_lock                    :Contains the status of the slit lock button, 0=off, 1=on
;qflag                        :Is set to 1 in intial setup window if quit is pressed
;s2v(x), s3v(x)               :Stores the slit opening distance of s2, s3 respect.
;                             :x stores the block refernce, x=0 is 1st block and so on
;graph()                      :Stores widget references for the illumination graph window
;                             :graph(3)=1 when window is open, else it is zero
;angres(x)                    :Stores the angular resolution for block x+1. x=0,1,2.
;submenu()                    :Stores widget references to the pop menu in the inital setup window
;			      :submenu(0)=1 when it is open, otherwise = 0
;restart_flag                 :Contains 1 if the perover variable has changed
;save_string                  :Contains the name of the save file
;                             :On saving the block number is concatenated with it and '.dat'
;k                            :stores the value of h/mass of neutron. In units of 1e10.
;numblocks                    :Stores the number of blocks, can take values of 1,2,3
;currentblock                 :Stores the current block, takes values of 0,1,2
;wid                          :Stores the widget references for the inital setup window
;qrangeb                      :Stores Maximum wavelenght/Minimum Wavelenght
;minq(x)                      :Stores the minimum q value for block (x+1), x=0,1,2. (amstrongs)^-1
;maxq(x)                      :Stores the maximum q value for block (x+1), x=0,1,2. (amstrongs)^-1
;widmax                       :Stores how many widgets are in intial setup window
;qmin, qmax                   :Stores q minimum and q maximum of current block
;val                          :many uses depends on context. All purpose variable. Not GLOBAL.
;info(),draw,draw2            :stores widget references, to data window, gfxs win1, gfxs win2
;gbase,frbase,pbase,dbase     :Stores base references
;r1,r2,q1,q2                  :tempory values. depends on context. *
;m                            :Contains the gradient of the time resolution graph, for the current block
;c                            :Contains the intercept of the time resolution grpah, for the current block
;foot_print                   :Holds the 'foot print' of the illumination of the slit (m)
;rr1,rr2,ll1,ll2              :Contains the coordinates for the illumination of the sample (m)
;x                            :Holds the value of delta x for the current block    
;banner                       :Holds a status which tells the program which buttons to deactivate
;grp                          :Hold the base reference to the illumination window
;sub                          :Contains the reference to the parameters window
;sres(min/max,block)          :Contains the slider resolutions. min=0,max=1, block=0,1,2 
;dr, wr, disr                 :Contains the saved values for d, w, and dis respectivly  
;slit(2)                      :Contains the minimum, maximum openings for the slits (in meters)
;chopper_to_sample            :Contains the chopper to sample lenght
;                   
;--------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------

dr=fltarr(3)
wr=fltarr(3)
disr=fltarr(3)
d=[0.,0.]
dis=[0.,0.]
w=[0.,0.]
lamda=[0.,0.]
phi=[0.,0.]
r=0.
perover=0.
maxres=[0.,0.]
minres=[0.,0.]
sres=fltarr(2,3)
sample_angle=fltarr(3)
global_flag=0
slit_lock=0
qflag=0
s2v=fltarr(3)
s3v=fltarr(3)
graph=intarr(10)
graph=long(graph)
angres=fltarr(3)
submenu=intarr(30)
submenu=long(submenu)
restart_flag=0
save_string='block'
minq=fltarr(3)
maxq=fltarr(3)
wid=intarr(60)
info=intarr(50)
phil=fltarr(3)
chopper_to_sample=0.

;*************************************************
;Slit openings (m)
;*************************************************
slit=[0.,0.01]


;-------------------------------------------------
;default chopper angles
;-------------------------------------------------
phil(0)=.144688
;phil(1)=1.47802
;phil(2)=0

;-------------------------------------------------
;allows upto 100 messages at once
;-------------------------------------------------
errbase=intarr(100)
errbase=long(errbase)

;-------------------------------------------------
;currently no messages on screen
;-------------------------------------------------
errsnum=0

;-------------------------------------------------
;Constants
;-------------------------------------------------
k=3956.0346035 
;-------------------------------------------------
;Read in data from the file ranges.dat
;-------------------------------------------------
openr,1,'ranges.dat'
readf,1,chopper_to_sample,d,dis,w,lamda,phi,r,perover,minres,maxres,slit_separation,slit_chopper_dist,chopper_width,$
sample_lenght,slit_sample_dist
close,1
bd=d & bdis=dis & bw=w
qrangeb=lamda(1)/lamda(0)
;*************************************************************************************************
;*                                      Initial Setup Window
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*************************************************************************************************

;-------------------------------------------------
;Default Data for intial setup window
;-------------------------------------------------
numblocks=2
currentblock=0
for u=0,2 do begin
  s2v(u)=.005
  s3v(u)=.005
endfor
minq(0)=0.005
maxq(0)=.05
minq(1)=.0455
maxq(1)=.455
sample_angle(0)=asin(lamda(1)*minq(0)/4/!pi)*180/!pi
sample_angle(1)=asin(lamda(1)*minq(1)/4/!pi)*180/!pi


;-------------------------------------------------
;Sets up the interface with default values
;See documentation for more information
;-------------------------------------------------
base=widget_base(title='Initial Setup, Press Next When Finished',uvalue='base')
wid(0)=widget_label(base,value='q min :',xoffset=0,yoffset=5)
wid(1)=widget_text(base,value=string(.005),xoffset=100,yoffset=0,uvalue=41,/editable)
wid(2)=widget_label(base,value='q max :',xoffset=380,yoffset=5)
wid(3)=widget_text(base,value=string(.5),xoffset=480,yoffset=0,uvalue=43,/editable)

wid(4)=widget_button(base,value="",xoffset=0,yoffset=50,xsize=870,ysize=7)

wid(5)=widget_label(base,value='- For Current Block -',xoffset=0,yoffset=70)

wid(6)=widget_label(base,value='q min :',xoffset=0,yoffset=105)
wid(7)=widget_text(base,value=string(.005),xoffset=100,yoffset=100,uvalue=47)
wid(8)=widget_label(base,value='q max :',xoffset=380,yoffset=100)
wid(9)=widget_text(base,value=string(.05),xoffset=480,yoffset=105,uvalue=49)

wid(10)=widget_label(base,value='l min :',xoffset=380,yoffset=150)
wid(11)=widget_label(base,value=string(lamda(0)),xoffset=480,yoffset=150)
wid(12)=widget_label(base,value='l max :',xoffset=0,yoffset=150)
wid(13)=widget_label(base,value=string(lamda(1)),xoffset=100,yoffset=150)

wid(14)=widget_button(base,value='ACTIVE ',xoffset=775,yoffset=72,uvalue=54)
wid(15)=widget_button(base,value='Block 2',xoffset=775,yoffset=102,uvalue=55)
wid(16)=widget_button(base,value='Block 3',xoffset=775,yoffset=132,uvalue=56)

wid(17)=widget_button(base,value='Quit',xoffset=792,yoffset=10,uvalue=57)

wid(18)=widget_label(base,value='Current Block is :',xoffset=380,yoffset=70)
wid(19)=widget_label(base,value=string(currentblock+1),xoffset=530,yoffset=71,uvalue=59)

wid(20)=widget_button(base,value="",xoffset=0,yoffset=180,xsize=870,ysize=5)

wid(21)=widget_label(base,value='S2 :',xoffset=0,yoffset=206)
wid(22)=widget_slider(base,minimum=slit(0)*1e7,maximum=slit(1)*1e7,value=slit(0)*1e7,uvalue=62,$
xoffset=70,yoffset=210,xsize=200,/suppress_value,/drag)
wid(23)=widget_text(base,value=string(slit(0)),uvalue=63,xoffset=300,yoffset=200,/editable)

wid(24)=widget_label(base,value='S3 :',xoffset=0,yoffset=266)
wid(25)=widget_slider(base,minimum=slit(0)*1e7,maximum=slit(1)*1e7,value=slit(0)*1e7,uvalue=65,$
xoffset=70,yoffset=270,xsize=200,/suppress_value,/drag)
wid(26)=widget_text(base,value=string(slit(0)),uvalue=66,xoffset=300,yoffset=260,/editable)

wid(35)=widget_button(base,value="OFF",uvalue=75,xoffset=140,yoffset=235,$
xsize=60,ysize=30)

wid(27)=widget_label(base,value='Sample Angle  :',xoffset=570,yoffset=205)
wid(28)=widget_label(base,value=string(.683934),uvalue=68,xoffset=710,yoffset=205)

wid(29)=widget_label(base,value='Sample Length :',xoffset=570,yoffset=245)
wid(30)=widget_label(base,value=string(sample_lenght),xoffset=710,yoffset=245)

wid(31)=widget_label(base,value='Slit-Sam Dist :',xoffset=570,yoffset=285)
wid(32)=widget_label(base,value=string(slit_sample_dist),xoffset=710,yoffset=285)

wid(33)=widget_label(base,value='Slit Separtn  :',xoffset=570,yoffset=325)
wid(34)=widget_label(base,value=string(slit_separation),xoffset=710,yoffset=325)

wid(36)=widget_button(base,value='Next',uvalue=76,xoffset=305,yoffset=305,xsize=250,ysize=45)

wid(37)=widget_label(base,value='Illumination :',xoffset=0,yoffset=295)
wid(38)=widget_label(base,value='Over',xoffset=210,yoffset=295,uvalue=78,xsize=70)

wid(39)=widget_button(base,value="",xoffset=0,yoffset=360,xsize=870,ysize=7)

wid(40)=widget_label(base,value='Block 1',xoffset=80,yoffset=380)
wid(41)=widget_label(base,value='Block 2',xoffset=380,yoffset=380)
wid(42)=widget_label(base,value='Block 3',xoffset=680,yoffset=380)

wid(43)=widget_label(base,value='Ang',xoffset=0,yoffset=420)
wid(44)=widget_label(base,value='Res',xoffset=0,yoffset=440)

wid(45)=widget_label(base,value='   0.119677',xoffset=35,yoffset=430,xsize=170)
wid(46)=widget_label(base,value='  0.0131257',xoffset=335,yoffset=430,xsize=170)
wid(47)=widget_label(base,value='Ready',xoffset=635,yoffset=430,xsize=170)
wid(48)=widget_button(base,value='Graph',xoffset=22,yoffset=323,uvalue=88)

wid(49)=widget_button(base,value='-',xoffset=50,yoffset=375,uvalue=89)
wid(50)=widget_button(base,value='-',xoffset=350,yoffset=375,uvalue=90)
wid(51)=widget_button(base,value='-',xoffset=650,yoffset=375,uvalue=91)

wid(52)=widget_label(base,value='Lock',xoffset=80,yoffset=238)

wid(53)=widget_label(base,value='F',xoffset=110,yoffset=317)
wid(54)=widget_label(base,value='%',xoffset=110,yoffset=336)
wid(55)=widget_label(base,value=string(0.),xoffset=120,yoffset=317,uvalue=95)
wid(56)=widget_label(base,value=string(0.),xoffset=120,yoffset=336,uvalue=96)

wid(57)=widget_button(base,value='>',xoffset=840,yoffset=395,uvalue=97)

widmax=57


;-------------------------------------------------
;to SEE the widgets one must call this
;-------------------------------------------------
widget_control,/realize,base

;-------------------------------------------------
;Make the separators inactive
;-------------------------------------------------
widget_control,wid(4),sensitive=0
widget_control,wid(20),sensitive=0
widget_control,wid(39),sensitive=0
widget_control,wid(16),sensitive=0

;-------------------------------------------------
;set footprint and illumination values
;-------------------------------------------------
h=bob(.005,.005,sample_angle(currentblock),sample_lenght,slit_sample_dist)
update_ftper

;----------------------------------------------------------------------
;lets the interaction of mouse and user start and registers button hits
;it calls the proedure base_event in the case of an interaction
;----------------------------------------------------------------------
xmanager,'base',base


;-------------------------------------------------
;If the quit button is pressed exit program
;-------------------------------------------------
if qflag eq 1 then goto,pass





;*************************************************************************************************
;*                                         Data Window
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*************************************************************************************************

;-------------------------------------------------
;Allocate the 'bases' in memory for the widgets
;-------------------------------------------------
gbase=widget_base(title='Graphical Output')
grbase=widget_base(title='Graphival Output 2')
pbase=widget_base(title='Ranges',/column)
dbase=widget_base(title='Data Window')

;-------------------------------------------------
;Draw two gfxs widgets for the graphs
;-------------------------------------------------
draw=widget_draw(gbase,xsize=400,ysize=700,yoffset=0,xoffset=0,retain=2)
;draw2=widget_draw(grbase,xsize=400,ysize=350,yoffset=0,xoffset=0,retain=2)

;-------------------------------------------------
;Set default Values
;-------------------------------------------------
currentblock=0
sres(0,0)=0.01*1e7 & sres(1,0)=0.02*1e7
sres(0,1)=0.01*1e7 & sres(1,1)=0.05*1e7
sres(0,2)=0.01*1e7 & sres(1,2)=0.02*1e7
qmin=minq(currentblock)
qmax=maxq(currentblock)
;prelimanry guess as limits depend upon, w and d also. Works because limits dont change much!
val=lims(sres(0,currentblock)*1e-7,sres(1,currentblock)*1e-7,qmin,qmax)

;----------------------------------------
;Default slider values for w, d, and dis 
;----------------------------------------
disr(0)=6.75 & dr(0)=.06 & wr(0)=122.7483
disr(1)=6.75 & dr(1)=.0375 & wr(1)=122.7483
disr(2)=6.75 & dr(2)=.0375 & wr(2)=122.7483

;--------------------------------------------------------------
;Setup the widgets for the bases, see documentation for details
;--------------------------------------------------------------
info(0)=widget_label(pbase,value='TOF Range : '+string(dis(0))+'  '+string(dis(1)),/align_left)
info(1)=widget_label(pbase,value='d Range    : '+string(d(0))+'  '+string(d(1)),/align_left)
info(2)=widget_label(pbase,value='Rpms Range : '+string(w(1)*30/!pi)+'  '+string(w(0)*30/!pi),/align_left)
info(3)=widget_label(pbase,value='Chopper Ang: '+string(phil(0))+'  '+string(phil(0)),/align_left)
;-------------------------------------------------
;delta x update
;-------------------------------------------------
info(4)=widget_label(pbase,value='dtx/T      : '+string(0.)+'  '+string(0.),/align_left)

info(10)=widget_label(dbase,value='TOF Distance',xoffset=0,yoffset=0)
info(11)=widget_slider(dbase,minimum=dis(0)*1e7,maximum=dis(1)*1e7,uvalue=9,/drag,/suppress_value,$
xoffset=0,yoffset=30,xsize=257,value=disr(0)*1e7)
info(12)=widget_text(dbase,value=string(disr(0)),uvalue=10,xoffset=0,yoffset=50,/editable)

info(13)=widget_label(dbase,value='d',xoffset=0,yoffset=85)
info(14)=widget_slider(dbase,minimum=d(0)*1e7,maximum=d(1)*1e7,uvalue=11,/drag,/suppress_value,$
xoffset=0,yoffset=115,xsize=257,value=dr(0)*1e7)
info(15)=widget_text(dbase,value=string(dr(0)),uvalue=12,xoffset=0,yoffset=140,/editable)

info(16)=widget_label(dbase,value='Rpms',xoffset=0,yoffset=175)
info(17)=widget_slider(dbase,minimum=w(0)*1e7,maximum=w(1)*1e7,uvalue=13,/drag,/suppress_value,$
xoffset=0,yoffset=205,xsize=257,value=wr(0)*1e7)
info(18)=widget_text(dbase,value=string(wr(0)*30/!pi),uvalue=14,xoffset=0,yoffset=230,/editable)

info(19)=widget_label(dbase,value='Chopper Angle:'+string(.144688),xoffset=0,yoffset=275)
;-------------------------------------------------
;set delta x
;-------------------------------------------------
info(20)=widget_label(dbase,value='Transmission :',xoffset=0,yoffset=305)
info(45)=widget_label(dbase,value='    --------',xoffset=120,yoffset=305,xsize=250)
info(30)=widget_label(dbase,value='Sample Angle :'+string(sample_angle(currentblock)),yoffset=330,xoffset=0)

;resolution sliders
info(21)=widget_label(dbase,value='Minimum resolution',xoffset=320,yoffset=0)
info(22)=widget_slider(dbase,minimum=minres(0)*1e7,maximum=minres(1)*1e7,uvalue=15,/suppress_value,$
value=sres(0,currentblock),xoffset=320,yoffset=30,xsize=258)
info(23)=widget_text(dbase,value=string(sres(0,currentblock)*1e-7),/editable,xoffset=320,yoffset=55,uvalue=16)

info(24)=widget_label(dbase,value='Maximum resolution',xoffset=320,yoffset=130)
info(25)=widget_slider(dbase,minimum=maxres(0)*1e7,maximum=maxres(1)*1e7,uvalue=17,/suppress_value,$
value=sres(1,currentblock),xoffset=320,yoffset=160,xsize=258)
info(26)=widget_text(dbase,value=string(sres(1,currentblock)*1e-7),/editable,xoffset=320,yoffset=185,uvalue=18)

info(27)=widget_button(dbase,value='ACTIVE ',uvalue=19,xoffset=400,yoffset=230)
info(28)=widget_button(dbase,value='Block 2',uvalue=20,xoffset=400,yoffset=265)
info(29)=widget_button(dbase,value='Block 3',uvalue=21,xoffset=400,yoffset=300)
info(31)=widget_label(dbase,value=string(currentblock+1),xoffset=450,yoffset=310)

info(8)=widget_button(dbase,value='QUIT',uvalue=7,xoffset=510,yoffset=265)

info(38)=widget_button(dbase,value='NewQ',uvalue=26,xoffset=510,yoffset=230)

info(43)=widget_button(dbase,value="",xoffset=0,yoffset=355,xsize=600,ysize=5)

info(40)=widget_label(dbase,value='Angular Resolution Block 1 :'+string(angres(0)),yoffset=365,xoffset=0)
info(41)=widget_label(dbase,value='Angular Resolution Block 2 :'+string(angres(1)),yoffset=385,xoffset=0)
info(42)=widget_label(dbase,value='Angular Resolution Block 3 :'+string(angres(2)),yoffset=405,xoffset=0)

info(44)=widget_button(dbase,value='Save Blocks',yoffset=450,xoffset=0,uvalue=30,xsize=600)

;max=45 so use 46 next

;-------------------------------------------------
;show the widgets on screen
;-------------------------------------------------
widget_control,/realize,pbase
widget_control,/realize,gbase
widget_control,/realize,dbase
;widget_control,/realize,grbase

;-------------------------------------------------
;De-activate unwanted select block buttons
;-------------------------------------------------
if(numblocks lt 3) then begin
  widget_control,info(29),sensitive=0
endif
if(numblocks lt 2) then begin
 widget_control,info(29),sensitive=0
 widget_control,info(28),sensitive=0
endif

;-------------------------------------------------
;blank out separators
;-------------------------------------------------
widget_control,info(43),sensitive=1

;-------------------------------------------------
;draw plot in graphics window
;-------------------------------------------------
;drawplot

;--------------------------------------------------------
;lets roll baby, calls main_event upon a user interaction
;--------------------------------------------------------
xmanager,'main',dbase

;-------------------------------------------------
;Restart?
;-------------------------------------------------
if global_flag then goto,newpass

pass:
end

;*************************************************************************************************
;*               	         	    End Of                                               *
;*					  Main Program	                                         *
;*						                                                 *
;*************************************************************************************************











;*************************************************************************************************
;* 				Procedure main_event
;*
;*This proedure is called by xmanager when a user interaction has taken place. eq clicking a
;*button. 
;*
;*Variables (local):
;*
;* ev       :Contains the user value of the activated widget
;* val      :Contains the val of the widget itself that the user has activated
;* tmp1     :Tempory variable, use depends on context
;* err      :Tempory variable, use depends on context
;* a        :Tempory variable, use depends on context
;* sresb    :Contains the tempory modified time resolution
;*
;*(This is for the data window)  (Prgs note : delta x here)
;*************************************************************************************************


pro main_event,event
common graph
common qs
common vars
common slitstuff
err=0.
widget_control,event.id,get_uvalue=ev,get_value=val


;-------------------------------------------------
;pressed quit button
;-------------------------------------------------
if ev eq 7 then widget_control,/reset

;-------------------------------------------------
;newq button is pressed
;-------------------------------------------------
if ev eq 26 then begin
  widget_control,/reset
  global_flag=1
endif

;-------------------------------------------------
;slide sliders (dis,d,w)
;-------------------------------------------------
if(ev eq 9)or(ev eq 11)or(ev eq 13)or(ev eq 10)or(ev eq 12)or(ev eq 14) then begin
 if ev eq 9 then begin
 
   widget_control,info(12),set_value=string(val*1e-7)
   widget_control,info(14),set_value=c*val
   widget_control,info(15),set_value=string(c*val*1e-7)
   widget_control,info(17),set_value=2*!pi*k/val/lamda(1)*1e14
   widget_control,info(18),set_value=string(2*!pi*k/val/lamda(1)*1e14*1e-7*30/!pi)

   tmp1=val
   tmp2=c*val
   tmp3=2*!pi*k/val/lamda(1)*1e14

   err=refresh() 
 endif
  if ev eq 10 then begin
    val=long(val*1e7)
    widget_control,info(11),set_value=val(0)    
    widget_control,info(14),set_value=c*val(0)
    widget_control,info(15),set_value=string(c*val(0)*1e-7)
    widget_control,info(17),set_value=2*!pi*k/val(0)/lamda(1)*1e14
    widget_control,info(18),set_value=string(2*!pi*k/val(0)/lamda(1)*30/!pi*1e14*1e-7)
   
    tmp1=val(0)
    tmp2=c*val(0)
    tmp3=2*!pi*k/val(0)/lamda(1)*1e14

    ;redraw the window
    err=refresh()

  endif   
 if ev eq 11 then begin
   widget_control,info(15),set_value=string(val*1e-7)
   widget_control,info(11),set_value=val/c
   widget_control,info(12),set_value=string(val/c*1e-7)
   widget_control,info(17),set_value=2*!pi*k/(val/c)/lamda(1)*1e14
   widget_control,info(18),set_value=string(2*!pi*k/(val/c)/lamda(1)*1e14*1e-7*30/!pi)
   tmp1=val/c
   tmp2=val
   tmp3=2*!pi*k/(val/c)/lamda(1)*1e14

   ;redraw the window
   err=refresh()

 endif
 if ev eq 12 then begin
   val=long(val*1e7) 
   widget_control,info(14),set_value=val(0)
   widget_control,info(11),set_value=val(0)/c
   widget_control,info(12),set_value=string(val(0)/c*1e-7)
   widget_control,info(17),set_value=2*!pi*k/(val(0)/c)/lamda(1)*1e14
   widget_control,info(18),set_value=string(2*!pi*k/(val(0)/c)/lamda(1)*1e14*1e-7*30/!pi)
   tmp1=val(0)/c
   tmp2=val(0)
   tmp3=2*!pi*k/(val(0)/c)/lamda(1)*1e14

   ;redraw the window
   err=refresh()

 endif
 if ev eq 13 then begin
   widget_control,info(18),set_value=string(val*1e-7*30/!pi)
   widget_control,info(11),set_value=2*!pi*k/val/lamda(1)*1e14
   widget_control,info(12),set_value=string(2*!pi*k/val/lamda(1)*1e14*1e-7)
   widget_control,info(14),set_value=2*!pi*k/val/lamda(1)*1e14*c
   widget_control,info(15),set_value=string(2*!pi*k/val/lamda(1)*1e14*c*1e-7)
   tmp1=2*!pi*k/val/lamda(1)*1e14
   tmp2=2*!pi*k/val/lamda(1)*1e14*c
   tmp3=val

   ;redraw the window
   err=refresh()

 endif
 if ev eq 14 then begin
   val=long(val*1e7*!pi/30)
   widget_control,info(17),set_value=val(0)
   widget_control,info(11),set_value=2*!pi*k/val(0)/lamda(1)*1e14
   widget_control,info(12),set_value=string(2*!pi*k/val(0)/lamda(1)*1e14*1e-7)
   widget_control,info(14),set_value=2*!pi*k/val(0)/lamda(1)*1e14*c
   widget_control,info(15),set_value=string(2*!pi*k/val(0)/lamda(1)*1e14*c*1e-7)
   tmp1=2*!pi*k/val(0)/lamda(1)*1e14
   tmp2=2*!pi*k/val(0)/lamda(1)*1e14*c
   tmp3=val(0)

   ;redraw the window
   err=refresh()

 endif

   tmp1=tmp1*1e-7
   tmp2=tmp2*1e-7
   tmp3=tmp3*1e-7

   disr(currentblock)=tmp1
   dr(currentblock)=tmp2
   wr(currentblock)=tmp3
   
   ;-------------------------------------------------
   ;up date dx
   ;-------------------------------------------------
   ;widget_control,info(20),set_value='dx           :'+string(deltax(tmp2,tmp3,lamda(1),phil(currentblock)))
   ;*?*
   
endif




;-------------------------------------------------
;resolution sliders (maximum value)
;-------------------------------------------------
if(ev eq 15) then begin
 
  sres(0,currentblock)=val
  err=refresh()

  if(err gt 0) then begin
    widget_control,info(23),set_value='Out Of Range'
    widget_control,info(26),set_value='Out Of Range'

    widget_control,info(11),sensitive=0
    widget_control,info(12),sensitive=0
    widget_control,info(14),sensitive=0
    widget_control,info(15),sensitive=0
    widget_control,info(17),sensitive=0
    widget_control,info(18),sensitive=0
    widget_control,info(27),sensitive=0
    widget_control,info(28),sensitive=0
    widget_control,info(29),sensitive=0

  endif else begin
    widget_control,info(23),set_value=string(val*1e-7)
    widget_control,info(25),get_value=val1
    widget_control,info(26),set_value=string(val1*1e-7)

    widget_control,info(11),sensitive=1
    widget_control,info(12),sensitive=1
    widget_control,info(14),sensitive=1
    widget_control,info(15),sensitive=1
    widget_control,info(17),sensitive=1
    widget_control,info(18),sensitive=1
    for u=0,numblocks-1 do begin
     widget_control,info(27+u),sensitive=1
    endfor

    ;drawplot
    redrawmain
    redrawparams
  endelse
endif


;-------------------------------------------------
;resolution text widget (maximum value)
;-------------------------------------------------
if(ev eq 16) then begin
  val=long(val*1e7)
  sres(0,currentblock)=val
  err=refresh()

  if(err gt 0) then begin
    widget_control,info(23),set_value='Out Of Range'
    widget_control,info(26),set_value='Out Of Range'

    widget_control,info(11),sensitive=0
    widget_control,info(12),sensitive=0
    widget_control,info(14),sensitive=0
    widget_control,info(15),sensitive=0
    widget_control,info(17),sensitive=0
    widget_control,info(18),sensitive=0
    widget_control,info(27),sensitive=0
    widget_control,info(28),sensitive=0
    widget_control,info(29),sensitive=0
 
  endif else begin
    widget_control,info(22),set_value=val(0)
    widget_control,info(25),get_value=val1
    widget_control,info(26),set_value=string(val1*1e-7)

    widget_control,info(11),sensitive=1
    widget_control,info(12),sensitive=1
    widget_control,info(14),sensitive=1
    widget_control,info(15),sensitive=1
    widget_control,info(17),sensitive=1
    widget_control,info(18),sensitive=1
    for u=0,numblocks-1 do begin
     widget_control,info(27+u),sensitive=1
    endfor
    
    redrawparams
    ;drawplot
    redrawmain
  
  endelse
endif

;-------------------------------------------------
;resolution sliders (minimum value)
;-------------------------------------------------
if(ev eq 17) then begin
 
  sres(1,currentblock)=val
  err=refresh()

  if(err gt 0) then begin
    widget_control,info(26),set_value='Out Of Range'
    widget_control,info(23),set_value='Out Of Range'

    ;block out unwanted
    widget_control,info(11),sensitive=0
    widget_control,info(12),sensitive=0
    widget_control,info(14),sensitive=0
    widget_control,info(15),sensitive=0
    widget_control,info(17),sensitive=0
    widget_control,info(18),sensitive=0
    widget_control,info(27),sensitive=0
    widget_control,info(28),sensitive=0
    widget_control,info(29),sensitive=0

  endif else begin
    widget_control,info(26),set_value=string(val*1e-7)
    widget_control,info(22),get_value=val1
    widget_control,info(23),set_value=string(val1*1e-7)

    ;block out unwanted
    widget_control,info(11),sensitive=1
    widget_control,info(12),sensitive=1
    widget_control,info(14),sensitive=1
    widget_control,info(15),sensitive=1
    widget_control,info(17),sensitive=1
    widget_control,info(18),sensitive=1
    for u=0,numblocks-1 do begin
     widget_control,info(27+u),sensitive=1
    endfor

    redrawparams
    ;drawplot
    redrawmain
    
  endelse
endif

;-------------------------------------------------
;resolution text widget (minimum value)
;-------------------------------------------------
if(ev eq 18) then begin
  val=long(val*1e7)
  sres(1,currentblock)=val
  err=refresh()

  if(err gt 0) then begin
    widget_control,info(26),set_value='Out Of Range'
    widget_control,info(23),set_value='Out Of Range'
 
    ;block out unwanted
    widget_control,info(11),sensitive=0
    widget_control,info(12),sensitive=0
    widget_control,info(14),sensitive=0
    widget_control,info(15),sensitive=0
    widget_control,info(17),sensitive=0
    widget_control,info(18),sensitive=0
    widget_control,info(27),sensitive=0
    widget_control,info(28),sensitive=0
    widget_control,info(29),sensitive=0

  endif else begin
    widget_control,info(25),set_value=val(0)
    widget_control,info(22),get_value=val1
    widget_control,info(23),set_value=string(val1*1e-7)

    ;block out unwanted
    widget_control,info(11),sensitive=1
    widget_control,info(12),sensitive=1
    widget_control,info(14),sensitive=1
    widget_control,info(15),sensitive=1
    widget_control,info(17),sensitive=1
    widget_control,info(18),sensitive=1
    for u=0,numblocks-1 do begin
     widget_control,info(27+u),sensitive=1
    endfor

    redrawparams
    ;drawplot
    redrawmain
   
  endelse
endif

;-------------------------------------------------
;block button 1 pressed
;-------------------------------------------------
if(ev eq 19) then begin

  ;save
  widget_control,info(22),get_value=a
  sres(0,currentblock)=a
  widget_control,info(25),get_value=a
  sres(1,currentblock)=a
  ;widget_control,info(11),get_value=a

  ;restore three slider bars
  currentblock=0
  qmin=minq(0) & qmax=maxq(0)

  vall=refresh()
  
  widget_control,info(11),set_slider_max=dis(1)*1e7,set_slider_min=dis(0)*1e7,set_value=disr(currentblock)*1e7
  widget_control,info(12),set_value=string(disr(currentblock))
  widget_control,info(14),set_slider_max=d(1)*1e7,set_slider_min=d(0)*1e7,set_value=dr(currentblock)*1e7
  widget_control,info(15),set_value=string(dr(currentblock))
  widget_control,info(17),set_slider_max=w(1)*1e7,set_slider_min=w(0)*1e7,set_value=wr(currentblock)*1e7
  widget_control,info(18),set_value=string(wr(currentblock)*30/!pi)
 
  widget_control,info(22),set_value=sres(0,currentblock)
  widget_control,info(23),set_value=string(sres(0,currentblock)*1e-7)
  widget_control,info(25),set_value=sres(1,currentblock)
  widget_control,info(26),set_value=string(sres(1,currentblock)*1e-7)
 
  widget_control,info(30),set_value='Sample Angle :'+string(sample_angle(currentblock))
  widget_control,info(31),set_value=string(currentblock+1)

  if currentblock eq 0 then begin
    widget_control,info(27),set_value='ACTIVE'
    widget_control,info(28),set_value='Block 2'
    widget_control,info(29),set_value='Block 3'
  endif
  if currentblock eq 1 then begin
    widget_control,info(27),set_value='Block 1'
    widget_control,info(28),set_value='ACTIVE'
    widget_control,info(29),set_value='Block 3'
  endif
  if currentblock eq 2 then begin
    widget_control,info(27),set_value='Block 1'
    widget_control,info(28),set_value='Block 2'
    widget_control,info(29),set_value='ACTIVE'
  endif

endif

 
;-------------------------------------------------
;block button 2 pressed
;-------------------------------------------------
if(ev eq 20) then begin
  
  ;save
  widget_control,info(22),get_value=a
  sres(0,currentblock)=a
  widget_control,info(25),get_value=a
  sres(1,currentblock)=a
  ;widget_control,info(11),get_value=a

  ;restore three slider bars
  currentblock=1
  qmin=minq(1) & qmax=maxq(1)

  vall=refresh()
  
  widget_control,info(11),set_slider_max=dis(1)*1e7,set_slider_min=dis(0)*1e7,set_value=disr(currentblock)*1e7
  widget_control,info(12),set_value=string(disr(currentblock))
  widget_control,info(14),set_slider_max=d(1)*1e7,set_slider_min=d(0)*1e7,set_value=dr(currentblock)*1e7
  widget_control,info(15),set_value=string(dr(currentblock))
  widget_control,info(17),set_slider_max=w(1)*1e7,set_slider_min=w(0)*1e7,set_value=wr(currentblock)*1e7
  widget_control,info(18),set_value=string(wr(currentblock)*30/!pi)
 
  widget_control,info(22),set_value=sres(0,currentblock)
  widget_control,info(23),set_value=string(sres(0,currentblock)*1e-7)
  widget_control,info(25),set_value=sres(1,currentblock)
  widget_control,info(26),set_value=string(sres(1,currentblock)*1e-7)

  widget_control,info(30),set_value='Sample Angle :'+string(sample_angle(currentblock))
  widget_control,info(31),set_value=string(currentblock+1) 
 
  if currentblock eq 0 then begin
    widget_control,info(27),set_value='ACTIVE'
    widget_control,info(28),set_value='Block 2'
    widget_control,info(29),set_value='Block 3'
  endif
  if currentblock eq 1 then begin
    widget_control,info(27),set_value='Block 1'
    widget_control,info(28),set_value='ACTIVE'
    widget_control,info(29),set_value='Block 3'
  endif
  if currentblock eq 2 then begin
    widget_control,info(27),set_value='Block 1'
    widget_control,info(28),set_value='Block 2'
    widget_control,info(29),set_value='ACTIVE'
  endif

endif

;-------------------------------------------------
;Block button 3 pressed
;-------------------------------------------------
if(ev eq 21) then begin
  
  ;save
  widget_control,info(22),get_value=a
  sres(0,currentblock)=a
  widget_control,info(25),get_value=a
  sres(1,currentblock)=a
  widget_control,info(11),get_value=a
  
  currentblock=2
  qmin=minq(2) & qmax=maxq(2)

  vall=refresh()
  
  widget_control,info(11),set_slider_max=dis(1)*1e7,set_slider_min=dis(0)*1e7,set_value=disr(currentblock)*1e7
  widget_control,info(12),set_value=string(disr(currentblock))
  widget_control,info(14),set_slider_max=d(1)*1e7,set_slider_min=d(0)*1e7,set_value=dr(currentblock)*1e7
  widget_control,info(15),set_value=string(dr(currentblock))
  widget_control,info(17),set_slider_max=w(1)*1e7,set_slider_min=w(0)*1e7,set_value=wr(currentblock)*1e7
  widget_control,info(18),set_value=string(wr(currentblock)*30/!pi)
 
  widget_control,info(22),set_value=sres(0,currentblock)
  widget_control,info(23),set_value=string(sres(0,currentblock)*1e-7)
  widget_control,info(25),set_value=sres(1,currentblock)
  widget_control,info(26),set_value=string(sres(1,currentblock)*1e-7)

  widget_control,info(30),set_value='Sample Angle :'+string(sample_angle(currentblock))
  widget_control,info(31),set_value=string(currentblock+1)

  if currentblock eq 0 then begin
    widget_control,info(27),set_value='ACTIVE'
    widget_control,info(28),set_value='Block 2'
    widget_control,info(29),set_value='Block 3'
  endif
  if currentblock eq 1 then begin
    widget_control,info(27),set_value='Block 1'
    widget_control,info(28),set_value='ACTIVE'
    widget_control,info(29),set_value='Block 3'
  endif
  if currentblock eq 2 then begin
    widget_control,info(27),set_value='Block 1'
    widget_control,info(28),set_value='Block 2'
    widget_control,info(29),set_value='ACTIVE'
  endif

endif


;-------------------------------------------------
;save button pressed
;-------------------------------------------------
if ev eq 30 then begin
  tmp=save_string+'.dat'
  openw,1,tmp

  ;-------------------------------------------------
  ;Distance from chopper to sample
  ;-------------------------------------------------
  tmp1=chopper_to_sample

  ;output block 1 to file

  for u=1,numblocks do begin

  
  printf,1,'BLOCK'+strtrim(string(u),2)

  ;DET is distance from SAMPLE to DETECTOR
  printf,1,'DET',string(disr(u-1)+(dr(u-1)/2)-tmp1)
  
  ;CMT is chopper separation
  printf,1,'CHT',string(dr(u-1))

  ;CS1 is rpm of chopper 1
  printf,1,'CS1',string(wr(u-1)*30/!pi)

  ;CS2 is rpm of chopper 2
  printf,1,'CS2',string(wr(u-1)*30/!pi)

  ;PHA is phase angle between chopper plates in degrees
  printf,1,'PHA',string(phil(u-1))

  ;SAM is sample angle of sample in degrees
  printf,1,'SAM',string(sample_angle(u-1))

  endfor

  close,1

  errormessage,'Data has been Saved','As',tmp
endif

return
end 





;*******************************************************************************************************
;* 				      Procedure DrawPLot
;*
;*This procedure draws the resolution vs q graph, and the resolution vs q/qmin
;*
;*
;*Variables (local)   :
;*
;*  ng                :Number of points used to draw the graph
;*  Tt()              :Contains the time resolution data points
;*  q                 :Contains the corrosponding q values
;*  gval              :contains the gfxs window reference
;*  rangelow          :Contains the lowest q value (in block 1)
;*  rangehigh         :Contains the hightest q value (in block 3)
;*  low               :Contains the time resolution maximum, from slider value, for current block
;*  high              :Contains the time resolution minimum, from slider value, for current block
;*  mm                :Contains value of the gradient
;*  cc                :Contains value of the intercept
;*  thetax(2)         :Contains value of minimum q and maximum q for a given block
;*  thetay(2)         :Contains value of minimum ang. res. and maximum ang. res. for a given block
;*  y                 :Contains value of minimum and maximum time resolution
;*  u                 :Counter in the for loop
;*  bcurrentblk       :Stores the orginal currentblock
;*  thick             :Contains a number representing the thickness of the line (1=normal, 2=double etc)
;*  ceiling           :highest value for the y axis
;*
;*******************************************************************************************************
;coool

pro drawplot

common graph
common qs
!p.multi=[0,0,0,0,0]
plot,[0,0],xstyle=5,ystyle=5
ceiling=0.
;--------------------------------------------------
;Set graphics window for output for resolution vs q
;--------------------------------------------------
;widget_control,draw,get_value=gval
;wset,gval

ng=51 
Tt=fltarr(ng) 
q=fltarr(ng) 
rangelow=minq(0)
rangehigh=maxq(numblocks-1)
bcurrentblk=currentblock
!p.multi=[2,1,2,0,1]
thick=2

;-------------------------------------------------
;Draw Block 1 in selected graphics window
;-------------------------------------------------
;draw the time resolution

low=sres(0,0)*1e-7
high=sres(1,0)*1e-7

;-------------------------------------------------
;Modify resolutions
;-------------------------------------------------

;-------------------------------------------------
;Draw Block 1 in the selected graphics window
;-------------------------------------------------

currentblock=0

;low=low-time_modification(minq(0),wr(0),disr(0))
;high=high-time_modification(maxq(0),wr(0),disr(0))

q=minq(0)+(findgen(ng)*(maxq(0)-minq(0))/(ng-1))
mm=(high-low)/(maxq(0)-minq(0))
cc=high-mm*maxq(0)
Tt=mm*q+cc
ceiling=sres(1,0)
if(numblocks gt 1) then ceiling=(sres(1,0)>sres(1,1))
if(numblocks gt 2) then ceiling=(ceiling>sres(1,2))
ceiling=ceiling*1e-7+.01

plot,q,Tt,xrange=[rangelow,rangehigh],yrange=[0,ceiling],/xstyle,/ystyle,xtitle="q",$
ytitle="Time And Angle Resolutions",title="Resolutions vs q",thick=thick
!p.multi=[2,1,2,0,1]
;draw the angular resolution
thetax=[minq(0),maxq(0)]
thetay=[angres(0),angres(0)]
plot,thetax,thetay,/noerase,xrange=[rangelow,rangehigh],yrange=[0,ceiling],xstyle=5,ystyle=5,color=2^15,thick=thick

;-------------------------------------------------
;Draw Block 2 in selected graphics window
;-------------------------------------------------
;draw time resolution

if(numblocks gt 1) then begin
  
  low=sres(0,1)*1e-7
  high=sres(1,1)*1e-7
  
  currentblock=1	

;  low=low-time_modification(minq(1),wr(1),disr(1))
;  high=high-time_modification(maxq(1),wr(1),disr(1))

  mm=(high-low)/(maxq(1)-minq(1))
  cc=high-mm*maxq(1)
  q=minq(1)+(findgen(ng)*(maxq(1)-minq(1))/(ng-1))
  Tt=mm*q+cc
  plot,q,Tt,/noerase,xstyle=5,ystyle=5,xrange=[rangelow,rangehigh],yrange=[0,ceiling],thick=thick
  
  ;draw angular resolution
  thetax=[minq(1),maxq(1)]
  thetay=[angres(1),angres(1)]
  plot,thetax,thetay,/noerase,xrange=[rangelow,rangehigh],yrange=[0,ceiling],xstyle=5,ystyle=5,color=2^15,thick=thick
  
endif


;-------------------------------------------------
;Draw Block 3 in selected graphics window
;-------------------------------------------------
;draw time resolution
if(numblocks gt 2) then begin
  
  low=sres(0,2)*1e-7
  high=sres(1,2)*1e-7
  
  currentblock=2	

;  low=low-time_modification(minq(2),wr(2),disr(2))
;  high=high-time_modification(maxq(2),wr(2),disr(2))

  mm=(high-low)/(maxq(2)-minq(2))
  cc=high-mm*maxq(2)
  q=minq(2)+(findgen(ng)*(maxq(2)-minq(2))/(ng-1))
  Tt=mm*q+cc
  plot,q,Tt,/noerase,xstyle=5,ystyle=5,xrange=[rangelow,rangehigh],yrange=[0,ceiling],thick=thick

  ;draw angular resolution
  thetax=[minq(2),maxq(2)]
  thetay=[angres(2),angres(2)]
  plot,thetax,thetay,/noerase,xrange=[rangelow,rangehigh],yrange=[0,ceiling],xstyle=5,ystyle=5,color=2^15,thick=thick
endif

;-------------------------------------------------------
;Set graphics window for output for resolution vs q/qmin
;------------------------------------------------------- 
;widget_control,draw2,get_value=gval
;wset,gval


;----------------------------------------------------
;Draw the time and angular resolutions for the blocks
;----------------------------------------------------     
for u=1,numblocks do begin

  currentblock=u-1   

  rangelow=minq(u-1)/minq(u-1) 
  rangehigh=maxq(u-1)/minq(u-1)
  low=sres(0,u-1)*1e-7
  high=sres(1,u-1)*1e-7
;  low=low-time_modification(minq(u-1),wr(u-1),disr(u-1))
;  high=high-time_modification(maxq(u-1),wr(u-1),disr(u-1))

  q=[rangelow,rangehigh]
  y=[low,high]

  if u eq 1 then begin
    ;draw time resolution 
    !p.multi=[1,1,2,0,1]
    plot,q,y,xrange=[rangelow,rangehigh],yrange=[0,ceiling],/xstyle,/ystyle,title='Resolutions vs q/qmin',xtitle='q/qmin',$
ytitle='Time and Angle Resolutions',thick=thick
    ;draw angular resolution
    !p.multi=[1,1,2,0,1]
    plot,q,[angres(0),angres(0)],color=2^15,xstyle=5,ystyle=5,yrange=[0,ceiling],$
xrange=[rangelow,rangehigh],/noerase,thick=thick
  endif else begin
    !p.multi=[1,1,2,0,0]
    ;draw time resolution
    plot,q,y,xrange=[rangelow,rangehigh],yrange=[0,ceiling],xstyle=5,ystyle=5,/noerase,thick=thick,linestyle=2^(u-1)
    ;draw angular resolution
    plot,q,[angres(u-1),angres(u-1)],color=2^15,xstyle=5,ystyle=5,yrange=[0,ceiling],xrange=[rangelow,rangehigh],/noerase,$
thick=thick,linestyle=2^(u-1)
  endelse
endfor

currentblock=bcurrentblk

return
end





;*************************************************************************************************
;* 				     Procedure ErrorMessage
;*
;*This procedure produces a window with a text message. mes1 is a string holding the first line
;*of text. mes2 is a string holding the second line of text. mes3 is a string holding the third
;*line of text.
;*
;*  Procedure parameters :
;*   
;*  mes1  : string of any lenght
;*  mes2  : string of any lenght
;*  mes3  : string of sny lenght
;*
;*  
;*
;*************************************************************************************************


pro errormessage,mes1,mes2,mes3
common error

errsnum=errsnum+1
if(errsnum eq 101) then errsnum=0

errbase(errsnum)=widget_base(title='MESSAGE WINDOW',/column)
mess1=widget_label(errbase(errsnum),value=mes1)
mess2=widget_label(errbase(errsnum),value=mes2)
mess3=widget_label(errbase(errsnum),value=mes3)
errbut=widget_button(errbase(errsnum),value='RETURN',uvalue=errsnum+300)

widget_control,/realize,errbase(errsnum)
xmanager,'error',errbase(errsnum)

return
end





;*************************************************************************************************
;*					 Procedure Error_event
;*
;*This procedure is called by 'xmanager' when a message window has been interacted with. The 
;*parameter -event- is a stucture that contains all the information about the widget that has
;*been interacted with.
;*
;* Parameters :
;*
;* event  : Structure
;*
;*
;* Variables (local)  :
;*
;* ev     :  Contains the user value of the interacted widget
;*************************************************************************************************


pro error_event,event
common error

widget_control,event.id,get_uvalue=ev
widget_control,errbase(ev-300),/destroy

return
end





;*************************************************************************************************
;*					  Function Lims
;*
;*This function does. (a) Produces the new limits of d, dis and w. (b) Produces the chopper angle
;*(c) Produces delta x.
;*
;* Paramters :
;*
;* rmin  : minimum time resolution for particular block
;* rmax  : maximum time resolution for particular block
;* qminn : minimum q value for particular block
;* qmaxx : maximum q value for particular block
;*
;* Variables (local) :
;*
;* tflag             :Stores the status of the limits of d,w and phi
;*                   :tflag is 0 :OK
;*                   :tflag is 1 :d's out of range
;*                   :tflag is 2 :w's out of range
;*                   :tflag is 3 :phi's out of range
;* dismin            :contains tempory used to work out new range for dis
;* dismax            :contains tempory used to work out new range for dis
;* dmin              :contains tempory used to work out new range for d
;* dmax              :contains tempory used to work out new range for d
;* flag              :contains tempory infomation used to set tflag
;* o                 :a constant used for convenience in calulations
;* wmin              :contains tempory used to work out new range for w
;* wmax              :contains tempory used to work out new range for w
;* velchop           :Stores the velocity of the chopper 
;* velbeam           :Stores the velocity of the beam
;* b                 :a constant used for convenience in calulations
;* h                 :a constant used for convenience in calulations
;*
;*
;*************************************************************************************************


function lims,rmin,rmax,qminn,qmaxx
common graph
common vars
common qs

;-------------------------------------------------
;Set default ranges to start with
;-------------------------------------------------
d=bd & dis=bdis & w=bw

;-------------------------------------------------
;By default nothing out of range yet
;-------------------------------------------------
tflag=0

;-------------------------------------------------
;obtain gradient and intercept
;-------------------------------------------------
m=(rmax-rmin)/(qmaxx-qminn)
c=rmax-m*qmaxx

;-------------------------------------------------
;obtain new d and dis ranges
;-------------------------------------------------
flag=0
dismin=dis(0) & dismax=dis(1)
dmin=d(0) & dmax=d(1)
dmin=c*dis(0)
if(dmin lt d(0)) then dmin=d(0) & dismin=dmin/c
if(dmin gt d(1)) then flag=1
dmax=c*dis(1)
if(dmax gt d(1)) then dmax=d(1) & dismax=dmax/c
if(dmax lt d(0)) then flag=1
if flag then begin
  tflag=1
  goto, trigger
endif else begin
  d(0)=dmin & d(1)=dmax
  dis(0)=dismin & dis(1)=dismax
endelse


;-------------------------------------------------
;obtain new w
;-------------------------------------------------
flag=0
o=2*!pi*k/lamda(1)
dismin=dis(0) & dismax=dis(1)
wmin=w(0) & wmax=w(1)
wmin=o/dis(1)
if(wmin lt w(0)) then wmin=w(0) & dismax=o/wmin
if(wmin gt w(1)) then flag=1
wmax=o/dis(0)
if(wmax gt w(1)) then wmax=w(1) & dismin=o/wmax
if(wmax lt w(0)) then flag=1
if flag then begin
  tflag=2
  goto,trigger
endif else begin
  dis(0)=dismin & dis(1)=dismax
  w(0)=wmin & w(1)=wmax
  d(0)=c*dis(0) & d(1)=c*dis(1)
endelse

;-------------------------------------------------
;obtain new chopper angle
;-------------------------------------------------
phil(currentblock)=2*!pi*m/((1/qminn)-(1/qmaxx))
phil(currentblock)=phil(currentblock)/!pi*180 
if ((phil(currentblock) gt phi(1)) or (phil(currentblock) lt phi(0))) then begin
        tflag=3
	goto,trigger
endif


;-------------------------------------------------
;Calculate delta x's RANGE
;-------------------------------------------------
;velchop=w*r
;velbeam=k/lamda(1)
;b=atan(rotate(velchop,2)/velbeam)
;x=d*sin(b)+phil(currentblock)*!pi/180*r
;-----------------------------------------------------------
;the beam width part
;(nb make sure x here is consistent with the deltax function
;-----------------------------------------------------------
;h=bob(s3v(currentblock),s2v(currentblock),90,chopper_width,slit_chopper_dist)
;x=x+h

trigger:
return, tflag
end






;*************************************************************************************************
;*					Procedure redrawparams
;*
;*
;*Procedure to Redraw the Ranges window. Thats all folks. 
;*
;*
;* Variables : (local)
;*
;* tmp       : Tempory variable 
;* vel       : Velocity of the neutrons
;* val       : Gives the time resolution at q half
;* mt        : Gradient of graph
;* ct        : Intercept of graph
;*
;*
;*(prgs note : call lims procedure before running this to ensure updated info)
;*
;*************************************************************************************************


pro redrawparams
common graph
common vars
common qs

widget_control,info(0),set_value='TOF Range  : '+string(dis(0))+'  '+string(dis(1))
widget_control,info(1),set_value='d Range    : '+string(d(0))+'  '+string(d(1))
widget_control,info(2),set_value='Rpms Range : '+string(w(1)*30/!pi)+'  '+string(w(0)*30/!pi)
widget_control,info(3),set_value='Chopper Ang: '+string(phil(currentblock))+'  '+string(phil(currentblock))
tmp=string(time_modification(minq(currentblock),wr(currentblock),disr(currentblock)))
tmp=tmp+'  '
tmp=tmp+string(time_modification(maxq(currentblock),wr(currentblock),disr(currentblock)))
widget_control,info(4),set_value='dtx/T      : '+tmp
widget_control,info(19),set_value='Chopper Angle:'+string(phil(currentblock))


;this is the calculation for the transmission or something proportional to it
mt=(sres(1,currentblock)*1e-7-sres(0,currentblock)*1e-7)/(maxq(currentblock)-minq(currentblock))
ct=sres(1,currentblock)*1e-7-mt*maxq(currentblock)

qhalf=(maxq(currentblock)-minq(currentblock))/2+minq(currentblock)

vel=k*qhalf/4/!pi/sin(sample_angle(currentblock)*!pi/180)

val=mt*qhalf+ct

tmp=wr(currentblock)/2./!pi*val*disr(currentblock)/vel

widget_control,info(45),set_value=string(tmp)

return
end






;*************************************************************************************************
;*				Procedure redrawmain
;*
;*This procedure centres the d, w and dis sliders
;*
;* Variables (local) :
;*
;* user1     :a constant used for convenience in calulations
;* user2     :a constant used for convenience in calulations
;* user3     :a constant used for convenience in calulations
;*
;*
;*
;*
;*
;* (Prgs note : delta x update here)  ***i think there is an error here***
;*************************************************************************************************


pro redrawmain
common graph
common vars
common qs

;-------------------------------------------------
;set some constants
;-------------------------------------------------
user1=(dis(1)-dis(0))/2+dis(0)
user2=(d(1)-d(0))/2+d(0)
user3=2*!pi*k/user1/lamda(1)

widget_control,info(12),set_value=string(user1)
widget_control,info(11),set_value=user1*1e7
disr(currentblock)=user1

widget_control,info(15),set_value=string(user2)
widget_control,info(14),set_value=user2*1e7
dr(currentblock)=user2

widget_control,info(18),set_value=string(user3*30/!pi)
widget_control,info(17),set_value=user3*1e7
wr(currentblock)=user3

return
end





;*************************************************************************************************
;*				     Function BOB
;*
;*This routine calculates the positions of illumination on a sample.
;*
;* Parameters  :
;*
;* s2          : Slit width (m)
;* s1          : Slit width (m)
;* th          : Sample angle (degrees)
;* sam         : sample lenght (m)
;* d2          : distance from slit to sample (m)
;*
;* Variables (local) :
;*
;* pi          : a contant containing Pi - here to make sure gives Exactly the same result
;*             : the fortran program
;* d1          : The distance between the slits (m)
;* amda        : Maximum wavelenght (1e10)
;* flag        : Contains 1 if there is neutron divergence otherwise 0
;* h           : Contains -1 if there is neutron divergence otherwise 0
;* 
;* (see bobs fortran program for details on this calulation and variables)
;*
;*
;* (Prgs note: slit references in the direction of the beam)
;*************************************************************************************************


function bob,s2,s1,th,sam,d2
common slitstuff
common qs


pi=3.1415926
d1=slit_separation
amda=30.
flag=0
th=th*pi/180 ; convert to radians
foot_print=sam*sin(th)


;-------------------------------------------------
;minimum s2 for under illimination
;-------------------------------------------------
s1min=((sam*sin(th)-s2)/((sam/2.)*cos(th)+d2))*d1-s2
;print,'Minimum mon slit for under illumination: ',s1min
div=.1*amda

if(s1 gt s2)then begin 
 fwhm=s1*180./(d1*pi)
endif else begin
 if(s2 ge s1)then begin
   fwhm=s2*180./(d1*pi)
 endif 
endelse
arange=2.*atan((s1+s2)/(2.*d1))
del=fwhm*pi/(th*180.)
arange=arange*180./pi
if(s1 ne s2)then begin
  x=d2-((d1*s2)/(s1-s2))
  w2=abs((s2*x)/(d2-x))
endif else begin
  w2=s1
  x=9999.
endelse
w1=s2+((d2*(s1+s2))/d1)
F=w2+(w1-w2)/2
if((arange/2.) gt div)then begin
  print,' Angular range limited by neutron divergence!'
  flag=1
endif

th1=atan((s1+s2)/(2.*d1))
if(s1 eq s2)then begin
 th2=0
endif else begin
  th2=atan(s2/(2.*(d2-x)))
endelse


;-------------------------------------------------
;Coordinates for the illumination of the slit
;-------------------------------------------------
rr1=(sam/2)+(w2/(2*cos(th)*(tan(th)+tan(th2))))
rr2=(sam/2)+(w1/(2*cos(th)*(tan(th)+tan(th1))))
ll1=(sam/2)-(w1/(2*cos(th)*(tan(th)-tan(th1))))
ll2=(sam/2)-(w2/(2*cos(th)*(tan(th)-tan(th2))))


if(flag eq 0) then begin
  ;half width at half maximum
  h=(rr1-ll2)+(ll2-ll1)/2+(rr2-rr1)/2
endif else begin
  h=-1
endelse

return,h
end





;*************************************************************************************************
;*				    Procedure base_event
;*
;*This procedure is called by the xmanager. It controls the events in the initial setup window.
;*
;* Parameters :
;* 
;* event      : Structure
;*
;* 
;* Variables  (local) :
;*
;* ev              : Stores the user value of the widget begin interacted with
;* val             : Stores the value of the widget itself.
;* q1,q2           : Stores the miniumum and maximum q values for whole range
;* qrange          : a constant that stores q2/q1
;* tmp, qtmp,junk  : tempory variables depends on context
;* u               : variable used for the counter in for loops
;* overlap,overlap1: contains the amount of shift due to the overlap parameter
;* sep             : contains slit separation
;* res             : contains angular resolution
;* dtheata         : contains delta theata
;* f               : contains new slider values for s2 and s3
;* w               : tempory variable
;*
;*
;*
;*************************************************************************************************


pro base_event,event
common qs
common graph
common vars

widget_control,event.id,get_uvalue=ev,get_value=val



banner=0
 
;-------------------------------------------------
;Quit button pressed
;-------------------------------------------------
if(ev eq 57) then begin
  widget_control,/reset
  qflag=1
endif

;-------------------------------------------------
;qmin changed
;-------------------------------------------------
if(ev eq 41)or(restart_flag eq 1) then begin
  if restart_flag eq 1 then begin
    restart_flag=0
    widget_control,wid(1),get_value=tmp
    val=float(tmp(0))
  endif
  
  widget_control,wid(3),get_value=qtmp
  q1=float(val(0))
  q2=float(qtmp(0))

  if(q1 lt 0) then begin
    for u=0,widmax do begin
      widget_control,wid(u),sensitive=0
    endfor
    errormessage,"q min should","not be","negative"
    widget_control,wid(1),sensitive=1    
    widget_control,wid(3),sensitive=1  
    widget_control,wid(17),sensitive=1
    banner=1
  endif 

  if(q1 gt q2) then begin
    for u=0,widmax do begin
      widget_control,wid(u),sensitive=0
    endfor
    errormessage,"q min should be","smaller than","q max"
    widget_control,wid(1),sensitive=1    
    widget_control,wid(3),sensitive=1  
    widget_control,wid(28),sensitive=1
    widget_control,wid(17),sensitive=1
    banner=1
  endif 

  qrange=q2/q1

  if(qrange gt 1000) then begin
    for u=0, widmax do begin
      widget_control,wid(u),sensitive=0
    endfor
    errmessage,"q max should not be more than 3 factors of","wavelenght max/wavelenght min","away from q min"
    widget_control,wid(1),sensitive=1    
    widget_control,wid(3),sensitive=1
    widget_control,wid(17),sensitive=1
    banner=1
  endif

  junk=check_math(1,1)
  if(qrange le qrangeb) and (qrange gt 0) then begin
    numblocks=1
    minq(0)=q1
    maxq(0)=q1*qrangeb
    sample_angle(0)=asin(lamda(1)*minq(0)/4/!pi)*180/!pi
  endif
  if(qrange le qrangeb^2) and (qrange gt qrangeb) then begin
    numblocks=2
    minq(0)=q1
    maxq(0)=q1*qrangeb
    overlap=perover*(maxq(0)-minq(0))
    minq(1)=q1*qrangeb-overlap
    maxq(1)=(q1*qrangeb-overlap)*qrangeb
    sample_angle(0)=asin(lamda(1)*minq(0)/4/!pi)*180/!pi
    sample_angle(1)=asin(lamda(1)*minq(1)/4/!pi)*180/!pi
    junk=check_math(0,0)
  endif
  if(qrange le qrangeb^3) and (qrange gt qrangeb^2) then begin
    numblocks=3
    minq(0)=q1
    maxq(0)=q1*qrangeb
    overlap=perover*(maxq(0)-minq(0))
    minq(1)=q1*qrangeb-overlap
    maxq(1)=(q1*qrangeb-overlap)*qrangeb
    overlap1=perover*(maxq(1)-minq(1))
    minq(2)=(q1*qrangeb-overlap)*qrangeb-overlap1
    maxq(2)=((q1*qrangeb-overlap)*qrangeb-overlap1)*qrangeb
    sample_angle(0)=asin(lamda(1)*minq(0)/4/!pi)*180/!pi
    sample_angle(1)=asin(lamda(1)*minq(1)/4/!pi)*180/!pi
    sample_angle(2)=asin(lamda(1)*minq(2)/4/!pi)*180/!pi
  endif
  
  if check_math(0,0) ne 0 then begin
    sample_angle(0)=-1.
    sample_angle(1)=-1.
    sample_angle(2)=-1.
  endif

  ;display current block
  widget_control,wid(19),set_value=string(currentblock+1)

  ;set the q for the block
  widget_control,wid(7),set_value=string(minq(0))
  widget_control,wid(9),set_value=string(maxq(0))

  ;set sample angle
  widget_control,wid(28),set_value=string(sample_angle(currentblock))

  if(sample_angle(currentblock) gt 25)or(sample_angle(currentblock) lt 0) then begin
    for u=0,widmax do begin
      widget_control,wid(u),sensitive=0
    endfor
    errormessage,"Sample Angle Exceeds","Bounds","Change Q Range"
    widget_control,wid(1),sensitive=1    
    widget_control,wid(3),sensitive=1  
    widget_control,wid(28),sensitive=1
    widget_control,wid(17),sensitive=1
    banner=1
  endif

  widget_control,wid(34),get_value=sep
  sep=float(sep(0))

  ;in radians
  dtheata=(s2v(currentblock)>s3v(currentblock))/sep
  ;convert to degrees
  dtheata=dtheata*180/!pi

  ;set the angular resolution widgets
  for u=1,numblocks do begin
    res=dtheata/sample_angle(u-1)
    widget_control,wid(44+u),set_value=string(res)
  endfor

  ;set the angular resolution widget to ready, if it is not needed
  if(numblocks ne 3) then begin
    for u=numblocks+1,3 do begin
      widget_control,wid(44+u),set_value='Ready'
    endfor
  endif

  if banner eq 0 then begin
    for u=0,widmax do begin
      widget_control,wid(u),sensitive=1
    endfor

    ;de-activate unused block buttons
    if(numblocks lt 3) then begin
      widget_control,wid(16),sensitive=0
    endif
    if(numblocks lt 2) then begin
      widget_control,wid(15),sensitive=0
    endif
  endif

  ;close down illumination graph if it is active
  if graph(3) eq 1 then graph  

  ;update foot print, and illumination values
  update_ftper
endif


;-------------------------------------------------
;qmax changed
;-------------------------------------------------
if(ev eq 43) then begin
  banner=0
  widget_control,wid(1),get_value=qtmp
  q2=float(val(0))
  q1=float(qtmp(0))

  if(q2 lt 0) then begin
    for u=0,widmax do begin
      widget_control,wid(u),sensitive=0
    endfor
    errormessage,"q max should","not be","negative"
    widget_control,wid(1),sensitive=1    
    widget_control,wid(3),sensitive=1  
    widget_control,wid(17),sensitive=1
    banner=1
  endif
 
  if(q1 gt q2) then begin
    for u=0,widmax do begin
      widget_control,wid(u),sensitive=0
    endfor
    errormessage,"q min should be","smaller than","q max"
    widget_control,wid(1),sensitive=1    
    widget_control,wid(3),sensitive=1  
    widget_control,wid(28),sensitive=1
    widget_control,wid(17),sensitive=1
    banner=1
  endif

  qrange=q2/q1

  if(qrange gt 1000) then begin
    for u=0,widmax do begin
      widget_control,wid(u),sensitive=0
    endfor
    errormessage,"q max should not be more than 3 factors of","wavelenght max/wavelenght min","away from q min"
    widget_control,wid(1),sensitive=1    
    widget_control,wid(3),sensitive=1
    widget_control,wid(17),sensitive=1
    banner=1
  endif

  junk=check_math(1,1)
  if(qrange le qrangeb) and (qrange gt 0) then begin
    numblocks=1
    minq(0)=q1
    maxq(0)=q1*qrangeb
    sample_angle(0)=asin(lamda(1)*minq(0)/4/!pi)*180/!pi
  endif
  if(qrange le qrangeb^2) and (qrange gt qrangeb) then begin
    numblocks=2
    minq(0)=q1
    maxq(0)=q1*qrangeb
    overlap=perover*(maxq(0)-minq(0))
    minq(1)=q1*qrangeb-overlap
    maxq(1)=(q1*qrangeb-overlap)*qrangeb
    sample_angle(0)=asin(lamda(1)*minq(0)/4/!pi)*180/!pi
    sample_angle(1)=asin(lamda(1)*minq(1)/4/!pi)*180/!pi
  endif
  if(qrange le qrangeb^3) and (qrange gt qrangeb^2) then begin
    numblocks=3
    minq(0)=q1
    maxq(0)=q1*qrangeb
    overlap=perover*(maxq(0)-minq(0))
    minq(1)=q1*qrangeb-overlap
    maxq(1)=(q1*qrangeb-overlap)*qrangeb
    overlap1=perover*(maxq(1)-minq(1))
    minq(2)=(q1*qrangeb-overlap)*qrangeb-overlap1
    maxq(2)=((q1*qrangeb-overlap)*qrangeb-overlap1)*qrangeb
    sample_angle(0)=asin(lamda(1)*minq(0)/4/!pi)*180/!pi
    sample_angle(1)=asin(lamda(1)*minq(1)/4/!pi)*180/!pi
    sample_angle(2)=asin(lamda(1)*minq(2)/4/!pi)*180/!pi
  endif

  if check_math(0,0) ne 0 then begin
    sample_angle(0)=-1.
    sample_angle(1)=-1.
    sample_angle(2)=-1.
  endif

  ;display current block
  widget_control,wid(19),set_value=string(currentblock+1)

  ;set the q for the block
  widget_control,wid(7),set_value=string(minq(0))
  widget_control,wid(9),set_value=string(maxq(0))

  ;set sample angle
  widget_control,wid(28),set_value=string(sample_angle(currentblock))

  if(sample_angle(currentblock) gt 25)or(sample_angle(currentblock) lt 0) then begin
    for u=0,widmax do begin
      widget_control,wid(u),sensitive=0
    endfor
    errormessage,"Sample Angle Exceeds","Bounds","Change Q Range"
    widget_control,wid(1),sensitive=1    
    widget_control,wid(3),sensitive=1  
    widget_control,wid(28),sensitive=1
    widget_control,wid(17),sensitive=1
    banner=1
  endif  

  widget_control,wid(34),get_value=sep
  sep=float(sep(0))


  dtheata=(s2v(currentblock)>s3v(currentblock))/sep
  dtheata=dtheata*180/!pi

  for u=1,numblocks do begin    
    res=dtheata/sample_angle(u-1)
    widget_control,wid(44+u),set_value=string(res)
  endfor

  ;blank out unused resolutions
  if(numblocks ne 3) then begin
    for u=numblocks+1,3 do begin
      widget_control,wid(44+u),set_value='Ready'
    endfor
  endif

  if banner eq 0 then begin
    for u=0,widmax do begin
      widget_control,wid(u),sensitive=1
    endfor

    ;blank out unneccesarry ones
    if(numblocks lt 3) then begin
      widget_control,wid(16),sensitive=0
    endif
    if(numblocks lt 2) then begin
      widget_control,wid(15),sensitive=0
    endif
  endif

  ;close down graph
  if graph(3) eq 1 then graph 

  update_ftper

endif

;-------------------------------------------------
;The s2 slider was moved
;-------------------------------------------------
if(ev eq 62) then begin
  ;update the text showing s2's value
  s2v(currentblock)=val*1e-7
  widget_control,wid(23),set_value=string(s2v(currentblock))
  if slit_lock eq 1 then begin
    s3v(currentblock)=s2v(currentblock)
    widget_control,wid(25),set_value=s3v(currentblock)*1e7
    widget_control,wid(26),set_value=string(s3v(currentblock))
  endif  

  widget_control,wid(34),get_value=sep
  sep=float(sep(0))

  dtheata=(s2v(currentblock)>s3v(currentblock))/sep
  dtheata=dtheata*180/!pi

  res=dtheata/sample_angle(currentblock)
  widget_control,wid(45+currentblock),set_value=string(res)

  ;update graph if it is on
  if graph(3) eq 1 then begin
    updategraph,s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist
  endif

  update_ftper  
endif

;-------------------------------------------------
;s3 slider was moved
;-------------------------------------------------
if(ev eq 65) then begin

  ;update text widget showing the value of s3
  s3v(currentblock)=val*1e-7
  widget_control,wid(26),set_value=string(s3v(currentblock))
  if slit_lock eq 1 then begin
    s2v(currentblock)=s3v(currentblock)
    widget_control,wid(22),set_value=s2v(currentblock)*1e7
    widget_control,wid(23),set_value=string(s2v(currentblock))
  endif
   
  widget_control,wid(34),get_value=sep
  sep=float(sep(0))

  dtheata=(s2v(currentblock)>s3v(currentblock))/sep
  dtheata=dtheata*180/!pi

  res=dtheata/sample_angle(currentblock)
  widget_control,wid(45+currentblock),set_value=string(res)

  ;update graph if it is on
  if graph(3) eq 1 then begin
    updategraph,s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist
  endif

  update_ftper

endif

;-------------------------------------------------
;text widget for s2 was altered
;-------------------------------------------------
if(ev eq 63) then begin

  ;update the slider s2
  val=long(val(0)*1e7)
  s2v(currentblock)=val*1e-7

  if s2v(currentblock) gt slit(1) then s2v(currentblock)=slit(1) 
  if s2v(currentblock) lt slit(0) then s2v(currentblock)=slit(0)
  widget_control,wid(23),set_value=string(s2v(currentblock))

  widget_control,wid(22),set_value=s2v(currentblock)*1e7
  if slit_lock eq 1 then begin
    s3v(currentblock)=s2v(currentblock)
    widget_control,wid(25),set_value=s3v(currentblock)*1e7
    widget_control,wid(26),set_value=string(s3v(currentblock))
  endif  

  widget_control,wid(34),get_value=sep
  sep=float(sep(0))

  dtheata=(s2v(currentblock)>s3v(currentblock))/sep
  dtheata=dtheata*180/!pi

  res=dtheata/sample_angle(currentblock)
  widget_control,wid(45+currentblock),set_value=string(res)

  ;update graph if it is on
  if graph(3) eq 1 then begin
    updategraph,s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist
  endif

  update_ftper 
endif 

;-------------------------------------------------
;the text widget for s3 was altered
;-------------------------------------------------
if(ev eq 66) then begin

  ;update the position of slider s3
  val=long(val(0)*1e7)
  s3v(currentblock)=val*1e-7

  if s3v(currentblock) gt slit(1) then s3v(currentblock)=slit(1) 
  if s3v(currentblock) lt slit(0) then s3v(currentblock)=slit(0)
  widget_control,wid(26),set_value=string(s3v(currentblock))

  widget_control,wid(25),set_value=s3v(currentblock)*1e7
  if slit_lock eq 1 then begin
    s2v(currentblock)=s3v(currentblock)
    widget_control,wid(22),set_value=s2v(currentblock)*1e7
    widget_control,wid(23),set_value=string(s2v(currentblock))
  endif  

  widget_control,wid(34),get_value=sep
  sep=float(sep(0))

  dtheata=(s2v(currentblock)>s3v(currentblock))/sep
  dtheata=dtheata*180/!pi

  res=dtheata/sample_angle(currentblock)
  widget_control,wid(45+currentblock),set_value=string(res)

  ;update graph if it is on
  if graph(3) eq 1 then begin
    updategraph,s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist
  endif

  update_ftper
endif 

;-------------------------------------------------
;Slit lock button was pressed
;-------------------------------------------------
if(ev eq 75) then begin
  slit_lock=(1-slit_lock)
  widget_control,wid(22),get_value=pos
  widget_control,wid(25),set_value=pos
  if slit_lock eq 0 then widget_control,wid(35),set_value='OFF'
  if slit_lock eq 1 then widget_control,wid(35),set_value='ON'  
endif


;-------------------------------------------------
;On of the block buttons was pressed
;-------------------------------------------------
for u=1,numblocks do begin
  if(ev eq 53+u) then begin    
    currentblock=u-1
    if (u-1) eq 0 then begin
      widget_control,wid(14),set_value='ACTIVE'
      widget_control,wid(15),set_value='Block 2'
      widget_control,wid(16),set_value='Block 3'
    endif
    if (u-1) eq 1 then begin
      widget_control,wid(14),set_value='Block 1'
      widget_control,wid(15),set_value='ACTIVE'
      widget_control,wid(16),set_value='Block 3'
    endif
    if (u-1) eq 2 then begin
      widget_control,wid(14),set_value='Block 1'
      widget_control,wid(15),set_value='Block 2'
      widget_control,wid(16),set_value='ACTIVE'
    endif
    widget_control,wid(7),set_value=string(minq(currentblock))
    widget_control,wid(9),set_value=string(maxq(currentblock))
    widget_control,wid(19),set_value=string(currentblock+1)
    widget_control,wid(28),set_value=string(sample_angle(currentblock))
    widget_control,wid(22),set_value=s2v(currentblock)*1e7
    widget_control,wid(23),set_value=string(s2v(currentblock))
    widget_control,wid(25),set_value=s3v(currentblock)*1e7
    widget_control,wid(26),set_value=string(s3v(currentblock))
    ;update graph
    if graph(3) eq 1 then begin
      ;update graph if it is on
      updategraph,s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist
    endif
    update_ftper
  endif
endfor

;-------------------------------------------------
;the graph button was pressed
;-------------------------------------------------
if(ev eq 88) then graph


;-------------------------------------------------------
;the level ang resolution button was pressed for block 1
;-------------------------------------------------------
if(ev eq 89) then begin
  
  for u=0,numblocks-2 do begin
    f=make_same(0,u+1)
    if(f eq -1) then begin
      errormessage,"Slider Values Can Not be Found","For A Block",""
    endif else begin
      
      widget_control,wid(45),get_value=num
      widget_control,wid(46+u),set_value=num

      if(currentblock eq u+1) then begin
        widget_control,wid(22),set_value=f*1e7
        widget_control,wid(25),set_value=f*1e7
        widget_control,wid(23),set_value=string(f)
        widget_control,wid(26),set_value=string(f)
      endif

      s2v(u+1)=f
      s3v(u+1)=f

    endelse
  endfor
endif

;-------------------------------------------------------
;the level ang resolution button was pressed for block 2
;-------------------------------------------------------
if numblocks gt 1 then begin
 if(ev eq 90) then begin

  for u=0,numblocks-2 do begin

    if(u eq 0) then w=0
    if(u eq 1) then w=2

    f=make_same(1,w)

    if(f eq -1) then begin
      errormessage,"Slider Values Can Not be Found","For A Block ",""
    endif else begin
      
      widget_control,wid(46),get_value=num
      widget_control,wid(45+w),set_value=num

      if(currentblock eq w) then begin
        widget_control,wid(22),set_value=f*1e7
        widget_control,wid(25),set_value=f*1e7
        widget_control,wid(23),set_value=string(f)
        widget_control,wid(26),set_value=string(f)
      endif

      s2v(w)=f
      s3v(w)=f

    endelse
  endfor
 endif
endif

;-------------------------------------------------------
;the level ang resolution button was pressed for block 3
;-------------------------------------------------------

if numblocks gt 2 then begin
 if(ev eq 91) then begin
  for u=0,numblocks-2 do begin
    w=u
    f=make_same(2,w)
    if(f eq -1) then begin
      errormessage,"Slider Values Can Not be Found","For A Block ",""
    endif else begin
     
      widget_control,wid(47),get_value=num
      widget_control,wid(45+w),set_value=num

      if(currentblock eq w) then begin
        widget_control,wid(22),set_value=f*1e7
        widget_control,wid(25),set_value=f*1e7
        widget_control,wid(23),set_value=string(f)
        widget_control,wid(26),set_value=string(f)
      endif

      s2v(w)=f
      s3v(w)=f

    endelse
  endfor
 endif
endif

;----------------------------------------------------------
;The > button was pressed to bring up the parameters window
;----------------------------------------------------------
if ev eq 97 then submenu

;-------------------------------------------------
;the next button was pressed
;-------------------------------------------------
if(ev eq 76) then begin  
  ;save some variable for next program
  ;resolutions delta theata over theata
 
  for u=1,numblocks do begin
    widget_control,wid(44+u),get_value=num
    angres(u-1)=float(num)
  endfor
  
  ;reset for next part of program
  widget_control,/reset
endif

ppass:
return
end






;*************************************************************************************************
;*				Procedure Graph 
;*
;*This procedure draw the illumination graph
;*
;* Variables (local) :
;*
;* x                 : holds the data for the x axis of the graph
;* y                 : holds the data for the y axis of the graph
;* h                 : hold the result from the bob() function
;* 
;*
;*
;*************************************************************************************************


pro graph
common qs
common vars
common graph

if(graph(3) eq 0) then begin
  h=bob(s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist)
  grp=widget_base(title='Illumination Graph',uvalue='grp')
  graph(0)=widget_draw(grp,xsize=600,ysize=550,yoffset=0,xoffset=0,retain=2)
  graph(1)=widget_button(grp,value="RETURN",xoffset=0,yoffset=551,uvalue=300,xsize=300)
  graph(2)=widget_button(grp,value="ReDraw",xoffset=300,yoffset=551,uvalue=301,xsize=300)
  graph(3)=1 ;graph on
  widget_control,grp,/realize
  xmanager,'grp',grp
  x=[ll1,ll2,rr1,rr2]
  y=[0,1,1,0]
  plot,x,y,thick=2,title='Illumination Of The Sample, block '+strtrim(string(currentblock+1),2),$
xtitle='Position',ytitle='Intensity',xrange=[0,sample_lenght],yrange=[0,1.1]
endif else begin
  graph(3)=0
  widget_control,grp,/destroy
endelse

return
end





;*************************************************************************************************
;*				Procedure updategraph
;*
;*This procedure updates the illumination graph  
;*
;* Parameters :
;*
;* s2         : holds the value of s2 (slit 2)
;* s3         : holds the value of s3 (slit 3)
;* sam        : holds the sample angle
;* samlen     : holds the sample lenght
;* slsamlen   : hold the distance from the slit to the sample
;*
;* Variables  :
;*
;* x          : holds the data for the x axis of the data
;* y          : holds the data for the y axis of the data
;* h          : holds the value of the bob() function
;*
;*
;*
;*************************************************************************************************


pro updategraph,s2,s3,sam,samlen,slsamlen
common qs
h=bob(s2,s3,sam,samlen,slsamlen)
if h eq -1 then print,'Procedure updategraph : bob() return -1 (neutron divergence)'
x=[ll1,ll2,rr1,rr2]
y=[0,1,1,0]
plot,x,y,thick=2,title='Illumination Of The Sample, block '+strtrim(string(currentblock+1),2),$
xtitle='Position',ytitle='Intensity',xrange=[0,sample_lenght],yrange=[0,1.1]
return
end





;*************************************************************************************************
;*				Procedure grp_event 
;*
;* Parameters :
;*
;* event      : holds information about the interacted widget (type: structure)
;*
;* Variables  :
;*
;* ev         : holds the value of the user value of the widget
;* val        : holds the actual value of the widget
;* x          : holds the x axis data for the graph
;* y          : holds the y axis data for the graph
;*
;*
;*
;************************************************************************************************* 


pro grp_event,event
common graph
common qs
common vars

widget_control,event.id,get_uvalue=ev,get_value=val


;-------------------------------------------------
;Return button was pressed
;-------------------------------------------------
if(ev eq 300) then begin
  graph(3)=0
  widget_control,grp,/destroy
endif

;-------------------------------------------------
;The redraw button was pressed
;-------------------------------------------------
if(ev eq 301) then begin
  h=bob(s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist)
  x=[ll1,ll2,rr1,rr2]
  y=[0,1,1,0]

;-------------------------------------------------
;plot graph
;-------------------------------------------------
plot,x,y,thick=2,title='Illumination Of The Sample, block '+strtrim(string(currentblock+1),2),$
xtitle='Position',ytitle='Intensity',xrange=[-.001,sample_lenght+.001],yrange=[0,1.1] 
endif   

return
end





;*************************************************************************************************
;*				    Procedure update_ftper
;*
;*This procedure updates the footprint and the illumination.
;*
;* Variables :
;*
;* p         : hold the illumination as a percentage
;*
;*
;*
;*
;*
;*
;*
;*
;*************************************************************************************************


pro update_ftper
common qs
h=bob(s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist)

;-------------------------------------------------
;updates the footprint
;-------------------------------------------------
widget_control,wid(55),set_value=string(foot_print)
;updates illumination
if(ll1 ge 0.) and (rr2 le sample_lenght) then begin
  widget_control,wid(38),set_value='Under'
endif else begin
  if(ll1 lt 0.)or(ll1 gt sample_lenght) then begin
    widget_control,wid(38),set_value='Over'
    ll1=0
  endif
  if(rr2 gt sample_lenght)or(rr2 lt 0.) then begin
    widget_control,wid(38),set_value='Over'
    rr2=sample_lenght
  endif
endelse

;-------------------------------------------------
;update the illumination
;-------------------------------------------------
p=(rr2-ll1)/sample_lenght*100
widget_control,wid(56),set_value=string(p)  
return
end





;*************************************************************************************************
;*				    Function Make_same
;*
;*This procedure attempts to make the angular resolutions the same by altering s2 and s3. 
;*
;*
;* Parameters :
;* 
;* indexa     : block number of the angular resolution to be copied to the other blocks
;* indexb     : block number of the angular resolution to be copied over
;* ang1       : sample angle of block with indexa (degrees)
;* ang2       : sample angle of block with indexb (degrees)
;* s21        : contains the slit value of s2 for block with indexa
;* s31        : contains the slit value of s3 for block with indexb
;* new_s      : this is the new slit value of slits s2 and s3 for block with indexb
;* 
;*
;*
;*
;*************************************************************************************************


function make_same,indexa,indexb
common qs

ang1=sample_angle(indexa)
ang2=sample_angle(indexb)

s21=s2v(indexa)
s31=s3v(indexa)

new_s=(s21>s31)*ang2/ang1

if(new_s gt .01) or (new_s lt 0) then new_s=-1

return,new_s
end





;*************************************************************************************************
;*				    Procedure submenu 
;*
;*This procedure creates the submenu window for the intial setup window when the > button
;*is pressed.
;*
;*
;*
;*
;*
;*
;*************************************************************************************************


pro submenu
  common qs
  common slitstuff

  if submenu(0) eq 0 then begin
    sub=widget_base(title='Parameters',uvalue='sub')
    submenu(1)=widget_label(sub,value='Sample Length    :',yoffset=5)
    submenu(2)=widget_text(sub,value=string(sample_lenght),xoffset=230,/editable,uvalue=202)
    submenu(3)=widget_label(sub,value='Slit Separation  :',yoffset=45)
    submenu(4)=widget_text(sub,value=string(Slit_separation),xoffset=230,/editable,uvalue=204,$
yoffset=40)
    submenu(5)=widget_label(sub,value='Slit Sample Dist :',yoffset=87)
    submenu(6)=widget_text(sub,value=string(slit_sample_dist),xoffset=230,/editable,uvalue=206,$
yoffset=80)

    submenu(7)=widget_label(sub,value='OverLap Parameter:',yoffset=127)
    submenu(8)=widget_text(sub,value=string(perover*100),uvalue=208,xoffset=230,yoffset=120,$
/editable)
    submenu(9)=widget_label(sub,value='%',xoffset=490,yoffset=127)

    submenu(10)=widget_button(sub,value='RETURN',xoffset=0,yoffset=210,xsize=510,uvalue=210)

    submenu(11)=widget_label(sub,value='Save Name       :',yoffset=173,xoffset=0)
    submenu(12)=widget_text(sub,value=save_string,/editable,uvalue=212,xoffset=230,yoffset=167)
    ;max=12
    
    submenu(0)=1 ; screen on
    widget_control,sub,/realize
    xmanager,'sub',sub

  endif else begin
    widget_control,sub,/destroy
    submenu(0)=0
  endelse
return
end





;*************************************************************************************************
;*				  Procedure sub_event 
;*
;* Paramters :
;*
;* event     : This contains the information of the interacted widget (Structure)
;*
;* 
;* Variables :
;*
;* ev        : This contains the uservalue for the widget that has been interacted with
;* val       : This contains the value of the widget that has been interacted with
;* tmp       : Holds the sample angle in radians
;* tmp1      : Holds the angular resolution
;*
;*
;*
;*
;*************************************************************************************************


pro sub_event,event
  common qs
  common slitstuff
  common graph
  widget_control,event.id,get_uvalue=ev,get_value=val
  
  ;-------------------------------------------------
  ;Sample lenght has been altered
  ;-------------------------------------------------
  if ev eq 202 then begin
    sample_lenght=float(val(0))
    update_ftper
    ;update graph if it is on
    if graph(3) eq 1 then begin
      updategraph,s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist
    endif
    widget_control,wid(30),set_value=string(sample_lenght)
  endif

  ;-------------------------------------------------
  ;Slit sepation has been altered
  ;-------------------------------------------------
  if ev eq 204 then begin
    slit_separation=float(val(0))
        for u=0,numblocks-1 do begin
      tmp=sample_angle(u)/180*!pi
      tmp1=(s2v(u)>s3v(u))/slit_separation/tmp
      widget_control,wid(45+u),set_value=string(tmp1)
    endfor
    widget_control,wid(34),set_value=string(val(0))
  endif 

  ;-------------------------------------------------
  ;Slit to sample distance has been changed
  ;-------------------------------------------------
  if ev eq 206 then begin
    slit_sample_dist=float(val(0))
    update_ftper
    if graph(3) eq 1 then begin
      ;update graph if it is on
      updategraph,s2v(currentblock),s3v(currentblock),sample_angle(currentblock),sample_lenght,slit_sample_dist
    endif
    widget_control,wid(32),set_value=string(val(0))
  endif
  
  ;-------------------------------------------------
  ;The overlap parameter has been changed
  ;-------------------------------------------------
  if ev eq 208 then begin
    perover=float(val(0))/100.
    restart_flag=1
  endif

  ;-------------------------------------------------
  ;The return button has been pressed
  ;-------------------------------------------------
  if ev eq 210 then begin
    submenu(0)=0
    widget_control,sub,/destroy
  endif

  ;-------------------------------------------------
  ;The save string has been changed
  ;-------------------------------------------------
  if ev eq 212 then begin
    save_string=strtrim(string(val(0)),2)
  endif
    
return
end





;*************************************************************************************************
;*				Function deltax
;*
;*This function returns the delta x value. It is important to note any changes to the formulae
;*for delta x must also be changed in the lims() procedure.THIS procedure returns a VALUE not
;*the RANGE of delta x. ce bon.
;*
;* Parameters :
;* xd         : This contains the chopper distance (m)
;* xw         : This contains the angular velocity (rad/s)
;* xl         : This contains lamda
;* xphil      : This contains the chopper phase angle (degrees)
;*
;*
;* Variables  :
;*
;* xx         : Contains deltax
;*
;*
;*************************************************************************************************


;function deltax,xd,xw,xl,xphil

;common graph
;common vars
;common qs

;-------------------------------------------------
;1st contribution to delta x
;-------------------------------------------------
;xx=xd*sin(atan(xw*r*xl/k))

;-------------------------------------------------
;2nd contribution to delta x
;-------------------------------------------------
;xx=xx+(xphil/180.)*!pi*r*cos(atan(xw*r*xl/k))


;second contribution is zero if phi is negative

;-------------------------------------------------
;3rd contribution to delta x
;-------------------------------------------------
;xx=bob(s3v(currentblock),s2v(currentblock),90,chopper_width,slit_chopper_dist)

;return,xx
;end





;*************************************************************************************************
;*				 function time_modification 
;*
;*This procedure calculates the modification to the time resolution entered by the user in the
;*'data window', window.
;*
;* Parameters :
;*
;* qq         : The q value in (amstrongs)^-1
;* ww         : The angular velocity of the choppers
;* disd       : The distance from the choppers to the detector
;*
;* Variables   :
;*
;* xx          : Contains the effect width of the choppers
;* tmp         : Variable used for ease in the calculations
;* tmp1        : Variable used for ease in the calculations
;* modification: Contains the modification to the time resolution
;*
;*************************************************************************************************


function time_modification,qq,ww,disd

common qs
common vars
common graph

;----------------------------------------------------------------------------------
;Calculates the full width at half maximum on the chopper, ie the effective delta x
;----------------------------------------------------------------------------------
xx=bob(s3v(currentblock),s2v(currentblock),90,chopper_width,slit_chopper_dist)

tmp=xx/ww/r

;print,xx,s2v(currentblock),s3v(currentblock),ww,r

tmp1=disd*4*!pi*sin(sample_angle(currentblock)*!pi/180)/qq/k

modification=tmp/tmp1

return, modification
end





;*************************************************************************************************
;*				  Function refresh 
;*
;*This procedure redraw the graphs, the range window the data window, basically everything.
;*This is required because changing any of the alterable parameters changes the effective 
;*resolution miniumum and maximum.
;*
;* Variables  : 
;*
;* sresb(2)   : Contains the modified time resolutions
;* err        : Contains the return of the function lims, 0 means no errors occured
;*
;*
;*
;*************************************************************************************************


function refresh
common vars
common qs
common graph
  
   ;calculate effective resolutions
   sresb=fltarr(2)
   
   sresb(0)=sres(0,currentblock)-time_modification(minq(currentblock),wr(currentblock),disr(currentblock))*1e7
   sresb(1)=sres(1,currentblock)-time_modification(maxq(currentblock),wr(currentblock),disr(currentblock))*1e7

   ;find new limits
   err=lims(sresb(0)*1e-7,sresb(1)*1e-7,minq(currentblock),maxq(currentblock))
   ;if err gt 0 then print, 'No ranges exist'

   if err eq 0 then begin
     ;change slider limits
     widget_control,info(11),set_slider_min=dis(0)*1e7,set_slider_max=dis(1)*1e7
     widget_control,info(14),set_slider_min=d(0)*1e7,set_slider_max=d(1)*1e7
     widget_control,info(17),set_slider_min=w(0)*1e7,set_slider_max=w(1)*1e7
   endif

   ;redraw the paramters window
   redrawparams 

   ;redraw graphs
   drawplot

   ;redraw delta x
   ;widget_control,info(20),set_value='dx           :'+string(deltax(dr(currentblock),wr(currentblock),lamda(1),phil(currentblock)))
  
return,err
end



;templates
  
;*************************************************************************************************
;* 
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*
;*************************************************************************************************

;-------------------------------------------------
;
;-------------------------------------------------

;
;************************************************ End Of File ************************************************
;
