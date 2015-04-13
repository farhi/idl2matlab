; $Id: test_pcl.pro,v 1.2 1995/02/02 22:33:22 billo Exp $

pro pcl_plot_test
;+
;
;   Procedure to test the basic plotting capabilities of the pcl driver
;
;-
olddevice=!d.name
set_plot,'pcl'
device,/land,xsize=8,ysize=6,/inches,file='pcl_plot_test.pcl'
old_pmulti=!p.multi
!p.multi=[4,2,2,0,0]
x=cos(6*(findgen(60)+1)/!radeg)
y=sin(6*(findgen(60)+1)/!radeg)
plot,x,xtitle='!8Complex Italic',ytitle='!5Duplex Roman', $
  title='!12Dashed, Dotted, Diamonds!3',linestyle=2,xrange=[0,60],xsty=1
oplot,y,linestyle=0,psym=-4  ; diamonds connected by solid line
index=[indgen(60),reverse(indgen(60))]
fillpts=[x,reverse(y)]
polyfill,index,fillpts,spacing=0.8,orientation=45
polyfill,index,fillpts,spacing=0.8,orientation=135   ;Finish crosshatch
;
v=dist(100)
contour,v,levels=[10,20,30,40,50],c_labels=[1,1,1,1,1], $
  c_annotation=['!3Ten','!5Twenty','!6Thirty','!8Forty','!12Fifty!3']
;
v=dist(20)
surface,v
;
x=[0,0,4,4,0]
y=[0,2,2,0,0]
plot,x,y,/nodata,title='Test of PLOTS'
f=0.25
for i=1,4 do $
 plots,[0+i*f,0+i*f,4-i*f,4-i*f,0+i*f],[0+i*f,2-i*f,2-i*f,0+i*f,0+i*f],linest=i
xyouts,0,0,'!6Unclipped, enlarged line at 45 degrees!3',/noclip, $
 size=1.6,orientation=45
;
!p.multi=old_pmulti
device,/close
set_plot,olddevice
return
end





pro pcl_line_test
;+
;
;   Procedure to test the linestyle capability of the IDL pcl driver
;
;-
olddevice=!d.name
set_plot,'pcl'
device,/land,xsize=8,ysize=6,/inches,file='pcl_line_test.pcl'
plot,[0,1],[1,1],xstyle=1,xrange=[-0.2,1.2],ystyle=1,yrange=[0,2],/nodata, $
  xticks=14,xticklen=0.04,xtitle='14 Tick Intervals', $
  title='!8Test of Linestyles (Thick Lines)',ytitle='!8Avail. Styles!3'
for i=0,5 do begin
   oplot,[0,1],[0.1+(0.3*i),0.1+(0.3*i)],linestyle=i,thick=2.0
   xyouts,0.2,(0.2+(0.3*i)),'!8Linestyle: '+string(i,'(i1)')+'!3'
endfor
device,/close
set_plot,olddevice
return
end






pro pcl_image_test
;+
;
;   Procedure to generate a series of TV images on a page, for a pcl driver
;     test
;
;-
olddevice=!d.name
set_plot,'pcl'
v = bytscl(dist(900))
v=255-v*(v lt 115)
v(450,*)=0	;Dark line
v(*,450)=0	;Crossing dark line
device,resol=300,/land,xsize=2700,ysize=1000,xoffset=150,yoffset=150,/pixels, $
       /floyd,file='pcl_image_test.pcl'
tv,v,0
xyouts,300,920,/device,'!6Floyd'
device,/ordered
tv,v,1
xyouts,1200,920,/device,'Ordered'  ; Should continue to use !6 font
device,threshold=100               ; Should create small black swaths
tv,v,2
xyouts,(2100./2700.),(920./1000.),/normal,'!6Threshold!3'
device,/close
set_plot,olddevice
return
end





pro pcl_optimize_test
;+
;
;   Procedure to test landscape and portrait graphics using all three
;      optimization methods.  A total of six output files will be
;      generated.
;
;   Note: All three optimization levels (0,1,2) are used; since not all
;      devices will support all levels, garbage could result.
;
;-
olddevice=!d.name
set_plot,'pcl'
;
device,resol=75,/land,xsize=20,ysize=12.5,xoffset=2.5,yoffset=2.5, $
  optimize=2,file='pcl_opt2_land.pcl'
surface,dist(20),title='!6Opt=2, land, 75res, 20x12.5cm, 2.5x2.5cm off!3'
device,/close
;
device,resol=75,/land,xsize=450,ysize=300,xoffset=150,yoffset=150,/pixels, $
  optimize=1,file='pcl_opt1_land.pcl'
surface,dist(20),title='!6Opt=1, land, 75res, 450x300, 150x150 off!3'
device,/close
;
device,resol=100,/land,xsize=6,ysize=6,xoffset=2.0,yoffset=2.0,/inches, $
  optimize=0,file='pcl_opt0_land.pcl'
surface,dist(20),title='!6Opt=0, land, 100res, 6x6, 2x2 off!3'
device,/close
;
device,resol=150,/port,xsize=900,ysize=600,xoffset=150,yoffset=300,/pixels, $
  optimize=2,file='pcl_opt2_port.pcl'
surface,dist(20),title='!6Opt=2, port, 150res, 900x600, 150x300 off!3'
device,/close
;
device,resol=100,/port,xsize=4,ysize=6,xoffset=2.0,yoffset=2.0,/inches, $
  optimize=1,file='pcl_opt1_port.pcl'
surface,dist(20),title='!6Opt=1, port, 100res, 4x6, 2x2 off!3'
device,/close
;
device,resol=300,/port,xsize=6,ysize=6,xoffset=1.0,yoffset=1.0,/inches, $
  optimize=0,file='pcl_opt0_port.pcl'
surface,dist(20),title='!6Opt=0, port, 300res, 6x6, 1x1 off!3'
device,/close
;
set_plot,olddevice
return
end




pro test_pcl
;+
; NAME:
;	test_pcl
; PURPOSE:
;	To exercise and demonstrate various aspects of the HP PCL (Printer
;	Command Language) IDL driver.
; CATEGORY:
;	Graphics Drivers.
; CALLING SEQUENCE:
;	test_pcl - Calls all test procedures, which may also be individually
;		   called:
;	pcl_plot_test     - plot, contour, surface, plots, and vector fonts
;	pcl_line_test     - test of linestyles
;	pcl_image_test    - raster image output, with various dithering options
;	pcl_optimize_test - test file compression (optimization) methods
; INPUTS:
;	None
; OPTIONAL INPUT PARAMETERS:
;	None
; KEYWORD PARAMETERS:
;	None
; OUTPUTS:
;	Creates PCL disk files: pcl_plot_test.pcl, pcl_line_test.pcl,
;          pcl_image_test.pcl, pcl_opt*_land.pcl, and pcl_opt*_port.pcl.
;	   These files are not automatically submitted for printing; neither
;	   are they deleted automatically.
;	To print the .pcl files properly on a VMS system, it is recommended
;	   that the /PASSALL qualifier be used with the PRINT command, to
;	   pass all file information directly to the pcl printer.
; OPTIONAL OUTPUT PARAMETERS:
;	None
; COMMON BLOCKS:
;	None
; SIDE EFFECTS:
;	Many .pcl files will be created and left in the default directory.
; RESTRICTIONS:
;	Designed for IDL Version 2; not compatible with VMS IDL Version 1.
;
;	Procedure pcl_optimize_test uses all three (0,1,2) optimization
;	   levels; since some devices do not support all of these levels,
;	   garbage may result.  Specifically, the HP LaserJet II does
;	   not support optimization level 1; optimization 2 was primarily
;	   designed for it.  The HP DeskJet Plus supports all three
;	   optimizations.
; PROCEDURE:
;	The called routines try different permutations of the allowable
;	   keywords to the DEVICE procedure for PCL.  Vector plotting, contour,
;	   line drawing, and surface plots are generated, as well as a series
;	   of gray-scale images which test all dithering methods.  File
;	   optimization options are also tested.
; MODIFICATION HISTORY:
;	Written, June 1990, TJA
;-
pcl_plot_test
print,'File pcl_plot_test.pcl has been completed.'
pcl_line_test
print,'File pcl_line_test.pcl has been completed.'
pcl_image_test
print,'File pcl_image_test.pcl has been completed.'
pcl_optimize_test
print,'Files pcl_opt*_land.pcl, and pcl_opt*_port.pcl have been completed.'
print,' '
print,'*** All Finished ***'
return
end
