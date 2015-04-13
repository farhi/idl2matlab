; $Id: test_lj.pro,v 1.2 1995/02/02 22:34:31 billo Exp $

pro lj_loadct,sixteen=sixteen,eight=eight,four=four
;+
;
;   Procedure to load a group of 16, 8, or 4 (depending on keyword)
;	colors for the LJ-250 .  If 8 or fewer colors are to be defined, it is
;	assumed that 180 dpi resolution mode will be accessed.  In this case,
;	selection is from a pre-defined set of 8 colors. For 16 colors, any
;	combination of the 256 available LJ-250 colors may be selected.
;	The group chosen here is an attempt to select a table which uses
;	a variety of colors, while maintaining some gradation between color
;	intensities.
;
;-
if keyword_set(sixteen) then begin   ; 16 color specification is controllable
   ; [black,med_gray,lt_gray,lt_blue,royal_blue,blue,lavendar,
   ;  aqua,dark_green,green,lt_green,red,orange,yellow,lt_yellow,white]
   r=2.55*[4,15,43,38,2,5,20,2,4,12,37,53,72,89,89,90]
   g=2.55*[4,16,43,58,22,5,5,24,9,38,58,8,41,83,88,88]
   b=2.55*[8,18,45,78,64,31,29,41,11,18,49,14,13,13,54,85]
endif else if keyword_set(eight) then begin   ; 8 color selection is fixed
   ; [black,blue,yellow,magenta,cyan,red,green,white] ; Keep highest white
   r=[10,10,227,135,5,135,8,229]
   g=[10,10,212,13,56,20,66,224]
   b=[15,74,33,64,163,36,56,217]
endif else if keyword_set(four) then begin  ; Keep highest (background) white
   ; [black,blue,yellow,white]
   r=[10,10,227,229]
   g=[10,10,212,224]
   b=[15,74,33,217]
endif else print,'Unable to load lj color table; keyword expected.'
tvlct,r,g,b   ; Just loads the first 8 or 16 values, the rest are unchanged
return
end






pro lj_plot_test
;+
;
;   Procedure to test the basic plotting capabilities of the LJ-250 driver,
;     including color selection.
;
;-
olddevice=!d.name
set_plot,'lj'
device,depth=3,/land,xsize=8,ysize=6,/inches,file='lj_plot_test.lj'
old_pmulti=!p.multi
!p.multi=[4,2,2,0,0]
x=cos(6*(findgen(60)+1)/!radeg)
y=sin(6*(findgen(60)+1)/!radeg)
lj_loadct,/eight
plot,x,xtitle='!8Complex Italic',ytitle='!5Duplex Roman', $
  title='!12Dashed, Diamonds, Blue fill!3',linestyle=2,xrange=[0,60],xsty=1
oplot,y,linestyle=0,psym=-4  ; diamonds connected by solid line
index=[indgen(60),reverse(indgen(60))]
fillpts=[x,reverse(y)]
polyfill,index,fillpts,color=1   ; Should fill with blue
;polyfill,index,fillpts,spacing=0.8,orientation=135   ;Finish crosshatch
;
v=dist(100)
contour,v,levels=[10,20,30,40,50],c_labels=[1,1,1,1,1], $
  c_annotation=['!3Ten','!5Twenty','!6Thirty','!8Forty','!12Fifty!3'], $
  color=3,title='Five levels, Magenta'
;
v=dist(20)
surface,v,title='Simple Surface (no color)'
;
x=[0,0,4,4,0]
y=[0,2,2,0,0]
plot,x,y,/nodata,title='Test of PLOTS, red interior channel'
f=0.25
for i=1,4 do $
 plots,[0+i*f,0+i*f,4-i*f,4-i*f,0+i*f],[0+i*f,2-i*f,2-i*f,0+i*f,0+i*f],linest=i
xcoord=[0.1,3.9,3.9,0.1,0.1,0.2,3.8,3.8,0.2,0.2]
ycoord=[0.1,0.1,1.9,1.9,0.1,0.2,0.2,1.8,1.8,0.2]
polyfill,xcoord,ycoord,color=5  ; fill interior box with red
xyouts,0,0,'!6Unclipped, enlarged line at 45 degrees!3',/noclip, $
 size=1.6,orientation=45
;
!p.multi=old_pmulti
device,/close
set_plot,olddevice
return
end





pro lj_line_test
;+
;
;   Procedure to test the monochrome linestyle capability of the IDL 
;     LJ-250 driver.
;
;-
olddevice=!d.name
set_plot,'lj'
device,/land,depth=1,xsize=8,ysize=6,/inches,file='lj_line_test.lj'
plot,[0,1],[1,1],xstyle=1,xrange=[-0.2,1.2],ystyle=1,yrange=[0,2],/nodata, $
  xticks=14,xticklen=0.04,xtitle='14 Tick Intervals', $
  title='!8Test of Linestyles (Thick Lines)',ytitle='!8Avail. Styles!3'
for i=0,5 do begin
   oplot,[0,1],[0.1+(0.3*i),0.1+(0.3*i)],linestyle=i,thick=5.0
   xyouts,0.2,(0.2+(0.3*i)), $
    '!8Linestyle: '+string(i,'(i1)')
endfor
device,/close
set_plot,olddevice
return
end






pro lj_image_test
;+
;
;   Procedure to generate a series of TV images on a page, for an LJ-250 driver
;     test.  Monochrome (depth=1) is used, so that the dithering may be tested.
;
;-
olddevice=!d.name
set_plot,'lj'
v = bytscl(dist(600))
v=255-v*(v lt 180)
v(300,*)=0	;Dark line
v(*,300)=0	;Crossing dark line
device,resol=180,/land,xsize=10*180,ysize=6*180,xoffset=90,yoffset=180, $
       /pixels,/floyd,file='lj_image_test.lj',depth=1
tv,v,0,180   ; Move up 1 inch to allow room for label below
xyouts,200,90,/device,'!6Floyd'
device,/ordered
tv,v,690,180  ; Should be 1/2 inch gap between images
xyouts,890,90,/device,'Ordered'  ; Should continue to use !6 font
xyouts,640,180,/device,orientation=90,'1/2 inch gap here, 90 deg. rotation'
device,threshold=100              ; Should create small black swaths
;tv,v,1380,180  ; NOTE: this doesn't work because monochrome, & outside area
tv,v(0:399,*),1380,180   ;Deliberately avoid last 200 pixels to fit plot area
xyouts,(1580./1800.),(90./1080.),/normal,'!6Threshold!3'
xyouts,1330,780,/device,orientation=270,'270 deg. rot., last inch truncated'
device,/close
set_plot,olddevice
return
end





pro lj_resolution_test
;+
;
;   Procedure to test landscape and portrait graphics using both 90 and 180 dpi
;      LJ-250 resolutions.  A total of four output files will be generated.
;
;-
olddevice=!d.name
set_plot,'lj'
;
device,depth=4,resol=90,/land,xsize=20,ysize=12.5,xoffset=2.5,yoffset=2.5, $
  file='lj_90_land_cm.lj'
lj_loadct,/sixteen    ; Load first 16 indices in color table with known colors
surface,dist(20),title='!6land, 90res, 20x12.5cm, 2.5x2.5cm off, color Red!3',$
   color=11
device,/close
;
device,depth=4,resol=90,/port,xsize=6,ysize=4,xoffset=1,yoffset=1,/inches, $
  file='lj_90_port_inches.lj'
lj_loadct,/sixteen    ; Load first 16 indices in color table with known colors
surface,dist(20),title='!6port, 90res, 6x4in, 1x1 off, color Lt. Gray!3', $
   color=2
device,/close
;
device,depth=3,resol=180,/land,xsize=6,ysize=6,xoffset=2.0,yoffset=2.0, $
  /inches,file='lj_180_land_inches.lj'
lj_loadct,/eight    ; Load first 8 indices in color table with known colors
surface,dist(20),title='!6land, 180res, 6x6in, 2x2 off, color Cyan!3', $
   color=4
device,/close
;
device,depth=3,resol=180,/port,xsize=1080,ysize=720,xoffset=180,yoffset=360, $
  /pixels,file='lj_180_port_pixels.lj'
lj_loadct,/eight    ; Load first 8 indices in color table with known colors
surface,dist(20),title='!6port, 180res, 1080x720, 180x360 off, color Green!3',$
   color=6
device,/close
;
set_plot,olddevice
return
end




pro lj_depth_test
;+
;
;    Procedure to create two disk files illustrating different depth
;	(bit plane) renditions available on the LJ-250 .
;
;-
olddevice=!d.name
set_plot,'lj'
image=dist(360)
;
device,/port,resol=180,depth=3,xsize=3,ysize=3,/inches, $
   file='lj_depth123_test.lj'
lj_loadct,/eight   ; load THE 8 colors for use in 180 dpi mode
tv,bytscl(image,top=7),0,90
xyouts,0,40,'Depth 3, 180 dpi, Portrait',/device
;
device,/land,depth=2,resolution=90,xsize=5,ysize=5,/inches
lj_loadct,/four   ; load 4 colors for use in 180 dpi mode, 2 bit planes
tv,bytscl(image,top=3),180,90   ; Should cleanly truncate last 1 inch on right
xyouts,270,50,'Depth 2, 90 dpi, Land, last inch truncated',/device
xyouts,100,90,orient=90,'Right inch of image & text should be clipped',/device
;
device,/port,depth=1,resolution=180,xsize=3,ysize=3,/inches
tv,image,90,90
xyouts,90,40,'Depth 1, 180 dpi, Portrait, Mono',/device
device,/close
;
device,/land,resol=90,depth=4,file='lj_depth4_test.lj'
lj_loadct,/sixteen   ; Load 16 colors for use at 90 dpi resolution
image=bytscl(image,top=15)  ; Set image range to match 16 selected colors
for i=0b,15 do image(22*i,0)=bytarr(22,22)+i   ; Place color bar at bottom
tv,image,0,90
xyouts,90,45,'Depth 4, 90 dpi, Land, 16 color bar',/device
device,/close
set_plot,olddevice
return
end





pro lj_show3_test
;+
;
;   Procedure to create an output file for use on the LJ-250, which will
;     use the library procedure SHOW3.
;
;-
olddevice=!d.name
set_plot,'lj'
device,/land,depth=3,resolution=180,filename='lj_show3_test.lj'
ljlct   ; Test this routine to load the default palette
show3,dist(16)
device,/close
set_plot,olddevice
return
end






pro test_lj
;+
; NAME:
;	test_lj
; PURPOSE:
;	To exercise and demonstrate various aspects of the IDL driver for
;	   the Digital Equipment Corporation LJ-250 color printer.
; CATEGORY:
;	Graphics Drivers.
; CALLING SEQUENCE:
;	test_lj - Calls all test procedures, which may also be individually
;		   called:
;	lj_plot_test       - plot, contour, surface, plots, and vector fonts
;	lj_line_test       - test of linestyles
;	lj_image_test      - raster image output, for various dithering options
;	lj_resolution_test - test the two available resolutions (90 and 180dpi)
;	lj_depth_test      - test different depth (bit plane) renditions
;	lj_show3_test      - test the output using the SHOW3 procedure
; INPUTS:
;	None
; OPTIONAL INPUT PARAMETERS:
;	None
; KEYWORD PARAMETERS:
;	None
; OUTPUTS:
;	Creates LJ-250 compatible disk files: lj_plot_test.lj, lj_line_test.lj,
;          lj_image_test.lj, lj_90*.lj, lj_180*.lj, lj_depth123_test.lj,
;	   lj_depth4_test.lj, and lj_show3_test.lj .
;	   These files are not automatically submitted for printing; neither
;	   are they deleted automatically.
;	To print the .lj files properly on a VMS system, it is recommended
;	   that the /PASSALL qualifier be used with the PRINT command, to
;	   pass all file information directly to the lj printer.
; OPTIONAL OUTPUT PARAMETERS:
;	None
; COMMON BLOCKS:
;	None
; SIDE EFFECTS:
;	Many .lj files will be created and left in the default directory.
; RESTRICTIONS:
;	Designed for IDL Version 2; not compatible with VMS IDL Version 1.
; PROCEDURE:
;	The called routines try different permutations of the allowable
;	   keywords to the DEVICE procedure for the LJ-250.  Vector plotting,
;	   contour, line drawing, and surface plots are generated, as well 
;	   as a series of images which test all dithering methods.
;	   The two resolutions of the LJ-250 are tested, and specific color
;	   maps are used and tested.
; MODIFICATION HISTORY:
;	Written, August 1990, TJA (derived from demo_pcl)
;-
lj_plot_test
print,'File lj_plot_test.lj has been completed.'
lj_line_test
print,'File lj_line_test.lj has been completed.'
lj_image_test
print,'File lj_image_test.lj has been completed.'
lj_resolution_test
print,'Files lj_90*.lj, and lj_180*.lj have been completed.'
lj_depth_test
print,'Files lj_depth123_test.lj, and lj_depth4_test.lj have been completed.'
lj_show3_test
print,'File lj_show3_test.lj has been completed.'
print,' '
print,'*** All Finished ***'
return
end
