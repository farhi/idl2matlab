;**************************************VENUS***************************************************
;Simple fitting program by Ross McKeown and Paolo Radaelli



;the first part of this program defines the functions needed for curve_fit 
; a gaussian function with gamma=FWHM

function gauss, x,x0,gamma

sigma=gamma/sqrt(8.*alog(2.))
y=1/sqrt(2.*!pi*sigma^2.0)*exp(-((x-x0)^2.0/2.0/sigma^2.0))

return, y

end

; the derivative of a gaussian function with gamma=FWHM with respect to x0

function dgaussdx0,x,x0,gamma

sigma=gamma/sqrt(8.*alog(2.))
y=gauss(x,x0,gamma)/sigma^2.0*(x-x0)

return, y

end

; the derivative of a gaussian function with gamma=FWHM with respect to gamma

function dgaussdgamma, x,x0,gamma

sigma=gamma/sqrt(8.*alog(2.))
y=gauss(x,x0,gamma)*((x-x0)^2/sigma^3.0-1.0/sigma)/sqrt(8.*alog(2.))

return, y

end
; a lorentzian function with gamma=FWHM

function lorentz, x,x0,gamma

y=gamma/2./!pi/((gamma/2.)^2+(x-x0)^2)


return, y

end

; the derivative of a lorentzian function with respect to x0

function dlorentzdx0, x, x0, gamma

y=lorentz(x,x0,gamma)*2*(x-x0)/((gamma/2.)^2+(x-x0)^2)

return, y

end

; the derivative of a lorentzian function with respect to gamma

function dlorentzdgamma, x, x0, gamma

y=lorentz(x,x0,gamma)*(1/gamma-gamma/2/((gamma/2.)^2+(x-x0)^2))

return, y

end
; a pseudovoigt function

function psvoigt, x,x0,gamma,eta

y=eta*lorentz(x,x0,gamma)+(1-eta)*gauss(x,x0,gamma)


return, y

end

; its derivative with respect to x0

function dpsvoigtdx0,x,x0,gamma,eta

y=eta*dlorentzdx0(x,x0,gamma)+(1-eta)*dgaussdx0(x,x0,gamma)

return, y

end

; its derivative with respect to gamma

function dpsvoigtdgamma,x,x0,gamma,eta

y=eta*dlorentzdgamma(x,x0,gamma)+(1-eta)*dgaussdgamma(x,x0,gamma)

return, y

end

; its derivative with respect to eta

function dpsvoigtdeta, x, x0, gamma, eta

y=lorentz(x,x0,gamma)-gauss(x,x0,gamma)

return, y

end

;this procedure constructs the functions and their derivatives from the
;expressions given above, for any number of peaks. The function created here
;will be entered into the curve_fit procedure. 


pro zfunct, x, a, f, pder
  n = n_elements(a)
  m=(n-4)/2
  f = a(0) + a(1)*x
  pder= [[x*0. +1.0],[x]]
  dg=x*0.
  de=x*0.

  for i =0,(m-1) do begin
     dg = dg+ a(2*i+4)*dpsvoigtdgamma(x,a(2*i+5),a(2),a(3))
     de = de+ a(2*i+4)*dpsvoigtdeta(x,a(2*i+5),a(2),a(3))
  endfor
  pder=[[pder],[dg],[de]]
 for i = 0,(m-1) do begin
  f = f +a(2*i+4)*psvoigt(x,a(2*i+5),a(2),a(3))
   di=psvoigt(x,a(2*i+5),a(2),a(3))
   dx0=a(2*i+4)*dpsvoigtdx0(x,a(2*i+5),a(2),a(3))
   pder=[[pder],[di],[dx0]]
   
 endfor

end



 
 
pro windb_event, event


;this is the event handler for the events that occur in the  base widget
;named bad
 
common block1,x,y,w,index,px,py, status, xarr,yarr,menu5,range,a,value,$
   menu4,row1,row2, $
   menu3,yfit,eta_input,gamma_input,sigmaa,b,l1,win_num,dat,$
   write, files,text, base,chi_new,f, convstr

common block, pr,prin, prindex

;this list explains the variables in the common block above,
;x:x values
;y:y values
;w:instrumental weighting for the values in the curve_fit procedure
;index:the variable which deals with the case statement for plotting and peak 
;      assignment
;px,py:x and y arrays containing the coordinates for the zoom window 
;status:variable which defines whether a zoom or an assignment procedure is activated
;xarr,yarr:the arrays containing the arrays of the x and y points selected
;menu5:widget which is desensitised/sensitised during the program
;range:defines the range of x and y coordinates
;a:the array which contains all the values necessary for curve_fit
;value:variable which controls whether a zoom or a peak assignment assignment button is selected
;men4,row1,row2:widget which is desensitised/sensitised during the program
;               widget which is desensitised/sensitised during the program
;               widget which is desensitised/sensitised during the program
;menu3:This widget id is stored so that its buttons can be changed non-interactively
;yfit:the array containing the y values of the fitted curve
;eta_input:id of  the widget which inputs an eta value into a array
;gamma_input:id of the widget which inputs an gamma value into a array


;sigmaa:The array containing the the errors of the a array values
;b: A string array containing the information which is  used for the screen and  printer output.
;l1:A string which contains the preliminary information for print out on screen
;win_num: Contains the window number of the draw widget 
;dat: widget id of the cw_field widget. The  file name is entered through 
;this widget.
;write:This  variable for the needed for sensitising/desensitising routine and for the widget_control call
;files:The value of the widget named above(write) which sets the criteria as to whether the file is appended or not 
;text:widget id if the text widget. It is called to widget_control to input the text of the data which is displayed on
; screen
;base:Base widget containing the widgets which output the data to the screen. They are sensitised after peaks  
;   have been  assigned.
;chi_new:The chi squared value of the fitted points.
;f : String array containing the parameter names. Used in the print out to the screen.
;convstr:The string containing the message which tells the user whether convergence  has been completed

;the first case statement looks for the name of the event produced 
;the routine below either performs the zoom  procedure
;
  
name=tag_names(event,/structure_name)


	   case name of
 'WIDGET_DRAW': begin
 		;Tells the program which window to look at.
  		wset,win_num
 		;converting the x and y values in pixels to coordinates
                d=convert_coord(event.x,event.y,/device,/to_data)
                case status of
                   'z': begin
                          if event.type eq 0 then begin
                     	    px(index)=d(0)
            		    py(index)=d(1)
            			 case index of 
               			    0:begin
				      index=1
				      device, cursor_standard=78
				      end
               			    1:begin
                		      index=0
				      device, cursor_standard=76
                		      ;assigns the size of the zoom region from px, py(arrays containing the x and y 
                		      ;points which are selected by the user.)
                                      range=where(x ge min([px(0),px(1)]) and x le max([px(0),px(1)]))
                                      if range(0) ne -1 then plot,x(range),y(range),/ynoz $
                                       ,xtitle='2*theta (degrees)',ytitle='Intensity (counts)'
                                       ;overplotting the peak assignments as crosses. This statement occurs here in
                                       ;case the zoom procedure is activated after peak assignment.
                                      oplot,xarr,yarr,psym = 1,symsize = 3  
                                      end
            			 endcase
           		  endif
                        end
                         
 		   'p': begin
                          ;this procedure plots the  points which are selected and  describes         
               	          ;them crosses, the coordinates are entered into the appropriate array: xarr
         	          ;or yarr,the arrays always contain a '0' at the beginning which is
         	          ;ignored whenever the values are taken from the array
         	          ;event.type eq 0 specifies that an event is only created when the button is pushed
         	          if event.type eq 0   then begin
              	               
         	          ;In case the range array is empty. The zeroth element always contains -1 in this case.
         	          if range(0) eq -1  then begin
         	            mess13=widget_message('range too small',title='assignment error')
         	            endif else begin
         	              ;only assigns peak coordinates to the xarr and yarr if they are selected in the correct range.i.e. within the 
         	              ;axis boundaries                                        
                              oplot, [d(0)], [d(1)], psym=1,symsize=3  
                              if d(0) ge min(x(range)) and d(0) le max(x(range)) $
                                 and d(1) ge min(y(range))$
            	                 and d(1) le max(y(range)+y(range)*.2)  then begin 
             		         xarr=[xarr,d(0)]               
                       	         yarr=[yarr,d(1)]
          	              endif 
           	              ;conversion of the y coordinates into intensities, these intensities 
           	              ;are needed as parameters in the zfunct.procedure
                              i =d(1)/(a(3)*(2/(!pi*a(2)))+((1-a(3))*sqrt(8*alog(2))$
             	              /(a(2)*sqrt(2*!pi))))             	            
             	             ;incrementing the a array with x and intensity values
                             a=[a,i]
                             a=[a,d(0)]
                             sigmaa=a*0.
                             ;sensitising the widgets below                                           	             
                             widget_control,menu5,sensitive=1
                             widget_control,eta_input,sensitive=1
                             widget_control,gamma_input,sensitive=1
                           endelse     
                         endif
                     end
                  endcase
         
          
               end
               
       ; The case  when an event is generated by the droplist widget.        
       'WIDGET_DROPLIST':begin
       
	     common block, pr,prin, prindex
             prindex=event.index
             		
             end  
               
    ;the third kind of event ,'', is handled here           
               
   '': begin

;this case statement  links the selected button to a specific routine. The event values returned are the names of the buttons 

     case event.value of
        'done':begin
		device, cursor_standard=34  ;	reset the cursor
		widget_control,event.top,/destroy 
	       end
        'replot':begin
        	;This case resets all the values but keeps the same range.
        	;The statement below checks if the range contains some values(see common block1'explanation of variables)
        	if range(0) ne -1  then begin
        	  ;resets the window to display one plot only.
                  !p.multi = 0
      	          yfit=dblarr(1)
                  plot,  x(range),y(range),/ynoz $
     		  ,xtitle='2*theta (degrees)',ytitle='Intensity (counts)'	        
 	          a=[2,4,0.2,0.0]
 	          sigmaa=a*0.
                  yarr=dblarr(1)
                  xarr=dblarr(1)
                  ;sensitises the following widgets.
                  widget_control,menu5,sensitive=0
                  widget_control,eta_input,sensitive =0                                        
                  widget_control,gamma_input,sensitive =0
                  widget_control,base,sensitive=0
      		 endif 
		 if status eq 'z' then begin
;		   *** reset the zoom
		   index=0 
	           device, cursor_standard=76
		 endif
 	       end
 	 'unzoom 20%':begin
 	 	    if range(0) ne -1  then begin
 	 	      !p.multi = 0 
 	 	      ;This case defines min(x), max(x) in terms of the range, increases the range by 20% and then resizes the
 	 	      ;the plot. 
 	 	      g=min(x(range))
 	 	      h=max(x(range))
 	 	      half=(h-g)/2
 	 	      cen= (h+g)/2
 	 	      d=(h-g) 	 	     
 	 	      if  range(0) ne -1 then plot,x(range),y(range),/ynoz 
 	 	      ;This if  statement deals with the case when 
 	 	      ;min(x) or max(x) are equal to nought. 	     
 	 	      if g eq 0 or h eq 0 then begin 	 	 	 	     
 	 	         g = g - .2*(h-g)
 	 	         h=h+.2*(h-g)	 	     
 	 	      endif
 	 	      en=(abs(h-g))*1.2
 	  	      range = where(x ge (cen -(en)/2) and x le $
 	  	      ((cen +(en)/2))) 	  	      	  	   
 	  	      plot,x(range),y(range),/ynoz $
 	  	      ,xtitle='2*theta (degrees)',ytitle='Intensity (counts)'
 	  	      if n_elements(xarr) ge 2 then begin
 	  	         oplot,xarr(1:*),yarr(1:*),psym = 1
 	  	      endif
		      if status eq 'z' then begin
;		        *** reset the zoom
		        index=0 
	                device, cursor_standard=76
		      endif

 	  	    endif  
               end
        'next range': begin
        	      if range(0) ne -1  then begin
                        !p.multi=0
                        yfit=dblarr(1)
         	        g=min(x(range))
 	 	        h=max(x(range))
 	 	        ;oldrange created here so that the original range can be used in case the range is empty 
 	 	        oldrange=range
 	 	        ;redefines the range.
 	 	        range=where(x gt h and x le 2*h-g)
 	 	        if range(0) ne -1 then begin
 	 	            ;resets and sensitises the values and widgets below
 	                    plot,x(range),y(range),/ynoz
  	                    a=[2,4,0.2,0.0]
 	                    sigmaa=a*0.
                            yarr=dblarr(1)
                            xarr=dblarr(1) 
                            widget_control,menu5,sensitive=0
                            widget_control,eta_input,sensitive =0                                        
                            widget_control,gamma_input,sensitive =0
      		            widget_control,base,sensitive=0
                       endif else begin
                         range=oldrange
                         plot,x(range),y(range),/ynoz $
                         ,xtitle='2*theta (degrees)',ytitle='Intensity (counts)'
                      endelse
		      if status eq 'z' then begin
;		        *** reset the zoom
		        index=0 
	                device, cursor_standard=76
		      endif

		      endif
                      end
          
       'peak assignment': begin
	                  status='p';referring the program to case statement above.(See events created by widget_draw)
			  device, cursor_standard=34
	                  end
       'zoom': begin
	       status='z';referring the program to case statement above.
	       device, cursor_standard=76
	       end
       'reset':begin
       	       ;resets all the values and the plot and then sensitises all the widgets which were active at the beginning 
               !p.multi = 0
               yfit=dblarr(1)
               a=[2,4,0.2,0.0]
               sigmaa=a*0.
               yarr=dblarr(1)
               xarr=dblarr(1)
      	       plot,x,y,/ynoz
               status = 'z'
;		reset the zoom
		index=0 
	        device, cursor_standard=76
              range = where(x ge min(x) and x le max(x))
               widget_control,menu4,/set_value
               value = 1
               widget_control,menu5,sensitive=0
               widget_control,eta_input,sensitive =0                                        
               widget_control,gamma_input,sensitive =0
               widget_control,base,sensitive=0      		    
               end
               
     ;this button, as the name suggests performs the curve fitting procedure
       'fitit': begin
       		;checks if there are enough points to fit.
       	        widget_control,base,sensitive=1    
       		; set the cursor to a watch
		device, cursor_standard=150
                if n_elements(y(range)) ge 7 then begin	
                     ;sets the values of eta and gamma from the input window.              		
                     widget_control,eta_input, get_value =eta
                     ;resets eta if the value is too large or too small.
                     if eta ge -2 and eta le 2 then a(3) = eta                   
                     widget_control,gamma_input, get_value =gamma
                     if gamma ge -2 and gamma le 2 then a(2) = gamma 
                     ;if the curve _fit procedure is called for the first time then chi_old is not calculated.
                     ;The points are always fitted twice when they are fitted for the first time.                    
                     if n_elements(yfit) eq 1 then begin                           			
                         ; the calling procedure for the curve_fit procedure
                         yfit=curvefit(x(range),y(range),w(range),a,sigmaa,$
                         function_name='zfunct')
                         ;reassigns the value of eta if the curve_fit  procedure produces a value which 
 	     	         ;is unacceptable
 	     	         if a(3) lt 0 then a(3) = 0
 	    	         if a(3) gt 2 then a(3) = 1

                    endif
                    ;chi squared values for the values before the curve_fit is executed for the second time.
                    chi_old = total(w(range)*(y(range)-yfit)^2)/$
 	     	    (n_elements(x(range))-n_elements(a))                  
        	    ; the calling procedure for the curve_fit procedure
                    yfit=curvefit(x(range),y(range),w(range),a,sigmaa,$
                    function_name='zfunct') 
                    ;chi squared values for the fitted points               	            		       		 	                      	     		
 	     	    chi_new = total(w(range)*(y(range)-yfit)^2)/$
 	     	    (n_elements(x(range))-n_elements(a))	     	    
 	     	    ;error statement in case the curves are not converging. Convergence is tested solely on the  change in chi
 	     	    ;squared values 	
 	     	    if chi_new lt (1.001*chi_old)  and chi_new gt $ 
 	     	    (chi_old -0.001*chi_old)  $
 	     	    ;string which is entered into the output window stating whether convergence is achieved
 	     	    then begin	     	    
 	     	       convstr='Convergence was achieved'
 	     	    endif else begin 	     	    
 	     	        convstr='Convergence was not achieved' 	     	     
 	     	    endelse 	     	
 	    	    xarr=dblarr(1)
 	            yarr=dblarr(1)
 	            a_num = n_elements(a)	            
 	            x_num = (a_num-4)/2
 	            ;redraws the cross position from the values created from curve_fit
 	            for i=0,x_num-1 do begin
 	              xarr = [xarr,a(2*i+5)]
 	              yarr = [yarr,a(2*i+4)*(a(3)*(2/(!pi*a(2)))+((1-a(3))*sqrt(8*alog(2))$
             	           /(a(2)*sqrt(2*!pi))))+a(0)+a(2*i+5)*a(1)]
 	            endfor
 	            ;resets the window to display one plot only.	           
                    !p.multi = 0 
                    ;plots all the results 	           		 
                    plot,x(range),y(range),psym = 1,$
                    yrange=[min(y(range)-yfit)+min(y(range))-0.1*(max(y(range))-min(y(range)))$
                    ,max(y(range))],xtitle='2*theta (degrees)',ytitle='Intensity (counts)'
                    oplot,xarr(1:*),yarr(1:*), psym=1,symsize =3
                    oplot, x(range), yfit
                    oplot,x(range),(y(range)-yfit)+min(y(range))-0.1*(max(y(range))-min(y(range)))                                         
                  endif else begin
                     ;error message widget.
                     mess12=widget_message('not enough data points',title='fit error')
                  endelse
                  
                  ;    the following block displays the results
                  ;f, l1 and b are explained above in the common block explanations
                  ;These string arrays are created here with carriage returns and tabs in place.
		  chi_new=string(chi_new)
		  f=strarr(n_elements(a))					
  	          f(0)='bk1'+string(9b)+string(9b)
  	          f(1)='bk2'+string(9b)+string(9b)
  	          f(2)='FWHM'+string(9b)+string(9b)
  	          f(3)='eta'+string(9b)+string(9b)
  	          if n_elements(a) gt 4 then begin
  	              ;for loop creating the parameter words for the peaks.
  	              for i=0, ((n_elements(a)-4)/2)-1  do begin
  	                  f(2*i+4)='peak '+ string(format='(I2)',(i+1))+' Int'+string(9b)
  	                  f(2*i+5)='peak '+ string(format='(I2)',(i+1))+' Pos'+string(9b)
  	              endfor
  	          endif
  	          b=f+string(format='(f10.4)',(a))+string(9b)+string(format='(f10.6)',(sigmaa))     
  	          l1='chi squared value:  '+chi_new +string(10b)+string(10b)+convstr $
  	          +string(10b)+string(10b)+'parameter'+string(9b)+'value'$
           	  +string(9b)+string(9b)+'error'+string(10b)
           	  ;sets the initial value of the text widget to l1
           	  ;The for loop then appends the text widget with the b array              
  	          widget_control,text,set_value = l1		
 	          if n_elements(a) ge 4 then begin  	                  	  	           	  	          	  	           	  	          	  	         
 	  	      for i=0, n_elements(a)-1 do $
  	  	      widget_control, text, set_value=b(i), /append 	  	        
	           endif
	           ;returns the cursor to the top of the page once all the data has been copied into the text widget 
	           widget_control,text,set_text_select=[0]
	           ;only sensitises the input widget when a file name has been entered
	           if files(n_elements(files)-1) eq '' then widget_control, write, sensitive=0 
  	  	   widget_control,base,/realize
  	  	   ;Event manager for the base widget base.     
  	  	   xmanager,'windb',base 
		   ; returns the cursor to a crosshair
		   device, cursor_standard=34
  	  	   end
                        
                   
                         ;the buttons below enable different perspectives of the plotted results  
       'split screen':begin
                     ;This line creates a window in which two plots can be displayed.
                     !p.multi= [0,1,2]
            
              
     	              plot,      x(range),y(range),psym = 1,/ynoz,xtitle='2*theta (degrees)',ytitle='Intensity (counts)'
     	             
 	       	      oplot,xarr(1:*),yarr(1:*), psym=1,symsize =3
              	      oplot, x(range), yfit
              	      oplot,x(range),(y(range)-yfit)
               	      plot, x(range),(y(range)-yfit)
             	      title ='observed minus expected'
             		
                      end
           'single screen':begin
           		
                    !p.multi = 0 
                    plot,x(range),y(range),psym = 1,$
                    yrange=[min(y(range)-yfit)+min(y(range))-0.1*(max(y(range))-min(y(range)))$
                    ,max(y(range))],xtitle='2*theta (degrees)',ytitle='Intensity (counts)'
                    oplot,xarr(1:*),yarr(1:*), psym=1,symsize =3
                    oplot, x(range), yfit
                    oplot,x(range),(y(range)-yfit)+min(y(range))-0.1*(max(y(range))-min(y(range)))
              	    end
         
         'print':begin	
          	 ;records th name of the present device.          	    
	         mydevice=!d.name
	         ;sets the device to postscript
	         set_plot,'PS'
	         ;performs the plot 	    	    	    	    	    	    
	         plot,x(range),y(range),psym = 1,$
                 yrange=[min(y(range)-yfit)+min(y(range))-0.1*(max(y(range))-min(y(range)))$
                 ,max(y(range))],xtitle='2*theta (degrees)',ytitle='Intensity (counts)'                 
                 ;+.1*max(y)+.01*(n_elements(xarr))
                 oplot,xarr(1:*),yarr(1:*), psym=1,symsize =3
                 oplot, x(range), yfit
                 oplot,x(range),(y(range)-yfit)+min(y(range))-0.1*(max(y(range))-min(y(range)))
                 ;creates string array fo numbering peaks
                 peak=indgen(n_elements(xarr))
                 peaks=string(format='(I2)',(peak)) 
                 ;xyouts outputs data  onto the screen. The statement here assigns numbers to the peaks 		            
                 xyouts,xarr(1:*),yarr(1:*)*1.1,peaks(1:*)          
                 l2='chi squared value'+chi_new
                 l3=convstr
                 ;the xyouts below put the data onto the plot to be printed out.
                 ;this ouput function can be called more than once, as is observed.
                 xyouts, 0.1, -0.1, l2, /norm,charsize=0.5
                 xyouts, 0.1, -0.125, l3, /norm,charsize=0.5
                 xyouts, 0.1, -0.175, 'parameter', /norm,charsize=0.5
                 xyouts, 0.3, -0.175, 'value', /norm,charsize=0.5
                 xyouts, 0.4, -0.175, 'error', /norm,charsize=0.5
                 xyouts,xarr(1:*),yarr(1:*)*1.1,peaks(1:*)
                 xposi=findgen(n_elements(b))*0.+0.1
                 yposi=-0.225-findgen(n_elements(b))*.025
             	;transforming the a and sigmaa arrays into string arrays.
                 astr=string(format='(f10.4)',a)
                 sigmaastr=string(format='(f10.4)',sigmaa)
		;This if statement deals with the formating of the data if there are a large number of peaks. In this case two 
		;columns are created to display the data.
                if n_elements(a) le 20 then begin
             
                   xyouts, xposi, yposi, f, /norm,charsize=0.5
                   xyouts, xposi+0.2, yposi, astr,/norm,charsize=0.5
                   xyouts, xposi+0.3, yposi, sigmaastr,/norm,charsize=0.5
            
               endif else begin
              
                 xyouts, 0.6, -0.175, 'parameter', /norm,charsize=0.5
                 xyouts, 0.8, -0.175, 'value', /norm,charsize=0.5
                 xyouts, 0.9, -0.175, 'error', /norm,charsize=0.5
                 xyouts, xposi(0:19), yposi(0:19), f(0:19), /norm,charsize=0.5
                 xyouts, xposi(0:19)+0.2, yposi(0:19), astr(0:19),/norm,charsize=0.5
                 xyouts, xposi(0:19)+0.3, yposi(0:19), sigmaastr(0:19),/norm,charsize=0.5
   
                 xyouts, xposi(20:*)+0.5, yposi(20:*)+0.5, f(20:*), /norm,charsize=0.5
                 xyouts, xposi(20:*)+0.7, yposi(20:*)+0.5, astr(20:*),/norm,charsize=0.5
                 xyouts, xposi(20:*)+0.8, yposi(20:*)+0.5, sigmaastr(20:*),/norm,charsize=0.5
               
              endelse
	    ;closes the current graphics file.
	    DEVICE,/close
	    ;build a default output file name by using the idl name for the current device(!d.name).Strlowcase converts the name 
	    ;to the lower case.
	    file='idl.'+strlowcase(!d.name)

	    cmd = 'lp -d'+pr(prindex)+' '+file
	    print,cmd
	   ;function which sends a command
	    spawn,cmd
	    ;resets the device.
	    set_plot ,  mydevice

	     
  	   end
         
         
       
         
  	    'write to a file':begin
  	    		    
			     ;this command puts the name of the file into dat_file
  	    		     widget_control,dat,get_value = dat_file

  	    		     dat_file=dat_file(0)
  	    		     place=where(files eq dat_file)
  	    		     ;if dat_file is empty then the first part of the if statement is performed
  	    		     ;otherwise the file is appended
  	    		     
   	    		     if dat_file ne '' then begin 
   	    		     
   	    		       
   	    		       headers="   position"+'  '+'    ESD'+'   '+'   Intensity'+'  '+'    ESD'+'   '+$
   	    		       '     FWHM'+string(10b)
 	    		       chi_new=string(chi_new)
 	    		       
 	    		       ;place is equal to -1 if it is empty
  	    		       case place(0) of
  	    		     
  	    		         -1: begin
  	    		     
  	    		             c=string(9b)+string(9b)+string(9b)+'***** FIT RESULTS *****'+$
  	                             string(10b)+string(10b)+'chi squared value'+chi_new+string(10b)
  	                             ;finds a file unit which is not being used
  	    		             get_lun,u
  	    		             openw,u,dat_file
  	    		             info= [c,headers]
  	    		             ;printing to a file.
 	                             printf,u,info

  	    		             mess=widget_message('Data written to new file '+ dat_file ,title=$
  	    		            'file out')

  	    		             end
  	    		         
  	    		       else: begin
  	    		             ;finds a file unit which is not being used
  	    		       	     get_lun,u
  	    		             openu,u,dat_file, /append
  	                             mess=widget_message('Data appended to old file '+ dat_file ,title=$
  	                             'file out')
  	    		             end
  	    		      endcase
   	    		      for i=0, ((n_elements(a)-4)/2)-1  do begin
   	    		      ;formating of the a and sigmaa arrrays.
			         printf,u, format='(2X,2(F10.6,2X),2(F10.4,2X),F10.6)', a(2*i+5),sigmaa(2*i+5),a(2*i+4),sigmaa(2*i+4),a(2)
			      endfor
			      ;frees the file unit that was used.
  	                      free_lun,u
  	                      ;adds files to the beginning of the dat_file
  	                      files=[files, dat_file]
  	                       
  	                     endif
  	                     
  	                   end
  	                     
  	          else  : begin
  	               ;sensitises the input widget only if a filename is entered
  	               if event.value ne '' then widget_control, write, sensitive=1 else $
  	                 widget_control, write, sensitive=0
  	                 
   	                    end      
		
       endcase
   end

endcase

end



 pro venus,x1,y1,group = group

; This is the main program which creates a window, a  menu of buttons and the
;plot. The events generated by this program are dealt with by an event manager  
;called xmanager 


;compound widget used  to obtain the menu of buttons
;draw widget created in which the plot is drawn, the buttons_events
;keyword enables events to be created as result of clicking in the draw window 	
;for explanation of the common block see above	      
	 
    common block1,x,y,w,index,px,py,status,xarr,yarr,menu5,range,a,$
    value  ,menu4,row1,row2  $
    ,menu3,yfit,eta_input,gamma_input,sigmaa,b,l1,win_num,dat,$
    write, files,text,base,chi_new,f, convstr
    
    
    common block, pr,prin, prindex
	   ;initial assignments
           x=x1 
           
           y=y1
           yfit=dblarr(1)
	   px=fltarr(2)
           py=fltarr(2)
           w=1./(y+1.)
           status='z'
	   device, cursor_standard=76
           index=0
           xarr=dblarr(1)
           yarr=dblarr(1)
           a=dblarr(4)           
           a=[min(y),0.0,0.2,0.0]
           sigmaa=a*0.         
           range = where(x ge min(x) and x le max(x))
           files=['']
           ;this widget heirarchy is constructed so widgets are aligned in rows and columns
           ;offsets are avoided 
           
	   bad=widget_base(title='VENUS',tlb_frame_attr=1)
	   ;tlb_frame_attr=1 stated so that the bad base widget cannot be resized 
	   ro =widget_base(bad,/row)
	  
	  
	  row3 = widget_base(ro,/column)
	 ro3= widget_base(ro,/column)
	   menu2=cw_bgroup(row3,['done','reset']$
	   ,/column,/return_name,/frame)
	   ;eturn name returns the name of the button as the event value
	   
	   menu3 = cw_bgroup(row3,['replot','unzoom 20%', $
	  'next range'],/return_name,/frame,/column)
	   
	   menu4=cw_bgroup(row3,['peak assignment','zoom'$
	   ],/column,/return_name,/exclusive,/frame)
	    
	   
	   
	   menu5=cw_bgroup(row3,['fitit','split screen','single screen'],/column,$
	   /return_name,/frame)
	   
	   
	   
	   draw=widget_draw(ro3, button_events=1,$
	   uvalue='graph',retain=2,xsize=950, ysize=500)
	   ;retain, to make sure window is redrawn when covered
	  lab = widget_label(row3,value='initial values:',yoffset = 400)
	  
	   ;these widgets deal with input of values from the user
	   gamma_input=cw_field(row3,value=0.2, $
	     title='FWHM :',/frame,/floating,xsize=10)
	  
	   
	   eta_input = cw_field(row3,title='  eta :',value=0.0, $
	   /frame,/floating,xsize=10)
	   base = widget_base(ro3,title = 'data output',/column,/align_left)
	   text = widget_text(base,/wrap,xsize=45,ysize=10,$
  	  	         value=(''), /scroll)
  	   ;command to obtain all the printers available
           spawn, 'lpstat -a', st, count=c 	 	   
           pr=strarr(c)	  
           for i=0, c-1 do begin
	     ;string separator	
             st1=str_sep(st(i), ' ')
             pr(i)=st1(0)
          
           endfor
                        	  	         
  	  base1= widget_base(base,/row)
  	  
          write = cw_bgroup(base1,['write to a file' $
	  ],/return_name,/frame,/row)
	  dat=cw_field(base1,title = 'enter filename:',/frame $
	  ,/return_events, value=files(n_elements(files)-1))         
  	  	         
  	  writ2=cw_bgroup(base1,['print'],/return_name,/frame)							
  	  prin = widget_droplist(base1,value=pr)	         
  	  prindex=pr(0)	         
  	  	         
  	  ;desensitises the widgets below and sets the zoom button to be set initially	         
  	  	         	   
	   
	    widget_control,menu5,sensitive =0
	   
	    widget_control,gamma_input,sensitive =0
	    widget_control,eta_input,sensitive =0
	    widget_control,base,sensitive=0
	    
	   
	   widget_control,bad,/realize
	   ;sets the zoom button to be initially pressed.
	   widget_control,menu4,/set_value
	   value =1
	 !p.multi = 0
	
	   plot, x,y,/ynozero,xtitle='2*theta (degrees)',ytitle='Intensity (counts)'
 
	   
	 ;obtain the window number.  
	 widget_control,draw,get_value = win_num

	  
	  ;event manager for the bad base widget. 
          xmanager,'windb',bad,group_leader = group
        
 
end




  
