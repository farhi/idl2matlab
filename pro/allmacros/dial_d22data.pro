;*********************
PRO dial_d22data_macro, D
;*********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_d22data

;***RESETING DIAL (OR FIRST CALL).
	IF D.init eq 0 then begin D.init=1

;**	   CHECK FOR INSTRUMENT CONFIGURATION.
	   IF D.numor_sim le 0 then begin
	      para  =DialNewValue(TYPE='t_para')
	      nother=DialNewValue(TYPE='t_nother')
	      D.dist_wave(0)=nother.actang(0)*100.
	      D.dist_wave(1)=nother.act_wave
	      D.tof     =para.tof
	      D.channel =para.tof_cha_resol
	      V         =DialNewValue(TYPE='data')
	      
	   ENDIF else begin rdset,inst='D22',base='d22' & V=rdrun(D.numor_sim)
	   ENDELSE  ; Simulation
	   
	   IF D.numor_cal gt 0 then begin
	      rdset,inst='D22',base='d22' & W= rdrun(D.numor_cal) + 1
	      mean= TOTAL(W)/n_elements(W)
	      W   = mean /W  & DialModValue,W, TAG='WATER'
	   ENDIF
	   
;*	   NORMAL ACQUISITION.
	   IF D.tof eq 0 then begin
	    DialModValue,V
	    DialInit ,'diagram'
	    DialStop ,'diagram' & DialTag,'diagram',tag='PATER',set=D.number
	    DialInit ,'showx'
	    DialStart,'showx'   & DialTag,'showx'  ,tag='PATER',set=D.number
 	    DialInit ,'showy'
	    DialStart,'showy'   & DialTag,'showy'  ,tag='PATER',set=D.number

;*	    FIND GRAVITY CENTER.
		Xsum    =TOTAL(V,2)     &  idx  =FINDGEN (n_elements(Xsum))+1
		Ysum    =TOTAL(V,1)     &  idy  =FINDGEN (n_elements(Ysum))+1
		Xsum    =Xsum-MIN(Xsum) &  Ysum =Ysum-MIN(Ysum)
		Xsum(WHERE(Xsum LE MAX(Xsum)/2))=0.
		Ysum(WHERE(Ysum LE MAX(Ysum)/2))=0.
		Xgravity=TOTAL(Xsum*idx)/TOTAL(Xsum)
		Ygravity=TOTAL(Ysum*idy)/TOTAL(Ysum)
		D.Xcenter =(Xgravity)-1
		D.Ycenter =(Ygravity)-1
	   ENDIF ELSE $

;**	   TIME OF FLIGHT ACQUISITION.
	   IF D.tof eq 4 then begin
	              DialModValue,TOTAL(TOTAL(V,1),2)

	   ENDIF ELSE DialModValue,'TOF='+string(D.tof)

	ENDIF ;(D.init)

;***GET DATA FROM MAD.
	IF D.numor_sim eq 0 then V =DialNewValue(TYPE='data') $
	                    else V =rdrun(D.numor_sim)
			       ; V =V+1
	IF D.numor_cal gt 0 then $
	   if n_elements(V) eq n_elements(D.WATER) then V=V * D.WATER

;***UPDATE PROJECTION DIALS.
	IF D.tof eq 0 then begin
	   px=TOTAL(V,2) & if D.xlog eq 1 then px=alog(px>.1)
	   py=TOTAL(V,1) & if D.ylog eq 1 then py=alog(py>.1)
	   DialTag,'showx',tag='VALUE',set=px
	   DialTag,'showy',tag='VALUE',set=py
	ENDIF

;***UPDATE RADIAL INTEGRATION.
	IF D.tof eq 0 then begin onoff=0
	  DialTag,'diagram',tag='ONOFF', get=onoff
	  if (onoff) then begin
;**	   MASK THE CENTER ON IMAGE.
	   S  =SIZE (V)
	   a0 =ROUND(D.Xcenter)  &  b0=ROUND(D.Ycenter)
	   a1 =(a0-D.Xmask/2)<(S(1)-1)>0 & a2=(a0+D.Xmask/2)<(S(1)-1)>0
	   b1 =(b0-D.Ymask/2)<(S(2)-1)>0 & b2=(b0+D.Ymask/2)<(S(2)-1)>0
	   ray=max([a0+1,b0+1,(S(1)-a0),(S(2)-b0)])

	   if (a1 ne a2) and (b1 ne b2) then V(a1:a2,b1:b2)=-1

;**	   DO THE WORK.
	   D.diagupd=D.diagupd+D.frequency ;(cpu moderator)
	   IF D.diagupd gt 0 then begin
	      D.vmask=D.vmask*0
	      D.vmask(*,0)=1 & D.vmask(*,s(2)-1)=1 & D.vmask(0,*)=1
	      if (a1 ne a2) and (b1 ne b2) then D.vmask(a1:a2,b1:b2)=1
	      RADIAL_AT, V, CENTER=[D.Xcenter,D.Ycenter], INCR=1., $
	                 DIAG=diam, X=xdiam, MASK=D.vmask, ERROR=error
	     ;DECOR, D.Xcenter+1,D.Ycenter+1, 0.,360., D.dist_wave(0)>1
	     ;DEPLI, V,max([D.Xmask,D.Ymask]),ray,arel,diam,xdiam
	      ytit='I'
	      if D.dlog eq 1 then begin diam=alog(diam>.1) & ytit='logn(I)'
	      	 error=(alog((diam+error)>.001)-alog((diam+error)>.001))/2.
	      endif
	      theta =ATAN(xdiam*0.75  ,D.dist_wave(0))/2.
	      Q     =4.*!pi*SIN(theta)/D.dist_wave(1)
	      DialTag,'diagram',tag= 'VALUE'   ,set=diam
	      DialTag,'diagram',tag='XVALUE'   ,set=Q
	      DialTag,'diagram',tag= 'ERROR'   ,set=error
	      DialTag,'diagram',tag= 'Y_TIT'   ,set=ytit
	      DialTag,'diagram',tag= 'X_TIT'   ,set='Q'
	   endif

;**	   MARK CENTER CROSS-HAIR.
	   IF D.plot  eq 0 then begin
		avg=-1
		a1 =0 &  a2=S(1)-1   & b1=0 & b2=S(2)-1
		a0 =a0*((a0 ge 0) and (a0 lt S(1)))
		b0 =b0*((b0 ge 0) and (b0 lt S(2)))
 		IF a0 gt 0 then V(a0,b1:b2)=avg
		IF b0 gt 0 then V(a1:a2,b0)=avg
	   ENDIF
	  ENDIF
	ENDIF
;***UPDATE VALUE OF CURRENT DIAL.
	IF (D.tof eq 0) or (D.TOF eq 4) then begin
	   IF D.tof eq 4 then V =TOTAL(TOTAL(V,1),2)
	   IF D.log eq 1 then D.value= ALOG(V>1) ELSE $
	   IF D.log eq 2 then D.value= SQRT(V>0) ELSE $
	                      D.value=      V
	ENDIF
end



;*********************
FUNCTION dial_d22data
;*********************
;**
;** The dial initialisation

   ;Dial Variables (Defaulted if not present in return statement)
   ;--------------
    GENERIC='mad'    ;connect to the mad-idl interface
    TYPE='data'      ;when DialNewValue() is used, get the data
    ONOFF=0          ;state of the Dial 1=running
    VALUE=fltarr(128,128) ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var.
    PLOT=0           ;-2=no plot 0=image 1=surface 2=contour n>2 means show vector of last n values of the scalar
    UPPERLIM=0.      ;upper limit of a 1dim. plot (LOWERLIM for lower limit)
    HISTORY=0        ;=1 to record values in file .his
    DURATION=0       ;if >0 then Dial is stopped after running duration seconds
    WUPDATE=1        ;update corresponding workspace
    FREQUENCY=7.     ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used

   ;User Variables (Must be present in return statement)
   ;--------------
    LOG =0	 ;  Detectors  not in log.
    DLOG=0	 ;  Diagram    not in log.
    XLOG=0	 ;X projection not in log.
    YLOG=0	 ;Y projection not in log.
    NUMOR_sim=0L ;Used for simulation
    NUMOR_cal=0L ;Used for calibration numor
    WATER=0	 ;Used for calibration data
    XMASK=12	 ;X size of mask
    YMASK=15	 ;Y size of mask
    VMASK=VALUE	 ;matrix containing the mask (1's)
    DIAGUPD=0	 ;1:Diagram may be updated
    DIST_WAVE=[400.,5.] ;Detector distance & wavelength

return, {generic:GENERIC,value:VALUE,FREQUENCY:FREQUENCY,vmask:VMASK,$
         PLOT:PLOT,init:0,log:LOG,xlog:XLOG,ylog:YLOG,dlog:DLOG,tof:0L,channel:1L,$
         xcenter:0.,ycenter:0.,xmask:XMASK,ymask:YMASK,diagupd:DIAGUPD,$
	 dist_wave:DIST_WAVE,numor_sim:NUMOR_sim,numor_cal:NUMOR_cal,water:WATER}
end






;***************************************************************************************

;** Usefull calls to be used in procedure dial_d22data_macro :
;** *************
;** V=DialNewValue([/SETVALUE, COMMENT=txt])   ;Get a new value from DIAL_'generic'_READ
;**                                            (a request is made to the instrument)
;**                                            (/SETVALUE means D.value is set to V)
;** C=DialControl ('command syntax',[CHECK=.5]);Send a command to the instrument control
;**                                            (CHECK means check every .5 sec till the
;**                                             command  is complete)
;** DialTag   ,    'temp2',TAG='VALUE',GET=V   ;Return V,the value for the tag  'VALUE'
;**                                                                of  the dial 'temp2'
;** DialTag   ,    'temp3',TAG='ONOFF',SET=1   ;Set to 1 the value of  the tag  'ONOFF'
;** DialStart ,    'temp3'                     ;A short  for previous call
;** DialStop  ,    'temp3'                     ;A short  too
;**
;** DialModValue,   V                          ;Set the new value for current dial or
;** D.value   =     V                          ;modify yourself the tag Value if type &
;**                                            ;dimensions don't change.(same for Error)
;** D.upperlim=   150.                         ;Set upper limit for plotting.
;**
;** R=DialOn ()                                ;Return 0 if Dial has been interrupted
;**                                            (To use inside loops)
;** DialInit,      'template4',[NEW='tmp4']    ;Initiate dial  'template4' from file:
;**                                                               dial_template4.pro
;**                                            (You may change its name to 'tmp4' and
;**                                            (use DialStart,'tmp4' to activate it)
;** DialMacro,     'template4'                 ;Force execution of DIAL_TEMPLATE4_MACRO
;**                                            ('template4'  is keept inactive, ONOFF=0)
;** DialClear,     'template4'                 ;Suppress dial  'template4' from memory
;** DialWSet                                   ;Reserve central draw window for next plot
;**
;** DialsFrequency,[GET=freq],[SET=.5],[/STOP] ;Set  or Get the general frequency value
;**                [DURATION=90.] ,   [/START] ;              (time is in seconds)
;**                                            ;Stop or Start the general process
;**                                            ;Set  Time  limit for the active process
