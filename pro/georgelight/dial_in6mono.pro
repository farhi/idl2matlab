;*****************************************************************************
PRO dial_in6mono_macro, D
;*****************************************************************************
;** METHOD TO POSITION THE MONOCHROMATORS on IN6.
;
; Usage: change lambda before calling the dial
;        replace Tst into Dial for real 

if strpos(D.Mode,'simulation') ge 0 then V=TstNewValue (type='status', /setvalue) $
else V=DialNewValue (type='status', /setvalue)

IF V eq 'Idle' THEN begin

  CASE  D.init OF

 ;RAZ MONOKs
 ;**********
  0:  begin
      ; look for measured lambda value closest to tabulated values
      if D.EnergyToSet eq 0 then begin
        if strpos(D.Mode,'simulation') ge 0 then wl =TstNewValue (type='wave') $
        else wl =DialNewValue (type='wave')
      endif else wl = D.EnergyToSet
      
      wl =round(wl*100)/100.
      idx=-1 &  tmp1=min(abs(D.EnergyDefinitions-wl), idx)
      D.TableIndex = idx
      wl = D.EnergyDefinitions(idx)
      if D.TableIndex lt 0 then begin
        D.value='Unknown Lambda:'+strtrim(string(wl),2)
        D.Mode = D.Mode+' error:lambda='+strtrim(string(wl),2)+' ?'
        D.Init = 6
      endif else begin
        D.value='Lambda='+strtrim(string(wl),2)+' RAZ'
        if strpos(D.Mode,'simulation') ge 0 then begin
          R=TstControl(D.MonokRAZCommand)
          D.Frequency = 0.1
        endif else begin
          R=DialControl(D.MonokRAZCommand)
          D.Frequency = 1
        endelse

        DialModValue,    D.MonokData(0,*,idx)  ,tag='CENTER'
        DialModValue,    D.MonokData(1,*,idx)  ,tag='RANGE'
        DialModValue,fix(D.MonokData(2,*,idx)) ,tag='TIME'
        DialModValue,    D.MonokData(3,*,idx)  ,tag='STEP'
        DialModValue,    D.MonokData(4,*,idx)  ,tag='RESULT'
        
        DialModValue,   -D.range(D.MonokSequence(0))/2 ,tag='LOOP_rang'
        D.LOOP_mono = 0
        D.Plot      = 0
        
        if strpos(D.Mode,'rocking') ge 0 then  begin
          IN6monoPlot, D,'first',idx
          D.init     =1        ; rocking curve
        endif else D.init = 4  ; position
      endelse
      end
 ;MOVE MONOKs
 ;***********
  1:  begin
      if D.LOOP_mono lt n_elements(D.MonokSequence) then begin
        im=D.MonokSequence(D.LOOP_mono)
        if D.LOOP_rang le D.range(im)/2-1 then begin
          ;MOVE CURRENT MONOK ONE STEP
          ;***************************
          Value = D.center(im)+D.LOOP_rang
          D.value=string(im+1, Value, format='(%"'+D.MonokSetCommand+'")')
          if strpos(D.Mode,'simulation') ge 0 then R=TstControl(D.value) $
          else R=DialControl(D.value)
          D.init=2
          D.MonokData(4,im,D.TableIndex) = Value
        endif else begin
          ;END OF RANGE-> FIT-> MOVE MONOK ASIDE-> NEXT MONOK
          ;**************************************************
          if strpos(D.Mode,'rocking') ge 0 then IN6monoPlot,D, 'fit'
          ; send mono to end (they are indeed be at end)
          D.value='mono' +strtrim(string(im+1),2)+' at end'
          ;R=TstControl(D.value)
          D.MonokData(4,im,D.TableIndex) = D.result(D.MonokSequence(D.LOOP_mono))
          D.LOOP_mono=D.LOOP_mono+1
          if D.LOOP_mono lt n_elements(D.MonokSequence) then $
             D.LOOP_rang=-D.range(D.MonokSequence(D.LOOP_mono))/2
        endelse
      endif else begin
         D.LOOP_mono=0 & D.init=4
      endelse
      end

 ;COUNT (no save)
 ;*****
  2:  begin
      D.value=string(D.time(D.MonokSequence(D.LOOP_mono)), format='(%"'+D.MonokCntCommand+'")')
      if strpos(D.Mode,'simulation') ge 0 then R=TstControl(D.value) $
      else R=DialControl(D.value)
      D.init=3
      end
 ;GET COUNT VALUE-> PLOT-> NEXT STEP
 ;**********************************
  3:  begin
      if strpos(D.Mode,'simulation') ge 0 then V=TstNewValue (type='monitor') $
      else V=DialNewValue (type='monitor')
      ;integrate monitor
      if strpos(D.Mode,'rocking') ge 0 then IN6monoPlot,D, 'plot', V
      D.LOOP_rang=D.LOOP_rang + D.step(D.MonokSequence(D.LOOP_mono))
      D.init=1
      end
 ;END OF LOOPs->MOVE MONOKS TO CALCULATED CENTERS
 ;**********************************************
  4:  begin
        ; send to home (raz) before 
        D.value='Mono RAZ'
        D.MonokData(4,D.MonokSequence(D.LOOP_mono),D.TableIndex) = 0
        if strpos(D.Mode,'simulation') ge 0 then R=TstControl(D.MonokRAZCommand) $
        else R=DialControl(D.MonokRAZCommand)
        D.init = 5
      end
  5:  begin
      if D.LOOP_mono lt n_elements(D.MonokSequence) then begin
        im=D.MonokSequence(D.LOOP_mono)
        ; send each mono to fitted position
        value = D.result(im)
        ; test if fitted value is reasonable within 5%
        NormRatio = abs((value/D.MonokData(0,im,D.TableIndex)) -1)
        if NormRatio ge 0.05 then begin
          LightVars=Light_Get_LightVars()
          Light_Event_Base_Alert, Lightvars, error='Mono'+strcompress(string(im+1), /remove_all)+' not set'
          Light_LogMessage, LightVars, 'silent','[E] Mono'+strcompress(string(im+1), /remove_all)+' fitted position '+strcompress(string(value), /remove_all)+' more than 5% out of '+strcompress(string(D.MonokData(0,im,D.TableIndex)), /remove_all)
          value = D.MonokData(0,im,D.TableIndex)
          D.Mode = D.Mode+' error:'+'Mono'+strcompress(string(im+1), /remove_all)
        endif else begin
          D.MonokData(4,im,D.TableIndex) = value
          D.value=string(im+1, value, format='(%"'+D.MonokSetCommand+'")')
          if strpos(D.Mode,'simulation') ge 0 then R=TstControl(D.value) $
          else R=DialControl(D.value)
        endelse
        D.LOOP_mono=D.LOOP_mono+1
      endif else begin
        D.value='Done'
        D.Mode = D.Mode+' completed'
        if strpos(D.Mode,'auto') ge 0 and strpos(D.Mode,'rocking') ge 0 then begin
          display, Display_Name='Rocking_Curve',Name_Data='Mono', /delete
          display, Display_Name='Rocking_Curve',Name_Data='Mono_Fit', /delete
        endif
        D.Init = 6
      endelse
      end
;STOP DIAL
;**********************************************      
  6:  begin
        D.Mode = D.Mode+' end'
        DialStop
      end
  else:
  endcase

endif else begin ; V ne 'Idle'
  ; test if Status means that something else is occuring at the same time:
  ; then stop in6mono to avoid conflicts
  if V eq 'WAVELENGTH SETTING'  or V eq 'CHOPPER SETTING' $
  or V eq 'COLLIMATION SETTING' or V eq 'ATTENUATOR SETTING' $
  or V eq 'BEAM STOP SETTING'   or V eq 'UNDEFINED' then begin
    Light_Event_Base_Alert, Lightvars, error='Monok Conflict: '+V
    Light_LogMessage, LightVars, 'silent','[E] The instrument is doing '+V+' which prevents Monochromators setting. Aborting.'
    D.Mode = D.Mode+' error:'+V
    DialStop
    D.Init = 6
  endif
endelse
;  wait in case of following operations:
; 'COUNTING' 'POSITIONNING' 'ENCODER READING' 'TEMPERATURE SETTING' 'WAITING'
; 'DATA UPDATING'
end



;*****************************************************************************
PRO IN6monoPlot, D, flag, V
;*****************************************************************************
;**PLOT RESULT
common IN6mono_c, xp, yp, xs, ys, comment, xd, yd

; DialWset
im=D.MonokSequence(D.LOOP_mono)

CASE flag of

'first': begin
    xp  = D.LOOP_rang+D.center(im)
    xpc = xp
    yp  = 0.
    rx  =[min(D.MonokData(0,*,V)-D.MonokData(1,*,V)),$
          max(D.MonokData(0,*,V)+D.MonokData(1,*,V))]
    wl  = strtrim(string(D.EnergyDefinitions(V)),2)
    mt  ='IN6 monochromators positions settings (WAVEL:'+wl+')'
    xt  ='Position' & yt='Counts'
   end
'plot' : begin
    nx=[D.LOOP_rang+D.center(im)] & xp=[xp,nx] & yp=[yp,V] & ep=[sqrt(V>0)]
    xt  ='Mono_Pos' & yt='Counts'
    display, IX=xp, Data=yp, Name_Data='Mono', Display_Name='Rocking_Curve', /scatter, $
      Name_IX=xt, Name_IY=yt, /detached, /update
   end
'fit'  : begin
    nbp=D.range(im)/D.step(im)+1
    nel=n_elements(xp)
    xe1=xp(nel-nbp:nel-1) & ye1=yp(nel-nbp:nel-1)
    ;ESTIMATE [height,center,width,constant]
    mii=min(ye1)
    maa=max(ye1) & paa=total(xe1*ye1)/total(ye1)
    waa=sqrt(total(xe1*xe1*ye1)/total(ye1) - paa*paa)
    S1 =[maa-mii,paa,waa,mii]
    ;FIT
    fit=gaussfit(xe1,ye1,A1,nterms=4,estimates=S1)
    D.result(im)=A1(1)

    xs1 = xe1
    ys1 = A1(0)*exp(-(((xs1-A1(1))/A1(2))^2)/2)+A1(3)  ;+A1(4)*xs1+A1(5)*xs1^2

    mess='M'+strtrim(string(im+1),2)+'='+strtrim(string(round(A1(1))),2)
    mess=mess+' ('+strtrim(string(round(A1(2))),2)+')'

    if D.LOOP_mono eq 0 then begin
      xs = xs1 & ys = ys1 & comment = mess
      xd = xe1 & yd = ye1
    endif else begin
      xs = [xs, xs1] & ys = [ys, ys1] & comment= [ comment, mess ]
      xd = [xd, xe1] & yd = [yd, ye1]
    endelse
    display,IX=xd, Data=yd, Display_Name='Rocking_Curve', Name_Data='Mono', Comment=comment
    display,IX=xs, Data=ys, Display_Name='Rocking_Curve', Name_Data='Mono_Fit', /update, /overlay

    if im eq n_elements(D.MonokSequence)-1 then begin
      D.Value='Done Fit'
    endif
   end
else:
endcase
end



;*****************************************************************************
FUNCTION dial_in6mono
;*****************************************************************************
;**CONSTRUCTOR.
    ; Default settings to be used
    
    ;           center    range    time     step    opt_value
    Data =  [[[4920.00,  200.00,  1.00000, 10.0000, 4920.00 ]   ,$  ;wave=4.14 monok 1
                [5060.00,  200.00,  1.00000, 10.0000, 5060.00 ]   ,$  ;          monok 2
                [5355.00,  200.00,  1.00000, 10.0000, 5355.00 ]]  ,$  ;          monok 3

               [[4400.00,  300.00,  1.00000, 30.0000, 4400.00 ]   ,$  ;wave=4.60
                [4556.00,  300.00,  1.00000, 30.0000, 4556.00 ]   ,$
                [4825.00,  300.00,  1.00000, 30.0000, 4825.00 ]]  ,$

               [[3800.00,  190.00,  1.00000, 10.0000, 3800.00 ]   ,$  ;wave=5.12
                [3960.00,  190.00,  1.00000, 10.0000, 3960.00 ]   ,$
                [4220.00,  190.00,  1.00000, 10.0000, 4220.00 ]]  ,$

               [[2640.00,  200.00,  10.0000, 20.0000, 2640.00 ]   ,$  ;wave=5.92
                [2790.00,  200.00,  10.0000, 20.0000, 2790.00 ]   ,$
                [3050.00,  200.00,  10.0000, 20.0000, 3050.00 ]]]

return,{  Generic           :'mad', $
          Value             :'Ready', $
          Frequency         : 1.0, $
          Mode              :'rocking simulation', $
          TableIndex        : -1, $
          LOOP_mono         : 0, $
          Plot              : 0, $
          EnergyDefinitions :[4.14, 4.60, 5.12, 5.92], $
          EnergyToSet       : 4.14, $
          MonokSequence     :[2,1,0], $
          MonokData         :Data, $
          MonokRAZCommand   :'raz', $
          MonokSetCommand   :'mono%d %d', $
          MonokCntCommand   :'count %d n' }
END
