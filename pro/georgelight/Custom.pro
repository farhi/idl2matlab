;***************************************************************
;** Project: dial_light
;** File:    Custom.pro
;** Version: 2.0.3
;** Date:    July, 30th, 2002
;** Author:  E. Farhi
;** Object:  Contains specific instrument routines (IN6)
;**          Most Procedures are Instrument dependent (either TOF or IN6)
;
;** Require: dial_light.pro, Tools_StructSetValue.pro, Dialog_Fields.pro, TagFile.pro, dial_display.pro
;
;Created  June 4th 2002: Contains specific instrument routines (IN6)
;Modified Jul 8th 2002:  added monok+choppers. Improved computation of TOF par
;Modified Jul 11th 2002: added check elastic peak+auto
;Modified Jul 24th 2002: added Run/Sequence execution+stop
;Modified Jul 30th 2002: reorganised procedure names
;
;***************************************************************

; These procedures handle stand-alone processes that are not directly related to user actions/events
;
; pro Light_Custom_IN6_Update, LightVars, update_buttons=update_buttons, stop=stop, start=start, $
; pro Light_Custom_IN6_Init, LightVars, update_buttons=update_buttons, stop=stop, start=start
; pro Light_Custom_IN6_Sensitive, LightVars, mode=mode
; pro Light_Custom_IN6_Collect, LightVars
; pro Light_Custom_IN6_Setup_Check, LightVars, Changed=Changed, FromMad=FromMad, PhaseSpeed=PhaseSpeed, wavelength=wavelength
; pro Light_Custom_IN6_Calc_TOF_Choppers, LightVars, Wavelength=Wavelength, show=show
;
; These procedures handle processes that are triggered by user events (managed through Light_Base_Event_Parser)
;
; pro Light_Event_Custom_IN6_Edit_Header, LightVars
; pro Light_Event_Custom_IN6_Stop, LightVars, motors=motors, choppers=choppers
; pro Light_Event_Custom_IN6_Default_Displays, LightVars
; pro Light_Event_Custom_IN6_Setup_Build, LightVars, auto=auto, help=help, wavelength=wavelength, update=update
; pro Light_Event_Custom_IN6_Setup_Choppers, LightVars, Wavelength=Wavelength, update=update, auto=auto, help=help
; pro Light_Event_Custom_IN6_Setup_Monochromators, LightVars, Wavelength=Wavelength, rocking=rocking, position=position, update=update, help=help
;
; These procedures handle processes that are either automatic or launched by user, but may then live for a given time
;
; pro Light_SubDial_Custom_IN6_Check_Choppers, LightVars
; pro Light_SubDial_Custom_IN6_Check_Monochromators, LightVars, Wavelength=Wavelength
; pro Light_SubDial_Custom_IN6_Check_Elastic_Peak, LightVars, auto=auto, help=help
; pro Light_SubDial_Custom_IN6_Check_Run, LightVars

; Still to do:
; pro Light_SubDial_Base_Check_Temperature, LightVars
;   will use Physica B A.Stunault et al., 180 (1992) 926 implemented in /home/cs/richard/lamp/lamp_mac/corel.pro
;   only if more than 1 repetition
;   call with wkin is [ [data0], [data1], [data2]... ] with dim(2) = nrepet. Returns R(i,c) (with dims of data0)
;   result should be around 1

;***************************************************************
; this routine is used only for off-line tests, and modifies directly the Lightvars.Mad structure members
; example: 
;   madtest,'status','"idle"'
;   madtest,'t_res.tempea(1)',5 
pro MadTest, type, value

  LightVars = Light_Get_LightVars()
  cmd = 'LightVars.Mad.'+type+' = '+string(value)
  print, 'MadTest:', cmd
  ok  = execute(cmd)
  DialTag, 'light', tag='Mad',       set=LightVars.Mad

end ; PRO MadTest

;***************************************************************
;** Main Custom procedure (executed through Instrument.InitPro), called from Light_Base
pro Light_Custom_IN6_Update, LightVars, update_buttons=update_buttons, stop=stop, start=start, $
                                        collect=collect, edit_header=edit_header

  if n_elements(start) ne 0 then Light_Custom_IN6_Init, LightVars
    
  if n_elements(collect) ne 0 then Light_Custom_IN6_Collect, LightVars
  
  if n_elements(edit_header) ne 0 then Light_Event_Custom_IN6_Edit_Header, LightVars
  
  if n_elements(stop) ne 0 then begin
    Light_Event_Custom_IN6_Stop, LightVars
    update_buttons=1 ; to update menu item/buttons according to SU/normal mode
  endif
  
  if n_elements(update_buttons) ne 0 then Light_Custom_IN6_Sensitive, LightVars

end ; PRO Light_Custom_IN6_Update

;***************************************************************
;** Procedure executed when starting light, specific to the instrument
; (see also light/custom.pro, line 221)
pro Light_Custom_IN6_Init, LightVars

    ; Define specific Custom blocks

    ; Here follows a default IN6 configuration
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

    CustomData = {LIGHT_CUSTOMDATA_IN6, $ ; instrument configuration to be loaded/saved from/into .ini files
      Type:'Time-of-Flight' , Name:'IN6', $
      CustomMenuItems   :['Instrument...', $
                              'Instrument (Choppers)...', $
                              'Instrument (Monochromators)...', $
                              'Restore IN6 Default Displays', $
                              'Experiment header...' ], $
      CustomMenuPro     :['Light_Event_Custom_IN6_Setup_Build, LightVars', $
                              'Light_Event_Custom_IN6_Setup_Build, LightVars & Light_Event_Custom_IN6_Setup_Choppers, LightVars', $
                              'Light_Event_Custom_IN6_Setup_Build, LightVars & Light_Event_Custom_IN6_Setup_Monochromators, LightVars', $
                              'Light_Event_Custom_IN6_Default_Displays, LightVars', $
                              'Light_Event_Custom_IN6_Edit_Header, LightVars'], $
      EnergyDefinitions : [4.14,  4.6,  5.12,  5.92], $    ; energy values physically achievable on the instrument
      EnergyOptions     :'discrete wavelength superuser', $   ; may be limits, free, discrete
      ElastPeakChannels : [235L, 275L, 315L, 350L], $  ; default elastic line channel for 512 channels
      MonokSequence     : [2,1,0], $
      MonokDSpacing     : 3.355, $          ; d[AA] lattice-spacing for monochromators. PG002=3.355
      MonokData         : Data, $           ; reform(Data, 5,n_elements(MonokSequence),n_elements(EnergyValues))
      MonokFluxMon_TOF  : 0L, $             ; 0 if monochromator rocking curve calibrates on Monitor, 1 if on TOF (detectors)
      MonokUseOldValues : 1L, $             ; 0 if monochromator rocking angles are computed, 1 if use table (Y. Blanc)
      EnergyCommand     :'par wavelength %f', $ ; <lambda>
      MonokRAZCommand   :'raz', $
      MonokSetCommand   :'mono%d %d', $       ; <number> <position>
      MonokCntCommand   :'count %f t n', $    ; <time_s>
      CountCommand      :'count %f %s %d %s',$; <time> <time|mon> <nrep> <save|nosave>
      TempReadCommand   :'rte', $
      TempWriteCommand  :'pte %f', $
      ChoppersStopCommand:'kill', $
      StopCommand       : 'stop', $
      CommentCommand    : '! %s', $
      SetUserCommand    :'par user %s', $       ; <UserName+' '+LocalContact>
      SetTitleCommand   :'par title %s', $      ; <title>
      SetSubTitleCommand:'par subtitle %s', $   ; <subtitle>
      SetEnvirCommand   :'par environement %s', $ ; <env. code>
      SetComputeCommand :'par compute %s', $    ; t_para.compute_par
      ChoppersSetCommand:'par chop s %7.2f r %d P1 %6.2f P2 %6.2f', $  ; <fermi_speed> <ratio> <phase_ferm> <phase_supp> (<offset>)
      ChoppersSynchro   :'chopper synchro wait', $
      TOFSetCommand     :'par tof channel %f width %f delay %f', $ ; <n_chan> <chan_width> <delay>
      ElPeakCommand     :'par elpeak %d', $     ; <elpeak chan>
      PeriodCommand     :'par period %f', $     ; <period>
      AutoFlags         : [1,1,1], $        ; automatic setup flags for Choppers/Monok/Check of elastic peak
      SetupEnergies     : [4.14, 0., 0.], $ ; values of the energy for which chopper/monok/elastic peak where set
      Distances_CC_CS_SD: [0.200, 0.395, 2.483], $; [m] Distances Chopper1-Chopper2, Chopper2-Sample, Sample-Detector
      ChopperSpeedRatio : 1., $             ; [1]   speed ratio Fermi/Supp
      Focus_Energy      : 0., $             ; [meV] focusing to specified energy transfert
      N_channels        : 512L, $           ; [512] Number of time channels
      ElectronicTimeBase: 0.125, $          ; [us]  Electronic base time
      ElectronicDelay   : 44.875, $         ; [us]  Default Electronic Delay
      ReferencePhase    : 90., $            ; [deg] Reference Phase
      PhaseOffset       : 0. $              ; [deg] Phase Offset (added to Fermi phase)
    } ; enrich Light.Instrument

    CustomPrivate = {LIGHT_PRIVATE_IN6, $
      MonokCurrentIndex : 0L, $             ; current monok index in sequence during rocking curve/positioning
      MonokScanPoint    : 0L, $             ; current scan point during rocking curve
      MonokPosition     : 0L, $             ; current position during rocking curve/positioning
      MonokLambdaData   : 0L, $             ; current part of MonokData table for given lambda
      MonokX            : [0.], $           ; vector of positions
      MonokY            : [0.], $           ; vector of counts
      MonokYfit         : [0.], $           ; vector of fitted counts
      MonokDisplayMsg   : ['','',''], $     ; comment lines for the tree monochromators of IN6

      EnergyValues      : [0.,0.,0.,0.], $  ; energy values stored from previous setting
      EnergyUnit        :'', $
      EnergyToSet       : 0., $
      EnergyIndex       : 0L, $
      Elastic_Peak_Chan : 315L, $           ; [315] Current Elastic_Peak position        
      KeepMadData       : 0L, $
      Environement      : 'xx', $
      Delay             : 0., $
      SpeedFermiTarget  : 0., $
      PhaseFermiTarget  : 0., $
      SpeedFermi        : 0., $
      PhaseFermi        : 0., $
      SpeedSupprTarget  : 0., $
      PhaseSupprTarget  : 0., $
      SpeedSuppr        : 0., $
      PhaseSuppr        : 0., $
      Period            : 0., $
      ChannelWidth      : 0., $
      AutoProcess       : 0L, $
      DeadTime          : 0. $
      }

    CustomHandles = { LIGHT_CUSTOMHANDLES_IN6, $
      SetupButtons: [0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L], $ ; chopper/monok/DoElastpeak/auto/stop/focusEn/Ratio/ChanNb/ElPeak/ElPeakMenu
      SetupStatus : [0L, 0L, 0L], $
      SetupFlags  : [0L, 0L, 0L], $     ;  auto flag handles
      Energy      : [0L, 0L, 0L, 0L], $ ;  energy value displays handles: lambda, E, k, v
      EnergySetup : 0L, $               ; main setup window handle
      MonokSetup  : 0L, $               ; main monok window handle
      MonokButtons: [0L, 0L, 0L, 0L], $ ; Mono setup: rocking curve/position/status/table handles
      ChoppersSetup:0L, $               ; main Choppers window handle
      ChoppersButtons: [0L, 0L, 0L, 0L], $ ; Choppers setup: set/status/table handles
      ChopTofText : 0L $               ; TOF Choppers text Display
    } ; enrich Light.Handles

    CustomNames = tag_names(CustomData)
    for index=0, n_elements(CustomNames)-1 do begin
      ThisTag = CustomNames(index)
      ; store and extend Light.Instrument Data block
      Tools_StructSetValue, struct=LightVars, tag='Instrument.'+ThisTag, val= CustomData.(index)
    endfor

    CustomNames = tag_names(CustomHandles)
    for index=0, n_elements(CustomNames)-1 do begin
      ThisTag = CustomNames(index)
      ; store and extend Light.Handles Data block
      Tools_StructSetValue, struct=LightVars, tag='Handles.'+ThisTag, val= CustomHandles.(index)
    endfor

    CustomNames = tag_names(CustomPrivate)
    for index=0, n_elements(CustomNames)-1 do begin
      ThisTag = CustomNames(index)
      ; store and extend Light.Handles Data block
      Tools_StructSetValue, struct=LightVars, tag='Private.'+ThisTag, val= CustomPrivate.(index)
    endfor

    if file_test('light_in6.mad') ne 0 and LightVars.Global.Simulation eq 3 then begin
      restore, 'light_in6.mad'
      Light_LogMessage, LightVars, 'normal','[ ] Importing real session MAD data. Setting frozen mode.'
      LightVars.Private.KeepMadData = 1
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_nother', val= t_nother
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_res'   , val= t_res
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_para'  , val= t_para
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_status', val= t_status
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_counts', val= t_counts
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_chopper',val= t_chopper
      LightVars.Global.Simulation = 1L
    endif

    ; execute IN6 Instrument Control startup commands: see ../light/DIALS/dial_set_inst_def.pro
    ;mad,'par lim mono1 -6000 6000'
    ;mad,'par lim mono2 -6000 6000'
    ;mad,'par lim mono3 -6000 6000'

end ; PRO Light_Custom_IN6_Init

;***************************************************************
pro Light_Custom_IN6_Sensitive, LightVars, mode=mode
; Setup window buttons in inactive/normal/extended/SU mode (activate/unactivate)

    ; force unactivate buttons if Monok/Choppers/Check Inelastic/RunsSequence are running
    MonokOn    = ''
    ChoppersOn = ''
    CheckElPOn = ''
    RunIsOn    = ''
    if LightVars.Handles.SetupStatus(0) ne 0 then $
      widget_control, LightVars.Handles.SetupStatus(0), get_uvalue=ChoppersOn, bad_id=tmp
    if LightVars.Handles.SetupStatus(1) ne 0 then $
      widget_control, LightVars.Handles.SetupStatus(1), get_uvalue=MonokOn,    bad_id=tmp
    if LightVars.Handles.SetupStatus(2) ne 0 then $
      widget_control, LightVars.Handles.SetupStatus(2), get_uvalue=CheckElPOn, bad_id=tmp
    if LightVars.Handles.RunStatus ne 0 then $
      widget_control, LightVars.Handles.RunStatus,      get_uvalue=RunIsOn, bad_id=tmp
      
    if strlen(MonokOn) gt 0 or strlen(ChoppersOn) gt 0 or strlen(CheckElPOn) gt 0 or  strlen(RunIsOn) gt 0 then mode = 'inactive'
    
    if n_elements(mode) eq 0 then begin
      if LightVars.Private.IsSuperUser eq 0 then begin
        if strpos(Lightvars.Instrument.EnergyOptions, 'superuser') lt 0 then mode='extended' else mode = 'normal'
      endif else mode ='superuser'
      if LightVars.Private.AutoProcess gt 0 then begin
        if LightVars.Private.AutoProcess gt 5 then LightVars.Private.AutoProcess = 0 $
        else LightVars.Private.AutoProcess = LightVars.Private.AutoProcess+1
      endif
    endif

    if mode eq 'inactive' then begin
      widget_control, LightVars.Handles.SetupButtons(2), sensitive=0, bad_id=tmp1   ; check elastic peak button
      widget_control, LightVars.Handles.SetupButtons(9), sensitive=0, bad_id=tmp1   ; check elastic peak menu
      widget_control, LightVars.Handles.RunStartButton(0), sensitive=0, bad_id=tmp1 ; START runs button/menu
      widget_control, LightVars.Handles.RunStartButton(1), sensitive=0, bad_id=tmp1 ; START runs button/menu
      widget_control, LightVars.Handles.RunStartButton(2), sensitive=0, bad_id=tmp1 ; START runs button/menu
      widget_control, LightVars.Handles.Energy(0), sensitive=0, bad_id=tmp1         ; Energy value
      widget_control, LightVars.Handles.Energy(1), sensitive=0, bad_id=tmp1         ; Energy value
      widget_control, LightVars.Handles.Energy(2), sensitive=0, bad_id=tmp1         ; Energy value
      widget_control, LightVars.Handles.Energy(3), sensitive=0, bad_id=tmp1         ; Energy value
    endif else begin
      widget_control, LightVars.Handles.SetupButtons(2), sensitive=1, bad_id=tmp1   ; check elastic peak button
      widget_control, LightVars.Handles.SetupButtons(9), sensitive=1, bad_id=tmp1   ; check elastic peak menu
      widget_control, LightVars.Handles.RunStartButton(0), sensitive=1, bad_id=tmp1 ; START runs button/menu
      widget_control, LightVars.Handles.RunStartButton(1), sensitive=1, bad_id=tmp1 ; START runs button/menu
      widget_control, LightVars.Handles.RunStartButton(2), sensitive=1, bad_id=tmp1 ; START runs button/menu
      widget_control, LightVars.Handles.Energy(0), sensitive=1, bad_id=tmp1         ; Energy value
      widget_control, LightVars.Handles.Energy(1), sensitive=1, bad_id=tmp1         ; Energy value
      widget_control, LightVars.Handles.Energy(2), sensitive=1, bad_id=tmp1         ; Energy value
      widget_control, LightVars.Handles.Energy(3), sensitive=1, bad_id=tmp1         ; Energy value
    endelse

    Lightvars.Instrument.EnergyOptions = strlowcase(Lightvars.Instrument.EnergyOptions)
    ; normal mode: LightVars.Private.IsSuperUser eq 0  and superuser is in options -> all inactivate
    ; SU mode: LightVars.Private.IsSuperUser eq 1 -> all activated
    ; extened normal mode: LightVars.Private.IsSuperUser eq 0  and superuser not in options -> some activated
    if mode eq 'normal' or mode eq 'extended' or mode eq 'inactive' then begin ; common normal mode
      ; Monok position, rocking and table edition
      widget_control, LightVars.Handles.MonokButtons(0), sensitive=0, bad_id=tmp1 ; rocking
      widget_control, LightVars.Handles.MonokButtons(3), editable=0,  bad_id=tmp1 ; table
      ; menu items Setup:Load/save config
      widget_control, LightVars.Handles.LoadSaveCfg(0), sensitive=0, bad_id=tmp1  ; load config setup menu item
      widget_control, LightVars.Handles.LoadSaveCfg(1), sensitive=0, bad_id=tmp1  ; save config setup menu item
      ;choppers set and table
      widget_control, LightVars.Handles.ChoppersButtons(0), sensitive=0, bad_id=tmp1; set
      widget_control, LightVars.Handles.ChoppersButtons(2), editable=0, bad_id=tmp1; table 1
      widget_control, LightVars.Handles.ChoppersButtons(3), editable=0, bad_id=tmp1; table 2
      if mode eq 'extended' then begin  ; extended mode
        ; restricted actions
        widget_control, LightVars.Handles.SetupButtons(3), sensitive=1, bad_id=tmp1 ; auto button
        widget_control, LightVars.Handles.SetupButtons(4), sensitive=1, bad_id=tmp1 ; auto menu item
        widget_control, LightVars.Handles.Lambda, sensitive=1, bad_id=tmp1          ; lambda in Runs
        widget_control, LightVars.Handles.MonokButtons(1), sensitive=1, bad_id=tmp1 ; position
      endif else begin  ; normal mode
        ; Auto+Lambda in runs
        widget_control, LightVars.Handles.SetupButtons(3), sensitive=0, bad_id=tmp1 ; auto button
        widget_control, LightVars.Handles.SetupButtons(4), sensitive=0, bad_id=tmp1 ; auto menu item
        widget_control, LightVars.Handles.Lambda, sensitive=0, bad_id=tmp1          ; lambda in Runs
        ; Monok position, rocking and table edition
        widget_control, LightVars.Handles.MonokButtons(1), sensitive=0, bad_id=tmp1 ; position
      endelse
    endif else begin  ; SU mode
      ; Auto+Lambda in runs
      widget_control, LightVars.Handles.SetupButtons(3), sensitive=1, bad_id=tmp1 ; auto button
      widget_control, LightVars.Handles.SetupButtons(4), sensitive=1, bad_id=tmp1 ; auto menu item
      widget_control, LightVars.Handles.Lambda, sensitive=1, bad_id=tmp1          ; lambda in Runs
      ; Monok position, rocking and table edition
      widget_control, LightVars.Handles.MonokButtons(1), sensitive=1, bad_id=tmp1 ; position
      widget_control, LightVars.Handles.MonokButtons(0), sensitive=1, bad_id=tmp1 ; rocking
      widget_control, LightVars.Handles.MonokButtons(3), editable=1,  bad_id=tmp1 ; table
      ;choppers set and table
      widget_control, LightVars.Handles.ChoppersButtons(0), sensitive=1, bad_id=tmp1; set
      widget_control, LightVars.Handles.ChoppersButtons(2), editable=1, bad_id=tmp1; table 1
      widget_control, LightVars.Handles.ChoppersButtons(3), editable=1, bad_id=tmp1; table 2
      ; menu items Setup:Load/save config
      widget_control, LightVars.Handles.LoadSaveCfg(0), sensitive=1, bad_id=tmp1  ; load config setup menu item
      widget_control, LightVars.Handles.LoadSaveCfg(1), sensitive=1, bad_id=tmp1  ; save config setup menu item
    endelse
    
end ; PRO Light_Custom_IN6_Sensitive

;***************************************************************
;** (Full) Collect data from the Generic Instrument Control program
; if LightVars.Private.Update_Nb is 1 then performs a major update
pro Light_Custom_IN6_Collect, LightVars

  forward_function DialNewValue

  Update_Nb       = LightVars.Private.Update_Nb
  Previous_Status = LightVars.Mad.Status

  ; MINOR UPDATE SECTION ***************************************************

  ; checks frozen mode from Simulation flag
  if LightVars.Global.Simulation eq 3 then begin
    LightVars.Private.KeepMadData = 1
    LightVars.Global.Simulation   = 1
  endif

  ; if frozen mode, retain previous t_status (structure)
  ; else gets new Mad.t_status
  if LightVars.Private.KeepMadData ne 0 then t_status = LightVars.Mad.t_status $
  else begin
    t_status = DialNewValue(type='t_status')
    Tools_StructSetValue, struct=LightVars, tag='Mad.t_status',val= t_status
  endelse

  ; now sets Mad.Status (string)
  if size(LightVars.Mad.t_status, /type) ne 8 then begin ; not a structure. MAD is off-line
    LightVars.Mad.Status = strupcase(LightVars.Generic)+' INACTIVE (Idle)'
    Light_Event_Base_Simulation, LightVars, Simulation = 1L
    LightVars.Global.Simulation = 1L
  endif else begin
    if LightVars.Private.KeepMadData ne 0 then begin
      ; frozen mode
      Light_Event_Base_Simulation, LightVars, Simulation = 2L
      LightVars.Global.Simulation = 2L
      LightVars.Mad.Status = ['Frozen (Idle)']
    endif else begin
      ; gets MAD.status
      LightVars.Mad.Status = DialNewValue(type='status')
      if LightVars.Global.Simulation ne 0 then LightVars.Mad.Status = '['+LightVars.Mad.Status+']'
    endelse
    ; is Light in simulation mode ?
    if LightVars.Global.Simulation eq 1L then begin
      Light_LogMessage, LightVars, 'silent','[w] MAD is active, but in Simulation mode (no Action).'
      LightVars.Global.Simulation = 2L
    endif
  endelse
  if size(LightVars.Mad.t_nother, /type) eq 8 and strpos(strupcase(LightVars.Mad.Status), 'IDLE') ge 0 then $
    if LightVars.Mad.t_nother.count_flag eq 2 then LightVars.Mad.Status = 'Paused'

  ; if a normal major Update is requested, or Mad Status just changed
  Major_Flag = (Update_Nb le 1 or strcmp(LightVars.Mad.Status, Previous_Status) eq 0)
  if Update_Nb ne 0 and LightVars.Private.KeepMadData eq 0 and Major_Flag ne 0 then begin
    Update_Message= "<UPDATING>" 
    widget_control, LightVars.Handles.Mad_Status, set_value=Update_Message, bad_id=tmp1 
  endif else if Update_Nb eq 0 then begin
    Update_Message="** Starting **"
    widget_control, LightVars.Handles.Mad_Status, set_value=Update_Message, bad_id=tmp1 
  endif

  ; MAJOR UPDATE SECTION ***************************************************

  ; update possibilities: 
  ;   simulation (3)    : retain MAD values (does not perform any collect). Static = 1. Store  = 0
  ; else
  ;   retrieve previous values
  ;   simulation (0,1,2): gets MAD Data at major Updates or when Status changed. Static = 0. Store = 1
  if LightVars.Private.KeepMadData eq 0 and Major_Flag ne 0 then begin 
    ; Major_Update, and not Static
    ;** Get MAD data from MAD_IDL shared memory, and will put them in the LightVars.Mad tags
    ;** (if MAD is not running: status='Idle', values are 3.14 for t_*)
    t_nother = DialNewValue(type='t_nother')
    t_res    = DialNewValue(type='t_res')
    t_para   = DialNewValue(type='t_para')
    t_counts = DialNewValue(type='t_counts')
    t_chopper= DialNewValue(type='t_chopper')
    data     = DialNewValue(type='data')
  endif else begin
    ; retrieve old values, as we are going to test them (and store them)
    if Major_Flag ne 0 then begin ; here LightVars.Private.KeepMadData ne 0 
      Mad = LightVars.Mad
      t_nother = Mad.t_nother
      t_res    = Mad.t_res
      t_para   = Mad.t_para
      t_status = Mad.t_status
      t_counts = Mad.t_counts
      t_chopper= Mad.t_chopper
      data     = Mad.Data
    endif
  endelse

  if Major_Flag ne 0 then begin
    ; test new values. If they are not properly defined, redefine them.
    if size(t_chopper, /type) ne 8 then t_chopper = 3.14

    if size(t_res, /type) ne 8 then begin
      tempea = [4.2, 4.7+0.1*randomn(seed), 4.8+0.1*randomn(seed), 0.1+0.01*randomn(seed)]
      t_res = { TEMPEA:tempea, LASNUM:101L, N_TOTAL_SPECT:340L, N_MON_SPECT:3, N_SINGLE_SPECT:337L, $
                PHASE_REQ:[99.3492, 90.0000], PHASE_ACT:[99.0640,90.0010], $
                SPEED_REQ:[6020.00,0.000000], SPEED_ACT:[6019.00,6020.00], $
                CHOP_STATUS:[63L, 63L], NREPET:0L, NPREP:0L, JC:0L, XC:0L, ISODIS:1L, NUM_TEMP:100L }
      LightVars.Private.KeepMadData = 1
    endif
    if size(t_para, /type) ne 8 then begin
      t_para = {TOF_CHA_RESOL:512L, WAVE:4.12, TOF_CHA_WIDTH:77L, NUM_CHAN_USED:0L, $
                RAT_CHOPPER:1., SPE_CHOPPER:[6020.00,6020.00], ICRY:1L, $
                UPDATE_TIMER:20L, STATE_TIMER:0L, COMPUTE_PAR:0L, $
                DEPH_CHOPPER:[99.3492,90.0000], DEPH_OFFSET:0., PERIOD:4983.39, $
                CHA_WIDTH:9.62500,TOF_DELAY:991.375, ELPEAK:315L, DISTANCE:fltarr(6), $
                DIS_S_M:0., ALARM_TIMER:0L, $
                C_TXT:'', C_USER:'', C_LC:'', SUB_TITLE:'', ENVIRONMENT:12L }
      LightVars.Private.KeepMadData = 1
    endif
    if size(t_nother, /type) ne 8 then begin
      t_nother = {PCODE:0L, PSTATE:0L, CSTATE:0L, XBU_STATE:0L, XBU_INTER:0L, NFUNC:0L, NKEY:0L, $
                  CHOP_STOP_NUMBER:lonarr(2), CHOP_STOP_TIME:lonarr(2),$
                  INST_NAME:'IN6     ', NSWITCH:0L, NWHICHR:0L, COUNT_FLAG:0L, $
                  ACTANG: fltarr(16), motor_error:lonarr(16), $
                  PRESET_REST:30L, SUCCES:25L, SIM:0L, PCP_KILL:0L, $
                  STOP_TIME:'00:00:00', START_TIME:'00:00:00' }
      LightVars.Private.KeepMadData = 1
    endif
    if size(t_STATUS, /type) ne 8 then begin
      t_STATUS= {PCODE:0L, PSTATE:0L, CSTATE:0L, count_flag:0L, JC:0L, $
                  det_sum:0L, ss_counts:lonarr(2), cstate_flags:0L, motor_flags:0L }
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_status',val= t_status
    endif

    N_cha = t_para.tof_cha_resol
    N_det = t_res.N_TOTAL_SPECT

    if size(t_counts,  /type) eq 8 and n_elements(data) le 1 then data = t_counts.data
    if n_elements(data) le 1 then data = findgen(N_cha,N_det)+uint(5*randomn(seed, N_cha,N_det))

    data = reform(data(0:(N_cha*N_det-1)), N_cha, N_det) ; for IN6 only
    if size(t_counts,  /type) ne 8 then $
      t_counts = {data:data, det_sums:total(data,1), sum:total(data) }

    ; Test previous Mad structures if we are not starting Light 
    ; (that is some reasonable Mad.t_* structures were stored at start-up or updated after)
    if Update_Nb eq 1 then begin

      ; ** test MAD changes (Temp, wavelength, cumulated data...).
      ; now handle n_repetitions: should store data from memory into cumulated data (before MAD update in memory)
      ; except when doing 'wait T control' countings
      ; if no repetition (t_res.NPREP eq 1) or wrong size, data_cumulated = data*0
      ; if changed repetition number, Mad.t_res.NPREP lt t_res.NPREP data_cumulated = data_cumulated + LightVars.Mad.data
      if LightVars.Private.LastValidNumor(0) eq 0 then begin
        ; if no repeptition (NREPET = 1) or no valid size -> data_cumulated = data
        data_cumulated = LightVars.Private.Data_Cumulated
        if n_elements(LightVars.Private.Data_Cumulated) ne n_elements(LightVars.Mad.data) $
        or t_res.NREPET le 1 then data_cumulated = data $
        else begin
          ; if just changed repetition number (t_res.NPREP), and t_res.NREPET gt 1
          if LightVars.Mad.t_res.NPREP lt t_res.NPREP then begin ; detected t_res.NPREP++
            data_cumulated = data_cumulated + LightVars.Mad.data  ; add data before starting new repetition counting
          endif
        endelse
        Tools_StructSetValue, struct=LightVars, tag='Private.Data_Cumulated', val=data_cumulated
      endif
      ;detects wavelength changes
      if  LightVars.Mad.t_para.wave ne t_para.wave then begin
        msg = '[ ] Wavelength changed from '+strcompress(string(LightVars.Mad.t_para.wave))+' [K] to '+strcompress(string(t_para.wave))+' [K]'
        Light_LogMessage, LightVars, 'normal', msg
      endif
      ; look if a temperature change was performed
      if t_res.tempea(0) ne  LightVars.Mad.t_res.tempea(0) then begin
          msg = '[ ] Temperature changed from '+strcompress(string( LightVars.Mad.t_res.tempea(0)))+' [K] to '+strcompress(string(t_res.tempea(0)))+' [K] (Set Point)'
          Light_LogMessage, LightVars, 'normal', msg
          LightVars.Private.WaitTempTime = systime(1)
      endif

      ; ** test MAD errors  (Choppers, Monoks...).  
      ; handle possible MAD errors and operation conflicts ***********************
      if (t_nother.XBU_STATE ne 0 and LightVars.Mad.t_nother.XBU_STATE eq 0) $
      or (t_nother.XBU_INTER ne 0 and LightVars.Mad.t_nother.XBU_INTER eq 0) $
      and LightVars.Global.Simulation eq 0 then begin
        ; something is going on with XBU files, independently of Light. Stop current controls, Put Light to simulation
        Light_Base_Alert, Lightvars, error='XBU file conflict'
        Light_LogMessage, LightVars, 'silent','[I] Switching to Simulation.'
        Light_Event_Base_Simulation, LightVars, Simulation=1L
      endif
      ; MAD is already in simulation mode ?
      if t_nother.sim ne 0 and LightVars.Mad.t_nother.sim eq 0 then Light_Event_Base_Simulation, LightVars, Simulation=1L
      tmp0 = where(LightVars.Mad.t_nother.MOTOR_ERROR ne 0)
      tmp1 = where(t_nother.MOTOR_ERROR ne 0)
      if tmp1(0) ge 0 and tmp0(0) lt 0 then begin
        ; new MOTOR_ERROR: display ERROR
        Light_LogMessage, LightVars, 'silent','[E] Motor '+strcompress(string(tmp1+1))+' error: '+strcompress(string(t_nother.MOTOR_ERROR(tmp1)))
      endif
      ; Chopper error check (bit 64, 128 or 256 in CHOP_STATUS)
      ErrorBytes = (64 or 128 or 256)
      tmp0 = where((LightVars.Mad.t_res.Chop_Status and ErrorBytes) ne 0)
      tmp1 = where((t_res.Chop_Status and ErrorBytes) ne 0)
      if tmp1(0) ge 0 and tmp0(0) lt 0 then begin
        ; new Chopper ERROR: stop and display ERROR
        Light_Base_Alert, Lightvars, error='Chopper '+strcompress(string(tmp1))+' setting'
        Light_LogMessage, LightVars, 'silent','[E] Chopper error: '+strcompress(string(t_res.Chop_Status(tmp1)))
        Light_Event_Custom_IN6_Stop, LightVars, /choppers
      endif
      if t_para.ALARM_TIMER ne 0 and LightVars.Mad.t_para.ALARM_TIMER eq 0 and LightVars.Global.Simulation eq 0 then begin
        ; new Alarm ERROR: stop and display ERROR
        Light_Base_Alert, Lightvars, error='Alarm Timer'
        Light_LogMessage, LightVars, 'silent','[E] An alarm timer occured in MAD. Switching to Simulation.'
        Light_Event_Base_Simulation, LightVars, Simulation=1L
      endif
      if  LightVars.Mad.t_nother.PCP_KILL eq 0 and t_nother.PCP_KILL ne 0 and LightVars.Global.Simulation eq 0 then begin
        ; new PCP event
        Light_LogMessage, LightVars, 'silent','[I] PCP Kill/Pause detected.'
      endif

      ; handle Major_Update, not frozen, no simulation: rte
      if LightVars.Global.Simulation eq 0 then begin
        ; force Temp Read when not in automatic mode
        if t_para.icry eq 0 then Light_Control, LightVars, LightVars.Instrument.TempReadCommand
      endif
      
      ; Now transfert Mad.t_* structure 'passive' members to dedicated Light variables
      ; 'user','local', ... are updated after edition by Dialog_Fields in Light_Event_Custom_IN6_Edit_Header
      LightVars.Private.ExpTitle    = string(t_para.C_TXT)
      LightVars.Private.UserName    = strcompress(string(t_para.C_USER))
      LightVars.Private.LocalContact= strcompress(string(t_para.C_LC))
      LightVars.Private.Environement= strcompress(string(t_para.ENVIRONMENT),/remove_all)
      LightVars.Private.SubTitle    = strcompress(string(t_para.SUB_TITLE))+' (Env:'+LightVars.Private.Environement+')'
      ; build the current numor.
      if t_res.isodis eq 0 then numor = t_res.num_temp $
      else numor = t_res.lasnum
      numor = strcompress(string(numor),/remove_all)
      numor = numor+' ['
      
      if t_status.count_flag ne 0 then begin
        ; t_res.NPREP current repetition times,  until eq t_res.NREPET
        ; t_res.npmes current point (usually 1), until eq t_res.NKMES
        ; t_nother.COUNT_FLAG: 0='not counting', 1='counting', 2='pause', 3='update', 4:'stop nosave'
        ; t_res.JC: preselection type: 1='time', 2='monitor'
        ; t_res.XC: preselection value (integer)
        numor = numor+strcompress(string(t_res.XC), /remove_all)
        if t_res.JC eq 1 then numor = numor+' Ti' else numor = numor+' Mon'
        numor = numor+', #'+strcompress(string(t_res.NPREP), /remove_all)+'/'+strcompress(string(t_res.NREPET), /remove_all)
        start_time = strsplit(string(t_nother.START_TIME), /extract)
        start_time = start_time(n_elements(start_time)-1)
        numor = numor+', '+start_time
      endif else begin
        stop_time = strsplit(string(t_nother.STOP_TIME), /extract)
        stop_time = stop_time(n_elements(stop_time)-1)
        numor = numor+' ended '+stop_time
      endelse
      numor = numor+']'
      ; detects change of Run numor
      if t_res.lasnum ne LightVars.Mad.t_res.lasnum then begin
        msg = 'Saved Run '+numor+' T='+strcompress(string(t_res.tempea(2)))+' K, " '+strcompress(string(t_para.SUB_TITLE))+'"'
        Light_LogMessage, LightVars, 'normal', msg
      endif

      LightVars.Private.Numor       = numor
      dims      = size(data, /dim)
      ndata     = data(0:(dims(0) -2), *) ; remove last channel = integral
      Monitor   = ndata(*,0)              ; this is Monitor1
      ndata     = ndata(*,t_res.N_MON_SPECT:(t_res.N_TOTAL_SPECT-1)) ; for IN6, remove monitor channels
      detectors = total(ndata,1)          ; Detectors on X (length=340), sum of row
      Temperatures = t_res.tempea
      TReg    = Temperatures(1)
      TSample = Temperatures(2)

      LightVars.Private.MonitorSum  = total(Monitor)
      LightVars.Private.DetectorSum = total(detectors)
      LightVars.Private.TSample     = Tsample
      LightVars.Private.TRegulation = TReg

    endif ; Update_Nb gt 0

    ; will also store new values
  endif ; Major_Flag (startup, or Major_Update or Status changed), also in forzen/simulation mode

  ; on-going SubDial processes ***********************************************
  ; handle on-going run/sequence process
  Light_SubDial_Custom_IN6_Check_Run, LightVars

  ; handle on-going monochromator setting process
  Light_SubDial_Custom_IN6_Check_Monochromators, LightVars

  ; handle on-going chopper setting process
  Light_SubDial_Custom_IN6_Check_Choppers, LightVars

  ; handle on-going check elastic peak process
  Light_SubDial_Custom_IN6_Check_Elastic_Peak, LightVars

  if Major_Flag ne 0  then begin
    ; store Mad variables into LightVars
    if Update_Nb le 1 then begin
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_nother', val= t_nother
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_res'   , val= t_res
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_para'  , val= t_para
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_counts', val= t_counts
      Tools_StructSetValue, struct=LightVars, tag='Mad.t_chopper',val= t_chopper
      Tools_StructSetValue, struct=LightVars, tag='Mad.Data',     val= data
    endif
  endif

end ; PRO Light_Custom_IN6_Collect

;***************************************************************
; compute Lambda, Ei, vi, Ki from last modified field (if valid).
; update Instrument fields from widget handles after edition
; optionally transfert Data from MAD into widgets (when edition of widget is off)
pro Light_Custom_IN6_Setup_Check, LightVars, Changed=Changed, FromMad=FromMad, PhaseSpeed=PhaseSpeed, wavelength=wavelength
; get current widget values

  if widget_info(LightVars.Handles.EnergySetup, /valid_id) eq 0 then return

  Mn     = 1.674928e-27 ; [kg] mass of neutron
  hbar   = 1.054572e-34 ; [Js] h bar Planck constant
  meV    = 1.6021773e-22; [J]
  kTOv   = hbar*1e10/Mn                 ; k   [A-1]    -> v [m/s] constant (629.622)
  k2TOe  = hbar*1e10/Mn/meV/2*1e10*hbar ; k2  [A-2]    -> E [meV] constant (2.07212)

; first look for the Value which is modified from Private.EnergyValues (if valid else return)

  if n_elements(Changed) gt 0 then begin
  ; update Energy values...
    if widget_info(LightVars.Handles.Energy(Changed), /name) eq 'DROPLIST' then begin
      res = widget_info(LightVars.Handles.Energy(Changed), /DROPLIST_select)
      value = Lightvars.Instrument.EnergyDefinitions(res)
    endif else begin
      widget_control, LightVars.Handles.Energy(Changed), get_value=Value
    endelse

    if n_elements(FromMad) gt 0 then Value    = LightVars.Mad.t_para.wave
    if n_elements(wavelength) ne 0 then Value = wavelength

    res = execute('Value = float(Value)')
    if res eq 0 or Value eq 0 then return
    case Changed of
      0: Lambda = Value                       ; lambda
      1: Lambda = sqrt(k2TOe*4*!pi*!pi/Value) ; energy
      2: Lambda = (2*!pi)/Value               ; k
      3: Lambda = (2*!pi*kTOv)/Value          ; v
      else: return
    endcase

    k =(2*!pi)/Lambda
    v = kTOv*abs(k)
    E = k2TOe*k^2
    NewValues = [ Lambda, E, k, v ]

    EnergyTable = ['wavelength','energy','wavevector','velocity']

    ; now constrain energy values following the EnergyOptions fields: look for unit to use
    EnergyUnit = 0
    for index = 0,3 do $
      if strpos(Lightvars.Instrument.EnergyOptions, EnergyTable(index)) ge 0 then EnergyUnit = index

    EnergyValue = NewValues(EnergyUnit)
    ; now set the constrain mode
    tmpe = EnergyValue
    if strpos(Lightvars.Instrument.EnergyOptions, 'limits') ge 0 then begin
      if EnergyValue lt min(Lightvars.Instrument.EnergyDefinitions) then EnergyValue = min(Lightvars.Instrument.EnergyDefinitions)
      if EnergyValue gt max(Lightvars.Instrument.EnergyDefinitions) then EnergyValue = max(Lightvars.Instrument.EnergyDefinitions)
      if tmpe ne EnergyValue then $
        Light_LogMessage, LightVars, 'normal','[w] Constraining '+EnergyTable(EnergyUnit)+' '+string(tmpe)+' within limits: '+strjoin(string(Lightvars.Instrument.EnergyDefinitions))
    endif else if strpos(Lightvars.Instrument.EnergyOptions, 'discrete') ge 0 then begin
      idx=-1 &  tmp1=min(abs(Lightvars.Instrument.EnergyDefinitions-EnergyValue), idx)
      droplist_select = idx
      LightVars.Private.EnergyIndex  = idx
      if Lightvars.Instrument.EnergyDefinitions(idx) ne EnergyValue then begin
        EnergyValue = Lightvars.Instrument.EnergyDefinitions(idx)
        Light_LogMessage, LightVars, 'normal','[w] Constraining '+EnergyTable(EnergyUnit)+' '+string(tmpe)+' to nearest discrete value in ['+strjoin(string(Lightvars.Instrument.EnergyDefinitions))+']'
      endif
    endif
    ; recompute values
    Value = EnergyValue
    case EnergyUnit of
      0: Lambda = Value                       ; lambda
      1: Lambda = sqrt(k2TOe*4*!pi*!pi/Value) ; energy
      2: Lambda = (2*!pi)/Value               ; k
      3: Lambda = (2*!pi*kTOv)/Value          ; v
      else: return
    endcase
    k =(2*!pi)/Lambda
    v = kTOv*abs(k)
    E = k2TOe*k^2
    NewValues = [ Lambda, E, k, v ]

    LightVars.Private.EnergyValues = NewValues
    LightVars.Private.EnergyUnit   = EnergyTable(EnergyUnit)
    LightVars.Private.EnergyToSet  = NewValues(EnergyUnit)

    Lightvars.Private.Elastic_Peak_Chan = Lightvars.Instrument.ElastPeakChannels(LightVars.Private.EnergyIndex)

    ; set widget values
    for index=0,3 do begin
      if widget_info(LightVars.Handles.Energy(index), /name) eq 'DROPLIST' then $
        widget_control, LightVars.Handles.Energy(index), set_droplist_select=droplist_select, Bad_Id=tmp1 $
      else $
        widget_control, LightVars.Handles.Energy(index), set_value=strcompress(string(NewValues(index)), /remove_all), Bad_Id=tmp1
    endfor

    ; update status and window titles
    EnergyStr = strcompress(string(LightVars.Private.EnergyToSet, format='(F4.2)'), /remove_all)
    TitleEner = '['+strupcase(LightVars.Instrument.Name)+'] Setup > Instrument ('+EnergyStr+')'
    TitleChop = '['+strupcase(LightVars.Instrument.Name)+'] Setup > Instrument > Choppers ('+EnergyStr+')'
    TitleMono = '['+strupcase(LightVars.Instrument.Name)+'] Setup > Instrument > Monochromators ('+EnergyStr+')'

    IsReady = 1
    for index=0,2 do begin
      if Value ne LightVars.Instrument.SetupEnergies(index) then begin
        NewStatus = 'Not Set'
        if index eq 0 then TitleChop = '* '+TitleChop
        if index eq 1 then TitleMono = '* '+TitleMono
        IsReady = 0
      endif else begin
        NewStatus = 'Ready'
      endelse
      if LightVars.Instrument.SetupEnergies(index) ne 0 then NewStatus = NewStatus+' ('+strcompress(string(LightVars.Instrument.SetupEnergies(index), format='(F6.2)'), /remove_all)+')'
      widget_control, LightVars.Handles.SetupStatus(index), set_value=NewStatus
      if index eq 0 then begin
        if n_elements(FromMad) ne 0 then NewStatus =  NewStatus+'. Data retrieved from MAD.'
        widget_control, LightVars.Handles.ChoppersButtons(1), set_value=NewStatus, Bad_Id=tmp1
      endif
      if index eq 1 then widget_control, LightVars.Handles.MonokButtons(2),    set_value=NewStatus, Bad_Id=tmp1
    endfor


    if IsReady eq 0 then begin
      TitleEner = '* '+TitleEner
    endif

    widget_control, LightVars.Handles.EnergySetup,   tlb_set_title= TitleEner, Bad_ID=Bad_ID
    widget_control, LightVars.Handles.ChoppersSetup, tlb_set_title= TitleChop, Bad_ID=Bad_ID
    widget_control, LightVars.Handles.MonokSetup,    tlb_set_title= TitleMono, Bad_ID=Bad_ID

    widget_control, LightVars.Handles.SetupButtons(8), set_value= LightVars.Private.Elastic_Peak_Chan, Bad_ID=Bad_ID

  endif ; Changed

  if n_elements(FromMad) ne 0 then begin
    ; get from MAD instrument parameters (choppers, tof)
    ; not updated yet: ReferencePhase
    Mad = LightVars.Mad
    t_para = Mad.t_para
    t_res  = Mad.t_res
    if n_elements(PhaseSpeed) eq 0 then begin
      LightVars.Instrument.ChopperSpeedRatio  = t_para.RAT_CHOPPER
      LightVars.Private.Elastic_Peak_Chan     = t_para.ELPEAK
      LightVars.Instrument.N_channels         = t_para.TOF_CHA_RESOL
      LightVars.Instrument.PhaseOffset        = t_para.DEPH_OFFSET
      ; LightVars.Instrument.Focus_Energy       = t_para.HOMEGA
    endif
    LightVars.Private.Period                = t_para.PERIOD
    LightVars.Private.Delay                 = t_para.TOF_DELAY
    LightVars.Private.SpeedFermiTarget      = t_para.SPE_CHOPPER(0)
    LightVars.Private.PhaseFermiTarget      = t_para.DEPH_CHOPPER(0)
    LightVars.Private.SpeedFermi            = t_res.SPEED_ACT(0)
    LightVars.Private.PhaseFermi            = t_res.PHASE_ACT(0)
    LightVars.Private.SpeedSupprTarget      = t_para.SPE_CHOPPER(1)
    LightVars.Private.PhaseSupprTarget      = t_para.DEPH_CHOPPER(1)
    LightVars.Private.SpeedSuppr            = t_res.SPEED_ACT(1)
    LightVars.Private.PhaseSuppr            = t_res.PHASE_ACT(1)
    LightVars.Private.ChannelWidth          = t_para.CHA_WIDTH

    ; now update associated widgets
    ; chopper/monok/DoElastpeak/auto/stop/focusEn/ChanNb/Ratio/ElPeak
    widget_control, LightVars.Handles.SetupButtons(5), set_value= LightVars.Instrument.Focus_Energy
    widget_control, LightVars.Handles.SetupButtons(7), set_value= LightVars.Instrument.N_channels
    widget_control, LightVars.Handles.SetupButtons(6), set_value= LightVars.Instrument.ChopperSpeedRatio
    widget_control, LightVars.Handles.SetupButtons(8), set_value= LightVars.Private.Elastic_Peak_Chan
    Lightvars.Instrument.ElastPeakChannels(LightVars.Private.EnergyIndex) = LightVars.Private.Elastic_Peak_Chan
  endif else begin
    ; now get value from associated widgets
    ; chopper/monok/DoElastpeak/auto/stop/focusEn/ChanNb/Ratio/ElPeak
    widget_control, LightVars.Handles.SetupButtons(5), get_value= a & LightVars.Instrument.Focus_Energy       = a
    widget_control, LightVars.Handles.SetupButtons(7), get_value= a 
    if a ne LightVars.Instrument.N_channels then begin
      LightVars.Private.Elastic_Peak_Chan = round( LightVars.Private.Elastic_Peak_Chan*(double(a)/LightVars.Instrument.N_channels))
      LightVars.Instrument.N_channels = a
      widget_control, LightVars.Handles.SetupButtons(8), set_value= LightVars.Private.Elastic_Peak_Chan
    endif else begin
      widget_control, LightVars.Handles.SetupButtons(8), get_value= a
      LightVars.Private.Elastic_Peak_Chan     = a
    endelse
    widget_control, LightVars.Handles.SetupButtons(6), get_value= a & LightVars.Instrument.ChopperSpeedRatio  = a

    Lightvars.Instrument.ElastPeakChannels(LightVars.Private.EnergyIndex) = LightVars.Private.Elastic_Peak_Chan

  endelse
  
  ; re-calculate phases, speeds, etc...
  Light_Custom_IN6_Calc_TOF_Choppers, LightVars, Wavelength=LightVars.Private.EnergyToSet

  for index=0,2 do begin
    ; now update AutoFlags
    widget_control, LightVars.Handles.SetupFlags(index), get_value=tmp1
    LightVars.Instrument.AutoFlags(index) = tmp1
  endfor

  ; now put the instrument specific configuration to SU/normal Mode (call InitPro in SU check mode)
  ;** execute Custom InitPro SU check procedure
  Light_Custom_IN6_Sensitive, LightVars

  ; update choppers tables with values
  ThisData = [ LightVars.Private.SpeedFermiTarget, Lightvars.Instrument.ChopperSpeedRatio, $
    LightVars.Private.PhaseFermiTarget, LightVars.Private.PhaseSupprTarget, Lightvars.Instrument.PhaseOffset ]
  widget_control, LightVars.Handles.ChoppersButtons(2), set_value=transpose(ThisData), bad_id=tmp

  ThisData = [ LightVars.Private.Period, LightVars.Private.ChannelWidth, LightVars.Private.DeadTime, LightVars.Private.Delay ]
  widget_control, LightVars.Handles.ChoppersButtons(3), set_value=transpose(ThisData), bad_id=tmp

  ; update Monok table shown on Monok window (if opened already)
  Light_Event_Custom_IN6_Setup_Monochromators, LightVars, /update

end ; PRO Light_Custom_IN6_Setup_Check

;***************************************************************
; choppers and TOF parameters computation
; based on original Fortran code by J. Cook (in6:mad:compute_par.f)
; improved by A. Schober for Light 1.0
; also displays detailed information in text window
pro Light_Custom_IN6_Calc_TOF_Choppers, LightVars, Wavelength=Wavelength, show=show

   ; inputs for computation (from Mad and edit boxes)
  if n_elements(Wavelength) eq 0 then begin
    Wavelength = LightVars.Private.EnergyToSet
  endif

  ; parameters that are obtained from Instrument (may be changed within IN6 Setup Interfaces)
  ratio       = Lightvars.Instrument.ChopperSpeedRatio     ; [1]   speed ratio Fermi/Supp
  homega      = Lightvars.Instrument.Focus_Energy          ; [meV] focusing to specified energy transfert
  el_peak     = Lightvars.Private.Elastic_Peak_Chan     ; [315] Elastic_Peak position
  n_chan      = Lightvars.Instrument.N_channels            ; [512] Number of time channels

  ; other parameters that are obtained from Instrument (usually not changed)
  de          = Lightvars.Instrument.MonokDSpacing         ; [AA] lattice d-spacing for monochromators
  lcc         = Lightvars.Instrument.Distances_CC_CS_SD(0) ; [m] Chopper1-Chopper2 distance
  lce         = Lightvars.Instrument.Distances_CC_CS_SD(1) ; [m] Chopper2-Sample distance
  led         = Lightvars.Instrument.Distances_CC_CS_SD(2) ; [m] Sample-Detector distance
  el_t_resol  = Lightvars.Instrument.ElectronicTimeBase    ; [us] Electronic Time Base
  ref_phas    = Lightvars.Instrument.ReferencePhase        ; [deg] Reference Phase
  phase_offset= Lightvars.Instrument.PhaseOffset           ; [deg] Phase Offset (added to Fermi phase)
  el_delay    = Lightvars.Instrument.ElectronicDelay          ; [us]  Default Electronic Delay

  ; other parameters that are computed
  Mn     = 1.674928e-27 ; [kg] mass of neutron
  hbar   = 1.054572e-34 ; [Js] h bar Planck constant
  meV    = 1.6021773e-22; [J]
  kTOv   = hbar*1e10/Mn                 ; k   [A-1]    -> v [m/s] constant (629.622)
  k2TOe  = hbar*1e10/Mn/meV/2*1e10*hbar ; k2  [A-2]    -> E [meV] constant (2.07212)
  ce     = 1/(kTOv*2*!pi)

  k      =(2*!pi)/Wavelength     ; incident wavevector [AA-1]
  Ezero  = k2TOe*k^2         ; incident energy     [meV]
  if LightVars.Instrument.MonokUseOldValues eq 0 then begin
    Theta  = asin(Wavelength/2/de)  ; Monochromator rotation for Bragg reflexion old=37.66   43.28   49.47   61.55
  endif else begin
    ThetaBlancValues = [37.66, 43.28, 49.47, 61.55]*!pi/180 ; Values of monochromator angles from Y. Blanc, Rap. ILL/83BL21G
    Theta = ThetaBlancValues(LightVars.Private.EnergyIndex)
  endelse
  neut_vel = kTOv*abs(k)

  if abs(homega) gt 0.75*abs(Ezero) then begin
    LightVars.Instrument.Focus_Energy = 0.75*abs(Ezero)
    homega = LightVars.Instrument.Focus_Energy
    widget_control, LightVars.Handles.SetupButtons(5), set_value= homega
    Light_LogMessage, LightVars, 'normal','[w] Focusing Energy transfert should not exceed 3/4 of the incident energy ('+strcompress(string(Ezero))+'). Constraining to '+strcompress(string(homega))

  endif

  ; Here we go...
  aa          = (1-(homega/ezero))^(-1.5)
  aa          = lce+(led*aa)
  bb          = ce*de*cos(theta)
  dd          = 1/(bb*aa)
  speed       = (dd*60)/(2*!pi)
  period      = 0.5e6 * 60 * ratio/speed
  chan_width  = period/n_chan
  chan_width  = fix(chan_width/el_t_resol)*el_t_resol
  dead_time   = period-(n_chan*chan_width)
  time_of_flight = (lcc+lce+led)/neut_vel*1e6
  trav_time   = lcc/neut_vel*1e6
  delta_phase = (trav_time/(period))*180.
  phas_ferm   = ref_phas + delta_phase
  if (ratio mod 2) eq 0 then phas_ferm=2*phas_ferm
  phas_ferm   = phas_ferm + phase_offset
  el_peak_O   = fix((time_of_flight + el_delay)/chan_width)
  delay       =  (el_peak_O-el_peak) * chan_width
  if el_peak ge el_peak_O then delay   =  delay + period
  if delay le 1 then delay = 2

  ; transfert results to LightVars.Private values
  LightVars.Private.Delay             = delay       ; delay                     para.TOF_DELAY=delay
  LightVars.Private.SpeedFermiTarget  = speed       ; Fermi chopper speed para.SPE_CHOPPER(1)
  LightVars.Private.PhaseFermiTarget  = phas_ferm   ; Fermi chopper phase para.DEPH_CHOPPER(1)
  LightVars.Private.SpeedSupprTarget  = speed/ratio ; Suppressor speed    para.SPE_CHOPPER(2)
  LightVars.Private.PhaseSupprTarget  = ref_phas   ; Suppressor phase    para.DEPH_CHOPPER(2)
  LightVars.Private.Period            = period      ; Chopper period (us)       para.PERIOD
  LightVars.Private.ChannelWidth      = chan_width  ; Channel Width (us)        para.CHA_WIDTH
  LightVars.Private.DeadTime          = dead_time

  ; display all computation results as a table

  Values = [ $
  Wavelength, $
  neut_vel, $
  theta*180/!pi, $
  Ezero, $
  homega, $
  trav_time, $
  (lcc+lce+led)/neut_vel*1e6, $
  delay, $
  dead_time, $
  period, $
  chan_width, $
  phas_ferm, $
  ref_phas, $
  speed, $
  speed/ratio, $
  n_chan, $
  el_peak, $
  el_peak_O ]

  Labels    = [ $
  'Wavelength               [AA] ', $
  'Neutron velocity         [m/s]', $
  'Monochr. Bragg angle     [deg]', $
  'Incident Energy          [meV]', $
  'Focusing Energy Transfert[meV]', $
  'Travel time: Fermi/Supp. [us] ', $
  'Travel time: Supp/Det.   [us] ', $
  'TOF Delay                [us] ', $
  'TOF Dead Time            [us] ', $
  'TOF Period (1 cycle)     [us] ', $
  'TOF Channel width        [us] ', $
  'CHOP Fermi Phase         [deg]', $
  'CHOP Suppressor Phase    [deg]', $
  'CHOP Fermi Speed         [rpm]', $
  'CHOP Suppressor Speed    [rpm]', $
  'Number of time channels       ', $
  'Current Elastic Peak Ch.      ', $
  'Elast. peak ch. for 0-delay   ' ]

  ; Now look the Chopper Status bytes
  t_res = LightVars.Mad.t_res
  StatusString = [ $
    'microC program runs       ', $
    'nominal speed achieved    ', $
    'actual phase within gate  ', $
    'magnetic bearing on       ', $
    'DC supply voltage on      ', $
    'drive generator on        ', $
    'drive in DC(stop-) mode   ', $
    'emergency rundown released', $
    'rotor in open position    ' ]

  ThisByte = 1
  for index=1,9 do begin
    if (t_res.Chop_Status[0] and ThisByte) ne 0 then ChopStatStr1 = 'X' else ChopStatStr1 = '.'
    if (t_res.Chop_Status[1] and ThisByte) ne 0 then ChopStatStr2 = 'X' else ChopStatStr2 = '.'
    ToAdd = '  '+ChopStatStr1+'     '+ChopStatStr2+'   '+StatusString[index-1]
    if index eq 1 then ChopStatStr = [ ToAdd ] else ChopStatStr = [ ChopStatStr, ToAdd ]
    ThisByte = ThisByte*2
  endfor
  ; now adds-up the actual Phase and speed for each chopper
  ChopStatStr = [ ChopStatStr, '', '', $
    strcompress(string(t_res.Phase_act[0])), $
    '       '+strcompress(string(t_res.Phase_act[1])), $
    strcompress(string(t_res.speed_act[0])), $
    '       '+strcompress(string(t_res.speed_act[0])), $
    '', '(corresponding to TOF Delay)', '']

  ToDisplay = ['IN6 Chopper/Time of flight calculated settings', 'Date: '+systime(0), '', $
    '** Calculated values                        Fermi/Suppr.:Chopper Status', $
                Labels+string(Values, format='(A13)')+' '+ChopStatStr ]

  widget_control, LightVars.Handles.ChopTofText, set_value=ToDisplay, Bad_ID=Bad_ID

  if Bad_ID ne 0 or LightVars.Handles.ChopTofText eq 0 and n_elements(show) ne 0 then  begin
    xdisplayfile, 'chopperstof.cfg', text=ToDisplay, Group=LightVars.Handles.EnergySetup, $
          title='[IN6] TOF/Choppers', /editable, $
          Height=25,Width=80,Done_Button='Close [Choppers/TOF]', WText=ID
    LightVars.Handles.ChopTofText = ID
  endif

end ; PRO Light_Custom_IN6_Calc_TOF_Choppers

;***************************************************************
;** Display experiment header and send it to mad
pro Light_Event_Custom_IN6_Edit_Header, LightVars

  Header = {Header, User_Name:LightVars.Private.Username,$
            Local_Contact:LightVars.Private.LocalContact,$
            Exp_Title:LightVars.Private.ExpTitle,$
            Environement:LightVars.Private.Environement}

  Dialog_Fields, Header, Group=LightVars.Handles.Base, COLUMN_WIDTHS=300, $
    Name='Please enter new experiment header', Title='Exp. Header in Light', $
    FieldNames = [ 'User Name', 'Local Contact', 'Exp Title', 'Environement Code']

  ;** check if fields where changed, and send modifications to MAD
  Light_Control, LightVars, string(Header.User_Name+' '+Header.Local_Contact, $
                                                        format='(%"'+LightVars.Instrument.SetUserCommand+'")')
  Light_Control, LightVars, string(strtrim(Header.Exp_Title,2),    format='(%"'+LightVars.Instrument.SetTitleCommand+'")')
  Light_Control, LightVars, string(Header.Environement, format='(%"'+LightVars.Instrument.SetEnvirCommand+'")')

end ; PRO Light_Event_Custom_IN6_Edit_Header

;***************************************************************
; stop all in6 setup procedures
pro Light_Event_Custom_IN6_Stop, LightVars, motors=motors, choppers=choppers

  MonokOn    = ''
  ChoppersOn = ''
  CheckElPOn = ''
  RunIsOn    = ''
  if LightVars.Handles.SetupStatus(0) ne 0 then $
    widget_control, LightVars.Handles.SetupStatus(0), get_uvalue=ChoppersOn, bad_id=tmp
  if LightVars.Handles.SetupStatus(1) ne 0 then $
    widget_control, LightVars.Handles.SetupStatus(1), get_uvalue=MonokOn,    bad_id=tmp

  
  if strlen(MonokOn) gt 0 or n_elements(motors) ne 0 then begin
    ; stop Monochromators
    Light_LogMessage, LightVars, 'silent','[I] STOP Monochromators.'
    Light_Control, LightVars, LightVars.Instrument.StopCommand
    widget_control, LightVars.Handles.SetupStatus(1), set_uvalue='', bad_id=tmp
  endif else begin
    if strpos(strupcase(LightVars.Mad.Status), 'COUNTING') ge 0 then begin
      Light_LogMessage, LightVars, 'silent','[I] STOP current counting'
      Light_Control, LightVars, LightVars.Instrument.StopCommand
    endif
  endelse

  if strlen(ChoppersOn) gt 0 or n_elements(choppers) ne 0 then begin
    ; stop Choppers
    Light_LogMessage, LightVars, 'silent','[I] STOP Choppers'
    widget_control, LightVars.Handles.SetupStatus(0), set_uvalue='', bad_id=tmp
    Light_Control, LightVars, LightVars.Instrument.ChoppersStopCommand
  endif
  
  ;stop auto process, check elastic peak
  LightVars.Private.AutoProcess = 0
  widget_control, LightVars.Handles.SetupStatus(2), set_uvalue='', bad_id=tmp
  
  Light_Custom_IN6_Sensitive, LightVars

end ; PRO Light_Event_Custom_IN6_Stop

;***************************************************************
; Defautl displays for IN6
pro Light_Event_Custom_IN6_Default_Displays, LightVars

  OPTIONS    = [ '/log, name_IX="Time_Channels",Name_IY="Detectors", comment="X:TOF,Y:Det."', $
                 'Name_IX="Det", Name_IY="Signal"', 'name_IX="Time_Channels", Name_IY="Signal", /log', $
                 '/catenate, display_name="temperatures", name_IX="Time", Name_IY="Temp"', $
                 '/catenate, display_name="temperatures"', $
                 '/catenate, display_name="temperatures"' ]
  TYPE       = [ 'image', '', '', '', '', '' ]
  VARIABLES  = [ 'data', 'detectors', 'tof', 'TReg', 'TSet', 'TSample' ]
  
  Light_LogMessage, LightVars, 'normal','[I] Default IN6 Displays were installed. They will appear after next Displays Refreshing.'
  
  Tools_StructSetValue, struct=LightVars, Tag='Displays.Variables', val=VARIABLES
  Tools_StructSetValue, struct=LightVars, Tag='Displays.Options',   val=OPTIONS
  Tools_StructSetValue, struct=LightVars, Tag='Displays.Type',      val=TYPE

end ; PRO Light_Event_Custom_IN6_Default_Displays

;*****************************************
; display incident energy/lambda/ki config,
; and enable automatic calibration (monok, choppers, etc...)
; as well as elastic peak range settings
pro Light_Event_Custom_IN6_Setup_Build, LightVars, auto=auto, help=help, wavelength=wavelength, update=update

  if n_elements(help) ne 0 then begin
    ;** Display information about Light/Light_Event_Custom_IN6_Setup_Build

    ToDisplay =['IN6 Instrument settings main window Help in "Light on George"',$
                '',$
                'This window enables to change IN6 Instrument configuration (incident energy,', $
                'monochromators, choppers, and time-of-flight). It displays the incident energy', $
                'being used, or to be changed. Both menu and buttons lead to the display, and', $
                'eventually the modification, of the current settings.', $
                '', $
                '[Auto]', $
                '   performs an automatic energy change for each specified process (when "auto"', $
                '   boxes are checked), in the order (only in SuperUser mode):', $
                '   1-choppers and TOF settings,', $
                '   2-monochromators positioning,', $
                '   3-check of the elastic peak position.', $
                '[Choppers]', $
                '   display Choppers and TOF settings (speed, phase, times), and',$
                '   enable their setting (only in SuperUser Mode)',$
                '   A detailed Choppers/TOF status may be displayed.', $
                '[Monochromators]',$
                '   display Monochromator settings (energy, position), and',$
                '   enable their setting (only in SuperUser Mode)',$
                '[Elastic Peak]',$
                '   measure and display Elastic Peak position',$
                '[STOP]', $
                '   Stop all current settings and countings processes' $
                ]

    widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='IN6 Monochromators Help in Light'
    if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 and n_elements(update) eq 0 then  begin
      xdisplayfile, 'monochromators.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
            title='Monochromators Help in Light', /editable, $
            Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
      LightVars.Handles.Base_Help = ID
    endif

    return
  endif

  ;** Raise existing Energy Setup window
  widget_control, LightVars.Handles.EnergySetup, Bad_ID=Bad_ID, show=1, iconify=0
  ;** If does not exist, make it
  if Bad_ID ne 0 or LightVars.Handles.EnergySetup eq 0 then  begin
    ; build EnergySetup window
    LightVars.Handles.EnergySetup  = widget_base(Group_Leader=LightVars.Handles.Base, $
                      title='['+strupcase(LightVars.Instrument.Name)+'] Setup > Instrument', mbar=Base_Menu, /column)
    ; Menu bar for manual operations
    Base_Run=widget_button(Base_Menu, value='Operations >',             uvalue='Light_Event_Custom_IN6_Setup_Build, LightVars', menu=2)
    tmp1= widget_button(Base_Run,value='SuperUser login...',   uvalue='Light_Event_Base_SuperUser, LightVars, Event=Event')
    LightVars.Handles.SetupButtons(4) = widget_button(Base_Run,value='Automatic setup...', uvalue='Light_Event_Custom_IN6_Setup_Build, LightVars, /auto')
    tmp1= widget_button(Base_Run,value='Choppers setup...',             uvalue='Light_Event_Custom_IN6_Setup_Choppers, LightVars', /  separator)
    tmp1= widget_button(Base_Run,value='Show Choppers detailed status...',uvalue='Light_Custom_IN6_Calc_TOF_Choppers, LightVars,   /show')
    tmp1= widget_button(Base_Run,value='Monochromators setup...',       uvalue='Light_Event_Custom_IN6_Setup_Monochromators, LightVars')
    LightVars.Handles.SetupButtons(9)= widget_button(Base_Run,value='Check elastic peak position...',uvalue='Light_SubDial_Custom_IN6_Check_Elastic_Peak, LightVars, /auto')
    tmp1= widget_button(Base_Run,value='STOP',                          uvalue='Light_Event_Custom_IN6_Stop, LightVars')
    tmp1= widget_button(Base_Run,value='Exit IN6 Setup',                uvalue='widget_control, LightVars.Handles.EnergySetup,   /destroy', /separator)

    Base_Run=widget_button(Base_Menu, value='Help >',                   uvalue='Light_Event_Custom_IN6_Setup_Build, LightVars  ', menu=2)
    tmp1= widget_button(Base_Run,value='About [Instrument setup]', uvalue='Light_Event_Custom_IN6_Setup_Build, LightVars  , /help')
    tmp1= widget_button(Base_Run,value='About [Choppers setup]',        uvalue='Light_Event_Custom_IN6_Setup_Choppers, LightVars, /help', /separator)
    tmp1= widget_button(Base_Run,value='About [Monochromators setup]',  uvalue='Light_Event_Custom_IN6_Setup_Monochromators, LightVars, /help')
    tmp1= widget_button(Base_Run,value='About [Check elastic peak]',    uvalue='Light_SubDial_Custom_IN6_Check_Elastic_Peak, /help')
    ; Incident Energy displays:
    ; we do the same as in Setup_Check, as window build occurs before call to that procedure
    tmp1  = widget_base(LightVars.Handles.EnergySetup, /row, frame=1)
    XSize = 7

    Titles=['Wavelength [AA]', 'Energy [meV]', 'Wavevector [AA-1]', 'Velocity [m/s]']
    for index=0,3 do begin
      ThisEnergyTitle = strsplit(Titles(index), /extract)
      ThisEnergyTitle = ThisEnergyTitle(0)
      if strpos(Lightvars.Instrument.EnergyOptions, 'discrete') ge 0 $
         and strpos(Lightvars.Instrument.EnergyOptions, strlowcase(ThisEnergyTitle)) ge 0 then begin
        tmp2 = widget_base(tmp1, /column)
        tmp3 = widget_label(tmp2, value=Titles(index))
        LightVars.Handles.Energy(index)  = widget_droplist(tmp2, value=string(Lightvars.Instrument.EnergyDefinitions), $
          uvalue  ='Light_Custom_IN6_Setup_Check, LightVars, Changed='+strcompress(string(index),/remove_all), title='')
      endif else LightVars.Handles.Energy(index)  = cw_field(tmp1, title=Titles(index), value  = 0, xsize=XSize,  $
        uvalue  ='Light_Custom_IN6_Setup_Check, LightVars, Changed='+strcompress(string(index),/remove_all), /column, /return_events, floating=1)
    endfor

    ; auto setup button
    LightVars.Handles.SetupButtons(3)= widget_button(tmp1, value='Auto Setup', uvalue='Light_Event_Custom_IN6_Setup_Build, LightVars, /auto')
    tmp1 = widget_button(tmp1,value=' STOP ', uvalue='Light_Event_Custom_IN6_Stop, LightVars')
    ; Monok/Choppers/Elastic peak setup
    tmp1     = widget_base(LightVars.Handles.EnergySetup, /column, frame=1)

    Base_r  = widget_base(tmp1, /row) ; Choppers: button+status+auto flag
    LightVars.Handles.SetupButtons(0)= widget_button(Base_r, value='Choppers', xsize=100, uvalue='Light_Event_Custom_IN6_Setup_Choppers, LightVars')
    LightVars.Handles.SetupStatus(0) = widget_label(Base_r, value='Not set (x.xx)  ', uvalue='')
    LightVars.Handles.SetupFlags(0)  = cw_bgroup(Base_r, label_left='',/row,$
        ['Auto'], /nonexclusive, uvalue='Light_Custom_IN6_Setup_Check, LightVars', set_value=LightVars.Instrument.AutoFlags(0))
    LightVars.Handles.SetupButtons(5)  = cw_field(Base_r, title='', value = 0., xsize=10,  $
        uvalue  ='Light_Custom_IN6_Setup_Check, LightVars', /row, /all_events, floating=1)
    tmp = widget_label(Base_r, value='Focus Energy Transfert [meV]')

    Base_r  = widget_base(tmp1, /row) ; Monochromators: button+status+auto flag
    LightVars.Handles.SetupButtons(1)= widget_button(Base_r, value='Monochromators', xsize=100, uvalue='Light_Event_Custom_IN6_Setup_Monochromators, LightVars')
    LightVars.Handles.SetupStatus(1) = widget_label(Base_r, value='Not set (x.xx)  ', uvalue='')
    LightVars.Handles.SetupFlags(1)  = cw_bgroup(Base_r, label_left='',/row,$
        ['Auto'], /nonexclusive, uvalue='Light_Custom_IN6_Setup_Check, LightVars', set_value=LightVars.Instrument.AutoFlags(1))
    LightVars.Handles.SetupButtons(7)  = cw_field(Base_r, title='', value = 0, xsize=4,  $
        uvalue  ='Light_Custom_IN6_Setup_Check, LightVars', /row, /return_events, integer=1)
    tmp = widget_label(Base_r, value='Channel Nb.   ')
    LightVars.Handles.SetupButtons(6)  = cw_field(Base_r, title='', value = 0, xsize=2,  $
        uvalue  ='Light_Custom_IN6_Setup_Check, LightVars', /row, /all_events, integer=1)
    tmp = widget_label(Base_r, value='Ratio [1-6]')


    Base_r  = widget_base(tmp1, /row) ; Elastic: button+status+auto flag
    LightVars.Handles.SetupButtons(2)= widget_button(Base_r, value='Elastic Peak', xsize=100, uvalue='Light_SubDial_Custom_IN6_Check_Elastic_Peak, LightVars, /auto')
    LightVars.Handles.SetupStatus(2) = widget_label(Base_r, value='Not set (x.xx)  ', uvalue='')
    LightVars.Handles.SetupFlags(2)  = cw_bgroup(Base_r, label_left='',/row,$
        ['Auto'], /nonexclusive, uvalue='Light_Custom_IN6_Setup_Check, LightVars', set_value=LightVars.Instrument.AutoFlags(2))
    LightVars.Handles.SetupButtons(8)  = cw_field(Base_r, title='', value = 0, xsize=4,  $
        uvalue  ='Light_Custom_IN6_Setup_Check, LightVars', /row, /all_events, integer=4)
    tmp = widget_label(Base_r, value='Elastic Peak Ch.  ')

    widget_control, LightVars.Handles.EnergySetup, /realize
    XManager, 'Base_Options', LightVars.Handles.EnergySetup, Event_Handler='Light_Base_Event_Parser', $
        /just_reg, /no_block
    Light_Custom_IN6_Setup_Check, LightVars, Changed=0, /FromMad

    ; now put the instrument specific configuration to SU/normal Mode (call InitPro in SU check mode)
    ;** execute Custom InitPro SU check procedure
    Light_Custom_IN6_Sensitive, LightVars
    
    Light_Event_Custom_IN6_Setup_Build, LightVars  , /help, /update
  endif

  ; is there an auto proces to start...
  if n_elements(auto) ne 0 and LightVars.Private.AutoProcess eq 0 then begin
    ; update values before action
    if n_elements(Wavelength) eq 0 then Wavelength = LightVars.Private.EnergyToSet
    Light_Custom_IN6_Setup_Check, LightVars, Changed=0, wavelength=wavelength
    
    if ((LightVars.Private.IsSuperUser eq 0 and strpos(Lightvars.Instrument.EnergyOptions, 'superuser') lt 0) $
    or (LightVars.Private.IsSuperUser eq 1)) then begin
      ; SU/normal extended mode: we can launch auto settings -> chopper+monochromators positioning
      ; normally, only runs (at Wavelength change) and active 'auto' button may get there
      Light_LogMessage, LightVars, 'normal','[I] Starting Auto Setup process for '+strcompress(string(Wavelength))
      LightVars.Private.AutoProcess = 1
      Light_Event_Custom_IN6_Setup_Choppers, LightVars, /auto
    endif
  endif

end ; PRO Light_Event_Custom_IN6_Setup_Build

;***************************************************************
; choppers setting window.
pro Light_Event_Custom_IN6_Setup_Choppers, LightVars, Wavelength=Wavelength, update=update, auto=auto, help=help

  if n_elements(help) ne 0 then begin
    ;** Display information about Light/Light_Event_Custom_IN6_Setup_Choppers

    ToDisplay =['IN6 Choppers settings Help in "Light on George"',$
                '',$
                'This window dialog enables to compute and set the IN6 Choppers and', $
                'Time-Of-Flight settings. It displays the current configuration, with the', $
                'wavelength that is currently used on the instrument or a new value that was', $
                'changed by the user.', '', $
                'IN6 neutron beam that exits from the 3 Monochromators first encounters the', $
                'Fermi chopper (horizontal rotating collimator), and then passes trough the', $
                'Suppressor chopper (classical disk chopper), that removes higher order and', $
                'background signal.', $
                '', $
                'The Status line on top of the window indicates if the choppers are set for', $
                'the selected wavelength', '', $
                '[Details...] Displays full choppers/TOF settings', $
                '[Set]        Sets the choppers parameters, wait for stabilisation, and finally', $
                '             sets TOF parameters. This button is only active in SuperUser mode.', $
                '[STOP]       Stop all current settings and countings processes' $
                ]

    widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='IN6 Choppers Help in Light'
    if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 and n_elements(update) eq 0 then  begin
      xdisplayfile, 'choppers.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
            title='Choppers Help in Light', /editable, $
            Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
      LightVars.Handles.Base_Help = ID
    endif

    return
  endif

  if n_elements(Wavelength) eq 0 then begin
    Wavelength = LightVars.Private.EnergyToSet
  endif

  Title = '['+strupcase(LightVars.Instrument.Name)+'] Setup > Instrument > Choppers ('+strcompress(string(Wavelength, format='(F6.2)'), /remove_all)+')'
  ; display Chopper values for current wavelength+buttons Rocking curve/position
  widget_control, LightVars.Handles.ChoppersSetup, Bad_ID=Bad_ID, iconify=0 ;, tlb_set_title=Title
  ;** If does not exist, make it
  if Bad_ID ne 0 or LightVars.Handles.ChoppersSetup eq 0 then begin
    ; do not build if table update was requested, but choppers window is not there...
    if n_elements(update) ne 0 then if update ne 0 then return
    ; build ChoppersSetup window
    LightVars.Handles.ChoppersSetup  = widget_base(Group_Leader=LightVars.Handles.EnergySetup, $
                      title=Title, /column)
    tmp1 = widget_base(LightVars.Handles.ChoppersSetup, /row)
    LightVars.Handles.ChoppersButtons(1) = widget_label(tmp1, value='Choppers Setup: Starting interface                               ')
    tmp = widget_button(tmp1, value='Details...', uvalue='Light_Custom_IN6_Calc_TOF_Choppers, LightVars, /show')
    LightVars.Handles.ChoppersButtons(0) = widget_button(tmp1,value=' Set ', uvalue='Light_Event_Custom_IN6_Setup_Choppers, LightVars, /auto')
    tmp1= widget_button(tmp1, value='STOP', uvalue='Light_Event_Custom_IN6_Stop, LightVars')

    tmp1 = widget_base(LightVars.Handles.ChoppersSetup, /row)
    ThisData = [ LightVars.Private.SpeedFermiTarget, Lightvars.Instrument.ChopperSpeedRatio, $
      LightVars.Private.PhaseFermiTarget, LightVars.Private.PhaseSupprTarget, Lightvars.Instrument.PhaseOffset ]

    LightVars.Handles.ChoppersButtons(2) = widget_table(tmp1, /all_events, $
      column_labels= ['Choppers'], $
      row_labels   = ['Speed Fermi [rpm]', 'Ratio Supp./Fermi', 'Phase Fermi [deg]', 'Phase Sup. [deg]', 'Offset'], $
      value = transpose(ThisData), alignment=1, uvalue='') ; no callback because if edited, SU will force to its own values

    ThisData = [ LightVars.Private.Period, LightVars.Private.ChannelWidth, LightVars.Private.DeadTime, LightVars.Private.Delay ]
    LightVars.Handles.ChoppersButtons(3) = widget_table(tmp1, /all_events, $
      column_labels= ['Time-Of-Flight'],$
      row_labels   = ['Period [us]', 'Channel Width [us]', 'Dead Time [us]', 'Delay [us]'], $
      value = transpose(ThisData), alignment=1, uvalue='') ; no callback because if edited, SU will force to its own values

    widget_control, LightVars.Handles.ChoppersSetup, /realize
    XManager, 'ChoppersSetup', LightVars.Handles.ChoppersSetup, Event_Handler='Light_Base_Event_Parser', $
        /just_reg, /no_block
        
    Light_Event_Custom_IN6_Setup_Choppers, LightVars, /help, /update
  endif
  ; display Status (Not set, ready, auto) from Status line in Setup window
  widget_control, LightVars.Handles.SetupStatus(0), get_value=Status

  ; Status line: look for unit to use
  Status = Status+'. '+LightVars.Private.EnergyUnit+'='+strcompress(string(Wavelength, format='(F6.2)'), /remove_all)
  ; if EnergyOptions=superuser and not SU then add message in Status
  if strpos(Lightvars.Instrument.EnergyOptions, 'superuser') ge 0 and LightVars.Private.IsSuperUser eq 0 then $
    Status = Status+'. Switch to SU mode for action'
  widget_control, LightVars.Handles.ChoppersButtons(1), set_value=Status

  Light_Custom_IN6_Sensitive, LightVars

  ; now look if we start settings...
  a = ''
  widget_control, LightVars.Handles.SetupStatus(0), get_uvalue =a, bad_id=tmp
  if n_elements(auto) ne 0 then begin
    if strlen(a) eq 0 then widget_control, LightVars.Handles.SetupStatus(0), set_uvalue = 'start', set_value='start', bad_id=tmp $
    else Light_LogMessage, LightVars, 'normal','[w] IN6 Choppers setting is already running !'
  endif

end ; PRO Light_Event_Custom_IN6_Setup_Choppers

;***************************************************************
; monochromators setting window. 
pro Light_Event_Custom_IN6_Setup_Monochromators, LightVars, Wavelength=Wavelength, rocking=rocking, position=position, update=update, help=help
; **

  if n_elements(help) ne 0 then begin
    ;** Display information about Light/Light_Event_Custom_IN6_Setup_Monochromators

    ToDisplay =['IN6 Monochromator settings Help in "Light on George"',$
                '',$
                'This window dialog enables to calibrate and set the IN6 Monochromator.', $
                'It displays the current position and rocking curve scans configuration, or a', $
                'new value that was changed by the user.', '', $
                'IN6 neutron beam exiting from the cold neutron guide illuminates three', $
                'monochromators mounted in series. Each of them reflects a monochromatic part', $
                'of the beam, occording to the Bragg law.', $
                'The three PG002 curved multiple blades monochromtors focus to the sample', $
                'position, and these 3 beams (that have slightly different energies) are', $
                'chopped by the Fermi and Suppressor choppers, before coming to the sample.', $
                '', $
                'The Status line on top of the window indicates if the monochromators are set', $
                'for the selected wavelength', '', $
                '[Rocking Curve] Starts a rocking curve process for each monochromator,', $
                '                computes maximum reflected flux position and position', $
                '                the monochromators there. These data are stored for further', $
                '                direct positionings. A plot showing the 3 monochromator', $
                '                reflected beam rocking curve. This button is only active in', $
                '                SuperUser mode (Light:Setup menu:SuperUser login)', $
                '[Position]      Position the monochromators according to a previously', $
                '                calibrated maximum flux table obtained from a rocking curve.', $
                '[STOP]          Stop all current settings and countings processes' $
                ]

    widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='IN6 Monochromators Help in Light'
    if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 and n_elements(update) eq 0 then  begin
      xdisplayfile, 'monochromators.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
            title='Monochromators Help in Light', /editable, $
            Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
      LightVars.Handles.Base_Help = ID
    endif

    return
  endif

  ; determine if wavelength was changed (need positioning ?) and if choppers are already set (to get signal !)
  if n_elements(Wavelength) eq 0 then begin
    Wavelength = LightVars.Private.EnergyToSet
  endif
  ; get the table section corresponding to the wavelength to set
  idx=-1 &  tmp1=min(abs(LightVars.Instrument.EnergyDefinitions-Wavelength), idx)
  Wavelength = LightVars.Instrument.EnergyDefinitions(idx)

  Data = LightVars.Instrument.MonokData
  Data = reform(Data, 5, n_elements(LightVars.Instrument.MonokSequence), n_elements(LightVars.Instrument.EnergyDefinitions))
  ThisData = data(*,*,idx)

  Title = '['+strupcase(LightVars.Instrument.Name)+'] Setup > Instrument > Monochromators ('+strcompress(string(Wavelength, format='(F6.2)'), /remove_all)+')'
  ; display Monok table for current wavelength+buttons Rocking curve/position
  widget_control, LightVars.Handles.MonokSetup, Bad_ID=Bad_ID, iconify=0  ; , tlb_set_title=Title
  ;** If does not exist, make it
  if Bad_ID ne 0 or LightVars.Handles.MonokSetup eq 0 then begin
    ; do not build if table update was requested, but Monok window is not there...
    if n_elements(update) ne 0 then if update ne 0 then return
    ; build MonokSetup window
    LightVars.Handles.MonokSetup  = widget_base(Group_Leader=LightVars.Handles.EnergySetup, $
                      title=Title, /column)
    tmp1 = widget_base(LightVars.Handles.MonokSetup, /row)
    LightVars.Handles.MonokButtons(2) = widget_label(tmp1, value='Monochromators Setup: Starting interface                   ')
    LightVars.Handles.MonokButtons(0) = widget_button(tmp1,value='Rocking Curve', uvalue='Light_Event_Custom_IN6_Setup_Monochromators, LightVars, /rocking')
    LightVars.Handles.MonokButtons(1) = widget_button(tmp1,value='Position',      uvalue='Light_Event_Custom_IN6_Setup_Monochromators, LightVars, /position')
    tmp1= widget_button(tmp1, value='STOP', uvalue='Light_Event_Custom_IN6_Stop, LightVars')

    LightVars.Handles.MonokButtons(3) = widget_table(LightVars.Handles.MonokSetup, /editable, /all_events, $
      column_labels= ['Center','Range','Duration[s]','Step Width','Optimum Value'],$
      row_labels   = ['Mono1', 'Mono2','Mono3'], value = ThisData, alignment=1, uvalue='')

    widget_control, LightVars.Handles.MonokSetup, /realize
    XManager, 'MonokSetup', LightVars.Handles.MonokSetup, Event_Handler='Light_Base_Event_Parser', $
        /just_reg, /no_block
    Light_Event_Custom_IN6_Setup_Monochromators, LightVars, /help, /update
  endif
  widget_control, LightVars.Handles.MonokButtons(3), set_value=ThisData
  ; display Status (Not set, ready, auto) from Status line in Setup window
  widget_control, LightVars.Handles.SetupStatus(1),  get_value=Status

  ; Status line: look for unit to use
  Status = Status+'. '+LightVars.Private.EnergyUnit+'='+strcompress(string(Wavelength, format='(F6.2)'), /remove_all)
  ; if EnergyOptions=superuser and not SU then add message in Status
  if strpos(Lightvars.Instrument.EnergyOptions, 'superuser') ge 0 and LightVars.Private.IsSuperUser eq 0 then $
    Status = Status+'. Switch to SU mode for action'
  widget_control, LightVars.Handles.MonokButtons(2), set_value=Status

  Light_Custom_IN6_Sensitive, LightVars

  ; return if only table update was requested (energy change -> update table)
  if n_elements(update) ne 0 then return

  if Wavelength ne LightVars.Instrument.SetupEnergies(0) then begin
    ; chopper energy is not set yet
    Light_Base_Alert, Lightvars, warning='Choppers not set'
    Light_LogMessage, LightVars, 'normal','[I] Please set choppers for '+strcompress(string(Wavelength), /remove_all)+' before monochromators'
    ; activate Light Warning
    ; add warning into Light Log
    return
  endif

  ; return when no action is requested
  if n_elements(rocking) eq 0 and n_elements(position) eq 0 then return
  
  ; now look if we start settings...
  a = ''
  widget_control, LightVars.Handles.SetupStatus(1), get_uvalue =a, bad_id=tmp

  if strlen(a) eq 0 then begin
    MonoUValue = ''
    if n_elements(rocking) ne 0  then MonoUValue = 'start rocking'
    if n_elements(position) ne 0 then MonoUValue = 'start positioning'
    ; if non empty, then initialise Data table at required wavelength
    Tools_StructSetValue, struct=LightVars, Tag='Private.MonokLambdaData',val=ThisData
    widget_control, LightVars.Handles.SetupStatus(1), set_uvalue = MonoUValue, set_value=MonoUValue, bad_id=tmp
    ; set StatusValue to 0.00 (so that Monok remains unset in case of error during positioning)
    LightVars.Instrument.SetupEnergies(1) = 0
  endif else Light_LogMessage, LightVars, 'normal','[w] IN6 Monochromators setting is already running !'


end ; PRO Light_Event_Custom_IN6_Setup_Monochromators

;***************************************************************
; choppers setting window: on going operation handling
pro Light_SubDial_Custom_IN6_Check_Choppers, LightVars

  Wavelength = LightVars.Private.EnergyToSet
  
  ChopUValue = ''
  ; we use the same philosophy as in usual dials, inside Light. We monitor the uvalue of the Chopper status
  widget_control, LightVars.Handles.SetupStatus(0), get_uvalue = ChopUValue, bad_id=tmp    ; choppers

  if strlen(ChopUValue) eq 0 then return

  ; test if MAD is Idle before doing anything
  if strpos(ChopUValue, 'start') ge 0 and strpos(strupcase(LightVars.Mad.Status), 'IDLE') lt 0 then begin
    Light_LogMessage, LightVars, 'normal','[w] Instrument control (MAD) is not ready (doing '+LightVars.Mad.Status+'). Ignoring Choppers operation. If MAD is blocked in non-Idle state, launch a short counting.'
    widget_control, LightVars.Handles.SetupStatus(0), set_uvalue = '', bad_id=tmp
    return
  endif
  if strpos(strupcase(LightVars.Mad.Status), 'IDLE') lt 0 then begin
    ; MAD Status should be either Idle, or a CHOPPER SETTING, or a WAVELENGTH SETTING
    Light_Custom_IN6_Setup_Check, LightVars, /FromMad, /PhaseSpeed
    if strpos(strupcase(LightVars.Mad.Status), 'CHOPPER SETTING') lt 0 and strpos(strupcase(LightVars.Mad.Status), 'WAVELENGTH SETTING') lt 0 then begin
      ; conflict with an other operation: stop process.
      Light_Base_Alert, LightVars, warning='Choppers conflict:'+LightVars.Mad.Status
      Light_LogMessage, LightVars, 'normal','[E] The instrument is doing '+LightVars.Mad.Status+' which prevents Choppers setting. Aborting. If MAD is blocked in non-Idle state, launch a short counting.'
      widget_control, LightVars.Handles.SetupStatus(0), set_uvalue = '', bad_id=tmp    ; choppers
      return
    endif else begin
      ; blink table
      if LightVars.Private.ErrorMsgToggle eq 0 then widget_control, LightVars.Handles.ChoppersButtons(2), SET_TABLE_SELECT=[-1, -1, -1, -1], bad_id=tmp1 $
      else widget_control, LightVars.Handles.ChoppersButtons(2), SET_TABLE_SELECT=[0, 0, 0, 5], bad_id=tmp1
    endelse
  endif else begin
    ; MAD is Idle
    widget_control, LightVars.Handles.SetupStatus(0),     set_value=ChopUValue, bad_id=tmp
    widget_control, LightVars.Handles.ChoppersButtons(1), set_value=ChopUValue, bad_id=tmp
    case ChopUValue of
    'start':  $
      begin
        widget_control, LightVars.Handles.SetupStatus(0), set_value= 'par chop', bad_id=tmp
        Light_LogMessage, LightVars, 'normal','[I] Starting IN6 Choppers setting for '+strcompress(string(Wavelength, format='(F6.2)'))
        Light_Control, LightVars, string('off', format='(%"'+LightVars.Instrument.SetComputeCommand+'")')
        Light_Control, LightVars, string(Wavelength, format='(%"'+LightVars.Instrument.EnergyCommand+'")')
        Light_Control, LightVars, string(LightVars.Private.SpeedFermiTarget, LightVars.Instrument.ChopperSpeedRatio, $
            LightVars.Private.PhaseFermiTarget, LightVars.Private.PhaseSupprTarget, $
            format='(%"'+LightVars.Instrument.ChoppersSetCommand+'")')
            ; LightVars.Instrument.PhaseOffset, 
        widget_control, LightVars.Handles.ChoppersButtons(2), SET_TABLE_SELECT=[0, 0, 0, 5], bad_id=tmp1
        widget_control, LightVars.Handles.ChoppersButtons(3), SET_TABLE_SELECT=[-1, -1, -1, -1], bad_id=tmp1
        Light_Control, LightVars, LightVars.Instrument.ChoppersSynchro
        ChopUValue = 'chop synchro wait'
      end
    'chop synchro wait': $
      begin
        Light_LogMessage, LightVars, 'normal','[I] Achieved Choppers synchronisation'
        ChopUValue = 'par tof'
      end
    'par tof': $
      begin
        Light_Control, LightVars, string(LightVars.Instrument.N_channels, LightVars.Private.ChannelWidth, $
            LightVars.Private.Delay, format='(%"'+LightVars.Instrument.TOFSetCommand+'")')
        Light_Control, LightVars, string(LightVars.Private.Elastic_Peak_Chan, format='(%"'+LightVars.Instrument.ElPeakCommand+'")')
        Light_Control, LightVars, string(LightVars.Private.Period, format='(%"'+LightVars.Instrument.PeriodCommand+'")')
        widget_control, LightVars.Handles.ChoppersButtons(3), SET_TABLE_SELECT=[0, 0, 0, 4], bad_id=tmp1
        widget_control, LightVars.Handles.ChoppersButtons(2), SET_TABLE_SELECT=[-1, -1, -1, -1], bad_id=tmp1
        ChopUValue = 'end'
      end
    'end': $
      begin
        Light_LogMessage, LightVars, 'normal','[I] Achieved TOF/Choppers settings'
        Light_Custom_IN6_Setup_Check, LightVars, changed=0, /FromMad
        widget_control, LightVars.Handles.ChoppersButtons(3), SET_TABLE_SELECT=[-1, -1, -1, -1], bad_id=tmp1
        widget_control, LightVars.Handles.ChoppersButtons(2), SET_TABLE_SELECT=[-1, -1, -1, -1], bad_id=tmp1
        ; now update SetupEnergies
        LightVars.Instrument.SetupEnergies(0) = Wavelength
        ChopUValue = ''
        ; if auto process is on then start monochromators
        if LightVars.Private.AutoProcess gt 0 then begin
          ; close choppers window
          widget_control, LightVars.Handles.ChoppersSetup, Bad_ID=Bad_ID, /destroy
          ; launch monok settings
          Light_Event_Custom_IN6_Setup_Monochromators, LightVars, /position
        endif
      end
    else:
    endcase

    widget_control, LightVars.Handles.SetupStatus(0), set_uvalue=ChopUValue, bad_id=tmp
    if strlen(ChopUValue) ne 0 then begin
      Light_Custom_IN6_Setup_Check, LightVars, /FromMad, /PhaseSpeed
    endif else Light_Custom_IN6_Setup_Check, LightVars, changed=0, /FromMad
  endelse ; if strpos(LightVars.Mad.Status, 'Idle') lt 0

end ; PRO Light_SubDial_Custom_IN6_Check_Choppers

;***************************************************************
; handle monochromators setting process: on going operation handling
pro Light_SubDial_Custom_IN6_Check_Monochromators, LightVars, Wavelength=Wavelength
; **

  if n_elements(Wavelength) eq 0 then begin
    Wavelength = LightVars.Private.EnergyToSet
  endif
  
  MonokUValue = ''
  ; we use the same philosophy as in usual dials, inside Light. We monitor the uvalue of the MonokUValue status
  widget_control, LightVars.Handles.SetupStatus(1), get_uvalue = MonokUValue, bad_id=tmp    ; MonokUValue

  if strlen(MonokUValue) eq 0 then return

  ; test if MAD is Idle before doing anything
  if strpos(MonokUValue, 'start') ge 0 and strpos(strupcase(LightVars.Mad.Status), 'IDLE') lt 0 then begin
    Light_LogMessage, LightVars, 'normal','[w] Instrument control (MAD) is not ready (doing '+LightVars.Mad.Status+'). Ignoring Monochromators operation. If MAD is blocked in non-Idle state, launch a short counting.'
    widget_control, LightVars.Handles.SetupStatus(1), set_uvalue = '', bad_id=tmp
    return
  endif
  Data = LightVars.Private.MonokLambdaData
  idx  = LightVars.Instrument.MonokSequence(LightVars.Private.MonokCurrentIndex) ; monok ID (0,1,2)
  if strpos(strupcase(LightVars.Mad.Status), 'IDLE') lt 0 then begin
    ; MAD Status should be either Idle, POSITIONNING, or COUNTING
    if strpos(strupcase(LightVars.Mad.Status), 'POSITIONNING') lt 0 and strpos(strupcase(LightVars.Mad.Status), 'COUNTING') lt 0 then begin
      ; conflict with an other operation: stop process.
      Light_Base_Alert, LightVars, warning='Monochromators conflict:'+LightVars.Mad.Status
      Light_LogMessage, LightVars, 'normal','[E] The instrument is doing '+LightVars.Mad.Status+' which prevents Monochromators setting. Aborting. If MAD is blocked in non-Idle state, launch a short counting.'
      widget_control, LightVars.Handles.SetupStatus(1), set_uvalue = '', bad_id=tmp    ; Monochromators
      return
    endif else begin
      ; display current Lightvars.Mad.t_nother.actang(x) value in last column of Table
      ActData = Data
      ActData(4,idx) = Lightvars.Mad.t_nother.actang(idx)
      ; update table values in widget
      widget_control, LightVars.Handles.MonokButtons(3), set_value=ActData
    endelse
  endif else begin
    ; MAD is Idle
    n_step = long(Data(1, idx)/Data(3, idx))  ; range/step_width
    ProcessStep = strsplit(MonokUValue, ':', /extract)
    widget_control, LightVars.Handles.SetupStatus(1),  set_value=ProcessStep(0), bad_id=tmp
    widget_control, LightVars.Handles.MonokButtons(2), set_value=MonokUValue, bad_id=tmp
    case ProcessStep(0) of
    'start rocking':  $
      begin
        Light_LogMessage, LightVars, 'normal','[I] Starting IN6 Monochromators Rocking Curve for '+strcompress(string(Wavelength, format='(F6.2)'))
        MonokUValue = 'RAZ (rocking)'
      end
    'start positioning':  $
      begin
        Light_LogMessage, LightVars, 'normal','[I] Starting IN6 Monochromators Positioning for '+strcompress(string(Wavelength, format='(F6.2)'))
        MonokUValue = 'RAZ (positioning)'
      end
    'RAZ (rocking)': $
      begin
        Light_LogMessage, LightVars, 'normal','[ ] Sending all monochromators to lower switch limit (reference point, before rocking curve)'
        if LightVars.Global.Simulation ne 0 then R=TstControl(LightVars.Instrument.MonokRAZCommand) $
        else R=Light_Control(LightVars.Instrument.MonokRAZCommand)
        widget_control,LightVars.Handles.MonokButtons(3),SET_TABLE_SELECT=[0, 0, 4, n_elements(LightVars.Instrument.MonokSequence)], bad_id=tmp1
        LightVars.Private.MonokCurrentIndex = 0L
        LightVars.Private.MonokScanPoint    = 0L
        LightVars.Private.MonokDisplayMsg(0)= ''
        LightVars.Private.MonokDisplayMsg(1)= ''
        LightVars.Private.MonokDisplayMsg(2)= ''
        idx  = LightVars.Instrument.MonokSequence(LightVars.Private.MonokCurrentIndex)
        MonokUValue = 'rotate monochromators: mono'+strcompress(string(idx+1))+', scan step '+strcompress(string(LightVars.Private.MonokScanPoint))+'/'+strcompress(string(n_step))
      end
    'rotate monochromators': $
      begin
        ; send positioning command for monochromator in sequence
        ; position = (center-range/2) + MonokScanPoint*step_width
        position = (Data(0, idx) - Data(1, idx)/2.0) + LightVars.Private.MonokScanPoint*Data(3, idx)
        LightVars.Private.MonokPosition = position
        value=string(idx+1, position, format='(%"'+LightVars.Instrument.MonokSetCommand+'")')
        if LightVars.Global.Simulation ne 0 then R=TstControl(value) $
        else R=Light_Control(value)
        MonokUValue = 'count: after '+value+' (mono'+strcompress(string(idx+1))+', scan step '+strcompress(string(LightVars.Private.MonokScanPoint))+'/'+strcompress(string(n_step))+')'
        ActData = Data
        ActData(4,idx) = position
        ; update table values in widget
        widget_control, LightVars.Handles.MonokButtons(3), SET_TABLE_SELECT=[0, idx, 4, idx], bad_id=tmp1, set_value=ActData
      end        
    'count': $
      begin
        ; send count command
        value=string(Data(2, idx), format='(%"'+LightVars.Instrument.MonokCntCommand+'")')
        if LightVars.Global.Simulation ne 0 then R=TstControl(value) $
        else R=Light_Control(value)
        MonokUValue = 'get count: after '+value+' (mono'+strcompress(string(idx+1))+', scan step '+strcompress(string(LightVars.Private.MonokScanPoint))+'/'+strcompress(string(n_step))+')'
      end
    'get count': $
      begin
        ; get count value
        if LightVars.Global.Simulation ne 0 then R=TstNewValue(type='monitor') $
        else begin
          ; use counts retrieved from last Data Collect
          ; get detectors, integrate over detectors (length=tof channels-1, last=integral)
          Mad       = LightVars.Mad
          t_res     = Mad.t_res
          data      = Mad.Data
          ; now specialised aliases for IN6 ************************************************
          dims      = size(data, /dim)
          data     = data(0:(dims(0) -2), *)  ; removes last channel (= integral)
          Monitor  = data(*,0)                ; this is Monitor1
          tof      = total(data(*,t_res.N_MON_SPECT:(t_res.N_TOTAL_SPECT-1)),2) 
          if LightVars.Instrument.MonokFluxMon_TOF ne 0 then R=total(tof) $   ; if we calibrate monochromators on Detectors flux optimum 
          else R=total(Monitor)  ; if we calibrate monochromators on Monitor flux optimum
          ; 
        endelse
        ; store [LightVars.Private.MonokPosition, count] for further fitting
        x = [LightVars.Private.MonokX, LightVars.Private.MonokPosition]
        y = [LightVars.Private.MonokY, R]
        if LightVars.Private.MonokScanPoint eq 0 then begin
          Light_LogMessage, LightVars, 'normal','[ ] Now scanning Monochromator '+strcompress(string(idx+1))
          if LightVars.Private.MonokCurrentIndex eq 0 then begin
            x = LightVars.Private.MonokPosition
            y = R
            ; will clear any Rocking_Curve previous display
            display, Display_Name='Rocking_Curve', /delete
          endif
        endif
        
        Tools_StructSetValue, struct=LightVars,  Tag='Private.MonokX',val=x
        Tools_StructSetValue, struct=LightVars,  Tag='Private.MonokY',val=y
        ; display current [position, count] on Dial_Display
        display, IX=x, Data=y, Name_Data='Mono', Display_Name='Rocking_Curve', $
          Name_IX='Position', Name_IY='Counts', /scatter, /detached, /update
        ; if LightVars.Private.MonokScanPoint le n_step=long(range/step_width) then go to next scan step
        if LightVars.Private.MonokScanPoint ge 0 and LightVars.Private.MonokScanPoint lt n_step then begin
          LightVars.Private.MonokScanPoint = LightVars.Private.MonokScanPoint+1
          MonokUValue = 'rotate monochromators: mono'+strcompress(string(idx+1))+', scan step '+strcompress(string(LightVars.Private.MonokScanPoint))+'/'+strcompress(string(n_step))
        endif else begin
          ; else go to 'fit maximum'
          MonokUValue = 'fit maximum: mono'+strcompress(string(idx+1))
        endelse
      end
    'fit maximum': $
      begin
        ; get last n_step points in [MonoX, MonokY]
        n_points = n_elements(LightVars.Private.MonokX)-1
        x = LightVars.Private.MonokX((n_points-n_step):n_points)
        y = LightVars.Private.MonokY((n_points-n_step):n_points)
        ;fit current curve, store into MonokData
        ;ESTIMATE [height,center,width,constant]
        mii=min(y)
        maa=max(y) & paa=total(x*y)/total(y)
        waa=sqrt(total(x*x*y)/total(y) - paa*paa)
        S1 =[maa-mii,paa,waa,mii]
        ;FIT. Fitted maximum is A1(1)
        fit=gaussfit(x,y,A1,nterms=4,estimates=S1)
        ; estimate fitted curve
        yfit = A1(0)*exp(-(((x-A1(1))/A1(2))^2)/2)+A1(3)  ;+A1(4)*x+A1(5)*x^2
        ; store fit result
        Private = LightVars.Private
        if LightVars.Private.MonokCurrentIndex eq 0 then $
          Tools_StructSetValue, struct=LightVars,    Tag='Private.MonokYfit',val=Yfit $
        else $
          Tools_StructSetValue, struct=LightVars,    Tag='Private.MonokYfit',val=[LightVars.Private.MonokYfit, Yfit ]
        Data(4, idx) = A1(1)
        Tools_StructSetValue, struct=LightVars,   Tag='Private.MonokLambdaData',val=Data
        ; update table values in widget
        widget_control, LightVars.Handles.MonokButtons(3), set_value=Data, bad_id=tmp
        ; display current fitted curve on Dial_Display (/update, /overlay)+ comment lines
        LightVars.Private.MonokDisplayMsg(idx) = 'M'+strtrim(string(idx+1),2)+'='+strtrim(string(round(A1(1))),2) $
                                                +' ('+strtrim(string(round(A1(2))),2)+')'
        display,IX=LightVars.Private.MonokX, Data=LightVars.Private.MonokY,    Display_Name='Rocking_Curve', Name_Data='Mono',     Comment=LightVars.Private.MonokDisplayMsg, /no, /catenate
        display,IX=LightVars.Private.MonokX, Data=LightVars.Private.MonokYfit, Display_Name='Rocking_Curve', Name_Data='Mono_Fit', /update, /overlay
        Light_LogMessage, LightVars, 'normal','[ ] Monochromator '+strtrim(string(idx+1),2)+' optimum position='+strtrim(string(round(A1(1))),2)+' width='+strtrim(string(round(A1(2))),2)
        
        ; go to next monochromator in sequence
        LightVars.Private.MonokScanPoint = 0L
        if LightVars.Private.MonokCurrentIndex lt n_elements(LightVars.Instrument.MonokSequence)-1 then begin
          LightVars.Private.MonokCurrentIndex = LightVars.Private.MonokCurrentIndex+1
          idx = LightVars.Instrument.MonokSequence(LightVars.Private.MonokCurrentIndex)
          MonokUValue = 'rotate monochromators: mono'+strcompress(string(idx+1))+', scan step '+strcompress(string(LightVars.Private.MonokScanPoint))+'/'+strcompress(string(n_step))
        endif else begin
          ; if last monochromator in sequence, go to 'RAZ (before positioning)'
          MonokUValue = 'RAZ (positioning)'
        endelse
      end
    'RAZ (positioning)': $
      begin
        Light_LogMessage, LightVars, 'normal','[ ] Sending all monochromators to lower switch limit (reference point, before final positioning)'
        if LightVars.Global.Simulation ne 0 then R=TstControl(LightVars.Instrument.MonokRAZCommand) $
        else R=Light_Control(LightVars.Instrument.MonokRAZCommand)
        widget_control,LightVars.Handles.MonokButtons(3),SET_TABLE_SELECT=[0, 0, 4, n_elements(LightVars.Instrument.MonokSequence)], bad_id=tmp1
        MonokUValue = 'positioning'
        LightVars.Private.MonokCurrentIndex = 0L
      end
    'positioning': $
      begin
        value = Data(4, idx)
        NormRatio = abs((value/Data(0, idx)) -1)
        if NormRatio ge 0.1 then begin
          Light_Base_Alert, Lightvars, error='Mono'+strcompress(string(idx+1), /remove_all)+' not set'
          Light_LogMessage, LightVars, 'silent','[E] Mono'+strcompress(string(idx+1), /remove_all)+' fitted position '+strcompress(string(value), /remove_all)+' more than 10% out of '+strcompress(string(Data(0, idx)), /remove_all)+'. Ignoring (not positioning).'
        endif else begin
          Light_LogMessage, LightVars, 'normal','[ ] Positioning Monochromator '+strtrim(string(idx+1),2)+' to optimum position='+strtrim(string(Data(4, idx)),2)
          value=string(idx+1, value, format='(%"'+LightVars.Instrument.MonokSetCommand+'")')
          if LightVars.Global.Simulation ne 0 then R=TstControl(value) $
          else R=Light_Control(value)
          widget_control, LightVars.Handles.MonokButtons(3), SET_TABLE_SELECT=[0, idx, 4, idx], bad_id=tmp1
        endelse
        
        if LightVars.Private.MonokCurrentIndex ge n_elements(LightVars.Instrument.MonokSequence)-1 then $
          MonokUValue = 'end: after '+value $
        else begin
          MonokUValue = 'positioning: after '+value
          LightVars.Private.MonokCurrentIndex = LightVars.Private.MonokCurrentIndex+1
        endelse
      end
    'end': $
      begin
        Light_LogMessage, LightVars, 'normal','[I] Achieved Monochromators settings'
        ; transfert Data to main MonokData table
        MonokData = LightVars.Instrument.MonokData
        MonokData(*,*,LightVars.Private.EnergyIndex) = Data
        Tools_StructSetValue, struct=LightVars,    Tag='Instrument.MonokData',val=MonokData
        
        widget_control, LightVars.Handles.MonokButtons(3), SET_TABLE_SELECT=[-1, -1, -1, -1], bad_id=tmp1
        ; now update SetupEnergies
        LightVars.Instrument.SetupEnergies(1) = Wavelength
        MonokUValue = ''
        ; if auto process is on then start CheckElastic
        if LightVars.Private.AutoProcess gt 0 then begin
          ; close monok windows
          widget_control, LightVars.Handles.MonokSetup, Bad_ID=Bad_ID, /destroy
          display, Display_Name='Rocking_Curve', /delete
          ; launch check of elastic peak
          Light_SubDial_Custom_IN6_Check_Elastic_Peak, LightVars, /auto
        endif else Light_Custom_IN6_Setup_Check, LightVars
      end
    else:
    endcase
    
    widget_control, LightVars.Handles.SetupStatus(1),   set_uvalue=MonokUValue, bad_id=tmp
    widget_control, LightVars.Handles.MonokButtons(2),  set_value =MonokUValue, bad_id=tmp
  endelse ; if strpos(LightVars.Mad.Status, 'Idle') lt 0

end ; PRO Light_SubDial_Custom_IN6_Check_Monochromators

;***************************************************************
; check of the elastic peak channel on detectors and monitor1
pro Light_SubDial_Custom_IN6_Check_Elastic_Peak, LightVars, auto=auto, help=help

  if n_elements(help) ne 0 then begin
    ;** Display information about Light/Light_SubDial_Custom_IN6_Check_Elastic_Peak

    ToDisplay =['IN6 Check of the Elastic Peak Channel Help in "Light on George"',$
                '',$
                'This item counts the signal for a short time, and determines the position', $
                'of the elastic line on the TOF axis from Monitor1 and the integral of all', $
                'Detectors. A graphic is displayed, as well as basic statistics.' $
                ]

    widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Elastic Peak Check Help in Light'
    if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 and n_elements(update) eq 0 then  begin
      xdisplayfile, 'checkelastic.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
            title='Elastic Peak Check Help in Light', /editable, $
            Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
      LightVars.Handles.Base_Help = ID
    endif
    return
  endif

  ChElPkUValue = ''
  ; we use the same philosophy as in usual dials, inside Light. We monitor the uvalue of the Check El Peak status
  widget_control, LightVars.Handles.SetupStatus(2), get_uvalue = ChElPkUValue, bad_id=tmp
  
  if n_elements(auto) ne 0 then begin
    if strlen(ChElPkUValue) gt 0 then begin
      Light_LogMessage, LightVars, 'normal','[w] IN6 Check of the Elastic Peak is already running !'
      return
    endif else begin
      if LightVars.Private.EnergyToSet ne LightVars.Instrument.SetupEnergies(1) then begin
        ; chopper energy is not set yet
        Light_Base_Alert, Lightvars, warning='Monochromators not set'
        Light_LogMessage, LightVars, 'normal','[I] Please set monochromators for '+strcompress(string(LightVars.Private.EnergyToSet), /remove_all)+' before checking elastic peak channel'
        ; activate Light Warning
        ; add warning into Light Log
        return
      endif else ChElPkUValue = 'start'
    endelse 
  endif

  if strlen(ChElPkUValue) eq 0 then return

  ; test if MAD is Idle before doing anything
  if strpos(ChElPkUValue, 'start') ge 0 and strpos(strupcase(LightVars.Mad.Status), 'IDLE') lt 0 then begin
    Light_LogMessage, LightVars, 'normal','[w] Instrument control (MAD) is not ready (doing '+LightVars.Mad.Status+'). Ignoring Check Elastic Peak operation. If MAD is blocked in non-Idle state, launch a short count.'
    widget_control, LightVars.Handles.SetupStatus(2), set_uvalue = '', bad_id=tmp
    return
  endif
  if strpos(strupcase(LightVars.Mad.Status), 'IDLE') lt 0 then begin
    ; MAD Status should be either Idle, or a COUNTING
    if strpos(strupcase(LightVars.Mad.Status), 'COUNTING') lt 0  then begin
      ; conflict with an other operation: stop process.
      Light_Base_Alert, LightVars, warning='Check Elastic Peak conflict:'+LightVars.Mad.Status
      Light_LogMessage, LightVars, 'normal','[E] The instrument is doing '+LightVars.Mad.Status+' which prevents the Check of the Elastic Peak channel. Aborting. If MAD is blocked in non-Idle state, launch a short count.'
      widget_control, LightVars.Handles.SetupStatus(2), set_uvalue = '', bad_id=tmp
      return
    endif ; else wait (not Idle but counting)...

  endif else begin
    ; MAD is Idle
    widget_control, LightVars.Handles.SetupStatus(2), set_value=ChElPkUValue, bad_id=tmp
    case ChElPkUValue of
    'start':  $
      begin
        Light_LogMessage, LightVars, 'normal','[I] Starting the Check of the Elastic Peak channel for '+strcompress(string(LightVars.Private.EnergyToSet, format='(F6.2)'))
        Light_Control, LightVars, string(1, format='(%"'+LightVars.Instrument.MonokCntCommand+'")')
        ChElPkUValue = 'measure'
      end
    'measure': $
      begin
        Light_LogMessage, LightVars, 'normal','[I] Counting achieved. Analysing.'
        ChElPkUValue = 'end'
        ;get detectors, integrate over detectors (length=tof channels-1, last=integral)
        Mad       = LightVars.Mad
        t_res     = Mad.t_res
        data      = Mad.Data
        
        ; now specialised aliases for IN6 ************************************************
        dims      = size(data, /dim)
        data     = data(0:(dims(0) -2), *)
        Monitor  = data(*,0)      ; this is Monitor1
        data     = data(*,t_res.N_MON_SPECT:(t_res.N_TOTAL_SPECT-1)) ; for IN6
        detectors= total(data,1)  ; Channels on X (length=340), sum of row
        tof      = total(data,2)  ; TOF on X (length=LightVars.Mad.t_para.tof_cha_resol = 512, 1024...)
        IX=lindgen(n_elements(Monitor))
        
        Monitor = Monitor/60  ; 1 minute counting -> per sec.
        tof     = tof/60
        
        ;get 1st, 2nd moments for Monitor(1)
        fmon = total(IX*Monitor)/total(Monitor)
        smon = total(IX*IX*Monitor)/total(Monitor) - fmon*fmon
        if smon gt 0 then smon = round(sqrt(smon))
        imon  = max(Monitor, mmon)
        Light_LogMessage, LightVars, 'verbose','[ ] Monitor 1st moment='+strcompress(string(total(fmon))) $
          +' width='+strcompress(string(smon))+' channels'
        Light_LogMessage, LightVars, 'normal','[ ] Monitor Channel='+strcompress(string(mmon))+' ('+strcompress(string(imon))+' counts/s) Sum='+strcompress(string(total(Monitor)))+' counts/s. 
        ;get 1st, 2nd moments for tof
        ftof = total(IX*tof)/total(tof)
        stof = total(IX*IX*tof)/total(tof) - ftof*ftof
        if stof gt 0 then stof = round(sqrt(stof))
        itof  = max(tof, mtof)
        ; now look if itof is around ftof 
        Light_LogMessage, LightVars, 'verbose','[ ] TOF 1st moment=' +strcompress(string(total(ftof))) $
          +' width='+strcompress(string(stof))+' channels'
          Light_LogMessage, LightVars, 'normal','[>] TOF Channel='+strcompress(string(mtof))+' ('+strcompress(string(itof))+' counts/s) Sum='+strcompress(string(total(tof)))+' counts/s [elastic channel].
        ; create a detached display
        comment=[ 'M1 ='+strcompress(string(mmon))+' ('+strcompress(string(smon))+')', $
                  'TOF='+strcompress(string(mtof))+' ('+strcompress(string(stof))+')' ]
        display,IX=IX, Data=Monitor, Name_Data='M1' , display_name='Check_Elastic', Name_IX='channel', Name_IY='Counts/s', Comment=comment, /detached
        display,IX=IX, Data=tof,     Name_Data='TOF', display_name='Check_Elastic', /update, /overlay
      end
    'end': $
      begin
        Light_LogMessage, LightVars, 'normal','[I] Achieved Check of the Elastic Peak'
        ; now update SetupEnergies
        LightVars.Instrument.SetupEnergies(2) = LightVars.Private.EnergyToSet
        ChElPkUValue = ''
        ; if auto process is on then enable runs to go on
        if LightVars.Private.Autoprocess gt 0 then begin
          Light_LogMessage, LightVars, 'normal','[I] Achieved Auto Setup process for '+strcompress(string(LightVars.Private.EnergyToSet))
          IsRunOn = ''
          if LightVars.Handles.RunStatus ne 0 then $
            widget_control, LightVars.Handles.RunStatus, get_uvalue=RunIsOn, bad_id=tmp
          if IsRunOn eq 'wave' then widget_control, LightVars.Handles.RunStatus, set_uvalue='wait', bad_id=tmp
          LightVars.Private.Autoprocess = 0
          widget_control, LightVars.Handles.EnergySetup, /destroy, bad_id=tmp
        endif
      end
    else:
    endcase
    widget_control, LightVars.Handles.SetupStatus(2), set_uvalue=ChElPkUValue, bad_id=tmp
    if strlen(ChElPkUValue) ne 0 then begin
      Light_Custom_IN6_Setup_Check, LightVars, /FromMad
    endif else Light_Custom_IN6_Setup_Check, LightVars, changed=0, /FromMad
  endelse ; if strpos(LightVars.Mad.Status, 'Idle') lt 0

end ; PRO Light_SubDial_Custom_IN6_Check_Elastic_Peak

;***************************************************************
;** Handles on going run/sequence
pro Light_SubDial_Custom_IN6_Check_Run, LightVars

  RunUValue = ''
  ; we use the same philosophy as in usual dials, inside Light. We monitor the uvalue of the Run status
  widget_control, LightVars.Handles.RunStatus, get_uvalue = RunUValue, bad_id=tmp

  if strlen(RunUValue) eq 0 then return
  
  Run_Nb = LightVars.Private.OnGoing_Run_Nb

  ; test if MAD is Idle before doing anything
  if strpos(RunUValue, 'start') ge 0 and strpos(strupcase(LightVars.Mad.Status), 'IDLE') lt 0 then begin
    Light_LogMessage, LightVars, 'normal','[w] Instrument control (MAD) is not ready (doing '+LightVars.Mad.Status+'). Ignoring Run/Sequence execution. If MAD is blocked in non-Idle state, launch a short count.'
    widget_control, LightVars.Handles.RunStatus, set_uvalue = '', bad_id=tmp, set_value=''
    return
  endif
  if strpos(strupcase(LightVars.Mad.Status), 'IDLE') lt 0 then begin
    ; MAD Status should be either Idle, or a COUNTING
    if strpos(strupcase(LightVars.Mad.Status), 'COUNTING') ge 0 then begin
      if LightVars.Private.TemperatureUnstable ge 1 and LightVars.Run.Flag_T(Run_Nb) ne 0 then begin
        LightVars.Private.TemperatureUnstable = 0
        msg = '[ ] Starting temporary counting (waiting to recover T control).'
        Light_LogMessage, LightVars, 'normal', msg
        ; stop current counting, then restart without incrementing
        Light_Control, LightVars, LightVars.Instrument.StopCommand
        RunUValue = 'T_Out'
        widget_control, LightVars.Handles.RunStatus,  set_uvalue=RunUValue, bad_id=tmp
        ; starting time for this temporary period is LightVars.Private.TempoTempTime
        ; (this is done in Light_SubDial_Base_Check_Temperature)
        ; a new TemperatureUnstable will be generated every Wait_Error (changing temporary counting numor)
      endif
      if LightVars.Private.TemperatureUnstable le -1 and LightVars.Run.Flag_T(Run_Nb) ne 0 then begin
        LightVars.Private.TemperatureUnstable = 0
        msg = '[ ] Stopping temporary counting (recovered T control).'
        Light_LogMessage, LightVars, 'normal', msg
        ; stop current counting without incrementing
        Light_Control, LightVars, LightVars.Instrument.StopCommand
        RunUValue = 'count'
        widget_control, LightVars.Handles.RunStatus,  set_uvalue=RunUValue, bad_id=tmp
        ; handle the remaining time and number of repetition for current counting (that continues)
        ; change current sequence number of repetition to t_res.NREPET-t_res.NPREP
        nrep = max([LightVars.Private.LastValidNumor(1) - LightVars.Private.LastValidNumor(2), 1])
        LightVars.Run.Repetition(Run_Nb)  = nrep
        ; recompute the next sequence times, so that total time is kept (as far as possible).
        ; each counting should have a minimum duration of Wait_Error
        LostTime = double(systime(1) - abs(LightVars.Private.TempoTempTime))/60.0 ; time lost in [min]
        LightVars.Private.TempoTempTime = 0
        TotalTime = 0
        ; first compute the Total+Waiting time for the remaining sequence
        for index=Run_Nb, n_elements(LightVars.Run.Temperatures)-1 do begin
          DuRep = LightVars.Run.Preset(index)*LightVars.Run.Repetition(index)
          if LightVars.Run.Flag_MonTi(index) then TotalTime = TotalTime+DuRep
        endfor
        ; now rescale measuring time (not waiting time)
        NewTotalTime = TotalTime - LostTime
        LostRatio = max([ double(NewTotalTime)/TotalTime, 0.25])
        NewTotalTime = 0
        for index=Run_Nb, n_elements(LightVars.Run.Temperatures)-1 do begin
          if LightVars.Run.Flag_MonTi(index) then begin
            tmp = max([LightVars.Run.Wait_Error(Run_Nb), LightVars.Run.Wait_T(Run_Nb)])
            if LightVars.Run.Preset(index) gt tmp then $
              NewRunDuration    = max([ LightVars.Run.Preset(index)*LostRatio, tmp, 5]) $
            else NewRunDuration = max([ LightVars.Run.Preset(index)*LostRatio, 5])
            NewTotalTime = NewTotalTime+NewRunDuration*LightVars.Run.Repetition(index)
          endif else NewRunDuration = LightVars.Run.Preset(index)*LostRatio
          LightVars.Run.Preset(index) = NewRunDuration
        endfor
        msg = '[ ] Re-scalling sequence (ratio='+strcompress(string(LostRatio))+') to maintain total execution time (as far as possible)'
        Light_LogMessage, LightVars, 'normal', msg
        ; show new sequence analysis
        Light_Event_Base_Run_Analyse, LightVars
        ; resuming Last Valid Numor...
        msg = '[>] Restarting run/sequence (same as numor '+strcompress(string(abs(long(LightVars.Private.LastValidNumor(0)))))+')'
        Light_LogMessage, LightVars, 'normal', msg
        LightVars.Private.LastValidNumor(0) = 0
      endif
      if LightVars.Private.AccuracyAchieved ge 0 and LightVars.Run.Flag_Count(Run_Nb) ne 0 then begin
        ; the ROI check determined that statistics was now ok
        LightVars.Private.AccuracyAchieved = 0
        Light_LogMessage, LightVars, 'normal','[ ] Stopping current counting and going to the next in the sequence (accuracy achieved)'
        ; stop current counting and go to next
        Light_Control, LightVars, LightVars.Instrument.StopCommand
        RunUValue = 'next'
        widget_control, LightVars.Handles.RunStatus,  set_uvalue=RunUValue, bad_id=tmp
      endif

    endif else begin
      if  strpos(strupcase(LightVars.Mad.Status), 'POSITIONNING') lt 0 $
      and strpos(strupcase(LightVars.Mad.Status), 'CHOPPER SETTING') lt 0 $
      and strpos(strupcase(LightVars.Mad.Status), 'WAVELENGTH SETTING') lt 0 then begin
        ; conflict with an other operation: stop process.
        Light_Base_Alert, LightVars, warning='Run/Sequence conflict:'+LightVars.Mad.Status
        Light_LogMessage, LightVars, 'normal','[E] The instrument is doing '+LightVars.Mad.Status+' which prevents the Run/Sequence execution Aborting. If MAD is blocked in non-Idle state, launch a short count.'
        widget_control, LightVars.Handles.RunStatus, set_uvalue = '', bad_id=tmp, set_value=''
        return
      endif ; else wait...
    endelse

  endif else begin
    ; MAD is Idle
    ProcessStep = strsplit(RunUValue, ':', /extract)
    widget_control, LightVars.Handles.RunStatus,  set_value=strcompress(string(Run_Nb))+':'+RunUValue, bad_id=tmp
    case ProcessStep(0) of
    'start':  $
      begin
        if LightVars.Private.First_Run_Nb eq LightVars.Private.Last_Run_Nb then $
          Light_LogMessage, LightVars, 'normal','[I] Starting Single Run '+strcompress(string(LightVars.Private.First_Run_Nb)) $
        else begin
          message = '[I] Starting Sequence '+strcompress(string(LightVars.Private.First_Run_Nb))+'-'
          if LightVars.Private.Last_Run_Nb eq -1 then message = message+'(end)' $
          else message = message+strcompress(string(LightVars.Private.Last_Run_Nb))
            Light_LogMessage, LightVars, 'normal', message
        endelse
        LightVars.Private.OnGoing_Run_Nb = LightVars.Private.First_Run_Nb
        RunUValue = 'setup'
      end
    'setup': $
      begin
        Light_Control, LightVars, $
          string(strtrim(LightVars.Private.ExpTitle,2),    format='(%"'+LightVars.Instrument.SetTitleCommand+'")')
        comment = 'This is Run '+strcompress(string(Run_Nb))+' from Light'
        Light_Control, LightVars, $
          string(comment ,                      format='(%"'+LightVars.Instrument.CommentCommand+'")')
        ; sets subtitle
        subtitle = LightVars.Run.Titles(Run_Nb)
        Light_Control, LightVars, string(subtitle, format='(%"'+LightVars.Instrument.SetSubTitleCommand+'")')
        ; execute optional command
        opt_command = LightVars.Run.Option_Command(Run_Nb)
        if strlen(opt_command) gt 0 then begin
          command_type = ''
          opt_cmd_tokens=strsplit(opt_command, ':', /extract)
          print, opt_cmd_tokens
          case opt_cmd_tokens(0) of
          'idl': command_type = 'idl'
          'system': command_type = 'system'
          'unix': command_type = 'system'
          'mad': command_type = 'mad'
          endcase
          if strlen(command_type) eq 0 then begin
            command_type = 'mad'
            command      = opt_command
          endif else command = strjoin(opt_cmd_tokens(1:(n_elements(opt_cmd_tokens)-1)), ':')
          
          Light_LogMessage, LightVars, 'normal', '[*] '+command_type+':'+command
          case command_type of
            'idl': ok = execute(command)
            'system': spawn, command
            'mad': Light_Control, LightVars, command
          endcase
        endif
        RunUValue = 'temp'
      end
    'temp': $
      begin
        ; if required: change temperature and sets stabilisation T-delay
        LightVars.Private.WaitTempTime = 0
        if LightVars.Run.Temperatures(Run_Nb) ne LightVars.Mad.t_res.tempea(0) then begin
          Light_Control, LightVars, $
            string(LightVars.Run.Temperatures(Run_Nb), format='(%"'+LightVars.Instrument.TempWriteCommand+'")')
          if LightVars.Run.Wait_T(Run_Nb) gt 0 then begin
            msg = '[ ] Wait Temperature stabilisation until about '+systime(0, systime(1)+LightVars.Run.Wait_T(Run_Nb)*60)
            Light_LogMessage, LightVars, 'normal', msg
          endif
        endif
        RunUValue = 'wave'
      end
    'wave': $
      begin
        ; this can take place during wait T-delay
        ; if desired wavelength is changed from current settings, launch an auto setup
        if LightVars.Private.IsSuperUser ne 0 and LightVars.Run.Lambda(Run_Nb) ne LightVars.Mad.t_para.wave then begin
          msg = '[ ] Changing incident wavelength from '+strcompress(string(LightVars.Mad.t_para.wave))+' [Angs] to '+strcompress(string(LightVars.Run.Lambda(Run_Nb)))+' [Angs] (automatic setup)'
          Light_LogMessage, LightVars, 'normal', msg
          Light_Event_Custom_IN6_Setup_Build, LightVars, /auto, wavelength=LightVars.Run.Lambda(Run_Nb)
        endif
        RunUValue = 'wait'
      end
    'wait': $
      begin
        ; ATTN: should also check that T is ok before starting counting !!
        if LightVars.Private.AutoProcess le 0 then begin
          ; check that T-delay has elapsed before starting to count 
          ; (also occurs when T is within limits). See Light_SubDial_Base_Check_Temperature
          if systime(1) ge LightVars.Private.WaitTempTime+LightVars.Run.Wait_T(Run_Nb)*60 then begin
            RunUValue = 'count'
            LightVars.Private.WaitTempTime = 0
          endif
        endif
      end
    'count': $
      begin
        if LightVars.Run.Repetition(Run_Nb) lt 1 then begin
          msg = '[ ] Ignoring Run '+strcompress(string(Run_Nb))+' (repetition='+strcompress(string(LightVars.Run.Repetition(Run_Nb)))+')'
          Light_LogMessage, LightVars, 'verbose', msg
        endif else begin
          ; send count command on time/mon, with or without saving and repetition number
          preset = LightVars.Run.Preset(Run_Nb)
          if LightVars.Run.Flag_MonTi(Run_Nb) ne 0 then TiMon = 'time' else TiMon = 'mon'
          if LightVars.Run.Flag_Save(Run_Nb)  ne 0 then Save  = 'save' else Save  = 'nosave'
          nrep   = LightVars.Run.Repetition(Run_Nb)
          cmd = string(preset, TiMon, nrep, Save, format='(%"'+LightVars.Instrument.CountCommand+'")')
          Light_Control, LightVars, cmd
        endelse
        RunUValue = 'next'
      end
    'T_Out': $
      begin
        base_time = LightVars.Run.Wait_Error(Run_Nb)
        wait_time = LightVars.Run.Wait_T(Run_Nb)
        preset= max([ base_time, wait_time, 5])
        if LightVars.Run.Flag_Save(Run_Nb)  ne 0 then Save  = 'save' else Save  = 'nosave'
        nrep  = 1
        TiMon = 1; 1:time, 0:monitor
        cmd = string(preset, TiMon, nrep, Save, format='(%"'+LightVars.Instrument.CountCommand+'")')
        Light_Control, LightVars, cmd
      end
    'next': $
      begin
        LightVars.Private.OnGoing_Run_Nb = LightVars.Private.OnGoing_Run_Nb+1
        if LightVars.Private.Last_Run_Nb lt 0 then last = n_elements(LightVars.Run.Repetition)-1 $
        else last = LightVars.Private.Last_Run_Nb
        if LightVars.Private.OnGoing_Run_Nb le last then begin
          Light_Event_Base_Run_Fields, LightVars, Run_Nb=LightVars.Private.OnGoing_Run_Nb, /restore
          RunUValue = 'setup'
        endif else begin
          RunUValue = 'end'
        endelse
      end
    'end': $
      begin
        Light_LogMessage, LightVars, 'normal','[I] End of Run/Sequence'
        LightVars.Private.OnGoing_Run_Nb = -1
        LightVars.Private.TemperatureUnstable = 0
        RunUValue = ''
        widget_control, LightVars.Handles.RunStatus,  set_value=RunUValue
      end
    else:
    endcase
    widget_control, LightVars.Handles.RunStatus,  set_uvalue=RunUValue, bad_id=tmp
  endelse ; if strpos(LightVars.Mad.Status, 'Idle') lt 0
  
end ; PRO Light_SubDial_Custom_IN6_Check_Run




