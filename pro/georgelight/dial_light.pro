;***************************************************************
;** Project: dial_light
;** File:    dial_light.pro
;** Version: 2.0.3
;** Date:    July, 24th, 2002
;** Author:  E. Farhi
;** Object:  Port of Light into Dials for George.
;**          Procedures not Instrument dependent
;
;** Require: dial_light.pro, Tools_StructSetValue.pro, Dialog_Fields.pro, TagFile.pro, dial_display.pro
;** Instrum: dial_in6mono.pro, custom.pro
;
;Modified Mar 8th 2002:  Added Displays
;Modified Mar 13th 2002: Changed to dial
;Modified June 4th 2002: (2.0.0) Extended to Custom Menu and procedures
;Modified Jul 11th 2002: (2.0.1) added alert+light_control
;Modified Jul 24th 2002: (2.0.2) added run/sequence execution/stop+ROI analysis
;Modified Aug 08th 2002: (2.0.3) added T check, reorganised procedure names. re-ordered major updates

;
; Procedures:
; These procedures handle stand-alone processes that are not directly related to user actions/events
;
; pro Light_LogMessage, LightVars, DisplayLevel, Message
; pro Light_Control, LightVars, command
;
; pro Light_Base_Init, LightVars
; pro Light_Base_Load_Config_File, LightVars, file, contains=contains, path=path
; pro Light_Base_Save_Config_File, LightVars, file=file, comment=comment, runs=runs, config=config, instrument=instrument, displays=displays
; pro Light_Base_Get_Fields_ToDisplay, LightVars, index=index, $
; pro Light_Base_Build, LightVars
; pro Light_Base_Alert, Lightvars, clear=clear, warning=warning, error=error, toggle=toggle, show=show
; pro Light_Base_Update, LightVars
; pro Light_Base_Event_Parser, event
;
; These procedures handle processes that are triggered by user events (managed through Light_Base_Event_Parser)
;
; pro Light_Event_Base_Close, LightVars
; pro Light_Event_Base_Restart, LightVars
; pro Light_Event_Base_Simulation, LightVars, Simulation=Simulation
; pro Light_Event_Base_SuperUser, LightVars, Event=Event
; pro Light_Event_Base_Load_Instr_File, Lightvars
; pro Light_Event_Base_Save_Instr_File, Lightvars
; pro Light_Event_Base_Save_Preferences, LightVars
; pro Light_Event_Base_Edit_Preferences, LightVars
; pro Light_Event_Base_Edit_Displays, LightVars
; pro Light_Event_Base_Edit_ROI, LightVars, update=update
; pro Light_Event_Base_Edit_Header, LightVars
; pro Light_Event_Base_Show_MAD_Log, LightVars
; pro Light_Event_Base_Show_Light_Log, LightVars
; pro Light_Event_Base_Help_About, LightVars
; pro Light_Event_Base_Help_File, LightVars
; pro Light_Event_Base_Help_Setup, LightVars
; pro Light_Event_Base_Help_Run_Start, LightVars
; pro Light_Event_Base_Help_Preferences, LightVars, update=update
; pro Light_Event_Base_Help_Displays, LightVars, update=update
; pro Light_Event_Base_Help_Run_Options, LightVars, update=update
; pro Light_Event_Base_Run_Analyse, LightVars
; pro Light_Event_Base_Run_Delete, LightVars, Run_Nb=Run_Nb
; pro Light_Event_Base_Run_Clear_All, LightVars
; pro Light_Event_Base_Run_Insert, LightVars, Run_Nb=Run_Nb
; pro Light_Event_Base_Run_Next, LightVars, Run_Nb=Run_Nb
; pro Light_Event_Base_Run_Nb, LightVars, Run_Nb=Run_Nb
; pro Light_Event_Base_Run_Fields, LightVars, Run_Nb=Run_Nb, restore=restore
; pro Light_Event_Base_Run_Options_Build, LightVars, Run_Nb=Run_Nb
; pro Light_Event_Base_Run_Load, LightVars
; pro Light_Event_Base_Run_Save, LightVars
; pro Light_Event_Base_Run_Stop, LightVars
; pro Light_Event_Base_Run_Start_Sequence, LightVars, first=first, last=last
; pro Light_Event_Base_Run_Start_Run, LightVars
;
; These procedures handle processes that are either automatic or launched by user, but may then live for a given time
;
; pro Light_SubDial_Base_Check_ROI, LightVars, show=show
; pro Light_SubDial_Base_Check_Temperature, LightVars
;
; function Light_Get_LightVars
;
;** Usefull Widget Tools:
;**   IDL:  XVarEdit, Display_File, Dialog_Message, Dialog_Pickfile
;**   Light: Dialog_Fields
;***************************************************************


;***************************************************************
;** Procedure to get Light data
function Light_Get_LightVars

  DialTag, 'light', Get=LightVars
  return, LightVars

end; FUN Light_Get_LightVars

;***************************************************************
;** Output Light message to IDL prompt, Light Log window and Light Log file
pro Light_LogMessage, LightVars, DisplayLevel, Message
; input: DisplayLevel, Message
;** Displays the Message if the present ActualLevel (LightVars.Verbosity)
;** matches the DisplayLevel (in silent, user, debug)
;** The date is appended before the message
;** LogString: Message is appened to LogString (string array)
;** File:      LogString is saved to file if specified

ActualLevel    = LightVars.Global.Verbosity

LevelList      = ['silent','normal','verbose','debug']
LocActualLevel = where(strcmp(LevelList, ActualLevel))
LocDisplayLevel= where(strcmp(LevelList, DisplayLevel))

; display message only if level is high enough
if LocActualLevel(0) ge LocDisplayLevel(0) then begin
  LogText = LightVars.Private.LogText
  err     = 0
  isdemo = lmgr(/demo)
  if strlen(LogText(0)) eq 0 then $
    LogText =  ['######################## Light Log File: [I]:Information, [w]:Warning, [E]:Error' ]
  if strlen(LightVars.Global.LogFile) ne 0 then begin  ; append Message into LogFile
    if isdemo eq 0 then begin
        openw, FileUnit, LightVars.Global.LogFile, /append, /get_lun, ERROR=err
        if err ne 0 then $
          print, '[Light:OpenW] Cannot open LogFile '+LightVars.Global.LogFile+':'+!error_state.msg
    endif
  endif
  for index=0,n_elements(Message)-1 do begin
    if n_elements(Message) eq 1 then $
      Message(index) = '['+systime(0)+'] '+Message(index)

    if LightVars.Global.TerminalEcho then print, Message(index)  ; print to IDL terminal
    if (strlen(LightVars.Global.LogFile) ne 0) and (err eq 0) and (isdemo eq 0) then $
      printf, FileUnit, Message(index)
    LogText = [ LogText, Message(index) ]                       ; add to Log string
  endfor
  if (strlen(LightVars.Global.LogFile) ne 0) and (err eq 0) and (isdemo eq 0) then begin
    close, FileUnit
    free_lun, FileUnit
  endif

  Tools_StructSetValue, struct=LightVars, Tag='Private.LogText', val=LogText
  if LightVars.Handles.Base_Light_Log ne 0 then begin
    tmp1 = n_elements(LogText)-20
    if tmp1 lt 0 then tmp1 = 0
    widget_control, LightVars.Handles.Base_Light_Log, set_value=LogText, $
                    SET_TEXT_TOP_LINE=tmp1, Bad_ID=Bad_ID
  endif
endif

end ; PRO Light_LogMessage

;***************************************************************
; do the same as DialControl, but handles Simulation mode
; input: command
pro Light_Control, LightVars, command

  command = strcompress(command)  ; remove duplicated/leading/trailing blanks
  if strlen(command) gt 79 then begin
    Light_LogMessage, LightVars, 'verbose' , '[E] '+LightVars.Generic+': command too long : '+command
    command = strmid(command,0,79)
  endif

  if LightVars.Global.Simulation eq 0 then begin
    res = DialControl(command)
    Light_LogMessage, LightVars, 'verbose' , '[ ] '+LightVars.Generic+': '+command
    if res lt 0 then Light_LogMessage, LightVars, 'verbose' , '[ ] '+LightVars.Generic+': Command error. Look at MAD terminal for details.'
  endif else Light_LogMessage, LightVars, 'verbose' , '[ ] '+LightVars.Generic+': <'+command+'>'

end ; PRO Light_Control

;***************************************************************
;** Init of Light (start and read Config files)
; in the order: default structure loaded at start-up, overload default instr config+user config/preferences
pro Light_Base_Init, LightVars

  forward_function DialNewValue

  if (strlen(LightVars.Global.LogFile) ne 0) then file_delete, LightVars.Global.LogFile, /quiet

  Light_LogMessage, LightVars, 'verbose','[ ] Inititialise Light'

  ; check ICS state at start-up
  Tools_StructSetValue, struct=LightVars, tag='Mad.t_status', val= DialNewValue(type='t_status')

  ;** execute Custom initilization procedure
  ; that extend Light Instrument and Handles structures and define Setup menu
  res = execute(LightVars.Instrument.CustomPro+', /start')
  
  Light_LogMessage, LightVars, 'normal','[ ] Welcome to Light '+LightVars.Private.Version
  Light_LogMessage, LightVars, 'normal','[ ] Instrument is '+strupcase(LightVars.Instrument.Name)+' '+LightVars.Instrument.Type
  
  if strlen(LightVars.Instrument.Name) eq 0 then LightVars.Instrument.Name = getenv('HOSTNAME')
  if strlen(LightVars.Instrument.Name) eq 0 then LightVars.Instrument.Name = getenv('USER')
  if strlen(LightVars.Instrument.Name) eq 0 then LightVars.Instrument.Name = 'ILL'

  ;** Initialises pathes and Ini files
  cd, current=pwd
  LightVars.Global.CurrentDir = pwd         ; Main (current) working LAMP dir
  IniFile = LightVars.Global.IniFile
  if file_test(IniFile) eq 0 then begin
    IniFile  = LightVars.Global.ConfigDir+'Light.ini'  ; no local Light.ini, use ConfigDir (defined in dial_light)
  endif
  if file_test(LightVars.Global.ConfigFile) eq 0 then begin
    LightVars.Global.ConfigFile = LightVars.Global.ConfigDir+'Config.ini' ; default physical instrument info (Monochromators and Choppers)
  endif
  ; read default configuration
  Light_Base_Load_Config_File, LightVars, LightVars.Global.ConfigDir+'Config.ini'
  ; read user configuration
  Light_Base_Load_Config_File, LightVars, IniFile
  ;Light_Base_Load_Config_File, LightVars, file=LightVars.Global.ConfigFile ; Instrument variables definition file
  
  if size(LightVars.Mad.t_status, /type) ne 8 then begin ; not a structure
    LightVars.Mad.Status = '['+strupcase(LightVars.Generic)+' INACTIVE]'
    LightVars.Global.Simulation = 1L
    Light_LogMessage, LightVars, 'silent','[w] '+strupcase(LightVars.Generic)+' is not running. Simulation mode activated.'
  endif

end ; PRO Light_Base_Init

;***************************************************************
;** Read .INI file (Call TagFile for IniFile and ConfigFile)
; input: file
; keyword contains acts as a filter, getting only fields containing 'contains'
; keyword path is used for dialog_pickfile default path
pro Light_Base_Load_Config_File, LightVars, file, contains=contains, path=path

  if  strlen(file) eq 0 then begin  ; file was '' -> set file from dialog_pickfile
    file = dialog_pickfile(Dialog_Parent=LightVars.Handles.Base, file='*.ini', filter='*.ini', path=path, $
            Get_Path=Selected_Path, /Must_Exist, Title='Select a Configuration File to Read into Light')
  endif
  if strlen(file_which(file, /include_current_dir)) eq 0 and file_test(file, /regular) eq 0 then begin
    Light_LogMessage, LightVars, 'verbose' , '[w] Using Default settings (invalid file name '+file+').'
    return
  endif
  Tags=''  ;** To get all Tags in file
  Light_LogMessage, LightVars, 'normal' , '[ ] Reading file '+file_which(file, /include_current_dir)
  TagFile, file, TAG=Tags, GET=StrValues, HEADER=Header
  Light_LogMessage, LightVars, 'verbose', Header

  for index=0, n_elements(Tags)-1 do begin
    ;** Get each Tag, and evaluate corresponding value
    ThisTag      = Tags(index)
    ThisStrValue = StrValues(index)
    ; restrict file tag reading to tag containing 'contains'
    if n_elements(contains) ne 0 then $
      if strpos(ThisTag, contains) lt 0 then ThisTag=''
    if strlen(ThisTag) ne 0 and strlen(ThisStrValue) ne 0 then begin
      idx          = strpos(ThisTag, '.')
      BaseTag      = strtrim(strmid(ThisTag,0,idx(0)),2)
      ThisTag      = strtrim(strmid(ThisTag,idx(0)+1, strlen(ThisTag)),2)
      ok = execute('ThisValue = '+ThisStrValue)
      if ok and strcmp(strupcase(BaseTag), 'LIGHT') then begin
        Tools_StructSetValue, struct=LightVars, Tag=ThisTag, Val=ThisValue
      endif else $
        Light_LogMessage, LightVars, 'normal' , '[w] '+BaseTag+'.'+ThisTag+' = '+ThisStrValue+' ?'
    endif

  endfor

end ; PRO Light_Read_Config_File

;***************************************************************
;** Save Config_File (Inifile)
; input: file
; keywords /runs /config /instrument /displays activate saving of specific sections
; keyword comment is added to header
pro Light_Base_Save_Config_File, LightVars, file=file, comment=comment, runs=runs, config=config, instrument=instrument, displays=displays

  if n_elements(file) eq 0 then file = LightVars.Global.IniFile

  if LightVars.Private.IsSuperUser eq 0 and strmatch(file, LightVars.Global.ConfigDir+'*') then begin
    Light_LogMessage, LightVars, 'normal','[ ] Only SuperUser can save to '+file_which(file, /include_current_dir)
    return
  endif

  FileType = ''
  RunsData = ['']
  DisplaysData = ['']
  if keyword_set(runs) then begin
    tags = tag_names(LightVars.Run)
    FileType = 'Runs '+FileType
    for index=0, n_elements(tags)-1 do $
      RunsData = [ RunsData, 'Run.'+tags(index) ]
  endif

  if keyword_set(displays) then begin
    tags = tag_names(LightVars.Displays)
    FileType = 'Displays '+FileType
    for index=0, n_elements(tags)-1 do $
      DisplaysData = [ DisplaysData, 'Displays.'+tags(index) ]
  endif

  InstrData = ['']
  if keyword_set(instrument) and LightVars.Private.IsSuperUser ne 0 then begin
    tags = tag_names(LightVars.Instrument)
    FileType = 'Instrument '+FileType
    for index=0, n_elements(tags)-1 do $
      InstrData = [ InstrData, 'Instrument.'+tags(index) ]
  endif

  ConfigData = ['']
  if keyword_set(config) then begin
    FileType = 'Preferences '+FileType
    ConfigData =['Global.ConfigDir       ',$
                 'Global.Verbosity       ',$
                 'Global.TerminalEcho    ',$
                 'Global.CurrentDir      ',$
                 'Global.IniFile         ',$
                 'Global.ConfigFile      ',$
                 'Global.LogFile         ',$
                 'Global.RunFile         ',$
                 'Global.Simulation      ',$
                 'Global.Append_Runs     ',$
                 'Global.Current_Run     ',$
                 'GENERIC                ',$
                 'FREQUENCY              ',$
                 'PLOT                   ',$
                 'UPPERLIM               ',$
                 'LOWERLIM               ',$
                 'WUPDATE                ']
  endif

  IniFileData = [ RunsData, InstrData, ConfigData, DisplaysData ]

  Header   =[ '# Light Config file: '+file+' ('+FileType+')',$
              '# Date:              '+systime(0) ]
  if keyword_set(comment) then Header = [ Header, '# '+comment ]
  for index=0, n_elements(IniFileData)-1 do begin
    if strlen(IniFileData(index)) gt 0 then begin
      tmp1 = execute('val=LightVars.'+IniFileData(index))
      tagfile, file, TAG='Light.'+IniFileData(index), SET=val, HEADER=Header
    endif
  endfor
  if strlen(file_which(file, /include_current_dir)) gt 0 then file = file_which(file, /include_current_dir)
  Light_LogMessage, LightVars, 'normal','[ ] Saving '+FileType+' into '+file

end ; PRO Light_Base_Save_Config_File

;***************************************************************
;*********************** MAIN  INTERFACE ***********************
;***************************************************************

;***************************************************************
;** set values to display (from Global, updated on request) using index as Run_Nb in the Run sequence
; input: index
; other keywords are return values
pro Light_Base_Get_Fields_ToDisplay, LightVars, index=index, $
        header=header, h_val=h_val, spy=spy, s_val=s_val, run=run, r_val=r_val, check=check, c_val=c_val

  header=['User Name:     ',$
          'Local Contact: ',$
          '']; was initially 'Exp. Title:    ', 'Exp. Number:   ' changed to get space on Light interface
  h_val =[LightVars.Private.UserName, LightVars.Private.LocalContact ]
  spy   =['Monit.[cts]' ,$
          'Detec.[cts]', $
          'T Sam.[K]', $
          'T Reg.[K]']
  s_val =[LightVars.Private.MonitorSum, LightVars.Private.DetectorSum, LightVars.Private.TSample, LightVars.Private.TRegulation]
  run   =['SubTitle', $
          'Temperature [K]', $
          'Lambda [AA]', $
          'Preset', $
          'Repetition' ]
  if n_elements(index) eq 0 then index  = LightVars.Global.Current_Run
  r_val =[0.0, $
          LightVars.Run.Temperatures(index), $
          LightVars.Run.Lambda(index), $
          LightVars.Run.Preset(index), $
          LightVars.Run.Repetition(index) ]
  check =['Preset unit in Run',$
          'Check T',$
          'Save Perm.']
  c_val =[LightVars.Run.Flag_MonTi(index), $
          LightVars.Run.Flag_T(index), $
          LightVars.Run.Flag_Save(index)]

end ; PRO Light_Base_Get_Fields_ToDisplay

;***************************************************************
;** Build the Main instrument panel (show user, local, experiments, runs, ...)
pro Light_Base_Build, LightVars

  ;** Build base interface (that can call Instrument setup and Monochromator setup with buttons)
  
  ;** Get information and labels from LightVars
  Light_Base_Get_Fields_ToDisplay, LightVars, index=index, $
      header=header, h_val=h_val, spy=spy, s_val=s_val, run=run, r_val=r_val, check=check, c_val=c_val

  widget_control, /HOURGLASS
  LightVars.Handles.Base = widget_base(/column, $
    title='['+strupcase(LightVars.Instrument.Name)+'] Instrument Control', $
    /tlb_size_events, /TLB_KILL_REQUEST_EVENTS, mbar=Base_Menu)
  widget_control, LightVars.Handles.Base, tlb_set_xoffset=80, tlb_set_yoffset=80  ; original position/upper-left
  widget_control, LightVars.Handles.Base, Set_Uname='Light_Base_Interface'

  ;** TOP menu and buttons
  Base_Top  =widget_base(LightVars.Handles.Base,/row, xpad=0)  ; frame=10

  Base_File =widget_button(Base_Menu,value='File >',uvalue='Light_Event_Base_File_Menu, LightVars', menu=2)
  ;** now comes the pop-up 'File >' menu items
  tmp1= widget_button(Base_File,value='Edit Light Preferences...', uvalue='Light_Event_Base_Edit_Preferences, LightVars')
  tmp1= widget_button(Base_File,value='Save Light Preferences...', uvalue='Light_Event_Base_Save_Preferences, LightVars')
  tmp1= widget_button(Base_File,value='Show Light Log file...', uvalue='Light_Event_Base_Show_Light_Log, LightVars')
  tmp1= widget_button(Base_File,value='Show '+strupcase(LightVars.Generic)+' Log file...', uvalue='Light_Event_Base_Show_MAD_Log, LightVars')
  tmp1= widget_button(Base_File,value='Restart Light',uvalue='Light_Event_Base_Restart, LightVars')
  tmp1= widget_button(Base_File,value='Exit Light',    uvalue='Light_Event_Base_Close, LightVars', /separator)
  widget_control, LightVars.Handles.Base, cancel_button=tmp1

  Base_Setup=widget_button(Base_Menu, value='Setup >',uvalue='Light_Event_Base_Setup_Menu, LightVars', menu=2)
  ;** now comes the pop-up 'Setup >' menu items (from LightVars.Instrument.Custom)
  for index=0,n_elements(LightVars.Instrument.CustomMenuItems)-1 do begin
    tmp1= widget_button(Base_Setup,value=LightVars.Instrument.CustomMenuItems(index), $
                                   uvalue=LightVars.Instrument.CustomMenuPro(index))
  endfor
  tmp1= widget_button(Base_Setup,value='Displays...', uvalue='Light_Event_Base_Edit_Displays, LightVars', /separator)
  tmp1= widget_button(Base_Setup,value='Regions of Interest (ROIs)...', uvalue='Light_Event_Base_Edit_ROI, LightVars')
  tmp1= widget_button(Base_Setup,value='Analyse ROIs...', uvalue='Light_Event_Base_Edit_ROI, LightVars, /update & Light_SubDial_Base_Check_ROI, LightVars, /show')
  tmp1= widget_button(Base_Setup,value='SuperUser login...',   uvalue='Light_Event_Base_SuperUser, LightVars, Event=Event', /separator)
  LightVars.Handles.Simulation= widget_button(Base_Setup,value='Control Mode: Active',     uvalue='Light_Event_Base_Simulation, LightVars')
  LightVars.Handles.LoadSaveCfg(0)= widget_button(Base_Setup,value='Load configuration...', uvalue='Light_Event_Base_Load_Instr_File, LightVars', /separator)
  LightVars.Handles.LoadSaveCfg(1)= widget_button(Base_Setup,value='Save configuration...', uvalue='Light_Event_Base_Save_Instr_File, LightVars')


  Base_Run=widget_button(Base_Menu, value='Runs >',uvalue='Light_Event_Base_Run_Menu, LightVars', menu=2)
  ;** now comes the pop-up 'Runs >' menu items
  tmp1= widget_button(Base_Run,value='Load Run/Sequence...',  uvalue='Light_Event_Base_Run_Load, LightVars')
  tmp1= widget_button(Base_Run,value='Save Run/Sequence...',  uvalue='Light_Event_Base_Run_Save, LightVars')
  tmp1= widget_button(Base_Run,value='Analyse/Show Sequence...', uvalue='Light_Event_Base_Run_Analyse, LightVars', /separator)
  tmp1= widget_button(Base_Run,value='Run Options...',        uvalue='Light_Event_Base_Run_Options_Build, LightVars')
  tmp1= widget_button(Base_Run,value='Clear All Runs',        uvalue='Light_Event_Base_Run_Clear_All, LightVars')
  tmp1= widget_button(Base_Run,value='Delete Current Run',    uvalue='Light_Event_Base_Run_Delete, LightVars')
  tmp1= widget_button(Base_Run,value='Insert Run',            uvalue='Light_Event_Base_Run_Insert, LightVars')
  tmp1= widget_button(Base_Run,value='Next Run (go to)',      uvalue='Light_Event_Base_Run_Next, LightVars')
  LightVars.Handles.RunStartButton(0)= widget_button(Base_Run,value='Start Single Run',      uvalue='Light_Event_Base_Run_Start_Run, LightVars', /separator)
  LightVars.Handles.RunStartButton(1)= widget_button(Base_Run,value='Start Sequence',        uvalue='Light_Event_Base_Run_Start_Sequence, LightVars')
  tmp1= widget_button(Base_Run,value='Stop  Run/Sequence',    uvalue='Light_Event_Base_Run_Stop, LightVars')

  Base_Help =widget_button(Base_Menu, value='Help >',uvalue='Light_Event_Base_Help_Menu, LightVars', menu=2)
  ;now comes the pop-up 'Help >' menu items
  tmp1= widget_button(Base_Help,value='About [File]',      uvalue='Light_Event_Base_Help_File, LightVars')
  tmp1= widget_button(Base_Help,value='About [Setup]',     uvalue='Light_Event_Base_Help_Setup, LightVars')
  tmp1= widget_button(Base_Help,value='About [Runs]',      uvalue='Light_Event_Base_Help_Run_Start, LightVars')
  tmp1= widget_button(Base_Help,value='About [Preferences]',uvalue='Light_Event_Base_Help_Preferences, LightVars')
  tmp1= widget_button(Base_Help,value='About [Displays]',  uvalue='Light_Event_Base_Help_Displays, LightVars')
  tmp1= widget_button(Base_Help,value='About [Run Options]',uvalue='Light_Event_Base_Help_Run_Options, LightVars')
  tmp1= widget_button(Base_Help,value='About Light...'    ,uvalue='Light_Event_Base_Help_About, LightVars', /separator)

  ;** MAD status (colors/styles are defined in lamp.ressource: light1mad, light2mad)
  Base_Mad_Info                     = widget_base(LightVars.Handles.Base,/column, frame=5)
  Base_Mad_Control                  = widget_base(Base_Mad_Info,       /row,    frame=3)
  LightVars.Handles.Base_Mad_Status  = widget_base(Base_Mad_Control, resource_name='light1ics', map=1)
  LightVars.Handles.Mad_Status       = widget_label(LightVars.Handles.Base_Mad_Status, value=LightVars.Mad.Status, xsize=130, /align_center, frame=1)

  ;** MAD alert (colors/styles are defined in lamp.ressource: light1alert, light2alert)
  Base_Alert= widget_base(Base_Mad_Control, uvalue='Light_Event_Base_Display_Log, LightVars')
  ;** 'Clear' alert button
  cl_al = 'Light_Base_Alert, LightVars, /clear, /show'
  LightVars.Handles.Base_Alert_Status = widget_button(Base_Mad_Control, value='no alert ---------------------', uvalue=cl_al, sensitive=0)

  ;** 'SPY abstract' section (sum of counts, monitor, T), whole section clears alert
  Base_Spy= widget_base(Base_Mad_Info, uvalue=cl_al, /row, frame=1)
  lab_id  = lonarr(n_elements(spy))
  tmp1    = widget_base(Base_Spy, uvalue=cl_al, /column , space=0, xpad=0);
  for index=0,1 do begin
    tmp2         = widget_base(tmp1, uvalue=cl_al, /row, ypad=0, xpad=0);
    tmp3         = widget_label(tmp2, value=spy(index))
    lab_id(index)= widget_text(tmp2,  value=string(s_val(index)), xsize=7)
  endfor
  tmp1    = widget_base(Base_Spy, uvalue=cl_al, /column , space=0, xpad=0)
  for index=2,3 do begin
    tmp2         = widget_base(tmp1, uvalue=cl_al, /row, ypad=0, xpad=0)
    lab_id(index)= widget_text(tmp2,  value=string(s_val(index)), xsize=7)
    tmp3         = widget_label(tmp2, value=spy(index))
  endfor
  LightVars.Handles.MonitorSum   = lab_id(0)
  LightVars.Handles.DetectorSum  = lab_id(1)
  LightVars.Handles.TSample      = lab_id(2)
  LightVars.Handles.TRegulation  = lab_id(3)

  ;**Experiment (Run) setup. (Was initially in custom.pro:run_experiment)
  Base_Run= widget_base(LightVars.Handles.Base, /column, frame=5)
  tmp1    = widget_base(Base_Run, /row)  ; first row
  tmp2    = widget_label  (tmp1, value='EXP. RUNS ')
  LightVars.Handles.RunStartButton(2) = widget_button (tmp1, value='Start >', menu=2, uvalue='')  ; pop-up menu
  tmp3    = widget_button (LightVars.Handles.RunStartButton(2), value='Start Sequence',  uvalue='Light_Event_Base_Run_Start_Sequence, LightVars')
  tmp3    = widget_button (LightVars.Handles.RunStartButton(2), value='Start Single Run',       uvalue='Light_Event_Base_Run_Start_Run, LightVars')
  ;** STOP and Analyse buttons
  tmp2    = widget_button (tmp1, value='STOP',            uvalue='Light_Event_Base_Run_Stop, LightVars')        ; STOP button
  tmp2    = widget_button (tmp1, value='Analyse...',      uvalue='Light_Event_Base_Run_Analyse, LightVars')     ; Analyse button
  LightVars.Handles.RunStatus = widget_label  (tmp1, value='         ', uvalue='')


  ;** Run buttons: Run No, Next, Delete, Insert
  Base_Run_Def= widget_base(Base_Run,    /column, frame=3)
  Base_Run    = widget_base(Base_Run_Def,/row,    frame=1)
  LightVars.Handles.Current_Run = widget_droplist(Base_Run, $
                                value=string(LightVars.Global.Current_Run, format='(I3)'), $
                                uvalue='Light_Event_Base_Run_Nb, LightVars, Run_Nb=Event.Index', title='Run No:', /DYNAMIC_RESIZE)
  tmp1        = widget_button( Base_Run, value='Next', uvalue='Light_Event_Base_Run_Next, LightVars')
  tmp1        = widget_base(   Base_Run, /row, xsize=15) ; empty space on row
  Base_Run    = widget_base(   Base_Run, /row)
  tmp1        = widget_button (Base_Run, value='Delete', uvalue='Light_Event_Base_Run_Delete, LightVars')
  tmp1        = widget_button (Base_Run, value='Insert', uvalue='Light_Event_Base_Run_Insert, LightVars')

  ;** Run Title, Temp, Lambda, Preset, repetition
  Base_Run    = widget_base(Base_Run_Def,/column, frame=1)
  Base_0      = widget_base(Base_Run,/row)
  
  lab_id      = [0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L ]

  index = 0 & XSize = 25 & value = LightVars.Run.Titles(index) ; Title is a 20 char string
  lab_id(0) = cw_field(Base_0, title='',$
      value  =value,   uvalue  ='Light_Event_Base_Run_Fields, LightVars', xsize=XSize, /row, /all_events)
  tmp2 = widget_label(Base_0, value=run(index))

  Base_0      = widget_base(Base_Run,/row)
  Base_1c      = widget_base(Base_0,/column)
  Base_2c      = widget_base(Base_0,/column)

  IntField    = [0,0,0,0,1]
  FloatField  = [0,1,1,1,0]
  order       = [2,3,4,1]
  ; 0 Title is a 20 char string
  ; 2 Lambda+Options
  ; 3 Preset+Mn/Ti
  ; 4 Repetition+Save
  ; 1 Temperature+Check T
  for o_index=0,n_elements(order)-1 do begin
    index = order(o_index)
    XSize = 7
    value = r_val(index)  ; other fields
    tmp1 = widget_base(   Base_1c, /row)
    lab_id(index) = cw_field(tmp1, title='',$
      value  =value,   uvalue  ='Light_Event_Base_Run_Fields, LightVars', xsize=XSize, /row, /all_events, $
      integer=IntField(index),floating=FloatField(index))
    tmp2 = widget_label(tmp1, value=run(index))
    if index eq 2 then begin  ;add a Modify/Options button after Lambda
      tmp2= widget_button( Base_2c, value='Options...', ysize=40, uvalue='Light_Event_Base_Run_Options_Build, LightVars')
    endif
    if index eq 1 then begin  ;add a Check T button after Temperature
      tmp2= widget_base(   Base_2c, /row)
      lab_id(7)= cw_bgroup(tmp2, label_left='',/row,$
      [' '], /nonexclusive, uvalue='Light_Event_Base_Run_Fields, LightVars', set_value=c_val(1))
      tmp2 = widget_label(tmp2, value=check(1))
    endif
    if index eq 3 then begin  ;add the Ti/Mn button text after Preset
      lab_id(5)= cw_bgroup(Base_2c, /exclusive, label_left='',/row,$
      ['Mon','Ti [min]'], uvalue='Light_Event_Base_Run_Fields, LightVars', set_value=c_val(0))
    endif
    if index eq 4 then begin  ;add the Save button after repetition
      tmp2= widget_base(   Base_2c, /row)
      lab_id(6)= cw_bgroup(tmp2, label_left='',/row,$
      [' '], /nonexclusive, uvalue='Light_Event_Base_Run_Fields, LightVars', set_value=c_val(2))
      tmp2 = widget_label(tmp2, value=check(2))
    endif
  endfor
  LightVars.Handles.Titles             = lab_id(0)
  LightVars.Handles.Temperatures       = lab_id(1)
  LightVars.Handles.Lambda             = lab_id(2)
  LightVars.Handles.Preset             = lab_id(3)
  LightVars.Handles.Repetition         = lab_id(4)
  LightVars.Handles.Flag_MonTi         = lab_id(5)
  LightVars.Handles.Flag_Save          = lab_id(6)
  LightVars.Handles.Flag_T             = lab_id(7)

  ;** Experiment (Header/info) setup. (Was initially in custom.pro:man_exp_head)
  lab_id        = lonarr(n_elements(header))
  Base_Exp_Base1=widget_base(LightVars.Handles.Base,/row)
  tmp1= widget_label(Base_Exp_Base1,  value='EXPERIMENT:  ')
  LightVars.Handles.UserName    = widget_label(Base_Exp_Base1, value=LightVars.Private.UserName,/align_left)
  LightVars.Handles.LocalContact= widget_label(Base_Exp_Base1, value=LightVars.Private.LocalContact,/align_left)

  tmp1= widget_button(Base_Exp_Base1, value='Edit Header',uvalue='Light_Event_Base_Edit_Header, LightVars')
  Base_Exp_Base1=widget_base(LightVars.Handles.Base,/column)
  row = widget_base(Base_Exp_Base1,/row)
  tmp                           = widget_label(row, value='Title:    ',/align_left)
  LightVars.Handles.ExpTitle    = widget_label(row, value=strmid(LightVars.Private.ExpTitle,0,46),/align_right)
  row = widget_base(Base_Exp_Base1,/row)
  tmp                           = widget_label(row, value='Sub Title:',/align_left)
  LightVars.Handles.SubTitle    = widget_label(row, value=strmid(LightVars.Private.SubTitle,0,46),/align_right)
  row = widget_base(Base_Exp_Base1,/row)
  tmp                           = widget_label(row, value='Numor:    ',/align_left)
  LightVars.Handles.Numor       = widget_label(row, value=LightVars.Private.Numor,/align_right)

  ;** Command File (Run)
  Base_Run= widget_base(LightVars.Handles.Base,/row, frame=1)
  LightVars.Handles.RunFile= widget_label(Base_Run, /align_left, /dynamic_resize, $
    value='Starting with '+LightVars.Global.RunFile)

  ;** display and launch XManager
  widget_control, LightVars.Handles.Base, /realize
  XManager, 'Light_Base_Interface', LightVars.Handles.Base, Event_Handler='Light_Base_Event_Parser', $
      /just_reg, /no_block

  Base_Geometry= widget_info(LightVars.Handles.Base, /geometry)
  LightVars.Private.Base_Size=[ Base_Geometry.XSize, Base_Geometry.YSize ]

  ;** Update Run fields
  Light_Event_Base_Run_Fields, LightVars, /restore
  Light_Event_Base_Simulation, LightVars, Simulation = LightVars.Global.Simulation

  ; now put the instrument specific configuration to SU/normal Mode (call CustomPro in SU check mode)
  ;** execute Custom CustomPro SU check procedure
  res = execute(LightVars.Instrument.CustomPro+', /update_buttons')

  Light_Base_Alert, Lightvars, /clear

end ; PRO Light_Base_Build

;*****************************************
; handle alert state
; warning/error message are also appended to Light Log
; keywords: 
; /clear: clear alert, 
; warning: add a warning
; error: add an error
; /toggle: toggle alert blinking state
; /show: show Lght log
pro Light_Base_Alert, Lightvars, clear=clear, warning=warning, error=error, toggle=toggle, show=show

if LightVars.Private.ErrorMsgToggle eq 0 then LightVars.Private.ErrorMsgToggle = 1 $
else LightVars.Private.ErrorMsgToggle = 0

; toggle sensitive state for blinking alert
if n_elements(toggle) ne 0 and strpos(LightVars.Private.LastErrorMsg, 'no alert') lt 0 then begin
  if LightVars.Private.ErrorMsgToggle eq 0 then begin
    widget_control, LightVars.Handles.Base_Alert_Status, sensitive=1, set_value=''
  endif else begin
    widget_control, LightVars.Handles.Base_Alert_Status, sensitive=1, set_value=LightVars.Private.LastErrorMsg
  endelse
endif

; clear alert, and show/raise Light Log
if n_elements(clear) ne 0 then begin
  widget_control, LightVars.Handles.Base_Alert_Status, sensitive=1
  if strpos(LightVars.Private.LastErrorMsg, 'no alert') lt 0 then show = 1
  LightVars.Private.LastErrorMsg = '   <no alert>    '
  widget_control, LightVars.Handles.Base_Alert_Status, set_value=LightVars.Private.LastErrorMsg, sensitive= 1
endif

if n_elements(show) ne 0 then Light_Event_Base_Show_Light_Log, LightVars

; set warning alert
if n_elements(warning) ne 0 then begin
  ToSet = '[W] '+warning
  Light_LogMessage, LightVars, 'silent' , '[W] WARNING '+warning
  ; only if was in 'no alert mode'
  if strpos(LightVars.Private.LastErrorMsg, 'no alert') ge 0 then begin
    LightVars.Private.LastErrorMsg = ToSet
    widget_control, LightVars.Handles.Base_Alert_Status, set_value=ToSet, sensitive= 1
  endif
endif

; set error alert
if n_elements(error) ne 0 then begin
  ToSet = 'ERROR '+error(0)
  Light_LogMessage, LightVars, 'silent' , '[E] '+ToSet(0)
  LightVars.Private.LastErrorMsg = ToSet(0)
  widget_control, LightVars.Handles.Base_Alert_Status, set_value=ToSet(0), sensitive= 1
endif

end ; PRO Light_Base_Alert

;***************************************************************
; Main Update procedure. Executed at each Dial Frequency
; calls instrument collect procedure, and update required base fields/widgets
pro Light_Base_Update, LightVars

  tic = systime(1)
  
  ;** Build base interface (that can call Instrument setup with buttons) if does not exist
  if LightVars.Handles.Base eq 0 or widget_info(LightVars.Handles.Base, /valid_id) eq 0 then Light_Base_Build, LightVars

  ;** MINOR UPDATE SECTION ***********************************************************
  
  ; Collect data from the Generic Instrument Control program (Custom routine)
  ; the collect procedure gets either a quick update status), or full data collect when Major Update occurs
  ; or when Mad.Status changed
  res = execute(LightVars.Instrument.CustomPro+', /collect')

  ;** Update MadStatus and Error
  Light_Base_Alert, Lightvars, /toggle
  widget_control, LightVars.Handles.Mad_Status, set_value=LightVars.Mad.Status
  
  LightVars.Value = LightVars.Mad.Status

  ;** Update SU mode
  widget_control, LightVars.Handles.Base_SuperUser, get_uvalue=Event, Bad_Id=Bad_Id
  LightVars.Private.IsSuperUser = 0
  if n_elements(Event) eq 0 then Event=''
  if LightVars.Handles.Base_SuperUser ne 0 and LightVars.Handles.Base_SuperUser eq Bad_Id then begin ; SU window was closed
    LightVars.Handles.Base_SuperUser = 0
    Light_LogMessage, LightVars, 'silent','[w] SuperUser logged out.'
    ; now put the instrument specific configuration to normal Mode (call CustomPro in SU check mode)
    ;** execute Custom Init SU procedure
    res = execute(LightVars.Instrument.CustomPro+', /update_buttons')
  endif
  if strcmp(strupcase(Event),'LIGHT_EVENT_SU_MODE') then begin
    LightVars.Private.IsSuperUser = 1
    ; now put the instrument specific configuration to SU Mode (call CustomPro in SU check mode)
    ;** execute Custom Init SU procedure
    res = execute(LightVars.Instrument.CustomPro+', /update_buttons')
  endif
  
  ; ** MAJOR UPDATE SECTION ***********************************************************
  if LightVars.Private.Update_Nb le 1 then begin

    ; update the displays, define aliases for Mad variables (temperature, data, etc...)
    Mad       = LightVars.Mad
    t_nother  = Mad.t_nother
    t_res     = Mad.t_res
    t_para    = Mad.t_para
    t_status  = Mad.t_status
    t_counts  = Mad.t_counts
    t_chopper = Mad.t_chopper
    data      = Mad.Data

    ; now specialised aliases for IN6 ************************************************
    dims      = size(data, /dim)
    data     = data(0:(dims(0) -2), *)
    Monitor  = data(*,0)     ; this is Monitor1
    data     = data(*,t_res.N_MON_SPECT:(t_res.N_TOTAL_SPECT-1)) ; for IN6
    detectors= total(data,1)  ; Detectors on X (length=340), sum of row
    tof      = total(data,2)  ; TOF channel on X (length=LightVars.Mad.t_para.tof_cha_resol = 512, 1024...)

    Temperatures = t_res.tempea
    TSet    = Temperatures(0)
    TReg    = Temperatures(1)
    TSample = Temperatures(2)
    Power   = Temperatures(3)
    ; end of specialised aliases for IN6 *********************************************

    ToUpdate = ['MonitorSum', 'DetectorSum',  'TSample',   'TRegulation', $
                'UserName',   'LocalContact', 'ExpTitle', 'Numor', 'Subtitle', $
                'RunFile' ]
    for index=0, n_elements(ToUpdate)-1 do begin
    ;** Update fields from ToUpdate, taking Widget Handle (ID) and its value...
      tmp1 = execute('Widget = LightVars.Handles.'+ToUpdate(index))
      if ToUpdate(index) eq 'RunFile' then $
        tmp2 = execute('Value  = LightVars.Global.'+ToUpdate(index)) $
      else tmp2 = execute('Value  = LightVars.Private.'+ToUpdate(index))
      if not tmp1 or not tmp2 then begin
        print, tmp1, ':Widget = LightVars.Handles.'+ToUpdate(index)+' ?'
        print, tmp2, ':Value  = LightVars.Global.'+ToUpdate(index)+' ?'
      endif else begin
        if strcmp(ToUpdate(index), 'RunFile') then begin
          if strlen(string(Value)) gt 25 then Value = 'Cmd File: ...'+strmid(string(Value),25,/reverse) $
          else Value = 'Cmd File: '+string(Value)
          if n_elements(Widget) ne 0 and Widget ne 0 then widget_control, Widget, set_value=string(Value), /dynamic_resize
        endif else begin
          Value = strcompress(string(Value))
          if n_elements(Widget) ne 0 and Widget ne 0 then widget_control, Widget, set_value=string(Value)
        endelse
      endelse
    endfor

    ; update displays and ROIs ***************************************************

    ; handle the temperature check
    Light_SubDial_Base_Check_Temperature, LightVars

    ; handle ROI regions (edition)
    Light_Event_Base_Edit_ROI, LightVars, /update
    ; handle ROI analysis
    Light_SubDial_Base_Check_ROI, LightVars
    
    toc = systime(1)
    Elapsed = toc-tic

    for index=0, n_elements(LightVars.Displays.Variables)-1 do begin
      Variable = LightVars.Displays.Variables(index)
      Type     = LightVars.Displays.Type(index)
      Options  = LightVars.Displays.Options(index)
      Name     = strjoin(strsplit(Variable,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')
      if strlen(Variable) gt 0 and LightVars.Private.Update_Nb eq 1 then begin
        if Name(0) ge '0' and Name(0) le '9' then Name='Light_'+Name
        ToExec   = "display, group_leader=LightVars.Handles.Base, Data="+Variable
        if strpos(strlowcase(Options), "name_data") lt 0 then ToExec=ToExec+", Name_Data='"+Name+"'"
        if strlen(Type) gt 0 then ToExec   = ToExec+", type='"+Type+"'"
        if strlen(Options) gt 0 then ToExec   = ToExec+', '+Options
        ; Light_LogMessage, LightVars, 'debug','[ ] Light: '+ToExec
        ok = execute(ToExec)
        if not ok and strpos(LightVars.Displays.Options(index), "unactivate") lt 0 then if strlen(Options) eq 0 then LightVars.Displays.Options(index) = '/unactivate' $
          else LightVars.Displays.Options(index) = [ Options+', /unactivate' ]
      endif
    endfor

  endif
  
  ; handle Update level 'Private.Update_Nb': 0: starting, 1:Major Update. ++ until Private.Major_Update_Nb
  LightVars.Private.Update_Nb = LightVars.Private.Update_Nb+1
  if LightVars.Private.Update_Nb gt LightVars.Private.Major_Update_Nb then LightVars.Private.Update_Nb = 1
  
  ;** Update Dial structure
  DialTag, 'light', tag='Global',    set=LightVars.Global
  DialTag, 'light', tag='Private',   set=LightVars.Private
  DialTag, 'light', tag='Mad',       set=LightVars.Mad
  DialTag, 'light', tag='Handles',   set=LightVars.Handles
  DialTag, 'light', tag='Run',       set=LightVars.Run
  DialTag, 'light', tag='Instrument',set=LightVars.Instrument

end ; PRO Light_Base_Update

;***************************************************************
;*************************** EVENTS  ***************************
;***************************************************************

;***************************************************************
;** Light_Base_Event_Parser : event manager
pro Light_Base_Event_Parser, event

  ;** retrieve the 'uvalue' from Widget -> Get event description
  widget_control, event.id, get_uvalue=uv
  LightVars = Light_Get_LightVars()

  ;** First handle close and resize events
  if  tag_names(event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' then begin
      Light_Event_Base_Close, LightVars
      return
  endif else begin
    if tag_names(event, /STRUCTURE_NAME) eq 'WIDGET_BASE' then begin
        Light_LogMessage, LightVars, 'debug','[ ] Light: Restore Base dimensions'
        widget_control, LightVars.Handles.Base, XSize=LightVars.Private.Base_Size(0), YSize=LightVars.Private.Base_Size(1)
    endif else begin
      if size(uv, /type) eq 7 then begin
        ;** Checks that the Event routine exists...
        Routine = uv
        res = execute(Routine)
        if res ne 0 then begin
         if strpos(Routine, 'Light_Event_Base_Close') ge 0 then return
         Light_LogMessage, LightVars, 'debug','[ ] Light_Base_Event_Parser:'+uv
        endif else begin
         Light_LogMessage, LightVars, 'debug','[E] PRO '+Routine+' ?'
        endelse
      endif else begin
        Light_LogMessage, LightVars, 'debug','[E] Light: Unknown event'
        help, event, uv
      endelse
    endelse
  endelse

  if LightVars.Handles.Base ne 0 then begin
    ;** Update Dial structure
      DialTag, 'light', tag='Global',    set=LightVars.Global
      DialTag, 'light', tag='Private',   set=LightVars.Private
      DialTag, 'light', tag='Mad',       set=LightVars.Mad
      DialTag, 'light', tag='Handles',   set=LightVars.Handles
      DialTag, 'light', tag='Run',       set=LightVars.Run
      DialTag, 'light', tag='Instrument',set=LightVars.Instrument
      DialTag, 'light', tag='Displays',  set=LightVars.Displays
  endif

end ; PRO Light_Base_Event_Parser

;***************************************************************
;** Display information about Light/File menu
pro Light_Event_Base_Close, LightVars

  ; verify that MAD is not running (counting, xbu, ...)
  widget_control, LightVars.Handles.Mad_Status, set_value='<EXITING>'
  Light_Event_Base_Save_Preferences, LightVars
  if strlen(LightVars.Global.LogFile) ne 0 then $
    Light_LogMessage, LightVars, 'normal','[ ] Light Log was saved for this session in '+file_which(LightVars.Global.LogFile, /include_current_dir)
  Light_LogMessage, LightVars, 'normal','[ ] Exiting'
  widget_control, LightVars.Handles.Base, /destroy
  DialStop,  'light'
  DialClear, 'light'
  LightVars.Handles.Base = 0L
  return

end ; PRO Light_Event_Base_Close

;***************************************************************
;** Restart Light and Reads Initilisation files (Ini/Config)
pro Light_Event_Base_Restart, LightVars

  Light_LogMessage, LightVars, 'normal','[ ] Restart Light -> Init'
  widget_control, LightVars.Handles.Base, /destroy
  LightVars.Handles.Base = 0L
  Light_Base_Init, LightVars
  Light_Base_Build, LightVars

end ; PRO Light_Event_Base_Restart

;*****************************************
; start/stop simulation mode/control mode
pro Light_Event_Base_Simulation, LightVars, Simulation=Simulation

  if n_elements(Simulation) eq 0L then begin ; toggle mode
    if LightVars.Global.Simulation eq 0L then aSimulation = 1L $
    else aSimulation = 0L
  endif else aSimulation = Simulation

  if (aSimulation ne 0) then begin
    LightVars.Global.Simulation = 1L
    widget_control, LightVars.Handles.Simulation, set_value='Control Mode: Simulation', bad_id=tmp1
  endif else begin
    LightVars.Global.Simulation = 0L
    widget_control, LightVars.Handles.Simulation, set_value='Control Mode: Active', bad_id=tmp1
  endelse

end ; PRO Light_Event_Base_Simulation

;***************************************************************
;** Ask for SuperUser mode on creation,
pro Light_Event_Base_SuperUser, LightVars, Event=Event

  widget_control, LightVars.Handles.Base_SuperUser, Bad_ID=Bad_ID, show=1, iconify=0
  ;** If does not exist, make it
  if Bad_ID ne 0 or LightVars.Handles.Base_SuperUser eq 0 then  begin

    Title='SuperUser Login for '+LightVars.Instrument.Name
    Base = widget_base(/column, Group_Leader=LightVars.Handles.Base, Title=Title)
    LightVars.Handles.Base_SuperUser = cw_field(Base, Title='Please enter SuperUser password', $
                                      /column, /RETURN_EVENTS, UName='Not SU', uvalue='Light_Event_Base_SuperUser, LightVars, Event=Event')
    widget_control, Base, /realize
    XManager, 'Base_SU', Base, Event_Handler='Light_Base_Event_Parser', $
        /just_reg, /no_block

  endif else begin
    if strcmp(tag_names(event, /structure), 'WIDGET_BUTTON') then begin
      Light_LogMessage, LightVars, 'normal' , '[w] You are already SuperUser, Close window to log out'
      return
    endif
    Passwd = Event.Value
    Top    = Event.Top
    if n_elements(Passwd) eq 0 then begin
      Light_LogMessage, LightVars, 'silent' , '[E] Please enter a non empty Passwd.'
      return
    endif else begin
      ;** check value of SU passwd.
      if strcmp(Passwd, 'anita') then begin
        ;** if OK, set IsSuperser flag and hide text field (map=0), but keep window
        widget_control, Top, tlb_set_title='SUPERUSER MODE ACTIVE'
        widget_control, LightVars.Handles.Base_SuperUser, set_value='Close to log out SU', sensitive=0, set_uvalue='Light_Event_SU_Mode'
        Light_LogMessage, LightVars, 'silent' , '[w] SuperUser Login: Success. Close window to log-out.'
      endif else begin
        Light_LogMessage, LightVars, 'silent' , '[E] SuperUser Login: Bad password.'
        widget_control, LightVars.Handles.Base_SuperUser, set_value='* Bad password *'
      endelse
    endelse
  endelse

end ; PRO Light_Event_Base_SuperUser

;*****************************************
; load instrument configuration file (when SU mode)
pro Light_Event_Base_Load_Instr_File, Lightvars

  if LightVars.Private.IsSuperUser eq 0 then begin
    Light_LogMessage, LightVars, 'normal','[w] Only SuperUser can load new instrument configuration files'
    return
  endif else begin
    file = ''
    Light_Base_Load_Config_File, LightVars, file, path=LightVars.Global.ConfigDir
    if strlen(file_which(file, /include_current_dir)) ne 0 or file_test(file, /regular) ne 0 then $
      LightVars.Global.ConfigFile = file
  endelse

end ; PRO Light_Event_Base_Load_Instr_File

;*****************************************
; load instrument configuration file (when SU mode)
pro Light_Event_Base_Save_Instr_File, Lightvars

  if LightVars.Private.IsSuperUser eq 0 then begin
    Light_LogMessage, LightVars, 'normal','[w] Only SuperUser can save new instrument configuration files'
    return
  endif else begin
    file = dialog_pickfile(Dialog_Parent=LightVars.Handles.Base, $
            file = LightVars.Global.ConfigFile, filter='*.ini', $
            path = LightVars.Global.ConfigDir, $
            Get_Path=Get_Path, Title='Save Instrument Config File to')

    if strlen(file) eq 0 then return  ; canceled

    ;** display warning if file name was re-used
    if strlen(file_which(file, /include_current_dir)) ne 0 or file_test(file) eq 1 then begin
      Button = dialog_message([ 'File '+file+' already exists.', $
                                'Do you want to replace it ?' ], /Question, Title='Replace '+file+' ?', /default_cancel)
      if not strcmp(Button,'Yes') then return
      Light_LogMessage, LightVars, 'normal' , '[w] '+file+' will be over-written.'
    endif
    LightVars.Global.ConfigFile = file
    LightVars.Global.ConfigDir  = Get_Path
    Light_Base_Save_Config_File, LightVars, file=file, /instrument, /displays, /config, comment='Created in Light SuperUser mode'
  endelse

end ; PRO Light_Event_Base_Save_Instr_File

;***************************************************************
;** Save Config_File (Inifile)
pro Light_Event_Base_Save_Preferences, LightVars

  Light_Base_Save_Config_File, LightVars, file=LightVars.Global.IniFile, /config, /runs, /displays, comment = 'User Preferences file'

end ; PRO Light_Event_Base_Save_Preferences

;***************************************************************
;** Display/Change Light Preferences
pro Light_Event_Base_Edit_Preferences, LightVars

  Preferences = {Light_Preferences, $
            Verbosity:LightVars.Global.Verbosity,$
            TerminalEcho:LightVars.Global.TerminalEcho,$
            LogFile:LightVars.Global.LogFile,$
            IniFile:LightVars.Global.IniFile,$
            ConfigDir:LightVars.Global.ConfigDir,$
            ConfigFile:LightVars.Global.ConfigFile, $
            Simulation:LightVars.Global.Simulation, $
            Append_Runs:LightVars.Global.Append_Runs, $
            Plot:LightVars.Plot, $
            Frequency:LightVars.Frequency, $
            Generic:LightVars.Generic }

  Light_LogMessage, LightVars, 'verbose','[I] You will find help in menu Help:Preferences'
  Light_Event_Base_Help_Preferences, LightVars, update=1


  Dialog_Fields, Preferences, Group=LightVars.Handles.Base, COLUMN_WIDTHS=500, $
    Name='Please enter new Light Preferences', Title='Preferences in Light', $
    y_scroll_size=n_elements(tag_names(Preferences)), $
    FieldNames = [ 'Verbosity', 'IDL Echo', 'LogFile', 'IniFile', 'ConfigDir', 'ConfigFile','Simulation','Append Runs','History size','Frequency','Control Program']

  LevelList      = ['silent','normal','verbose','debug']
  tmp1 = where(strcmp(LevelList, Preferences.Verbosity))
  if tmp1(0) ne -1 then $
    LightVars.Global.Verbosity = Preferences.Verbosity $
  else $
    Light_LogMessage, LightVars, 'normal','[w] Verbosity can be: silent, normal, verbose, debug'
  if LightVars.Private.IsSuperUser then begin
    if file_test(Preferences.ConfigDir, /directory) ne 0 then $
      LightVars.Global.ConfigDir = Preferences.ConfigDir $
    else begin
      Light_LogMessage, LightVars, 'normal','[w] ConfigDir must be a valid (configuration) directory'
      Light_LogMessage, LightVars, 'normal','[w] Check: ' + Preferences.ConfigDir
    endelse
    LightVars.Global.ConfigFile = Preferences.ConfigFile
  endif else begin
    if not strcmp(strtrim(LightVars.Global.ConfigDir,2), strtrim(Preferences.ConfigDir,2)) $
       or not strcmp(strtrim(LightVars.Global.ConfigFile,2), strtrim(Preferences.ConfigFile,2)) then begin
      Light_LogMessage, LightVars, 'silent','[w] ConfigDir/ConfigFile may only be changed in SuperUser mode.'
      Light_LogMessage, LightVars, 'silent','[w] Ask Local Contact or Instrument Responsible.'
    endif
  endelse
  LightVars.Global.IniFile      = Preferences.IniFile
  LightVars.Global.TerminalEcho = Preferences.TerminalEcho
  LightVars.Global.LogFile      = Preferences.LogFile
  LightVars.Global.Append_Runs  = Preferences.Append_Runs
  LightVars.Plot             = Preferences.Plot
  LightVars.Frequency        = Preferences.Frequency
  LightVars.Generic          = Preferences.Generic

  if long(Preferences.Simulation) eq 0 then LightVars.Global.Simulation = 0L else LightVars.Global.Simulation = 1L

  ;** Update Dial structure
  DialTag, 'light', tag='Frequency',set=LightVars.Frequency
  DialTag, 'light', tag='Plot',     set=LightVars.Plot
  DialTag, 'light', tag='Generic',  set=LightVars.Generic

end ; PRO Light_Event_Base_Edit_Preferences

;***************************************************************
;** Edit variables to be displayed by dial_display
pro Light_Event_Base_Edit_Displays, LightVars

  ToUpdate = [ 'Variables', 'Type', 'Options' ]

  Light_Event_Base_Help_Displays, LightVars, update=1

  for index = 0, n_elements(LightVars.Displays.Variables)+4 do begin
    if index ge n_elements(LightVars.Displays.Variables) then begin
      Display = {Variable:'',Type:'auto',Options:''}
    endif else begin
      Display = {Variable:LightVars.Displays.Variables(index),$
         Type:LightVars.Displays.Type(index), $
         Options:LightVars.Displays.Options(index)}
    endelse
    if index eq 0 then Displays = Display else Displays = [ Displays, Display ]
  endfor
  ; automatic call of Displays/Help if active

  ; now call Dialog_Fields to display the Displays settings
  Dialog_Fields, Displays, Group=LightVars.Handles.Base, COLUMN_WIDTHS=300, $
    Name='Please enter Displays specifications', Title='Displays in Light', $
    FieldNames = [ 'Variable', 'Type', 'Options'], /swap

  ; update displays and remove invalid entries
  StructIndex = -1

  Mad = LightVars.Mad
  t_res    = Mad.t_res
  t_para   = Mad.t_para
  t_nother = Mad.t_nother
  t_chopper= Mad.t_chopper
  t_status = Mad.t_status
  data     = Mad.Data

  Monitor  = data(0, *) & data = data(1:(t_para.tof_cha_resol-1),*) ; for IN6
  detectors= total(data,1)  ; Channels on X (length=340), sum of row
  tof      = total(data,2)  ; TOF on X (length=LightVars.Mad.t_para.tof_cha_resol = 512, 1024...)

  Temperatures = t_res.tempea
  TSet    = Temperatures(0)
  TReg    = Temperatures(1)
  TSample = Temperatures(2)
  Power   = Temperatures(3)
  Lambda  = t_para.wave

  for index = 0, n_elements(LightVars.Displays.Variables)+4 do begin
    Display = Displays(index)
    if strlen(Display.Variable) gt 0 then begin
      ok = execute('DataToDisplay='+Display.Variable)
      if not ok then Light_LogMessage, LightVars, 'normal' , '[w] Light: Displays: "'+Display.Variable+'" does not seem to be a valid value. May be inactivated when displayed.'
      
      StructIndex = StructIndex+1
      IsValidType = where(Display.Type eq ['text','plot','surface','image','contour'])
      IsValidType = IsValidType(0)
      if IsValidType eq -1 then Display.Type = ''
      if StructIndex eq 0 then begin
        Variables = Display.Variable
        Type      = Display.Type
        Options   = Display.Options
      endif else begin
        Variables = [ Variables, Display.Variable ]
        Type      = [ Type, Display.Type ]
        Options   = [ Options, Display.Options ]
      endelse
    endif
  endfor
  if StructIndex ge 0 then begin
    NewDisplays={Variables:Variables, Type:Type, Options:Options}
    Tools_StructSetValue, struct=LightVars, Tag='Displays', val=NewDisplays
    LightVars.Private.Update_Nb = 10
  endif

end ; PRO Light_Event_Base_Edit_Displays

;***************************************************************
; Define and retrieve ROI (through IDL XROI tool)
pro Light_Event_Base_Edit_ROI, LightVars, update=update
  
  FORWARD_FUNCTION	LookupManagedWidget

  ; redimension the cumulated Mad.data image so that it fits in screen (max is ScreenDim pixels)
  if n_elements(LightVars.Private.Data_Cumulated) ne n_elements(LightVars.Mad.data) then $
   DataImage = LightVars.Private.Data_Cumulated+LightVars.Mad.data $
  else DataImage = LightVars.Mad.data
  DataImage = double(DataImage)
  
  ScreenDim = 512.0
  data_dims = size(DataImage, /dimensions)
  if data_dims(0) eq 0 then return
  
  ; setting monitors to zero (IN6 dependent)
  DataImage((data_dims(0) -2):(data_dims(0) -1), *)     = 0 ; removes last channel (= integral)
  DataImage(*,0:(LightVars.Mad.t_res.N_MON_SPECT-1))  = 0 ; removes monitors

  max_dim   = max(data_dims)
  scaling   = round(max_dim/ScreenDim)
  data_dims = data_dims/scaling
  data      = 0
  
  ; call xroi if not present yet
  if xregistered('xroi', /noshow) eq 0 and n_elements(update) eq 0 then begin
    
    ; now build a new regions_in array of IDLgrROI from xyrange
    if size(LightVars.Private.ROIs, /type) eq 8 then begin  ; is a structure
      for index = 0,(n_elements(LightVars.Private.ROIs) - 1) do begin
        ThisROIstr = LightVars.Private.ROIs(index)
        ; now build the 4 vertex (rectangle corners), and adjust to shown image size (scaling)
        X = round([ ThisROIstr.xrange(0), ThisROIstr.xrange(0), ThisROIstr.xrange(1), ThisROIstr.xrange(1) ]/scaling)
        Y = round([ ThisROIstr.yrange(0), ThisROIstr.yrange(1), ThisROIstr.yrange(1), ThisROIstr.yrange(0) ]/scaling)
        ThisROIobj = obj_new('IDLgrROI', X, Y, $
          color=[0, 255, 255], NAME=ThisROIstr.name, style=ThisROIstr.style, $
          THICK=ThisROIstr.THICK, type=ThisROIstr.type)
        if index eq 0 then Regions = ThisROIobj $
        else Regions = [Regions, ThisROIobj]
      endfor
    endif
    title = '['+LightVars.Instrument.name+'] ROI on TOF-det (log)'
    title = title+' 1:'+strcompress(string(round(scaling*10)/10))
    data  = congrid(DataImage+1, data_dims(0), data_dims(1), /interp)
    Light_LogMessage, LightVars, 'verbose','[I] ROI data axes:X is TOF channel, Y is det. number'
    Light_LogMessage, LightVars, 'verbose',['# Cumulated Data is shown as log scale from 0 to 255', '# Color table is Hue Sat Value 2 (22)' ]
    xroi, data,  $
      regions_in = Regions, regions_out = Regions, $
      tools=['Rectangle','TRANSLATE-SCALE', 'Selection'], $
      title=title
    ROI_ID = LookupManagedWidget('xroi')
    widget_control, ROI_ID, get_uvalue=sState
    ; change the palette
    ; Blue-Red(11), EOS B(27), GREEN-PINK(10), Hue Sat Value 2(22),  RAINBOW(13), Purple-red + Stripes (23)
    ; STERN SPECIAL(15)
    (*sState).oPalette->LoadCT, 22
    (*sState).oImage->SetProperty, PALETTE=(*sState).oPalette
  endif

  if xregistered('xroi', /noshow) ne 0 then begin
    ROI_ID = LookupManagedWidget('xroi')
    
    ; show window (not in update mode), if was iconified, or hiden
    if n_elements(update) eq 0 then $
      widget_control, ROI_ID, show=1, iconify=0
      
    if n_elements(data) eq 1 then data  = congrid(DataImage+1, data_dims(0), data_dims(1), /interp)
    data = alog(data)
    data = data-min(data)
    data = data/max(data)*255
    
    ; now retrieve IDLgrROI objects
    widget_control, ROI_ID, get_uvalue=sState
    Private = LightVars.private
    a = (*sState).oROIModel

    ROIs = 0L
    if a->Count() gt 0 then begin
      for index = 0, (a->Count()-1) do begin
        ThisROIobj = a->Get(position=index)
        ThisROIobj->GetProperty, all=ThisROIstr
        ThisROIstr.xrange = round(ThisROIstr.xrange*scaling)  ; real indexes on full image
        ThisROIstr.yrange = round(ThisROIstr.yrange*scaling)
        if index eq 0 then ROIs = ThisROIstr $
        else ROIs = [ROIs, ThisROIstr]
      endfor

      Tools_StructSetValue, struct=LightVars,  Tag='Private.ROIs',val=ROIs
    endif else Tools_StructSetValue, struct=LightVars,  Tag='Private.ROIs',val=0L
    (*sState).oImage->SetProperty, data=data
    (*sState).oWindow->Draw, (*sState).oView
  endif

end ; PRO Light_Event_Base_Edit_ROI

;***************************************************************
pro Light_Event_Base_Edit_Header, LightVars

  ;** execute Custom procedure
  res = execute(LightVars.Instrument.CustomPro+', /edit_header')

end ; PRO Light_Event_Base_Edit_Header

;***************************************************************
;** Show MAD log (recent lines)
pro Light_Event_Base_Show_MAD_Log, LightVars

  Private = LightVars.Private
  ToDisplay= DialNewValue(type='log')
  MadLog = Private.MadLogText
  MadLog = [ MadLog, ToDisplay ]
  n_lines = n_elements(MadLog)
  min_index=max([0, n_lines-1000])
  MadLog = MadLog(min_index:(n_lines-1))
  if min_index ne 0 then MadLog = [ 'Last 1000 lines of MAD Log ['+systime(0)+']', MadLog ]
  Tools_StructSetValue, struct=LightVars, Tag='Private.MadLogText', val=MadLog

  widget_control, LightVars.Handles.Base_Mad_Log, set_value=MadLog, Bad_ID=Bad_ID, show=1, iconify=0
  if Bad_ID ne 0 or LightVars.Handles.Base_Mad_Log eq 0 then  begin
    xdisplayfile, 'mad_light.log', text=MadLog, Group=LightVars.Handles.Base, $
          title=strupcase(LightVars.Generic)+' Log (tail of mad.log)',$
          Done_Button='Close ['+strupcase(LightVars.Generic)+' Log]', WText=ID, /editable
    LightVars.Handles.Base_Mad_Log = ID
  endif


end; Light_Event_Base_Show_MAD_Log

;***************************************************************
;** Show Light log (of session)
pro Light_Event_Base_Show_Light_Log, LightVars

  ToDisplay= LightVars.Private.LogText
  if n_elements(ToDisplay) ge 1 and strlen(ToDisplay(0)) gt 0 then begin
    widget_control, LightVars.Handles.Base_Light_Log, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0
    if Bad_ID ne 0 or LightVars.Handles.Base_Light_Log eq 0 then  begin
      Title = 'Light Log'
      if (strlen(LightVars.Global.LogFile) ne 0) then Title = Title+' ('+LightVars.Global.LogFile+')'
      xdisplayfile, 'light_light.log', text=ToDisplay, Group=LightVars.Handles.Base, $
            title=Title,$
            Done_Button='Close [Light Log]', WText=ID, /editable
      LightVars.Handles.Base_Light_Log = ID
    endif
  endif

end; Light_Event_Base_Show_Light_Log

;************************* Help  Events ************************
;***************************************************************
;** Display information about Light
pro Light_Event_Base_Help_About, LightVars

  Mem = memory(/current)
  ToDisplay = ['This is Light for George.',$
               '', $
               'Running on:  '+strupcase(LightVars.Instrument.Name), $
               'Version:     '+LightVars.Private.Version, $
               'Author:      '+LightVars.Private.Author,$
               'Active Run:  '+string(LightVars.Global.Current_Run), $
               'Memory used: '+strtrim(Mem/1024,2)+' ko',$
               'Date:        '+systime(0), '', $
               'Contributions: D. Richard, ILL for Dial_ByGeorge; A. Schober for Light 1.0', $
               'License: This software may be used, copied, or redistributed as long as it is ', $
               ' not sold and this copyright notice is reproduced on each copy made. This ', $
               ' routine is provided as is without any express or implied warranties whatsoever.' ]

  widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='About Light'
  if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 then  begin
    xdisplayfile, 'aboutlight.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
          title='About Light on ['+strupcase(LightVars.Instrument.Name)+']',$
          Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID

    LightVars.Handles.Base_Help = ID
  endif

end ; PRO Light_Event_Base_Help_About

;***************************************************************
;** Display information about Light/File menu
pro Light_Event_Base_Help_File, LightVars

  ToDisplay =['FILE menu Help in "Light on George"',$
              '',$
              'This menu gathers operations dealing with files, input and output', '', $
              'Edit Light Preferences',$
              '   set simulation mode, directories, configuration files, verbosity...', $
              'Save Light Preferences',$
              '   save Preferences (also done at Exit)', $
              'Show Light Log File',$
              '   display the "Light on George" Log file (errors, events, etc...)',$
              'Show '+strupcase(LightVars.Generic)+' Log File',$
              '   display the '+strupcase(LightVars.Generic)+' Log file (instrument commands, counts, errors, etc...)',$
              'Restart Light',$
              '   restart Light and read configuration files.',$
              'Exit',$
              '   save Preferences, exit Light program, close associated IDL windows']

  widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='File Help in Light'
  if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 then  begin
    xdisplayfile, 'file.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
          title='File Help in Light', /editable, $
          Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
    LightVars.Handles.Base_Help = ID
  endif

end ; PRO Light_Event_Base_Help_File

;***************************************************************
;** Display information about Light/Setup menu
pro Light_Event_Base_Help_Setup, LightVars

  ToDisplay =['SETUP menu Help in "Light on George"',$
              '',$
              'This menu enables to customise the configuration of the instrument/experiment', '', $
              'Refer to the specific Help menu of the Light:Setup:Instrument window for', $
              'additional information', $
              '', $
              'Displays', $
              '   set the displays to show in the Display Panel. See dedicated Help on that topic.', $
              'Regions of Interest (ROIs)', $
              '   edit/define ROIs on which to perform accuracy analysis (see below), using', $
              '   the cumulated signal (for repeated countings). In order to be efficient', $
              '   each ROI should contain, as far as possible, isolated peaks or features', $
              '   that are thought to be of particular interest to the physicist.', $
              'Analyse ROIs', $
              '   basic statistics are performed for each defined ROI. This includes the computation', $
              '   of the noise/signal ratio, the normalised Chi squared corresponding to a fit', $
              '   using a single Gaussian peak over a background parabola, and an estimate of the ', $
              '   deduced relative uncertainty for this simple model. When all these values are', $
              '   better (lower) than the "Relative Error" parameter of the Run Options, it is', $
              '   assumed that a reasonable statistics has been achieved. Then, if the "Count on ', $
              '   Statistics" Run Option is active, the counting is terminated, and goes-on with ', $
              '   the next run in the sequence.', $
              'SuperUser Login',$
              '   ask the SuperUser password. Close the dialog to log-out from SuperUser Mode.', $
              'Control Mode {Active|Simulation}', $
              '   display/toggle the current Instrument Control Mode', $
              'Load Configuration', $
              '   loads a Light configuration file (extended preferences)', $
              'Save Configuration', $
              '   saves a Light configuration file (extended preferences)' ]

  widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Setup Help in Light'
  if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 then  begin
    xdisplayfile, 'setup.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
          title='Setup Help in Light', /editable, $
          Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
    LightVars.Handles.Base_Help = ID
  endif

end ; PRO Light_Event_Base_Help_Setup

;***************************************************************
;** Display information about Light/Setup menu
pro Light_Event_Base_Help_Run_Start, LightVars

  ToDisplay =['START/RUN menu Help in "Light on George"',$
              '',$
              'This menu gathers operations concerning how to execute experiment runs/scans', $
              'Each run is an acquisition period at a given temeperature. You may repeat runs',$
              'and give them descriptive tiles (facilitate identification of files). Some', $
              'options concerning the automatic checks/tasks during runs may be activated.', $
              '', $
              'Load Run/Sequence (XBU)',$
              '   load a Run/Sequence file and display its contents (in Exp. Runs section)',$
              'Save Run/Sequence (XBU)',$
              '   save the current Run/Sequence as an XBU file',$
              'Analyse/Show', $
              '   display a time analysis of Runs as a table', $
              'Run Options',$
              '   set temperature control and options, as well as',$
              '   Diagnosis and Count On Statistics parameters',$
              'Clear All Runs',$
              '   remove all Run definitions but the first',$
              'Delete Current Run',$
              '   remove current Run definition from the Sequence',$
              'Insert', $
              '   duplicate the current Run in the Sequence', $
              'Next Run (go to)', $
              '   display next Run of the Sequence', $
              'Start Single Run',$
              '   start only the current displayed Run',$
              'Start Sequence',$
              '   start the whole run sequence (from Run #0 to the last Run)', $
              'Stop Run/Sequence', $
              '   stop current Run/Sequence']

  widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Start/Run Help in Light'
  if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 then  begin
    xdisplayfile, 'run.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
          title='Start/Run Help in Light', /editable, $
          Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
    LightVars.Handles.Base_Help = ID
  endif

end ; PRO Light_Event_Base_Help_Run_Start

;***************************************************************
;** Display information about Light/Preferences
pro Light_Event_Base_Help_Preferences, LightVars, update=update

  ToDisplay =['PREFERENCES menu Help in "Light on George"',$
              '',$
              'This menu enables to change some internal settings controling the execution of Light',$
              'Verbosity=<level>',$
              '   affects the amount of messages displayed by Light.',$
              '   values can be: silent, normal, verbose, debug',$
              'IDL Echo=<0 or 1>', $
              '   will display Light message in IDL promp window when true.', $
              'LogFile=<filename or empty>',$
              '   When non-empty, sets the name of the Light Log file, where messages are written.', $
              'IniFile=<filename>',$
              '   the name of the initialise file, that Light reads on start-up.',$
              '   it contains preferences and other stuff. e.g. Light.ini',$
              'ConfigDir=<dir>',$
              '   the directory where instrument configuration files are.',$
              '   e.g. /home/cs/lambda/CALIBRATION/<inst>/',$
              'ConfigFile=<filename>',$
              '   the name of the file containing instrument parameters.',$
              '   e.g. Config.ini',$
              '   ConfigDir and ConfigFile may only be changed in SuperUser mode',$
              'Simulation=<0 or 1>',$
              '   sets the Instrument Control mode to "simulation" when true',$
              '   unactivate calls and commands to the instrument control program '+strupcase(LightVars.Generic), $
              'Append Runs=<0 or 1>',$
              '   will append new loaded run file contents to the existing sequence if true', $
              'History size=<-2,0,3:100>', $
              '   sets the number of counts values to remember and plot in George', $
              'Frequency', $
              '   sets the update frequency of Light. A Major Update (displays) takes place every 10 updates.', $
              'Control Program', $
              '   sets the name of the program to control the instrument' $
              ]

  widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Preferences Help in Light'
  if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 and n_elements(update) eq 0 then  begin
    xdisplayfile, 'preferences.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
          title='Preferences Help in Light', /editable,$
          Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
    LightVars.Handles.Base_Help = ID
  endif

end ; PRO Light_Event_Base_Help_Preferences

;***************************************************************
;** Display information about Light/Displays
pro Light_Event_Base_Help_Displays, LightVars, update=update

  DisplayHelp = ''
  if xregistered('Dial_Display_Interface') ne 0 then $
    Display, DisplayHelp=DisplayHelp, /unactivate

  ToDisplay =['Displays menu Help in "Light on George"',$
              '',$
              'This menu enables to change/modify the definition of variables to monitor',$
              'Variable=[expression]',$
              '   defines the expression to be evaluated and displayed (Data)',$
              '   variables may be of type text (string array), scalar (single numerical values)', $
              '   numerical vector, and numerical image/map', $
              '   a single numerical value will be appended (catenate)', $
              'Type=<text|plot|surface|image|contour>', $
              '   defines the type of display to use', $
              '   default is to use a text, vector or surface display, depending on the Variable type', $
              '   but other displays may be chosen for images/maps, and it is also possible', $
              DisplayHelp, $
              'Ex:Variable          Type    Options', $
              '   memory(/current)                                                    (displays IDL memory usage with time)', $
              '   Mad.Temperature   surface Name_IX="Time", Name_IY="Temp", /catenate (record Temperatures vs time)', $
              '   Mad.Data,         image   Name_IX="Channels", Name_IX="TOF"         (displays all detectors)' $
              ]

  widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Displays Help in Light'
  if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 and n_elements(update) eq 0 then  begin
    xdisplayfile, 'displays.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
          title='Displays Help in Light', /editable, $
          Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
    LightVars.Handles.Base_Help = ID

  endif

end ; PRO Light_Event_Base_Help_Displays

;***************************************************************
;** Display information about Light/Displays
pro Light_Event_Base_Help_Run_Options, LightVars, update=update

  DisplayHelp = ''

  ToDisplay =['Run Options menu Help in "Light on George"',$
              '',$
              'This window enables to customize the way runs and sequences are executed', '', $
              '[Temperature control]', $
              '   Checks that temperature remains within specified limits. If temperature has', $
              '   gone outside limits for [wait on T Error] minutes:', $
              '     A warning is generated', $
              '     A new counting is started. The numor is thus incremented, keeping other run', $
              '       parameters unchanged (temporary counting, with saving). These countings', $
              '       may be repeated as long as the temperature is not stabilised', $
              '     When temperature is recovered and stable, the remaining sequence time is ', $
              '       rescalled, so that the total execution time, including lost time, still', $
              '       remains close to the original execution time.', $
              '   During temperature changes, the sequence suspends for [wait on Reach T]', $
              '   before starting to count.', $
              '   NOTE: the [wait on T Error] value should be higher than the [wait on Reach T] one', $
              '[Count on Statistics]', $
              '   basic statistics are performed for each defined ROI. This includes the computation', $
              '   of the noise/signal ratio, the normalised Chi squared corresponding to a fit', $
              '   using a single Gaussian peak over a background parabola, and an estimate of the ', $
              '   worse deduced relative uncertainty for this simple model (6 parameters). When all', $
              '   these values are better (lower) than the "Relative Error" parameter, it is', $
              '   assumed that a reasonable statistics has been achieved. Then, if the "Count on ', $
              '   Statistics" is active, the current counting is terminated, and goes-on with ', $
              '   the next run in the sequence.', $
              '[Optional Command]', $
              '   This line may contain a command to be executed when starting the run.', $
              '   It is sent to the Instrument by default, or usage may be specified with words', $
              '   "idl:", "system:", "mad:". Valid examples follow:', $
              '     idl: d=indgen(10) & plot, d', $
              '     system: ls ; nedit', $
              '     mad: par chop' $
              ]

  widget_control, LightVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Run Options Help in Light'
  if Bad_ID ne 0 or LightVars.Handles.Base_Help eq 0 and n_elements(update) eq 0 then  begin
    xdisplayfile, 'runoptions.hlp', text=ToDisplay, Group=LightVars.Handles.Base, $
          title='Run Options Help in Light', /editable, $
          Height=20,Width=80,Done_Button='Close [Light Help]', WText=ID
    LightVars.Handles.Base_Help = ID
  endif

end ; PRO Light_Event_Base_Help_Run_Options

;*********************** RUN EVENTS ****************************
;***************************************************************
;** Show/Analyse Run sequence
pro Light_Event_Base_Run_Analyse, LightVars

  Light_Base_Get_Fields_ToDisplay, LightVars, index=0, $
        header=header, h_val=h_val, spy=spy, s_val=s_val, run=run, r_val=r_val, check=check, c_val=c_val

  Titles    = ['Run NO', 'Title','Dur*Rept +','Max Wait', 'Targ Temp', 'Lambda', $
               'Preset', 'Check T', 'Save', 'Diagnosis', 'Count/Stat'  ]

  IsSamePresetType = 1
  TotalTime = 0
  TotalWait = 0
  for index=0, n_elements(LightVars.Run.Temperatures)-1 do begin
    if LightVars.Run.Flag_MonTi(0) ne LightVars.Run.Flag_MonTi(index) then IsSamePresetType=0
    RunNO = 'Run_'+strcompress(index,/remove_all)
    DuRep = LightVars.Run.Preset(index)*LightVars.Run.Repetition(index)
    MaxWt = LightVars.Run.Wait_T(index)
    if index eq LightVars.Private.OnGoing_Run_Nb then RunNO = '> '+RunNO $
    else if index eq LightVars.Global.Current_Run then RunNO = '* '+RunNO
    if LightVars.Run.Flag_T(index)     then TChek = 'Yes' else TChek = 'no'
    if LightVars.Run.Flag_Save(index)  then SaveP = 'Yes' else SaveP = 'no'
    if LightVars.Run.Flag_Diag(index)  then Diags = 'Yes' else Diags = 'no'
    if LightVars.Run.Flag_Count(index) then Count = 'Yes' else Count = 'no'
    if LightVars.Run.Flag_MonTi(index) then begin
      MonTi = 'Time'
      TotalTime = TotalTime+DuRep
    endif else MonTi = 'Monitor'
    TotalWait = TotalWait+MaxWt
    tmp1 = {RunNO:RunNO, $
          Title:LightVars.Run.Titles(index), $
          DuRep:DuRep, $
          MaxWt:MaxWt, $
          TargT:LightVars.Run.Temperatures(index),$
          Lambda:LightVars.Run.Lambda(index),$
          MonTi:MonTi, $
          TChek:TChek, $
          SaveP:SaveP, $
          Diags:Diags, $
          Count:Count }

    if index eq 0 then ToDisplay = tmp1 else ToDisplay = [ ToDisplay, tmp1]
  endfor
  if IsSamePresetType then $
    Name = 'Total Time ='+string(TotalTime)+' [min]' $
  else $
    Name = 'Mixed Monitor/Time runs'
  Name  = Name + '. Temperature Wait Time ='+ string(TotalWait)+' [min]'
  Title = 'Run/Sequence 0-'+strcompress(n_elements(LightVars.Run.Temperatures)-1)+' ['+systime(0)+']'

  ID = LightVars.Handles.Base_Run_Analyse
  Dialog_Fields, ToDisplay, Group=LightVars.Handles.Base, COLUMN_WIDTHS=100, $
    x_scroll_size=n_elements(tag_names(ToDisplay(0))), $
    y_scroll_size=n_elements(ToDisplay), $
    Name=Name, Title=Title, $
    FieldNames=Titles(0:(n_elements(tag_names(tmp1))-1)), $
    /swap,/display, ID=ID
  LightVars.Handles.Base_Run_Analyse = ID
end ; PRO Light_Event_Base_Run_Analyse

;***************************************************************
;** Delete Run definitions
pro Light_Event_Base_Run_Delete, LightVars, Run_Nb=Run_Nb

  ; delete active run if not given as parameter
  if n_elements(Run_Nb) eq 0 then $
    Run_Nb = LightVars.Global.Current_Run

  if n_elements(Run_Nb) gt 1 then begin
    ; delete each Run Number that is in the Run_Nb vector.
    for index=n_elements(Run_Nb)-1, 0, -1 do $
      Light_Event_Base_Run_Delete, LightVars, Run_Nb=Run_Nb(index)

  endif else begin
    ; delete a single Run
    Runs         = LightVars.Run
    Run_TagNames = tag_names(Runs)
    Run_Nelements= n_elements(Runs.Temperatures)-1
    if Run_Nelements eq 0 then begin
      Light_LogMessage, LightVars, 'verbose' , '[w] I must keep at least one Run element.'
      return
    endif
    if Run_Nb lt 0 then Run_Nb = 0
    if Run_Nb ge Run_Nelements then Run_Nb = Run_Nelements
    for index=0,n_elements(Run_TagNames)-1 do begin
      ; for each Run variable, make a copy of array when removing the element Run_Nb
      Run_Array = Runs.(index)
      if Run_Nb eq 0 then New_Array = Run_Array(1:Run_Nelements) $
      else begin
        if Run_Nb eq Run_Nelements then $
          New_Array = Run_Array(0:(Run_Nelements-1)) $
        else New_Array = [ Run_Array(0:(Run_Nb-1)), Run_Array((Run_Nb+1):Run_Nelements) ]
      endelse
      Tools_StructSetValue, struct=LightVars, Tag='Run.'+Run_TagNames(index), val=New_Array
    endfor
    Light_Event_Base_Run_Fields, LightVars, /restore

  endelse

end ; PRO Light_Event_Base_Run_Delete

;***************************************************************
;** Clear All Run definitions
pro Light_Event_Base_Run_Clear_All, LightVars

  ; clear all Run def arrays. Keep current one as the first.
  ; make a loop with Base_Run_Delete

  tmp1 = n_elements(LightVars.Run.Temperatures)-1
  if tmp1 gt 0 then begin
    ; Close analyse window
    widget_control, LightVars.Handles.Base_Run_Analyse, Bad_Id=Bad_Id, /destroy
    LightVars.Handles.Base_Run_Analyse = 0L
    ; close all
    Run_Nb = indgen(tmp1+1)
    Light_Event_Base_Run_Delete, LightVars, Run_Nb=Run_Nb
    Light_LogMessage, LightVars, 'verbose' , '[ ] Deleting '+string(tmp1+1)+' Run elements.'
  endif

  Light_Event_Base_Run_Fields, LightVars, /restore

end ; PRO Light_Event_Base_Run_Clear_All

;***************************************************************
;** Insert a new Run definition, and shift furthers (if any)
pro Light_Event_Base_Run_Insert, LightVars, Run_Nb=Run_Nb

  ; insert at active run if not given as parameter
  if n_elements(Run_Nb) eq 0 then $
    Run_Nb = LightVars.Global.Current_Run

  ; insert a new run in place of the current one, and duplicate its contents
  Runs         = LightVars.Run
  Run_TagNames = tag_names(Runs)
  Run_Nelements= n_elements(Runs.Temperatures)-1

  if Run_Nb lt 0 then Run_Nb = 0
  if Run_Nb ge Run_Nelements then Run_Nb = Run_Nelements
  for index=0,n_elements(Run_TagNames)-1 do begin
    ; for each Run variable, make a copy of array when removing the element Run_Nb
    Run_Array = Runs.(index)

    if Run_Nb eq Run_Nelements then $
      New_Array = [ Run_Array(0:Run_Nb), Run_Array(Run_Nb) ] $
    else New_Array = [ Run_Array(0:Run_Nb), Run_Array(Run_Nb), Run_Array((Run_Nb+1):Run_Nelements) ]

    Tools_StructSetValue, struct=LightVars, Tag='Run.'+Run_TagNames(index), val=New_Array
  endfor
  Light_Event_Base_Run_Fields, LightVars, /restore

end ; PRO Light_Event_Base_Run_Insert

;***************************************************************
;** Jump to next Run_Nb
pro Light_Event_Base_Run_Next, LightVars, Run_Nb=Run_Nb

  ; use active run if not given as parameter
  if n_elements(Run_Nb) eq 0 then $
    Run_Nb = LightVars.Global.Current_Run

  Light_Event_Base_Run_Fields, LightVars, Run_Nb=Run_Nb+1, /restore

end ; PRO Light_Event_Base_Run_Next

;***************************************************************
;** Change Run number from Pop-up:
pro Light_Event_Base_Run_Nb, LightVars, Run_Nb=Run_Nb

  widget_control, LightVars.Handles.Current_Run, SET_DROPLIST_SELECT=Run_Nb
  Light_Event_Base_Run_Fields, LightVars, Run_Nb=Run_Nb, /restore

end ; PRO Light_Event_Base_Run_Nb

;***************************************************************
;** Update Current Run definition (called when user changes the current Run settings on Base)
pro Light_Event_Base_Run_Fields, LightVars, Run_Nb=Run_Nb, restore=restore
; not keyword_set(restore): get from Widgets into LightVars.Run if restore not set
; keyword_set(restore): get from LightVars.Run into Widgets

  ; use active run if not given as parameter
  if n_elements(Run_Nb) eq 0 then $
    Run_Nb = LightVars.Global.Current_Run

  Runs         = LightVars.Run
  Run_TagNames = tag_names(Runs)
  Run_Nelements= n_elements(Runs.Temperatures)-1

  if Run_Nb lt 0 then Run_Nb = 0
  if Run_Nb gt Run_Nelements then Run_Nb = Run_Nelements
  LightVars.Global.Current_Run = Run_Nb

  ToUpdate = [ $
                  'Titles', 'Temperatures', 'Lambda', 'Preset', 'Repetition', $
                  'Wait_T', 'Wait_Error', 'dT_Low', 'dT_High', $
                  'Flag_T', 'Flag_Diag', 'Flag_Count', 'Flag_Save', 'Flag_MonTi', 'Flag_dT_AbsRel', $
                  'Option_Command', 'CountStatAccuracy' ]

  for index=0, n_elements(ToUpdate)-1 do begin
  ;** Update fields from ToUpdate, taking Widget Handle (ID) and its value...
    tmp1 = execute('Widget = LightVars.Handles.'+ToUpdate(index))

    if Widget ne 0 and widget_info(Widget, /valid_id) ne 0 then begin
      if keyword_set(restore) then begin
        tmp1 = execute('Value  = LightVars.Run.'+ToUpdate(index))
        Value = Value(Run_Nb)
        ; special IN6: force Lambda to actual value in non SU mode
        if LightVars.Private.IsSuperUser eq 0 and strcmp(ToUpdate(index), 'Lambda') ne 0 and LightVars.Private.Update_Nb gt 0 then Value = LightVars.Mad.t_para.wave
        if not strcmp(strupcase(strmid(ToUpdate(index),0,4)), 'FLAG_') then $
          Value = string(Value)
        widget_control, Widget, set_value=Value, Bad_Id=tmp1
      endif else begin
        widget_control, Widget, get_value=Value, Bad_Id=tmp1
        if tmp1 eq 0 then begin
          if strcmp(strupcase(ToUpdate(index)), 'TITLES') or strcmp(strupcase(ToUpdate(index)), 'OPTION_COMMAND') then begin
            Value = strsplit(Value(0),"'",/extract, /PRESERVE_NULL)
            Value = strjoin(Value,'"')
            tmp3 = execute('Value = '''+Value(0)+'''') 
          endif else Value = abs(Value)
          tmp1 = execute('LightVars.Run.'+ToUpdate(index)+'(Run_Nb) = Value(0)')
        endif
      endelse
    endif
  endfor
  Title = 'Options for Run '+strtrim(string(Run_Nb),2)
  widget_control, LightVars.Handles.Run_Options, tlb_set_title=Title, Bad_ID=Bad_ID
  
  if LightVars.Run.Wait_Error(Run_Nb) lt LightVars.Run.Wait_T(Run_Nb) then begin
    LightVars.Run.Wait_Error(Run_Nb) = LightVars.Run.Wait_T(Run_Nb)
    widget_control, LightVars.Handles.Wait_Error, set_value=LightVars.Run.Wait_Error(Run_Nb), Bad_Id=tmp1
  endif

  ;** Resize/change LightVars.Handles.Current_Run value and accessible elements...
  widget_control, LightVars.Handles.Current_Run, set_value=string(indgen(n_elements(LightVars.Run.Temperatures)), Format='(I3)')
  widget_control, LightVars.Handles.Current_Run, SET_DROPLIST_SELECT=Run_Nb

  widget_control, LightVars.Handles.Base_Run_Analyse, Bad_Id=Bad_Id
  if LightVars.Handles.Base_Run_Analyse ne 0 and Bad_Id eq 0 then $
    Light_Event_Base_Run_Analyse, LightVars

end ; PRO Light_Event_Base_Run_Fields

;***************************************************************
;** Define/display current Run Options (safety margins, etc...)
pro Light_Event_Base_Run_Options_Build, LightVars, Run_Nb=Run_Nb

  ; use active run if not given as parameter
  if n_elements(Run_Nb) eq 0 then $
    Run_Nb = LightVars.Global.Current_Run
    
  Light_Event_Base_Help_Run_Options, LightVars, update=1

  ;** Change title for existing Options window
  Title = 'Options for Run '+strtrim(string(Run_Nb),2)
  widget_control, LightVars.Handles.Run_Options, tlb_set_title=Title, Bad_ID=Bad_ID, show=1, iconify=0
  ;** If does not exist, make it
  if Bad_ID ne 0 or LightVars.Handles.Run_Options eq 0 then  begin

    LightVars.Handles.Run_Options = widget_base(/column, Group_Leader=LightVars.Handles.Base, Title=Title, frame=7)
    ;** Temperature Control and checks
    option_base    = widget_base(LightVars.Handles.Run_Options, /column)
    temp_base      = widget_base(option_base, /column, frame=3)
    tmp2           = widget_label(temp_base,  /align_left,      value='Temperature Control')
    temp_marg_base = widget_base(temp_base,                    /column, frame=1)
    tmp2           = widget_label(temp_marg_base, /align_left, value='Margins')
    val_base       = widget_base(temp_marg_base,               /row)
    labels         = ['Lower: ','Upper: ']
    LightVars.Handles.dT_Low        = cw_field(val_base, title='Lower: ', value=LightVars.Run.dT_Low(Run_Nb), $
                       xsize=8, /all_events, /floating, /column, uvalue='Light_Event_Base_Run_Fields, LightVars')
    LightVars.Handles.dT_High       = cw_field(val_base, title='Upper: ', value=LightVars.Run.dT_High(Run_Nb), $
                       xsize=8, /all_events, /floating, /column, uvalue='Light_Event_Base_Run_Fields, LightVars')
    LightVars.Handles.Flag_dT_AbsRel= cw_bgroup(val_base, label_top='Unit', ['[K]','[%]'], set_value=LightVars.Run.Flag_dT_AbsRel(Run_Nb), $
                        /exclusive, /row, uvalue='Light_Event_Base_Run_Fields, LightVars')
    val_base       = widget_base(temp_marg_base, /column)
    tmp1           = widget_base(temp_marg_base, /row)
    LightVars.Handles.Wait_T        = cw_field(tmp1, title='Max Wait ', value=fix(LightVars.Run.Wait_T(Run_Nb)), $
                       xsize=5, /all_events,  /floating, /row, uvalue='Light_Event_Base_Run_Fields, LightVars')
    tmp2           = widget_label(tmp1, value=' [min] on Reach T')
    tmp1           = widget_base(temp_marg_base, /row)
    LightVars.Handles.Wait_Error    = cw_field(tmp1, title='Max Wait ', value=fix(LightVars.Run.Wait_Error(Run_Nb)), $
                       xsize=5, /all_events,  /floating, /row, uvalue='Light_Event_Base_Run_Fields, LightVars')
    tmp2           = widget_label(tmp1, value=' [min] on T Error')

    ;** Run Diagnosis Flag and params
    temp_base      = widget_base(LightVars.Handles.Run_Options, /row, frame=3)
    LightVars.Handles.Flag_Diag      = cw_bgroup(temp_base, label_left='',/row,$
        [' '], /nonexclusive, uvalue='Light_Event_Base_Run_Fields, LightVars', set_value=LightVars.Run.Flag_Diag(Run_Nb))
    tmp2 = widget_label(temp_base, value='Run Diagnosis')

    ;** Count on Statistics Flag and params
    temp_base      = widget_base(LightVars.Handles.Run_Options, /row, frame=3)
    col1           = widget_base(temp_base, /column)
    col2           = widget_base(temp_base, /column)
    tmp1           = widget_base(col1, /row)
    LightVars.Handles.Flag_Count     = cw_bgroup(tmp1, label_left='',/row,$
        [' '], /nonexclusive, uvalue='Light_Event_Base_Run_Fields, LightVars', set_value=LightVars.Run.Flag_Count(Run_Nb))
    tmp2 = widget_label(tmp1, value='Count on Statistics')
    tmp2 = widget_button (col2, value=' Set ROIs ',            uvalue='Light_Event_Base_Edit_ROI, LightVars')   
    
    tmp1           = widget_base(col1, /row)
    LightVars.Handles.CountStatAccuracy = cw_field(tmp1, title='Rel. Err.', value=fix(LightVars.Run.CountStatAccuracy(Run_Nb)), $
                       xsize=5, /all_events,  /floating, /row, uvalue='Light_Event_Base_Run_Fields, LightVars')
    tmp2 = widget_label(tmp1, value='%')
    tmp2 = widget_button(col2, value='Show Stats',            uvalue='Light_Event_Base_Edit_ROI, LightVars, /update & Light_SubDial_Base_Check_ROI, LightVars, /show')  

    ;** Other options
    temp_base      = widget_base(LightVars.Handles.Run_Options, /column, frame=3)
    LightVars.Handles.Option_Command = cw_field(temp_base, title='Optional Command (idl, system, '+LightVars.Generic+'):', value=LightVars.Run.Option_Command(Run_Nb), $
                      /all_events, /column, uvalue='Light_Event_Base_Run_Fields, LightVars')

    widget_control, LightVars.Handles.Run_Options, /realize
    XManager, 'Base_Options', LightVars.Handles.Run_Options, Event_Handler='Light_Base_Event_Parser', $
        /just_reg, /no_block

  endif

end ; PRO Light_Event_Base_Run_Options_Build

;***************************************************************
;** Load an existing Run/Sequence file.
pro Light_Event_Base_Run_Load, LightVars

  file = dialog_pickfile(Dialog_Parent=LightVars.Handles.Base, $
          file=LightVars.Global.RunFile, filter='*.ini', $
          Get_Path=Selected_Path, /must_exist, Title='Load Run/Sequence File from')

  if strlen(file) eq 0 then return  ; canceled

  LightVars.Global.RunFile = file
  SaveLightVars = LightVars
  Light_Base_Load_Config_File, LightVars, file, contains='Run'
  Run_Nb = string(n_elements(LightVars.Run.Temperatures))
  if LightVars.Global.Append_Runs ne 0 then begin
    tags = tag_names(LightVars.Run)
    for index=0, n_elements(tags)-1 do begin
      ok = execute('val=[ SaveLightVars.Run.'+tags(index)+', LightVars.Run.'+tags(index)+' ]')
      Tools_StructSetValue, struct=LightVars, Tag='Run.'+tags(index), val=val
    endfor
    Light_LogMessage, LightVars, 'normal' , '[ ] Loading '+Run_Nb+' runs after current sequence (append).'
    Light_LogMessage, LightVars, 'normal' , '[ ] Now '+string(n_elements(LightVars.Run.Temperatures))+' runs.'
  endif else $
    Light_LogMessage, LightVars, 'normal' , '[ ] Loading '+Run_Nb+' runs as a new sequence.'
  Light_Event_Base_Run_Fields, LightVars, /restore

  ;** Show analyse window
  Light_Event_Base_Run_Analyse, LightVars

end ; PRO Light_Event_Base_Run_Load

;***************************************************************
;** Save Run/Sequence into a file
pro Light_Event_Base_Run_Save, LightVars

  file = dialog_pickfile(Dialog_Parent=LightVars.Handles.Base, $
            file=LightVars.Global.RunFile, filter='*.run', $
            Get_Path=Selected_Path, Title='Save Run/Sequence File to')

  if strlen(file) eq 0 then return  ; canceled

  LightVars.Global.RunFile = file
  ;** display warning if file name was re-used
  if strlen(file_which(file, /include_current_dir)) ne 0 or file_test(file) eq 1 then begin
    Button = dialog_message([ 'File '+file+' already exists.', $
                              'Do you want to replace it ?' ], /Question, Title='Replace '+file+' ?', /default_cancel)
    if not strcmp(Button,'Yes') then return
    Light_LogMessage, LightVars, 'normal' , '[w] '+file+' will be over-written.'
  endif
  Light_Base_Save_Config_File, LightVars, file=file, /runs, comment='Run/sequence description file'

end ; PRO Light_Event_Base_Run_Save

;***************************************************************
;** Stop any on going Run/Sequence, as well as any setup process
pro Light_Event_Base_Run_Stop, LightVars

  Light_LogMessage, LightVars, 'silent','[I] STOP signal caught.'
  
  ; stop run/sequence
  widget_control, LightVars.Handles.RunStatus, set_uvalue='', set_value='', bad_id=tmp
  ;** execute Custom stop procedure
  res = execute(LightVars.Instrument.CustomPro+', /stop')

end ; PRO Light_Event_Base_Run_Save

;***************************************************************
;** Starts a Sequence, starting from first, until last
; if first = -1, starts at 0
; if last = -1,  ends with last run_nb
pro Light_Event_Base_Run_Start_Sequence, LightVars, first=first, last=last

  if n_elements(first) eq 0 then first = 0
  if n_elements(last) eq 0 then  last  = -1

  ; now look if we start settings...
  a = ''
  widget_control, LightVars.Handles.RunStatus, get_uvalue =a, get_value =b, bad_id=tmp

  if strlen(a) eq 0 then begin
    RunUValue = 'start'
    LightVars.Private.First_Run_Nb  = first
    LightVars.Private.Last_Run_Nb   = last
    LightVars.Private.OnGoing_Run_Nb= first
    
    widget_control, LightVars.Handles.RunStatus, set_uvalue = 'start', set_value='start', bad_id=tmp
  endif else Light_LogMessage, LightVars, 'normal','[w] Run/Sequence is already running ! (now at '+b+')'

end ; PRO Light_Event_Base_Run_Start_Sequence

;***************************************************************
;** Starts a Single Run (LightVars.Global.Current_Run)
pro Light_Event_Base_Run_Start_Run, LightVars

  n_run = LightVars.Global.Current_Run
  Light_Event_Base_Run_Start_Sequence, LightVars, first=n_run, last=n_run

end ; PRO Light_Event_Base_Run_Start_Sequence

;***************************************************************
; check all defined ROIs
; compute for each :
; 1-the signal/noise ratio normalized to pixel cell number
; 2-a gaussfit nterms=6 fit, getting sigmaa
; 3-abs(smoothed signal/signal)
; finish current counting if all criteria for all ROI are below specified accuracy
pro Light_SubDial_Base_Check_ROI, LightVars, show=show

  if LightVars.Private.OnGoing_Run_Nb ge 0 then Run_Nb = LightVars.Private.OnGoing_Run_Nb $
  else Run_Nb = LightVars.Global.Current_Run
  Accuracy = LightVars.Run.CountStatAccuracy(Run_Nb)/100
  
  ; will not perform accuracy analysis when Temperature is out limits
  ; LightVars.Private.LastValidNumor(0) = 0 normally (T is ok)
  ; LightVars.Private.LastValidNumor(0) = t_res.lasnum if Tout for Wait_Error
  ;  LightVars.Private.LastValidNumor(0) = -val if recovered T control
  if LightVars.Private.LastValidNumor(0) gt 0 then return 
  
  ; get all criteria for each region
  if size(LightVars.Private.ROIs, /type) eq 8 then begin  ; is a structure
    if n_elements(LightVars.Private.ROIs) gt 0 then begin
    
      DataImage = LightVars.Private.Data_Cumulated  ; use latest data set (updated in Collect)
  
      Criteria = fltarr(n_elements(LightVars.Private.ROIs))
      ToDisplay = ['Regions Of Interest (count statistics) on Cumulated Data', $
        'Date:              '+systime(0), $
        'Specified accuracy'+strcompress(string(Accuracy))+' for Run #'+strcompress(string(Run_Nb)), $
        'ROI_name Xmin Xmax Ymin Ymax   Noise/Signal  norm_ChiSq           dp/p' ]
      AccuracyNotAchieved = 0
      for index = 0,(n_elements(LightVars.Private.ROIs) - 1) do begin
        ThisROIstr = LightVars.Private.ROIs(index)
        ; now build the 4 vertex (rectangle corners), and adjust to shown image size (scaling)
        sData = size(DataImage, /dimensions)
        Xi =  ThisROIstr.xrange > 0 < sData(0)
        Yi =  ThisROIstr.yrange > 0 < sData(1)
        Data        = DataImage[ Xi(0):Xi(1), Yi(0):Yi(1)]
        ThisROItof  = total(Data, 2)+1
        S_over_N    = sqrt(n_elements(Data)/total(ThisROItof)) ; per pixel signal/noise (Poisson)

        ;ThisROItof  = ThisROItof/total(ThisROItof)                    ; normalised tof signal

        ; now fit a polynom and then a Gaussian+parabola: nt=6
        myexcept = !except
        !except = 0
        y = double(ThisROItof)
        errors = 1/sqrt(y)
        n = n_elements(y)
        x = double(lindgen(n))
        ; first try to fit with a 2nd order polynom
        poly_coefs = poly_fit(x, y, 2, chisq=poly_chisq, /double, $
          measure_errors=errors, sigma=poly_sigma, yfit=poly_yfit)
        n_param = 2
        poly_error = sqrt(total((y-poly_yfit)^2))/(n_elements(x)-n_param) ; normalised ChiSq per degree of freedom

        ; try gaussian+quad
        gauss_yfit = gaussfit(x,y,a, nterms=6)  ; guess parameters and compile GAUSS_FUNCT
        gauss_error= sqrt(total((y-gauss_yfit)^2))/(n_elements(x)-6)
        if gauss_error lt poly_error then begin
          ; Gauss+Poly is better than a single polynom. We estimate the sigma
          a = double(a)
          gauss_yfit = curvefit(x,y,errors,a,gauss_sigma, $
            function_name = "GAUSS_FUNCT", /double, iter=iter, chisq=chisq) ;call curvefit and estimate errors

          ; reject non-realistic parameter values
          if abs(a(0)) gt max(y)-min(y) or abs(a(0)) lt (max(y)-min(y))/100  then gauss_sigma(0) = 0 ; gaussian intensity
          if a(1) gt max(x) or a(1) lt min(x)             then gauss_sigma(1) = 0 ; gaussian center
          if abs(a(2)) gt max(x)-min(x) or abs(a(2)) lt 2 then gauss_sigma(2) = 0 ; gaussian width
          gauss_coefs = a

          ; use polynomial std dev if they were smaller
          tmp = where(abs(poly_sigma) lt abs(gauss_sigma(3:5)))
          if tmp(0) ge 0 then gauss_sigma(tmp+3) = poly_sigma(tmp)

          yfit    = gauss_yfit
          sigma   = gauss_sigma
          param   = gauss_coefs
          n_param = 6
        endif else begin
          ; 2nd order polynom is a good fit
          yfit    = poly_yfit
          sigma   = poly_sigma
          param   = poly_coefs
          n_param = 2
        endelse
        a = check_math()
        !except   = myexcept
        errors    = errors/total(errors)
        error     = sqrt(total(((y-yfit)/errors)^2))/(n_elements(x)-n_param)/total(y)

        ; now compute mean parameter relative error for sigma ne 0 
        tmp = where(sigma ne 0)
        if tmp(0) ne -1 then begin
          RelParamErr = abs(sigma/param)
          RelParamErr = RelParamErr(tmp) 
          RelParamErr = max(RelParamErr)
        endif else RelParamErr = 0

        name = ThisROIstr.name
        if strlen(name) gt 8 then name = name(0:7)

        ToDisplay = [ToDisplay, $
            string(name, format='(A8)') $
            +string(Xi(0), format='(I4)')+' '+string(Xi(1), format='(I4)')+' ' $
            +string(Yi(0), format='(I4)')+' '+string(Yi(1), format='(I4)')+' ' $
            +string(S_over_N)+' '+string(error)+' '+string(RelParamErr) ]

        if S_over_N gt Accuracy or error gt Accuracy $
          or RelParamErr gt Accuracy then AccuracyNotAchieved = 1

      endfor
      
      if AccuracyNotAchieved eq 0 then begin
        msg = 'Required accuracy for defined ROIs is now achieved on cumulated signal'
        ; reached required precision for all ROIs on signal -> command next count (if sequence is running)
        if LightVars.Private.AccuracyAchieved eq 0 or n_elements(show) ne 0 then begin
          Light_LogMessage, LightVars, 'normal', msg
          Light_LogMessage, LightVars, 'verbose', ToDisplay
          LightVars.Private.AccuracyAchieved = 1
        endif
        ToDisplay = [ ToDisplay, msg ]
      endif
      
      ; send statistics results to text window
      widget_control, LightVars.Handles.CountStatsText, set_value=ToDisplay, Bad_ID=Bad_ID

      if ((Bad_ID ne 0) or (LightVars.Handles.CountStatsText eq 0)) and n_elements(show) ne 0 then  begin
        xdisplayfile, 'countstats.txt', text=ToDisplay, Group=LightVars.Handles.Base, $
              title='Region of Interest analysis', /editable, $
              Height=15,Width=80,Done_Button='Close [ROI Stats]', WText=ID
        LightVars.Handles.CountStatsText = ID
      endif ; has 1 or more elements
    endif ; is structure
  endif else if n_elements(show) ne 0 then Light_LogMessage, LightVars, 'verbose', '[ ] No ROIs are defined. Please use menu Setup:Regions of Interest.'

end ; PRO Light_SubDial_Base_Check_ROI

;***************************************************************
; analyse last 10 temperature measures
; tells if temperature is within specified Run Options limits, or too high/low
; compute linear slope from Temp_History using linfit
; if Tout and AlarmTempTime eq 0 -> AlarmTempTime=now
; if Tin and AlarmTempTime ne 0 -> AlarmTempTime = 0, LastValidNumor=0 (temperature got back within limits)
; if Tout and now gt AlarmTempTime+Wait_Error (has been out for enough time) then
;   AlarmTempTime = 0 (remove alarm has it is handled here, a new alarm will be generated if required every Wait_T_error)
;   if slope is small then [set warning message]
;   if slope is high  then [set error message]
;   [stop, start 'wait T control counting' -> RunUValue='count', if LastValidNumor eq 0 -> = numor]
;
; when temperature change is requested, LightVars.Private.WaitTempTime ne 0 

pro Light_SubDial_Base_Check_Temperature, LightVars
  
  ; get current run or displayed one (active)
  if LightVars.Private.OnGoing_Run_Nb ge 0 then Run_Nb = LightVars.Private.OnGoing_Run_Nb $
  else Run_Nb = LightVars.Global.Current_Run
  
  ; determine temperature high/low limits in absolute [K] units
  dTLow  = abs(LightVars.Run.dT_Low(Run_Nb))
  dTHigh = abs(LightVars.Run.dT_High(Run_Nb))
  
  if LightVars.Run.Flag_dT_AbsRel(Run_Nb) ne 0 then begin ; % of TSet
    dTLow = dTlow*LightVars.Mad.t_res.tempea(0)/100
    dTHigh= dTHigh*LightVars.Mad.t_res.tempea(0)/100
  endif
  
  ; get set-point and regulation temperature
  TReg = LightVars.Mad.t_res.tempea(1)
  TSet = LightVars.Mad.t_res.tempea(0)
  
  ; update Temp_History
  History_length = 10
  Temp_History = Lightvars.Private.Temp_History
  if n_elements(Temp_History) eq 1 then Temp_History = [ TReg, TReg ] $ ; starting
  else Temp_History = [Temp_History, TReg]
  max_index = n_elements(Temp_History)-1
  min_index = max([0, max_index-History_length+1])
  Temp_History = Temp_History[min_index:max_index]
  Tools_StructSetValue, struct=LightVars, tag='Private.Temp_History', val=Temp_History
  
  if max_index le 2 then return
  
  ; compute mean-TSet value, stddev, as well as slope
  LightVars.Private.Temp_Stats(0) = mean(Temp_History) - TSet ; deviation from set Point
  LightVars.Private.Temp_Stats(1) = stddev(Temp_History)      ; fluctuations
  time_axis = lindgen(n_elements(Temp_History))*LightVars.Frequency*LightVars.Private.Major_Update_Nb
  lin_coefs = linfit(time_axis, Temp_History)
  LightVars.Private.Temp_Stats(2) = lin_coefs(1)*60              ; slope (K/min)
  
  ; Warning: Wait_T must be le Wait_Error 
  ; (else we generate temporary countings each time a T change is requested)
  base_time = max([ LightVars.Run.Wait_Error(Run_Nb), LightVars.Run.Wait_T(Run_Nb), 0.1])
  base_slope= abs(dTHigh+dTLow)/base_time/2         ; [K/min]
  
  dTLow  = TSet - dTLow
  dTHigh = TSet + dTHigh
  
  ; if T out and no AlarmTempTime -> AlarmTempTime=now
  ; this occurs when T deviates or T changes (wait time)
  if (dTHigh lt TReg or TReg lt dTLow) $
  and LightVars.Private.AlarmTempTime le 0 then begin
    LightVars.Private.AlarmTempTime = systime(1)  ; time when T gets out
    msg = '[ ] Regulation Temperature '+strcompress(string(TReg))+' [K] exits limits ['+strcompress(string(dTLow))+','+strcompress(string(dTHigh))+'] (defined by Sequence item'+strcompress(string(Run_Nb))+'). Slope '+strcompress(string(LightVars.Private.Temp_Stats(2)))+' [K/min]'
    Light_LogMessage, LightVars, 'verbose', msg
  endif
  
  ; if T in and AlarmTempTime ne 0 -> AlarmTempTime = 0, LastValidNumor=0 (temperature got back within limits)
  ; this ocurs when T change has succeeded (usually after Wait_T), or when T control is recovered within base_slope [K/s]
  if (dTLow lt TReg and TReg lt dTHigh) $
    and LightVars.Private.AlarmTempTime ne 0 $
    and abs(LightVars.Private.Temp_Stats(2)) lt base_slope then begin
    LightVars.Private.AlarmTempTime = 0
    msg = '[ ] Regulation Temperature '+strcompress(string(TReg))+' [K] gets back within limits ['+strcompress(string(dTLow))+','+strcompress(string(dTHigh))+'] (defined by Sequence item'+strcompress(string(Run_Nb))+')'
    Light_LogMessage, LightVars, 'verbose', msg
    if LightVars.Private.LastValidNumor(0) ge 0 then begin
      msg = '[ ] Next numor should be added to Last Valid numor '+strcompress(string(long(LightVars.Private.LastValidNumor(0))))+' that was interrupted when Temperature went out.'
      Light_LogMessage, LightVars, 'normal', msg
    endif
    if LightVars.Private.TempoTempTime ge 0 then begin
      LostTime = double(systime(1) - LightVars.Private.TempoTempTime)/60.0
      msg = '[ ] Lost '+strcompress(string(LostTime))+' [min] with unstable temperature'
      Light_LogMessage, LightVars, 'verbose', msg
      LightVars.Private.TempoTempTime = -LightVars.Private.TempoTempTime
    endif
    LightVars.Private.LastValidNumor(0)   = -abs(LightVars.Private.LastValidNumor(0))
    LightVars.Private.TemperatureUnstable = -1
    LightVars.Private.AlarmTempTime       = 0
    LightVars.Private.WaitTempTime        = 0 ; ends stabilisation if slope is ok
  endif
  
  ; if Tout and now gt AlarmTempTime+Wait_Error (has been out for enough time)
  ; this occurs when T is unstable, or T change can not be achieved
  if LightVars.Private.AlarmTempTime ne 0 $
      and (dTHigh lt TReg or TReg lt dTLow) $
      and systime(1) ge LightVars.Private.AlarmTempTime+base_time*60 then begin
    if LightVars.Private.LastValidNumor(0) le 0 then begin
      ; IN6: store valid numor, repetition number [numor, t_res.NREPET, t_res.NPREP, t_res.xc, t_res.jc]
      ; last counting with good T regulation. This unactivate cumulation of countings, until T recovers control
      LightVars.Private.LastValidNumor(0) = LightVars.Mad.t_res.lasnum
      LightVars.Private.LastValidNumor(1) = LightVars.Mad.t_res.NREPET
      LightVars.Private.LastValidNumor(2) = LightVars.Mad.t_res.NPREP
      LightVars.Private.LastValidNumor(3) = LightVars.Mad.t_res.xc
      LightVars.Private.LastValidNumor(4) = LightVars.Mad.t_res.jc
      msg = '[ ] Last Valid numor in the original sequence is '+strcompress(string(long(LightVars.Private.LastValidNumor(0))))
      Light_LogMessage, LightVars, 'normal', msg
    endif
    ;   if slope is small then [set warning message]
    ;   if slope is high  then [set error message]
    if abs(Lightvars.Private.Temp_Stats(2)) lt base_slope then begin
      Light_Base_Alert, LightVars, warning='Temperature unstable'
    endif else begin
      Light_Base_Alert, LightVars, error='Temperature error'
    endelse
    msg = '[ ] Temperature '+strcompress(string(TReg))+' [K] has been outside limits ['+strcompress(string(dTLow))+','+strcompress(string(dTHigh))+'] since '+systime(0, LightVars.Private.AlarmTempTime)+' Slope '+strcompress(string(LightVars.Private.Temp_Stats(2)))+' [K/min]' 
    Light_LogMessage, LightVars, 'normal', msg
    LightVars.Private.TemperatureUnstable = 1
    ; (remove alarm as it is handled here, a new alarm will be generated if required every Wait_T_error)
    LightVars.Private.AlarmTempTime = 0
    if LightVars.Private.TempoTempTime le 0 then LightVars.Private.TempoTempTime = systime(1)
  endif

end ; PRO Light_SubDial_Base_Check_Temperature


;***************************************************************
;*********************** End of Light internal routines ********
;***************************************************************

;*********************
PRO dial_light_macro, D
;*********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_light
;** This macro procedure is called by George every D.frequency seconds

    if D.init eq 0 then begin   ; first start
        ; test if Light is already active... This is done in 'dial_light'
        D.init = 1
    endif
    if D.init eq 1 then begin   ; normal start or restart
        Light_Base_Init, D  ; set user and pathes, and read config files
        D.init=2
    endif
    Light_Base_Update, D  ; create/update panel


    ; V=DialNewValue()    ; get value to display on Dial display
    ; DialModValue, V ,tag='VALUE'    ;(or D.value=V if dimensions are correct)
end



;*********************
FUNCTION dial_light
;*********************
;**
;** The dial initialisation (and stucture definition)

  if xregistered('Light_Base_Interface') ne 0 then begin
    LightVars = Light_Get_LightVars()
    Light_LogMessage, LightVars, 'verbose','[ ] Light is already running !'
    widget_control, LightVars.Handles.Base, Bad_ID=Bad_ID, show=1, iconify=0
    return, LightVars
  endif else begin

   ;Dial Variables (Defaulted if not present in return statement)
   ;--------------
    GENERIC='mad'    ;connect to the mad-idl interface
    TYPE='monitor'   ;then V=DialNewValue() stands for V=DialNewValue(TYPE='monitor')
    ONOFF=1          ;state of the Dial 1=running
    FREQUENCY=1.     ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used
    VALUE='Starting Light ' ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var.
    PLOT=100           ;-2=none 0=plot 1=surface 2=contour n>2 means show vector of last n scalar values
    UPPERLIM=0.      ;upper limit of the plot (LOWERLIM for lower limit)
    HISTORY=0        ;=1 to record values in file light.his
    Preset=0       ;if >0 then Dial is stopped after running Preset seconds
    WUPDATE=0        ;=1 to automaticaly update corresponding workspace, =-1 silent!
                     ;=2 to automaticaly update and plot workspace to the main window
                     ;   0,1,2 are set by pressing the left,middle,right mouse button on the dial snapshot
    INIT=0

   ;User Variables (Must be present in return statement to be available)
   ;-------------
    GLOBAL ={LIGHT_GLOBAL,$
            ConfigDir:'/home/cs/lambda/CALIBRATION/in6/',$
            Verbosity:'debug',$         ; information display verbosity level(silent, normal, verbose, debug)
            TerminalEcho:1L, $          ; if non 0 will output messages to IDL prompt window
            CurrentDir:'',$             ; starting directory
            IniFile:'Light.ini',$       ; contains Light init info
            ConfigFile:'Config.ini',$   ; contains info about Monochromators and Choppers
            LogFile:'Light.log',$       ; name of Light Log file
            RunFile:'Run.run',$         ; contains runs info
            Simulation:3L, $            ; 1:simulation mode (no action on instrument), 0:instrument control, 2: Mad active but in simulation mode, 3:use real MAD data
            Append_Runs:0L, $           ; 1: add new loaded runs after existing run sequence. 0: replace run sequence
            Current_Run:0L $            ; current run (displayed/running)
            }

    PRIVATE = {LIGHT_PRIVATE, $         ; these are only internal variables, not to be saved in .ini files
            LogText:'', $
            MadLogText:'Starting MAD Log recording at '+systime(0), $
            UserName:'unknown ',$
            LocalContact:'ILLstaff',$
            ExpTitle:'00-00-000 <sample><config><params> comments---', $
            Numor:   '99999                                         ', $
            SubTitle:'00-00-000 <sample><config><params> comments---', $
            MonitorSum:0., DetectorSum:0., TSample:0., TRegulation:0., $
            IsSuperUser:0L,$            ; set to true(1) for Restricted operations
            Version:'2.0.3 (Aug 08th 2002)',$
            Author:'E. Farhi, A. Schober, D. Richard. (c)ILL', $
            Base_Size:[0L, 0L], $
            Major_Update_Nb:10L, $
            Update_Nb:0L, $             ; this value is incremented, and every 10 steps, a Major Update is done
            OnGoing_Run_Nb:-1, $        ; # of running 'Run', -1 if inactive
            First_Run_Nb:0L, $          ; first run # in sequence
            Last_Run_Nb:-1L, $          ; last run # in sequence, -1 means go to end of sequence
            LastErrorMsg:'no alert', $
            WaitTempTime:0L, $          ; time of beginning of Wait temperature stabilisation
            AlarmTempTime:0L, $         ; time when T first gets out from limits
            TempoTempTime:0L, $         ; first time a temporary counting is requested
            AccuracyAchieved:0L, $      ; activated when statistics is ok for all ROIs on cumulated data, reset 
            ROIs  : 0L, $ ; list of last actualised {xrange, yrange} structures retrieved from XROI
            Data_Cumulated:0, $         ; cumulated data in same counting (repetition)
            Temp_History:0, $           ; last 10 regulation Temperature measures
            Temp_Stats:[0.,0.,0.], $; [mean-TSet, stddev, slope of Temp_History
            LastValidNumor:[0., 0., 0., 0., 0.], $ ; non 0 when T got out of limits, [numor, t_res.nrepet, t_res.nprep, t_res.xc, t_res.jc]
            TemperatureUnstable:0L, $   ; 0 means T is ok, 1 means out of limits since Wait_Error
            ErrorMsgToggle:1L}

    RUN = {LIGHT_RUN, Titles:['Run description'],$    ; all are arrays...
            Temperatures:[300.0],$
            Preset:[30.0],$
            Repetition:[1],$
            Lambda:[5.12], $
            Wait_T:[15.0],$            ; wait time between T changes
            Wait_Error:[15.0],$        ; wait time in case of T/scan error
            dT_Low:[1.0],$            ; lower check value for T stability
            dT_High:[1.0],$           ; higher check value for T stability
            Option_Command:[''], $  ; optional command to execute in Run. Can begin with: 'idl:','<generic>:','system:'. Default is 'idl:'
            CountStatAccuracy:[5.0], $
            Flag_T:[1L],$           ; flags for CheckT,RunDiag,CountStat,SavePerm,TShiftType(T/%)...
            Flag_Diag:[0L],$        ; use Diagnosis
            Flag_Count:[0L],$       ; Count on Statistics
            Flag_Save:[1L],$        ; save permanently
            Flag_MonTi:[1L],$       ; false/true for acquisition on Monitor/Time respect.
            Flag_dT_AbsRel:[0L] }   ; false/true for check DeltaT in Absolute/Relative}

    MAD     ={LIGHT_MAD, t_nother:0, t_res:0, t_para:0, t_status:0, t_counts:0, t_chopper:0, status:'<Starting>', data:0} ; will be re-dimensioned with Tools_StructSetValue

    HANDLES ={LIGHT_HANDLES, Base:0L,$
            Base_Mad_Status:0L, $ ; Base of MAD Status
            Mad_Status:0L, $ ; Text within Base of MAD Status
            Base_Alert_Status:0L, $   ; widget ID of Alert states (on top)
            MonitorSum:0L, DetectorSum:0L, TSample:0L, TRegulation:0L, $  ; widget IDs of Mon, Det, TSam, TReg
            UserName:0L, $      ; widget IDs of UserName, LocalContact, ExpN, ExpTitle
            LocalContact:0L, $
            ExpTitle:0L, $
            Current_Run:0L,$
            Titles:0L,$         ; widget IDs of Run Title, Temp, Preset, Repetition, Remaining time
            Temperatures:0L,$
            Preset:0L,$
            Repetition:0L,$
            Lambda:0L, $
            Wait_T:0L,$
            Wait_Error:0L,$
            dT_Low:0L,$
            dT_High:0L,$
            Flag_T:0L,$         ; widget IDs of Check temp, Run Diags, CountOnStat, SaverPerm
            Flag_Diag:0L,$
            Flag_Count:0L,$
            Flag_Save:0L,$
            Flag_MonTi:0L,$
            Flag_dT_AbsRel:0L,$
            CountStatAccuracy:0L, $
            RunFile:0L, $
            Base_Help:0L, $
            Base_Mad_Log:0L, $
            Base_Light_Log:0L, $
            Run_Options:0L, $
            Base_SuperUser:0L, $
            Option_Command:0L, $
            Base_Run_Analyse:0L, $
            Simulation:0L, $
            Numor:0L, $
            SubTitle:0L, $
            RunStartButton:[0L, 0L, 0L], $
            RunStatus:0L, $
            CountStatsText:0L, $
            LoadSaveCfg:[0L, 0L] }

    DISPLAYS = {LIGHT_DISPLAYS, $
            Variables:[''], $
            Type:[''], $
            Options:['']}

    ; should be loaded from Config.cfg (protected parameters) but here is a default config (just in case the file can not be found)
    INSTRUMENT = { Name:"", Type:"", CustomPro:'Light_Custom_IN6_Update, LightVars' }; custom Data block where are saved specific instrument data

    LightVars = {generic:GENERIC,type:TYPE,value:VALUE,frequency:FREQUENCY, OnOff:OnOff, init:INIT,plot:PLOT, global:GLOBAL,mad:MAD,handles:HANDLES, run:RUN, instrument:INSTRUMENT, displays:DISPLAYS, private:PRIVATE}

    return, LightVars
  endelse
end






;***************************************************************************************

;** Usefull calls to be used in procedure dial_dialexample_macro :
;** *************
;** V=DialNewValue([/SETVALUE],[COMMENT=txt]   ;Get a new value from DIAL_'generic'_READ
;**                [TYPE='monitor'])           (a request is made to the instrument)
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
;** DialModValue,   V ,[tag='VALUE']           ;Set the new value for current dial or
;** D.value   =     V                          ;modify yourself the tag Value if type &
;**                                            ;dimensions don't change.(same for Error)
;** D.upperlim=   150.                         ;Set upper limit for plotting.
;**
;** R=DialOn ()                                ;Return 0 if Dial has been interrupted
;**                                            (To use inside loops)
;** DialInit,      'template4',[NEW='tmp4']    ;Initiate dial  'template4' from file:
;**                           ,[PATH=path ]                dial_template4.pro
;**                                            (You may change its name to 'tmp4' and
;**                                            (use DialStart,'tmp4' to activate it)
;** DialMacro,     'template4'                 ;Force execution of DIAL_TEMPLATE4_MACRO
;**                                            ('template4'  is keept inactive, ONOFF=0)
;** DialClear,     'template4'                 ;Suppress dial  'template4' from memory
;** DialWSet                                   ;Reserve central draw window for next plot
;**
;** DialsFrequency,[GET=freq],[SET=.5],[/STOP] ;Set  or Get the general frequency value
;**                [Preset=90.] ,   [/START] ;              (time is in seconds)
;**                                            ;Stop or Start the general process
;**                                            ;Set  Time  limit for the active process

