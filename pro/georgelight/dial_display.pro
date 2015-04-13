;***************************************************************
;** Project: Light
;** File:    Light.pro
;** Version: 0.2
;** Date:    Jan, 23rd, 2002
;** Author:  E. Farhi
;** Object:  Dial_Display to display anything
;
;** Require: Tools_StructSetValue.pro, Dialog_Fields.pro
;
;** Modified Feb 28th 2002: use structure elements with numbered labels instead
;                           of array of structure (that MUST have all same size in IDL)
;** Modified Mar 7th 2002:  stand-alone version is now ok, except File/Edit Help
;** Modified Mar 11th 2002: automatic recording ok.
;** Modified Jul 24th 2002: corrected small bugs (/delete when no valid display), added group_leader
;
; USAGE EXAMPLE:
; dial_bygeorge,'display' & x=lindgen(10)+10 & y=exp(x)
; display, ix=x, data=y, name_data='exp10'
; display, data=dist(20), name_data='e2',type='surface'
;
;***************************************************************
; pro Dial_Display_LogMessage, DisplayVars, DisplayLevel, Message
; pro Dial_Display_Set_Index, Structure=Structure, Index=Index, Value=Value
; pro Dial_Display_Add_Data, DisplayVars, Display_Name=Display_Name, $
; pro Dial_Display_Delete_Data, DisplayVars, Display_Name=Display_Name, Display_Index=Display_Index, $
; pro Dial_Display_Delete_Display, DisplayVars, Display_Name=Display_Name, Display_Index=Display_Index
; pro Dial_Display_Get_List, DisplayVars, DataList=DataList, DataArr=DataArr, DisplayArr=DisplayArr
; pro Dial_Display_Build, DisplayVars
; pro Dial_Display_Event_Parser, event
; pro Dial_Display_Event_Close, DisplayVars
; pro Dial_Display_Event_Resize, DisplayVars
; pro Dial_Display_Event_Show_List, DisplayVars, no_raise=no_raise
; pro Dial_Display_Event_Help_Preferences, DisplayVars, update=update
; pro Dial_Display_Event_Help_File, DisplayVars
; pro Dial_Display_Event_Help_Edit, DisplayVars
; pro Dial_Display_Event_Displays_Help, DisplayVars, DisplayHelp=DisplayHelp
; pro Dial_Display_Event_Preferences, DisplayVars
; pro Dial_Display_Event_Update_All, DisplayVars
; pro Dial_Display_Event_Update, DisplayVars
; pro Dial_Display_Event_Save_Data_Image, DisplayVars, Display_Index=Display_Index, Data_Index=Data_Index, Type=Type, filename=filename, Name_Data=Name_Data
; pro Dial_Display_Event_Close_DisplayData, DisplayVars
; pro Dial_Display_Event_Save_Data_Text, DisplayVars
; pro Dial_Display_Event_Save_Data_PS, DisplayVars
; pro Dial_Display_Event_Delete_All, DisplayVars
; pro Dial_Display_Event_AutoSave, DisplayVars
; pro Dial_Display_Event_AutoUpdate, DisplayVars
; pro Display, Display_Name=Display_Name,
;
; function Dial_Display_Get_DisplayVars
; function Dial_Display_Get_Index, Structure, Index
; function Dial_Display_Get_Display, DisplayVars, Display_Name
; function Dial_Display_Get_Data, DisplayVars, Data_Name, Display_Index=Display_Index
; function Dial_Display_Add_Display, DisplayVars, Name, Type

;***************************************************************
;** Output Dial_Display message to IDL prompt
pro Dial_Display_LogMessage, DisplayVars, DisplayLevel, Message
;** Displays the Message if the present ActualLevel (DisplayVars.Verbosity)
;** matches the DisplayLevel (in silent, user, debug)
;** The date is appended before the message
;** LogString: Message is appened to LogString (string array)
;** File:      LogString is saved to file if specified

ActualLevel    = DisplayVars.Global.Verbosity

LevelList      = ['silent','normal','verbose','debug']
LocActualLevel = where(strcmp(LevelList, ActualLevel))
LocDisplayLevel= where(strcmp(LevelList, DisplayLevel))

if LocActualLevel(0) ge LocDisplayLevel(0) then begin

  for index=0,n_elements(Message)-1 do begin
    if n_elements(Message) eq 1 then $
      Message(index) = '['+systime(0)+'] '+Message(index)
    print, Message(index)  ; print to IDL terminal
  endfor

endif

end; PRO Light_LogMessage

;***************************************************************
;** Procedure to get Dial_Display data
function Dial_Display_Get_DisplayVars

  DialTag, 'display', Get=DisplayVars
  return, DisplayVars

end; FUN Dial_Display_Get_DisplayVars

;***************************************************************
; gets the index element of an array-like structure
; this is necessary as IDL arrays can only be of elements of identical size
; and we nned here variable size elements for Data and Displays
function Dial_Display_Get_Index, Structure, Index

    if Index lt 0 then Index = 0
    if Index ge Structure.Length then Index = Structure.Length-1
    ToReturn = -1
    ok = execute('ToReturn = Structure.Element_'+strtrim(string(Index),2))
    return, ToReturn

end; FUN Dial_Display_Get_Index

;***************************************************************
; sets the index element of an array-like structure to given value
pro Dial_Display_Set_Index, Structure=Structure, Index=Index, Value=Value

    n_Index = index
    if n_Index lt 0 then n_Index = 0
    Tools_StructSetValue, struct=Structure, tag='Element_'+strtrim(string(n_Index),2), val=Value

end; PRO Dial_Display_Set_Index


;***************************************************************
; look for the Display 'Display_Name' in Displays(..).Name array
function Dial_Display_Get_Display, DisplayVars, Display_Name

  Display_Name = strjoin(strsplit(Display_Name,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')

  Display_Index = -1
  for index=0, DisplayVars.Displays.Length-1 do begin
    Display = Dial_Display_Get_Index(DisplayVars.Displays, index)
    if strcmp(Display_Name, Display.Name) ne 0 then Display_Index = index
  endfor
  return, Display_Index

end; FUN Dial_Display_Get_Display

;***************************************************************
; returns the Data_Index, (and Display_Index in parameters)
function Dial_Display_Get_Data, DisplayVars, Data_Name, Display_Index=Display_Index

  if n_elements(Data_Name) eq 0 then return, -1
  Data_Name       = strjoin(strsplit(Data_Name,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')

  Display_Index = -1
  if n_elements(Data_Name) eq 0 then return, -1
  ; look for the Data 'Data_Name' in Display(..).Data(..).Name array
  Data_Index    = -1

  for index=0, DisplayVars.Displays.Length-1 do begin
    Display = Dial_Display_Get_Index(DisplayVars.Displays, index)
    for d_index=0, Display.Length-1 do begin
      Data = Dial_Display_Get_Index(Display, d_index)
      if strcmp(Data_Name, Data.Name_Data) ne 0 then begin
        Display_Index = index
        Data_Index    = d_index
      endif
    endfor
  endfor
  return, Data_Index

end; PRO Dial_Display_Get_Display

;***************************************************************
; Create/activate a new Display (or re-use an existing one) and retuns its Display_Index
function Dial_Display_Add_Display, DisplayVars, Name, Type

  Name       = strjoin(strsplit(Name,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')

  ; if Display already exists, return it
  Display_Index = Dial_Display_Get_Display(DisplayVars, Name)
  if Display_Index ge 0 then begin
    Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Display: using Display '+strtrim(string(Display_Index),2)+' "'+Name+'" '+Type
    return, Display_Index
  endif

  ; if a display is empty, re-use array slot and create live_tool
  ; empty displays are created at init, or by myself (see below)
  Empty_Display = Dial_Display_Get_Display(DisplayVars, "")
  Type = strlowcase(Type)
  if Empty_Display ge 0 then begin  ; re-use that array slot, and create live_tool
    Displays = DisplayVars.Displays
    Display  = Dial_Display_Get_Index(DisplayVars.Displays, Empty_Display)
    Display.Name  = Name
    Display.Type  = Type
    Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Display: Creating Display '+strtrim(string(Empty_Display),2)+' "'+Name+'" '+Type
    ; now create text display/live_tool
    if DisplayVars.Global.OrientRow eq 0 then dimension =  DisplayVars.Global.XSize $
    else dimension = DisplayVars.Global.YSize
    DrawDimension = [ dimension, dimension ]
    ExampleDim = 10
    ToExec = ''
    case Type of
      'text': begin
                xdisplayfile, 'dummy', text='['+systime(0)+'] New Text Display '+Name+' created.', $
                Group=DisplayVars.Handles.Base, /Editable, title=Name, Done_Button='Close ['+Name+']', WText=ID
                ID = strtrim(string(ID),2)
              end
      'plot':   begin
                  Data0 = sin(lindgen(ExampleDim)/3.0)
                end
      'image':  begin
                  Data0 = dist(ExampleDim)
                end
      'surface':begin
                  Data0 = dist(ExampleDim)
                end
      'contour':begin
                  Data0 = dist(ExampleDim)
                end
      else: begin
              Dial_Display_LogMessage, DisplayVars, 'normal','[E] Dial_Display_Add_Display. Unknow Display type'
              Dial_Display_LogMessage, DisplayVars, 'normal','[I] valid types are text, plot, image, surface, contour'
              return, -1
            end
    endcase

    if Type ne 'text' then begin
      ok = execute(Name+' = Data0')
      ToExec = ToExec+', '+Name+', DRAW_DIMENSIONS=DrawDimension, reference_out=R_out'
      if DisplayVars.Global.Gathered eq 1 then $
        ToExec = ToExec+', PARENT_BASE=DisplayVars.Handles.DisplayPanel' $
      else ToExec = ToExec+', TITLE=Name'
      ok = execute('live_'+Type+' '+ToExec+', Error=Error')
      if strlen(Error) ne 0 then begin
        Dial_Display_LogMessage, DisplayVars, 'verbose','[E] Dial_Display_Add_Display '+Error
        return, -1
      endif
      ID = R_out.win
      if strlen(ID) eq 0 then $
        ID = R_out.vis
      Display.Vis = R_out.vis
    endif

    ; store new ID (live tool name or string(widget_ID)). Data is still empty here
    Display.ID = ID

    ; save new Display in Dial Structure
    Dial_Display_Set_Index, Structure=Displays, Index=Empty_Display, Value=Display
    Tools_StructSetValue, struct=DisplayVars, tag='Displays', val=Displays
    DialTag, 'display', tag='Displays',  set=Displays
    Display_Index = Empty_Display
  endif else begin
    ; add an empty DisplayStructure slot in Displays array and call again myself (see up there)
    Displays = DisplayVars.Displays
    Dial_Display_Set_Index, Structure=Displays, Index=Displays.Length, Value=DisplayVars.Global.EmptyDisplay
    Displays.Length = Displays.Length+1

    Tools_StructSetValue, struct=DisplayVars, tag='Displays', val=Displays
    DialTag, 'display', tag='Displays',  set=Displays

    Display_Index = Dial_Display_Add_Display(DisplayVars, Name, Type)
  endelse
  ; else add new DisplatStruct in Displays array and create a new named Display live_tool of given Type with empty Data
  return, Display_Index

end; PRO Dial_Display_Add_Display

;***************************************************************
; Add/Set/Catenate new Data to a given Display/Data
pro Dial_Display_Add_Data, DisplayVars, Display_Name=Display_Name, $
                           IX=IX, IY=IY, Data=Data, $
                           Name_IX=Name_IX, Name_IY=Name_IY, Name_Data=Name_Data, $
                           Type = Type, Options=Options, Auto_Load=Auto_Load, $
                           Add=Add, Overlay=Overlay, Catenate=Catenate, $
                           Scatter=Scatter, Histogram=Histogram, Polar=Polar, Log=Log, $
                           Comment=Comment, No=No, Exact=Exact

  if n_elements(Type)    ne 0 then inputType = Type  ; save Type from input parameters

  ; determine the type of data if not given
  if n_elements(Type) eq 0 then Type = ''
  sData = size(Data)
  if strlen(Type) eq 0 then begin
    if size(Data,/type) eq 7 then Type = 'text' $
    else if sData(0) eq 1 or sData(0) eq 0 then Type='plot' $
    else if sData(0) eq 2 then Type='surface' $
    else Type = 'unknown'
  endif

  if Type eq 'unknown' then begin
    Dial_Display_LogMessage, DisplayVars, 'normal','[E] Dial_Display_Add_Data: Unknown data type. size='+string(sData)
    help, Data
    return
  endif

  if n_elements(Name_Data) eq 0 then Name_Data = "" $
  else Name_Data       = strjoin(strsplit(Name_Data,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')

  ; look if Name_Data already exists in opened Displays. Returns Data_Index and Display_Index
  Data_Index     = Dial_Display_Get_Data(DisplayVars, Name_Data, Display_Index=Display_Index)

  if n_elements(Overlay) ne 0 and Data_Index lt 0 then begin
      ; force creation of a new Data set, but keep Display if it was found
      Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Data: Overlay'
      Data_Index = -1
  endif

  ; look for the Display 'Display_Name' in Displays(..).Name array.
  ; if exists, use it (Display_Index ne 0), else if none exist find it by Name (Display_Name ne 0)
  ; or create new display
  if Display_Index lt 0 or n_elements(Display_Name) ne 0 then begin
    if n_elements(Display_Name) eq 0 then Display_Name = Name_Data $
    else Display_Name       = strjoin(strsplit(Display_Name,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')
    Display_Index = Dial_Display_Add_Display(DisplayVars, Display_Name, Type)
  endif

  ; here, a display is available (either a new one, or use existing one)
  Displays     = DisplayVars.Displays
  Display      = Dial_Display_Get_Index(Displays, Display_Index)    ; this DisplayStruct
  Display_Name = Display.Name

  if Display.Type eq 'text' and size(Data, /type) ne 7 then begin
    Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Data: Translating numerics into text'
    Data = string(Data)
  endif

  if Display.Type ne 'text' and Type eq 'text' then begin
      Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Data: Translating text into numerics'
      sData= size(Data, /dimensions)
      ok = execute('Data = ['+strjoin(strjoin(string(Data)))+']')
      if Display.Type ne 'plot' then $
        Data = reform(Data, sData)
  endif

  Update = 0
  if Display.Type ne 'text' and Display.Type ne 'plot' and n_elements(inputType) ne 0 then begin
    ; can change display type for 2d (surface, image, contour)
    if inputType ne 'text' and inputType ne 'plot' and inputType ne Display.Type then begin
      Display.Type = inputType
      Update = 2
    endif
  endif

  if Display.Type eq 'plot' and sData(0) ne 1 then begin
    ; reshape Data to 1D vector
    if size(Data, /type) eq 0 then begin
      Dial_Display_LogMessage, DisplayVars, 'verbose','[I] Dial_Display_Add_Data: undefined data'
    endif else begin
      if sData(0) eq 0 then begin
        Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Data: forcing catenate of scalar'
        Catenate = 1
      endif else begin
        Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Data: Changing 2D array to 1D vector'
        Data = reform(Data, sData(n_elements(sData)-1))
      endelse
    endelse
  endif

  if Display.Type ne 'plot' and Display.Type ne 'text' and sData(0) eq 1 then begin
    Data = transpose(Data)
  end

  if Data_Index lt 0 or Data_Index ge Display.Length then begin
    ; Data does not exist yet
    ; search for empty Data slot in display and use that if any, else extend array
    Data_Index = -1
    for d_index=0, Display.Length-1 do begin
      This_Data = Dial_Display_Get_Index(Display, d_index)
      if strlen(This_Data.Name_Data) eq 0 then Data_index    = d_index
    endfor

    if Data_Index lt 0 then begin
      ; new Data to create (no empty slot found)
      This_Data = DisplayVars.Global.EmptyData
      ; extend data array in Display(Struct). Index is Display.Length
      Dial_Display_Set_Index, Structure=Display, Index=Display.Length, Value=DisplayVars.Global.EmptyData
      Display.Length = Display.Length+1
      Data_index = Display.Length-1
    endif else begin
      ; we found an empty slot. Use that
      This_Data = Dial_Display_Get_Index(Display, Data_Index)     ; Unamed Data "" (Empty) to act on is there
      ; Name_Data = This_Data.Name_Data
    endelse
    This_Data.Date = systime(1)

  endif else begin
    ; Data exists already
    This_Data = Dial_Display_Get_Index(Display, Data_Index)     ; Named Data to act on is there
    Name_Data = This_Data.Name_Data
  endelse

  ; This_Data is now defined

  if strlen(Display_Name) eq 0 then begin
    Display_Name = 'Display'+strtrim(string(Display_Index),2)
    Display.Name = Display_Name
  endif

  if n_elements(Options) ne 0 then NewOption = Options else NewOption = ''

  if n_elements(Add)        ne 0 then NewOption = NewOption+'add '
  if n_elements(Delete)     ne 0 then NewOption = NewOption+'delete '
  if n_elements(Catenate)   ne 0 then NewOption = NewOption+'catenate '
  if n_elements(Scatter)    ne 0 then NewOption = NewOption+'scatter '
  if n_elements(Histogram)  ne 0 then NewOption = NewOption+'histogram '
  if n_elements(Polar)      ne 0 then NewOption = NewOption+'polar '
  if n_elements(Log)        ne 0 then NewOption = NewOption+'log '
  if n_elements(Exact)      ne 0 then NewOption = NewOption+'exact '

  NewOption    = strlowcase(NewOption)
  This_Option  = strlowcase(This_Data.Options)


  if strlen(NewOption) gt 0 then begin
    if n_elements(No) ne 0 then begin
      This_OptionA = strsplit(This_Option, /extract)
      ; set This_OptionA(index) to '' for elements that are found in NewOption
      for o_index = 0, n_elements(This_OptionA)-1 do begin
        if strpos(NewOption, This_OptionA(o_index)) ge 0 then This_OptionA(o_index) = ''
      endfor
      This_Option = strjoin(This_OptionA,' ')
    endif else begin
      NewOptionA   = strsplit(NewOption, /extract)
      ; add NewOptionA(index) to This_Option for elements that are not in This_Option
      for o_index = 0, n_elements(NewOptionA)-1 do begin

        if strpos(This_Option, NewOptionA(o_index)) lt 0 then This_Option = This_Option+' '+NewOptionA(o_index)
      endfor
    endelse
  endif

  if This_Option ne This_Data.Options then begin
    This_Data.Options = This_Option
    Update = 2
  endif

  ; handle the Data options...

  if strpos(This_Data.Options, 'add') ge 0 then begin
    Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Data: Add'
    ; add new data to the existing one. Do not touch the axes
    ; dimensions must match !!
    if n_elements(Data) gt 0     then Data = This_Data.Data + Data
  endif

  if strpos(This_Data.Options, 'delete') ge 0 then begin
    Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Data: Delete'
    ; delete data
    Dial_Display_Delete_Data, DisplayVars, Display_Index=Display_Index, Data_Index=Data_Index
    widget_control, DisplayVars.Handles.Base_About, Bad_ID=Bad_ID
    ; Bad_ID: If no error occurs, a zero is stored.
    if Bad_ID eq 0 and DisplayVars.Handles.Base_About ne 0 then Dial_Display_Event_Show_List, DisplayVars, /no_raise
    return
  endif

  if n_elements(IX) gt 0 then nIX=IX
  if n_elements(IY) gt 0 then nIY=IY
  if n_elements(Data) gt 0 then nData=Data

  if strpos(This_Data.Options, 'catenate') ge 0 then begin
    Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Data: Catenate'
    ; catenate new data, and optionally (if given) new axes
    if n_elements(IX) eq 0 then IX = systime(1) - This_Data.Date
    if strlen(This_Data.Name_Data) eq 0 then begin
      if n_elements(IX) gt 0       then nIX   = [ IX(0), IX ]
      if n_elements(IY) gt 0       then nIY   = [ IY(0), IY ]
      if n_elements(Data) gt 0     then nData = [ Data(0), Data ]
    endif else begin
      if n_elements(IX) gt 0       then nIX   = [ This_Data.IX, IX ]
      if n_elements(IY) gt 0       then nIY   = [ This_Data.IY, IY ]
      if n_elements(Data) gt 0     then nData = [ This_Data.Data, Data ]
    endelse

    if DisplayVars.Plot ge 2 then begin ; keep last Plot values
      sData = size(nData, /dim)
      max_index = sData(0)
      min_index = max([0, max_index - DisplayVars.Plot +1])
      index = lindgen(max_index - min_index +1)+min_index
      if n_elements(nIX) gt 0       then nIX   = nIX(index,*)
      if n_elements(nIY) gt 0       then nIY   = nIY(index,*)
      if n_elements(nData) gt 0     then nData = nData(index,*)
    endif
  endif

  ; Replace: will use Data as supplied in parameters, and override This_Data
  Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Add_Data: Update Data'

  ; now fill-in the data X,Y,Data in Data_index slot; determine Update level.
  ; Update = 1: keep current live view, and just update the data block (faster)
  ; Update = 2: destroy current view and rebuild a new one at the same place, with present values
  if n_elements(Name_Data) gt 0 then begin
    if Name_Data ne This_Data.Name_Data then begin
      Tools_StructSetValue, struct=This_Data, tag='Name_Data', val= Name_Data
      Update = 2
    endif
  endif
  if n_elements(Auto_Load) gt 0 then begin
    if Auto_Load(0) ne This_Data.Auto_Load then begin
      Tools_StructSetValue, struct=This_Data, tag='Auto_Load', val= Auto_Load(0)
      if Update eq 0 then Update = 1
    endif
  endif
  if n_elements(Data) gt 0      then begin
    Tools_StructSetValue, struct=This_Data, tag='Data'    , val= nData
    ; if same size,  update = 1
    ; if other size, update = 2
    if n_elements(nData) eq n_elements(This_Data.Data) and Update eq 0 then Update = 1
    Tools_StructSetValue, struct= DisplayVars, tag='Value' , val= nData
  endif
  if n_elements(IX) gt 0        then begin
    if n_elements(where(nIX ne This_Data.IX)) or n_elements(nIX) ne n_elements(This_Data.IX) then begin
      Tools_StructSetValue, struct=This_Data, tag='IX'      , val= nIX
      Update=2
    endif
  endif
  if n_elements(IY) gt 0        then begin
    if n_elements(where(nIY ne This_Data.IY)) then begin
      Tools_StructSetValue, struct=This_Data, tag='IY'      , val= nIY
      Update = 2
    endif
  endif
  if strlen(Name_Data) eq 0 then begin
    Name_Data = 'Display'+strtrim(string(Display_Index),2)+'_Data'+strtrim(string(Data_index),2)
    This_Data.Name_Data = Name_Data
    Update = 2
  endif
  if n_elements(Name_IX) gt 0   then begin
    if Name_IX(0) ne This_Data.Name_IX then begin
      Tools_StructSetValue, struct=This_Data, tag='Name_IX' , val= Name_IX(0)
      Update = 2
    endif
  endif
  if n_elements(Name_IY) gt 0   then begin
    if Name_IY(0) ne This_Data.Name_IY then begin
      Tools_StructSetValue, struct=This_Data, tag='Name_IY' , val= Name_IY(0)
      Update = 2
    endif
  endif
  if n_elements(Comment) gt 0   then begin
    if strjoin(Comment) ne strjoin(This_Data.Comment) then begin
      Tools_StructSetValue, struct=This_Data, tag='Comment',  val= Comment
      Update = 2
    endif
  endif

  ; activate Redraw Flag for display

  ; save Structures
  ; Display_Data(Data_Index)    = This_Data
  Dial_Display_Set_Index, Structure=Display, Index=Data_Index, Value=This_Data
  Display.Update              = Update
  ; Displays(Display_Index)     = Display
  Dial_Display_Set_Index, Structure=Displays, Index=Display_Index, Value=Display

  Tools_StructSetValue, struct= DisplayVars, tag='Displays' , val= Displays
  DialTag, 'display', tag='Displays',  set=Displays

end; PRO Dial_Display_Add_Data

;***************************************************************
; set the Name to "" and remove Data (free memory) (given by Name or Index)
pro Dial_Display_Delete_Data, DisplayVars, Display_Name=Display_Name, Display_Index=Display_Index, $
                           Name_Data=Name_Data, Data_Index=Data_Index

  if n_elements(Display_Name) gt 0 then begin
    Display_Name       = strjoin(strsplit(Display_Name,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')
    if n_elements(Display_Index) eq 0 and strlen(Display_Name) gt 0 then $
      Display_Index = Dial_Display_Get_Display(DisplayVars, Display_Name)
  endif
  if n_elements(Name_Data) gt 0 then begin
    Name_Data       = strjoin(strsplit(Name_Data,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')
    if n_elements(Data_Index) eq 0 and strlen(Name_Data) gt 0 then $
      Data_Index = Dial_Display_Get_Data(DisplayVars, Data_Name, Display_Index=Display_Index)
  endif

  if n_elements(Name_Data) eq 0 and n_elements(Data_Index) eq 0 then begin
    Dial_Display_Delete_Display, DisplayVars, Display_Index=Display_Index
    return
  endif

  if n_elements(Display_Index) eq 0 then return

  Displays = DisplayVars.Displays
  Display = Dial_Display_Get_Index(Displays, Display_Index)

  if Display.Type ne 'text' then begin
    if strlen(Display.vis) gt 0 then ok = execute('live_destroy, "'+Display.vis+'", WINDOW_IN=Display.ID, Error=Error')
  endif

  ; now shifts following Data elements in Display
  if Data_Index+1 ge Display.Length-1 then begin
    for index = Data_Index+1, Display.Length-1 do begin
      Next_Data = Dial_Display_Get_Index(Display, Index)
      Dial_Display_Set_Index, Structure=Display, Index=Index-1, Value=Next_Data
    endfor
  endif
  Display.Length = Display.Length-1
  Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Delete_Data: Delete Data ' $
    +strtrim(string(Data_Index),2)+'. Now '+strtrim(string(Display.Length),2)+' elements in Display '+strtrim(string(Display_Index),2)
  if Display.Length gt 0 then Display.Update = 2

  Tools_StructSetValue, struct=DisplayVars, tag='Displays', val=Displays
  DialTag, 'display', tag='Displays',  set=Displays

  if Display.Length ge 1 then begin
    Dial_Display_Set_Index, Structure=Displays, Index=Display_Index, Value=Display
    Tools_StructSetValue, struct=DisplayVars, tag='Displays', val=Displays
    DialTag, 'display', tag='Displays',  set=Displays
  endif else Dial_Display_Delete_Display, DisplayVars, Display_Index=Display_Index

end; PRO Dial_Display_Delete_Data

;***************************************************************
; delete a given Display (by Name or Index)
pro Dial_Display_Delete_Display, DisplayVars, Display_Name=Display_Name, Display_Index=Display_Index

  if n_elements(Display_Name) gt 0 then begin
    Display_Name       = strjoin(strsplit(Display_Name,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')
    if n_elements(Display_Index) eq 0 and strlen(Display_Name) gt 0 then $
      Display_Index = Dial_Display_Get_Display(DisplayVars, Display_Name)
  endif

  ; delete a non-empty Display name, when more than one Display exists
  if Display_Index ge 0 then begin
      Display = Dial_Display_Get_Index(DisplayVars.Displays, Display_Index)
      if strcmp(Display.Type, 'text') ne 0 then begin
        top = widget_info(long(Display.ID), /parent)
        widget_control, top, /destroy, Bad_id=id
      endif else live_destroy, window_in=Display.ID, Error=Error
      Displays = DisplayVars.Displays

      ; now shifts following Display elements in Displays
      if Display_Index+1 ge Displays.Length-1 then begin
        for index = Display_Index+1, Displays.Length-1 do begin
          Next_Display = Dial_Display_Get_Index(Displays, Index)
          Dial_Display_Set_Index, Structure=Displays, Index=Index-1, Value=Next_Display
        endfor
      endif
      Displays.Length = Displays.Length-1
      Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Delete_Display: Delete Display ' $
    +strtrim(string(Display_Index),2)+'. Now '+strtrim(string(Displays.Length),2)+' elements in Displays '

      Tools_StructSetValue, struct=DisplayVars, tag='Displays', val=Displays
      DialTag, 'display', tag='Displays',  set=Displays

  endif ; else begin
    ; if n_elements((Display_Name) eq 0 then widget_control, DisplayVars.Handles.Base, /destroy, bad_id=tmp
  ; endelse

end; PRO Dial_Display_Delete_Display

;***************************************************************
pro Dial_Display_Get_List, DisplayVars, DataList=DataList, DataArr=DataArr, DisplayArr=DisplayArr

  for index=0, DisplayVars.Displays.Length-1 do begin
    Display = Dial_Display_Get_Index(DisplayVars.Displays, index)
    ToDisplay = [ 'Display "'+strtrim(Display.Name,2)+'" (' $
                + Display.Type +') '+Display.ID ]
    if n_elements(DataList) eq 0 then DataList = ToDisplay else DataList = [ DataList, ToDisplay ]

    if n_elements(DisplayArr) eq 0 then DisplayArr = index else DisplayArr = [ DisplayArr, index ]
    if n_elements(DataArr) eq 0    then DataArr = -1       else DataArr = [ DataArr, -1 ]
    if strlen(Display.Name) gt 0 then begin
      for d_index=0, Display.Length-1 do begin
        Data = Dial_Display_Get_Index(Display, d_index)
        DataArr = [ DataArr, d_index ]
        DisplayArr = [ DisplayArr, index ]
        if strlen(Data.Name_Data) gt 0 then begin
          if Display.Type eq 'text' then begin
                      sv = size(Data.Data)
                      if sv(0) eq 0 then  DSize = [ strtrim(string(sv(1)),2)+' characters' ] $
                      else DSize = [ strtrim(string(sv(1)),2)+' lines' ]
                      ToDisplay = [ ' + Data "'+strtrim(Data.Name_Data,2)+'" ['+DSize+'] '+Data.Auto_Load ]
          endif
          if Display.Type eq 'plot' then begin
                      Stats = ''
                      sdata = size(Data.Data)
                      DSize = strtrim(string(sdata(1)),2)+' elements'
                      ToDisplay = [ ' + Data "'+strtrim(Data.Name_Data,2)+'" ['+DSize+'] '+Data.Auto_Load ]
          endif
          if Display.Type eq 'image' or Display.Type eq 'surface' or Display.Type eq 'contour' then begin
                    Stats = ''
                    sdata = size(Data.Data)
                    DSize = strtrim(string(sdata(1)),2)+' x '+strtrim(string(sdata(2)),2)+' elements'
                    ToDisplay = [ ' + Data "'+strtrim(Data.Name_Data,2)+'" ['+DSize+'] '+Data.Auto_Load ]
          endif
          DataList = [ DataList, ToDisplay ]
        endif
      endfor
    endif
  endfor

end; PRO Dial_Display_Get_List

;***************************************************************
; build main Display panel interface and menu
pro Dial_Display_Build, DisplayVars

  ; if not exist or Descriptor is not found in properties, then create whole Dial Interface
  if DisplayVars.Handles.Base eq 0 and xregistered('Dial_Display_Interface') eq 0 then begin

    Dial_Display_LogMessage, DisplayVars, 'normal','[ ] Welcome to Dial Display '+DisplayVars.Global.Version

    widget_control, /HOURGLASS

    if DisplayVars.Global.ParentBase eq 0 then begin ; create new window
      if DisplayVars.Global.Group_Leader eq 0 then begin
        DisplayVars.Handles.Base = widget_base(/column, title='Display Panel', $
          /TLB_SIZE_EVENTS, /TLB_KILL_REQUEST_EVENTS, mbar=Base_Menu) 
      endif else begin
        DisplayVars.Handles.Base = widget_base(/column, title='Display Panel', $
          Group_Leader=DisplayVars.Global.Group_Leader, /TLB_SIZE_EVENTS, $
          /TLB_KILL_REQUEST_EVENTS, mbar=Base_Menu) 
      endelse
    endif else begin ; incorporate into existing Widget_Base
      DisplayVars.Handles.Base = widget_base(DisplayVars.Global.ParentBase, /column)
      Base_Menu = widget_base(DisplayVars.Handles.Base, /row)
    endelse

    Base_File = widget_button(Base_Menu,value='File >',uvalue='Display_File_Menu', menu=2)
    ;** now comes the pop-up 'File >' menu items
    tmp1= widget_button(Base_File,value='Export as Image...', uvalue='Save_Data_Image')
    tmp1= widget_button(Base_File,value='Export as Postscript/Print...',     uvalue='Save_Data_PS')
    tmp1= widget_button(Base_File,value='Export as Text...',     uvalue='Save_Data_Text')
    tmp1= widget_button(Base_File,value='Close Data/Display...',    uvalue='Close_DisplayData')
    tmp1= widget_button(Base_File,value='Close All',    uvalue='Delete_All')
    tmp1= widget_button(Base_File,value='Exit',           uvalue='Close', /separator)

    Base_Edit = widget_button(Base_Menu,value='Edit >',uvalue='Display_Edit_Menu', menu=2)
    ;** now comes the pop-up 'Edit >' menu items
    tmp1= widget_button(Base_Edit,value='Edit Preferences...',     uvalue='Preferences')
    tmp1= widget_button(Base_Edit,value='Show Display List...',    uvalue='Show_List')
    tmp1= widget_button(Base_Edit,value='Update Displays',         uvalue='Update_All')
    DisplayVars.Handles.AutoUpdate= widget_button(Base_Edit,value='Auto Update: ON',         uvalue='AutoUpdate', /separator)
    DisplayVars.Handles.AutoSave  = widget_button(Base_Edit,value='Auto Save: OFF',         uvalue='AutoSave')

    Base_Help = widget_button(Base_Menu,value='Help >',uvalue='Display_Help_Menu', menu=2)
    ;** now comes the pop-up 'Help >' menu items
    tmp1= widget_button(Base_Help,value='About [File]',         uvalue='Help_File')
    tmp1= widget_button(Base_Help,value='About [Edit]',         uvalue='Help_Edit')
    tmp1= widget_button(Base_Help,value='About [Preferences]',  uvalue='Help_Preferences')
    tmp1= widget_button(Base_Help,value='About [Options]',      uvalue='Displays_Help')
    tmp1= widget_button(Base_Help,value='About Displays',       uvalue='Show_List', /separator)

    ;** now comes the Display fields Panel into a Base
    if DisplayVars.Global.OrientRow eq 0 then $
      DisplayVars.Handles.DisplayPanel = widget_base(DisplayVars.Handles.Base,/column, xpad=0, /scroll) $
    else $
      DisplayVars.Handles.DisplayPanel = widget_base(DisplayVars.Handles.Base,/row, xpad=0, /scroll)

    if DisplayVars.Global.XSize ne 0 then $
      widget_control, DisplayVars.Handles.DisplayPanel, xsize=DisplayVars.Global.XSize, bad_id=id
    if DisplayVars.Global.YSize ne 0 then $
      widget_control, DisplayVars.Handles.DisplayPanel, ysize=DisplayVars.Global.YSize, bad_id=id

    widget_control, DisplayVars.Handles.Base, /realize, bad_id=id
    XManager, 'Dial_Display_Interface', DisplayVars.Handles.Base, Event_Handler='Dial_Display_Event_Parser', $
        /just_reg, /no_block

  endif

  ; if exist, but change display type then delete display widget (text or draw) in Dial Interface

end; PRO Dial_Display_Build

;***************************************************************
;** Dial_Display_Event_Parser : event manager
pro Dial_Display_Event_Parser, event

  ;** retrieve the 'uvalue' from Widget -> Get event description
  widget_control, event.id, get_uvalue=uv, bad_id=id
  DisplayVars = Dial_Display_Get_DisplayVars()
  res = execute('Displays = DisplayVars.Displays.Length')
  if DisplayVars.Displays.Length eq 0 then res=Dial_Display_Add_Display( DisplayVars, 'example', 'plot')

  ;** First handle close and resize events
  if  tag_names(event, /STRUCTURE_NAME) eq 'WIDGET_KILL_REQUEST' then begin
      Dial_Display_Event_Close, DisplayVars
      return
  endif else begin
    type = size(uv, /type)

    if type eq 7 then begin ; String type
      ;** Checks that the Event routine exists...
      Routine = 'Dial_Display_Event_'+uv
      tmp1 = routine_info('')
      tmp1 = where(strcmp(tmp1, strupcase(Routine)))
      if tmp1(0) ge 0 then begin
       Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Event_Parser:'+uv
       call_procedure, Routine, DisplayVars
       if uv eq 'Close' then return
      endif else begin
       Dial_Display_LogMessage, DisplayVars, 'debug','[E] Dial_Display_Event_Parser: PRO '+Routine+' ?'
      endelse
    endif else begin
      if tag_names(event, /STRUCTURE_NAME) eq 'WIDGET_BASE' then begin
        DisplayVars.Global.XSize = min([event.X, 1024])
        DisplayVars.Global.YSize = min([event.Y, 768])
        Dial_Display_Event_Resize, DisplayVars

      endif else begin
        Dial_Display_LogMessage, DisplayVars, 'debug','[E] Dial_Display_Event_Parser: Unknown event'
        help, event, uv
      endelse
    endelse
  endelse

  if DisplayVars.Handles.Base ne 0 then begin
    ;** Update Dial structure
    DialTag, 'display', tag='Global',  set=DisplayVars.Global
    DialTag, 'display', tag='Handles', set=DisplayVars.Handles
    DialTag, 'display', tag='Displays',set=DisplayVars.Displays
  endif else begin
    Dial_Display_LogMessage, DisplayVars, 'normal','[ ] Dial_Display: Exiting'
  endelse

end; PRO Dial_Display_Event_Parser

;***************************************************************
pro Dial_Display_Event_Close, DisplayVars

  Dial_Display_Event_Delete_All, DisplayVars

  ; close main base
  widget_control, DisplayVars.Handles.Base, /destroy, Bad_id = id
  ;DisplayVars.Handles.Base  = 0L
  DialStop,  'display'
  DialClear, 'display'

end; PRO Dial_Display_Event_Close


;***************************************************************
; resize Base, and all Displays
pro Dial_Display_Event_Resize, DisplayVars
; Get new size of Base as event.X and event.Y depending on Global.OrientRow

  widget_control, DisplayVars.Handles.Base, map=0
  ; store original base location
  Base_Geometry = widget_info(DisplayVars.Handles.Base, /geometry)

  ; set size of DisplayPanel with that (- borders)
  widget_control, DisplayVars.Handles.DisplayPanel, xsize=DisplayVars.Global.XSize, bad_id=id
  widget_control, DisplayVars.Handles.DisplayPanel, ysize=DisplayVars.Global.YSize, bad_id=id

  new_dim = min([DisplayVars.Global.XSize, DisplayVars.Global.YSize])

  ; set size of each Live_display inside
  for index=0, DisplayVars.Displays.Length-1 do begin
    Display = Dial_Display_Get_Index(DisplayVars.Displays, index)
    if strlen(Display.Name) gt 0 then begin

      live_info, Window_In=Display.ID, Properties=Prop_Vis, Error=Error
      if strlen(Error) eq 0 and DisplayVars.Global.Gathered eq 1 then begin
        Prop_Vis.Dimensions = [ new_dim, new_dim]
        if (DisplayVars.Global.OrientRow) ne 0 then $
          Prop_Vis.Location   = [ (index)*new_dim, 0 ] $
        else $
          Prop_Vis.Location   = [ 0, (index)*new_dim ]
        live_control, Window_In=Display.ID, Properties=Prop_Vis, /no_draw
      endif else Dial_Display_LogMessage, DisplayVars, 'debug','[E] Dial_Display_Event_Resize '+Error

    endif
  endfor
  ; reset Base location to original one
  if DisplayVars.Global.XSize ne 0 then $
      widget_control, DisplayVars.Handles.DisplayPanel, xsize=DisplayVars.Global.XSize, bad_id=id
  if DisplayVars.Global.YSize ne 0 then $
      widget_control, DisplayVars.Handles.DisplayPanel, ysize=DisplayVars.Global.YSize, bad_id=id
  widget_control, DisplayVars.Handles.Base, xoffset=Base_Geometry.Xoffset, yoffset=Base_Geometry.Yoffset, bad_id=id, map=1

end; PRO Dial_Display_Event_Resize

;***************************************************************
pro Dial_Display_Event_Show_List, DisplayVars, no_raise=no_raise

  ToDisplay = [ 'List of Displays and Data sets ['+systime(0)+']', '' ]
  for index=0, DisplayVars.Displays.Length-1 do begin
    Display = Dial_Display_Get_Index(DisplayVars.Displays, index)
    if strlen(Display.Name) gt 0 then begin
      ToDisplay = [ ToDisplay, $
                'Display "'+strtrim(Display.Name,2)+'" (' $
                + Display.Type +') '+Display.ID ]
      for d_index=0, Display.Length-1 do begin
        Data = Dial_Display_Get_Index(Display, d_index)
        Head = '    '
        if strlen(Data.Name_Data) gt 0 and n_elements(data.data) gt 1 then begin
          myexcept = !except
          !except = 0
          if Display.Type eq 'text' then begin
                      sv = size(Data.Data)
                      if sv(0) eq 0 then  DSize = [ strtrim(string(sv(1)),2)+' characters' ] $
                      else DSize = [ strtrim(string(sv(1)),2)+' lines' ]
                      ToDisplay = [ ToDisplay, ' + Data "'+strtrim(Data.Name_Data,2)+'" ['+DSize+'] '+Data.Auto_Load ]
          endif
          if Display.Type eq 'plot' then begin
                      Stats = ''
                      sdata = size(Data.Data)
                      if n_elements(Data.IX) eq sdata(1) then IX = Data.IX else IX=lindgen(sdata(1))
                      IX = double(IX)
                      DSize = strtrim(string(sdata(1)),2)+' elements '
                      Stats = [ Head+'min='+strtrim(string(min(Data.Data)),2)+' max='+strtrim(string(max(Data.Data)),2)+' mean='+strtrim(string(mean(Data.Data)),2)+' std='+strtrim(string(stddev(Data.Data)),2)+' sum='+strtrim(string(total(Data.Data)),2) ]
                      if total(Data.Data) ne 0 then begin
                        fmon = total(IX*Data.Data)/total(Data.Data)
                        smon = total(IX*IX*Data.Data)/total(Data.Data) - fmon*fmon
                      endif else begin
                        fmon = 'NaN'
                        smon = 0
                      endelse
                      if smon gt 0 then smon = sqrt(smon)  else smon = 'NaN'
                      Stats = [ Stats, Head+'X=['+strtrim(string(min(IX)),2)+':'+strtrim(string(max(IX)),2)+']. Center='+strtrim(string(fmon),2)+' width='+strtrim(string(smon),2) ]
                      ToDisplay = [ ToDisplay, ' + Data "'+strtrim(Data.Name_Data,2)+'" ['+DSize+'] '+Data.Auto_Load+' with '+Data.Options, Stats ]
          endif
          if Display.Type eq 'image' or Display.Type eq 'surface' or Display.Type eq 'contour' then begin
                    Stats = ''
                    sdata = size(Data.Data)

                    DSize = strtrim(string(sdata(1)),2)+' x '+strtrim(string(sdata(2)),2)+' elements'
                    Stats = [ Head+'min='+strtrim(string(min(Data.Data)),2)+' max='+strtrim(string(max(Data.Data)),2)+' mean='+strtrim(string(mean(Data.Data)),2)+' std='+strtrim(string(stddev(Data.Data)),2)+' sum='+strtrim(string(total(Data.Data)),2) ]
                    if n_elements(Data.IX) eq sdata(1) then IX = Data.IX else IX=lindgen(sdata(1))
                    IX = double(IX)
                    XData = total(Data.Data,1)
                    if total(XData) ne 0 then begin
                      fmon = total(IX*XData)/total(XData)
                      smon = total(IX*IX*XData)/total(XData) - fmon*fmon
                    endif else begin
                      fmon = 'NaN'
                      smon = 0
                    endelse
                    if smon gt 0 then smon = sqrt(smon)  else smon = 'NaN'
                    Stats = [ Stats, Head+'X=['+strtrim(string(min(IX)),2)+':'+strtrim(string(max(IX)),2)+']. Center='+strtrim(string(fmon),2)+' width='+strtrim(string(smon),2) ]
                    if n_elements(Data.IY) eq sdata(2) then IY = Data.IY else IY=lindgen(sdata(2))
                    IY = double(IY)
                    YData = total(Data.Data,2)
                    if total(YData) ne 0 then begin
                      fmon = total(IY*YData)/total(YData)
                      smon = total(IY*IY*YData)/total(YData) - fmon*fmon
                    endif else begin
                      fmon = 'NaN'
                      smon = 0
                    endelse
                    if smon gt 0 then smon = sqrt(smon)  else smon = 'NaN'
                    Stats = [ Stats, Head+'Y=['+strtrim(string(min(IY)),2)+':'+strtrim(string(max(IY)),2)+']. Center='+strtrim(string(fmon),2)+' width='+strtrim(string(smon),2) ]
                    ToDisplay = [ ToDisplay, ' + Data "'+strtrim(Data.Name_Data,2)+'" ['+DSize+'] '+Data.Auto_Load+' with '+Data.Options, Stats ]
          endif
          a = check_math()
          !except   = myexcept
        endif
      endfor
    endif
  endfor

  Mem = memory(/current)
  ToDisplay = [ToDisplay, '', $
               'This is Dial Display for George.',$
               'Version:     '+DisplayVars.Global.Version, $
               'Author:      '+DisplayVars.Global.Author, $
               'Memory used: '+strtrim(Mem/1024,2)+' ko', $
               '',$
               'This IDL interface is a Dial that displays/updates data sets', $
               'originating from other IDL programs.',$
               'Contributions: D. Richard, ILL for Dial_ByGeorge; R. Sterner', $
               'and Johns Hopkins University/Applied Physics Laboratory for XList', $
               'License: This software may be used, copied, or redistributed as', $
               'long as it is not sold and this copyright notice is reproduced', $
               'on each copy made. This routine is provided as is without any', $
               'express or implied warranties whatsoever.' ]

  widget_control, DisplayVars.Handles.Base_About, set_value=ToDisplay, Bad_ID=Bad_ID
  if n_elements(no_raise) eq 0 then widget_control, DisplayVars.Handles.Base_About, show=1, iconify=0, Bad_ID=Bad_ID
  if Bad_ID ne 0 or DisplayVars.Handles.Base_About eq 0 then  begin
    xdisplayfile, 'dummy', text=ToDisplay, Group=DisplayVars.Handles.Base, $
          title='About Displays', /Editable, $
          Height=15,Width=60,Done_Button='Close [About Displays]', WText=ID

    DisplayVars.Handles.Base_About = ID
    ;** Update Dial structure
    DialTag, 'display', tag='Handles', set=DisplayVars.Handles
  endif

end; PRO Dial_Display_Event_Show_List

;***************************************************************
;** Display information about Displays/Preferences
pro Dial_Display_Event_Help_Preferences, DisplayVars, update=update

  ToDisplay =['PREFERENCES menu Help for "Displays"',$
              '',$
              'This menu enables to change some internal settings controling the execution of Displays',$
              'OrientRow=<0|1>',$
              '   sets the prefered orientation of display panel',$
              'Gathered=<0|1>',$
              '   when 1, all displays are gathered in the same panel',$
              '   when 0, they appear as separate windows',$
              'Verbosity=<level>',$
              '   affects the amount of messages displayed by Displays',$
              'History size=<-2,0,3:100>', $
              '   sets the number of counts values to remember and plot (and optionally save)', $
              'Frequency', $
              '   sets the update frequency of Displays' $
              ]

  widget_control, DisplayVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Preferences Help for Displays'
  if Bad_ID ne 0 or DisplayVars.Handles.Base_Help eq 0 and n_elements(update) eq 0 then  begin
    xdisplayfile, 'dummy', text=ToDisplay, Group=DisplayVars.Handles.Base, $
          title='Preferences Help for Displays',$
          Height=20,Width=80,Done_Button='Close [Displays Help]', WText=ID
    DisplayVars.Handles.Base_Help = ID
    ;** Update Dial structure
    DialTag, 'display', tag='Handles', set=DisplayVars.Handles
  endif

end; PRO Dial_Display_Event_Help_Preferences

;***************************************************************
;** Display information about Displays/File menu
pro Dial_Display_Event_Help_File, DisplayVars

  ToDisplay =['File menu Help for "Displays"',$
              '',$
              'This menu enables to export data and close exit appilcation',$
              'Export ar Image',$
              '   choose a Display and save it as JPG, GIF, TIFF, BMP, PICT, PNG', $
              'Export ar Postscript/Print',$
              '   choose a Display and save it as PostScript or send it to a Printer', $
              'Export ar Text',$
              '   choose a Display and save it as a text file', $
              'Close Data/Display', $
              '   choose a Display/Data set and close it (delete data/remove)', $
              'Exit', $
              '   exit Display application' $
              ]

  widget_control, DisplayVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Preferences Help for Displays'
  if Bad_ID ne 0 or DisplayVars.Handles.Base_Help eq 0 then  begin
    xdisplayfile, 'dummy', text=ToDisplay, Group=DisplayVars.Handles.Base, $
          title='File Help for Displays',$
          Height=20,Width=80,Done_Button='Close [Displays Help]', WText=ID
    DisplayVars.Handles.Base_Help = ID
    ;** Update Dial structure
    DialTag, 'display', tag='Handles', set=DisplayVars.Handles
  endif

end; PRO Dial_Display_Event_Help_File

;***************************************************************
;** Display information about Displays/Edit menu
pro Dial_Display_Event_Help_Edit, DisplayVars

  ToDisplay =['Edit menu Help for "Displays"',$
              '',$
              'This menu enables to configure/view display options',$
              'Edit Preferences',$
              '   edit the Dial Display preferences (see Help for that topic)', $
              'Show display list',$
              '   displays the list of active displays, as well as basic statistics', $
              'Update displays',$
              '   force the update of displays (instead of waiting for automatic update)', $
              'Auto Update: ON/OFF', $
              '   toggles the automatic display update (with Frequency specified in Preferences)', $
              'Auto Save: ON/OFF', $
              '   toggles the automatic display save as text files (with each display Update)' $
              ]

  widget_control, DisplayVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Preferences Help for Displays'
  if Bad_ID ne 0 or DisplayVars.Handles.Base_Help eq 0 then  begin
    xdisplayfile, 'dummy', text=ToDisplay, Group=DisplayVars.Handles.Base, $
          title='File Help for Displays',$
          Height=20,Width=80,Done_Button='Close [Displays Help]', WText=ID
    DisplayVars.Handles.Base_Help = ID
    ;** Update Dial structure
    DialTag, 'display', tag='Handles', set=DisplayVars.Handles
  endif

end; PRO Dial_Display_Event_Help_Edit

;***************************************************************
;** Display information about Display options
pro Dial_Display_Event_Displays_Help, DisplayVars, DisplayHelp=DisplayHelp

  ToDisplay =['Options=</log,/scatter,/histogram,/polar>', $
              '        </add,/overlay,/catenate,/unactivate,/exact,/no,/delete>', $
              '        <IX=[expr], IY=[expr], Name_IX="...", Name_IY="..."', $
              '         Comment="...", Auto_Load="...">', $
              '   sets some display options to use. ', $
              '   options that affect Data', $
              '      IX=[expr] sets the expression/variable to use as X axis values (for 1D/2D data sets)', $
              '      IY=[expr] sets the expression/variable to use as Y axis values (for 2D maps)', $
              '      Name_IX="..." sets the name of X axis (when IX is given)', $
              '      Name_IY="..." sets the name of Y axis (when IY is given)', $
              '      Name_Data="..." sets the name of the Data set (default is the Variable name)', $
              '      Auto_Load="..." sets the expression to be evaluated before each update', $
              '      Comment="..." sets a comment to appear on plot', $
              '   options that affect the way data is stored. Default is to replace data.', $
              '      /add: to add (sum-up) new data with the existing one', $
              '      /overlay: to display new data set on the same axis system', $
              '      /catenate: to append new data with previous one', $
              '      /unactivate: do not update/display data', $
              '      /delete: delete data set/display', $
              '      /no: remove the other options from data/plot options list', $
              '   options that affect the way the display appears', $
              '      /scatter: only plots symbols (default is line and symbols)', $
              '      /histogram: plots data as an histogram (steps)', $
              '      /polar: plots data in polar coordinates, when IX is given', $
              '      /detached: when a new plot will be created, it will appear as a separate window', $
              '      /exact: the [XYZ] axis will be tight around data' $
              ]

  if n_elements(DisplayHelp) ne 0 then begin
    DisplayHelp = ToDisplay
    return
  endif

  ToDisplay = [ 'Dial Display Options', '', $
                'These options are to be used when calling the "display" command:', '', $
                '    display, Data=<data set>, Name_Data="<Data set Name>", [Options]', $
                'with:', '', $
                ToDisplay, $
                '']

  widget_control, DisplayVars.Handles.Base_Help, set_value=ToDisplay, Bad_ID=Bad_ID, show=1, iconify=0, tlb_set_title='Display Options Help'
  if Bad_ID ne 0 or DisplayVars.Handles.Base_Help eq 0 and n_elements(update) eq 0 then  begin
    xdisplayfile, 'dummy', text=ToDisplay, Group=DisplayVars.Handles.Base, $
          title='Display Options Help',$
          Height=20,Width=80,Done_Button='Close [Displays Help]', WText=ID
    DisplayVars.Handles.Base_Help = ID
    ;** Update Dial structure
    DialTag, 'display', tag='Handles', set=DisplayVars.Handles
  endif

end; PRO Dial_Display_Event_Displays_Help


;***************************************************************
; enable to change preferences for Dial Displays
pro Dial_Display_Event_Preferences, DisplayVars

  Preferences = {Display_Preferences, $
            OrientRow:DisplayVars.Global.OrientRow,$
            Gathered:DisplayVars.Global.Gathered,$
            Verbosity:DisplayVars.Global.Verbosity, $
            Plot:DisplayVars.Plot, $
            Frequency:DisplayVars.Frequency}

  Dial_Display_Event_Help_Preferences, DisplayVars, update=1

  Dialog_Fields, Preferences, Group=DisplayVars.Handles.Base, COLUMN_WIDTHS=500, $
    Name='Please enter new Display Preferences', Title='Preferences for Displays', $
    y_scroll_size=n_elements(tag_names(Preferences)), $
    FieldNames = [ 'Orient Row', 'Gathered','Verbosity', 'History size', 'Frequency' ]

  DisplayVars.Global.OrientRow = Preferences.OrientRow
  DisplayVars.Global.Gathered  = Preferences.Gathered
  DisplayVars.Plot             = Preferences.Plot
  DisplayVars.Frequency        = Preferences.Frequency
  LevelList      = ['silent','normal','verbose','debug']
  tmp1 = where(strcmp(LevelList, Preferences.Verbosity))
  if tmp1(0) ne -1 then $
    DisplayVars.Global.Verbosity = Preferences.Verbosity $
  else $
    Light_LogMessage, DisplayVars, 'normal','[w] Verbosity can be: silent, normal, verbose, debug'

  ;** Update Dial structure
  DialTag, 'display', tag='Global',  set=DisplayVars.Global
  DialTag, 'display', tag='Frequency',  set=DisplayVars.Frequency
  DialTag, 'display', tag='Plot',  set=DisplayVars.Plot


end; PRO Dial_Display_Event_Preferences

;***************************************************************
; update all displays
pro Dial_Display_Event_Update_All, DisplayVars

  Displays = DisplayVars.Displays
  DisplayVars.Global.Update = 1;

  for index=0,DisplayVars.Displays.Length-1 do begin
    Display = Dial_Display_Get_Index(DisplayVars.Displays, index)
    Display.Update = 2
    Dial_Display_Set_Index, Structure=Displays, Index=index, Value=Display
  endfor

  Tools_StructSetValue, struct=DisplayVars, tag='Displays', val=Displays
  DialTag, 'display', tag='Displays',  set=Displays

  Dial_Display_Event_Update, DisplayVars

end; PRO Dial_Display_Event_Update_All

;***************************************************************
; update displays that have their Display.Update =1
pro Dial_Display_Event_Update, DisplayVars

  widget_control, DisplayVars.Handles.Base, bad_id=tmp1
  if tmp1 ne 0 then begin
    ; Dial_Display_Event_Close, DisplayVars
    return
  endif

  Displays = DisplayVars.Displays

  for index=0,DisplayVars.Displays.Length-1 do begin

    Display = Dial_Display_Get_Index(DisplayVars.Displays, index)
    ; execute autoload for Each Data set if exists
    for d_index = 0, Display.Length-1 do begin
      This_Data = Dial_Display_Get_Index(Display, d_index)
      if strlen(This_Data.Auto_Load) gt 0 then begin
        ok = execute(This_Data.Auto_Load)
        if ok ne 0 then begin
          if n_elements(IX) ne 0 or n_elements(IY) ne 0 or n_elements(Data) ne 0 $
          or n_elements(Auto_Load) ne 0 or n_elements(Type) ne 0 or n_elements(Options) ne 0 then begin
            Dial_Display_Add_Data, DisplayVars, IX=IX, IY=IY, Data=Data, Name_Data=This_Data.Name_Data, Type=Type, Options=Options, Auto_Load=Auto_Load
            Display = Dial_Display_Get_Index(DisplayVars.Displays, index)
          endif
        endif
      endif
    endfor  ; d_index

    ; then update Displays if requested (DisplayVars.Global.Update)

    if Display.Update ne 0 and DisplayVars.Global.Update ne 0 then begin
      Dial_Display_LogMessage, DisplayVars, 'debug','[I] Dial_Display_Event_Update: Updating Display '+string(index)+' "'+Display.Name+'":'+' '+Display.ID+' ('+Display.Type+') Update level='+strtrim(string(Display.Update),2)
      IsLogScale = 0
      Comment = ""
      Name_IX = ""
      Name_IY = ""
      Name_Data = ""
      Comments = ""
      for d_index = 0, Display.Length-1 do begin
        if DisplayVars.Global.AutoSave ne 0 then Dial_Display_Event_Save_Data_Image, DisplayVars, Display_Index=Index, Data_Index=D_Index, filename='auto'
        This_Data = Dial_Display_Get_Index(Display, d_index)
        Data = This_Data.Data
        SizeData = size(Data)
        Name_Data = This_Data.Name_Data
        if strpos(This_Data.Options, 'log') ge 0 and n_elements(Data) ge 0 then begin
          if Display.Type eq 'contour' or Display.Type eq 'image' then begin
            tmp1 = where(Data gt 0)
            if tmp1(0) lt 0 then begin
              Data = data+1e-37
              tmp1 = where(Data gt 0)
            endif
            minData = min(Data(tmp1))
            tmp1 = where(Data le 0)
            if tmp1(0) ge 0 then Data(tmp1) = (tmp1*0)+minData
            Data= alog10(Data)
          endif
          IsLogScale = 1
        endif
        if strlen(strjoin(This_Data.Comment)) gt 0 then $
          if strlen(Comments(0)) eq 0 then Comments = This_Data.Comment $
          else  Comments = [ Comments, This_Data.Comment ]
        ok = execute('Data'+strtrim(string(d_index),2)+' = Data')
        if d_index eq 0 then begin
          X = This_Data.IX
          Y = This_Data.IY
          Comment = This_Data.Comment
          Name_IX = This_Data.Name_IX
          Name_IY = This_Data.Name_IY
          Names = Name_Data
          if strlen(Name_IX) gt 0 and n_elements(X) ne SizeData(1) then X = lindgen(SizeData(1))
          if strlen(Name_IY) gt 0 and n_elements(Y) ne SizeData(2) then Y = lindgen(SizeData(2))
          minX = min(X) & maxX = max(X)      & if minX eq maxX then maxX = minX + 1e-3
          minY = min(Y) & maxY = max(Y)      & if minY eq maxY then maxY = minY + 1e-3
          minD = min(Data) & maxD=max(Data)  & if minD eq maxX then maxD = minD + 1e-3
          ;if strpos(This_Data.Options, 'log') then Name_IX = Name_IX+' (log)'
          Indep = ''
          if Display.Type ne 'text' and strpos(This_Data.Options,'exact') ge 0 then Indep = Indep+', XRange=[minX, maxX], YRange=[minY, maxY]'
          if Display.Type eq 'plot' then begin
            ToName = '{Data:Names, I:Name_IX}'
            if SizeData(1) eq n_elements(X) then Indep = Indep+', INDEPENDENT=X'
            minY = minD & maxY=maxD
          endif else begin
            if Display.Type ne 'image' then begin
              ToName = '{Data:Names, IX:Name_IX, IY:Name_IY}'
              if Display.Type ne 'text' and strpos(This_Data.Options,'exact') ge 0 then Indep = Indep+', ZRange=[minD, maxD]'
              if SizeData(1) eq n_elements(X) then Indep = Indep+', XINDEPENDENT=X'
              if SizeData(2) eq n_elements(Y) then Indep = Indep+', YINDEPENDENT=Y'
            endif else ToName = '{Data:Names}'
          endelse
        endif else Names = [ Names, Name_Data]
        ok = execute('Name = '+ToName)
        if strcmp(Display.Type, 'text') ne 0 then begin
          if d_index eq 0 then ToDisplay = Data0 $
          else ok = execute('ToDisplay = [ ToDisplay, Data'+strtrim(string(d_index),2)+']')
        endif else begin
          if d_index eq 0 then ToDisplay = 'Data0' $
          else ok = execute("ToDisplay = ToDisplay+', Data"+strtrim(string(d_index),2)+"'")
        endelse
      endfor  ; d_index
      if Display.Length ne 0 then begin
        case Display.Type of
        'text': begin
                  widget_control, long(Display.ID), set_value=ToDisplay, Bad_ID=Bad_ID
                  if Bad_ID ne 0 and long(Display.ID) ne 0 then begin
                    xdisplayfile, 'dummy', text=ToDisplay, $
                    Group=DisplayVars.Handles.Base, /Editable, title=Display.Name, Done_Button='Close ['+Display.Name+']', WText=ID
                    Display.ID = strtrim(string(ID),2)
                  endif
                end
        else:   begin
                  Error = ''
                    ; Update = 1: keep current live view, and just update the data block (faster)
                    ; Update = 2: destroy current view and rebuild a new one at the same place, with present values
                  if Display.Update eq 1 and Display.Length eq 1 then begin
                    ; fast update when only Data changes:
                    ; require to use same Variable Name as Data Name
                    ok = execute(Display.Name+' = Data0')
                    ok = execute('live_control, '+Display.Name+', WINDOW_IN=Display.ID, Error=Error, /update_data')
                    if ok eq 0 or strlen(Error) ne 0 then begin
                      Display.Update = 2
                      Dial_Display_LogMessage, DisplayVars, 'verbose','[E] Dial_Display_Event_Update (fast)'+Error
                    endif
                  endif
                  if Display.Update eq 2 or Display.Length gt 1 then begin
                   if strlen(Display.vis) gt 0 then ok = execute('live_destroy, "'+Display.vis+'", WINDOW_IN=Display.ID, Error=Error')
                    if strlen(Error) ne 0 then Dial_Display_LogMessage, DisplayVars, 'verbose','[E] Dial_Display_Event_Update (destroy)'+Error
                    ; handle specific options
                    if Display.Type eq 'plot' and size(This_Data, /type) eq 8 then begin
                      if strpos(This_Data.Options, 'scatter') ge 0 then ToDisplay = ToDisplay+', /scatter' $
                      else if strpos(This_Data.Options, 'polar') ge 0 and strpos(Indep, 'INDEPENDENT=X') then ToDisplay = ToDisplay+', /polar' $
                      else if strpos(This_Data.Options, 'histogram') ge 0 then ToDisplay = ToDisplay+', /histogram'
                    endif
                    ok = execute('live_'+Display.Type+', '+ToDisplay+', WINDOW_IN=Display.ID, Name=Name, Reference_out=R_out, Error=Error, Title=Display.Name '+Indep)
                    if strlen(Error) ne 0 then begin
                      ; recreate that Display (external window that was closed, Gathered must be 0 for that)
                      if DisplayVars.Global.Gathered eq 0 then begin
                        ok = execute('live_'+Display.Type+', '+ToDisplay+', Name=Name, Reference_out=R_out, Error=Error, Title=Display.Name, DRAW_DIMENSIONS=[200, 200] '+Indep)
                      endif else begin
                        ok = execute('live_'+Display.Type+', '+ToDisplay+', Name=Name, Reference_out=R_out, Error=Error, Title=Display.Name, PARENT_BASE=DisplayVars.Handles.DisplayPanel, DRAW_DIMENSIONS=[200, 200] '+Indep)
                      endelse
                      if strlen(Error) ne 0 then begin
                        Dial_Display_LogMessage, DisplayVars, 'normal','[E] Dial_Display_Event_Update (rebuild)'+Error
                        Dial_Display_LogMessage, DisplayVars, 'normal','[I] Dial_Display_Event_Update Removing Invalid Data Set '+string(d_index)+' in '+string(index)
                        Dial_Display_Delete_Data, DisplayVars, Display_Index=index, Data_Index=D_Index
                      endif

                    endif
                    ; now update axis/titles
                    if strlen(Error) eq 0 then begin
                      Display.ID = R_out.win
                      if strlen(R_out.win) eq 0 then $
                        Display.ID = R_out.vis
                      Display.Vis = R_out.vis

                      if Display.Type ne 'image' then begin
                        ModifiedX = 0
                        ModifiedY = 0
                        ModifiedZ = 0
                        live_info, R_out.xaxis, WINDOW_IN=Display.ID, Error=Error, Properties=XaxisProp
                        live_info, R_out.yaxis, WINDOW_IN=Display.ID, Error=Error, Properties=YaxisProp
                        if Display.Type ne 'plot' and Display.Type ne 'contour' then begin
                          live_info, R_out.zaxis, WINDOW_IN=Display.ID, Error=Error, Properties=ZaxisProp
                          if strpos(Indep, 'ZRange') ge 0 and ZaxisProp.exact ne 1 then begin
                            ModifiedZ       = 1
                            ZaxisProp.exact = 1
                          endif
                        endif
                        if Display.Type eq 'plot' and Display.Length eq 1 and Display.Update eq 2 then begin
                          Colors = ['Red','Green','Blue','Magenta','Cyan','Dark Gray','Brown','Light Red','Light Green','Light Blue','Light Magenta']
                          info = {color:Colors(randomu(seed, /long) mod n_elements(Colors))}
                          live_control, R_out.graphic, properties=info, window_in=Display.ID
                        endif

                        if strpos(Indep, 'XRange') ge 0 and XaxisProp.exact ne 1 then begin
                          ModifiedX       = 1
                          XaxisProp.exact = 1
                        endif
                        if strpos(Indep, 'YRange') ge 0 and YaxisProp.exact ne 1 then begin
                          ModifiedY       = 1
                          YaxisProp.exact = 1
                        endif

                        if IsLogScale then begin
                          if Display.Type eq 'plot' or Display.Type eq 'contour' then begin
                            if (YaxisProp.axistitle ne Name_IY+' (log)' and YaxisProp.axistitle ne Name_Data+' (log)') or YaxisProp.log ne 1 then begin
                              ModifiedY     = 1
                              YaxisProp.log = 1
                              if strlen(Name_IY) gt 0 then YaxisProp.axistitle = Name_IY+' (log)' $
                              else YaxisProp.axistitle = Name_Data+' (log)'
                            endif
                          endif else begin
                            if ZaxisProp.axistitle ne Name_Data+' (log)' or ZaxisProp.log ne 1 then begin
                              ModifiedZ     = 1
                              ZaxisProp.log = 1
                              ZaxisProp.axistitle = Name_Data+' (log)'
                            endif
                          endelse
                        endif else begin
                          if Display.Type ne 'plot' and Display.Type ne 'contour' then begin
                            if ZaxisProp.axistitle ne Name_Data then begin
                              ModifiedZ      = 1
                              ZaxisProp.axistitle = Name_Data
                            endif
                          endif else if Display.Type ne 'contour' and YaxisProp.axistitle ne Name_IY then begin
                              ModifiedY      = 1
                              YaxisProp.axistitle = Name_IY
                            endif
                        endelse

                        if ModifiedX ne 0 then live_control, R_out.xaxis, WINDOW_IN=Display.ID, Error=Error, Properties=XaxisProp
                        if ModifiedY ne 0 then live_control, R_out.yaxis, WINDOW_IN=Display.ID, Error=Error, Properties=YaxisProp
                        if ModifiedZ ne 0 then live_control, R_out.zaxis, WINDOW_IN=Display.ID, Error=Error, Properties=ZaxisProp

                      endif else begin
                        live_info,    R_out.graphic, properties=info, window_in=Display.ID
                        if info.palette ne DisplayVars.Global.DefaultColorMap and Display.Update eq 2 then begin
                          info.palette          = DisplayVars.Global.DefaultColorMap
                          info.SIZING_CONSTRAINT= 2
                          live_control, R_out.graphic, properties=info, window_in=Display.ID
                        endif
                      endelse
                      if strlen(strjoin(Comment)) gt 0 then begin
                        live_text, Comments, WINDOW_IN=Display.ID, Error=Error, Location=[0.3,0.9]
                      endif
                    endif else Dial_Display_LogMessage, DisplayVars, 'verbose','[E] Dial_Display_Event_Update (rebuild)'+Error
                  endif ; whole update
                end
        endcase
      endif ; Display.Length
      Display.Update = 0
      widget_control, DisplayVars.Handles.Base_About, Bad_ID=Bad_ID
      ; Bad_ID: If no error occurs, a zero is stored.
      if Bad_ID eq 0 and DisplayVars.Handles.Base_About ne 0 then Dial_Display_Event_Show_List, DisplayVars, /no_raise

      n_index = index+1
      n_index = n_index-1
      Dial_Display_Set_Index, Structure=Displays, Index=n_index, Value=Display

    endif ; Display.Update ne 0


  endfor  ; index
  DisplayVars.Global.Update = 0;
  Tools_StructSetValue, struct=DisplayVars, tag='Displays', val=Displays
  DialTag, 'display', tag='Displays',  set=Displays

end; PRO Dial_Display_Event_Update

;***************************************************************
; ask for a given data set/display to be saved as an image file/text file or to delete
; other calls with Type='text' [to save as a text] and Type='close' to delete a DataSet/Display
; filename can be given for automatic Data save as type='text'. Provide Display_Index/Data_Index/Name_Data
pro Dial_Display_Event_Save_Data_Image, DisplayVars, Display_Index=Display_Index, Data_Index=Data_Index, Type=Type, filename=filename, Name_Data=Name_Data

  if n_elements(type) eq 0 then  type ='image'
  if n_elements(Name_Data) gt 0 then begin
    Name_Data       = strjoin(strsplit(Name_Data,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')
    if n_elements(Data_Index) eq 0 and strlen(Name_Data) gt 0 then $
      Data_Index = Dial_Display_Get_Data(DisplayVars, Data_Name, Display_Index=Display_Index)
  endif

  if n_elements(Display_Index) eq 0 then begin
    Dial_Display_Get_List, DisplayVars, DataList=DataList, DataArr=DataArr, DisplayArr=DisplayArr
    if n_elements(DataList) eq 0 then begin
      Dial_Display_LogMessage, DisplayVars, 'verbose','[E] Dial_Display_Event_Save_Data_Image: no active Display '
      return
    endif
    title = 'Select a Display to export as an Image'
    if type eq 'close' then title = 'Select a Display to Close/Delete' $
    else if type eq 'text' then title = 'Select a Display to export as Text'

    UserChoice = xlist(DataList, title=title)
    if strlen(UserChoice) gt 0 then begin
      index = where(UserChoice eq DataList)
      index = index(0)
      if index eq -1 then begin
        Dial_Display_LogMessage, DisplayVars, 'verbose','[E] Dial_Display_Event_Save_Data_Image: '+UserChoice+' ?'
        return
      endif
      Display_Index = DisplayArr(index)
      Data_Index    = DataArr(index)
    endif else return
  endif

  if Display_Index lt 0 or Display_Index ge DisplayVars.Displays.length then begin
    Dial_Display_LogMessage, DisplayVars, 'verbose','[E] Dial_Display_Event_Save_Data_Image: invalid Display index '+string(Display_Index)
    return
  endif

  Display = Dial_Display_Get_Index(DisplayVars.Displays, Display_Index)

  if n_elements(filename) ne 0 then  type ='text'

  if type eq 'close' then begin
    if Data_Index ge 0 then $
      Dial_Display_Delete_Data, DisplayVars, Display_Index=Display_Index, Data_Index=Data_Index $
    else $
      Dial_Display_Delete_Display, DisplayVars, Display_Index=Display_Index
    return
  endif

  if type eq 'image' or type eq 'postscript' then begin
    if Display.Type eq 'text' then begin
      Dial_Display_LogMessage, DisplayVars, 'normal','[E] Dial_Display_Event_Save_Data_Image: can not save text Display as an image'+string(index)+' "'+Display.Name+'":'+' '+Display.ID+' ('+Display.Type+')'
      return
    endif
    filename = strjoin(strsplit(Display.Name,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')
    if type eq 'image' then begin
      live_export, WINDOW_IN=Display.ID, /dialog, filename=filename+'.jpg'
    endif else begin
      live_print, WINDOW_IN=Display.ID, /dialog, Error=Error, /vector
    endelse
    Dial_Display_LogMessage, DisplayVars, 'verbose','[E] Dial_Display_Event_Save_Data_Image: save Display as an image/postscript '+string(index)+' "'+Display.Name+'":'+' '+Display.ID+' ('+Display.Type+')'
    return
  endif

  if type eq 'text' then begin
    if Data_Index lt 0 then begin
      for d_index = 0, Display.Length-1 do $ ; for all Data Sets
        Dial_Display_Event_Save_Data_Image, DisplayVars, Display_Index=Display_Index, Data_Index=d_index, Type='text
    endif else begin ; for an existing Data Set
      This_Data = Dial_Display_Get_Index(Display, Data_Index)
      sData = size(This_Data.Data, /dim)
      if n_elements(filename) ne 0 then $
        if filename eq 'auto' then $
          if Display.Name ne This_Data.Name_Data then filename = strjoin(strsplit(Display.Name+This_Data.Name_Data,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')+'.dat' $
          else filename = strjoin(strsplit(This_Data.Name_Data,' /\;*?$!~<>{}()|[]%&', /extract),'_')+'.dat'
      if n_elements(filename) eq 0 then begin
        if Display.Name ne This_Data.Name_Data then filename = strjoin(strsplit(Display.Name+This_Data.Name_Data,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')+'.dat' $
          else filename = strjoin(strsplit(This_Data.Name_Data,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')+'.dat'
        filename = dialog_pickfile(Dialog_Parent=DisplayVars.Handles.Base, file=filename, filter='*.dat', $
            Get_Path=Selected_Path, Title='Saving Data "'+This_Data.Name_Data+'" as Text')
      endif
      if n_elements(filename) eq 0 then return
      Header = [ '# Display: '+string(Display_Index)+' "'+Display.Name+'":'+' '+Display.ID+' ('+Display.Type+')', $
                 '# Data:    '+string(Data_Index)+' "'+This_Data.Name_Data+'" ['+strtrim(strjoin(string(sData)),2)+'] elements', $
                 '# Comment: '+strjoin(This_Data.Comment,' '), $
                 '# Date:    '+systime(0)+' [Started '+systime(0, This_Data.Date)+'='+strtrim(string(This_Data.Date, format='(I11)'),2)+']', $
                 '# Exported by Dial Display '+DisplayVars.Global.Version ]
      ToExport = Header
      if Display.Type ne 'text' then begin
        ; translate IX, IY, Data to text
        Data = This_Data.Data
        IX = This_Data.IX
        IY = This_Data.IY
        Data = string(Data)
        if n_elements(sData) eq 2 then begin
          Data = strjoin(transpose(Data))
          for index = 0, sData(1)-1 do ToExport = [ ToExport, Data(index) ]
        endif else ToExport = [ ToExport, strjoin(Data) ]
        if sData(0) eq size(IX, /n_elements) then begin
          IX = strjoin(string(IX),/single)
          ToExport = [ ToExport, '# X axis values ('+This_Data.Name_IX+')', IX ]
        endif
        if n_elements(sData) gt 1 then $
          if sData(1) eq size(IY, /n_elements) then begin
            IY = strjoin(string(IY),/single)
            ToExport = [ ToExport, '# Y axis values ('+This_Data.Name_IY+')', IY ]
          endif
      endif else begin
        ToExport = [ ToExport, This_Data.data ]
      endelse
      OPENW, unit, FILENAME, /GET_LUN, ERROR=i    ;open the file and then
      if i lt 0 then begin    ;OK?
        a = [ !error_state.msg, filename + ' could not be opened for writing.']  ;No
        void = DIALOG_MESSAGE(a, /ERROR, DIALOG_PARENT=wText)
      endif else begin
        ON_IOERROR, done_writing
        ; print out each line separately in order to get desired line breaks
        for j = 0, N_ELEMENTS(ToExport)-1 do PRINTF, unit, ToExport[j]
        done_writing:
        ON_IOERROR, null
        FREE_LUN, unit        ;free the file unit.
      endelse
      Dial_Display_LogMessage, DisplayVars, 'verbose','[I] Dial_Display_Event_Save_Data_Image: save Data as text '+filename

    endelse
    return
  endif

end; PRO Dial_Display_Event_Save_Data_Image

;***************************************************************
; delete a given data set/display
pro Dial_Display_Event_Close_DisplayData, DisplayVars

  Dial_Display_Event_Save_Data_Image, DisplayVars, type='close'

end; PRO Dial_Display_Event_Close_DisplayData

;***************************************************************
; ask for a given data set/display to be saved as a text file or to delete
pro Dial_Display_Event_Save_Data_Text, DisplayVars

  Dial_Display_Event_Save_Data_Image, DisplayVars, type='text'

end; PRO Dial_Display_Event_Save_Data_Text

;***************************************************************
; ask for a given data set/display to be saved as a text file or to delete
pro Dial_Display_Event_Save_Data_PS, DisplayVars

  Dial_Display_Event_Save_Data_Image, DisplayVars, type='postscript'

end; PRO Dial_Display_Event_Save_Data_PS


;***************************************************************
; ask for a given data set/display to be saved as a text file or to delete
pro Dial_Display_Event_Delete_All, DisplayVars

; close all Live_displays and text windows
tmp = where(strmatch(tag_names(DisplayVars),'DISPLAYS'))
  if tmp(0) eq -1 then return
  for index=DisplayVars.Displays.Length-1, 0, -1 do begin
    Dial_Display_Delete_Display, DisplayVars, Display_Index=Index
  endfor

end; PRO Dial_Display_Event_Delete_All

;*****************************************
; start/stop auto save of updated data
pro Dial_Display_Event_AutoSave, DisplayVars

  if (DisplayVars.Global.AutoSave eq 0) then begin
    DisplayVars.Global.AutoSave = 1
    widget_control, DisplayVars.Handles.AutoSave, set_value='Auto Save: ON'
  endif else begin
    DisplayVars.Global.AutoSave = 0
    widget_control, DisplayVars.Handles.AutoSave, set_value='Auto Save: OFF'
  endelse
  DialTag, 'display', tag='Global',  set=DisplayVars.Global

end; PRO Dial_Display_Event_AutoSave

;*****************************************
; start/stop auto update of displays
pro Dial_Display_Event_AutoUpdate, DisplayVars

  if (DisplayVars.Global.AutoUpdate eq 0) then begin
    DisplayVars.Global.AutoUpdate = 1
    widget_control, DisplayVars.Handles.AutoUpdate, set_value='Auto Update: ON'
  endif else begin
    DisplayVars.Global.AutoUpdate = 0
    widget_control, DisplayVars.Handles.AutoUpdate, set_value='Auto Update: OFF'
  endelse
  DialTag, 'display', tag='Global',  set=DisplayVars.Global

end; PRO Dial_Display_Event_AutoUpdate

;***************************************************************
; Add/Set/Catenate new Data to a given Display/Data (user entry call)
pro Display, Display_Name=Display_Name, $
             IX=IX, IY=IY, Data=Data, $
             Name_IX=Name_IX, Name_IY=Name_IY, Name_Data=Name_Data, $
             Type = Type, Options=Options, Auto_Load=Auto_Load, unactivate=unactivate, $
             Add=Add, Overlay=Overlay, Catenate=Catenate, $
             Scatter=Scatter, Histogram=Histogram, Polar=Polar, Log=Log, $
             Comment=Comment, Update=Update, Delete=Delete, Gathered=Gathered, No=No, Exact=Exact, $
             Detached=Detached, DisplayHelp=DisplayHelp, save=save, group_leader=group_leader

  ; launch Dial_Display if not active yet
  DisplayVars = Dial_Display_Get_DisplayVars()
  if DisplayVars.Name ne 'display' or xregistered('Dial_Display_Interface') eq 0 then begin
    DialInit,  'display'
    DisplayVars = Dial_Display_Get_DisplayVars()
    DisplayVars.OnOff = 0
    if n_elements(group_leader) ne 0 then DisplayVars.Global.group_leader = group_leader
  endif
  widget_control, DisplayVars.Handles.Base, Bad_id = Bad_id
  if (Bad_ID ne 0 or DisplayVars.Handles.Base eq 0) then Dial_Display_Build, DisplayVars
  if DisplayVars.OnOff eq 0 then DialStart, 'display'

  if n_elements(Name_Data) gt 0 then Name_Data       = strjoin(strsplit(Name_Data,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')
  if n_elements(Display_Name) gt 0 then Display_Name = strjoin(strsplit(Display_Name,' /\;*?$!~<>{}()|[]%&.,', /extract),'_')

  if n_elements(DisplayHelp)       ne 0 then Dial_Display_Event_Displays_Help, DisplayVars, DisplayHelp=DisplayHelp
  if n_elements(unactivate) gt 0 then return
  if n_elements(save)       ne 0 and n_elements(Name_Data) ne 0 then Dial_Display_Event_Save_Data_Image, DisplayVars, type='text', filename=filename, Name_Data=Name_Data
  if n_elements(Detached)   ne 0 then Gathered = 0
  if n_elements(Gathered)   ne 0 then begin
    GatheredOld = DisplayVars.Global.Gathered
    DisplayVars.Global.Gathered = Gathered(0)
  endif

  if n_elements(Delete) gt 0 then begin
    if n_elements(Name_Data) eq 0 and n_elements(Display_Name) eq 0 then Dial_Display_Event_Delete_All, DisplayVars $
    else Dial_Display_Delete_Data, DisplayVars, Display_Name=Display_Name, Name_Data=Name_Data
    return
  endif

  Dial_Display_Add_Data, DisplayVars, Display_Name=Display_Name, $
                           IX=IX, IY=IY, Data=Data, $
                           Name_IX=Name_IX, Name_IY=Name_IY, Name_Data=Name_Data, $
                           Type = Type, Options=Options, Auto_Load=Auto_Load, $
                           Add=Add, Overlay=Overlay, Catenate=Catenate, $
                           Scatter=Scatter, Histogram=Histogram, Polar=Polar, Log=Log, $
                           Comment=Comment, No=No, Exact=Exact

  if n_elements(Update) gt 0 then begin
    DisplayVars.Global.Update = 1;
    Dial_Display_Event_Update, DisplayVars
  endif

  if n_elements(Gathered) ne 0 then begin
    DisplayVars.Global.Gathered = GatheredOld
  endif

  ;** Update Dial structure
  DialTag, 'display', tag='Global',  set=DisplayVars.Global
  DialTag, 'display', tag='Handles', set=DisplayVars.Handles
  DialTag, 'display', tag='Displays',set=DisplayVars.Displays

end; PRO Display

;*********************
PRO dial_display_macro, D
;*********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_display
;** This macro procedure is called by George every D.frequency seconds

    if D.init eq 0 then begin
      D.init = 1
      Dial_Display_Build, D
    endif
    if D.init eq 1 then begin
      if D.Global.AutoUpdate ne 0 then D.Global.Update = 1;
      Dial_Display_Event_Update, D
    endif

    ;V=DialNewValue()    ; get value to display on Dial display
    ;DialModValue, V ,tag='VALUE'    ;(or D.value=V if dimensions are correct)
end



;*********************
FUNCTION dial_display
;*********************
;**
;** The dial initialisation (and stucture definition)

  if xregistered('Dial_Display_Interface') ne 0 then begin
    DisplayVars = Dial_Display_Get_DisplayVars()
    Dial_Display_LogMessage, DisplayVars, 'normal','[E] Dial_Display_Event_Update: Dial_Display is already running !'
    widget_control, DisplayVars.Handles.Base, Bad_ID=Bad_ID, show=1, iconify=0
    return, DisplayVars
  endif else begin

   ;Dial Variables (Defaulted if not present in return statement)
   ;--------------
    GENERIC='mad'    ;connect to the mad-idl interface
    TYPE='monitor'   ;then V=DialNewValue() stands for V=DialNewValue(TYPE='monitor')
    ONOFF=1          ;state of the Dial 1=running
    FREQUENCY=60.0     ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used
    VALUE=0 ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var.
    PLOT=0L           ;-2=none 0=plot 1=surface 2=contour n>2 means show vector of last n scalar values
    UPPERLIM=0.      ;upper limit of the plot (LOWERLIM for lower limit)
    HISTORY=0        ;=1 to record values in file light.his
    Preset=0       ;if >0 then Dial is stopped after running Preset seconds
    WUPDATE=0        ;=1 to automaticaly update corresponding workspace, =-1 silent!
                     ;=2 to automaticaly update and plot workspace to the main window
                     ;   0,1,2 are set by pressing the left,middle,right mouse button on the dial snapshot
    INIT=0

    ;User Variables (Must be present in return statement to be available)
    ;-------------

    DataStruct = { IX:[0], IY:[0], Data:[0], $  ; Description and content of one data set
                Name_IX:'X', Name_IY:'Y', Name_Data:'', $
                Auto_Load:"", $     ; executed for each Frequency dial tic
                Date:systime(1), $  ; time of first load
                Comment:"", $       ; comment to be displayed
                Options:"" }        ; may contain Add, Overlay, Replace, Catenate, Scatter, Histogram, Polar

    DisplayStruct = { Name:"", $    ; Description and content of one display
                    ID:"", $        ; ID is either a widget handle or a Reference.win from Live tools
                    Vis:"", $        ; ID is a Reference.vis from Live tools
                    Type:"plot", $
                    Update:0L, $           ; true when Display should be redrawn (updated)
                    Length:0L, $         ; number of Data to show in that display
                    Element_0:DataStruct} ; each Display can show many data structure contents.

    Displays= { Length: 0L, $ ; number of active displays
                Element_0: DisplayStruct}   ; one for each display

    Global = { $
              OrientRow:0L, $ ; orient Panel as a Column/Row for false/true
              ParentBase:0L, $  ; ID of Widget_base to put Display in
              XSize:200, $    ; default starting size of panel
              YSize:200, $
              Update:0L, $  ; 1 when whole update is requested
              Gathered:1L, $  ; 0: Each Display is in a separate window, 1 gathered on same panel
              Version:'0.2.1 (Jul 19th, 2002)', $
              Author:'E. Farhi (c)ILL', $
              AutoSave:0L, $
              AutoUpdate:1L, $
              Verbosity:'normal',$
              group_leader:0L, $
              DefaultColorMap:'Hue Sat Value 2', $  ; Blue-Red(11), EOS B(27), GREEN-PINK(10), Hue Sat Value 2(22),  RAINBOW(13)
              EmptyData:DataStruct, $     ; used when creating a new data set record in display
              EmptyDisplay:DisplayStruct }; used when creating a new display sub-panel

    Handles = { DISPLAYS_HANDLES, $
                Base:0L, $        ; main window (if not inserted in a previously existing Widget_Base)
                DisplayPanel:0L, $  ; Widget_Base containing all displays
                Base_About:0L, $
                Base_Help:0L, $
                AutoUpdate:0L, $
                AutoSave:0L $
                }

    D = {generic:GENERIC,type:TYPE,value:VALUE,frequency:FREQUENCY,init:INIT, OnOff:OnOff, PLOT:Plot, Global:Global, Handles:Handles, Displays:DisplayStruct}
    return, D

  endelse
end

;***************************************************************
;*********************** End of Dial_Display.pro ***************
;***************************************************************
