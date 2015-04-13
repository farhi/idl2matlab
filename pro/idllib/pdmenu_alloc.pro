;
; $Id: pdmenu_alloc.pro,v 1.9 1995/01/20 19:41:01 tonyh Exp $
;
;  WidPdmenu
;   Widget Pull Down Menu class library
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  PDMENU_Icon
;       Return the pulldown menu toolbar icon
;
FUNCTION PDMENU_Icon
  RETURN, [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 255b, 247b, 223b, 255b ], $
    [ 1b, 20b, 80b, 128b ], $
    [ 1b, 20b, 80b, 128b ], $
    [ 249b, 148b, 83b, 158b ], $
    [ 249b, 148b, 83b, 158b ], $
    [ 1b, 20b, 80b, 128b ], $
    [ 1b, 20b, 80b, 128b ], $
    [ 255b, 247b, 255b, 255b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 144b, 127b, 16b ], $
    [ 0b, 144b, 127b, 16b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 144b, 255b, 19b ], $
    [ 0b, 144b, 255b, 19b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 144b, 231b, 19b ], $
    [ 0b, 144b, 231b, 19b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 144b, 255b, 17b ], $
    [ 0b, 144b, 255b, 17b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 16b, 0b, 16b ], $
    [ 0b, 240b, 255b, 31b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
END


;
;  PDMENU_Copy
;   Copy a pdmenu.
;   Method 1: Copies contents out out ptr and into copy, destroying ptr.
;   Method 2: Duplicates ptr including submenus
;
PRO PDMENU_Copy, Ptr, Copy

  COMMON WidEd_Comm

    IF KEYWORD_SET(Copy) THEN BEGIN     ; Copy is already allocated
        Ptr2Obj, Copy, Contents
        SubPtrs = Contents.Value.SubMenuPtr
        FOR I=1,N_ELEMENTS(SubPtrs)-1 DO BEGIN
            IF(SubPtrs(I) NE 0L) THEN PDMENU_Destroy, Copy, /LEAVE_DIALOGS
        ENDFOR

        Ptr2Obj, Ptr, Obj               ; Remove object from original pointer
        Obj2Ptr, Obj, Copy              ; Store in copy pointer
        WIDGET_CONTROL, Ptr, /DESTROY

    ENDIF ELSE BEGIN

        Ptr2Obj, Ptr, Obj, /COPY                ; Copy original ptr contents
        Copy    = WIDGET_BASE(GROUP=TopDlg)     ; Make a new pointer

        ;       Copy submenus

        SubPtrs = Obj.Value.SubMenuPtr
        FOR I=1,N_ELEMENTS(SubPtrs)-1 DO BEGIN
            IF SubPtrs(I) NE 0L THEN BEGIN
                ClearVar, NewChild
                PDMENU_Copy, SubPtrs(I), NewChild
                SubPtrs(I)      = NewChild
            ENDIF
        ENDFOR
        Obj.Value.SubMenuPtr    = SubPtrs

        Obj2Ptr, Obj, Copy              ; Store copy into new pointer
    ENDELSE
END


;
;  PDMENU_Destroy
;   Release resources for the given pdmenu
;
PRO PDMENU_Destroy, Ptr, LEAVE_DIALOGS=ClearDlg

    ClearDlg    = KEYWORD_SET(ClearDlg) ; Make keyword 0/1, never <UNDEFINED>
    Ptr2Obj, Ptr, Obj                   ; Get object

    IF N_ELEMENTS(Obj) EQ 0 THEN RETURN

    ; Reset Dialog Pointer to prevent destruction?
    IF ClearDlg THEN Obj.Dialog = 0L

    IF WIDGET_INFO(Obj.Dialog, /VALID) THEN $           ; Valid Dialog?
        WIDGET_CONTROL, Obj.Dialog, /DESTROY            ; Destroy it

    SubPtrs     = Obj.Value.SubMenuPtr                  ; Destroy submenus
    FOR I=1,N_ELEMENTS(SubPtrs)-1 DO BEGIN
        IF(SubPtrs(I) NE 0L) THEN $
            PDMENU_Destroy, SubPtrs(I), LEAVE_DIALOGS=ClearDlg
    ENDFOR

    WIDGET_CONTROL, Ptr, /DESTROY                       ; Destroy pointer

    ; Obj is local variable and is freed on return
END


;
;  PDMENU_MakeStrVal
;       Create a string array based on a menu's contents
;       appropriate for placement inside a menu list box.
;
;       Precede submenus with a '*' notation.
;
PRO PDMENU_MakeStrVal, Obj, Val
    Strs        = Obj.Value.Str
    Ptrs        = Obj.Value.SubMenuPtr
    Val         = STRARR(N_ELEMENTS(Strs))
    Val(0)      = "<Top Of List>"
    FOR I=1, N_ELEMENTS(Strs)-1 DO BEGIN
        IF Ptrs(I) NE 0L THEN Prefix = "* " ELSE Prefix = "  "
        Val(I)  = Prefix + Strs(I)
    ENDFOR
END


;
;  PDMENU_Event
;   Event handling routine for a pdmenu dialog.  Shares common code
;   (c.f. widbuild.pro)
;
PRO PDMENU_Event, Event

  COMMON WidEd_Comm

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev                 ; Get Event
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo, /NO_COPY   ; Get Dialog info
    Ptr2Obj, Binfo.ObjPtr, Obj                              ; Get Object

    CASE Ev OF

    'NAME':     Obj.Name        = Event.Value
    'FONT':     Obj.Font        = Event.Value
    'UVALUE':   Obj.UValue      = Event.Value

    ;   User has selected an item in list.  The first item cannot
    ;   be deleted or used as a submenu (just a place holder so we
    ;   can add after it)
    'LIST':     BEGIN
        Binfo.Current   = Event.Index
        WIDGET_CONTROL, Binfo.Delete, SENSITIVE=(Event.Index NE 0)
        WIDGET_CONTROL, Binfo.SubMenu, SENSITIVE=(Event.Index NE 0)
        END

    'XFONT':    DoXFont, Obj, Binfo.Foci(1)

    'DELETE':   BEGIN

        ;       If the item being deleted is a submenu, there are
        ;       resources associated with it and those must be released
        DelItem = Obj.Value(Binfo.Current)
        IF DelItem.SubMenuPtr NE 0 THEN PDMENU_Destroy, DelItem.SubMenuPtr

        ;       Create a new value array

        Last    = N_ELEMENTS(Obj.Value)-1
        IF Binfo.Current EQ Last THEN BEGIN
                Vals    = Obj.Value(0:Last-1)
        ENDIF ELSE BEGIN
                Vals    = [ Obj.Value(0:Binfo.Current-1), $
                                Obj.Value(Binfo.Current+1:Last) ]
        ENDELSE


        ;       Redefine object to reflect new value.
        NewObj = {                      $
            Type:       Obj.Type,       $
            Parent:     Obj.Parent,     $
            Id:         Obj.Id,         $
            Dialog:     Obj.Dialog,     $
            Next:       Obj.Next,       $
            Name:       Obj.Name,       $
            Font:       Obj.Font,       $
            UValue:     Obj.UValue,     $
            Value:      Vals            $
        }

        ;       Update list box to reflect changes
        ;       Set list box selection to <Top of List>
        ;       Have to desensitize delete/Add as Submenu buttons

        PDMENU_MakeStrVal, NewObj, Strs
        WIDGET_CONTROL, Binfo.ListId, SET_VALUE=Strs
        WIDGET_CONTROL, Binfo.ListId, SET_LIST_SELECT=0
        Binfo.Current   = 0
        WIDGET_CONTROL, Binfo.Delete, SENSITIVE=0
        WIDGET_CONTROL, Binfo.SubMenu, SENSITIVE=0
        Obj     = NewObj
        END

    'ADDID':    BEGIN
        ;       User has typed in the 'Button value:' field
        ;       Consider <CR> to be the same as the 'Add' button
        ;       Of course, don't add nil buttons

        WIDGET_CONTROL, Binfo.AddId, GET_VALUE=Val
        Sens    = (Val(0) NE '')
        WIDGET_CONTROL, Binfo.Add, SENSITIVE=Sens
        IF Sens AND Event.Update THEN GOTO, AddEquiv
        END

    'ADD':      BEGIN
        WIDGET_CONTROL, Binfo.AddId, GET_VALUE=Val
    AddEquiv:

        ;       If the user hasn't chosen where they want to add
        ;       the new value, add it to the end of the list

        IF Binfo.Current NE -1 THEN     N = Binfo.Current       $
        ELSE                            N = N_ELEMENTS(Obj.Value)-1

        Last    = N_ELEMENTS(Obj.Value)-1
        Vals    = Obj.Value
        NewVal  = { WE_MENUITEM, Val(0), 0L }
        IF N EQ Last THEN BEGIN
             Vals       = [ Vals, NewVal ]
        ENDIF ELSE BEGIN
             Vals       = [ Vals(0:N), NewVal, Vals(N+1:*) ]
        ENDELSE

        ;       Redefine object to reflect addition

        NewObj = {                      $
            Type:       Obj.Type,       $
            Parent:     Obj.Parent,     $
            Id:         Obj.Id,         $
            Dialog:     Obj.Dialog,     $
            Next:       Obj.Next,       $
            Name:       Obj.Name,       $
            Font:       Obj.Font,       $
            UValue:     Obj.UValue,     $
            Value:      Vals            $
        }

        ;       Update list box to reflect change as well
        PDMENU_MakeStrVal, NewObj, Strs
        WIDGET_CONTROL, Binfo.ListId, SET_VALUE=Strs
        Obj             = NewObj
        Binfo.Current   = N+1

        ;       Set it to be the current selection
        WIDGET_CONTROL, Binfo.ListId, SET_LIST_SELECT=N+1
        WIDGET_CONTROL, Binfo.Delete, SENSITIVE=1
        WIDGET_CONTROL, Binfo.SubMenu, SENSITIVE=1

        ;       Clear the 'Button Value:' field after adding
        ;       a value.  Its easier for the user

        WIDGET_CONTROL, Binfo.AddId, SET_VALUE=''
        WIDGET_CONTROL, Binfo.Add, SENSITIVE=0
        END

    'SUBMENU':  BEGIN
        ;       Build a submenu dialog (and allocate submenu if
        ;       necessary).
        SUBMENU_Build, Binfo.ObjPtr, Obj, Binfo.Current

        ;       Update parent (us) list box to reflect possible change
        ;       of state.
        PDMENU_MakeStrVal, Obj, Strs
        WIDGET_CONTROL, Binfo.ListId, SET_VALUE=Strs
        WIDGET_CONTROL, Binfo.ListId, SET_LIST_SELECT=Binfo.Current
        END

    'DONE':     BEGIN
        Accept, Obj, Binfo.ObjPtr
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        WIDGET_CONTROL, Event.Top, /DESTROY
        RETURN
        END

    'CANCEL':   BEGIN
        Cancel, Obj, Binfo.ObjPtr
        RETURN
        END
    ELSE:           MESSAGE, 'Unprocessed event: ' + Ev
    ENDCASE

    Dirty   = 1

    SetNextFocus, Binfo, Event      ; Set next keyboard focus as necessary
    Obj2Ptr, Obj, Binfo.ObjPtr      ; Put object back into pointer
    WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
END


;
;  SUBMENU_Build
;   Create a dialog box a pdmenu submenu object.  If ptr is nil then
;   create the object as well.
;
;   The dialog and the object description contain unused fields.  They
;   are there so that we can use the PDMENU_Event and PDMENU_Alloc
;   routines on both menus and submenus.  Theres not many fields to begin
;   with and I feel the memory expense is justifyable
;
PRO SUBMENU_Build, ParPtr, ParObj, SubIdx

  COMMON WidEd_Comm

    ; Lookup pointer in parent menu item table.  We need to pull it
    ; out into a named variable for the alloc routine.
    ; If the pointer was nil, now we need to put it back into the list
    ; Otherwise, we did a little extra work

    Ptr = ParObj.Value(SubIdx).SubMenuPtr
    PDMENU_Alloc, ParPtr, Ptr
    ParObj.Value(SubIdx).SubMenuPtr     = Ptr

    MgrName = 'WE_PDMENU' + STRTRIM(Ptr, 2) ; Create dialog box name
    IF XRegistered(MgrName) THEN RETURN     ; See if it already exists

    ;  Use the name of the submenu for generating title

    Title   = 'SubMenu ' + ParObj.Value(SubIdx).Str

    Ptr2Obj, Ptr, Obj

    ;   Create dialog box

    Base    = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg)

    Foci    = 0L

    Base1   = WIDGET_BASE(Base, /ROW, /FRAME)

    PDMENU_MakeStrVal, Obj, Val
    List    = WIDGET_LIST(Base1, /FRAME, YSIZE=8, VALUE=Val, UVALUE='LIST')
    Base2   = WIDGET_BASE(Base1, /FRAME, /COLUMN)
    SubMenu = WIDGET_BUTTON(Base2, VALUE='Edit as a SubMenu', UVALUE='SUBMENU')
    Delete  = WIDGET_BUTTON(Base2, VALUE='Delete', UVALUE='DELETE')
    Add     = WIDGET_BUTTON(Base2, VALUE='Add', UVALUE='ADD')
    AddFld  = Field(Base2, "Button Value:", "", 'ADDID', SIZE=20, /STRING)

    Dummy   = WIDGET_LABEL(Base, VALUE=" ")
    Done    = WIDGET_BUTTON(Base, VALUE='Done', UVALUE='DONE')

    DlgInfo     = {             $
        ListId:         List,   $
        SubMenu:        SubMenu,$
        Delete:         Delete, $
        Add:            Add,    $
        AddId:          AddFld, $
        Current:        -1,     $
        Foci:           Foci,   $
        ObjPtr:         Ptr     $
    }
    Obj.Dialog  = Base

    WIDGET_CONTROL, SubMenu, SENSITIVE=0
    WIDGET_CONTROL, Delete, SENSITIVE=0
    WIDGET_CONTROL, Add, SENSITIVE=0
    WIDGET_CONTROL, List, SET_LIST_SELECT=0
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='PDMENU_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


;
;  PDMENU_Build
;   Create a dialog box a pdmenu object.  If ptr is nil then
;   create the object as well.
;
PRO PDMENU_Build, Ptr, ParPtr

  COMMON WidEd_Comm

    PDMENU_Alloc, ParPtr, Ptr               ; Allocate object if necessary
    MgrName = 'WE_PDMENU' + STRTRIM(Ptr, 2) ; Create dialog box name
    IF XRegistered(MgrName) THEN RETURN     ; See if it already exists

    Title   = GetId(Ptr) + '(Child of ' + GetId(ParPtr) + ')'
    Ptr2Obj, Ptr, Obj

    ;   Create dialog box

    Base    = WIDGET_BASE(/COLUMN, TITLE=Title, GROUP_LEADER=TopDlg)
    Foci    = LONARR(3)

    ;   Event Related Info

    Base1   = WIDGET_BASE(Base, /ROW, /FRAME)

    PDMENU_MakeStrVal, Obj, Val
    List    = WIDGET_LIST(Base1, /FRAME, YSIZE=8, VALUE=Val, UVALUE='LIST')
    Base2   = WIDGET_BASE(Base1, /FRAME, /COLUMN)
    SubMenu = WIDGET_BUTTON(Base2, VALUE='Edit as a SubMenu', UVALUE='SUBMENU')
    Delete  = WIDGET_BUTTON(Base2, VALUE='Delete', UVALUE='DELETE')
    Add     = WIDGET_BUTTON(Base2, VALUE='Add', UVALUE='ADD')
    AddFld  = Field(Base2, "Button Value:", "", 'ADDID', SIZE=20, /STRING)


    Foci(0) = Field(Base, "Name:", Obj.Name, 'NAME', SIZE=50, /STRING)
    Base1   = WIDGET_BASE(Base,/ROW)
    Foci(1) = Field(Base1, "Font:", Obj.Font, 'FONT', SIZE=50, /STRING)
    IF !Version.OS NE 'Win32' AND !Version.OS NE 'MacOS' THEN $
        XFontBtn    = WIDGET_BUTTON(Base1, VALUE="XFont", UVALUE="XFONT")
    Foci(2) = Field(Base, "User Value:", Obj.UValue, 'UVALUE', SIZE=30,/STRING)

    BuildOkCancel, Base, Obj

    DlgInfo     = {             $
        ListId:         List,   $
        SubMenu:        SubMenu,$
        Delete:         Delete, $
        Add:            Add,    $
        AddId:          AddFld, $
        Current:        -1,     $
        Foci:           Foci,   $
        ObjPtr:         Ptr     $
    }
    Obj.Dialog  = Base

    WIDGET_CONTROL, SubMenu, SENSITIVE=0
    WIDGET_CONTROL, Delete, SENSITIVE=0
    WIDGET_CONTROL, Add, SENSITIVE=0
    WIDGET_CONTROL, List, SET_LIST_SELECT=0
    WIDGET_CONTROL, Base, SET_UVALUE=DlgInfo, /NO_COPY
    WIDGET_CONTROL, Base, /REALIZE
    XMANAGER, MgrName, Base, EVENT_HANDLER='PDMENU_Event', CLEANUP='MISC_Kill'
    Obj2Ptr, Obj, Ptr
END


PRO PDMENU_MakeDesc, Obj, MenuDesc, Idx

    IF N_ELEMENTS(MenuDesc) EQ 0 THEN BEGIN
        MenuDesc        = REPLICATE( { CW_PDMENU_S, 0, '' }, 1000 )
        Idx             = 0
    ENDIF

    N   = N_ELEMENTS(Obj.Value)
    IF N EQ 1 THEN BEGIN
        MenuDesc(Idx).Name      = '<Empty Menu>'
        MenuDesc(Idx).Flags     = 2
        Idx                     = Idx + 1
    ENDIF ELSE BEGIN
        FOR I=1,N-1 DO BEGIN
            MenuDesc(Idx).Name  = Obj.Value(I).Str
            Last                = Idx
            Idx                 = Idx + 1
            SubPtr              = Obj.Value(I).SubMenuPtr
            IF SubPtr NE 0L THEN BEGIN
                MenuDesc(Last).Flags    = 1
                Ptr2Obj, SubPtr, SubObj
                PDMENU_MakeDesc, SubObj, MenuDesc, Idx
                Obj2Ptr, SubObj, SubPtr
            ENDIF
        ENDFOR

        ; Set the 'final entry' bit in the last entry of each menu/submenu
        MenuDesc(Last).Flags    = MenuDesc(Last).Flags OR 2
    ENDELSE
END


;
;  PDMENU_Save
;   Save pdmenu information to a file.
;   This is a simple object to save.
;
PRO PDMENU_Save, Unit, Ptr

  COMMON WidEd_Comm

    ON_IOERROR, BadWrite
    Ptr2Obj, Ptr, Obj
    WRITEU, Unit, N_ELEMENTS(Obj.Value) ; Save sizeof(value)
    WRITEU, Unit, Obj                   ; Save basic information

    ;   Save any submenus
    SubPtrs     = Obj.Value.SubMenuPtr
    FOR I=1,N_ELEMENTS(SubPtrs)-1 DO BEGIN
        IF SubPtrs(I) NE 0L THEN BEGIN
            PDMENU_Save, Unit, SubPtrs(I)
        ENDIF
    ENDFOR

    Obj2Ptr, Obj, Ptr
    RETURN

  BadWrite:
    Dirty   = 2
END


;
;  PDMENU_Restore
;   Read in a pdmenu object from a file
;
PRO PDMENU_Restore, Unit, Parent, Ptr

    NItem       = 0
    READU, Unit, NItem
    PDMENU_Alloc, Parent, Ptr, N_ITEMS=NItem
    Ptr2Obj, Ptr, Obj
    READU, Unit, Obj

    SubPtrs     = Obj.Value.SubMenuPtr
    FOR I=1,N_ELEMENTS(SubPtrs)-1 DO BEGIN
        IF SubPtrs(I) NE 0L THEN BEGIN
            ClearVar, Child
            PDMENU_Restore, Unit, Ptr, Child
            SubPtrs(I)  = Child
        ENDIF
    ENDFOR
    Obj.Value.SubMenuPtr        = SubPtrs

    Obj2Ptr, Obj, Ptr
END


;
;  PDMENU_Generate
;   Create a pdmenu object for previewing
;
PRO PDMENU_Generate, Base, Ptr

    Ptr2Obj, Ptr, Obj
    Id  = 0L            ; Prevent EXECUTE from creating a new variable

    ;   Build a command string

    PDMENU_MakeDesc, Obj, MenuDesc

    Cmd = 'Id = CW_PDMENU(Base, MenuDesc'
    SAddCmd, Cmd, Obj.Font, 'FONT'

    Obj2Ptr, Obj, Ptr

    ; Create pdmenu by executing the command string we just built

    IF EXECUTE(Cmd+')') NE 1 THEN BEGIN
        MESSAGE,'Could not create Pdmenu ' + VarName(Ptr)
    ENDIF
END


;
;  PDMENU_GenWid
;   Create IDL code for creating a PDMENU
;
PRO PDMENU_GenWid, Unit, Ptr, Parent

    Name    = VarId(Ptr)                ; Get variable name of object
    Ptr2Obj, Ptr, Obj                   ; Get object info

    DescName    = 'MenuDesc' + STRTRIM(Ptr,2)   ; Create value name

    PDMENU_MakeDesc, Obj, MenuDesc

    XPRINTF, Unit, '  ' + DescName + ' = [ $'

    ;   Now print out the contents of the description,
    ;   indenting to indicate what is a child of what
    ;   Note that if the final object is also a submenu
    ;   one need to keep track of that fact.

    ;   Its a little non-intuitive how the indentation stuff works
    ;   But basically, we keep track of the current indent level
    ;   and how it will effect things for the NEXT item

    MenuIdx     = 0     ; Current index in the Description array
    Level       = 1     ; Current indent level
    PreDec      = 0     ; Indent level(s) not yet acknowledged

    REPEAT BEGIN
        ;       Every item but the first one needs a comma and
        ;       a continuation character appended to the line above

        IF MenuIdx NE 0 THEN XPRINTF, Unit, ', $ ; ', MenuIdx-1

        ;       Indent 4 characters + 2 characters for every level
        ;       of indentation after the first

        FOR I=-1,LEVEL DO XPRINTF, Unit, FORMAT='(A)', "  ", /NO_EOL
        Flags = MenuDesc(MenuIdx).Flags
        XPRINTF, Unit, Flags, Qstring(MenuDesc(MenuIdx).Name), /NO_EOL, $
                FORMAT='("{ CW_PDMENU_S, ", I, ", ''",A,"'' }")'

        ;       See how the current object affects the level

        CASE Flags OF
        0:      ;
        1:      Level   = Level + 1
        2:      BEGIN
                Level   = Level - 1 - PreDec
                PreDec  = 0
                END
        3:      BEGIN
                Level   = Level + 1
                PreDec  = PreDec + 1
                END
        ENDCASE

        ;       Go on to the next object

        MenuIdx = MenuIdx + 1

        ;       We know we are done when the level goes to 0.
        ;       This happens after the last object has been written

    ENDREP UNTIL LEVEL EQ 0

    XPRINTF, Unit, FORMAT='(" $  ;",I)', MenuIdx-1

    PRINTF, Unit, FORMAT='(/"  ]"//)'   ; XPRINTF does wierd stuff

        ;       Lastly, write the code which calls the
        ;       pulldown menu creation function with the description
        ;       we just wrote.

    XPRINTF, Unit, FORMAT='("  ", A," = CW_PDMENU( ", A, ", ", A)', $
        Name, Parent, DescName, /NO_EOL
    XPRINTF, Unit, FORMAT='(A)', ", /RETURN_FULL_NAME", /NO_EOL
    SSaveCmd, Unit, Obj.Font, 'FONT'
    SSaveCmd, Unit, UValue(Obj, Ptr), "UVALUE"
    XPRINTF, Unit, ')'

    Obj2Ptr, Obj, Ptr
END


;
;  SavePDCase
;       Generate a case statement for the given String.
;       If the Item is actually a submenu, then call
;       SavePDCase for each child.
;
;       Parent contains the concatenation of all of the previous
;       submenu names separated by periods. This is done to
;       match the event.value returned by the event procedure
;       when CW_PDMENU is called with the RETURN_FULL_NAME flag --
;       which we do.
;
PRO SavePDCase, Unit, Item, Parent


    IF Item.SubMenuPtr NE 0L THEN BEGIN
        NewParent       = Parent + Item.Str + '.'
        Ptr2Obj, Item.SubMenuPtr, Obj
        FOR I=1,N_ELEMENTS(Obj.Value)-1 DO BEGIN
            SavePDCase, Unit, Obj.Value(I), NewParent
        ENDFOR
        Obj2Ptr, Obj, Item.SubMenuPtr

    ENDIF ELSE BEGIN

        ;       Normal string. Build a statement of the form:
        ;       "<full-name>": BEGIN
        ;               PRINT, "Event for <full-name>"
        ;               END

        Name = Qstring(Parent + Item.Str)
        PRINTF, Unit, "  '" + Name + "': BEGIN"
        PRINTF, Unit, "    PRINT, 'Event for ", Name, "'"
        PRINTF, Unit, '    END'

    ENDELSE
END

;
;  PDMENU_MenuEv
;
;       Generate IDL code to handle a menu event
;
PRO PDMENU_MenuEv, Unit, OldUnit, Ptr

    Ptr2Obj, Ptr, Obj

    CASE Obj.Type OF

    'MAIN':     Goto, BaseType
    'DEP':      Goto, BaseType
    'BASE':     BEGIN
    BaseType:

        ;       If the user wants to provide an event handler, don't
        ;       do anything
        ;       Otherwise, look for pull down menus

        IF Obj.EventFunc EQ '' AND Obj.EventProc EQ '' THEN BEGIN
            DoFList2, Obj.children, 'PDMENU_MenuEv', Unit, OldUnit
        ENDIF

        END

    'PDMENU':   BEGIN

        ;       Found a pull down menu.  Create an event handler
        ;       routine for this menu.

        Obj2Ptr, Obj, Ptr
        Id      = VarId(Ptr)
        Ptr2Obj, Ptr, Obj

        IF FindMagic(Id, Unit, OldUnit) EQ 0 THEN BEGIN

            ; Write the routine header

            BeginMagic, Unit, Id

            PRINTF, Unit, FORMAT='(//"PRO ",A,"_Event, Event")', Id
            PRINTF, Unit, FORMAT='(//A//A/)',               $
                '  CASE Event.Value OF '

            FOR I=1,N_ELEMENTS(Obj.Value)-1 DO BEGIN
                SavePDCase, Unit, Obj.Value(I), ''
            ENDFOR

            ;       Write routine footer

            PRINTF, Unit, '  ENDCASE'
            PRINTF, Unit, 'END'

            EndMagic, Unit, Id

        ENDIF

        END
    ELSE:       ; Do nothing
    ENDCASE

    Obj2Ptr, Obj, Ptr
END


;
;  PDMENU_Alloc
;       Allocate a menu object.  Don't allocate if ptr is non-nil
;
;       If we are restoring an menu object it may have several
;       menu items already.  Allocate appropriately (N_ITEMS flag)
;
PRO PDMENU_Alloc, Parent, Ptr, N_ITEMS=NItem
  COMMON WidEd_Comm

    IF KEYWORD_SET(NItem) EQ 0 THEN NItem=1     ; Allocate 1 Item by dflt

    IF KEYWORD_SET(Ptr) NE 0 THEN RETURN    ; if(ptr != NULL) return;

    Val = REPLICATE( { WE_MENUITEM, '', 0L }, NItem )

    ;   No WE_PDMENU structure type
    ;   Menus must be typeless so they can have dynamically alterable
    ;   definition (Value changes)

    Ptr = WIDGET_BASE(GROUP=TopDlg)                         ; Make a pointer
    Obj = {                     $
        Type:           'PDMENU', $
        Parent:         Parent, $ ; Pointer to parent
        Id:             NewId(),$ ; Permanent Id
        Dialog:         0L,     $ ; Save Dialog ID (need for Cut consistency)
        Next:           0L,     $ ; index of next child/free/top
        Name:           '',     $ ; object name
        Font:           '',     $
        UValue:         '',     $
        Value:          Val     $
    }
    Obj2Ptr, Obj, Ptr
END
