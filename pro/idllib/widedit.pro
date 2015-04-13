;
; $Id: widedit.pro,v 1.8 1994/06/01 23:08:48 ali Exp $
;
;  WidEdit
;   Cut/Paste/Copy/Edit dialog routines.
;   These are the other 'Top Level' dialogs and as main, have many
;   externally visible hooks (c.f. wided.com)
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;
;


;
;  UpdateCut
;   Update the given dialog box (cut, copy or edit) to reflect a
;   change in the current widget tree.
;
;   General notes:
;
;   If we are looking at some portion of the widget tree and any portion
;   of that tree has been removed, give up and look at the top level.
;
;   If the widget tree is empty (widget tree consists of MAIN alone)
;   then close the dialog (nothing to cut copy or edit)
;
PRO UpdateCut, Id, Removed

  COMMON WidEd_Comm

    WIDGET_CONTROL, Id, GET_UVALUE=Binfo, /NO_COPY      ; Get dialog info

    OldList         = Binfo.Ptrs                        ; Get list aux info

    ;   See if someone has removed any of base objects in our parenting
    TreeDisturbed   = WHERE(Binfo.Parents EQ Removed, Count)
    IF Count NE 0 THEN BEGIN
    Again:
        Binfo.Parents   = TopPtr            ; Make top base current parent
        NewTop          = TopPtr
    ENDIF ELSE BEGIN
        NewTop          = Binfo.Parents     ; Use current parent
        NewTop          = NewTop(0)
    ENDELSE

    MakeLists, NewTop, List, Strs   ; Make list to reflect (possibly)
                                    ; new current parent

    ;   No items in list?
    IF List(0) EQ 0 THEN BEGIN

        ;   If we aren't looking at the top level base there still might
        ;   be something to look at anyway.  Look at the root of the tree
        ;   and try again.
        IF NewTop NE TopPtr THEN GOTO, Again

        WIDGET_CONTROL, Id, /DESTROY        ; Nothing.

        ;   Clear the appropriate common block id to show that this
        ;   dialog is no longer being shown

        IF Id EQ CutId THEN CutId = 0
        IF Id EQ CopyId THEN CopyId = 0
        IF Id EQ EditId THEN EditId = 0
        RETURN
    ENDIF

    ;   If the list has not been disturbed then leave it as it is.

    IF N_ELEMENTS(OldList) EQ N_ELEMENTS(List) THEN BEGIN
        Dummy   = WHERE(OldList NE List, Count)

        ;   If the list has not been disturbed, see if the current
        ;   selection is a base object and has had children added
        ;   or removed from it.  Update the ability to look at its
        ;   children accordingly.

        IF Count EQ 0 THEN BEGIN
            WIDGET_CONTROL, Binfo.ChildBtn, $
                SENSITIVE=HasChildren(List(Binfo.ListIdx))
            WIDGET_CONTROL, Id, SET_UVALUE=Binfo, /NO_COPY
            RETURN
        ENDIF
    ENDIF

    ;   The list has been altered.  Update it to reflect the new changes.

    ;   Redeclare the dialog information.  Because List is an array
    ;   and in all likelyhood is a different size than the original
    ;   Binfo.Ptrs we can't just use:  Binfo.Ptrs   = list


    Binfo   = {                     $
        Ptrs:List,                  $
        Parents:Binfo.Parents,      $
        LabelId:Binfo.LabelId,      $
        ListId:Binfo.ListId,        $
        ListIdx:0,                  $
        ActionBtn:Binfo.ActionBtn,  $
        PasteBtn:Binfo.PasteBtn,    $
        ChildBtn:Binfo.ChildBtn,    $
        ParentBtn:Binfo.ParentBtn   $
    }

    ;   Can't cut or copy DEP objects. Set cut/copy action button accordingly
    Child   = List(0)
    IF Id EQ CutId OR Id EQ CopyId THEN BEGIN
        GetType, Child, Type
        WIDGET_CONTROL, Binfo.ActionBtn, SENSITIVE=(Type NE "DEP")
    ENDIF

    ;   Set labels and controls to reflect our new list

    WIDGET_CONTROL, Binfo.LabelId, SET_VALUE='Children of '+VarId(NewTop)
    WIDGET_CONTROL, Binfo.ListId, SET_VALUE=Strs
    WIDGET_CONTROL, Binfo.ListId, SET_LIST_SELECT=0
    WIDGET_CONTROL, Binfo.ChildBtn, SENSITIVE=HasChildren(Child)
    WIDGET_CONTROL, Binfo.ParentBtn, SENSITIVE=(NewTop NE TopPtr)

    ;   Restore dialog information
    WIDGET_CONTROL, Id, SET_UVALUE=Binfo, /NO_COPY
END


;
;  UpdateEdit
;   Entry point to updating all of the cut/copy/paste/edit dialogs
;   to reflect a change in the widget tree.
;
PRO UpdateEdit, Removed

  COMMON WidEd_Comm

    ;   Get MAIN Base and dialog information

    Ptr2Obj, TopDlg, DlgInfo        ; So its not really a pointer
    Ptr2Obj, TopPtr, TopObj

    ;   Set the sensitivity of the main EDIT pulldown menu
    Sens    = ((TopObj.Children NE 0) OR (TopObj.Next NE 0))

    ;   Cut/copy/edit requires child widgets to be acted on

    WIDGET_CONTROL, DlgInfo.CutId, SENSITIVE=Sens
    WIDGET_CONTROL, DlgInfo.CopyId, SENSITIVE=Sens
    WIDGET_CONTROL, DlgInfo.EditId, SENSITIVE=Sens

    ;   Pasting requires that there be something to paste
    Sens    = (CutList NE 0L)
    WIDGET_CONTROL, DlgInfo.PasteId, SENSITIVE=Sens

    Obj2Ptr, TopObj, TopPtr     ; Restore info to top pointer
    Obj2Ptr, DlgInfo, TopDlg    ; Restore info to top dialog

    IF N_ELEMENTS(Removed) EQ 0 THEN Removed = 0L

    ;   Update dialogs

    IF CutId NE 0 THEN UpdateCut, CutId, Removed
    IF CopyId NE 0 THEN UpdateCut, CopyId, Removed
    IF EditId NE 0 THEN UpdateCut, EditId, Removed

    ;   Paste dialog is unique.  Special update code required.

    IF PasteId NE 0 THEN BEGIN      ; Do we even have a paste dialog visible?

        WIDGET_CONTROL, PasteId, GET_UVALUE=Binfo, /NO_COPY

        ;   Has a parent widget been removed?

        OldList         = Binfo.Ptrs
        TreeDisturbed   = WHERE(Binfo.Parents EQ Removed, Count)
        IF Count NE 0 THEN BEGIN
            Binfo.Parents   = TopPtr
            NewTop          = TopPtr
        ENDIF ELSE BEGIN
            NewTop          = Binfo.Parents
            NewTop          = NewTop(0)
        ENDELSE

        MakeLists, NewTop, List, Strs   ; Make list to reflect (possibly)
                                        ; new current parent

        ;   Paste list has an additional fake entry. the Top-Of-List.

        IF List(0) EQ 0 THEN BEGIN
            List    = -1L
            Strs    = '<Top of List>'
        ENDIF ELSE BEGIN
            List    = [ -1L, List]
            Strs    = [ '<Top of List>', Strs ]
        ENDELSE

        ;   See if the list remains undisturbed

        IF N_ELEMENTS(OldList) EQ N_ELEMENTS(List) THEN BEGIN
            Dummy   = WHERE(OldList NE List, Count)
            IF Count EQ 0 THEN GOTO, FixCutList
        ENDIF

        ;   Rebuild Dialog info to reflect new child list.
        ;   Note: the from list is built on the fly and not
        ;   reflected in the dialog information structure

        Binfo   = {                     $
            Ptrs:List,                  $
            Parents:Binfo.Parents,      $
            FromIdx:Binfo.FromIdx,      $
            ToIdx:0,                    $
            FromListId:Binfo.FromListId,$
            LabelId:Binfo.LabelId,      $
            ToListId:Binfo.ToListId,    $
            PasteBtn:Binfo.PasteBtn,    $
            ChildBtn:Binfo.ChildBtn,    $
            ParentBtn:Binfo.ParentBtn   $
        }

        ;   Set controls

        WIDGET_CONTROL, Binfo.LabelId, SET_VALUE='Children of '+VarId(NewTop)
        WIDGET_CONTROL, Binfo.ToListId, SET_VALUE=Strs
        WIDGET_CONTROL, Binfo.ToListId, SET_LIST_SELECT=0
        WIDGET_CONTROL, Binfo.ParentBtn, SENSITIVE=(NewTop NE TopPtr)

    FixCutList:
        ;   If we jumped here, something has changed but it is
        ;   outside of the visible list.  This can however, affect
        ;   the 'show children' action.

        WIDGET_CONTROL, Binfo.ChildBtn, $
            SENSITIVE=HasChildren(List(Binfo.ToIdx), /NONE_OK)


        ;   Rebuild the cutlist (From)

        FromStrs    = ""
        Ptr         = CutList
        WHILE Ptr NE 0 DO BEGIN
            IdStr   = GetId(Ptr)
            IF StrLen(IdStr) GT 20 THEN $
                IdStr = STRING(FORMAT='(A17,"...")', IdStr)
            FromStrs    = [ FromStrs, VarId(Ptr)+' ('+IdStr+')' ]
            Ptr         = NextPtr(Ptr)
        ENDWHILE

        ;   Show changes
        Binfo.FromIdx   = 0
        WIDGET_CONTROL, Binfo.FromListId, SET_VALUE=FromStrs(1:*)
        WIDGET_CONTROL, Binfo.FromListId, SET_LIST_SELECT=0


        ;   Save new dialog info
        WIDGET_CONTROL, PasteId, SET_UVALUE=Binfo, /NO_COPY
    ENDIF
END


;
;  MakeLists
;   Common routine that given a pointer to a base object, creates
;   2 parallel arrays:  One with the pointers to its children and the
;   other with a name list for the same children.
;
;   List            Strs
;   12              LABEL12 (Press Me)
;
PRO MakeLists, ParPtr, List, Strs

  COMMON WidEd_Comm

    Ptr2Obj, ParPtr, Obj

    ; Dependent bases are considered to be children of the MAIN object

    List    = 0L            ; Start lists
    Strs    = ""

    ;   Add true children first

    Ptr     = Obj.Children
    WHILE Ptr NE 0 DO BEGIN
        List    = [ List, Ptr]
        IdStr   = GetId(Ptr)
        IF StrLen(IdStr) GT 20 THEN $
            IdStr = STRING(FORMAT='(A17,"...")', IdStr)
        Strs    = [ Strs, VarId(Ptr)+' ('+IdStr+')' ]
        Ptr     = NextPtr(Ptr)
    ENDWHILE

    ;   If this is the MAIN base, add dependent bases to the list

    IF ParPtr EQ TopPtr THEN BEGIN
        Ptr     = Obj.Next
        WHILE Ptr NE 0 DO BEGIN
            List    = [ List, Ptr ]
            IdStr   = GetId(Ptr)
            IF StrLen(IdStr) GT 20 THEN $
                IdStr = STRING(FORMAT='(A17,"...")', IdStr)
            Strs    = [ Strs, VarId(Ptr)+' ('+IdStr+')' ]
            Ptr     = NextPtr(Ptr)
        ENDWHILE
    ENDIF

    ;   If we have at least one child, remove the fake entry used
    ;   to start the list.  Otherwise return the fake entry to
    ;   indicate that there are no children

    IF N_ELEMENTS(List) GT 1 THEN BEGIN
        List    = List(1:*)
        Strs    = Strs(1:*)
    ENDIF

    Obj2Ptr, Obj, ParPtr        ; Restore parent information
END


;
;  RemoveDialogs
;   Close any active dialog box associated with the given portion
;   of the object tree.
;   Note: Base objects have 2 possible dialog boxes.
;
PRO RemoveDialogs, Ptr

    Ptr2Obj, Ptr, Obj

    ;   If we have an active dialog box, destroy it

    IF WIDGET_INFO(Obj.Dialog, /VALID_ID) THEN BEGIN
        WIDGET_CONTROL, Obj.Dialog, /DESTROY
        Obj.Dialog  = 0L
    ENDIF

    ;   If we are a base object, see if we have an aux dialog box
    ;   and destroy it and destroy our children's dialog boxes as well

    IF TAG_NAMES(Obj, /STRUCTURE) EQ 'WE_BASE' THEN BEGIN
        IF WIDGET_INFO(Obj.AttrDlg, /VALID_ID) THEN BEGIN
            WIDGET_CONTROL, Obj.AttrDlg, /DESTROY
            Obj.AttrDlg = 0L
        ENDIF
        DoList, Obj.Children, "RemoveDialogs"
    ENDIF

    Obj2Ptr, Obj, Ptr
END


;
;  CutEvent
;   Handle events for the cut/copy/edit dialogs.  Naming is historical
;
PRO CutEvent, Event

  COMMON WidEd_Comm

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev         ; Get Event
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo     ; Get Dialog info

    CASE Ev OF

    'LIST': BEGIN
        ;   Get new current selection
        Idxs            = Binfo.Ptrs
        Child           = Idxs(Event.Index)
        Binfo.ListIdx   = Event.Index

        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo

        ;   Cannot cut or copy a Dependent base
        IF Event.Top EQ CutId OR Event.Top EQ CopyId THEN BEGIN
            GetType, Child, Type
            WIDGET_CONTROL, Binfo.ActionBtn, SENSITIVE=(Type NE "DEP")
        ENDIF

        ;   Can only look at the children of something that has children
        WIDGET_CONTROL, Binfo.ChildBtn, SENSITIVE=HasChildren(Child)
        END

    'Cut':  BEGIN
        Dirty       = 1
        Ptrs        = Binfo.Ptrs
        ChildPtr    = Ptrs(Binfo.ListIdx)
        Ptr2Obj, ChildPtr, Child            ; Get Child Info

        Parents     = Binfo.Parents
        ParentPtr   = Parents(0)
        Ptr2Obj, ParentPtr, Parent          ; Get Parent Info

        ;   First child?
        IF Binfo.ListIdx EQ 0 THEN BEGIN
            Parent.Children = Child.Next

            ;   Parent now has no children?
            IF Parent.Children EQ 0 THEN Parent.LastChild = 0
        ENDIF ELSE BEGIN
            PrevPtr = Ptrs(Binfo.ListIdx-1)     ; Get PrevPtr from list
            SetTag, PrevPtr, "Next", Child.Next ; Remove child from list

            ;   Was child lastchild of base?  Set LastChild to show change.
            IF Child.Next EQ 0 THEN Parent.LastChild = PrevPtr
        ENDELSE

        Child.Next  = CutList           ; Child goes onto cut list
        CutList     = ChildPtr
        Obj2Ptr, Child, ChildPtr        ; Save new child info
        Obj2Ptr, Parent, ParentPtr      ; Save new parent info

        RemoveDialogs, ChildPtr         ; Remove active dialog for child tree

        Count   = 1                     ; Cutlist has 10 objects at most
        Ptr     = CutList
        WHILE Ptr NE 0 AND Count LE 10 DO BEGIN
            Prev    = Ptr
            Ptr     = NextPtr(Ptr)
            Count   = Count + 1
        ENDWHILE

        IF Ptr NE 0 THEN BEGIN
            SetTag, Ptr, "Dialog", 0L   ; Don't destroy dialog box
            Destroy, Ptr                ; Destroy 11th item
            SetTag, Prev, "Next", 0L    ; Remove Destroyed item from CutList
        ENDIF
        UpDateEdit, ChildPtr            ; Show changes in cut/copy/paste
        ; We could have destroyed ourselves if we have deleted
        ; the last widget in the hierarchy
        IF CutId NE 0 THEN WIDGET_CONTROL, Binfo.PasteBtn, SENSITIVE=1
        IF CopyId NE 0 THEN BEGIN
            Ptr2Obj, CopyId, CopyInfo
            WIDGET_CONTROL, CopyInfo.PasteBtn, SENSITIVE=1
            Obj2Ptr, CopyInfo, CopyId
        ENDIF

        END

    'Copy': BEGIN
        Ptrs        = Binfo.Ptrs        ; Get Child pointer and type
        ChildPtr    = Ptrs(Binfo.ListIdx)
        GetType, ChildPtr, Type

        CALL_PROCEDURE, Type+"_Copy", ChildPtr, Copy    ; Copy it

        SetTag, Copy, "Next", CutList       ; Add copy to cut list
        CutList     = Copy

        Count       = 1                 ; Cutlist has 10 objects at most
        Ptr         = CutList
        WHILE Ptr NE 0 AND Count LE 10 DO BEGIN
            Prev    = Ptr
            Ptr     = NextPtr(Ptr)
            Count   = Count + 1
        ENDWHILE

        IF Ptr NE 0 THEN BEGIN
            SetTag, Ptr, "Dialog", 0L   ; Don't destroy dialog box
            Destroy, Ptr                ; Destroy 11th item
            SetTag, Prev, "Next", 0L    ; Remove Destroyed item from CutList
        ENDIF

        UpDateEdit                      ; Show changes
        WIDGET_CONTROL, Binfo.PasteBtn, SENSITIVE=1
        IF CutId NE 0 THEN BEGIN
            Ptr2Obj, CutId, CutInfo
            WIDGET_CONTROL, CutInfo.PasteBtn, SENSITIVE=1
            Obj2Ptr, CutInfo, CutId
        ENDIF

        END

    'EDIT': BEGIN
        Ptrs        = Binfo.Ptrs        ; Get child pointer
        ChildPtr    = Ptrs(Binfo.ListIdx)

        Parents     = Binfo.Parents     ; Get parent pointer
        ParentPtr   = Parents(0)

        GetType, ChildPtr, Type         ; Get child type

        ;   Dependent bases are a special case (unfortunately)
        ;   but take the same NUMBER of ARGUMENTS.
        ;   ParentPtr gets set to WIDGET_BASE for the dialog --
        ;   a fact we can ignore.  Just be aware.

        CALL_PROCEDURE, Type+"_Build", ChildPtr, ParentPtr

        ;   If the object being editted is not a base class object
        ;   then we can 'Cancel' it.  That is restore it to a previous
        ;   state.  Here is where we take care of that.  Take a snapshot
        ;   of the current state of the object (copy it) and store
        ;   that information in the active dialog list (NewDialogs)
        ;
        IF Type NE 'BASE' AND Type NE 'DEP' THEN BEGIN
            CALL_PROCEDURE, Type+"_Copy", ChildPtr, Copy

            ;   If we already have dialog in active list (just
            ;   popping it forward) then don't add it to the list

            Dummy       = WHERE(NewDialogs.ObjPtr EQ ChildPtr, Count)
            IF Count EQ 0 THEN BEGIN
                Active  = { WE_NEWOBJ, ParentPtr, ChildPtr, Copy }
                NewDialogs      = [ NewDialogs, Active ]
            ENDIF
        ENDIF
        END

    'CHILD':    BEGIN
        Ptrs        = Binfo.Ptrs            ; Get current pointer
        NewTop      = Ptrs(Binfo.ListIdx)
        Parents     = [ NewTop, Binfo.Parents ] ; Now its the current parent

        MakeLists, Parents(0), List, Strs       ; Make new list

        Binfo   = {                     $       ; Update dialog info
            Ptrs:List,                  $
            Parents:Parents,            $
            LabelId:Binfo.LabelId,      $
            ListId:Binfo.ListId,        $
            ListIdx:0,                  $
            ActionBtn:Binfo.ActionBtn,  $
            PasteBtn:Binfo.PasteBtn,    $
            ChildBtn:Binfo.ChildBtn,    $
            ParentBtn:Binfo.ParentBtn   $
        }

        ;   Set controls to reflect change

        WIDGET_CONTROL, Binfo.LabelId, SET_VALUE='Children of '+VarId(NewTop)
        WIDGET_CONTROL, Binfo.ListId, SET_VALUE=Strs
        WIDGET_CONTROL, Binfo.ListId, SET_LIST_SELECT=0

        Child   = List(0)           ; Get type of current (first) child
        GetType, Child, Type

        WIDGET_CONTROL, Binfo.ActionBtn, SENSITIVE=(Type NE "DEP")
        WIDGET_CONTROL, Binfo.ChildBtn, SENSITIVE=HasChildren(Child)
        WIDGET_CONTROL, Binfo.ParentBtn, SENSITIVE=1    ; Alway can go ..
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        END

    'PARENT':   BEGIN
        Parents     = Binfo.Parents(1:*)    ; New Parent list
        NewTop      = Parents(0)            ; New Top
        MakeLists, NewTop, List, Strs       ; New list of children

        Binfo   = {                     $   ; Update Dialog info
            Ptrs:List,                  $
            Parents:Parents,            $
            LabelId:Binfo.LabelId,      $
            ListId:Binfo.ListId,        $
            ListIdx:0,                  $
            ActionBtn:Binfo.ActionBtn,  $
            PasteBtn:Binfo.PasteBtn,    $
            ChildBtn:Binfo.ChildBtn,    $
            ParentBtn:Binfo.ParentBtn   $
        }

        ;   Set controls to reflect new list

        WIDGET_CONTROL, Binfo.LabelId, SET_VALUE='Children of '+VarId(NewTop)
        WIDGET_CONTROL, Binfo.ListId, SET_VALUE=Strs
        WIDGET_CONTROL, Binfo.ListId, SET_LIST_SELECT=0

        ;   Can't cut or copy a DEP base.
        Child   = List(0)
        IF Event.Top EQ CutId OR Event.Top EQ CopyId THEN BEGIN
            GetType, Child, Type
            WIDGET_CONTROL, Binfo.ActionBtn, SENSITIVE=(Type NE "DEP")
        ENDIF
        WIDGET_CONTROL, Binfo.ChildBtn, SENSITIVE=HasChildren(Child)
        WIDGET_CONTROL, Binfo.ParentBtn, SENSITIVE=(NewTop NE TopPtr)
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        END

    'DOPASTE':  PasteChild

    'DONE':     WIDGET_CONTROL, Event.top, /DESTROY

    ENDCASE
END

;
;  CutChild
;   Bring up Dialog box to allow user to look through
;   existing objects for the one they want to cut/copy
;
PRO CutChild, COPY=Copy

  COMMON WidEd_Comm

    ;   Which kind of dialog are we anyway?
    IF KEYWORD_SET(Copy) THEN Action='Copy' ELSE Action='Cut'

    MgrName = 'Wid' + Action + 'Top'            ; Already up?
    IF XRegistered(MgrName) THEN RETURN

    Title   = Action + ' Widget'
    Ptr2Obj, TopPtr, Obj
    IF Obj.Children EQ 0 AND Obj.Next EQ 0 THEN BEGIN
        ErrorDialog, TopDlg, ['Internal Error: ', $
            'There are no child widgets to ' + Action ]
        RETURN
    ENDIF
    Obj2Ptr, Obj, TopPtr

    ;   Start by using the top level base

    MakeLists, TopPtr, List, Strs

    ;   Build Dialog

    BaseId      = WIDGET_BASE(GROUP_LEADER=TopDlg, /COLUMN, TITLE=Title)
    LabelId     = WIDGET_LABEL(BaseId, VALUE='Children of ' + VarId(TopPtr))
    ListId      = WIDGET_LIST(BaseId, VALUE=Strs, YSIZE=10,UVALUE='LIST')
    ActionBtn   = WIDGET_BUTTON(BaseId, VALUE=Action +' Child', UVALUE=Action)
    ChildBtn    = WIDGET_BUTTON(BaseId, VALUE='Show Children', UVALUE='CHILD')

    Child   = List(0)       ; Can't cut/copy a DEP base
    GetType, Child, Type
    WIDGET_CONTROL, ActionBtn, SENSITIVE=(Type NE "DEP")
    WIDGET_CONTROL, ChildBtn, SENSITIVE=HasChildren(Child)

    ;   Top Base has no parent.  Create the button but disable it.
    ParentBtn   = WIDGET_BUTTON(BaseId, VALUE='Show Parent Base', $
            UVALUE='PARENT')
    WIDGET_CONTROL, ParentBtn, SENSITIVE=0

    PasteBtn    = WIDGET_BUTTON(BaseId, VALUE='Show Paste Dialog', $
                                UVALUE='DOPASTE')
    WIDGET_CONTROL, PasteBtn, SENSITIVE=(CutList NE 0L)

    Button      = WIDGET_BUTTON(BaseId, VALUE='Done', UVALUE='DONE')

    Binfo   = {                 $
        Ptrs:List,              $
        Parents: TopPtr,        $
        LabelId: LabelId,       $
        ListId: ListId,         $
        ListIdx: 0,             $
        ActionBtn: ActionBtn,   $
        PasteBtn: PasteBtn,     $
        ChildBtn: ChildBtn,     $
        ParentBtn: ParentBtn    $
    }
    WIDGET_CONTROL, BaseId, /REALIZE
    WIDGET_CONTROL, ListId, SET_LIST_SELECT=0
    WIDGET_CONTROL, BaseId, SET_UVALUE=Binfo, /NO_COPY
    XMANAGER, MgrName, BaseId, EVENT_HANDLER='CutEvent',CLEANUP='KillEdit'
    IF KEYWORD_SET(Copy) THEN CopyId = BaseId ELSE CutId = BaseId
END


;
;  EditChild
;   Allow user to get to dialog boxes they have dismissed previously.
;
PRO EditChild

  COMMON WidEd_Comm

    MgrName = 'WidEditTop'
    IF XRegistered(MgrName) THEN RETURN             ; Already up?

    Title   = 'Edit Widget'
    Ptr2Obj, TopPtr, Obj
    IF Obj.Children EQ 0 AND Obj.Next EQ 0 THEN BEGIN
        ErrorDialog, TopDlg, ['Internal Error: ', $
            'There are no child widgets to Edit' ]
        RETURN
    ENDIF
    Obj2Ptr, Obj, TopPtr

    ;   Start by showing the top level children.

    MakeLists, TopPtr, List, Strs

    ;   Build Dialog

    EditId  = WIDGET_BASE(GROUP_LEADER=TopDlg, /COLUMN, TITLE=Title)
    LabelId = WIDGET_LABEL(EditId, VALUE='Children of '+ VarId(TopPtr))
    ListId  = WIDGET_LIST(EditId, VALUE=Strs, YSIZE=10, UVALUE='LIST')
    EditBtn = WIDGET_BUTTON(EditId, VALUE='Edit Child', UVALUE='EDIT')
    ChildBtn    = WIDGET_BUTTON(EditId, VALUE='Show Children', UVALUE='CHILD')
    WIDGET_CONTROL, ChildBtn, SENSITIVE=HasChildren(List(0))
    ParentBtn   = WIDGET_BUTTON(EditId, VALUE='Show Parent Base', $
            UVALUE='PARENT')
    WIDGET_CONTROL, ParentBtn, SENSITIVE=0
    Button  = WIDGET_BUTTON(EditId, VALUE='Done', UVALUE='DONE')

    ;   We don't really use the ActionBtn or PasteBtn fields but we
    ;   need to have them so that the dialog information is identical
    ;   to that used by Cut/Copy dialogs (all 3 share a common event handler)

    Binfo   = {             $
        Ptrs:List,          $
        Parents: TopPtr,    $
        LabelId:LabelId,    $
        ListId:ListId,      $
        ListIdx:0,          $
        ActionBtn:EditBtn,  $
        PasteBtn:0L,        $
        ChildBtn:ChildBtn,  $
        ParentBtn:ParentBtn $
    }
    WIDGET_CONTROL, EditId, /REALIZE

    WIDGET_CONTROL, ListId, SET_LIST_SELECT=0
    WIDGET_CONTROL, EditId, SET_UVALUE=Binfo, /NO_COPY
    XMANAGER, MgrName, EditId,  $
        EVENT_HANDLER='CutEvent',CLEANUP='KillEdit'
END


;
;  PasteChildEvent
;   Event handler for the Paste dialog.
;
PRO PasteChildEvent, Event
  COMMON WidEd_Comm

    WIDGET_CONTROL, Event.Id, GET_UVALUE=Ev         ; Get Event
    WIDGET_CONTROL, Event.Top, GET_UVALUE=Binfo     ; Get Dialog Info

    CASE Ev OF

    'CUTLIST':  BEGIN
        Binfo.FromIdx   = Event.Index                   ; Set Currnt From Idx
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo
        END

    'PASTELIST':    BEGIN
        Idxs            = Binfo.Ptrs
        Child           = Idxs(Event.Index)
        Binfo.ToIdx     = Event.Index

        IF Child EQ -1L THEN Type = ';BOGUS' $         ; <Top of List> ?
        ELSE GetType, Child, Type                       ; Regular selection

        ;   Cant paste after a dependent base
        WIDGET_CONTROL, Binfo.PasteBtn, SENSITIVE=(Type NE 'DEP')
        ;   Can look in a base which has no children
        WIDGET_CONTROL, Binfo.ChildBtn, SENSITIVE=HasChildren(Child,/NONE_OK)

        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo     ; Update Dialog info
        END

    'PASTE':    BEGIN
        Ptrs        = Binfo.Ptrs            ; Get Parent pointer
        Parents     = Binfo.Parents
        ParentPtr   = Parents(0)
        ChildPtr    = Ptrs(Binfo.ToIdx)     ; Get child pointer

        Ptr         = CutList               ; Given From List index
        Count       = Binfo.FromIdx         ; figure out the current
        WHILE Count NE 0 DO BEGIN           ; item to paste
            Ptr     = NextPtr(Ptr)
            Count   = Count - 1
        ENDWHILE

        GetType, Ptr, Type                  ; Copy item to paste
        CALL_PROCEDURE, Type+"_Copy", Ptr, Dup
        SetTag, Dup, "Id", NewId()          ; Give it a new Id.

        ;   Add into child list

        IF ChildPtr EQ -1L THEN BEGIN        ;   First child?

            Ptr2Obj, ParentPtr, PObj

            SetTag, Dup, "Next", PObj.Children
            IF PObj.Children EQ 0 THEN PObj.LastChild = Dup
            PObj.Children    = Dup

            Obj2Ptr, PObj, ParentPtr

        ENDIF ELSE BEGIN

            SetTag, Dup, "Next", NextPtr(ChildPtr)  ; Dup->next = prev->next
            SetTag, ChildPtr, "Next", Dup           ; prev->next = dup

            ;   We keep track of last child.  If ChildPtr was the
            ;   last child it isn't any longer.  Change the Parent's
            ;   LastChild to reflect the addition

            Ptr2Obj, ParentPtr, PObj
            IF PObj.LastChild EQ ChildPtr THEN PObj.LastChild = Dup
            Obj2Ptr, PObj, ParentPtr

        ENDELSE

        UpdateEdit          ; Show changes in cut/paste/etc.
        END

    'CHILD':    BEGIN
        Ptrs        = Binfo.Ptrs                ; Current selection is now
        NewTop      = Ptrs(Binfo.ToIdx)         ; the current parent
        Parents     = [ NewTop, Binfo.Parents ]

        MakeLists, Parents(0), List, Strs

        List    = [ -1L, List]                  ; Add top-of-list item
        Strs    = [ '<Top of List>', Strs ]

        Binfo   = {                     $       ; Create dialog info
            Ptrs:List,                  $
            Parents:Parents,            $
            FromIdx:Binfo.FromIdx,      $
            ToIdx:0,                    $
            FromListId:Binfo.FromListId,$
            LabelId:Binfo.LabelId,      $
            ToListId:Binfo.ToListId,    $
            PasteBtn:Binfo.PasteBtn,    $
            ChildBtn:Binfo.ChildBtn,    $
            ParentBtn:Binfo.ParentBtn   $
        }

        ;   Set dialog controls to reflect current conditions

        WIDGET_CONTROL, Binfo.LabelId, SET_VALUE='Children of '+VarId(NewTop)
        WIDGET_CONTROL, Binfo.ToListId, SET_VALUE=Strs
        WIDGET_CONTROL, Binfo.ToListId, SET_LIST_SELECT=0
        WIDGET_CONTROL, Binfo.ChildBtn, SENSITIVE=0
        WIDGET_CONTROL, Binfo.ParentBtn, SENSITIVE=1    ; Alway can go ..
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        END

    'PARENT':   BEGIN
        Parents     = Binfo.Parents(1:*)   ; Go toward root widget 1 level
        NewTop      = Parents(0)
        MakeLists, NewTop, List, Strs

        List    = [ -1L, List]                  ; Add top-of-list item
        Strs    = [ '<Top of List>', Strs ]

        Binfo   = {                     $       ; Create dialog info
            Ptrs:List,                  $
            Parents:Parents,            $
            FromIdx:Binfo.FromIdx,      $
            ToIdx:0,                    $
            FromListId:Binfo.FromListId,$
            LabelId:Binfo.LabelId,      $
            ToListId:Binfo.ToListId,    $
            PasteBtn:Binfo.PasteBtn,    $
            ChildBtn:Binfo.ChildBtn,    $
            ParentBtn:Binfo.ParentBtn   $
        }

        ;   Set dialog controls to reflect current conditions

        WIDGET_CONTROL, Binfo.LabelId, SET_VALUE='Children of '+VarId(NewTop)
        WIDGET_CONTROL, Binfo.ToListId, SET_VALUE=Strs
        WIDGET_CONTROL, Binfo.ToListId, SET_LIST_SELECT=0
        WIDGET_CONTROL, Binfo.ChildBtn, SENSITIVE=0
        WIDGET_CONTROL, Binfo.ParentBtn, SENSITIVE=(NewTop NE TopPtr)
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        END

    'DELETE':   BEGIN

        ;       Cutting first item on the list?

        IF Binfo.FromIdx EQ 0 THEN BEGIN

            Item        = CutList
            CutList     = NextPtr(CutList)
            Destroy, Item

            ;  Nothing left to paste?  We are done.

            IF CutList EQ 0 THEN BEGIN
                ; Can't paste no more

                Ptr2Obj, TopDlg, DlgInfo
                WIDGET_CONTROL, DlgInfo.PasteId, SENSITIVE=0
                Obj2Ptr, DlgInfo, TopDlg 

                IF CutId NE 0 THEN BEGIN
                    Ptr2Obj, CutId, CutInfo
                    WIDGET_CONTROL, CutInfo.PasteBtn, SENSITIVE=0
                    Obj2Ptr, CutInfo, CutId
                ENDIF

                IF CopyId NE 0 THEN BEGIN
                    Ptr2Obj, CopyId, CopyInfo
                    WIDGET_CONTROL, CopyInfo.PasteBtn, SENSITIVE=0
                    Obj2Ptr, CopyInfo, CopyId
                ENDIF

                WIDGET_CONTROL, Event.top, /DESTROY
                RETURN
            ENDIF

        ENDIF ELSE BEGIN

            Ptr         = CutList           ; Given From List index
            Count       = Binfo.FromIdx     ; figure out the current
            WHILE Count NE 0 DO BEGIN       ; item to paste
                Prev    = Ptr
                Ptr     = NextPtr(Ptr)
                Count   = Count - 1
            ENDWHILE

            SetTag, Prev, "Next", NextPtr(Ptr)
            Destroy, Ptr

        ENDELSE

        ;   Create list entries based on Cutlist contents

        FromStrs    = ""
        Ptr         = CutList
        WHILE Ptr NE 0 DO BEGIN
            IdStr   = GetId(Ptr)
            IF StrLen(IdStr) GT 20 THEN $
                IdStr = STRING(FORMAT='(A17,"...")', IdStr)
            FromStrs    = [ FromStrs, VarId(Ptr)+' ('+IdStr+')' ]
            Ptr         = NextPtr(Ptr)
        ENDWHILE

        ;   Update list and binfo structure

        WIDGET_CONTROL, Binfo.FromListId, SET_VALUE=FromStrs(1:*)
        WIDGET_CONTROL, Binfo.FromListId, SET_LIST_SELECT=0
        Binfo.FromIdx   = 0
        WIDGET_CONTROL, Event.Top, SET_UVALUE=Binfo, /NO_COPY
        END

    'DONE':         WIDGET_CONTROL, Event.top, /DESTROY

    ENDCASE
END


;
;  PasteChild
;   Bring up a Dialog box to allow the user to paste(copy) objects
;   that are on the cutlist into the current widget tree.
;
PRO PasteChild

  COMMON WidEd_Comm      

    MgrName = 'WidPasteTop'
    IF XRegistered(MgrName) THEN RETURN         ; Already have dialog?

    Title   = 'Paste Widgets'
    IF CutList EQ 0L THEN BEGIN
        ErrorDialog, TopDlg, ['Internal Error: ', $
            'There are no child widgets to Paste' ]
        RETURN
    ENDIF

    ;   Create the dialog

    PasteId = WIDGET_BASE(GROUP_LEADER=TopDlg, /COLUMN, TITLE=Title)
    Base1   = WIDGET_BASE(PasteId, /ROW)
    Base2   = WIDGET_BASE(Base1, /COLUMN)
    Label   = WIDGET_LABEL(Base2, VALUE='Clipboard')

    ;   Create list entries based on Cutlist contents

    FromStrs    = ""
    Ptr         = CutList
    WHILE Ptr NE 0 DO BEGIN
        IdStr   = GetId(Ptr)
        IF StrLen(IdStr) GT 20 THEN $
            IdStr = STRING(FORMAT='(A17,"...")', IdStr)
        FromStrs    = [ FromStrs, VarId(Ptr)+' ('+IdStr+')' ]
        Ptr         = NextPtr(Ptr)
    ENDWHILE

    FromListId  = WIDGET_LIST(Base2, VALUE=FromStrs(1:*), YSIZE=10, $
            UVALUE='CUTLIST')
    Delete  = WIDGET_BUTTON(Base2, VALUE='Delete Widget', UVALUE='DELETE')
    Base2   = WIDGET_BASE(Base1, /COLUMN)
    LabelId = WIDGET_LABEL(Base2, VALUE='Children of '+VarId(TopPtr))

    MakeLists, TopPtr, List, Strs           ; Make list of top level children

    List    = [ -1L, List]                  ; Add top-of-list entry
    Strs    = [ '<Top of List>', Strs ]

    ToListId    = WIDGET_LIST(Base2, VALUE=Strs, YSIZE=10, $
            UVALUE='PASTELIST')
    PasteBtn    = WIDGET_BUTTON(Base2, VALUE='Paste After', UVALUE='PASTE')
    ChildBtn    = WIDGET_BUTTON(Base2, VALUE='Show Children', UVALUE='CHILD')
    WIDGET_CONTROL, ChildBtn, SENSITIVE=0
    ParentBtn   = WIDGET_BUTTON(Base2, VALUE='Show Parent Base', $
            UVALUE='PARENT')
    WIDGET_CONTROL, ParentBtn, SENSITIVE=0
    Button  = WIDGET_BUTTON(PasteId, VALUE='Done', UVALUE='DONE')

    Binfo   = {                 $               ; Create Dialog info
        Ptrs:List,              $
        Parents: TopPtr,        $
        FromIdx:0,              $
        ToIdx:0,                $
        FromListId:FromListId,  $
        LabelId:LabelId,        $
        ToListId:ToListId,      $
        PasteBtn:PasteBtn,      $
        ChildBtn:ChildBtn,      $
        ParentBtn:ParentBtn     $
    }
    WIDGET_CONTROL, PasteId, /REALIZE
    WIDGET_CONTROL, FromListId, SET_LIST_SELECT=0   ; Select item 0 of from
    WIDGET_CONTROL, ToListId, SET_LIST_SELECT=0     ; Select item 0 of to

    ;   Note:  We know that we can always paste the 1st cutlist item
    ;   after the top-of-list item.  Thats why we don't play with
    ;   PasteBtn sensitivity here

    WIDGET_CONTROL, PasteId, SET_UVALUE=Binfo, /NO_COPY
    XMANAGER, MgrName, PasteId, $
        EVENT_HANDLER='PasteChildEvent',CLEANUP='KillEdit'
END


;
;  KillEdit
;   Common kill routine for all cut/paste/etc. dialogs.  Reset common
;   block id for the appropriate dialog to show dialog has been destroyed.
;
PRO KillEdit, Id

  COMMON WidEd_Comm      

    IF Id EQ CutId THEN CutId = 0L
    IF Id EQ CopyId THEN CopyId = 0L
    IF Id EQ PasteId THEN PasteId = 0L
    IF Id EQ EditId THEN EditId = 0L
END

PRO WidEdit
END
