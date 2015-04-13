;
; $Id: widalloc.pro,v 1.7 1994/06/01 23:08:48 ali Exp $
;
;  WidAlloc
;   Widget Editor allocation related routines.
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;
; MODIFICATION HISTORY
;       Written by:     Joshua Goldstein,       12/93
;



;
;  Destroy
;   Generic function to cause an object to delete itself from memory.
;   This is assumed to be recursive (objects containing other objects
;   should destroy their children as well).
;
PRO Destroy, Ptr

    ;   An invalid pointer is considered to be already destroyed
    ;   Not much we could do anyway.
    IF WIDGET_INFO(Ptr, /VALID_ID) EQ 0 THEN RETURN

    ;   Figure out the class specific routine name and call it
    GetType, Ptr, Type
    CALL_PROCEDURE,Type+"_Destroy",Ptr
END


;
;  GenDestroy
;   Generic Object destruction routine.
;
PRO GenDestroy, Ptr, HASVALUE=HasValue
    Ptr2Obj, Ptr, Obj

    IF N_ELEMENTS(Obj) EQ 0 THEN RETURN

    IF KEYWORD_SET(HasValue) THEN $
        WIDGET_CONTROL, Obj.Value1, /DESTROY    ; Destroy value contents

    IF WIDGET_INFO(Obj.Dialog, /VALID) THEN $   ; Destroy dialog box
        WIDGET_CONTROL, Obj.Dialog, /DESTROY

    WIDGET_CONTROL, Ptr, /DESTROY               ; Release pointer memory
    ;   Object is in local variable and goes away as we return
END


;
;  GenCopy
;   Copy an object.  2 copy methods:
;
;   if( copy != NULL)       { *copy = *ptr; free(ptr); }
;   else                    { *(copy = malloc(...)) = *ptr; }
;
PRO GenCopy, Ptr, Copy, HASVALUE=HasValue
  COMMON WidEd_Comm

    IF KEYWORD_SET(Copy) THEN BEGIN     ; Copy is already allocated

        Ptr2Obj, Copy, ThrowAway        ; Release current copy contents

        IF KEYWORD_SET(HasValue) THEN $
            WIDGET_CONTROL, ThrowAway.Value1, /DESTROY  ; Destroy value too

        Ptr2Obj, Ptr, Obj               ; Remove object from original pointer
        Obj2Ptr, Obj, Copy              ; Store in copy pointer
        WIDGET_CONTROL, Ptr, /DESTROY   ; Release original pointer memory

    ENDIF ELSE BEGIN

        Ptr2Obj, Ptr, Obj, /COPY        ; Make a copy of ptr contents

        IF KEYWORD_SET(HasValue) THEN $
            Ptr2Obj, Obj.Value1, Value1, /COPY  ; Get copy of value1 contents

        Copy    = WIDGET_BASE(GROUP=TopDlg)     ; Make a new pointer

        IF KEYWORD_SET(HasValue) THEN BEGIN
            Obj.Value1  = WIDGET_BASE(GROUP=TopDlg) ; New ptr for value
            IF N_ELEMENTS(Value1) NE 0 THEN $
                Obj2Ptr, Value1, Obj.Value1     ; Save value
        ENDIF

        Obj.Id  = NewId()
        Obj2Ptr, Obj, Copy              ; Store copy into new pointer

    ENDELSE
END


;
;  Ptr2Obj
;   Pointers are really unrealized base widget objects and the
;   contents are their UVALUEs.  In general, copying structures is
;   an expensive operation so the default is to REMOVE the UVALUE
;   from the pointer.  This is much faster.  Note that is has the
;   side effect that the pointer is no longer valid (has no object
;   in it).
;
PRO Ptr2Obj, Ptr, Obj, COPY=Copy
    WIDGET_CONTROL, Ptr, GET_UVALUE=Obj, NO_COPY=(1 - KEYWORD_SET(Copy))
END


;
;  Obj2Ptr
;   The reverse of Ptr2Obj.  Place the given object (as a UVALUE) into
;   the given pointer (unrealized base widget).  Copy the value if
;   explicitly requested, otherwise the Object given us will no longer
;   contain a value upon return from this function.
;
PRO Obj2Ptr, Obj, Ptr, COPY=Copy
    WIDGET_CONTROL, Ptr, SET_UVALUE=Obj, NO_COPY=(1 - KEYWORD_SET(Copy))
END


;
;  NextPtr
;   Returned the contents of the .Next field of an object.  All objects
;   must have a .Next field.
;
FUNCTION NextPtr, Ptr
    Ptr2Obj, Ptr, Obj
    Next    = Obj.Next
    Obj2Ptr, Obj, Ptr
    RETURN, Next
END


;
;  AddChild
;       Add an object to a base object.
;
;   NO_UPDATE -- If set, DONT call the routine to update the Cut/Copy/Paste
;           dialog boxes.  Set when adding multiple objects (as in File Open)
;
;   NO_CANCEL -- If set the object is not added to the Active dialog
;           list.  This is done for base objects (Base Object Dialog boxes
;           have no CANCEL button) and should be done for any object class
;           which can not/should not be removed.
;
PRO AddChild, Parent, Child, NO_UPDATE=NoUpdate, NO_CANCEL=NoCancel

  COMMON WidEd_Comm

    ;   Set Parent Ptr in child
    SetTag, Child, "Parent", Parent

    ;   Get the Parent object structure
    Ptr2Obj, Parent, PObj

    ;   First Child ? Set parent child list.
    ;   Else last child in list now has a next child
    ;
    IF PObj.Children EQ 0 THEN PObj.Children    = Child $
    ELSE SetTag, PObj.LastChild, "Next", Child

    ;   Remember the last child
    PObj.LastChild  = Child

    Obj2Ptr, PObj, Parent       ; Restore structure into parent pointer

    ;   Add to active dialog list so we can delete it in the event
    ;   user CANCELs addition.
    IF KEYWORD_SET(NoCancel) EQ 0 THEN BEGIN
        NewDialogs  = [ NewDialogs, { WE_NEWOBJ, Parent, Child, 0L } ]
    ENDIF

    ;   Update Cut/Copy/Paste/Edit Dialog boxes
    IF KEYWORD_SET(NoUpDate) EQ 0 THEN UpdateEdit

    ;   Note that the object tree has been altered.
    Dirty   = 1
END


;
;  Cancel
;   Common routine to handle the 'CANCEL' button on dialog boxes.
;   This performs 2 separate actions depending upon what has happened
;   so far:
;
;   If the object has been added but never realized (no Rebuild) then
;   OldPtr is NULL and there is no state to revert the widget to.
;   Otherwise, we have a copy of the widgets previous state and we
;   restore it instead of deleting it.
;
;   Note: The object information has already been pulled out of the pointer
;   so we need both to be passed in.
;
PRO Cancel, Obj, Ptr

  COMMON WidEd_Comm    

    Type    = Obj.Type      ; Get the type
    Dialog  = Obj.Dialog    ; Get the Dialog Box widget Id.
    Obj2Ptr, Obj, Ptr       ; Stick Object back into pointer so that
                            ; other routines can access object via its ptr

    ;   See if this object can be cancelled. Should always be found.
    ;   In fact we display an error message if we don't

    Idx = WHERE(NewDialogs.ObjPtr EQ Ptr, Count)

    IF Count NE 1 THEN ErrorDialog, TopDlg, $
        "Internal Error: CANCEL'ed widget could not be found"

    ;   Do we have an previous version of this widget to revert to?

    Idx = Idx(0)
    IF NewDialogs(Idx).OldPtr NE 0L THEN BEGIN

        ;   Call class specific copy routine to revert object to
        ;   its previous state:  *ObjPtr = *OldPtr

        CALL_PROCEDURE, Type + "_Copy", NewDialogs(Idx).OldPtr, $
                    NewDialogs(Idx).ObjPtr
        WIDGET_CONTROL, Dialog, /DESTROY    ; Take down dialog box

    ENDIF ELSE BEGIN

        ;   This is more difficult.  Remove the object from its
        ;   parent's list of children

        ParPtr  = NewDialogs(Idx).ParPtr        ; Get Parent Object
        Ptr2Obj, ParPtr, ParObj

        ;   Is it the first child?
        IF ParObj.Children EQ Ptr THEN BEGIN
            ParObj.Children = NextPtr(Ptr)

            ;   Was it an only child?
            IF ParObj.LastChild EQ Ptr THEN ParObj.LastChild = ParObj.Children

        ENDIF ELSE BEGIN


            PrevPtr = ParObj.Children           ; Run down the list of children
            CurrPtr = NextPtr(PrevPtr)          ; until we find the child in
            NxtPtr  = NextPtr(Ptr)              ; question. Keep track of the
            WHILE CurrPtr NE Ptr DO BEGIN       ; previous child.
                PrevPtr = CurrPtr
                CurrPtr = NextPtr(CurrPtr)
            ENDWHILE

            ; Unlink the deleted child from the list of children
            ; If it was the last child of the parent object then set
            ; the LastChild field to the new last child.
            SetTag, PrevPtr, "Next", NxtPtr
            IF ParObj.LastChild EQ Ptr THEN ParObj.LastChild = PrevPtr

        ENDELSE

        Obj2Ptr, ParObj, ParPtr     ; Restore Parent pointer

        ;   The child has been removed from the object tree. Delete
        ;   the child.
        CALL_PROCEDURE, Type + "_Destroy", Ptr  ; Destroy Object
    ENDELSE

    ;   Now we have to remove the deleted object from the list
    ;   of objects with active dialog boxes.

    N   = N_ELEMENTS(NewDialogs)
    IF Idx(0) EQ N-1 THEN $                 ; Last active object?
        NewDialogs  = NewDialogs(0:N-2) $
    ELSE $
        NewDialogs  = [ NewDialogs(0:Idx-1), NewDialogs(Idx+1,*) ]
    UpdateEdit
END


;
;  Accept
;   The user has pressed the 'DONE' button or just closed the dialog
;   box via the window manager menu Close option.  Unfortunately these
;   look like 'DONE's and not 'CANCEL's. Thats what documentation is for.
;
;   Note: as with Cancel, the Object structure is already outside of
;       the pointer
;       Unlike Cancel, Base Objects also need to be handled (not that
;       this makes any difference here)
;
PRO Accept, Obj, Ptr
    Obj2Ptr, Obj, Ptr       ; Stick Object back into pointer
END

PRO WidAlloc
END
