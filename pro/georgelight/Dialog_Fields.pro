;***************************************************************
;** Project: Light
;** File:    Dialog_Fields.pro
;** Version: 0.1
;** Date:    Jan, 23rd, 2002
;** Author:  E. Farhi
;** Object:  Display a Widget_Table in modal or non modal mode and many options
;
;** Require: <none>
;
;** procedure copied from /usr/local/rsi/idl/lib/utilities/xvaredit.pro
;** Moved butons below table, centered. Display Structures as columns
;** Same usage, but with additional keywords.
;** Added Jan 16th, 2002
;**   Title=<string> set the title of the dialog
;**   Name=<string> add the string in front of Variable name, even for structures
;**       this text is displayed on top of the table
;**   COLUMN_WIDTHS=<value> set the width value of cells (e.g. about 100)
;**   FIELDNAMES=<string array> set the name of fields in structures
;**   /DISPLAY non-sensitive Table (only show, no modify)
;** Added Jan 22rd, 2002
;**   ID=ID replace the previous ID widget base
;**   SWAP transpose table
;**
;** Example: Dialog_Fields, Preferences, Group=Handles.Base,
;**  COLUMN_WIDTHS=500, Name='PEnter Preferences', Title='Preferences',
;**  y_scroll_size=5, FieldNames = [ 'Verbosity', 'IniFile', 'ConfigDir',
;** 'ConfigFile','Simulation']

FUNCTION Dialog_Fields__n_elements, arg
;
;function Dialog_Fields__N_ELEMENTS: return the number of elements in arg.
;This function is similar to N_ELEMENTS, but counts recursively,
;where applicable.
;
ON_ERROR, 2 ;return to caller on error.

CASE 1 OF
    SIZE(arg, /TYPE) EQ 8: BEGIN    ;structure or array of structures.
        result = 0L

        FOR i=0L,N_ELEMENTS(arg)-1 DO BEGIN
            FOR j=0L,N_TAGS(arg)-1 DO BEGIN
                result = result + Dialog_Fields__n_elements((arg[i]).(j))
            END
        END

        RETURN, result
    END

    ELSE: $
        RETURN, N_ELEMENTS(arg)
ENDCASE

END
;------------------------------------------------------------------------------
;   procedure Dialog_Fields_event
;------------------------------------------------------------------------------
; This procedure processes the events being sent by the XManager.
;------------------------------------------------------------------------------
PRO Dialog_Fields_event, event

COMPILE_OPT hidden
WIDGET_CONTROL, event.id, GET_UVALUE = whichevent
IF N_ELEMENTS(whichevent) EQ 0 THEN RETURN
IF whichevent NE "THEBUTTON" THEN RETURN

CASE event.value OF

    0: BEGIN                                            ;the user chose the
        WIDGET_CONTROL, event.top, /DESTROY     ;return the initial
      END                       ;variable

    1: BEGIN                        ;the user chose accept
        WIDGET_CONTROL, event.top, GET_UVALUE = pEval, /HOURGLASS
        IF (*pEval).usetable THEN BEGIN
            edit_cell = WIDGET_INFO((*pEval).table, /TABLE_EDIT_CELL)
            if (edit_cell[0] EQ -1 AND edit_cell[1] EQ -1) or ((*pEval).display) then begin
                (*pEval).modified = 1
                WIDGET_CONTROL, (*pEval).table, GET_VALUE = var
                if (SIZE((*pEval).var))[0] EQ 0 then begin
                  (*pEval).var = var[0]
                endif else begin
                  (*pEval).var = TEMPORARY(var)
                endelse
            endif else begin
                tmp = DIALOG_MESSAGE(['Please commit or cancel the edit',$
                                      'before pressing Accept.'])
                RETURN
            endelse
        ENDIF ELSE BEGIN
            i = 0LL
            ;so go ahead and modify the variable
            WIDGET_CONTROL, (*pEval).table, GET_VALUE = var
            WHILE(i LT N_ELEMENTS(var))DO BEGIN
                CASE (*pEval).entries[i].type OF
                    6: assign = '=COMPLEX'
                    9: assign = '=DCOMPLEX'
                    ELSE: assign = '='
                ENDCASE
                IF ((*pEval).entries[i].type EQ 10 AND var[i] NE '') $
                OR ((*pEval).entries[i].type EQ 11 AND var[i] NE '') $
                OR ((*pEval).entries[i].type NE 10 AND $
                    (*pEval).entries[i].type NE 11) $
                THEN BEGIN
                    str = "(*pEval)." + $
                        (*pEval).entries[i].name + $
                        assign + $
                        ((*pEval).entries[i].type EQ 7 ? "var[i]" : var[i])
                    IF NOT EXECUTE(str, 1) THEN BEGIN
                        void = DIALOG_MESSAGE( $
                            [ $
                                'Dialog_Fields: error converting "' $
                                    + var[i] $
                                    + '" to ' $
                                    + (*pEval).entries[i].name, $
                                '', $
                                !error_state.msg, $
                                !error_state.sys_msg $
                                ], $
                            /ERROR $
                            )
                        conversion_err = 1
                    ENDIF
                ENDIF
                i = i + 1
            ENDWHILE
        ENDELSE
        IF NOT KEYWORD_SET(conversion_err) THEN BEGIN
            (*pEval).modified = 1
            WIDGET_CONTROL, event.top, /DESTROY
        ENDIF
    END
    ELSE:

ENDCASE

END ;============= end of Dialog_Fields event handling routine task =============


;------------------------------------------------------------------------------
;   procedure AddEditEntry
;------------------------------------------------------------------------------
; This procedure adds an entry to the list that contains the variables names
; and the widget id for the edit field corresponding to the variable name.
;------------------------------------------------------------------------------

PRO Dialog_Fields__AddEditEntry, $
    entries, $      ; IN/OUT
    n_ents, $       ; IN/OUT
    thename, $      ; IN
    thetype, $      ; IN
    value, $        ; IN
    n_elems         ; IN

COMPILE_OPT hidden

IF(NOT(KEYWORD_SET(entries))) THEN BEGIN
    entries = REPLICATE({entstr, $
        name: thename, $
        value: value, $
        type: thetype $
        }, n_elems)
    n_ents = 1L
ENDIF ELSE BEGIN
    IF (N_ELEMENTS(entries) LE n_ents) THEN BEGIN
        entries = [temporary(entries), REPLICATE({entstr}, n_elems > 100)]
    END
    entries[n_ents].name = thename
    entries[n_ents].value = value
    entries[n_ents].type = thetype
    n_ents = n_ents + 1
ENDELSE
END ;============== end of Dialog_Fields event handling routine task ===============


;------------------------------------------------------------------------------
;   procedure Dialog_FieldsField
;------------------------------------------------------------------------------
;  This routine is used to create the widget or widgets needed for a given
;  variable type.  It could call itself recursively if the variable was itself
;  a structure comprised of other IDL variables.
;------------------------------------------------------------------------------

FUNCTION Dialog_FieldsField, $
    base, $     ; IN
    val, $      ; IN
    usetable, $ ; OUT
    entries, $  ; IN/OUT
    nentries, $ ; IN/OUT
    TOTAL_N_ELEMENTS = total_n_elements, $  ; IN/OUT (opt)
    NAME = NAME, $
    RECNAME = RECNAME, $
    X_SCROLL_SIZE = X_SCROLL_SIZE, $
    Y_SCROLL_SIZE = Y_SCROLL_SIZE, $
    COLUMN_WIDTHS = COLUMN_WIDTHS, $
    FIELDNAMES = FIELDNAMES, $
    SWAP = SWAP

COMPILE_OPT hidden
FORWARD_FUNCTION Dialog_FieldsField

typarr = [ $
    "Undefined", $                  ; 0
    "Byte", $                       ; 1
    "Integer", $                    ; 2
    "Longword Integer", $           ; 3
    "Floating Point", $             ; 4
    "Double Precision Floating", $  ; 5
    "Complex Floating", $           ; 6
    "String", $                     ; 7
    "Structure", $                  ; 8
    "Double Precision Complex", $   ; 9
    "Pointer", $                    ; 10
    "Object Reference", $           ; 11
    "Unsigned Integer", $           ; 12
    "Unsigned Longword Integer", $  ; 13
    '64-bit Integer', $             ; 14
    "Unsigned 64-bit Integer" $     ; 15
    ]

varsize = size(val)
vardims = N_ELEMENTS(varsize) - 2
type = varsize[vardims]
numelements = varsize[vardims + 1]

usetable = 0
IF (NOT(KEYWORD_SET(RECNAME)) $
AND (varsize[0] EQ 1 OR varsize[0] EQ 2)) THEN BEGIN
    IF(type EQ 8) THEN BEGIN
        IF varsize[0] EQ 1 THEN BEGIN
            IF !VERSION.OS_FAMILY EQ 'Windows' AND N_TAGS(val) GT 200 THEN $
                Goto, Cplx_Struct
            FOR i = 0, N_TAGS(val) - 1 DO BEGIN
                strsize = size(val.(i))
                strdims = N_ELEMENTS(strsize) - 2
                IF strsize[strdims] EQ 8 $  ; Structure
                OR strsize[strdims] EQ 10 $ ; Pointer
                OR strsize[strdims] EQ 11 $ ; Object Reference
                OR strsize[strdims + 1] NE varsize[vardims + 1] THEN $
                    Goto, Cplx_Struct
            ENDFOR
            usetable = 1
        ENDIF
    ENDIF ELSE BEGIN
        IF !VERSION.OS_FAMILY EQ 'Windows' AND varsize[1] GT 200 THEN $
            Goto, Cplx_Struct
        usetable = 1
    ENDELSE
ENDIF
Cplx_Struct:
recurse = KEYWORD_SET(RECNAME)

IF (NOT recurse) THEN $
  abase = WIDGET_BASE(base, /FRAME, /COLUMN, XPAD = 8, YPAD = 8)

IF(numelements GT 1) THEN BEGIN             ;if the variable is an
  suffix = " Array("                        ;array, then say so and
  FOR j = 1, varsize[0] DO BEGIN            ;show the array
    suffix = suffix + strtrim(varsize[j], 2)        ;dimensions.
    IF j NE varsize[0] THEN suffix = suffix + ", "
  ENDFOR
  suffix = suffix + ")"
ENDIF ELSE suffix = ""

LOCNAME = ''
IF(type EQ 8) THEN LOCNAME = TAG_NAMES(val, /STRUCTURE) ;if the variable is a
                            ;structure, use its
                            ;name

;build up the name of variable with the type in parentheses
IF(NOT recurse) THEN BEGIN
    IF(KEYWORD_SET(NAME)) THEN begin
      LOCNAME = NAME
    endif ELSE begin
        LOCNAME = LOCNAME + " " + typarr[type] + suffix
    endelse

    lbl = WIDGET_LABEL(abase, value = LOCNAME)
ENDIF

IF(NOT(KEYWORD_SET(RECNAME))) THEN BEGIN
    RECNAME = 'var'         ;establish the name
                            ;if not being called
                            ;recursively
END

IF(N_ELEMENTS(X_SCROLL_SIZE) EQ 0) THEN $
  XSCROLL_SIZE = 4 ELSE XSCROLL_SIZE = X_SCROLL_SIZE
IF(N_ELEMENTS(Y_SCROLL_SIZE) EQ 0) THEN $
  YSCROLL_SIZE = 4 ELSE YSCROLL_SIZE = Y_SCROLL_SIZE

if not(keyword_set(COLUMN_WIDTHS)) then COLUMN_WIDTHS = 100

IF (usetable) THEN BEGIN
    IF(type EQ 8) THEN BEGIN
        if not(keyword_set(FIELDNAMES))then $
            row_labels = TAG_NAMES(val) $
        else begin
            if n_elements(TAG_NAMES(val)) ne n_elements(FIELDNAMES) then $
                row_labels = TAG_NAMES(val) $
            else $
                row_labels = FIELDNAMES
        endelse
        column_labels = [TAG_NAMES(val(0), /structure)]
        COLUMN_MAJOR = 1
        ROW_MAJOR    = 0
        if keyword_set(SWAP) then begin
            tmp1 = column_labels
            column_labels = row_labels
            row_labels = tmp1
            COLUMN_MAJOR = 0
            ROW_MAJOR    = 1
        endif
        RETURN, WIDGET_TABLE( $
            abase, $
            value         = val, $
            ROW_LABELS    = row_labels, $
            COLUMN_LABELS = column_labels, $
            COLUMN_WIDTHS = COLUMN_WIDTHS, $
            /RESIZEABLE_COLUMNS, $
            /EDIT, $
            COLUMN_MAJOR  = COLUMN_MAJOR, $
            ROW_MAJOR     = ROW_MAJOR, $
            X_SCROLL_SIZE = XSCROLL_SIZE, $
            Y_SCROLL_SIZE = YSCROLL_SIZE $
            )
    ENDIF ELSE BEGIN
        RETURN, WIDGET_TABLE( $
            abase, $
            value = val, $
            /RESIZEABLE_COLUMNS, $
            /EDIT, $
            ALIGNMENT    = 1, $
            X_SCROLL_SIZE = XSCROLL_SIZE, $
            Y_SCROLL_SIZE = YSCROLL_SIZE $
            )
    ENDELSE
ENDIF

IF(varsize[0] GT 1) THEN BEGIN
  moduli = LONARR(varsize[0]-1) + 1
  FOR i = varsize[0], 2,-1 DO BEGIN
    FOR j = 1,i-1 DO $
      moduli[i - 2] = moduli[i - 2] * varsize[j]
  ENDFOR
ENDIF

IF N_ELEMENTS(total_n_elements) EQ 0 THEN BEGIN
    total_n_elements = Dialog_Fields__n_elements(val)
END

FOR element = 0L, numelements - 1 DO BEGIN       ;for each array element

  IF(numelements NE 1) THEN BEGIN           ;use array subscripting
    indexname = "("
    indexname = indexname + $
        strtrim(element mod varsize[1],2)
    IF(varsize[0] GT 1) THEN BEGIN
      indexarr = lonarr(varsize[0] - 1)
      flatindex = element
      FOR i = varsize[0] - 2, 0, -1 DO BEGIN
    indexarr[i] = flatindex / moduli[i]
    flatindex = flatindex mod moduli[i]
      ENDFOR
      FOR i = 0, varsize[0] - 2 DO $
    indexname = indexname + ", " + $
        strtrim(indexarr[i], 2)
    ENDIF
    indexname = indexname + ")"
    thename = RECNAME + indexname
  ENDIF ELSE BEGIN
    thename = RECNAME
  ENDELSE

  ;depending on the type, build a string variable with proper formatting
  CASE type OF
    0: thevalue = "Undefined Variable"          ;Undefined

    1: thevalue = string(val[element], $        ;Byte
        FORMAT = '(I3)')

    7: thevalue = val[element]              ;String

    8: BEGIN                        ;Structure
        tags = TAG_NAMES(val[element])
        FOR i = 0, N_ELEMENTS(tags) - 1 DO BEGIN
            error = EXECUTE("fieldvalue = val[element]." + tags[i])
            fldsize = size(fieldvalue)
            flddims = N_ELEMENTS(fldsize) - 2
            id = Dialog_FieldsField( $
                abase, $
                fieldvalue, $
                usetable, $
                entries, $
                nentries, $
                TOTAL_N_ELEMENTS = total_n_elements, $
                NAME = tags[i], $
                RECNAME = thename + "." + tags[i], $
                X_SCROLL_SIZE = XSCROLL_SIZE, $
                Y_SCROLL_SIZE = YSCROLL_SIZE, $
                COLUMN_WIDTHS = COLUMN_WIDTHS $
                )
        ENDFOR
    END
    10: thevalue = ''
    11: thevalue = ''
    ELSE: thevalue = strtrim(val[element], 2)
  ENDCASE

  IF(type NE 8) THEN BEGIN
    Dialog_Fields__AddEditEntry, $
        entries, $
        nentries, $
        thename, $
        type, $
        thevalue, $
        total_n_elements
  END

ENDFOR

table = 0
IF (NOT recurse) THEN BEGIN
    IF (N_ELEMENTS(entries.value) GT 1) THEN BEGIN
        table = WIDGET_TABLE( $
            abase, $
            VALUE = TRANSPOSE((entries.value)[0:nentries-1]), $
            ROW_LABELS = TRANSPOSE((entries.name)[0:nentries-1]), $
            COLUMN_LABELS = '', $
            COLUMN_WIDTHS = COLUMN_WIDTHS, $
            /RESIZEABLE_COLUMNS, $
            /EDIT, $
            Y_SCROLL_SIZE = YSCROLL_SIZE $
            )
    ENDIF ELSE BEGIN
        table = WIDGET_TABLE( $
            abase, $
            VALUE = [entries.value], $
            ROW_LABELS = [entries.name], $
            COLUMN_LABELS = '', $
            COLUMN_WIDTHS = COLUMN_WIDTHS, $
            /RESIZEABLE_COLUMNS, $
            /EDIT, $
            Y_SCROLL_SIZE = YSCROLL_SIZE $
            )
    ENDELSE
ENDIF

return, table
END ;============= end of Dialog_Fields event handling routine task =============


;------------------------------------------------------------------------------
;   procedure Dialog_Fields
;------------------------------------------------------------------------------
; this is the actual routine that is called.  It builds up the variable editing
; fields by calling other support routines and then registers the widget
; heiarchy with the XManager.  Notice that the widget is registered as a MODAL
; widget so it will desensitize all other current widgets until it is done.
;------------------------------------------------------------------------------
PRO Dialog_Fields, var, GROUP = GROUP, NAME = NAME, $
              X_SCROLL_SIZE = X_SCROLL_SIZE, Y_SCROLL_SIZE = Y_SCROLL_SIZE, $
              TITLE = TITLE, COLUMN_WIDTHS = COLUMN_WIDTHS, $
              FIELDNAMES = FIELDNAMES, SWAP=SWAP, DISPLAY=DISPLAY, $
              ID=ID

on_error, 2 ; Return to caller on error.

if(n_params() ne 1) THEN $
  MESSAGE, "Must have one parameter"

if n_elements(var) eq 0 then $
  MESSAGE, 'Argument is undefined.'

if not keyword_set(display) then display=0

if n_elements(ID) ne 0 then begin
    ; get geometry, iconified status
    widget_control, ID, Bad_Id=Bad_Id
    if Bad_Id eq 0 and ID ne 0 then begin
        Geometry = widget_info(ID, /geometry)
        Active   = widget_info(ID, /active)
        widget_control, ID, /destroy
    endif
endif

; Create parent of modal base if needed
if (N_ELEMENTS(GROUP) NE 1) then begin
  GROUP_ID = WIDGET_BASE(MAP=0)
endif else begin
  GROUP_ID = GROUP
endelse

if (not(keyword_set(TITLE))) then TITLE="Dialog_Fields"

if xregistered("Dialog_Fields") ne 0 and n_elements(ID) eq 0 then begin
  print, "Dialog_Fields: is already active. Please close the other Dialog (Table) first"
  return
endif

Dialog_Fieldsbase = WIDGET_BASE(TITLE = TITLE, $    ;create the main base
        /COLUMN, GROUP_LEADER=GROUP_ID)

WIDGET_CONTROL, /HOURGLASS
entries = 0
nentries = 0
table = Dialog_FieldsField(Dialog_Fieldsbase, var, usetable, entries, nentries, $
                      NAME = NAME, X_SCROLL_SIZE = X_SCROLL_SIZE, $
                      Y_SCROLL_SIZE = Y_SCROLL_SIZE, $
                      COLUMN_WIDTHS = COLUMN_WIDTHS, $
                      FIELDNAMES = FIELDNAMES, SWAP=SWAP)



Dialog_FieldsStat = {var:var, $
                entries:entries, $
                modified:0, $
                table: table, $
                usetable: usetable, $
                display:display}
pDialog_FieldsStat = PTR_NEW(Dialog_FieldsStat)
WIDGET_CONTROL, Dialog_Fieldsbase, SET_UVALUE=pDialog_FieldsStat

if display then WIDGET_CONTROL, table, sensitive=0, alignment=1

tmp1 = widget_base(Dialog_Fieldsbase, /row)
tmp1 = widget_base(Dialog_Fieldsbase, /align_center)

if not display then $
    buttons = ['Cancel', 'Accept'] $
else $
    buttons = ['OK']

menu = Cw_Bgroup( $
    tmp1, $
    buttons, $
    /ROW, $
    IDS=IDS, $
    UVALUE="THEBUTTON" $
    )

if not arg_present(var) and n_elements(ids) gt 1 then begin
    widget_control, ids[1], sensitive=0
end

if n_elements(ID) ne 0 and n_elements(Geometry) ne 0 then begin
    WIDGET_CONTROL, Dialog_Fieldsbase, xoffset=Geometry.xoffset-2*Geometry.xpad, yoffset=Geometry.yoffset-9*Geometry.ypad
end

WIDGET_CONTROL, Dialog_Fieldsbase, /REALIZE

if n_elements(ID) ne 0 then begin
    ID = Dialog_Fieldsbase
    XManager, "Dialog_Fields"+title, Dialog_Fieldsbase    , /just_reg, /no_block, event_handler='Dialog_Fields_event'
endif else begin
    ID = Dialog_Fieldsbase
    XManager, "Dialog_Fields", Dialog_Fieldsbase , event_handler='Dialog_Fields_event'
    ; Get the return value
    IF ((*pDialog_FieldsStat).modified) THEN var = (*pDialog_FieldsStat).var
endelse
PTR_FREE, pDialog_FieldsStat

; Destroy parent of modal base
if (N_ELEMENTS(GROUP) NE 1) then begin
  WIDGET_CONTROL, GROUP_ID, /DESTROY
endif

END ;================== end of Dialog_Fields main routine =======================

