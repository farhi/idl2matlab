;+
; NAME:
; GREAT
;
;-
; DO NOT REMOVE THIS COMMENT: END HEADER
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.

; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
; DO NOT REMOVE THIS COMMENT: BEGIN PDMENU9

PRO PDMENU9_Event, Event
  CASE Event.Value OF 
  'Tetris': BEGIN
    PRINT, 'Tetris'
    tetris
    END
  'TWO.Four': BEGIN
    PRINT, 'Event for TWO.Four'
    END
  'TWO.Five': BEGIN
    PRINT, 'Event for TWO.Five'
    END
  'TWO.Six': BEGIN
    PRINT, 'Event for TWO.Six'
    END
  'Sinus': BEGIN
    PRINT, 'sin(x)'
    a=findgen(1000)/100.
    b=sin(a)
    plot,a,b
    END
  ENDCASE
END

; DO NOT REMOVE THIS COMMENT: END PDMENU9
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.

; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
; DO NOT REMOVE THIS COMMENT: BEGIN MAIN13

PRO MAIN13_Event, Event
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  CASE Ev OF 
 'DRAW5': BEGIN
      Print, 'Event for DRAW5'
      END
  ; Event for PDMENU9
  'PDMENU9': PDMENU9_Event, Event
  ENDCASE
END

; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.

PRO great, GROUP=Group
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0
  junk   = { CW_PDMENU_S, flags:0, name:'' }
  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='Function Plotter', $
      UVALUE='MAIN13')
  BASE2 = WIDGET_BASE(MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')
  LABEL8 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL8', $
      VALUE='Choose your function to be plotted')
  DRAW5 = WIDGET_DRAW( BASE2, $
      RETAIN=2, $
      UVALUE='DRAW5', $
      XSIZE=400, $
      YSIZE=200)
  MenuDesc737 = [ $
      { CW_PDMENU_S,       0, 'Tetris' }, $ ;        0
      { CW_PDMENU_S,       1, 'TWO' }, $ ;        1
        { CW_PDMENU_S,       0, 'Four' }, $ ;        2
        { CW_PDMENU_S,       0, 'Five' }, $ ;        3
        { CW_PDMENU_S,       2, 'Six' }, $ ;        4
      { CW_PDMENU_S,       2, 'Sinus' } $  ;      5
  ]
  PDMENU9 = CW_PDMENU( BASE2, MenuDesc737, /RETURN_FULL_NAME, $
      UVALUE='PDMENU9')
  WIDGET_CONTROL, MAIN13, /REALIZE
  ; Get drawable window index
  COMMON DRAW5_Comm, DRAW5_Id
  WIDGET_CONTROL, DRAW5, GET_VALUE=DRAW5_Id
  XMANAGER, 'MAIN13', MAIN13
END
