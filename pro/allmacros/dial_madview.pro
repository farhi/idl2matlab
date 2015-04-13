;******************************************************************************
;------------------------------------------------------------------------------
;
	PRO dial_madview_macro, Dial
;
;       Displays MAD output in window
;
;						JRS 10/8/01
;******************************************************************************
;------------------------------------------------------------------------------

	COMMON c_lamp_font

;	ft_biggest , ft_bigger  , ft_normal , ft_smaller, ft_smallest,$
;	ft_b_bigger, ft_b_normal, ft_propor
	
	IF(Dial.init EQ 0) THEN BEGIN
		Dial.init=1
		ML_BASE_0=Widget_Base(TITLE="MAD Output")
		ML_TEXT_0=Widget_Text(ML_BASE_0,XSIZE=78,YSIZE=60,/SCROLL $
			,FONT=ft_propor)
		Widget_Control,ML_BASE_0,/REALIZE
		Dial.tbase=ML_TEXT_0
 	ENDIF
	V=DialNewValue(/setvalue)
	V=STRTRIM(V,2)
	IF(N_ELEMENTS(V) GT 1) OR (V(0) GT " ") THEN BEGIN
		Widget_Control,Dial.tbase,BAD_ID=ii,SET_VALUE=V,/APPEND
		IF(ii NE 0) THEN DIAL.init=0
	ENDIF	
   	END

function dial_madview
;******* ***********
;**

    RETURN, {NAME:'madlog',GENERIC:'mad',TYPE:'log',VALUE:'',FREQUENCY:1,$
	     INIT:0,TBASE:0L}
    END
