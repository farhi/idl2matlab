;*******************
pro TOF_EVENT, event

    WIDGET_CONTROL,event.top,GET_UVALUE= P
    WIDGET_CONTROL,event.id ,GET_VALUE = scale
    TOF_PLOT ,P.W , P.X,P.Y , P.ID , P.MIN , P.MAX*scale/100.
RETURN
END

;***********************************************
pro TOF_PLOT, wksp , x,y , draw_id , mini , maxi

    old_id =!D.WINDOW
    WSET   , draw_id
    SURFACE, wksp,x,y,ZRANGE =[mini,maxi],/HORIZONTAL,AX=55.,AZ=0.,ZSTYLE=4,$
    		      XMARGIN=[2,2],YMARGIN =[2,2],BACKGROUND=255 ,COLOR =0,$
    		      XTICKLEN=1.,XGRIDSTYLE=1
    WSET   , old_id
RETURN
END

;***************************
pro TOF, win , group , width

    s = SIZE(win)
    IF  s(0) EQ 2 THEN BEGIN
	IF  N_ELEMENTS(group) EQ 0 THEN group= 6
	IF  N_ELEMENTS(width) EQ 0 THEN width= 40
	wksp = LINEUP (win,elas)
	a1   = (elas-width)>0
	a2   = (elas+width)<(s(1)-1)
	wksp = SMOOTH (CONGRID(wksp(a1:a2,*), width*2+1,group<s(2) ),3)
	maxi = MAX    (wksp , MIN=mini)
	x    = INDGEN (width*2+1)-width
	y    = INDGEN (group) +1
	
	base = WIDGET_BASE  (TITLE='Lamp TOF lineup & group',/ROW)
	draw = WIDGET_DRAW  (base , XSIZE=512, YSIZE=400)
	slid = WIDGET_SLIDER(base ,/VERTICAL , VALUE=100,/SUPPRESS_VALUE,/DRAG)
	
	WIDGET_CONTROL,base,/REALIZE
	WIDGET_CONTROL,draw, GET_VALUE = draw_id
	WIDGET_CONTROL,base, SET_UVALUE={ID:draw_id,MIN:mini,MAX:maxi,W:wksp,X:x,Y:y}
	XMANAGER,'TOF'+STRING(base) ,base, EVENT_HANDLER='TOF_EVENT' , /JUST_REG
	
	TOF_PLOT, wksp , x,y , draw_id , mini , maxi
    ENDIF
RETURN
END
