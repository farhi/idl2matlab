;RDFILTER was written by Philippe Cuerq on August 1996
;With this interface it is possible to select a part of a run, and
;make three different projections too.

;Version 2.0		Date : 08/96

; ************************ Widgets Creating Procedure ******************
; Rd_Filter			Create main RDFILTER interface

; ************************ Event processing procedures *****************
; P_Fil_Event		,event,uv

; ************************ Functions of this file **********************
; Ph_Verif		,value1,value2,vect,siz

; ************************ Procedures of this file *********************
; Ph_Common
; P_Fil_Event		,event,uv
; Ph_Write		,label,mess
; Ph_Help_RS	
; Ph_Filter		,wks
; Rd_Filter	


; ******************************** BEGINNING ***************************

;    *******************************************************************
;    *                       PROCEDURE PH_COMMON                       *
;    *								       *
;    * Description :   This procedure is simply used to declare        *
;    *		   the commons.					       *
;    *								       *
;    *******************************************************************

pro Ph_Common

Common cm_filgal	,filter_config $; This structure contains the state of
					;  xrange,yrange,zrange,xproj,yproj,
					; zproj,cons,moni
					; Ex. filter_config.xproj=1 then
					; x projection is selected etc....
			,mes_lab $	; Widget Id of message label
			,xtol    $	; Widget Id of tolerance
			,xmkb    $	; Widget Id of X mask list
			,ymkb    $	; Widget Id of Y mask list
			,nomb    $	; Widget Id of Monitor
			,xtolf   $	; float  value of tolerance
			,roto    $	; other  value of tolerance
			,xmsk,mk_x    $	; X mask list (string,value)
			,ymsk,mk_y    $	; Y mask list (string,value)
			,nomo    $	; Monitor value
			,first_time	; To know if RDFILTER is or was active
					; in the same Lamp session. 
			
Common cm_filini	,xmin $		; Keep the xmin value
			,xmax $		; Keep the xmax value
			,ymin $		; Keep the ymin value
			,ymax $		; Keep the ymax value
			,zmin $		; Keep the zmin value
			,zmax $		; Keep the zmax value
			,spect1 $ 	; Keep the first  "run selection"
			,spect2 $	; Keep the second "run selection"
			,spect3		; Keep the third  "run selection"
	
Return 
End

;    *******************************************************************
;    *                   INTERFACES WITH -NW MODE OF LAMP              *
;    *								       *
;    *******************************************************************

pro ph_setmask, wmsk,mk_w
;** **********
;**
		on_ioerror,miswmsk & ok=0
		mk_w=intarr(50)-1  & tm1=' -1 -1 -1 -1 -1 -1 -1 -1 -1 -1'
		reads,wmsk+tm1+tm1+tm1+tm1+tm1, mk_w & ok=1
		miswmsk:   idx =where(mk_w ge 0)
		if (idx (0) ge 0) and (ok) then mk_w=mk_w(idx) else mk_w=-1
		ni=n_elements(idx) & mk_w=reform(mk_w,ni,1)
		wmsk='' & if mk_w(0) ge  0 then for i=0,ni-1 do $
					wmsk=wmsk+strtrim(string(mk_w(i)),2)+' '
end

pro ph_works, selection ,uv
;** ********
;**
@lamp.cbk
Common cm_filgal
Common cm_filini
		selection=strtrim(selection(0),2)
		If (selection ne '') then begin			; Calls Rdmulti
		    monoto=0
		    if (filter_config.moni eq 2) then monoto=-1
		    if (filter_config.moni eq 3) then begin on_ioerror,mismoni & monoto=float(nomo) & mismoni: & endif

		    if (filter_config.cons) and (strpos(selection,':') lt 0) then $
			Ph_Write,mes_lab,'!Sigma apply with concatenation'$
		    else begin
			if (strpos(selection,'>') ge 0) or (strpos(selection,'+') ge 0) then begin
			    if xtol gt 0 then begin roto='0' & widget_control,xtol,bad_id=i,get_value=roto & endif
			    on_ioerror,misxtol & r=0 & r=float(roto(0)) & misxtol:
			    if r ne xtolf then tolerance=r
			endif
		   	w20=0
			if (monimon lt 0) and (monoto ge 0) then RDSET,/def
			if (monimon ge 0) and (monoto lt 0) then RDSET,/raw
			Filterpro,'Ph_Filter'				; on each
			RdMulti,selection,status,uv(0),20,monoto	; selected part
			Filterpro,''					; of the numor
			
			If not status  then begin			; Display the run

				if (strpos(selection,'>') ge 0) or (strpos(selection,'+') ge 0) then begin
				    if xtol gt 0 then widget_control,xtol,bad_id=i,set_value=strtrim(string(toler),2) $
						 else roto=toler
				    xtolf=float(string(toler))
				endif

				to_don_history, 20,0,'w20=RDOPR("'+selection+'") ;RDFILTER '+inst_value
    			     
				r=execute('spect'+strtrim(string(uv(1)),1)+'=selection')
				
				If (filter_config.cons and (size(w20))(0) ge 2) $
				then begin Ph_Write,mes_lab,'Sigma evaluation ...' & w19=0
					   XICUTER,'w19=corel(w20)>1' 
					   if uv(0) gt 0 then forcplot,w=19
				endif else if uv(0) gt 0 then forcplot,w=20
			endif
		   endelse	
		endif
end

;    *******************************************************************
;    *                       PROCEDURE PH_ FIL_EVENT                   *
;    *								       *
;    * Description :   This procedure is the events maintenance .It    *
;    *		   is called by Lamp with the event and the user value *
;    *             of the event.                                       *
;    *								       *
;    *******************************************************************

pro P_Fil_Event,event,uv
;** ***********
;**
Common cm_filgal
Common cm_filini


case uv(2) of 

	1 : filter_config.xrange=event.select	; filter_config.xrange=1 => x
						; range selected
	
	2 : filter_config.yrange=event.select	; idem
	
	3 : filter_config.zrange=event.select	; idem
	
	4 :  filter_config.xproj=event.select	; filter_config.xproj=1 ==> x 
						; projection selected
				
	5 :  filter_config.yproj=event.select	; idem
					    	
	6 :  filter_config.zproj=event.select	; idem
	
	7 :  begin
	       filter_config.cons =event.select	; idem
	       If event.select then chaine='Sigma W19' $
	       		       else chaine='Sigma'
	       Widget_Control,event.id,Set_Value=chaine
	       end
	
	8 :  filter_config.moni =uv(3)		; Normalize

	10 : Ph_Help_RS				; help for run selector
	
	11 : Widget_Control,event.top,/Destroy	; exit event : destroy all 
						; the bases
	
	12 : Begin Ph_Write,mes_lab,''		; initialization
		   Ph_Write,uv(10) ,''

		Widget_Control,nomb,Get_Value=nomo & nomo=strcompress(nomo(0),/remove_all)
		Widget_Control,xmkb,Get_Value=xmsk & xmsk=strtrim(xmsk(0),2)
		Widget_Control,ymkb,Get_Value=ymsk & ymsk=strtrim(ymsk(0),2)

		if xmsk ne ''  then begin ph_setmask, xmsk,mk_x
					  Widget_Control,xmkb,Set_Value=xmsk & endif
		if ymsk ne ''  then begin ph_setmask, ymsk,mk_y
					  Widget_Control,ymkb,Set_Value=ymsk & endif

		If (filter_config.xrange eq 1) then begin	 ; to catch xmin 
			Widget_Control,uv(3),Get_Value=mini	 ; and xmax
			on_ioerror,misxmin & xmin=float(mini(0)) & misxmin:
			Widget_Control,uv(4),Get_Value=maxi
			on_ioerror,misxmax & xmax=float(maxi(0)) & misxmax:
			If (xmax lt xmin) then begin
				Ph_Write,mes_lab,'Xmax > Xmin  !!!'
				return
			endif  
		endif
		
		If (filter_config.yrange eq 1) then begin	 ; to catch ymin
			Widget_Control,uv(5),Get_Value=mini	 ; and ymax
			on_ioerror,misymin & ymin=float(mini(0)) & misymin:
			Widget_Control,uv(6),Get_Value=maxi
			on_ioerror,misymax & ymax=float(maxi(0)) & misymax:
			If (ymax lt ymin) then begin
				Ph_Write,mes_lab,'Ymax > Ymin  !!!'
			   	return
			endif
		endif
		
		If (filter_config.zrange eq 1) then begin	 ; to catch zmin
			Widget_Control,uv(7),Get_Value=mini	 ; and zmax
			on_ioerror,miszmin & zmin=float(mini(0)) & miszmin:
			Widget_Control,uv(8),Get_Value=maxi
			on_ioerror,miszmax & zmax=float(maxi(0)) & miszmax:
			If (zmax lt zmin) then begin
				Ph_Write,mes_lab,'Zmax > Zmin  !!!'
				return
			endif
		endif
					   	   
		selection=''			   		; to catch 
		Widget_Control,uv(9),Get_Value=selection	; the runs
		PH_WORKS, selection ,[uv(10),uv(11)]
	     end
	
	else : return      	

endcase		
return
end

;    *******************************************************************
;    *                       PROCEDURE PH_ WRITE                       *
;    *                                                                 *
;    * Description :   This procedure displays a message(mess)         *
;    *                 in a label(label).                              *
;    *                                                                 *
;    *******************************************************************

pro Ph_Write,label,mess


; Write messages in the label
if label gt 0	then Widget_Control,label,Set_Value=mess $
		else print,mess
Return
End

;    *******************************************************************
;    *                       PROCEDURE PH_ HELP_RS                     *
;    *                                                                 *
;    * Description :   This procedure calls a Lamp function :show_helps*
;    *                 which display a help.                           *
;    *                                                                 *
;    *******************************************************************

pro Ph_Help_RS
show_helps,[-88,594]
return
end



;    *******************************************************************
;    *                       FUNCTION PH_ VERIF                        *
;    *                                                                 *
;    * Description :   This function checks :    		       *
;    *                       - if at least one value is in the interval*  
;    *                  ==> returns a structure with a specific        *
;    *                  value (Aff=1)and the indices of the values     *
;    *                  in the vector (ex:{Aff=1,min=10,max=20}.       *
;    *                       - if not ==> returns a structure with a   *
;    *		        specific value(ex: {Aff=0}).                   *
;    *                                                                 *
;    *******************************************************************

function Ph_Verif,value1,value2,vect,siz


error1=0 & error2=0

If n_elements(vect) ne siz then vect=indgen(siz)+1 

If (vect(0)-vect(1)) gt 0 then begin 
	val=value1
	value1=value2 
	value2=val  
endif

indmax=siz-1 
indmin=0

If (value1 lt vect(0) or value1 gt vect(n_elements(vect)-1)) then error1=1 $
else begin 
	indmin=where(vect ge value1)  
	indmin=indmin(0)
endelse

If (value2 gt vect(n_elements(vect)-1) or value2 lt vect(0)) then error2=1 $
else begin	 
	indmax=where(vect le value2) 
	indmax=indmax(n_elements(indmax)-1)
endelse

If (error1 and error2) then aff={aff:0} $
else begin 
	vect=vect(indmin:indmax)
	aff={aff:1,min:indmin,max:indmax}
endelse	

return, aff	       	
end      	




;    *******************************************************************
;    *                       PROCEDURE PH_FILTER                       *
;    *                                                                 *
;    * Description :   This function selects a part of a workspace and *
;    *                 applies projections to it.                      *
;    *                 Note that data are always restored in workspace *
;    *                 20 and Ph_filter is called by Lamp, Lamp        *
;    *                 specifies that the work happens in W20.	       *
;    *		  		                                       *
;    *                                                                 *
;    *******************************************************************

pro Ph_Filter,wks

; Work= 0 ---> makes projections (if selected)
; Work= 1 ---> modifies workspace and makes projections (if selected)
; Work=-1 ---> does nothing

@lamp.cbk
Common cm_filgal
Common cm_filini

;changes the Lamp workspace for the w20

If wks ne 20 then XICUTE,'w20=w'+strtrim(string(wks),2)
Ph_Write,mes_lab,'---> '+w_numor(20)
siz =Size(w20)
sizn=Size(N20)
Work=0
Wmsk=0
If n_elements(E20) eq  n_elements(W20)	  then ero=1 else ero=0
If (sizn(0) ge 1) and (sizn(1) eq siz(1)) then ern=1 else ern=0

;---------------------- Selection on x axis ----------------------

if siz(0) ge 1 then begin
   If  xmsk ne '' then begin
	If n_elements(x20) lt siz(1) then  x20=indgen(siz(1))+1
	If (size(mk_x))(0) eq 2 then begin tmp=lonarr(siz(1)) & mk_x=mk_x<(siz(1)-1)
					   tmp(mk_x)=-1 & mk_x=where(tmp ge 0)  & endif
	x20=x20 (mk_x,*)    &	if (size(y20))(0) eq 2 then y20=y20(mk_x,*)
	w20=w20 (mk_x,*,*)  &	if  ero then E20=E20(mk_x,*,*)
				if  ern then N20=N20(mk_x,*)     & siz =Size(w20)
   endif
   indxmin=0 & indxmax=siz(1)-1

   If  filter_config.xrange then begin
	S_result=Ph_Verif(xmin,xmax,x20,siz(1))
	If S_result.aff then begin
		indxmin=S_result.min
		indxmax=S_result.max
		if (size(y20))(0) eq 2 then y20=y20(indxmin:indxmax,*)
		Work=1
	endif else begin
		Ph_Write,mes_lab, 'Warning : Xmin='+strtrim(string(min(x20)),2)$
		+'  Xmax='+strtrim(string(max(x20)),2)
		Work=-1
		endelse
   endif
endif
;---------------------- Selection on y axis ----------------------
			       
if siz(0) ge 2 then begin
   If  ymsk ne '' then begin
	If n_elements(y20) ne siz(2) then  y20=indgen(siz(2))+1 
	If (size(mk_y))(0) eq 2 then begin tmp=lonarr(siz(2)) & mk_y=mk_y<(siz(2)-1)
					   tmp(mk_y)=-1 & mk_y=where(tmp ge 0) & endif 
	if (size(y20))(0) eq 2  then y20=y20 (*,mk_y) else  y20=y20  (mk_y)
	if (size(x20))(0) eq 2  then x20=x20 (*,mk_y)
	w20=w20 (*,mk_y,*)  &   if  ero then E20=E20(*,mk_y,*)   & siz =Size(w20)
   endif
   indymin=0 & indymax=siz(2)-1

   If (filter_config.yrange) and (Work ne -1)  then begin
	S_result=Ph_Verif(ymin,ymax,y20,siz(2))
	If S_result.aff then begin
		indymin=S_result.min
		indymax=S_result.max
		If not filter_config.xrange then if (size(x20))(0) eq 2 then x20=x20(*,indymin:indymax)
		Work=1
	endif else begin 
		Ph_Write,mes_lab, 'Warning : Ymin='+strtrim(string(min(y20)),2)$
		+'  Ymax='+strtrim(string(max(y20)),2)
		Work=-1
	        endelse
   endif    	  	     
endif
;---------------------- Selection on z axis ----------------------

indzmin=0 & indzmax=siz(3)-1
If (filter_config.zrange and Work ne -1 and siz(0) eq 3) then begin
	S_result=Ph_Verif(zmin-1,zmax-1,indgen(siz(3)),siz(3))
	If S_result.aff then begin
		indzmin=S_result.min
		indzmax=S_result.max
		Work=1
	endif else begin 
		Ph_Write,mes_lab, 'Warning : Zmin= 1 '+'  Zmax='+$
		strtrim(string(siz(3)),2)
		Work=-1
	        endelse	    	  	     
endif
;--------------------- X,Y or Z projection according to workspace size ---

If Work ne -1 then  CASE siz(0) of
		1 : begin
			If Work then  begin w20=w20(indxmin:indxmax)
				if ern then N20=N20(indxmin:indxmax,*)
				if ero then E20=E20(indxmin:indxmax) & endif
			If filter_config.yproj then  begin w20=total(w20,1)
				if ern then N20=total(N20(*,0))
				if ero then E20=sqrt(total(E20^2,1)) & endif
		    end

		2 : begin
			If Work then  begin w20=w20(indxmin:indxmax,indymin:indymax)
				if ern then N20=N20(indxmin:indxmax,*)
				if ero then E20=E20(indxmin:indxmax,indymin:indymax) & endif
			If filter_config.xproj then  begin w20=total(w20,2)
				if ero then E20=sqrt(total(E20^2,2)) & endif
			If filter_config.yproj then  begin w20=total(w20,1)
				if ern then N20=total(N20(*,0))
				if ero then E20=sqrt(total(E20^2,1)) & endif
		    end

		3 : begin
			If Work then  begin w20=w20(indxmin:indxmax,indymin:indymax,indzmin:indzmax)
				if ern then N20=N20(indxmin:indxmax,*)
				if ero then E20=E20(indxmin:indxmax,indymin:indymax,indzmin:indzmax) & endif
			If filter_config.zproj then  begin w20=total(w20,3)
				if ero then E20=sqrt(total(E20^2,3)) & endif
			If filter_config.xproj then  begin w20=total(w20,2)
				if ero then E20=sqrt(total(E20^2,2)) & endif
			If filter_config.yproj then  begin w20=total(w20,1)
				if ern then N20=total(N20(*,0))
				if ero then E20=sqrt(total(E20^2,1)) & endif
		    end
				    
		else:   return

		    ENDCASE

If Work ne -1 then begin
	if (siz(0) eq 3) and (filter_config.zproj) then begin
	    x_tit(20)=x_tit(20)+' (Frames projection)'
	endif
	if (siz(0) ge 1) and (filter_config.xproj) then begin
	    y_tit(20)=z_tit(20)
	    y20=z20(0) & if  y20 eq 0 then y_tit(20)='Numor'
	    if y20 eq 0 then y20=long(w_numor(20))
	endif
	if (siz(0) ge 2) and (filter_config.yproj) then begin
	    x_tit(20)=y_tit(20) & y_tit(20)=z_tit(20) & x20=y20
	    y20=z20(0) & if  y20 eq 0 then y_tit(20)='Numor'
	    if y20 eq 0 then y20=long(w_numor(20))
	endif
endif
	
If n_elements(w20) eq 1 then begin 
	x_tit(20)=''   
	y_tit(20)='Total Values'
	on_ioerror,mis & x20=long(w_numor(20))
	mis: 
endif

;re-establishes Lamp workspace

If wks ne 20 then XICUTE,'w'+strtrim(string(wks),2)+'=w20'

return		    	    	
end



;    *******************************************************************
;    *                       PROCEDURE RDFILTER                        *
;    *                                                                 *
;    * Description :   This  procedure builds the interface            *
;    *                                                                 *
;    *		  		                                       *
;    *******************************************************************


pro rdfilter ,pth ,XRANGE=xrg ,YRANGE=yrg ,ZRANGE=zrg ,XMASK=xtext ,YMASK=ytext, MONIMOD=momod $
		  ,XPROJ=xproj,YPROJ=yproj,ZPROJ=zproj,WKSP =wksp $
		  ,MONIVAL=moval ,SIGMA=sigma ,TOLERANCE=latol ,SELECTION=selec

@lamp.cbk

Common cm_filgal
Common cm_filini

If n_elements(first_time) eq 0 then begin 
	first_time=1
	mes_lab   =0
	xtol	  =0
	filter_config={xrange:0,yrange:0,zrange:0,xproj:0,yproj:0,zproj:0,cons:0,moni:1}
	xmin  =0 & xmax=0
	ymin  =0 & ymax=0
	zmin  =0 & zmax=0
	spect1='' & spect2='' & spect3='' & xmsk='' & ymsk='' & nomo='100000.' & roto='0'
endif
kef=0		
if n_elements(xtext) eq 1 then begin xmsk =string(xtext) & if xmsk ne ''  then ph_setmask, xmsk,mk_x & kef=1 & endif
if n_elements(ytext) eq 1 then begin ymsk =string(ytext) & if ymsk ne ''  then ph_setmask, ymsk,mk_y & kef=1 & endif

if n_elements(xrg)   eq 2 then begin xmin =xrg(0) & xmax=xrg(1)		& kef=1
				     if xmax le xmin then filter_config.xrange=0 else filter_config.xrange=1
				     if xmax lt xmin then Ph_Write,mes_lab,'Xmax > Xmin  !!!'      &   endif
if n_elements(yrg)   eq 2 then begin ymin =yrg(0) & ymax=yrg(1)		& kef=1
				     if ymax le ymin then filter_config.yrange=0 else filter_config.yrange=1
				     if ymax lt ymin then Ph_Write,mes_lab,'Ymax > Ymin  !!!'      &   endif
if n_elements(zrg)   eq 2 then begin zmin =zrg(0) & zmax=zrg(1)		& kef=1
				     if zmax le zmin then filter_config.zrange=0 else filter_config.zrange=1
				     if zmax lt zmin then Ph_Write,mes_lab,'Zmax > Zmin  !!!'      &   endif

if n_elements(xproj) eq 1 then begin filter_config.xproj=xproj  & if xproj eq 1 then kef=1 & endif
if n_elements(yproj) eq 1 then begin filter_config.yproj=yproj  & if yproj eq 1 then kef=1 & endif
if n_elements(zproj) eq 1 then begin filter_config.zproj=zproj  & if zproj eq 1 then kef=1 & endif

if n_elements(momod) eq 1 then begin filter_config.moni=fix(momod)>1<3	& kef=1 & endif
if n_elements(moval) eq 1 then begin nomo=string(moval)			& kef=1 & endif

if n_elements(sigma) eq 1 then begin if sigma then filter_config.cons=1 else filter_config.cons=0  & kef=1 & endif
if n_elements(latol) eq 1 then begin roto=string(latol) & kef=1 & endif

if n_elements(selec) eq 1 then begin if n_elements(wksp) eq 1 then wk='W'+strtrim(string(wksp),2) else wk=''
				     if wk ne '' then XICUTER, wk+'=0'
				     PH_WORKS  ,selec ,[0,1] & kef =1
				     if wk ne '' then XICUTER, wk+'=w20'
				     endif
if (!D.flags and 65536)    ne 0 then if b_labins(3) eq 0 then $
if xregistered('RDFILTER') le 0 then if    kef      eq 0 then begin

filter_config.cons=0

;============================== BASE  ========================================	
base	   =Widget_Base(/Column	,Title='FILTER 2.0',resource_name='lamptouch')


;============================== BASE 1 =======================================
base1	   =Widget_Base   (base    	,/Column)


;============================== BASE 11 SCALES FILTER ========================
base11	   =Widget_Base   (base1   ,/Column,/Frame)

b_ico	   =Widget_Base   (base11  ,/row)
fs_lab	   =Widget_Label  (b_ico   ,value='SCALES',font=ft_biggest)
	    put_logo,b_ico
if sys_dep('MACHINE') eq 'win' then cap=3 else cap=0

base111	   =Widget_Base   (base11  ,/Row)

base1111   =Widget_Base   (base111 ,/Nonexclusive)
xrange_but =Widget_Button (base1111,value='X Range :',uvalue=[-88,379,1] ,font=ft_normal)
xmin_text  =Widget_Text   (base111 ,xsize=5+cap,value=strtrim(string(xmin),1),/Editable,font=ft_propor)
xmax_text  =Widget_Text   (base111 ,xsize=5+cap,value=strtrim(string(xmax),1),/Editable,font=ft_propor)

base112    =Widget_Base   (base11	 ,/Row)

base1121   =Widget_Base   (base112 ,/Nonexclusive)              
yrange_but =Widget_Button (base1121,value='Y Range :',uvalue=[-88,379,2] ,font=ft_normal)
ymin_text  =Widget_Text   (base112 ,xsize=5+cap,value=strtrim(string(ymin),1),/Editable,font=ft_propor)
ymax_text  =Widget_Text   (base112 ,xsize=5+cap,value=strtrim(string(ymax),1),/Editable,font=ft_propor)

base113    =Widget_Base   (base11  ,/Row)

base1131   =Widget_Base   (base113 ,/Nonexclusive)              
zrange_but =Widget_Button (base1131,value='Z Range :',uvalue=[-88,379,3],font=ft_normal)
zmin_text  =Widget_Text   (base113 ,xsize=5+cap,value=strtrim(string(zmin),1),/Editable,font=ft_propor)
zmax_text  =Widget_Text   (base113 ,xsize=5+cap,value=strtrim(string(zmax),1),/Editable,font=ft_propor)

;============================== BASE 12 PROJECTIONS , SIGMA ===================

base12     =Widget_Base   (base1   ,/Column,/Frame)
pf_lab	   =Widget_Label  (Widget_Base(base12,/row)  ,value='PROJECTIONS',font=ft_biggest)

base112    =Widget_Base   (base12  ,/Row,/Nonexclusive)
xproj_but  =Widget_Button (base112 ,value='X Projection',uvalue=[-88,379,4],font=ft_normal)
yproj_But  =Widget_Button (base112 ,value='Y Projection',uvalue=[-88,379,5],font=ft_normal)
base113    =Widget_Base   (base12  ,/Row,/Nonexclusive)
zproj_But  =Widget_Button (base113 ,value='Z Projection',uvalue=[-88,379,6],font=ft_normal)
cons_but   =Widget_Button (base113 ,value='Sigma       ',uvalue=[-88,379,7],font=ft_normal)	

base113    =Widget_Base   (base12  ,/row)
bid	   =Widget_Label  (base113 ,value='X merging Tolerance=',font=ft_normal)
xtolf	   =float(string(tolerance))
xtol	   =Widget_Text	  (base113 ,value=strtrim(string(tolerance),2),xsize=8+cap,/Editable,font=ft_propor)

;============================== BASE 12b MASKS & NORM ===================

base12     =Widget_Base   (base1   ,/Column,/Frame)
pf_lab	   =Widget_Label  (Widget_Base(base12,/row)  ,value='MASKS & NORM.',font=ft_biggest)

base113    =Widget_Base   (base12  ,/row)
bid	   =Widget_Label  (base113 ,value='X masks 0,..n :',font=ft_normal)
xmkb	   =Widget_Text	  (base113 ,value=strtrim(string(xmsk),2),xsize=14,/Editable,font=ft_propor)

base113    =Widget_Base   (base12  ,/row)
bid	   =Widget_Label  (base113 ,value='Y masks 0,..n :',font=ft_normal)
ymkb	   =Widget_Text	  (base113 ,value=strtrim(string(ymsk),2),xsize=14,/Editable,font=ft_propor)

base113    =Widget_Base   (base12  ,/row)
moni_but   =Widget_Base   (base113 ,/row,/exclusive)
moni_def   =Widget_Button (moni_but,value='Def',font=ft_normal,uvalue=[-88,379,8,1],/no_release)
moni_raw   =Widget_Button (moni_but,value='Raw',font=ft_normal,uvalue=[-88,379,8,2],/no_release)
moni_mon   =Widget_Button (moni_but,value='M:' ,font=ft_normal,uvalue=[-88,379,8,3],/no_release)
nomb	   =Widget_Text	  (base113 ,value=strtrim(string(nomo),2),xsize=7+cap,/Editable  ,font=ft_propor)

;============================== BASE 13 RUNS SELECTOR ========================
base13     =Widget_Base  (base1	   ,/Column,/Frame)

base131	   =Widget_Base	 (base13   ,/Row)
rs1_lab	   =Widget_Label (base131  ,value='RUNS SELECTOR ',font=ft_biggest)
helprs_but =Widget_Button(base131  ,value='?',uvalue=[-88,379,10]  ,font=ft_b_normal)

base132	   =Widget_Base	 (base13   ,/Row)
spect1_text=Widget_Text	 (base132  ,xsize=20,/Editable,value=spect1,font=ft_propor)
read1_but  =Widget_Button(base132  ,value='Read',font=ft_b_normal)

base133	   =Widget_Base	 (base13   ,/Row)
spect2_text=Widget_Text	 (base133  ,xsize=20,/Editable,value=spect2,font=ft_propor)
read2_but  =Widget_Button(base133  ,value='Read',font=ft_b_normal)

mes_lab	   =Widget_Label (base13   ,value=string(replicate(32b,50)),font=ft_b_normal,xsize=200)

rsmes_lab  =Widget_Label (base13   ,value=string(replicate(32b,50)),font=ft_b_normal,xsize=200)

;============================== EXIT =========================================

if n_elements(pth) eq 1 then begin
	buse=Widget_Base   (base,/Row)
	exit_but   =Widget_Button (buse,value='EXIT',uvalue=[-88,379,11],font=ft_biggest)
	bid =Widget_Label  (buse,value='Path:',font=ft_b_normal)
	bid =widget_button(buse,font=ft_b_normal,value=cycle,menu=2) 
	  uval =[-88,561,0,b_labins(0),b_labins(1)]
	  for i=0,n_elements(lamp_ali)-1 do begin
	      if strpos(strlowcase(lamp_ali(i)),'c_year') ge 0 then begin
		yr =strtrim(strmid(lamp_ali(i),7,15),2) & yr=strmid(yr,2,2)
		didon=widget_button(bid  ,font=ft_b_normal,menu=2		    	,value=lamp_ali(i))
		for j=1,5 do begin  yrs=yr+strtrim(string(j),2)
		 bido=widget_button(didon,font=ft_b_normal,uvalue=[uval,i,bid,long(yrs)],value='Cycle '+yrs)
		endfor
	      endif else $
		bidon=widget_button(bid  ,font=ft_b_normal,uvalue=[uval,i,bid,0]	,value=lamp_ali(i))
	  endfor   

endif else exit_but=Widget_Button (base,value='EXIT',uvalue=[-88,379,11],font=ft_biggest)

;============================== END BASE =====================================

;============================== Set_Uvalue Read buttons ======================

if (sys_dep('MACHINE') eq 'win') and (sys_dep('VERSION') lt '5.3') then txev=0 else txev=1

Widget_Control,	read1_but  ,Set_Uvalue	=[-88,379,12,xmin_text,xmax_text,$
					ymin_text,ymax_text,zmin_text,$
					zmax_text,spect1_text,rsmes_lab,'1']
Widget_Control,	read2_but  ,Set_Uvalue	=[-88,379,12,xmin_text,xmax_text,$
					ymin_text,ymax_text,zmin_text,$
					zmax_text,spect2_text,rsmes_lab,'2']
if txev then $
Widget_Control,	spect1_text,Set_Uvalue	=[-88,379,12,xmin_text,xmax_text,$
					ymin_text,ymax_text,zmin_text,$
					zmax_text,spect1_text,rsmes_lab,'1']
if txev then $
Widget_Control,	spect2_text,Set_Uvalue	=[-88,379,12,xmin_text,xmax_text,$
					ymin_text,ymax_text,zmin_text,$
					zmax_text,spect2_text,rsmes_lab,'2']

					
If filter_config.xrange then Widget_Control,	xrange_but,	/Set_Button				
If filter_config.yrange then Widget_Control,	yrange_but,	/Set_Button
If filter_config.zrange then Widget_Control,	zrange_but,	/Set_Button

If filter_config.xproj  then Widget_Control,	xproj_but,	/Set_Button				
If filter_config.yproj  then Widget_Control,	yproj_but,	/Set_Button
If filter_config.zproj  then Widget_Control,	zproj_but,	/Set_Button

If filter_config.moni eq 1 then Widget_Control,	moni_def ,	/Set_Button
If filter_config.moni eq 2 then Widget_Control,	moni_raw ,	/Set_Button
If filter_config.moni eq 3 then Widget_Control,	moni_mon ,	/Set_Button


Widget_Control,	 base   ,group_leader=lamp_b1,/Realize & put_logo

if lamp_b1 gt 0  then $
	Xmanager,'RDFILTER',base,Event_Handler='LAMP_EVENT_PARSER',/just_reg $
else	Xmanager,'RDFILTER',base,Event_Handler='LAMP_EVENT_PARSER'

endif
return
end


