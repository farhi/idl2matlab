;;; ***************************************************************************
;;; ***			           SHARED FUNCTIONS			    ***
;;; ***************************************************************************

function NUM2STR,x,y
;;;;; This function converts any number to a string, with `y' digits after
;;;;; the dot. `y' is always less than 7.
   temp=size(y)
   if ((temp(0) eq 0) and (temp(1) eq 1)) then y=4 $
   else y=max([0,min([y,6])])
   temp=size(x)
   res='0'
   if  (temp(0) eq 0) then $
   if ((temp(1) ge 2) and (temp(1) le 5)) then begin xx=long(x)
      
      if ((temp(1) eq 4) or (temp(1) eq 5)) then begin
	 temp=abs(round(10^y*(x-xx)))
	 xx  =xx+ float(temp)/10^y
	 xl  =long(xx)
	 temp=abs(round(10^y*(xx-xl)))
	 res =string(xl)
	 str =strcompress(('000000'+string(temp)),/remove_all)
	 leng=strlen(str)
	 str =strmid(str,leng-y,y)
	 if ((temp ge 1) and (str ne '')) then res=res+'.'+str
      endif else res=string(xx)
   endif
   return,strcompress(res,/remove_all)
end

function BEN_UNDEFINED,x
;;;;; Return ` if the variable `x' is undefined.
   temp=size(x)
   if (temp(0) ne 0) then return,0
   return,(temp(1) eq 0)
end

function BEN_READTEXT,widgetID
;;;;; This function reads the value of the widget_text `widgetID',
;;;;; and returns a float equal to this value.
   widget_control,bad_ID=bad,get_value=temp,widgetID
   if (bad ne 0) then return,0
   x=temp(0)
   if (strmid(x,0,1) eq '-') then begin
      sign=-1
      x=strmid(x,1,strlen(x)-1)
   end else sign=1
   return,(sign*float('0' + x))
end

function BEN_SELECTWID,base,i,n,labelID,wkn,comp
@lamp.cbk
;;;;; This function creates three widget_buttons, for workspace selection.
;;;;; The message ('Load', or 'Write') is determine by the parameter `n'.
;;;;; The user value of this widgets are 401, and (403+n). The '<-' and '->'
;;;;; buttons should call the BEN_CHANGE_WORKSPACE procedure, that will
;;;;; handle everything. Additionnal elements can be put in the user value
;;;;; of the center button by passing them in the `comp' parameter.
;;;;; The value returned by this function are the three buttons ID number.
   decr=widget_button(base,value="<-",font=ft_b_normal)
   load=widget_button(base,uvalue=[-88,(403+n),i,wkn,labelID,comp], $
      font=ft_b_normal)
   incr=widget_button(base,value="->",font=ft_b_normal)
   case n of
      0:    title="Load W"
      1:    title="Write to W"
      else: title="W"
   end
   if (wkn eq 0) then begin
      title=(' ' + title + ' ')
      widget_control,sensitive=0,load
   end else if (wkn ge 10) then title=(title + NUM2STR(wkn,0)) $
   else title=(title + ' ' + NUM2STR(wkn,0))
   widget_control,set_value=title,load
   widget_control,sensitive=(wkn gt 1),decr
   widget_control,sensitive=(wkn lt 20),incr
   widget_control,set_uvalue=[-88,401,i,0,load,decr,incr,n],decr
   widget_control,set_uvalue=[-88,401,i,1,load,decr,incr,n],incr
   return,[base,decr,load,incr]
end

function BEN_COMMANDS,base,i,masq,comp
@lamp.cbk
;;;;; This function creates the 'Done', workspace selection widgets, and
;;;;; 'help' buttons that are present at the top of the function windows.
;;;;; It also creates a widget_label. The `mask' parameter is an array
;;;;; used for preventing the function from creating the 'help' button,
;;;;; or the label. Those widget are created if the corresponding value
;;;;; (mask(0): help, mask(1): label) are set to 0. The default value is
;;;;; mask=[0,0]. The workspace selection widgets are created by calling
;;;;; the BEN_SELECTWID function.
;;;;; The returned value is an array of the widgets ID number.
   if BEN_UNDEFINED(masq) then masq=[0,0]
   if BEN_UNDEFINED(comp) then comp=[0]
   top=widget_base(base,/row)
   done=widget_button(top,value=" Done ",uvalue=[-88,400,i], $
      font=ft_b_normal)
   temp=widget_base(top,/row,/frame)
   if masq(0) then help=-1 $
   else help=widget_button(top,value='?',uvalue=[-88,402,1])
   if masq(1) then labelID=-1 $
   else labelID=widget_label(base,value="   You must load W ",font=ft_b_normal)
   if BEN_UNDEFINED(one) then wkn=0 else wkn=abs(one)
   slct=BEN_SELECTWID(temp,i,0,labelID,wkn,comp)
   return,[done,0,slct(0),slct(1),slct(2),slct(3),help,labelID]
end

pro BEN_CHANGE_WORKSPACE,uv
;;;;; This procedure should be called to handle the workspace selection
;;;;; widgets. Only the user value must be passed. Its form is:
;;;;; uv=[-88,4xx,x,{add/sub},{btn "Wxx"},{btn "<-"},{btn "->"},x]
;;;;; The currently selected workspace number is stored into the element
;;;;; number 3 of the center button.
   widget_control,get_uvalue=x,uv(4)
   if uv(3) then n=x(3)+1 $
   else n=x(3)-1
   widget_control,sensitive=(n ne 1),uv(5)
   widget_control,sensitive=(n ne 20),uv(6)
   x(3)=n
   widget_control,/sensitive,uv(4)
   case uv(7) of
      0:    str="Load W"
      1:    str="Write to W"
      else: str="W"
   end
   if ((n ge 1) and (n le 9)) then $
      widget_control,set_value=(str + " " + NUM2STR(n,0)),set_uvalue=x,uv(4) $
   else if ((n ge 10) and (n le 20)) then $
      widget_control,set_value=(str + NUM2STR(n,0)), set_uvalue=x,uv(4)
end

function BEN_WK_SIZE,x
;;;;; This function returns the size of the workspace Wx.
   common c_lamp_w,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11, $
      w12,w13,w14,w15,w16,w17,w18,w19,w20
   wksize=0
   temp=execute("wksize=size(W" + NUM2STR(x,0) + ")")
   return,wksize
end

pro BEN_READ_WK,x,work,wkx,wky,wkn,wkp,datpar
@lamp.cbk
;;;;; This procedure reads the LAMP workspace Wx, and all the linked data
;;;;; (eg Xx, Yx, PARx ...), and writes them in the parameter variables.
datpar=['','','']
   if ((x lt 0) or (x gt 20)) then return
   temp=execute("work=w"    + NUM2STR(x,0))
   temp=execute("wkx=x"     + NUM2STR(x,0))
   temp=execute("wky=y"     + NUM2STR(x,0))
   temp=execute("wkn=n"     + NUM2STR(x,0))
   temp=execute("wkp=p"     + NUM2STR(x,0))
   temp=execute("datpar=par"+ NUM2STR(x,0))
   datpar=[w_numor(x),string(x),his(x)]
end

pro BEN_WRITE_WK,x,work,wkx,wky,wkn,wkp,datpar
@lamp.cbk
;;;;; This procedure writes the parameters into LAMP variables,
;;;;; creating or replacing the workspace Wx and accompaying data.
   temp=execute("w" + NUM2STR(x,0) + "=work")
   if BEN_UNDEFINED(wkx) then return
   temp=execute("x" + NUM2STR(x,0) + "=wkx")
   if BEN_UNDEFINED(wky) then return
   temp=execute("y" + NUM2STR(x,0) + "=wky")
   if BEN_UNDEFINED(wkn) then return
   temp=execute("n" + NUM2STR(x,0) + "=wkn")
   if BEN_UNDEFINED(wkp) then return
   temp=execute("p" + NUM2STR(x,0) + "=wkp")
   if BEN_UNDEFINED(datpar) then return
;  temp=execute("par" + NUM2STR(x,0) + "=datpar")
   w_numor(x)=datpar(0)
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; DESKTOP EVENTS
;;; -+-+-+-+-+-+-+

function desk_name,i,test,string
;;;;; If (i lt 0), this function inits the commons, and returns the number
;;;;; of display function that it 'knows'. If (i ge 0), then this function
;;;;; returns, depending on the value of `test', the function `i' name, its
;;;;; event handler name...
   common c_ben,winList,maxWin,singleRun,topBase,regNames,winNames,winNum
   common ben_data,wk,benw,benx,beny,benn,benp,benpar
   if (i lt 0) then begin
      regNames=["desk","int","df","rgp"]
      winNames=["LAMP Desktop","Radial Integration ","Scroll Spectra", $
	 "Mask & Group"]
      maxWin=3
      temp=size(winNames)
      winNum=temp(1)
      winList=intarr(winNum,maxWin)
      singleRun=intarr(winNum)
      wk=0 & benw=0 & benx=0 & beny=0 & benn=0 & benp=0 & benpar=0
      return,winNum-1
   end
   if (i ge winNum) then return,"Unknown"
   case test of
      0:    return,("ben_" + regNames(i))
      1:    return,winNames(i)
      2:    return,(regNames(i) + "_event")
      3:    return,("ben_" + regNames(i) + "_create")
      4:    return,(regNames(i) + "_testModule('areYouHere')")
      else: return,regNames(i)
   end
end

function desk_winNum,str
;;; Given the ID string of a module (e.g. "df", "int"),
;;; returns the module number.
   common c_ben,winList,maxWin,singleRun,topBase,regNames
   temp=where(regNames eq str,count)
   if (count eq 0) then return,0 $
   else return,temp(0)
end

function desk_ismodule,x
;;;;; Calls the "testModule" function of the specified module, in order
;;;;; to check if the module really exists. The value returned by this
;;;;; function determines whether the display function `i' can only be
;;;;; run once, or not. Those values are stored in an array, for use
;;;;; by the desk_event procedure.
   common c_ben,winList,maxWin,singleRun
   on_error,3
   status=0
   catch,status
   if (status ne 0) then res=0
   test=0
   res=execute("test=" + desk_name(x,4))
   if (res eq 0) then print,("The '" + desk_name(x,1) + $
      "' function could not be found." + string(7b))
   singleRun(x)=test
   return,res
end

function desk_newWin,x
;;;;; This function `x' returns a number to open a new module `X'. It does
;;;;; NOT check if the function can be run several times or not.
;;;;; If the function `x' is not already running, the result is always 0.
   common c_ben,winList,maxWin
   i=xregistered(desk_name(x,0))
   if (i eq 0) then return,0
   for j=0,maxWin-1 do begin
      if (winList(x,j) eq 0) then return,j
      widget_control,bad_ID=bad,winList(x,j)
      if (bad ne 0) then return,j
   end
   return,maxWin
end

function desk_loadRequest,module,wkn,w,x,y,n,p,par
;;; This function is used by a module, to have another module
;;; load the data {w x y n p par} (wkn is the workspace number if needed).
;;; The function checks how many wanted modules are already opened;
;;; if all available modules (maxWin) are opened, the data are loaded
;;; in the first module.
   common ben_data,wk,benw,benx,beny,benn,benp,benpar
   common c_ben,winList,maxWin,singleRun,topBase
   success=1
   modNum=desk_winNum(module)
   m=desk_newWin(modNum)
   if ((m ge maxWin) or (singleRun(modNum) and (m gt 0))) then begin
      widget_control,bad_ID=bad,get_uvalue=vars,winList(modNum,0)
      if (bad ne 0) then return,0
      res=execute("success=" + module + "_loadRequest(vars,wkn,w,x,y,n,p,par)")
   end else begin
      wk=wkn & benw=w & benx=x & beny=y & benn=n & benp=p & benpar=par
      wind=0
      res=execute(desk_name(modNum,3) + ",topBase,modNum,m,wind,1")
      winList(modNum,m)=wind
   end
   if res then return,success
   return,-1
end

pro desk_event,ev,uv
;;;;; This procedure is called by the LAMP desktop display function buttons.
;;;;; It checks if the wanted function (uv(1)-400) is already running,
;;;;; via the desk_newWin function, and runs a new one, or unmap the running
;;;;; one, depending if it can be opened several times, and if the maximum
;;;;; windows are already opened.
   common c_ben,winList,maxWin,singleRun,topBase
   x=uv(1)-400
   if uv(2) eq -1 then WDIAG         else  $
   if uv(2) eq -2 then TOMOGRAPHY    else  $
   if (x eq 0)    then print,"HELP WANTED" $
   else begin
      m=desk_newWin(x)
      if ((m ge maxWin) or (singleRun(x) and (m gt 0))) $
      then widget_control,bad_ID=bad,/map,winList(x,0) $
      else begin
	 wind=0
	 temp=execute(desk_name(x,3) + ",topBase,x,m,wind,0")
	 winList(x,m)=wind
      end
   end
end

pro P_BEN_EVENT,ev,uv
;;;;; Executes a "xxx_event" procedure, where xxx is a function code.
   temp=execute(desk_name(uv(2)>0,2) + ",ev,uv")
end

pro P_BEN_CREATE,base,only
;;;;; Creation of the widget_buttons that call the display function.
;;;;; The procedure checks the presence of a function by calling the
;;;;; desk_ismodule function for every function. If a function appears
;;;;; to be absent, no button is created for it.
@lamp.cbk
   common c_ben,winList,maxWin,singleRun,topBase
   topBase=base
   if (not only) then begin
      desk=widget_base(base ,/column)
      header1=widget_base(desk,/row)
      header2=widget_base(desk,/row)
      title1=widget_label(header1,value=' DISPLAY',font=ft_biggest)
      title2=widget_label(header2,value=' FUNCTIONS',font=ft_biggest)
      help=widget_button(header1,value='?',uvalue=[-88,589,0])
   endif else desk=base
   btns=desk_name(-1)
   for i=1,btns do if desk_ismodule(i) then begin
      libel=desk_name(i,1)
      if strpos(libel,"Radial") ge 0 then begin
                 btn=widget_button(desk,value=libel,menu=2)
                 bid=widget_button(btn ,value="Using Sectors (up to [128,128] data)",uvalue=[-88,(400+i),0])
                 bid=widget_button(btn ,value="Unrolling Image (large data)",uvalue=[-88,(400+i),-1]) ;See WDIAG
                 bid=widget_button(btn ,value="Tomography",uvalue=[-88,(400+i),-2]) ;See TOMOGRAPHY
      endif else btn=widget_button(desk,value=libel,uvalue=[-88,(400+i),0])
      endif
end

;;; ***************************************************************************
;;; ***				  SCROLL SPECTRA			    ***
;;; ***************************************************************************


pro df_event,ev,uv
   widget_control,get_uvalue=varAccess,ev.top
   case uv(1) of
      400:  begin wait,.05 & widget_control,/destroy,ev.top & end
      401:  BEN_CHANGE_WORKSPACE,uv
      402:  df_help,ev.top
      403:  df_loadWorkspace,varAccess,uv(3),uv(4)
      406:  df_change_scale,varAccess,uv(3)
      407:  df_mini_event,varAccess,ev
      408:  df_slider_event,varAccess,ev.value
      409:  df_animate,varAccess,uv(3),ev.top
      410:  df_change_numb,varAccess,ev.value
      412:  df_psfile,varAccess,ev.top,uv(2)
      413:  df_select_sym,varAccess,uv(3),uv(4)
      414:  df_dialog_event,varAccess,ev,uv
      415:  df_toggle_load,varAccess,ev.select
      416:  df_draw_event,varAccess,ev
      417:  df_unzoom,varAccess,1,-1
      else: print,"DF_EVENT:",uv
   end
end

function df_testModule,x
;;;;; Returns 0, for `singleRun'. That means it can run several times.
   return,0
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; DEFILEMENT DE TOUS LES SPECTRES
;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-


pro df_animate,varAccess,x,base
;;;;; This procedure is called by the animation menu. Whether an animation
;;;;; is already running or not, it only changes the speed, or calls
;;;;; the df_animation_loop procedure.
   widget_control,get_uvalue=opt,varAccess.options
   opt.speed=x
   if (x lt 0) then opt.anim=1
   widget_control,set_uvalue=opt,varAccess.options
   if ((x ge 0) and opt.anim) then df_animation_loop,varAccess,base
end

pro df_animation_loop,varAccess,base
;;;;; This procedure contains the loop for the animation.
;;;;; This loop does several things: first, the user_value are read,
;;;;; so that the variables used for plotting are always updated.
;;;;; Then the plot is done; the number of curves shown on each
;;;;; images depends on the value of the 'number of plots' slider.
;;;;; At the end of the loop, another 'wait' loop is run, so the animation
;;;;; does not run too fast; the `speed' setting is used in this
;;;;; loop. The loop also contains a call to the widget_event procedure.
;;;;; So, during the animation, the user can change the speed, the xrange,
;;;;; the number or plots, or the displayed workspace. A test to check
;;;;; if the window has not been destroyed (eg, by using the 'Done' button)
;;;;; is done in the main loop.
common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current

   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,/sensitive,IDs.stopMenu
   widget_control,sensitive=0,IDs.psBtn
   widget_control,sensitive=0,IDs.dfSlider
   opt.anim=0
   widget_control,set_uvalue=opt,varAccess.options
   wset,IDs.draw
   erase
   i=opt.disp
   window,0,xsize=cst.dimx,ysize=cst.dimx,/pixmap,retain=2
 ;DID!!^
   CATCH,stat
   IF stat eq 0 then begin  
   repeat begin
      widget_control,get_uvalue=temp,varAccess.datAccess
      widget_control,get_uvalue=data,temp.w
      widget_control,get_uvalue=datx,temp.x
      widget_control,get_uvalue=daty,temp.y
      widget_control,get_uvalue=dims,temp.dims
      widget_control,get_uvalue=rgs,varAccess.ranges
      widget_control,get_uvalue=lmt,varAccess.limits
      i=(i+dims(1)+2) mod (dims(1)+1)
      if (opt.scale le 1) then begin
	 r=round(rgs.xspec)
	 if (r(0) eq r(1)) then r=fix([rgs.xspec(0)-1,rgs.xspec(1)+1])
	 yrg=[min(lmt.minsp(r(0):r(1))),max(lmt.maxsp(r(0):r(1)))]
      end $
      else yrg=rgs.yspec
      
      wset,0 & trap_current=0

      plot,rgs.dxspec,yrg,/nodata,/device, $
	 xstyle=9,ystyle=9,xtitle=("Spectrum # " + NUM2STR(1+i,0) + $
	 " - Value " + NUM2STR(daty(i),1)),charsize=1.2,font=0, $
	 pos=[cst.borders(0),cst.borders(0),cst.borders(1),cst.borders(1)], $
	 yticklen=1.,ygridstyle=1
;DID!!^
      for j=0,(opt.num-1) do begin
	 ind=j+i-opt.num/2
	 if ((ind ge 0) and (ind le dims(1))) then $
	    if (ind eq i) then oplot,datx,data(*,ind), $
	       color=(130+3*opt.num-(20-opt.num)*j),psym=opt.sym $
	    else oplot,datx,data(*,ind),lineStyle=1, $
	       color=(130+3*opt.num-(20-opt.num)*j),psym=opt.sym
      end
      df_set_slider,varAccess,i
      widget_control,bad_ID=bad,base
      if (bad ne 0) then return
      wset,IDs.draw
      device,copy=[0,0,cst.dimx,cst.dimx,0,0,0]
      j=0
      repeat begin
	 temp=widget_event(/nowait)
	 widget_control,bad_ID=bad,get_uvalue=opt,varAccess.options
	 if (bad ne 0) then opt.anim=1
	 if opt.anim then j=opt.speed $
	 else j=j+1
	 wait,.05
      endrep until (j ge opt.speed)
   endrep until opt.anim
   ENDIF  else  opt.anim=1
   widget_control,bad_ID=bad,set_uvalue=opt,varAccess.options
   widget_control,bad_ID=bad,sensitive=0,IDs.stopMenu
   widget_control,bad_ID=bad,/sensitive,IDs.psBtn
   widget_control,bad_ID=bad,/sensitive,IDs.dfSlider
   widget_control,bad_ID=bad,/clear_events,base
   if (bad eq 0) then df_display,varAccess,i,0,1,-1
end


;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; ZOOM, RANGES MODIFICATION
;;; -+-+-+-+-+-+-+-+-+-+-+-+-

pro df_zoom,varAccess,test,add,xrg,yrg,minPix
;;;;; This procedure zooms, with an xrange of xrg, and an yrange or yrg if
;;;;; the autoscale variable is set to <no autoscale>. If test is set to 0,
;;;;; the xrg and yrg are given in 'mini' window coordinates; otherwise,
;;;;; the parameters are given in the main 'scroll' window corrdinates.
;;;;; If `add' is set to 1, then the current ranges are added to the zooms
;;;;; list, by calling df_add_zoom.
;;;;; The `minPix' parameter is only to be passed to the df_display procedure.
;;;;; Its purpose is to avoid recalculating a picture when it is already
;;;;; known somewhere; it is used in loops, when a pixmap is created before
;;;;; the loop, and df_zoom called in the loop.
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=datx,temp.x
   widget_control,get_uvalue=dims,temp.dims
   widget_control,get_uvalue=rgs,varAccess.ranges
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=cst,varAccess.constants
   if add then df_add_zoom,varAccess,rgs.Xmini,rgs.Ymini
   if (opt.scale le 1) then temp=(xrg(0) eq xrg(1)) $
   else temp=((xrg(0) eq xrg(1)) or (yrg(0) eq yrg(1)))
   if temp then return
   if test then begin
      newrg=xrg*(rgs.dxspec(1)-rgs.dxspec(0)) + rgs.dxspec(0)
      rgs.yspec=[min(yrg),max(yrg)]
   end else begin
      rgs.Xmini=[max([0,min(xrg)]),min([cst.dimx,max(xrg)])]
      if (opt.scale ne 2) then rgs.Ymini=[0,cst.dimy] $
      else rgs.Ymini=[max([0,min(yrg)]),min([cst.dimy,max(yrg)])]
      newrg=(rgs.Xmini*(rgs.dxmini(1)-rgs.dxmini(0)))/cst.dimx+rgs.dxmini(0)
      rgs.yspec=(rgs.Ymini*(rgs.range(1)-rgs.range(0)))/cst.dimy+rgs.range(0)
   end
   newrg=[min(newrg),max(newrg)]
   temp=where((datx ge newrg(0)) and (datx le newrg(1)),found)
   if (found ge 0) then begin
      rgs.dxspec=newrg
      rgs.xspec=[min(temp),max(temp)]
   end
   if test then begin
      rgs.Xmini=(cst.dimx*(newrg-rgs.dxmini(0)))/ $
	 (rgs.dxmini(1)-rgs.dxmini(0))
      if (opt.scale ne 2) then rgs.Ymini=[0,cst.dimy] $
      else rgs.Ymini=(cst.dimy*(rgs.yspec-rgs.range(0)))/ $
	 (rgs.range(1)-rgs.range(0))
   end
   widget_control,set_uvalue=rgs,varAccess.ranges
   df_autoscale,varAccess,1
   df_display,varAccess,opt.disp,1,1,minPix
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; GESTION DE LA LISTE DES ZOOMS (+ APPEL A 'DF_ZOOM')
;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

pro df_unzoom,varAccess,test,minPix
;;;;; This procedure calls the df_zoom procedure with the top element
;;;;; of the zooms list (a 'Last In First Out' stack) if test is set to 0.
;;;;; Otherwise, df_zoom is called with the full ranges.
;;;;; If the zooms list is used, then the top element is removed from
;;;;; the stack.
   widget_control,get_uvalue=zooms,varAccess.zooms
   widget_control,get_uvalue=cst,varAccess.constants
   temp=size(zooms)
   xrg=[0,cst.dimx]
   yrg=[0,cst.dimy]
   if test then df_zoom,varAccess,0,1,xrg,yrg,minPix $
   else begin
      if (temp(0) gt 1) then begin
	 xrg=zooms(0:1,temp(2)-1)
	 yrg=zooms(2:3,temp(2)-1)
	 zooms=zooms(*,0:temp(2)-2)
	 widget_control,set_uvalue=zooms,varAccess.zooms
      end
      df_zoom,varAccess,0,0,xrg,yrg,minPix
   end
end

pro df_add_zoom,varAccess,valx,valy
;;;;; Adds [valx,valy] to the zooms list. The zoom list is a 'Last In First Out'
;;;;; stack, of a maximum size of 'varAccess.constants.zoom_max' elements.
   widget_control,get_uvalue=zooms,varAccess.zooms
   widget_control,get_uvalue=cst,varAccess.constants
   temp=size(zooms)
   if (temp(2) ge cst.zoom_max) then begin
      temp=zooms(0)
      zooms=shift(zooms,0,-1)
      zooms(*,0)=temp
      zooms(*,cst.zoom_max-1)=[valx,valy]
   end $
   else zooms=[[zooms],[[valx,valy]]]
   widget_control,set_uvalue=zooms,varAccess.zooms
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; MINI PICTURE UPDATE
;;; -+-+-+-+-+-+-+-+-+-

pro df_update_mini,varAccess,pixWin,test,xrg,yrg
;;;;; This procedure copies the current spectrum in the mini window,
;;;;; and draws over it the 'selection' rectangle, defined by the
;;;;; coordinates xrg and yrg, if test is set to 1, or by the saved
;;;;; coordinates otherwise. If `pixWin' is less than 1, then it is assumed
;;;;; that no pixmap window is containing the desired plot, so it is
;;;;; created by calling df_makePix. If `pixWin' is greater or equal to 1,
;;;;; then the window `pixWin' is copied into the mini window.
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=IDs,varAccess.IDs
   if (pixWin le 0) then begin
      pixWin=1
      df_makePix,varAccess,pixWin,0
   end
   widget_control,get_uvalue=rgs,varAccess.ranges
   x=rgs.Xmini
   y=rgs.Ymini
   if test then begin
      x=[max([0,min(xrg)]),min([cst.dimx,max(xrg)])]
      y=[max([0,min(yrg)]),min([cst.dimy,max(yrg)])]
   end
   window,0,xsize=cst.dimx,ysize=cst.dimy,/pixmap,retain=2
   device,copy=[0,0,cst.dimx,cst.dimy,0,0,pixWin]
;;;;; The graphics function number 10 (=> GXinvert) is used for
;;;;; drawing the selection rectangle.
   invert=sys_dep      ('INVERT')
   device,get_graphics_function=temp,set_graphics_function=invert
   polyfill,[x(0),x(1),x(1),x(0)],[y(0),y(0),y(1),y(1)],/device
   device,set_graphics_function=temp
   wset,IDs.mini
   device,copy=[0,0,cst.dimx,cst.dimy,0,0,0]
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; MODIFYING THE SETTINGS
;;; -+-+-+-+-+-+-+-+-+-+-+

pro df_change_numb,varAccess,n
;;;;; This procedure is called by the 'number of plots' slider. It updates
;;;;; the variables 'options.num', the 'pos' array, and the display.
   widget_control,get_uvalue=opt,varAccess.options
   if (n eq opt.num) then return
   widget_control,get_uvalue=pos,varAccess.pos
   widget_control,get_uvalue=cst,varAccess.constants
   opt.num=n
   pos=cst.dimx*rotate(indgen(7*n),2)/(7.0*n)
   widget_control,set_uvalue=opt,varAccess.options
   widget_control,set_uvalue=pos,varAccess.pos
   df_display,varAccess,opt.disp,1,0,-1
end

pro df_slider_event,varAccess,x
;;;;; Event sent by the scrolling slider. The 'options.disp' variable is
;;;;; modified by calling 'df_display', which also updates the display.
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=dims,temp.dims
   df_display,varAccess,(dims(1)-x)>0,0,1,-1
end

pro df_set_slider,varAccess,x
;;;;; This procedure is used to set the scrolling slider to a given value.
;;;;; It is used in the 'df_animation_loop' procedure, for example.
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=dims,temp.dims
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,bad_ID=temp,set_value=long(dims(1)-x)>0,IDs.dfslider
end

pro df_change_scale,varAccess,type
;;;;; Updates the yrange and the display when the autoscale type is changed.
   widget_control,get_uvalue=opt,varAccess.options
   if (type ne opt.scale) then begin
      opt.scale=type
      widget_control,set_uvalue=opt,varAccess.options
      df_autoscale,varAccess,1
      df_display,varAccess,opt.disp,1,1,-1
   end
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; AUTOSCALE
;;; -+-+-+-+-

pro df_autoscale,varAccess,test
;;;;; This procedure calculates the new yrange, according to the
;;;;; current xrange, and to the autoscale type. If the autoscale is set
;;;;; to <all plots>, then the new yrange is not calculated, unless
;;;;; the `test' parameter is set to 1.
   widget_control,get_uvalue=opt,varAccess.options
   if ((opt.scale ne 1) and ((opt.scale gt 1) or (test eq 0))) then return
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=data,temp.w
   widget_control,get_uvalue=datx,temp.x
   widget_control,get_uvalue=dims,temp.dims
   widget_control,get_uvalue=rgs,varAccess.ranges
   widget_control,get_uvalue=lims,varAccess.limits
   temp=round(rgs.xspec)
   if (temp(0) eq temp(1)) then temp=fix([rgs.xspec(0)-1,rgs.xspec(1)+1])
   temp=[max([0,temp(0)]),min([dims(0),temp(1)])]
   if (opt.scale eq 1) then begin
      arr=data(temp(0):temp(1),max([0,opt.disp-opt.num/2]): $
	 min([dims(1),opt.disp+(opt.num-1)/2]))
      rgs.yspec=[min(arr),max(arr)]
   end $
   else rgs.yspec=[min(lims.minsp(temp(0):temp(1))), $
      max(lims.maxsp(temp(0):temp(1)))]
   temp=data(*,opt.disp)
   if (rgs.yspec(0) eq rgs.yspec(1)) then rgs.yspec(0)=rgs.yspec(0)-0.01
   widget_control,set_uvalue=rgs,varAccess.ranges
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; 'DF_DRAW_EVENT' SUBROUTINES
;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-

function df_zone,varAccess,y
;;;;; This function returns the number of the 'zone' containing
;;;;; the points at (x,`y'). If the points is not in any 'zone', then
;;;;; the returned value is -1.
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=pos,varAccess.pos
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=dims,temp.dims
   found=-1
   for i=0,opt.num-1 do $
      if ((y le pos(7*i)) and (y ge pos(7*i+4))) then found=i
   temp=found+opt.disp-opt.num/2
   if ((temp ge 0) and (temp le dims(1))) then return,found $
   else return,-1
end

function df_correct,varAccess,x
;;;;; This function returns a corrected value of x, so that when plotting
;;;;; the selection rectangle, it is included in the axis.
   widget_control,get_uvalue=cst,varAccess.constants
   return,1.0*min([cst.borders(1),max([cst.borders(0),x])])
end

function df_selecty,varAccess,y0,y1
;;;;; This function returns data used when dragging the mouse in the
;;;;; scroll window, to draw the selection rectangle. It returns two
;;;;; values, r0 and r1; the selection rect will be of the form
;;;;; {x0,(zone_bottom + `r0'),x1,(zone_top - `r1')}.
   widget_control,get_uvalue=pos,varAccess.pos
   widget_control,get_uvalue=opt,varAccess.options
   r0=0
   r1=0
   if (opt.scale eq 2) then begin
      temp=df_zone(varAccess,y0)
      if (temp ge 0) then begin
	 r0=y0-pos(7*temp+4)
	 r1=max([0,pos(7*temp)-max([pos(7*temp+4),y1])])
      end $
      else begin
	 temp=df_zone(varAccess,y1)
	 if (temp ge 0) then $
	    if (y1 gt y0) then $
	    r1=pos(7*temp)-max([pos(7*temp+4),y1]) $
	    else r0=y1-pos(7*temp+4)
      end
   end
   return,[r0,r1]
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; SCROLL WINDOW EVENT
;;; -+-+-+-+-+-+-+-+-+-

pro df_draw_event,varAccess,ev
;;;;; This procedure exits immediatly if the event is a release event.
;;;;; If an animation is running, the clicking in this window just stops
;;;;; the animation. Pressing the right mouse button calls the df_unzoom
;;;;; procedure. Dragging the mouse with the left mouse button draws a
;;;;; selection rectangle; when the mouse button is released, the df_zoom
;;;;; procedure is called. For drawing the selection rectangle, a pixmap
;;;;; of the displayed plots is first created; then, a loop reads the mouse
;;;;; position, and copies the pixmap into the picmap ID 0, and plots over
;;;;; this pixmap the selection rectangles. Then, the pixmap ID 0, is
;;;;; copied to the scroll window.
   if (ev.type ne 0) then return
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=dims,temp.dims
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=pos,varAccess.pos
   widget_control,get_uvalue=rgs,varAccess.ranges
   if (opt.anim eq 0) then begin
      opt.anim=1
      widget_control,set_uvalue=opt,varAccess.options
      return
   end
   if (ev.press eq 1) then begin
      p0=[df_correct(varAccess,ev.x),ev.y]
      window,0,xsize=cst.dimx,ysize=cst.dimx,/pixmap,retain=2
      thePix=1
      df_makePix,varAccess,thePix,1
      wset,IDs.draw
      repeat begin
	 cursor,x,y,/device,/nowait
	 if (x lt 0) then begin
	    if opt.anim then $
	       device,copy=[0,0,cst.dimx,cst.dimx,0,0,thePix]
	    return
	 end
	 x0=df_correct(varAccess,x)
	 x=[p0(0),x0,x0,p0(0)]
	 y=df_selecty(varAccess,p0(1),y)
	 wset,0
	 device,copy=[0,0,cst.dimx,cst.dimx,0,0,1]
   	 invert=sys_dep      ('INVERT')
	 device,get_graphics_function=temp,set_graphics_function=invert
	 for i=0,(opt.num-1) do begin
	    ind=i+opt.disp-opt.num/2
	    if ((ind ge 0) and (ind le dims(1))) then begin
	       temp1=pos(7*i+4)+y(0)
	       temp2=pos(7*i)-y(1)
	       polyfill,x,[temp1,temp1,temp2,temp2],/device
	    end
	 end
	 device,set_graphics_function=3
	 wset,IDs.draw
	 device,copy=[0,0,cst.dimx,cst.dimx,0,0,0]
	 device,set_graphics_function=temp
      end until ((!err mod 2) eq 0)
      xrg=([p0(0),x0]-cst.borders(0))/(cst.borders(1)-cst.borders(0))
      temp=[y(0),pos(0)-pos(4)-y(1)]/(pos(0)-pos(4))
      yrg=(rgs.yspec(1)-rgs.yspec(0))*temp+rgs.yspec(0)
      df_zoom,varAccess,1,1,xrg,yrg,-1
   end
   if (ev.press eq 4) then df_unzoom,varAccess,0,-1
   widget_control,/clear_events,ev.top
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; MINI WINDOW EVENT
;;; -+-+-+-+-+-+-+-+-

pro df_mini_event,varAccess,ev
;;;;; This procedure quits if the event is not a <button pressed>.
;;;;; If the button is the right one, then the df_unzoom is called. Otherwise,
;;;;; a pixmap with the currently displayed spectrum is created. It is used
;;;;; in the loops that read the cursor position. If the pressed button is
;;;;; the center one, then the loop draws a rectangle of a constant size
;;;;; in pixmap ID 0, where the previous pixmap has been copied, then pixmap
;;;;; ID 0 is copied to the mini window, and the df_zoom procedure is called,
;;;;; so that moving the selection rect is updated in real time. If the
;;;;; pressed button is the right one, then the loop just draws a new selection
;;;;; rect, with the same method as above. In both case, the loop exits when
;;;;; the mouse button is released. The events are cleared at the end of
;;;;; the procedure, so pressing several buttons generates only one event,
;;;;; for the first button pressed.
   thePix=1
   if (ev.type ne 0) then return
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=rgs,varAccess.ranges
   widget_control,get_uvalue=IDs,varAccess.IDs
   df_makePix,varAccess,thePix,0
   if (ev.press eq 1) then begin
      x0=ev.x
      y0=ev.y
      p0=[x0,y0]
      wset,IDs.mini
      repeat begin
	 cursor,x,y,/device,/nowait
	 if (x lt 0) then $
	    if (x0 ge p0(0)) then x0=cst.dimx $
	    else x0=0 $
	 else x0=x
	 if (y lt 0) then $
	    if (y0 ge p0(1)) then y0=cst.dimy $
	    else y0=0 $
	 else y0=y
	 if (opt.scale le 1) $
	 then df_update_mini,varAccess,thePix,1,[p0(0),x0],[0,cst.dimy] $
	 else df_update_mini,varAccess,thePix,1,[p0(0),x0],[p0(1),y0]
      end until ((!err mod 2) eq 0)
      df_zoom,varAccess,0,1,[p0(0),x0],[p0(1),y0],thePix
   end
   if (ev.press eq 2) then begin
      if ((ev.x lt rgs.Xmini(0)) or (ev.x gt rgs.Xmini(1)) or $
	 (ev.y lt rgs.Ymini(0)) or (ev.y gt rgs.Ymini(1))) then return
      p0=[ev.x,ev.y]
      x=ev.x
      y=ev.y
      used1=rgs.Xmini
      used2=rgs.Ymini
      repeat begin
	 if (opt.scale le 1) $
	 then df_zoom,varAccess,0,0,(used1-p0(0)+x),[0,cst.dimy],thePix $
	 else df_zoom,varAccess,0,0,(used1-p0(0)+x),(used2-p0(1)+y),thePix
	 cursor,x,y,/device,/nowait
      end until ((!err mod 4) lt 2)
   end
   if (ev.press eq 4) then df_unzoom,varAccess,0,thePix
   widget_control,/clear_events,ev.top
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; PIXMAP PLOTTING, SCROLL WINDOW DISPLAY UPDATE
;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro df_makePix,varAccess,x,y
;;;;; This procedure creates a pixamp ID `x', and draws inside the
;;;;; plots corresponding to the scroll window (`y' set to 1),
;;;;; or to the mini window (`y' set to 0), according to the settings.
common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current

   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=data,temp.w
   widget_control,get_uvalue=datx,temp.x
   widget_control,get_uvalue=daty,temp.y
   widget_control,get_uvalue=dims,temp.dims
   widget_control,get_uvalue=rgs,varAccess.ranges
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=pos,varAccess.pos
   widget_control,get_uvalue=cst,varAccess.constants
   window,x,xsize=cst.dimx,ysize=cst.dimx,/pixmap,retain=2 & trap_current=x
   if y then $
      for i=0,(opt.num-1) do begin
	 ind=i+opt.disp-opt.num/2
	 if ((ind ge 0) and (ind le dims(1))) then begin
	    plot,rgs.dxspec,rgs.yspec,/nodata,/device,/noerase, $
	       xtitle=("Spectrum # " + NUM2STR(1+ind,0) + " - Value " + $
	       NUM2STR(daty(ind),1)),yticks=1+5/sqrt(opt.num), $
	       pos=[cst.borders(0),pos(7*i+4),cst.borders(1),pos(7*i)], $
	       charsize=1.2,font=0,xstyle=9,ystyle=9, $
	       xticklen=1. ,yticklen=1.,xgridstyle=1,ygridstyle=1
;DID!!^
	    oplot,datx,data(*,ind),psym=opt.sym, $
	       color=(130+3*opt.num-(20-opt.num)*i)
	 end
      end $
   else begin
      rgs.range=[min(data(*,opt.disp)),max(data(*,opt.disp))]
      if (opt.scale ne 2) then rgs.Ymini=[0,cst.dimy] $
      else rgs.Ymini=(cst.dimy*(rgs.yspec-rgs.range(0)))/ $
	 (rgs.range(1)-rgs.range(0))
      plot,rgs.dxmini,rgs.range,/nodata,pos=[0,0,cst.dimx,cst.dimy], $
	 /device,xstyle=5,ystyle=5
      oplot,datx,data(*,opt.disp),color=120
      widget_control,set_uvalue=rgs,varAccess.ranges
   end
end

pro df_display,varAccess,x,y,z,minPix
;;;;; This procedure displays the spectrum `x', if the displayed one is
;;;;; not the `x' one, or if `y' is set to 1. It modifies the variable
;;;;; settings.disp and after that, calls df_makePix to create the new
;;;;; plots picture; the pixmap is then copied into the scroll window.
;;;;; If z is set to 1, the mini window is also updated, by calling
;;;;; the procedure df_update_mini.
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=dims,temp.dims
   widget_control,get_uvalue=opt,varAccess.options
   if (((x eq opt.disp) and (y eq 0)) or (x gt dims(1))) then return
   opt.disp=x
   widget_control,set_uvalue=opt,varAccess.options
   df_autoscale,varAccess,0
   if opt.anim then begin
      df_makePix,varAccess,0,1
      widget_control,get_uvalue=cst,varAccess.constants
      widget_control,get_uvalue=IDs,varAccess.IDs
      wset,IDs.draw
      device,copy=[0,0,cst.dimx,cst.dimx,0,0,0]
   end
   if z then df_update_mini,varAccess,minPix,0
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; LECTURE DES WORKSPACES DE LAMP
;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

pro df_loadRange,varAccess,test,r1,r2,r3,r4,r5,r6,r7
;;;;; This procedure writes the parameters into user value of
;;;;; the correct widget_label; which params, and which widget_label
;;;;; is determined by the `test' parameter: when set to 0, the seven
;;;;; parameters r1 to r7 are stored as ranges, when set to 1, r1 and r2
;;;;; only are stored into varAccess.limits.
   case test of
      0:    widget_control,varAccess.ranges, $
	       set_uvalue={dxmini:r1,dxspec:r2, $
	       xmini:r3,ymini:r4,range:r5,xspec:r6,yspec:r7}
      1:    widget_control,varAccess.limits, $
		  set_uvalue={minsp:r1,maxsp:r2}
      else: return
   end
end

pro df_loadData,varAccess,w,x,y,n,p,par,dims
;;;;; Writes the parameters into the data widget user values.
;;;;; The purpose of this procedure is only to avoid to type
;;;;; several times the same commands.
   widget_control,get_uvalue=data,varAccess.datAccess
   widget_control,set_uvalue=w,data.w
   widget_control,set_uvalue=x,data.x
   widget_control,set_uvalue=y,data.y
   widget_control,set_uvalue=n,data.n
   widget_control,set_uvalue=p,data.p
   widget_control,set_uvalue=par,data.par
   widget_control,set_uvalue=dims,data.dims
end

function df_loadRequest,varAccess,wkn,w,x,y,n,p,par
;;;;; This procedure modifies all variables for loading new data.
;;;;; All the needed data are passed as parameters. The returned value
;;;;; is 0 if `w' does not meet the size requirements (ie two-dimensional
;;;;; array), otherwise, the result is 1.
;;;;; When loading new data, the ranges, the limit values must be modified;
;;;;; the options.disp might need to be changed too, if the new data contains
;;;;; less spectra than the currently displayed spectrum.
   wkSize=size(w)
   if (wkSize(0) ne 2) then return,0
   temp=size(x)
   if (temp(0) ne 1) or (temp(1) ne wksize(1)) then x=indgen(wksize(1))
   temp=size(y)
   if (temp(0) ne 1) or ((temp(0) eq 1) and (temp(1) ne wksize(2))) then y=indgen(wksize(2))
   temp=size(p)
   if (temp(0) ne 1) or (temp(1) ne 31) then p=fltarr(31)
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=rgs,varAccess.ranges
   widget_control,get_uvalue=cst,varAccess.constants
   df_loadData,varAccess,w,x,y,n,p,par,[wkSize(1)-1,wkSize(2)-1]
   df_numor,varAccess
   opt.disp=min([opt.disp,wkSize(2)-1])
   rgs.dxmini=[min(x),max(x)]
   rgs.dxspec=rgs.dxmini
   maxs=fltarr(wkSize(1))
   mins=fltarr(wkSize(1))
   for i=0,wkSize(1)-1 do maxs(i)=max(w(i,*))
   for i=0,wkSize(1)-1 do mins(i)=min(w(i,*))
   df_loadRange,varAccess,1,mins,maxs
   widget_control,set_slider_min=0                ,IDs.dfslider
   widget_control,set_slider_max=long(wkSize(2)-1),IDs.dfslider
   widget_control,set_uvalue=[[0,cst.dimx,0,cst.dimy]],varAccess.zooms
   widget_control,set_uvalue=opt,varAccess.options
   widget_control,set_uvalue=rgs,varAccess.ranges
   if opt.anim then begin
      df_set_slider,varAccess,opt.disp
      df_update_mini,varAccess,-1,0
      if opt.surf then df_surf,varAccess
   end
   widget_control,get_uvalue=opt,varAccess.options
   temp=opt.anim
   opt.anim=(opt.surf eq 0)
   widget_control,set_uvalue=opt,varAccess.options
   df_zoom,varAccess,0,0,[0,cst.dimx],[0,cst.dimy],-1
   widget_control,get_uvalue=opt,varAccess.options
   opt.anim=temp
   widget_control,set_uvalue=opt,varAccess.options
   return,1
end

pro df_loadWorkspace,varAccess,x,labID
;;;;; This procedure reads all the data linked to the LAMP workspace Wx,
;;;;; and calls the df_loadRequest. It also prints messages about the
;;;;; current status ("loading", or "invalid workspace").
   wksize=BEN_WK_SIZE(x)
;DID!!
   if (wkSize(0) lt 1) or (wkSize(0) gt 2) then begin
      widget_control,set_value=("'W" + NUM2STR(x,0) + "' is invalid."),labID
      print,string(7b)
      return
   end $
   else widget_control,set_value=("Loading 'W" + NUM2STR(x,0) + "'."),labID
   BEN_READ_WK,x,data,datx,daty,datn,datp,datpar
;DID!!
   if (wkSize(0) eq 1) then begin
   	data  =[[data],[data]]
   	datay =[1,1]
   	wkSize=size(data)
   endif

   temp=df_loadRequest(varAccess,x,data,datx,daty,datn,datp,datpar)
   if (temp eq 0) then begin
      widget_control,set_value=("Failed !"),labID
      print,string(7b)
   end else widget_control,set_value=" ",labID
end

pro df_toggle_load,varAccess,x
;;;;; This procedure modifies the options.surface variable, that sets
;;;;; whether a surface is plotted or not, when loading a workspace.
   widget_control,get_uvalue=opt,varAccess.options
   opt.surf=x
   widget_control,set_uvalue=opt,varAccess.options
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; PLOT SYMBOL SELECTION
;;; SURFACE PLOT WHEN LOADING
;;; -+-+-+-+-+-+-+-+-+-+-+-+-

pro df_select_sym,varAccess,test,sym
;;;;; This procedure modifies the variable that hold the current
;;;;; plot symbol; then, the display is updated.
   widget_control,get_uvalue=opt,varAccess.options
   if test then opt.sym=sym $
   else opt.lines=-opt.lines
   if (opt.sym lt 10) then opt.sym=opt.lines*abs(opt.sym) $
   else opt.sym=10
   widget_control,set_uvalue=opt,varAccess.options
   df_display,varAccess,opt.disp,1,0,-1
end

pro df_surf,varAccess
;;;;; Simply plots a surface, using Xi and Yi, in the scroll window.
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=data,temp.w
   widget_control,get_uvalue=datx,temp.x
   widget_control,get_uvalue=daty,temp.y
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=cst,varAccess.constants
   wset,IDs.draw
   m=64.0
   shade_surf,data,datx,daty,pos=[m,m,cst.dimx-m,cst.dimx-m],/device
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; HELP
;;; -+-+

pro df_help,base
   print,"HELP WANTED"
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; POSTSCRIPT FILE
;;; -+-+-+-+-+-+-+-

pro df_psfile,varAccess,base,n
;;;;; This procedure creates and displays a dialog window, to enter
;;;;; the PostScript file name. A 'Cancel' button destroy the new
;;;;; window. The creation of the PS file is done in the df_dialog_event
;;;;; procedure.
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=datpar,temp.par
   widget_control,get_uvalue=dims,temp.dims
   widget_control,get_uvalue=opt,varAccess.options
   if (datpar(0) eq '') then defName="Spectra#" $
   else defName=("N#" +      datpar(0)    + "_Spectra#")
;DID!!
;  else defName=("N#" + NUM2STR(datpar,0) + "_Spectra#")
   if (opt.num eq 1) then defName=(defName + NUM2STR(1+opt.disp ,0))  $
   else defName=(defName + NUM2STR(1+max([0,opt.disp-opt.num/2]),0) + $
      "-" + NUM2STR(1+min([dims(1),opt.disp+opt.num/2]),0))
   wind=widget_base(title="PS file name:",/column, $
      group_leader=base,uvalue=varAccess,resource_name="lampben")
   text=widget_text(wind,value=defName,/editable,xsize=32, $
      uvalue=[-88,414,n,0])
   subase=widget_base(wind,/row)
   label=widget_label(subase,value="'.ps' will be appended.   ")
   btn=widget_button(subase,value="Cancel",uvalue=[-88,414,n,2])
   btn=widget_button(subase,value=" OK ",uvalue=[-88,414,n,1,text])
   widget_control,/realize,wind
   widget_control,/input_focus,btn
   ;widget_control,/input_focus,text
   widget_control,set_text_select=[0,strlen(defName)],text
   xmanager,"ben_dfps",wind,event_handler="lamp_event_parser",/just_reg
end

pro df_dialog_event,varAccess,ev,uv
;;;;; This procedure is called by any widget of the PS file dialog, created
;;;;; by df_psfile. If the widget is the 'Cancel' button, then the window
;;;;; is simply destroyed. If it is the 'OK' button or the text widget,
;;;;; the content of the widget_text is read. If it is empty, then the
;;;;; procedure acts if the 'Cancel' button had been pressed. Otherwise,
;;;;; the plot is set to 'ps', and the file_name to it. The plot is done,
;;;;; and the file closed. After that, the previous plot device is restored.
;;;;; If an output error occurs, the current file is closed, and the device
;;;;; restored too, and an error string is displayed. In both case, the
;;;;; dialog window is destroyed.
   case uv(3) of
      0:    widget_control,get_value=psFile,ev.id
      1:    widget_control,get_value=psFile,uv(4)
      else: psFile=""
   end
   psFile=sys_dep      ('BLANKS',psFile(0))
   if (psFile eq '') then begin
      widget_control,/destroy,ev.top
      return
   end
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=data,temp.w
   widget_control,get_uvalue=datx,temp.x
   widget_control,get_uvalue=daty,temp.y
   widget_control,get_uvalue=dims,temp.dims
;DID!!
   widget_control,get_uvalue=datpar,temp.par
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=rgs,varAccess.ranges
   psFile=psFile + '.ps'
   wplot=!D.name
   err=1
   on_ioerror,iferr
   wmulti=!P.multi
   set_plot,'ps'
   if opt.num eq 1 THEN $
   device,yoffset=11.,xoffset=.5,/inches,/landscape $
   ELSE $
   device,xsize=7.5,ysize=10.,yoffset=.75,xoffset=.5,/inches,/portrait
   device,bits_per_pixel=8,/color
   !P.multi=[0,1,opt.num,0,0]
   device,filename=psFile
   for i=0,(opt.num-1) do begin
      ind=i+opt.disp-opt.num/2
      if ((ind ge 0) and (ind le dims(1))) then begin
	 plot,rgs.dxspec,rgs.yspec,/nodata,xstyle=9,ystyle=9,charsize=1.6,font=0, $
	    xtitle=("Spectrum # " + NUM2STR(1+ind,0) + " - Value " + $
	    NUM2STR(daty(ind),1)),yticks=1+5/sqrt(opt.num), $
	    xticklen=1.,yticklen=1.,xgridstyle=1,ygridstyle=1,color=128
;DID!!
	 oplot,datx,data(*,ind),psym=opt.sym,color=128
      end
   end
;DID!!
   if n_elements(datpar) lt 2 then widx=0 else widx=fix(datpar(1))

   IF opt.num eq 1 THEN p=7. ELSE p=10.
   P_DID_PS_HEADER, p ,widx ,psFile
   err=0
iferr : $
   if err eq 1 then device,/close_file
   set_plot,wplot
   !P.multi=wmulti
   if uv(3) then tID=uv(4) $
   else tID=ev.id
   if err then widget_control,set_value=("ERROR:" + !ERR_STRING),tID $
   else widget_control,set_value=('PS file : ' + psFile),tID
   wait,.05
   widget_control,/destroy,ev.top
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; DATA INITIALIZATION
;;; -+-+-+-+-+-+-+-+-+-

pro df_numor,varAccess
;;;;; This procedure displays the number numor in a label.
   widget_control,get_uvalue=temp,varAccess.datAccess
   widget_control,get_uvalue=datpar,temp.par
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=IDs,varAccess.IDs
   if (datpar(0) eq '') then temp=' ' $
   else temp=("Numor # " +      datpar(0)   )
;DID!!
;  else temp=("Numor # " + NUM2STR(datpar,0))
   if n_elements(datpar) gt 1 then if datpar(2) ne '' then temp=strmid(datpar(2),0,52)
   widget_control,set_value=temp,IDs.numLab
   if (temp eq ' ') then temp='' $
   else temp=('  (' + temp + ')')
   widget_control,tlb_set_title=(cst.title + temp),IDs.numLab
end

pro df_initData,varAccess,request
;;;;; Fill the data variables, according to `request'. If `request' is
;;;;; set to 1, the scroll spectra window was opened because of a
;;;;; request from the desk; the function must start with the
;;;;; requested data, stored in the common `ben_data'. So, if `request'
;;;;; is set to 1, those data are loaded; if it is set to 0, default
;;;;; data are loaded, by calling `df_defaultData'.
   widget_control,get_uvalue=cst,varAccess.constants
   common ben_data,wk,w,x,y,n,p,par
   if request then begin
      wkSize=size(w)
      if (wkSize(0) eq 2) then begin
	 temp=size(x)
	 if (temp(0) ne 1) or (temp(1) ne wkSize(1)) then $
	    x=indgen(wksize(1))
	 temp=size(y)
	 if (temp(0) ne 1) or (temp(1) ne wkSize(2)) then $
	    y=indgen(wksize(2))
	 temp=size(p)
	 if (temp(0) ne 1) or (temp(1) ne 31) then $
	    p=fltarr(31)
	 df_loadData,varAccess,w,x,y,n,p,par,(wkSize(1:2)-1)
	 maxsp=fltarr(wkSize(1))
	 minsp=fltarr(wkSize(1))
	 for i=0,wkSize(1)-1 do maxsp(i)=max(w(i,*))
	 for i=0,wkSize(1)-1 do minsp(i)=min(w(i,*))
	 dxRg=float([min(x),max(x)])
	 widget_control,get_uvalue=opt,varAccess.options
	 temp=w(*,opt.disp)
	 df_loadRange,varAccess,0,dxRg,dxRg,[0,cst.dimx], $
	    [0,cst.dimy],[min(temp),max(temp)], $
	    [0,wkSize(1)-1],[min(minsp),max(maxsp)]
	 df_loadRange,varAccess,1,minsp,maxsp
      end else df_defaultData,varAccess
   end else df_defaultData,varAccess
end

pro df_defaultData,varAccess
;;; Inits the data when 'Scroll Spectra' is not called via 'request'
   widget_control,get_uvalue=cst,varAccess.constants
   dimx=63L
   dimy=63L
   data=dist(dimx+1,dimy+1)
   datx=indgen(dimx+1)
   df_loadData,varAccess,data,datx,indgen(dimy+1), $
      0,fltarr(31),0,[dimx,dimy]
   maxsp=fltarr(dimx+1)
   minsp=fltarr(dimx+1)
   for i=0,dimx do maxsp(i)=max(data(i,*))
   for i=0,dimx do minsp(i)=min(data(i,*))
   dxRg=float([min(datx),max(datx)])
   widget_control,get_uvalue=opts,varAccess.options
      temp=data(*,opts.disp)
   df_loadRange,varAccess,0,dxRg,dxRg,[0,cst.dimx],[0,cst.dimy], $
      [min(temp),max(temp)],[0,dimx],[min(minsp),max(maxsp)]
   df_loadRange,varAccess,1,minsp,maxsp
end

function df_inits,test,x
;;;;; This function returns some default value for the parameter
;;;;; settings, or constants of the program.
@lamp.cbk
   if lamp_siz lt 800 then xxx=384.0 else xxx=512.0
   if lamp_siz lt 800 then yyy= 64.0 else yyy= 64.0
   num=1
   case test of
      0:    res={dimx:xxx,dimy:yyy,zoom_max:16, $
		 borders:[yyy,xxx-32.0],title:x}
      1:    res={disp:0,num:num,sym:0,lines:-1,scale:1,anim:1,speed:3,surf:0}
      2:    res=[[0,xxx,0,yyy]]
      3:    res=xxx*rotate(indgen(7*num),2)/(7.0*num)
      else: res=0
   end
   return,res
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; WIDGET CREATION
;;; -+-+-+-+-+-+-+-

pro ben_df_create,lamp,n,m,base,request
@lamp.cbk
   if (m eq 0) then winTitle=desk_name(n,1) $
   else winTitle=(desk_name(n,1) + " " + NUM2STR(1+m,0))
   if (lamp_siz lt 1000) then begin
      base=widget_base(title=winTitle,/row,group_leader=lamp, $
	   resource_name='lampben')
      bas1=widget_base(base,/column)
      bas2=widget_base(base,/column)
      test=1
   end else begin
      base=widget_base(title=winTitle,/column,group_leader=lamp, $
	   resource_name='lampben')
      bas1=base & bas2=base
      test=0
   end

   constants=df_inits(0,winTitle)
   options  =df_inits(1)

;;;;; Creation of the first button row.
   if test then begin
      comID   =BEN_COMMANDS(bas1,n,[0,0])
      surfBase=widget_base (bas1,/nonexclusive)
      hint    =widget_base (bas1,/column,/frame)
      vars    =widget_base (bas1,/row)
   end else begin
      temp    =widget_base (bas1,/row)
      left    =widget_base (temp,/column)
      vars    =widget_base (temp,/column)
      comID   =BEN_COMMANDS(left,n,[0,0])
      surfBase=widget_base (left,/nonexclusive)
      hint    =widget_base (vars,/column,/frame)
   end

   surfBtn=widget_button(surfBase,uvalue=[-88,415,n,comID(4)], $
      value="Plot surface when loading workspace")
   numLab=widget_label  (bas1,value=" ",xsize=constants.dimx/(test+1),font=ft_b_bigger)

;;;;; Creation of the widget_label containing in their user value the
;;;;; variables used by the Scroll spectra function. Their IDs are stored
;;;;; in the top_level base user value.
   datAccess=widget_base(vars,/row)
   data	 =widget_label(datAccess,value="")
   datx	 =widget_label(datAccess,value="")
   daty	 =widget_label(datAccess,value="")
   datn	 =widget_label(datAccess,value="")
   datp	 =widget_label(datAccess,value="")
   datppp=widget_label(datAccess,value="")
   dims	 =widget_label(datAccess,value="")
   dat={w:data,x:datx,y:daty,n:datn,p:datp,par:datppp,dims:dims}
   widget_control,set_uvalue=dat,datAccess

   vars=widget_base(vars,/row)
   const =widget_label(vars,value="",uvalue=constants)
   opts	 =widget_label(vars,value="",uvalue=options)
   ranges=widget_label(vars,value="")
   zooms =widget_label(vars,value="",uvalue=df_inits(2))
   pos	 =widget_label(vars,value="",uvalue=df_inits(3))
   lmt	 =widget_label(vars,value="")
   IDs	 =widget_label(vars,value="")
   varAccess={datAccess:datAccess,constants:const,options:opts, $
      ranges:ranges,zooms:zooms,pos:pos,limits:lmt,IDs:IDs}
   widget_control,set_uvalue=varAccess,base

;;;;; All the 'variable widgets' are now created. The data is initialized
;;;;; by calling the df_initData procedure.
   df_initData,varAccess,request
   widget_control,get_uvalue=dims,dat.dims

;;;;; Creation of the 'hint' labels.
   temp=widget_label(hint,value="left button zooms")
   temp=widget_label(hint,value="right button unzooms")
   temp=widget_label(hint,value="center button moves selection")

;;;;; Creation of the mini draw window, that will show a miniature of
;;;;; the displayed spectrum, and of the scrolling window, with its
;;;;; slider, used as a scroll bar.
   mini=widget_draw(widget_base(bas2,/row,/frame),/button_events, $
      xsize=constants.dimx,ysize=constants.dimy,retain=2,uvalue=[-88,407,n])
   picts=widget_base(bas2,/row,/frame)
   draw=widget_draw(picts,xsize=constants.dimx,ysize=constants.dimx, $
      retain=2,uvalue=[-88,416,n],/button_events)
   dfslider=widget_slider(picts,min=0,max=long(dims(1)),value=long(dims(1)), $
      /vertical,/suppress_value,/drag,uvalue=[-88,408,n],ysize=constants.dimx)

;;;;; Creation of the 'number of plots' slider, the 'draw lines' slider,
;;;;; and the 'plot symbol' menu. The 'draw lines' has effect only when a
;;;;; symbol is chosen, to determine whether the points should be connected
;;;;; or not; if no plot is chosen, there is always lines between points...
   if test then begin
      temp=widget_slider(bas1,title="Number of plots",min=1,max=8, $
	                 value=options.num,/drag,uvalue=[-88,410,n])
      controls=bas1
      b_logo   =widget_base  (controls,/row)
		put_logo     ,b_logo
      drawlines=widget_button(widget_base(b_logo,/nonexclusive), $
			      value="Draw lines",uvalue=[-88,413,n,0,0])
      menus=widget_base(controls,/row)
      psBase=controls
   end else begin
      controls=widget_base(bas1,/row,space=16)
      left=widget_base(controls,/column)
      temp=widget_slider(left,title="Number of plots",min=1,max=8, $
			 value=options.num,/drag,uvalue=[-88,410,n])
      b_logo   =widget_base  (left,/row)
		put_logo     ,b_logo
      drawlines=widget_button(widget_base(b_logo,/nonexclusive), $
			      value="Draw lines",uvalue=[-88,413,n,0,0])
      menus=widget_base(left,/row,space=32)
      psBase=menus
   end
   symbol=widget_button(menus,value="Plot symbol",menu=2)
   temp=widget_button(symbol,value="< None >",uvalue=[-88,413,n,1,0])
   temp=widget_button(symbol,value=" +",uvalue=[-88,413,n,1,1])
   temp=widget_button(symbol,value=" *",uvalue=[-88,413,n,1,2])
   temp=widget_button(symbol,value=" .",uvalue=[-88,413,n,1,3])
   temp=widget_button(symbol,value=" X",uvalue=[-88,413,n,1,7])
   temp=widget_button(symbol,value="Histogram",uvalue=[-88,413,n,1,10])
   sub=widget_button(symbol,value="Other",/menu)
   temp=widget_button(sub,value="Diamond",uvalue=[-88,413,n,1,4])
   temp=widget_button(sub,value="Triangle",uvalue=[-88,413,n,1,5])
   temp=widget_button(sub,value="Square",uvalue=[-88,413,n,1,6])

;;;;; Creation of the 'animate' menu, that starts the animation, or
;;;;; modifies its speed. It also contains a 'stop' menu item, that
;;;;; is sensitive only when an animation is running.
   anim=widget_button(menus,value="Animate",menu=2)
   stopMenu=widget_button(anim,value="Stop",font=ft_b_normal, $
      uvalue=[-88,409,n,-1])
   temp=widget_button(anim,value="Slow",uvalue=[-88,409,n,15])
   temp=widget_button(anim,value="Normal",uvalue=[-88,409,n,7])
   temp=widget_button(anim,value="Fast",uvalue=[-88,409,n,2])
   psBtn=widget_button(psBase,value="To PS file...",uvalue=[-88,412,n])

;;;;; Creation of the ranges buttons. The 'full range' button has nothing
;;;;; to do with autoscale, but simply makes a total unzoom of the spectra.
   right=widget_base(controls,/column)
   fscl=widget_button(right,value="Full range",uvalue=[-88,417,n])
   scl=widget_base(right,/column,/frame)
   temp=widget_label(scl,value="Autoscale using:",font=ft_b_normal)
   temp=widget_base(scl,/exclusive,/column)
   scl0=widget_button(temp,value="All plots",uvalue=[-88,406,n,0])
   scl1=widget_button(temp,value="Displayed plots",uvalue=[-88,406,n,1])
   scl2=widget_button(temp,value="No Autoscale",uvalue=[-88,406,n,2])

;;;;; All the widgets are declared. According to the startup options,
;;;;; the toggle button states are modified, and widget IDs are stored.
   if options.surf then widget_control,/set_button,surfBtn
   temp=execute("widget_control,/set_button,scl" + NUM2STR(options.scale,0))
   if (options.lines le 0) then widget_control,/set_button,drawlines
   widget_control,sensitive=0,stopMenu
   bid=sys_dep      ('DYNLAB',base,1)
   widget_control,/realize,base & put_logo
   widget_control,get_value=drawID,draw
   widget_control,get_value=minID,mini
   IDlist={mini:minID,draw:drawID,dfSlider:dfSlider, $
      stopMenu:stopMenu,psBtn:psBtn,numLab:numLab}
   widget_control,set_uvalue=IDlist,varAccess.IDs
   df_numor,varAccess
   df_change_numb,varAccess,options.num
   df_display,varAccess,options.disp,1,1,-1
   xmanager,desk_name(n,0),base,event_handler="lamp_event_parser",/just_reg
end

;;; ***************************************************************************
;;; ***			      RADIAL INTEGRATION			    ***
;;; ***************************************************************************

pro int_event,ev,uv
   widget_control,get_uvalue=varAccess,ev.top
   case uv(1) of
      400:  begin wait,.05 & widget_control,/destroy,ev.top & end
      401:  BEN_CHANGE_WORKSPACE,uv
      402:  int_help,varAccess,ev.top
      403:  int_loadWorkspace,varAccess,uv(3),uv(4)
      405:  int_plotMenu,varAccess,uv
      406:  int_updateRay,varAccess,ev,uv(3)
      407:  int_changeZone,varAccess,ev,uv(3),uv(4)
      408:  int_miniPix,varAccess,1
      409:  int_changeCenter,varAccess
      410:  int_zonePlot,varAccess,ev.select
      411:  int_updateRes,varAccess,uv(3),uv(4),uv(5)
      412:  int_updateMini,varAccess,ev,uv
      else: print,"INT_EVENT:",varAccess,uv
   end
end

function int_testModule,x
   return,0
end

pro int_help,base
   print,"HELP WANTED"
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; PROCEDURES FOR DRAWING THE SELECTION PICTURE
;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

function int_middle,varAccess,x,y
;;;;; This function returns the middle angle of x and y.
   if (x lt 0) then x=360.0 + x
   if (x gt y) then return,(180.0+(x+y)/2.0) $
   else return,(x+y)/2.0
end

pro int_poly,a1,a2,a3,c,c_x,c_y,r1,r2,deg2rad,test
;;;;; Procedure 'int_poly' draws the section enclosed in angles
;;;;; a1 and a3, and radia r1 and r2, in the current window.
   a12=int_middle(varAccess,a1,a2)
   a23=int_middle(varAccess,a2,a3)
   temp1=[a1,a12,a2,a23,a3]*deg2rad
   temp2=[a3,a23,a2,a12,a1]*deg2rad
   x=[r1*cos(temp1),r2*cos(temp2),r1*cos(a1*deg2rad)]
   y=[r1*sin(temp1),r2*sin(temp2),r1*sin(a1*deg2rad)]
   oplot,c_x+x,c_y+y,color=c,thick=2,psym=0
   oplot,c_x-x,c_y-y,color=c,thick=2,psym=0,lineStyle=2*test
end

pro int_drawAxes,varAccess
;;;;; This procedure updates the mini widget_draw.
common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current

   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   widget_control,get_uvalue=set,varAccess.settings
   widget_control,get_uvalue=pix,varAccess.pixmap
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=col,varAccess.colors
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=opt,varAccess.options
   c_x=set.cx+0.5
   c_y=set.cy+0.5
   if (dims.dim eq dims.dimx) $
   then c_y=c_y+(dims.dim-dims.dimy)/2.0*(dims.dimx/dims.dim) $
   else c_x=c_x+(dims.dim-dims.dimx)/2.0*(dims.dimy/dims.dim)
   cur=set.alpha
   window,0,xsize=cst.minSize,ysize=cst.minSize,/pixmap,retain=2 & trap_current=0
   tvscl,pix.img,pix.xmarg,pix.ymarg,order=0
   if ((pix.xmarg ne 0) or (pix.ymarg ne 0)) then begin
      x=pix.xmarg*[1,1,-1,-1,1]+[-1,-1,cst.minSize,cst.minSize,-1]
      y=pix.ymarg*[1,-1,-1,1,1]+[-1,cst.minSize,cst.minSize,-1,-1]
      plot,x,y,xrange=[0,cst.minSize],yrange=[0,cst.minSize], $
	 /device,/noerase,pos=[0,0,cst.minSize,cst.minSize], $
	 lineStyle=1,xstyle=5,ystyle=5,psym=0
   end
   temp=[-dims.dim,dims.dim]*sqrt(2)
   plot,[0,dims.dim],[0,dims.dim],/nodata,/noerase,/device, $
      xstyle=5,ystyle=5,pos=[0,0,cst.minSize,cst.minSize],psym=0
   for i=0,(set.zones-1) do begin
      if set.zones le 8 then $
      oplot   , cos(cur*cst.deg2rad)*temp+c_x,sin(cur*cst.deg2rad)* $
	        temp+c_y,color=col(i),thick=2,psym=0
      int_poly,(cur-set.beta),cur,(cur+set.beta),col(i), $
	        c_x,c_y,set.r1,set.r2,cst.deg2rad,opt.plotZone
      cur=cur+set.gamma
   end
   polyfill,c_x+set.mx*[-1,-1,1,1],c_y+set.my*[1,-1,-1,1],color=0
   wset,IDs.mini
   device,copy=[0,0,cst.minSize,cst.minSize,0,0,0]
end

pro int_miniPix,varAccess,test
;;;;; This function calculates an array, according to the
;;;;; options, of the same size as the mini widget_draw.
;;;;; This array is stored, so that, when changing the
;;;;; mini widget_draw, it can be used for `tvscl'.
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   widget_control,get_uvalue=data,varAccess.datAccess.w
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=opt,varAccess.options
   if test then opt.logAspect=(opt.logAspect ne 1)
   xmarg=0
   ymarg=0
   if opt.logAspect then img=alog(1.0+data) $
   else img=data
   if (dims.dimx eq dims.dimy) then $
      if (((cst.minSize mod dims.dim) eq 0) or $
	 ((dims.dim mod cst.minSize) eq 0)) $
;bug Ulong then miniPix=rebin(img,cst.minSize,cst.minSize,/sample) $
           then miniPix=congrid(img,cst.minSize,cst.minSize) $
      else miniPix=congrid(img,cst.minSize,cst.minSize) $
   else begin
      dx=cst.minSize & dy=cst.minSize
      if (dims.dimx gt dims.dimy) $
      then dy=round(cst.minSize*(dims.dimy/dims.dimx)) $
      else dx=round(cst.minSize*(dims.dimx/dims.dimy))
      miniPix=congrid(img,dx,dy)
      xmarg=round((cst.minSize-dx)/2.0)
      ymarg=round((cst.minSize-dy)/2.0)
   end
   uv={img:miniPix,xmarg:xmarg,ymarg:ymarg}
   widget_control,set_uvalue=uv,varAccess.pixmap
   widget_control,set_uvalue=opt,varAccess.options
   int_drawAxes,varAccess
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; 'PLOT' MENU EVENTS
;;; -+-+-+-+-+-+-+-+-+

function int_radial,win
;******* **********
@lamp.cbk
common didmod,IDdid ,wbuf,xbuf,ybuf,titx,tity

x20=xbuf & y20=ybuf & x_tit(20)=titx & y_tit(20)=tity
return,reform(wbuf)
end

pro int_plotMenu,varAccess,uv
;** ************
;**

   widget_control,get_uvalue=set,varAccess.settings
   widget_control,get_uvalue=opt,varAccess.options
   if (uv(3) eq 0) then begin
      widget_control,get_uvalue=prev,varAccess.plots(uv(4))
      range=[min([set.r1,set.r2,prev.r(0)]),max([set.r1,set.r2,prev.r(1)])]
      int_updateRes,varAccess,uv(5),uv(6),uv(7)
      widget_control,get_uvalue=new,varAccess.plots(0)
      maxVal=max([max(prev.res),max(new.res)])
      widget_control,set_value=("Plotting..."),uv(5)
      int_plot,varAccess, new.res,round(range),opt.plotZone,1,maxVal  ,'X',uv(6),0
      int_plot,varAccess,prev.res,round(range),opt.plotZone,0,maxVal ,'X',uv(6),0
      widget_control,set_value=("Ready."),uv(5)
   end $
   else if (uv(3) eq 1) then begin
      widget_control,get_uvalue=IDs,varAccess.IDs
      widget_control,get_uvalue=temp,varAccess.plots(0)
      widget_control,set_uvalue=temp,varAccess.plots(uv(4))
      widget_control,/sensitive,IDs.menu(uv(4))
      widget_control,/sensitive,IDs.menu(uv(4)+2)
   end $
   else if (uv(3) eq 2) then begin
      widget_control,get_uvalue=toplot,varAccess.plots(uv(4))
;DID!!
;     odat=0 & range=0
;     temp=execute("odat=p"+NUM2STR(uv(4),0))
;     temp=execute("range=p" + NUM2STR(uv(4),0) + "r")
;     int_plot,varAccess,toplot.res,round(toplot.r),opt.plotZone,1,max(odat)      ,'X',uv(6),0
      int_plot,varAccess,toplot.res,round(toplot.r),opt.plotZone,1,max(toplot.res),'X',uv(6),0
   end $
   else if (uv(3) eq 3) then begin
;DID!!
      widget_control,get_uvalue=toplot,varAccess.plots(0)

      widget_control,get_uvalue=datpar,varAccess.datAccess.par
      widx=0 & cfil=''
      if n_elements(datpar) gt 1 then begin cfil=strtrim(datpar(0),2)
      					    widx=fix(datpar(1)) & endif
      psFile='radial'+cfil+'.ps'
      wplot=!D.name
      err=1
      on_ioerror,iferr
      if uv(6) eq 0 then begin
      	set_plot,'ps'
      	device,xsize=7.5,ysize=10.,yoffset=.75,xoffset=.5,/inches,/portrait
      	device,filename=psFile
      	device,bits_per_pixel=8,/color
	endif
      int_plot,varAccess,toplot.res,round(toplot.r),opt.plotZone,1,max(toplot.res),'PS',uv(6),0

      if uv(6) eq 0 then P_DID_PS_HEADER, 10. ,widx , psFile $
		    else XICUTE,'w20=int_radial(w'+strtrim(fix(widx),2)+') ;'+datpar(0)
      err=0
      iferr: if err eq 1 then device,/close_file
      set_plot,wplot
   end
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; SUB-PROCEDURES CALLED DURING INTEGRATION
;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

function int_area,varAccess,bet1,bet2
;;;;; This function returns the minimum rectangle containing
;;;;; the sector between bet1 and bet2, and r1 and r2.
   widget_control,get_uvalue=set,varAccess.settings
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   deg2rad=!pi/180.0
   r1=min([set.r1,set.r2],max=r2)
   temp=cos([bet1,bet2]*deg2rad)
   projx=[r1*temp,r2*temp]
   temp=sin([bet1,bet2]*deg2rad)
   projy=[r1*temp,r2*temp]
   left=min(projx)-1
   right=max(projx)+1
   top=max(projy)+1
   bottom=min(projy)-1
   if (bet1 lt 180) and (bet2 gt 180) then left=-(r2+1) $
   else if (bet1 gt bet2) then right=r2+1
   if (bet1 lt 90) and (bet2 gt 90) then top=r2+1 $
   else if (bet1 lt 270) and (bet2 gt 270) then bottom=-(r2+1)
   temp=round([max([0,left+set.cx]),min([dims.dimx-1,right+set.cx]), $
      max([0,bottom+set.cy]),min([dims.dimy-1,top+set.cy])])
   return,temp
end

function int_inZone,varAccess,x,y,a1,a2,r1,r2,mx,my
;;;;; This function tests if the point (x,y) is in the zone
;;;;; defined by the angles a1 and a2, and the radia r1 adn r2.
;;;;; It also verifies that (x,y) is not within the mask mx,my.
;;;;; If the point is in the zone and not masked, then the function
;;;;; returns the radius it belongs to; else, it returns -1.
   if ((abs(x) le mx) and (abs(y) le my)) then return,-1
   ray=round(sqrt(x^2+y^2))
   if ((ray lt r1) or (ray gt r2)) then return,-1
   angle=180.0*atan(y,x)/!pi
   if (angle lt 0) then angle=angle + 360.0
   if (a1 gt a2) then test=((angle lt a1) and (angle gt a2)) $
   else test=((angle lt a1) or (angle gt a2))
   if test then return,-1 $
   else return,ray
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; RADIAL INTEGRATION AND RESULT PLOTTING
;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro int_updateRes,varAccess,labelID, logID ,didID
;;;;; This procedure first calls `int_changeCenter', to read the
;;;;; widget_text, in case the user forgot to type return to validate.
;;;;; Then the integration is done. When the result is obtained, it
;;;;; is plotted by calling the `int_plot' procedure.
   int_changeCenter,varAccess
   widget_control,get_uvalue=set,varAccess.settings
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   widget_control,get_uvalue=data,varAccess.datAccess.w
   r1=min([set.r1,set.r2],max=r2)
   if (r1 eq r2) then if (r2 eq 0) then r2=1 else r1=r1-1
;   print,("alpha=" + num2str(set.alpha,0) + ", beta=" + $
;      num2str(set.beta,0) + ", r1=" + num2str(r1,0) + $
;      ", r2=" + num2str(r2,0))
   res=fltarr(2*dims.dim,2*set.zones)
   cur=set.alpha
   startTime=systime(1)
   for k=0,(set.zones-1) do begin
      if (labelID gt 0) then $
	 widget_control,set_value=("Sector #" + NUM2STR(k+1,0)),labelID
      for p=0,1 do begin
	 numb=intarr(2*dims.dim)
	 sum=fltarr(2*dims.dim)
	 bet1=(cur-set.beta+360.0+p*180.0) mod 360
	 bet2=(cur+set.beta+p*180.0) mod 360
	 rect=int_area(varAccess,bet1,bet2)
	 for i=rect(0),rect(1) do $
	    for j=rect(2),rect(3) do begin
	       temp=int_inZone(varAccess,i-set.cx,j-set.cy, $
		  bet1,bet2,r1,r2,set.mx,set.my)
	       if (temp ge 0) then begin
		  sum(temp)=sum(temp)+data(i,j)
		  numb(temp)=numb(temp)+1
	       end
	    end
	 temp=where(numb eq 0,count)
	 if (count ne 0) then begin
	    numb(temp)=1
	    sum(temp)=-1
	 end
	 res(*,2*k+p)=sum/numb
      end
      cur=cur+set.gamma
   end
   int_time=systime(1)-startTime
   ;;; The current result is stored in plots(0)...
   widget_control,set_uvalue={res:res,r:[r1,r2]},varAccess.plots(0)
;  widget_control,/sensitive,IDs.menu(0)
   ;;; ... and then it is plotted.
   widget_control,bad_ID=bad,set_value=("Plotting..."),labelID
   int_plot,varAccess,res,round([r1,r2]),opt.plotZone,1,max(res) ,'X',0 ,didID
;;; !!!
;;;  TEST RADIES (A ENLEVER QUAND MEME, APRES...)
;;; !!!
;widget_control,bad_ID=bad,set_value=("RADIES..."),labelID
;res=fltarr(dims.dim,2*set.zones)
;cur=set.alpha
;startTime=systime(1)
;for k=0,(set.zones-1) do begin
;   for p=0,1 do begin
;      a1=(cur-set.beta+360.0+p*180.0) mod 360
;      a2=(cur+set.beta+p*180.0) mod 360
;      res(*,2*k+p)=radtst(data,set.cx,set.cy,r1,r2,a1,a2)
;   end
;   cur=cur+set.gamma
;end
;rad_time=systime(1)-startTime
;window,8,retain=2,title="Radies result",xsize=422,ysize=448
;maxVal=max(res)
;plot,[r1,r2],[0,maxVal],/nodata,xstyle=9,ystyle=9,color=0,background=255, $
;   xtitle='Radius (pixels)',ytitle='Mean value',pos=[.2,.1,.9,.9],charsize=1.2
;for k=0,(set.zones-1) do begin
;colors=120/max([1.0,(set.zones-1)])*indgen(set.zones)
;for k=0,(set.zones-1) do begin
;   oplot,res(*,2*k),color=colors(k),max_value=maxVal
;   oplot,res(*,2*k+1),color=colors(k),lineStyle=2, $
;end
;      thick=3,max_value=maxVal
;end
;if (int_time ge rad_time) then temp=[" ","*"] else temp=["*"," "]
;print,(temp(0) + " Intrad : " + NUM2STR(int_time,2) + ' s')
;print,(temp(1) + " Radies : " + NUM2STR(rad_time,2) + ' s')
;;; !!!
;;; !!!
   widget_control,bad_ID=bad,set_value=("Ready."),labelID
end

pro int_plot,varAccess,data,rays,test,new,maxVal ,XPS ,logID ,didID
;;;;; This procedure plots the data (result of an integration), with
;;;;; an xrange of `rays', and a maximum value equal to `maxVal'.
;;;;; If new is equal to 1, then the previous plot is erased. The
;;;;; `test' param is used to test if `data' is the major plot or not.
;;;;; This procedure is useful to plot several results, obtained with
;;;;; different angles and/or rays, in the same window.

common didmod,IDdid ,wbuf,xbuf,ybuf,titx,tity
common c_trap,	trap_x1,trap_x2,trap_y1,trap_y2,trap_ws, trap_current


if n_elements(IDdid) eq 0 then IDdid=1
if didID ne 0 then IDdid=didID

   widget_control,get_uvalue=IDs   ,varAccess.IDs

;DID!! logID if =1 then output to a workspace

   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=datpar,varAccess.datAccess.par
   if n_elements(datpar) gt 1 then mtit=datpar(0) else mtit=''

   temp=size(data)
   zones=temp(2)/2
   colors=120/max([1.0,(zones-1)])*indgen(zones)
   if XPS eq 'X' then wset,IDs.draw
   trap_current=IDs.draw
   
   modata=data
   if (rays(0) gt 0)	     then modata(0:rays(0)-1,*)		=1+maxVal
   temp=size(modata)
   if (rays(1) lt temp(1)-1) then modata(rays(1)+1:temp(1)-1,*) =1+maxVal

   if IDdid eq 1 then begin
     	ranx=rays
     	rany=[0,maxVal]+opt.logAspect*0.1
     	titx='Radius (pixels)'
   endif else begin
        div  =rays(1)-rays(0)+1
   	if (test and new) then begin
   		 didat=fltarr(zones*2+1)
   		 div  =rays(1)-rays(0)+1
      		 for k=0,(zones-1) do begin dat =modata(rays(0):rays(1),2*k)
      		 			    idx =where(dat lt 0,count)
      		 			    if count eq div then didat(k+1)=-1 else $
      		 			       didat(k+1)      = total(dat>0) / ((div-count)>1)
      		 			    dat =modata(rays(0):rays(1),2*k+1)
      		 			    idx =where(dat lt 0,count)
      		 			    if count eq div then didat(k+zones+1)=-1 else $
      		 			       didat(k+zones+1)= total(dat>0) / ((div-count)>1)  & endfor
     		 ranx =[1,zones*2]
   	endif else begin
   		 didat=fltarr(zones+1)
      		 for k=0,(zones-1) do begin dat =modata(rays(0):rays(1),2*k:2*k+1)
      		 			    idx =where(dat lt 0,count)
      		 			    if count eq 2*div then didat(k+1)=-1 else $
      		 			       didat(k+1)      = total(dat>0) /((2*div-count)>1) & endfor
     		 ranx =[1,zones]
   	endelse
   	maxd=max(didat)
     	rany=[0,maxd]+opt.logAspect*0.1
     	titx='Sectors'
   endelse

   tity='Mean value'

   if new and (not logID)  then begin
     if opt.logAspect ne 1 then begin
      if XPS eq 'X' then plot,ranx,rany,/nodata,xstyle=9,ystyle=9,title=mtit,color=0,$
      		xtitle=titx,ytitle=tity,pos=[.2,.1,.9,.9],background=255 $
      else		 plot,ranx,rany,/nodata,xstyle=9,ystyle=9,title=mtit,$
      		xtitle=titx,ytitle=tity,pos=[.2,.1,.9,.9],$
	        xticklen=1.,yticklen=1.,xgridstyle=1,ygridstyle=1,color=128
     endif else begin
      if XPS eq 'X' then plot_io,ranx,rany,/nodata,xstyle=9,ystyle=9,title=mtit,color=0,$
      		xtitle=titx,ytitle=tity+' LOG)',pos=[.2,.1,.9,.9],background=255 $
      else		 plot_io,ranx,rany,/nodata,xstyle=9,ystyle=9,title=mtit,$
      		xtitle=titx,ytitle=tity+' LOG)',pos=[.2,.1,.9,.9],$
	        xticklen=1.,yticklen=1.,xgridstyle=1,ygridstyle=1,color=128
     endelse
   endif

   if IDdid eq 1 then begin
    if (test and new) then begin
      if logID then begin  wbuf  =fltarr(rays(1)-rays(0),zones*2)
			   modata=modata(rays(0)+1:rays(1),*)
			   for k=0,(zones-1) do begin wbuf(0,k)      =modata(*,2*k)
						      wbuf(0,k+zones)=modata(*,2*k+1) & endfor
			   xbuf=findgen(rays(1)-rays(0))+rays(0)+1
			   ybuf=indgen (2*zones)+1 &  tity=tity+'/sectors'
      endif else begin
      	temp=where(modata lt 0,count)
      	if (count ne 0) then modata(temp)=2+maxVal
      	for k=0,(zones-1) do begin
	    oplot,modata(*,2*k)  ,color=colors(k),max_value=maxVal
	    oplot,modata(*,2*k+1),color=colors(k),max_value=maxVal,lineStyle=2,thick=3
      	endfor
      endelse
    endif else begin
      if logID then begin wbuf=fltarr (rays(1)-rays(0),zones)
			  xbuf=findgen(rays(1)-rays(0))+rays(0)+1
			  ybuf=indgen (zones) +1
			  if zones gt 1 then tity=tity+'/sectors' & endif
      for k=0,(zones-1) do begin
       datz=modata(*,2*k:2*k+1)
       temp=size(datz)
       for i=0,temp(1)-1 do $
	  if (datz(i,0) lt 0) then $
	     if (datz(i,1) lt 0) and (not logID) then datz(i,0)=2+maxVal $
	     else datz(i,0)=datz(i,1) $
	  else if (datz(i,1) ge 0) then datz(i,0)=(datz(i,0)+datz(i,1))/2.0
       if logID then wbuf(0,k)=datz(rays(0)+1:rays(1),0) else $
       if new   then oplot,datz(*,0),color=colors(k),max_value=maxVal $
      	        else oplot,datz(*,0),color=colors(k),max_value=maxVal,lineStyle=2,thick=2
      endfor
    endelse

   endif else begin
   	 if XPS eq 'X'then coco=0 else coco=128
         temp=where(didat lt 0,count)
         if (count ne 0) and (not logID) then didat(temp)=2+maxd
         if logID then begin wbuf=didat(1:*) & xbuf=findgen(ranx(1)-ranx(0)+1)+ranx(0)
			     ybuf=0          & endif else $
   	 if new then oplot,didat,color=coco,max_value=maxd $
   		else oplot,didat,color=coco,max_value=maxd,lineStyle=2,thick=2
   endelse
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; PARAMETER MODIFICATIONS
;;; -+-+-+-+-+-+-+-+-+-+-+-

pro int_updateMini,varAccess,ev,uv
;;;;; This procedure is called to respond to an event from
;;;;; an angle (alpha,beta or gamma) widget_slider.
   widget_control,get_uvalue=set,varAccess.settings
   if (uv(3) eq 0) then set.alpha=ev.value else $
   if (uv(3) eq 1) then begin
   			set.beta =ev.value
   			widget_control,uv(4),SET_SLIDER_MAX=(180/(ev.value>1))<36
   endif else begin
      set.gamma=ev.value
      uv(4)=set.gamma
      widget_control,set_uvalue=uv,ev.id
   endelse
   widget_control,set_uvalue=set,varAccess.settings
   int_drawAxes,varAccess
end

pro int_changeZone,varAccess,ev,gamID,openID
;;;;; Called when the '# of sectors' slider sends an event.
;;;;; If the value has not changed, the procedure return; if the
;;;;; new value is 2, then the gamma value stored in the gamma slider
;;;;; is restored. Then , Called when the '# of sectors' slider sends an event.
;;;;; If the value has not changed, the procedure return; if the
;;;;; new value is 2, then the gamma value stored in the gamma slider
;;;;; is restored. Then, the mini picture is updated.
   widget_control,get_uvalue=set,varAccess.settings
   if (ev.value eq set.zones) then return
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,set_uvalue=120/max([1.0,(ev.value-1)])* $
      indgen(ev.value),varAccess.colors
   set.gamma=180.0/ev.value
   set.zones=ev.value
   if (ev.value eq 2) then begin
      widget_control,/sensitive,gamID
      widget_control,get_uvalue=temp,gamID
      set.gamma=temp(4)
   end $
   else widget_control,sensitive=0,gamID
;DID!!
   widget_control,set_slider_max=round(90./ev.value),openID

   widget_control,set_uvalue=set,varAccess.settings
   int_drawAxes,varAccess
end

pro int_changeCenter,varAccess
;;;;; This procedure reads the four widget_texts, and updates
;;;;; the corresponding variables, and the mini picture.
;;;;; It is called by the 'updateRes' procedure, and when the
;;;;; return key is pressed within a widget_text.
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=set,varAccess.settings
   set.cx=BEN_READTEXT(IDs.cx)-1.0
   widget_control,set_value=NUM2STR(set.cx+1.0,2),IDs.cx
   set.cy=BEN_READTEXT(IDs.cy)-1.0
   widget_control,set_value=NUM2STR(set.cy+1.0,2),IDs.cy
   set.mx=BEN_READTEXT(IDs.mx)
   widget_control,set_value=NUM2STR(set.mx,2),IDs.mx
   set.my=BEN_READTEXT(IDs.my)
   widget_control,set_value=NUM2STR(set.my,2),IDs.my
   widget_control,set_uvalue=set,varAccess.settings
   int_drawAxes,varAccess
end

pro int_updateRay,varAccess,ev,x
;;;;; Updates the ray variables, and redraws the mini picture.
   widget_control,get_uvalue=set,varAccess.settings
   test=0
   if x then if (set.r2 eq ev.value) then test=1 else set.r2=ev.value $
   else if (set.r1 eq ev.value) then test=1 else set.r1=ev.value
   if test then return
   widget_control,set_uvalue=set,varAccess.settings
   int_drawAxes,varAccess
end

pro int_zonePlot,varAccess,test
;;;;; Called by the two-state button 'Separate symetrical sectors'
   widget_control,get_uvalue=opt,varAccess.options
   opt.plotZone=test
   widget_control,set_uvalue=opt,varAccess.options
   int_drawAxes,varAccess
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; LAMP-DATA LOADING
;;; -+-+-+-+-+-+-+-+-

function int_loadRequest,varAccess,wkn,w,x,y,n,p,par
;;;;; This function checks the size of the parameter `w'. If it
;;;;; is not a 2-dimension array, then it returns 0. If `w' is a
;;;;; 2-dimension array, it is loaded, and the radia, and center
;;;;; widgets and variables are modified. Then the mini picture
;;;;; is modified, and an integration is calculated.
   wkSize=size(w)
   if (wkSize(0) ne 2) then return,0
   widget_control,get_uvalue=oldims,varAccess.datAccess.dims
   int_loadData,varAccess,w,x,y,n,p,par,wkSize(1:2)
   ;int_numor,varAccess
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=set,varAccess.settings
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   wset,IDs.draw
   erase
   rapport=dims.dim/oldims.dim
   temp=1+round(sqrt(2)*dims.dim)
   set.r1=min([set.r1*rapport,temp])
   set.r2=min([set.r2*rapport,temp])
   widget_control,set_slider_max=temp,IDs.r1
   widget_control,set_value=set.r1,IDs.r1
   widget_control,set_slider_max=temp,IDs.r2
   widget_control,set_value=set.r2,IDs.r2
   set.cx=set.cx*float(wkSize(1))/oldims.dimx
   set.cy=set.cy*float(wkSize(2))/oldims.dimy
   widget_control,set_value=NUM2STR(set.cx+1.0,2),IDs.cx
   widget_control,set_value=NUM2STR(set.cy+1.0,2),IDs.cy
   widget_control,set_uvalue=set,varAccess.settings
   int_miniPix,varAccess,0
;  widget_control,sensitive=0,IDs.menu(0)
   widget_control,sensitive=0,IDs.menu(1)
   widget_control,sensitive=0,IDs.menu(2)
;DID!!   int_updateRes,varAccess,-1,-1,1
   return,1
end

pro int_loadData,varAccess,w,x,y,n,p,par,dims
;;;;; This procedure just writes its parameters in their
;;;;; appropriate place.
   widget_control,set_uvalue=w,varAccess.datAccess.w
   widget_control,set_uvalue=x,varAccess.datAccess.x
   widget_control,set_uvalue=y,varAccess.datAccess.y
   widget_control,set_uvalue=n,varAccess.datAccess.n
   temp=size(p)
   if (temp(0) ne 1) or (temp(1) ne 31) then p=fltarr(31)
   widget_control,set_uvalue=p,varAccess.datAccess.p
   widget_control,set_uvalue=par,varAccess.datAccess.par
   dims=float(dims)
   temp={dimx:dims(0),dimy:dims(1),dim:max(dims)}
   widget_control,set_uvalue=temp,varAccess.datAccess.dims
end

pro int_loadWorkspace,varAccess,k,labelID
;;;;; Tests if the workspace Wk has good dimensions. If it is
;;;;; the case, the procedure `int_loadRequest' is called. If the
;;;;; workspace does not fit dimension requirement, an error
;;;;; message is displayed in the widget_label `labelID'.
   temp=BEN_WK_SIZE(k)
   if (temp(0) ne 2) then begin
      widget_control,set_value=("'W" + NUM2STR(k,0) + "' is invalid."),labelID
      print,string(7b)
      return
   end
   widget_control,set_value=("Loading 'W" + NUM2STR(k,0) + "'."),labelID
   BEN_READ_WK,k,data,datx,daty,datn,datp,datpar
   temp=int_loadRequest(varAccess,k,data,datx,daty,datn,datp,datpar)
   widget_control,set_value="Ready.",labelID
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; INITIALIZATION OF DATA AND PROGRAM VARIABLES
;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

pro int_numor,varAccess
;;;;; The display of the numor is done only in the title bar,
;;;;; because there is no room left in the window !
;;;;; It is never used, because Px does not seem to contain
;;;;; the numor number.
   widget_control,get_uvalue=datpar,varAccess.datAccess.par
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=cst,varAccess.constants
   if (datpar(0) le '') then res=cst.title $
   else res=(cst.title + '  (Numor #' +      datpar(0)    + ')')
;  else res=(cst.title + '  (Numor #' + NUM2STR(datpar,0) + ')')
   widget_control,tlb_set_title=res,IDs.menu(0)
end

pro int_initData,varAccess,request
;;;;; Fill the data variables, according to `request'. If `request' is
;;;;; set to 1, the integration window was opened because of a
;;;;; request from the desk; the function must start with the
;;;;; requested data, stored in the common `ben_data'. So, if `request'
;;;;; is set to 1, those data are loaded; if it is set to 0, default
;;;;; data are loaded, by calling `int_defaultData'.
   widget_control,get_uvalue=cst,varAccess.constants
   common ben_data,wk,w,x,y,n,p,par
   if request then begin
      wkSize=size(w)
      if (wkSize(0) eq 2) $
      then int_loadData,varAccess,w,x,y,n,p,par,wkSize(1:2) $
      else int_defaultData,varAccess
   end else int_defaultData,varAccess
;;;;; The data is now stored. The startup settings can be declared.
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   widget_control,get_uvalue=set,varAccess.settings
   set.r1=dims.dim/6.0
   set.r2=dims.dim/2.0
   set.cx=(dims.dimx-1.0)/2.0
   set.cy=(dims.dimy-1.0)/2.0
   widget_control,set_uvalue=set,varAccess.settings
end

pro int_defaultData,varAccess
;;;;; This procedure is called to store the default data, when
;;;;; the integration function is not opened for a request.
   dimx=64.0
   dimy=64.0
   midx=(dimx-1.0)/2.0
   midy=(dimx-1.0)/2.0
   data=intarr(dimx,dimy)
   data(0:midx,0:midy)=0
   data(0:midx,midy+1:dimy-1)=1
   data(midx+1:dimx-1,midy+1:dimy-1)=2
   data(midx+1:dimx-1,0:midy)=3
   int_loadData,varAccess,data,indgen(dimx),indgen(dimy), $
      0,fltarr(31),0,[dimx,dimy]
end

function int_inits,test,x
;;;;; This function returns some default value for the parameter
;;;;; settings, or constants of the program.
@lamp.cbk
   if lamp_siz lt 800 then drS=384.0 else drS=448.0
   if lamp_siz lt 800 then miS=224.0 else miS=256.0
   initz=2.0
   case test of
      0:    res={minSize:miS,drawSize:drS,deg2rad:!pi/180, $
		 title:x}
      1:    res=120/max([1.0,(initz-1)])*indgen(initz)
      2:    res={logAspect:0,plotZone:0}
      3:    res={alpha:0.0,beta:30.0,gamma:180.0/initz,zones:initz, $
		 r1:0.0,r2:0.0,cx:0.0,cy:0.0,mx:0.0,my:0.0}
      else: res=0
   end
   return,res
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
;;; WIDGET CREATION PROCEDURE
;;; -+-+-+-+-+-+-+-+-+-+-+-+-

pro ben_int_create,desk,n,m,base,request
@lamp.cbk
;;;;; Creation of the top-level base, and the top `command bar'.
   if (m eq 0) then winTitle=desk_name(n,1) $
   else winTitle=(desk_name(n,1) + " " + NUM2STR(1+m,0))
   base=widget_base(title=winTitle,/column,group_leader=desk, $
	resource_name='lampben')
   topRow=widget_base(base,/row)
   comID=BEN_COMMANDS(topRow,n,[0,0])

;;;;; Creation of the widgets containing all the `common'
;;;;; variables used by the program. The IDs of those widgets
;;;;; are stored into the top-level base user-value.
   varBase  =widget_base(title=(winTitle + " vars"),map=0,group=base)
   datAccess=widget_base(varBase,/row)
   data	 =widget_label(datAccess,value="")
   datx	 =widget_label(datAccess,value="")
   daty	 =widget_label(datAccess,value="")
   datn	 =widget_label(datAccess,value="")
   datp	 =widget_label(datAccess,value="")
   datppp=widget_label(datAccess,value="")
   dims	 =widget_label(datAccess,value="")
   dat={w:data,x:datx,y:daty,n:datn,p:datp,par:datppp,dims:dims}
   constants=int_inits(0,winTitle)
   options=int_inits(2)
   set	 =widget_label(varBase,value="",uvalue=int_inits(3))
   opt	 =widget_label(varBase,value="",uvalue=options)
   cst	 =widget_label(varBase,value="",uvalue=constants)
   IDs	 =widget_label(varBase,value="")
   col	 =widget_label(varBase,value="",uvalue=int_inits(1))
   pix	 =widget_label(varBase,value="")
   p0	 =widget_label(varBase,value="",uvalue=0)
   p1	 =widget_label(varBase,value="",uvalue=0)
   p2	 =widget_label(varBase,value="",uvalue=0)
   varAccess={datAccess:dat,settings:set,options:opt,constants:cst, $
      IDs:IDs,colors:col,pixmap:pix,plots:[p0,p1,p2]}
   widget_control,set_uvalue=varAccess,base
   int_initData,varAccess,request
   widget_control,get_uvalue=set,varAccess.settings
   widget_control,get_uvalue=temp,varAccess.datAccess.dims
   dim=temp.dim

;;;;; The main draw window, to plot the integration result.
;;;;; If the screen ysize is lower than 900 pixels, then the window
;;;;; is created horizontal; otherwise, it has a more vertical aspect.
   if (lamp_siz lt 1000) then begin
      temp=widget_base(base,/row)
      settings=widget_base(temp,/row,/frame)
      result=widget_draw(temp,retain=2, $
	 xsize=constants.drawSize*(constants.drawSize/503.37),ysize=constants.drawSize)
   end else begin
      result=widget_draw(base,  xsize=constants.drawSize*(constants.drawSize/503.37),  $
      				ysize=constants.drawSize,retain=2)
      settings=widget_base(base,/row,/frame)
   end

;;;;; Creation of the column of sliders for setting angles and rays.
   sliders=widget_base(settings,/column,xsize=constants.minSize/2.0)
	   put_logo   ,widget_base(sliders,/row)
   asl=widget_slider(sliders,value=set.alpha,title="Total rotation", $
      min=0,max=90,       uvalue=[-88,412,n,0],/drag)
   gsl=widget_slider(sliders,value=set.gamma,title="Single rotation", $
      min=0,max=90,       uvalue=[-88,412,n,2,set.gamma],/drag)
   bsl=widget_slider(sliders,value=set.beta,title="Opening", $
      min=0,max=45,       uvalue=[-88,412,n,1,0L],/drag)
   zoneID=widget_slider(sliders,value=set.zones,title="# of sectors", $
      min=1,max=8,        uvalue=[-88,407,n,gsl,bsl],/drag)
   widget_control,bsl,set_uvalue=[-88,412,n,1,zoneID]
   r1ID=widget_slider(sliders,value=set.r1,title="Inner radius", $
      min=0,max=1+round(sqrt(2)*dim),uvalue=[-88,406,n,0],/drag)
   r2ID=widget_slider(sliders,value=set.r2,title="Outer radius", $
      min=0,max=1+round(sqrt(2)*dim),uvalue=[-88,406,n,1],/drag)

;;;;; The right column contains the mini draw window, where
;;;;; zones are represented, and the fields for center and
;;;;; mask settings, plus the plots buttons.
   others=widget_base(settings,/column)
   plots=widget_base(others,/row)
   mini=widget_draw(widget_base(others),retain=2, $
        xsize=constants.minSize,ysize=constants.minSize)

;;; Toggle button to set the mini to logAspect or not
   logBtn=widget_button(widget_base(plots,/nonexclusive), $
			value="Log aspect",uvalue=[-88,408,n,0])

   update=widget_button(plots,value="I(R)",uvalue=[-88,411,n,comID(7),logBtn,1], $
          font=ft_b_normal)
   update=widget_button(plots,value="I(S)",uvalue=[-88,411,n,comID(7),logBtn,2], $
          font=ft_b_normal)
;;; Creation of the 'plot' menu.
   menuID=intarr(5,/nozero)
   menuID(0)=widget_button(plots      ,value="Plot",menu=2)
   temp     =widget_button(menuID(0)  ,value="Over last plot", $
      	     					uvalue=[-88,405,n,0,0,comID(7),logBtn,0])
   menuID(1)=widget_button(menuID(0)  ,value="Over plot #1", $
      	     					uvalue=[-88,405,n,0,1,comID(7),logBtn,0])
   menuID(2)=widget_button(menuID(0)  ,value="Over plot #2", $
      	     					uvalue=[-88,405,n,0,2,comID(7),logBtn,0])
   temp     =widget_button(menuID(0)  ,value="Into Mem 1",uvalue=[-88,405,n,1,1])
   temp     =widget_button(menuID(0)  ,value="Into Mem 2",uvalue=[-88,405,n,1,2])
   menuID(3)=widget_button(menuID(0)  ,value="Plot Mem 1",uvalue=[-88,405,n,2,1])
   menuID(4)=widget_button(menuID(0)  ,value="Plot Mem 2",uvalue=[-88,405,n,2,2])
;DID!!
   temp     =widget_button(menuID(0)  ,value=" ")
   temp     =widget_button(menuID(0)  ,value="To PS File...",uvalue=[-88,405,n,3,0,comID(7),0,0])
   temp     =widget_button(menuID(0)  ,value="To Wsp W20",uvalue=[-88,405,n,3,0,comID(7),1,0])

;;; Creation of the center and mask widget_texts.
   temp=widget_base(others,/row)
   temp_lab=widget_label(temp,value="Cx = ")
   cxID=widget_text(temp,value=NUM2STR(set.cx+1.0,2),/editable, $
      /no_newline,uvalue=[-88,409,n],xsize=6,font=ft_b_bigger)
   temp_lab=widget_label(temp,value="Cy = ")
   cyID=widget_text(temp,value=NUM2STR(set.cy+1.0,2),/editable, $
      /no_newline,uvalue=[-88,409,n],xsize=6,font=ft_b_bigger)
   temp=widget_base(others,/row)
   temp_lab=widget_label(temp,value="Mask X:")
   mxID=widget_text(temp,value=NUM2STR(set.mx,2),uvalue=[-88,409,n], $
      /editable,/no_newline,xsize=5,font=ft_b_bigger)
   temp_lab=widget_label(temp,value=" Y:")
   myID=widget_text(temp,value=NUM2STR(set.my,2),uvalue=[-88,409,n], $
      /editable,/no_newline,xsize=5,font=ft_b_bigger)
   plotBtn=widget_button(widget_base(others,/nonexclusive), $
      value="Separate Symetrical Sectors",uvalue=[-88,410,n])

;;;;; All the widgets have been declared. The two-state buttons
;;;;; are now set to their correct value, widget IDs are stored,
;;;;; some widgets are made non-sensitive (the plot menu), and
;;;;; the xmanager is called.
   bid=sys_dep      ('DYNLAB',base,1)
   widget_control,/realize,base & put_logo
   widget_control,set_button=options.plotZone,plotBtn
   widget_control,set_button=options.logAspect,logBtn
   widget_control,get_value=minID,mini
   widget_control,get_value=drawID,result
   uv={menu:menuID,cx:cxID,cy:cyID,mx:mxID,my:myID, $
      r1:r1ID,r2:r2ID,draw:drawID,mini:minID}
   widget_control,set_uvalue=uv,varAccess.IDs
   for i=1,4 do widget_control,sensitive=0,menuID(i)
   int_numor,varAccess
   int_miniPix,varAccess,0
   widget_control,set_value="<- You must load W",comID(7)
   xmanager,desk_name(n,0),base,event_handler="lamp_event_parser",/just_reg
end


;;; ***************************************************************************
;;; ***				 SPECTRA GROUPING			    ***
;;; ***************************************************************************

pro rgp_event,ev,uv
   widget_control,get_uvalue=varAccess,ev.top
   case uv(1) of
      400:  begin wait,.05 & widget_control,/destroy,ev.top & end
      401:  BEN_CHANGE_WORKSPACE,uv
      402:  rgp_help,varAccess,ev.top
      403:  rgp_loadWorkspace,varAccess,uv(3),uv(4)
      404:  rgp_writeWorkspace,varAccess,uv(3)
      405:  rgp_mask_event,varAccess,ev
      406:  rgp_toggleBtn,varAccess,ev.select,uv(3)
      407:  rgp_changeMax,varAccess,ev.value,ev.drag,uv(3),uv(4)
      408:  begin
      		rgp_writeMask,varAccess,uv(3)
      		widget_control,bad_id=i,ev.id,get_value=labut
      		labut=strlowcase(labut)
      		widget_control,bad_id=i,ev.id,set_value=labut
      	    end
      409:  begin
      		rgp_loadMask,varAccess,uv(3)
      		widget_control,bad_id=i,ev.id,get_value=labut
      		labut=strlowcase(labut)
      		widget_control,bad_id=i,ev.id,set_value=labut
      	    end
      410:  rgp_groupNum,varAccess,ev.value,1
      411:  rgp_writeWorkspace,varAccess,-1
      412:  rgp_toggleExport,varAccess,ev.select,uv(3),uv(4)
      else: print,"RGP_EVENT:",uv
   end
end

function rgp_testModule,x
;;;;; This function returns the value of 'singleRun' for the grouping
;;;;; function. Also it could be run several times, it is limited to
;;;;; only one value, because several ones should be confusing.
   return,1
end

pro rgp_help,varAccess,base
   print,"HELP WANTED"
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

function mask,x,apMask
@lamp.cbk
;;;;; This function is to be used as a macro; its syntax is:
;;;;;		  Wi=mask(Wj,m), or Wi=mas(j,m)
;;;;; where `m' is the number of a mask (from 1 to 4)
;;;;; The masks are stored into the 'rgp_lampmask' common.
   common rgp_lampmask,mask1,mask2,mask3,mask4
   temp=execute("destwk=w" + NUM2STR(one,0))
   temp=size(x)
;;;;; If x is not a number, try the common variable `two'
;;;;; `two' contains a non-zero number when user calls `wi = mask(wj,m)'
   if ((temp(0) ne 0) or (temp(1) gt 4)) then wk=two $
   else wk=x
   if ((wk le 0) or (wk gt 20)) then return,destwk
   temp=BEN_WK_SIZE(wk)
   if (temp(0) ne 2) then return,destwk
   dimx=temp(1)
   dimy=temp(2)
   BEN_READ_WK,wk,work,datx,daty,datn,datp,datpar
   temp=execute("apMask=mask" + NUM2STR(apMask,0))
   temp=size(apMask)
   if (temp(0) ne 1) then return,destwk
   maskSize=temp(1)
   if (dimy ne maskSize) then $
      if (dimy gt maskSize) then begin
	 expMask=intarr(dimy)
	 expMask(0:maskSize-1)=apMask
	 apMask=expMask
      end
   rgp_applyMask,work,daty,apMask,ind,res,resy
   if (ind lt 0) then return,destwk
   res=res(*,0:ind)
;  datp(30)=ind+1
   BEN_WRITE_WK,one,res,datx,resy(0:ind),datn,datp,datpar
   return,res
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro rgp_loadMask,varAccess,x
;;;;; This porcedure is called by one of the 'load mask' button.
;;;;; It reads one of the four mask stored in the common, and
;;;;; uses it for the current mask. It calls the imask procedure
;;;;; that updates the display and the grouping. Then the infos
;;;;; about the spectra are updated too.
   common rgp_lampmask,mask1,mask2,mask3,mask4
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   temp=execute("newMask=mask" + NUM2STR(x,0))
   temp=size(newMask)
   if (temp(0) ne 1) then return
   last=-1
   cur=intarr(dims.x,dims.y)
   cur(*,*)=1
   for i=0,min([dims.y,temp(1)])-1 do cur(*,i)=newMask(i)
   widget_control,set_uvalue={cur:cur,last:-1},varAccess.mask
   rgp_imask,varAccess,[-1],0,0
   rgp_info,varAccess,1
end

pro rgp_writeMask,varAccess,x
;;;;; This procedure writes the current mask in the 'Mx' variable
;;;;; of the rgp_lampmask common.
   common rgp_lampmask,mask1,mask2,mask3,mask4
   widget_control,get_uvalue=mask,varAccess.mask
   temp=execute("mask" + NUM2STR(x,0) +"=reform(mask.cur(0,*))")
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro rgp_numor,varAccess
;;;;; This procedure is called at startup, and when loading a new
;;;;; workspace, to display the numor in the window title and on
;;;;; a label.
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=datpar,varAccess.datAccess.par
   widget_control,get_uvalue=IDs,varAccess.IDs
   if (datpar(0) eq '') then temp=' ' $
   else temp=datpar (0)

   widget_control,set_value=temp,IDs.numor
   if (temp eq ' ') then temp='' $
   else temp=('  (Numor:' + temp + ')')
   widget_control,tlb_set_title=(cst.title + temp),IDs.numor
end

pro rgp_loadData,varAccess,wkn,w,x,y,n,p,par,dims
;;;;; This procedure just writes its parameters in their
;;;;; appropriate place.
   widget_control,set_uvalue=w  ,varAccess.datAccess.w
   widget_control,set_uvalue=x  ,varAccess.datAccess.x
   widget_control,set_uvalue=y  ,varAccess.datAccess.y
   widget_control,set_uvalue=n  ,varAccess.datAccess.n
   temp=size(p)
   if (temp(0) ne 1) or (temp(1) ne 31) then p=fltarr(31)
   widget_control,set_uvalue=p,varAccess.datAccess.p
   widget_control,set_uvalue=par,varAccess.datAccess.par
   temp={x:dims(0),y:dims(1)}
   widget_control,set_uvalue=temp,varAccess.datAccess.dims
   varAccess.datAccess.wkn=wkn
end


function rgp_loadRequest,varAccess,wk,w,x,y,n,p,par
;;;;; This function is called by the desk, or by the 'loadWorkspace'
;;;;; procedure, to make its parameters the current data. First,
;;;;; the procedure checks the data is a 2-dimensional array, and
;;;;; then the other parameters are also checked, and replaced or
;;;;; modified if needed. All variables are updated if needed, and so
;;;;; is the display and the grouping.
;;;;; The value returned by this function is 1 if the data were
;;;;; correctly loaded, 0 if there was a problem, such as trying to
;;;;; load a 'bad sized' workspace.
   wksize=size(w)
   if (wksize(0) ne 2) then return,0
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=data,varAccess.datAccess.w
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   dims.x=wksize(1)
   dims.y=wksize(2)
   cur=intarr(dims.x,dims.y)
   cur(*,*)=1
   oldMax=max(data)
   temp=size(x)
   if ((temp(0) ne 1) or (temp(1) ne dims.x)) then x=indgen(dims.x)
   temp=size(y)
   if ((temp(0) ne 1) or (temp(1) ne dims.y)) then y=indgen(dims.y)
   temp=size(p)
   if ((temp(0) ne 1) or (temp(1) ne 31)) then begin
      p=fltarr(31)
      p(30)=dims.y
   end
;;;;; All the parameters have been checked. The data are stored,
;;;;; and some variables are modified.
   rgp_loadData,varAccess,wk,w,x,y,n,p,par,wkSize(1:2)
   temp=max(w)
   widget_control,get_uvalue=uv,IDs.maxSlider
   uv(3)=temp
   widget_control,set_uvalue=uv,IDs.maxSlider
   rgp_changeMax,varAccess,-1,1,temp,uv(4)
   widget_control,set_slider_max=max([2,dims.y/2]),IDs.gpSlider
   widget_control,sensitive=(dims.y ge 2),get_value=ng,IDs.gpSlider
   widget_control,set_uvalue={cur:cur,last:-1},varAccess.mask
;;;;; The numor, the infos, grouping and display are updated.
   rgp_numor,varAccess
   rgp_info,varAccess,0
   rgp_groupNum,varAccess,ng,0
   rgp_imask,varAccess,[-1],1,1
   return,1
end

pro rgp_loadWorkspace,varAccess,wk,labelID
;;;;; This procedure simply calls the loadRequest function, but it
;;;;; displays messages in its parent window if the loading failed.
   wksize=BEN_WK_SIZE(wk)
   if (wksize(0) ne 2) then begin
      widget_control,set_value=("'W" + NUM2STR(wk,0) + "' is invalid."),labelID
      print,string(7b)
      return
   end
   widget_control,set_value=("Loading 'W" + NUM2STR(wk,0) + "'."),labelID
   BEN_READ_WK,wk,w,x,y,n,p,par
   temp=rgp_loadRequest(varAccess,wk,w,x,y,n,p,par)
   widget_control,set_value=' ',labelID
end

pro rgp_writeWorkspace,varAccess,x
;;;;; This procedure first transforms the current data by applying
;;;;; the mask and grouping the spectra, and then output the result
;;;;; either to the LAMP workspace Wx if x is greater than 0, or to
;;;;; the 'Scroll Spectra' function.
   widget_control,get_uvalue=data,varAccess.datAccess.w
   widget_control,get_uvalue=datx,varAccess.datAccess.x
   widget_control,get_uvalue=daty,varAccess.datAccess.y
   widget_control,get_uvalue=datn,varAccess.datAccess.n
   widget_control,get_uvalue=datp,varAccess.datAccess.p
   widget_control,get_uvalue=datpar,varAccess.datAccess.par
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=grp,varAccess.groups
   widget_control,get_uvalue=mask,varAccess.mask
   if opt.mApply then $
   ;;;;; If the user wants to apply the mask, ...
      if opt.doGroup then begin
      ;;;; ... and also group the spectra.
	 work=data*mask.cur
	 temp=size(grp.index)
	 ind=temp(1)-1
	 res=fltarr(dims.x,ind)
	 for i=0,ind-1 do begin
	    temp=where(mask.cur(0,grp.index(i):grp.index(i+1)-1) ne 0,count)
	    if (count eq 1) then res(*,i)=work(*,grp.index(i)) $
	    else if (count gt 0) then $
	       res(*,i)=total(work(*,grp.index(i):grp.index(i+1)-1),2)/count
	 end
	 resy=grp.sinAver
	 newp=datp
;	 newp(30)=ind-1
      end else begin
      ;;; ... but not do the grouping, applyMask is called.
	 rgp_applyMask,data,daty,reform(mask.cur(0,*)),ind,res,resy
	 newp=datp
;	 datp(30)=ind+1
      end $
   else begin
      res=data
      resy=daty
      newp=datp
   end
   if (x gt 0) then begin
   ;;;;; If the output is a workspace, the workspace and its linked
   ;;;;; variables are stored, and the history is updated.
;DID!!
   if n_elements(datpar) ge 2 then from='w'+ strtrim(datpar(1),2) $
   			      else from='w'+ NUM2STR(varAccess.datAccess.wkn,0)
   too='w'+ NUM2STR(x,0)
   xicute,too+'=groupp('+from+')'

   BEN_WRITE_WK,x,res,datx,resy,datn,newp,datpar
   TO_DON_HISTORY,x,0,too + '=mask(' + from + ')'

   end else begin
   ;;;;; The procedure request a 'df' (ie Scroll Spectra) from the
   ;;;;; desk. If the 'Scroll Spectra' procedures are present, a new
   ;;;;; window will be opened with the parameters if possible, or
   ;;;;; the data will be loaded by an already opened window.
      temp=desk_loadRequest('df',varAccess.datAccess.wkn,res,datx,resy, $
	 datn,newp,datpar)
      if (temp eq 0) then print,"Failed."
   end
end

function groupp, w_in
;******* ******
;DID!!
	w_out=bytarr(2,2)
return, w_out
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro rgp_mask_event,varAccess,ev
;;;;; This is the procedure called by the widget_draw when the mouse is
;;;;; used inside it. It calls the imask procedure to modify the
;;;;; current mask, and to update everything.
   if (ev.type ne 0) then return
   widget_control,get_uvalue=dim,varAccess.datAccess.dims
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=mask,varAccess.mask
   spec=min([dim.y*(cst.dimy-cst.borm(3)-ev.y)/ $
      (cst.dimy-cst.borm(1)-cst.borm(3)),dim.y-1])
   if (ev.x gt (cst.dimx-cst.borm(2))) then return
   if ((mask.last lt 0) or (ev.press eq 1)) $
   then rgp_imask,varAccess,[spec],0,0 $
   else rgp_imask,varAccess,[spec,mask.last],0,0
   widget_control,get_uvalue=mask,varAccess.mask
   mask.last=spec
   widget_control,set_uvalue=mask,varAccess.mask
   rgp_info,varAccess,1
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro rgp_info,varAccess,test
;;;;; Called to update the two info labels. The first one contains info
;;;;; about the masked spectra. The second label is updated only if
;;;;; `test' is set to one, and contains info about the total number
;;;;; of spectra in the current data.
   widget_control,get_uvalue=msk,varAccess.mask
   widget_control,get_uvalue=IDs,varAccess.IDs
   temp=where(msk.cur(0,*) eq 0,count)
   case count of
      0:    temp='None masked.'
      1:    temp='One  masked.'
      else: temp=(NUM2STR(count,0) + ' masked.')
   end
   widget_control,set_value=temp,IDs.info(1)
   if test then return
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   temp=('Dim='+NUM2STR(dims.y,0))
   widget_control,set_value=temp,IDs.info(0)
end

pro rgp_changeMax,varAccess,x,drag,dataMax,labelID
;;;;; This procedure is called by the 'maximum value' slider, when it's
;;;;; moved. A value is displayed in a label beside the slider, to
;;;;; simulate a non-linear slider. The display is updated only when
;;;;; the slider is released (`drag' is set to 0).
   widget_control,get_uvalue=set,varAccess.settings
   if (x lt 0)  then begin x=set.maxVal & y=dataMax
   endif	else	   y=dataMax*(float(x)/100.0)^2
   widget_control,set_value=NUM2STR(y,0),labelID
   if drag then return
   set.maxVal=y
   widget_control,set_uvalue=set,varAccess.settings
   rgp_imask,varAccess,[-1],0,1
end

pro rgp_toggleBtn,varAccess,x,test
;;;;; This procedure handles the two toggle buttons. It modifies the
;;;;; correct variable/uservalue.
   widget_control,get_uvalue=opt,varAccess.options
   if test then opt.hideMask=x $
   else opt.logAspect=x
   widget_control,set_uvalue=opt,varAccess.options
   rgp_imask,varAccess,[-1],0,1
end

pro rgp_toggleExport,varAccess,x,test,btnID
;;;;; This procedure modifies the variables concerning the output.
   widget_control,get_uvalue=opt,varAccess.options
   if test then opt.mApply=x $
   else opt.doGroup=x
;DID!
;  if test then widget_control,sensitive=x,btnID
   widget_control,set_uvalue=opt,varAccess.options
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro rgp_imask,varAccess,n,eras,redrw
;;;;; This procedure modifies the mask: it masks/unmasks the spectra
;;;;; n(0) to n(1), depending if n(0) is already maked or not. It then
;;;;; replots the column showing the mask, updates the data display
;;;;; if the opt.hideMask is set to 1. The new grouping is also
;;;;; calculated, and the new groups displayed on the right.
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=set,varAccess.settings
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=data,varAccess.datAccess.w
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=mask,varAccess.mask
   if ((n(0) ge 0) and (n(0) lt dims.y)) then begin
      if mask.cur(0,n(0)) then temp=0 else temp=1
      for i=min(n),max(n) do mask.cur(*,i)=temp
   end
   widget_control,set_uvalue=mask,varAccess.mask
   rgp_groupNum,varAccess,-1,0
   wset,IDs.mask
   if eras then erase
   if ((opt.hideMask eq 0) or redrw) then begin
      if opt.hideMask then temp=(data<set.maxVal) $
      else temp=(data<set.maxVal)*mask.cur
      if opt.logAspect then temp=alog(1+temp)
      tempx=cst.dimx-cst.borm(0)-cst.borm(2)
      tempy=cst.dimy-cst.borm(1)-cst.borm(3)
      temp=congrid(temp,tempx,tempy)

      tvscl,temp,cst.borm(0),cst.borm(1),order=opt.tvOrder
   end $
   else tempy=cst.dimy-cst.borm(1)-cst.borm(3)
   sSel=cst.borm(0)/3
   temp=110-39*mask.cur(0,*)
   tv,congrid(temp,cst.borm(0)-sSel,tempy),sSel,cst.borm(1),order=opt.tvOrder
   if (n(0) ge 0) then temp(n(0))=-1 $
   else if (mask.last ge 0) then temp(mask.last)=-1
   tv,congrid(temp,sSel,tempy),0,cst.borm(1),order=opt.tvOrder
   rgp_drawGroups,varAccess
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro rgp_applyMask,win,yin,cur,ind,res,resy
;;;;; This procedure applies the mask `cur' to the `Win' spectra, and
;;;;; also modifies the angles contained in `Yin'. The results are
;;;;; returned into `res' and `resy'.
   wkSize=size(win)
   ind=-1
   res=fltarr(wkSize(1),wkSize(2),/nozero)
   resy=fltarr(wkSize(2),/nozero)
   for i=0,wkSize(2)-1 do $
      if cur(i) then begin
	 ind=ind+1
	 res(*,ind)=win(*,i)
	 resy(ind)=yin(i)
      end
   if (ind lt 0) then return
   res=res(*,0:ind)
   resy=resy(0:ind)
end

pro rgp_groupNum,varAccess,n,disp
;;;;; This procedure calculates the groupings.
;;;;; The range of unmasked angles is searched, and then the list of
;;;;; the sinus of the half-angles is divided into `n' parts, which
;;;;; are of the same width. If a part is empty, ie it does not include
;;;;; any spectrum, it is removed. In a part, the `average' angle is
;;;;; set to the mean value of its limits, and not to the mean of
;;;;; the angles corresponding to the spectra. The 'grouped' spectrum
;;;;; is the average of all the included spectra.
   widget_control,get_uvalue=set,varAccess.settings
   if (n lt 0) then n=set.ngroups $
   else if (set.ngroups eq n) then return $
   else set.ngroups=n
   widget_control,get_uvalue=mask,varAccess.mask
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=data,varAccess.datAccess.w
   widget_control,get_uvalue=daty,varAccess.datAccess.y
   msk=reform(mask.cur(0,*))
   temp=where(msk ne 0,usedCount)
   if (usedCount le 0) then return
   widget_control,set_uvalue=set,varAccess.settings
   sinList=1000*sin(!pi*msk*daty/360.0)
   temp=where((daty eq 0) and (msk ne 0),count)
   if (count gt 0) then begin
      minSub=min(temp)
      infVal=0
   end else begin
      minSub=min(where(sinList gt 0))
      infVal=sinList(minSub)
   end
   aver=(max(sinList,maxSub)-infVal)/n
   index=intarr(n+1,/nozero)
   sinAver=(360.0/!pi)*asin(aver/1000*(findgen(n)+0.5))
   index(0)=minSub
   for i=1,n-1 do begin
      infVal=infVal + aver
      temp=where(sinList gt infVal,count)
      if (count ne 0) then index(i)=min(temp) $
      else index(i)=index(i-1)
   end
   index(n)=maxSub+1
   ind=0
   ;;;;; The empty parts are now removed.
   for i=1,n do $
      if (index(i) ne index(ind)) then begin
	 ind=ind+1
	 index(ind)=index(i)
	 sinAver(ind-1)=sinAver(i-1)
      end
   if (ind eq 0) then return
   index=index(0:ind)
   sinAver=sinAver(0:ind-1) + min(daty) ;DID!
   ;;;;; The info label is updated.
   if (ind eq 1) then res='One group.' $
   else res=(NUM2STR(ind,0) + ' groups.')
   temp=n-ind
   if (temp ne 0) then begin
      if (temp eq 1) then temp='(-1)' $
      else temp=('(-'+NUM2STR(temp,0) + ')')
      res=(res + temp)
   end
   widget_control,set_value=res,IDs.info(2)
   widget_control,set_uvalue={index:index,sinAver:sinAver},varAccess.groups
   ;;;;; The display is updated if `disp' is set to 1.
   if disp then rgp_drawGroups,varAccess
end

pro rgp_drawGroups,varAccess
;;;;; This procedure just draws the left-most ribbon in the display
;;;;; area, to represent the current groups.
   widget_control,get_uvalue=cst,varAccess.constants
   widget_control,get_uvalue=set,varAccess.settings
   widget_control,get_uvalue=grp,varAccess.groups
   widget_control,get_uvalue=msk,varAccess.mask
   widget_control,get_uvalue=IDs,varAccess.IDs
   widget_control,get_uvalue=opt,varAccess.options
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   tempy=cst.dimy-cst.borm(1)-cst.borm(3)
   if (set.ngroups gt 0) then begin
      res=110+intarr(1,dims.y)
      temp=size(grp.index)
      for i=1,temp(1)-1 do begin
	 col=35+ 36*(-1)^i
	 for j=grp.index(i-1),grp.index(i)-1 do $
	    if msk.cur(0,j) then res(0,j)=col
      end
   end else begin
      res=intarr(cst.borm(2),cst.borm(2))
      for i=0,cst.borm(2)-1 do begin
	 res(i,i)=1
	 res(i,cst.borm(2)-i-1)=1
      end
   end
   wset,IDs.mask
   tv,congrid(res,cst.borm(2),tempy),cst.dimx-cst.borm(2), $
      cst.borm(1),order=opt.tvOrder
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro rgp_initData,varAccess,request
;;;;; Fill the data variables, according to `request'. If `request' is
;;;;; set to 1, the function window was opened because of a
;;;;; request from the desk; the function must start with the
;;;;; requested data, stored in the common `ben_data'. So, if `request'
;;;;; is set to 1, those data are loaded; if it is set to 0, default
;;;;; data are loaded, by calling `rgp_defaultData'.
   widget_control,get_uvalue=cst,varAccess.constants
   common ben_data,wk,w,x,y,n,p,par
   if request then begin
      wkSize=size(w)
      if (wkSize(0) eq 2) $
      then rgp_loadData,varAccess,wk,w,x,y,n,p,par,wkSize(1:2) $
      else rgp_defaultData,varAccess
   end else rgp_defaultData,varAccess
;;;;; The data is now stored. The startup settings can be declared.
   widget_control,get_uvalue=dims,varAccess.datAccess.dims
   widget_control,get_uvalue=data,varAccess.datAccess.w
   set={maxVal:100L,ngroups:max([2,dims.y/2])}
   mask={cur:(intarr(dims.x,dims.y)+1),last:-1}
   widget_control,set_uvalue=set,varAccess.settings
   widget_control,set_uvalue=mask,varAccess.mask
end

pro rgp_defaultData,varAccess
;;;;; This procedure is called to store the default data, when
;;;;; this function is not opened for a request.
   dimx=64
   dimy=64
   data=dist(dimx,dimy)
   rgp_loadData,varAccess,0,data,indgen(dimx),indgen(dimy), $
      0,fltarr(31),0,[dimx,dimy]
end

function rgp_inits,test,x
;;;;; This function is used for variable initialization.
@lamp.cbk
if lamp_siz lt  800 then xxx=384. else xxx=512.
if lamp_siz lt 1000 then yyy=320. else yyy=384.
if lamp_siz lt  800 then yyy=256. else yyy=384.
   case test of
      0:    res={dimx:xxx   ,dimy:yyy*3 ,borm:[24,0,24,0],title:x}
      1:    res={logAspect:1,hideMask:0,tvOrder:1,mApply:1,doGroup:0}
      else: res=0
   end
   return,res
end

;;; -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

pro ben_rgp_create,desk,n,m,base,request
@lamp.cbk
;;;;; This the procedure called by the LAMP Desktop to create a new
;;;;; window. `n' is the item 2 of the widgets user value. Is is used
;;;;; by P_BEN_EVENT to select which function the incoming events must
;;;;; be sent too. `m' is used only for the title. `base' is an output
;;;;; variable, used to return the new window ID number. If the window
;;;;; is created to respond to another function request, then `request'
;;;;; is set to 1, so the function will inits its data using the
;;;;; variables stored in the 'ben_data' common.
   if (m eq 0) then winTitle=desk_name(n,1) $
   else winTitle=(desk_name(n,1) + ' ' + NUM2STR(1+m,0))
   base=widget_base(title=winTitle,/column,group_leader=desk, $
		    resource_name="lampben")
   topRow=widget_base(base,/row)
   comID=BEN_COMMANDS(topRow,n,[0,0])
   widget_control,set_value="<-- You must load W",comID(7)
;;;;; Creation of the widget_label containing all the variables.
   datAccess=widget_base(topRow,/row)
   data	 =widget_label(datAccess,value="")
   datx	 =widget_label(datAccess,value="")
   daty	 =widget_label(datAccess,value="")
   datn	 =widget_label(datAccess,value="")
   datp	 =widget_label(datAccess,value="")
   datppp=widget_label(datAccess,value="")
   dims	 =widget_label(datAccess,value="")
   dat={w:data,x:datx,y:daty,n:datn,p:datp,par:datppp,dims:dims,wkn:0}
   constants=rgp_inits(0,winTitle)
   options=rgp_inits(1)
   set	 =widget_label(topRow,value="")
   opt	 =widget_label(topRow,value="",uvalue=options)
   cst	 =widget_label(topRow,value="",uvalue=constants)
   IDs	 =widget_label(topRow,value="")
   mask	 =widget_label(topRow,value="")
   grp	 =widget_label(topRow,value="")
   varAccess={datAccess:dat,settings:set,options:opt,constants:cst, $
      IDs:IDs,mask:mask,groups:grp}
   widget_control,set_uvalue=varAccess,base
;;;;; Initialization of the data, by calling 'initData'
   rgp_initData,varAccess,request
   widget_control,get_uvalue=set,varAccess.settings
   widget_control,get_uvalue=data,varAccess.datAccess.w
   widget_control,get_uvalue=dims,varAccess.datAccess.dims

;;;;; Creation of the output (to a workspace or the 'Scroll Spectra'
;;;;; function) widgets, and the masks input/output.
   temp=widget_base(base,/row)
   left=widget_base(temp,/column,space=8)
   right=widget_base(widget_base(temp,/column),/row)
   output=widget_base(left,/column,/frame)

   temp=widget_base    (base,/row)
   numorID=widget_label(temp,value='__________'		,font=ft_b_bigger)
   info   =intarr(3,/nozero)
   info(0)=widget_label(temp,value='_________'		,font=ft_propor)
   info(1)=widget_label(temp,value='_______________'	,font=ft_propor)
   info(2)=widget_label(temp,value='_______________'	,font=ft_propor)

;DID!
;  xopt=widget_base (output,/nonexclusive,/row,space=32)
;  applyBtn=widget_button(xopt,value="Apply mask")
   ropt=widget_base (output,/row)
   bido=widget_label(ropt  ,value='OUTPUT',font=ft_b_normal)
   xopt=widget_base (ropt  ,/nonexclusive)
   groupBtn=widget_button(xopt,value="Group spectra",uvalue=[-88,412,n,0,0])
;  widget_control,set_uvalue=[-88,412,n,1,groupBtn],applyBtn

   out=widget_base(output,/row)
   temp=widget_label(temp,value='')
   out=widget_base(out,/column)
   output=BEN_SELECTWID(widget_base(out,/row),n,1,0,0,0)
   useDF=widget_button(out,uvalue=[-88,411,n], $
      value=("Export to '" + desk_name(desk_winNum('df'),1) + "'"))
;   numor=widget_label(left,value=' ')
   right1=widget_base(right,/column)
   temp=widget_label(right1,value='Load mask')
   for i=1,3 do $
      temp=widget_button(right1,value=("M" + NUM2STR(i,0)), $
      uvalue=[-88,409,n,i])
   right2=widget_base(right,/column)
   temp=widget_label(right2,value='Save mask in')
   for i=1,3 do $
      temp=widget_button(right2,value=("M" + NUM2STR(i,0)), $
      uvalue=[-88,408,n,i])

;;;;; Creation of the spectra display, and the options widgets.
   visual=widget_base(base,/column,/frame)
;DID!
   imask=widget_draw(widget_base(visual),/button_events,retain=2  , $
      xsize=constants.dimx,ysize=constants.dimy,uvalue=[-88,405,n], $
      x_scroll_size=constants.dimx,y_scroll_size=constants.dimy/3)
   opts=widget_base(visual,/row)
	put_logo   ,opts
   togs=widget_base(opts,/row,/nonexclusive)
   hidMask=widget_button(togs,value="Hide mask",uvalue=[-88,406,n,1])
   logBtn=widget_button(togs,value="Log aspect",uvalue=[-88,406,n,0])
   maxDat=max(data)
   maxSlider=widget_slider(opts,title="Maximum value",/suppress_value, $
      /drag,value=set.maxVal)
   slidLab=widget_label(opts,value=NUM2STR(maxDat,0))
   widget_control,set_uvalue=[-88,407,n,maxDat,slidLab],maxSlider

;;;;; Creation of a slider widget to select the desired number of
;;;;; groups; below this slider, three info widget_label are created.
   gpSlider=widget_slider(base,value=set.ngroups,uvalue=[-88,410,n], $
      min=1,max=set.ngroups,title="Number of groups",/drag)

;;;;; All the widgets are declared. They are now realized, and the
;;;;; widgets IDs are stored. The toggle buttons states are set to
;;;;; their default value, according to the "rgp_inits' function
;;;;; result. The data is displayed, and the groups calculated.
   bid=sys_dep      ('DYNLAB',base,1)
   widget_control,/realize,base & put_logo
   widget_control,get_value=maskID,imask
   if options.logAspect then widget_control,/set_button,logBtn
   if options.hideMask then widget_control,/set_button,hidMask
   if options.doGroup then widget_control,/set_button,groupBtn
;DID!
;  if options.mApply then widget_control,/set_button,applyBtn $
;  else widget_control,sensitive=0,groupBtn
   widget_control,set_uvalue={mask:maskID,maxSlider:maxSlider, $
      gpSlider:gpSlider,numor:numorID,info:info},varAccess.IDs
   rgp_numor,varAccess
   rgp_info,varAccess,0
   rgp_changeMax,varAccess,-1,1,maxDat,slidLab
   rgp_groupNum,varAccess,set.ngroups,0
   rgp_imask,varAccess,[-1],1,1
   xmanager,desk_name(n,0),base,event_handler="lamp_event_parser",/just_reg
end

pro bens
;** ****
return
end
