pro Show_Version, wid, version, bil
;** ************
;**
@lamp.cbk
	version='22 Aug 2002 for idl 3.6.1 -> 5.5'

	if wid gt 0 then begin
	 bil=widget_base (wid,/row)
	 bid	=widget_label (bil  ,value='Version '+version            ,font=ft_smaller)
	 bid	=widget_label (bil  ,value='.',resource_name='spelab1'   ,font=ft_smaller)
	 bid	=widget_label (bil  ,value='.',resource_name='spelab2'   ,font=ft_smaller)
	 bid	=widget_label (bil  ,value='.',resource_name='spelab3'   ,font=ft_smaller)
	 bid	=widget_label (bil  ,value='.',resource_name='spelab4'   ,font=ft_smaller)
	 bid	=widget_label (bil  ,value='.',resource_name='spelab5'   ,font=ft_smaller)
	 bid	=widget_label (bil  ,value='.',resource_name='spelab6'   ,font=ft_smaller)
	 bid	=widget_label (bil  ,value='.',resource_name='spelab7'   ,font=ft_smaller)
	 bid	=widget_label (bil  ,value='.',resource_name='spelab8'   ,font=ft_smaller)
	 bid	=widget_label (bil  ,value='.',resource_name='spelab9'   ,font=ft_smaller)
	endif
end

;*			******************
;*			**              **
			  PRO LAMP_,just
;*			**              **
;*			******************


;**		LARGE ARRAY MANIPULATION PROGRAM
;**		----- ----- ------------ -------

;**	This module manages the LAMP application.
;**	It is written at ILL Grenoble (France ill.fr) by M. Ferrand  for data integrity
;**							 G. Kearley  for users  integrity
;**							 D. Richard  for project  integrity
;**							 B. Vettier  for excellent functions
;**							 R. Jouffrey for wonderfull superplot

;**	The LAMP package is distributed as "Shareware". If you find this application
;**	useful, you may register your copy simply by sending an electronic mail
;**	message to lamp@ill.fr. We would gratefully appreciate any feedback on the
;**	LAMP application.

;** Is LAMP already managed ...
;** -- ---- ------- -------
@lamp.cbk
common for_users,	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

  if n_elements(just)    le 0 then just=''
  if (!D.name eq 'TEK') and (just ne 'just')              then LAMP_B
  if n_elements(lamp_focus) eq 1 then if lamp_focus eq -1 then LAMP_B

  if n_elements(lamp_b1) gt 0 then ii=xregistered('LAMP') else ii=0
  if ii eq 0 then                  ii=xregistered('TOUCH')
  if ii eq 0 then                  ii=xregistered('TRIPX')
  if ii gt 0 then if just ne 'just' then ii=widget_info(lamp_b1,/realize)
  if ii gt 0 then begin if sys_dep('VERSION') le 5.0 then XMANAGER
  endif else begin

;***************************************
	if !D.name ne 'TEK' then ii=sys_dep('PSEUDO')
;***************************************
	!quiet=1
	p_screen
        if n_elements(lamp_dir) eq 0 then lamp_dir=sys_dep('GETENV','LAMP_DIR')

;** Workspaces
;** ----------
;** Wi	    = input data and manipulated data
;** wtb(i)  = 0 each time the contents of Wi is modified and set to 1 on display
;** wintb(i)= used by Scan

    w1 =0 & w2 =0 & w3 =0 & w4 =0 & w5 =0 & w6 =0 & w7 =0 & w8 =0 & w9 =0 & w10=0
    w11=0 & w12=0 & w13=0 & w14=0 & w15=0 & w16=0 & w17=0 & w18=0 & w19=0 & w20=0
    w21=0 & w22=0 & w23=0
    w_in =0 & w_out=0 & w_buf=0

    wn   =23+1

    wtb  =bytarr(wn) & wtb  (*)= 0
    wintb=intarr(wn) & wintb(*)=-1

    w_min  =fltarr(wn)
    w_max  =fltarr(wn)
    w_numor=strarr(wn) & w_numor(0)='lamp.ps'

;** Titles
;** ------
    x_tit    =strarr(wn)
    y_tit    =strarr(wn)
    z_tit    =strarr(wn)
    w_tit    =strarr(wn)
    other_tit=strarr(wn)
    head_tit =strarr(wn,10)

;** Pi	   = set of scalars used in interactive commands (data parameters)
;** --
    p0 =0 & p1 =0 & p2 =0 & p3 =0 & p4 =0 & p5 =0 & p6 =0 & p7 =0 & p8 =0 & p9 =0 & p10=0
    p11=0 & p12=0 & p13=0 & p14=0 & p15=0 & p16=0 & p17=0 & p18=0 & p19=0 & p20=0
    p21=0 & p22=0 & p23=0
    p_in =0 & p_out=0 & p_buf=0

    pv0 =0 & pv1 =0 & pv2 =0 & pv3 =0 & pv4 =0 & pv5 =0 & pv6 =0 & pv7 =0 & pv8 =0 & pv9 =0 & pv10=0
    pv11=0 & pv12=0 & pv13=0 & pv14=0 & pv15=0 & pv16=0 & pv17=0 & pv18=0 & pv19=0 & pv20=0
    pv21=0 & pv22=0 & pv23=0
    pv_in =0 & pv_out=0 & pv_buf=0

    if  n_elements(npars) eq 0 then npars=40
    par_txt=strarr(wn ,npars)
    par_txt_all=strarr(npars)

;** Motif
;** -----

    p_set_font,0

;   Stuff for data transformations
;** ----- --- ---- ---------------
    x0 =0 & x1 =0 & x2 =0 & x3 =0 & x4 =0 & x5 =0 & x6 =0 & x7 =0 & x8 =0 & x9 =0
    x10=0 & x11=0 & x12=0 & x13=0 & x14=0 & x15=0 & x16=0 & x17=0 & x18=0
    x19=0 & x20=0 & x21=0 & x22=0 & x23=0
    x_in =0 & x_out=0 & x_buf=0

    y0 =0 & y1 =0 & y2 =0 & y3 =0 & y4 =0 & y5 =0 & y6 =0 & y7 =0 & y8 =0 & y9 =0
    y10=0 & y11=0 & y12=0 & y13=0 & y14=0 & y15=0 & y16=0 & y17=0 & y18=0
    y19=0 & y20=0 & y21=0 & y22=0 & y23=0
    y_in =0 & y_out=0 & y_buf=0

    z0 =0 & z1 =0 & z2 =0 & z3 =0 & z4 =0 & z5 =0 & z6 =0 & z7 =0 & z8 =0 & z9 =0
    z10=0 & z11=0 & z12=0 & z13=0 & z14=0 & z15=0 & z16=0 & z17=0 & z18=0
    z19=0 & z20=0 & z21=0 & z22=0 & z23=0
    z_in =0 & z_out=0 & z_buf=0

    e0 =0 & e1 =0 & e2 =0 & e3 =0 & e4 =0 & e5 =0 & e6 =0 & e7 =0 & e8 =0 & e9 =0
    e10=0 & e11=0 & e12=0 & e13=0 & e14=0 & e15=0 & e16=0 & e17=0 & e18=0
    e19=0 & e20=0 & e21=0 & e22=0 & e23=0
    e_in =0 & e_out=0 & e_buf=0

    n0 =0 & n1 =0 & n2 =0 & n3 =0 & n4 =0 & n5 =0 & n6 =0 & n7 =0 & n8 =0 & n9 =0
    n10=0 & n11=0 & n12=0 & n13=0 & n14=0 & n15=0 & n16=0 & n17=0 & n18=0
    n19=0 & n20=0 & n21=0 & n22=0 & n23=0
    n_in =0 & n_out=0 & n_buf=0

    par1 =0 & par2 =0 & par3 =0 & par4 =0 & par5 =0 & par6 =0 & par7 =0 & par8 =0
    par9 =0 & par10=0 & par11=0 & par12=0 & par13=0 & par14=0 & par15=0 & par16=0
    par17=0 & par18=0 & par19=0 & par20=0 & par21=0 & par22=0 & par23=0

    Sna1 =0 & Sna2 =0 & Sna3 =0 & Sna4 =0 & Sna5 =0 & Sna6 =0 & Sna7 =0 & Sna8 =0
    Sna9 =0 & Sna10=0 & Sna11=0 & Sna12=0 & Sna13=0 & Sna14=0 & Sna15=0 & Sna16=0
    Sna17=0 & Sna18=0 & Sna19=0 & Sna20=0 & Sna0 =0

;   Stuff for DIALS
;** ----- --- -----
    d0 =0 & d1 =0 & d2 =0 & d3 =0 & d4 =0 & d5 =0 & d6 =0 & d7 =0 & d8 =0 & d9 =0
    d10=0 & d11=0 & d12=0 & d13=0 & d14=0 & d15=0 & d16=0 & d17=0 & d18=0
    d19=0 & d20=0 & d21=0 & d22=0 & d23=0
    dial_ini=0
    duduch1 =0
    duduch2 =0
    duduch3 =''
    proxcod =''
    if  n_elements(GEORGE) eq 0 then GEORGE=0

;   W info's
;** --------
    lamp_sys  =!version.os
    lamp_devps=''
    lamp_dvd  =sys_dep      ('DIVIDER')
    ihis      =0
    his_info  =0
    his       =strarr(wn+1) & his (*) =' ' & his (wn)='.'
    lims      =strarr(wn+1) & lims(*) =' ' & lims(wn)='.'
    histxt    =strarr(1)
    limtxt    =strarr(1)
    tolerance =0. & toler=0. & monimon=-1
    inst_value='    '
    inst_group=''
    cycle     ='  ????  '
    one=1 & two=0 & three=0 & alone=0

    if (just ne 'just') then begin

;** User Directory
;** ---- ---------
@dons.cbk
      basem=widget_base (title='Lamp',/column,resource_name='lamp')
      bidon=widget_label(basem,value='LARGE ARRAY MANIPULATION PROGRAM',font=ft_biggest)
      Show_Version,basem
      w0=2
      LOGO,w0 & pax1=size(w0)
      bidon=widget_base (basem,/row)
      bicon=widget_draw (bidon,retain=2,xsize=pax1(1),ysize=pax1(2)    ,colors=-30)
      bidon=widget_label(basem,value='                               ' ,font=ft_normal)
      basm1=widget_base (basem,       /column)
      basm2=widget_base (basem,       /column)
      basm3=widget_label(basem,value='______________________________________' ,font=ft_biggest)
      bid=sys_dep      ('DYNLAB',basem,1)
      widget_control,bad_id=ii  ,basem,/realize
      widget_control,bad_id=ii  ,bicon,get_value=pax4 & wset,pax4
      tvscl,w0,0,0 & pax2=0   &  pax3=pax1(1)

      lamp_user =''  & lamp_user =STRLOWCASE(sys_dep ('GETENV','USER'))
           host =''  &      host =STRLOWCASE(sys_dep ('GETENV','HOST'))
      cd,current=mee & sl=strlen(host)+1
      path='lambda'
;           ******
      if (lamp_user eq path) or (rstrpos(mee,path) ge strlen(mee)-sp) or ((rstrpos(mee,host) ge (strlen(mee)-sl)>0 ) and (sl gt 2)) then begin
	mess=widget_label (basm1,value='Your name is the way to define your own directory',font=ft_bigger)
	namm=widget_text  (basm1,xsize=15,ysize=1,font=ft_biggest,/editable)
	rbut=widget_base  (basm1,/frame,/row)
	if sys_dep('MAP') ne -1 then $
	okbt=widget_button(rbut,value='OK',font=ft_bigger,resource_name='discret') else $
	okbt=widget_button(rbut,value='OK',font=ft_bigger)
 	print,string(7b)
	name='' & ii=0
	while (name eq '') and (ii eq 0) do begin
		widget_control,bad_id=ii,namm,/input_focus
		even=widget_event (basm1,bad_id=ii)
	        widget_control,bad_id=ii,namm,get_value=name
	        name=strupcase(strcompress(name(0),/remove_all))
	endwhile
	widget_control,bad_id=ii,basm1,sensitive=0
	if (name ne '') and (ii eq 0) then begin
		pwd=sys_dep      ('NEWDIR',lamp_user,name)
	   	stat=0 & ii=0
	   	catch,stat
	   	if (stat ne 0) and (ii eq 0) then begin
	   			  ii=1
	   			  bid=sys_dep      ('MKDIR',pwd)
	   			  catch,stat & if stat eq 0 then cd,pwd
	   	endif
	   	if stat eq 0 then cd,pwd & catch,/cancel
	endif
;	device,copy=[pax2,0,pax1(1),pax1(2),pax2+pax3,0,pax4] & pax2=pax2+pax3
      endif

;** Restore last session
;** ------- ---- -------
      fil=findfile('lamp.ses',count=true)
      if (true lt 1) then fil=findfile('lamp.ses.Z',count=true) else true=100
      if (true gt 0) and (ii eq 0) then begin
	mess =widget_label (basm2,value='Previous Session lamp.ses exists !!',font=ft_biggest)
	but  =widget_base  (basm2,/row)
	r_rm =widget_button(but ,value='Restore & Remove',font=ft_b_normal)
	r_kp =widget_button(but ,value='Restore & Keep'  ,font=ft_b_normal)
	rm   =widget_button(but ,value='Remove'          ,font=ft_b_normal)
	ign  =widget_button(but ,value='Ignore'          ,font=ft_b_normal)
 	print,string(7b)
        bid=sys_dep      ('DYNLAB',basm2,0)
	widget_control,bad_id=ii  ,basm2,/realize
	even=widget_event (basm2  ,bad_id=ok)
	widget_control,bad_id=ii  ,basm2,sensitive=0
	if ok eq 0 then begin
		if (even.id eq r_rm) or (even.id eq r_kp) then begin
        		widget_control,bad_id=ii,basm3,set_value='RESTORING lamp.ses ...'
			if true ne 100 then bid=sys_dep      ('UN_Z','lamp.ses.Z',lamp_dir)
						kpGEORGE=GEORGE
			catch,stat
	   		if stat eq 0 then begin RESTORE, 'lamp.ses'  &  GEORGE=kpGEORGE
						AFTER_RESTORE
			endif else print,"Bad version for file lamp.ses !!!" & catch,/cancel
    		endif
		if (even.id eq r_rm) or (even.id eq rm)   then $
	   		bid=sys_dep      ('DELET','lamp.ses')
	endif
;	device,copy=[pax2,0,pax1(1),pax1(2),pax2+pax3,0,pax4] & pax2=pax2+pax3
      endif else begin
		w1=shift(sin (dist(64)/15),-12,-12)*30    & w_tit(1)='w1=shift(sin (dist(64)/15),-12,-12)*30'
		x1=long (alog(findgen(64)+1.)*20) & y1=x1 & x_tit(1)='x1=long (alog(findgen(64)+1.)*20)'
		y_tit(1)='Y axis' & z_tit(1)='Counts' & other_tit(1)='Just an example'
      endelse

      if (lamp_siz ge 800) and (sys_dep('MACHINE') eq 'win' or $
				sys_dep('MACHINE') eq 'mac') then $
			SL_RESTSCAN,lamp_dir+lamp_dvd+'scan.exe' ,cnt
    endif

;** Developers
;** ----------
    lamp_b1    =0L
    lamp_act   =0
    lamp_focus =0
    lamp_entry =['',' ']
    l_message  =0L
    b_labins   =lonarr(9) & b_labins(*)=0
    jou_c      =['*******','SESSION','*******'] & jou_w=[' ',!stime,' ']
    last_form  =''
    path       =''
    my_path    =[path,'','',!D.NAME]

;** Data base directories
;** --------- -----------
    P_ENVI

;** Base Constitution
;** ---- ------------
    if (just ne 'just') then $
        widget_control,bad_id=ii,basm3,set_value='CREATING manipulation display TOOLS'

    if (just eq '') or (just eq 'lamp') then begin
	mbar=0L
	if sys_dep('VERSION') lt 4.0 then mkbar=0 else mkbar=1 ;& mkbar=0 ;;;for tests
	tit='LAMP '
	mmp =1
	if lamp_siz ge 600  then  if mkbar  then  mmp=0
	if sys_dep('MACHINE') eq 'mac'      then  mmp=1
	if GEORGE eq 3 then begin GEORGE=2      & mmp=1 & endif
	if GEORGE eq 1 then begin tit='GEORGE ' & mmp=1 & endif

	if GEORGE eq 2 then tit='LAMP'
	tit      =tit+' ftp.ill.fr/pub/cs/lamp... (email:lamp@ill.fr)'
	tit      =tit+'     powered by Idl'
	if sys_dep('EMBEDDED') then tit=tit+' from Research Systems(www.rsinc.com)'
	if lamp_siz lt 600 then $
	 if not mkbar then $
	 lamp_b1  =widget_base  (title=tit,resource_name='lamp',kill_notify='P_DYING',/column,$
				 x_scroll=590,y_scroll=lamp_siz-50) else $
	 lamp_b1  =widget_base  (title=tit,resource_name='lamp',kill_notify='P_DYING',/column,$
				 x_scroll=590,y_scroll=lamp_siz-50,MBAR=mbar,rname_mbar='lampben')
	if lamp_siz ge 600 then $
	 if not mkbar then $
	 lamp_b1  =widget_base  (title=tit,resource_name='lamp',kill_notify='P_DYING',/column) else $
	 lamp_b1  =widget_base  (title=tit,resource_name='lamp',kill_notify='P_DYING',/column,MBAR=mbar,rname_mbar='lampben')

	lamp_tmp1=widget_base  (lamp_b1  ,/row)
;don
	lamp_don   =lonarr(5)
	lamp_don(0)=widget_base (lamp_b1  ,/frame,resource_name='don',map=mmp)

	lamp_tmp2  =widget_base (lamp_tmp1,/column)

;ben
	lamp_ben   =lonarr(17)
	if GEORGE eq 2 then begin lamp_ben(0) =widget_base (lamp_tmp1   ,map=mmp)
				  lamp_ben(7) =widget_base (lamp_ben(0) ,/column,map=1)   & bas_ben=lamp_ben(7)
				  lamp_ben(8) =widget_base (lamp_ben(0) ,/column,map=0)   & endif

	if GEORGE ne 2 then begin lamp_ben(0) =widget_base (lamp_tmp1   ,map=mmp)
				  bas_ben     =widget_base (lamp_ben(0) ,/column)
				  lamp_ben(7) =bas_ben   &  lamp_ben(8) =bas_ben          & endif
				  
	if mbar   gt 0 then begin
				  lamp_ben(10)=widget_base (lamp_ben(0) ,/column,map=0)
				  lamp_ben(11)=widget_base (lamp_ben(10),/column,/frame,resource_name='ben')
				  mllab       =widget_label(lamp_ben(11),value='..MACROS LIST..',font=ft_propor)
				  endif
	ben_f=1
	if GEORGE ne 1  then begin
	  lamp_ben(2)=widget_base  (bas_ben ,/frame,resource_name='ben')
	  lamp_ben(1)=widget_base  (bas_ben)
	  ben_f=0
	endif
	if GEORGE ne 0  then begin
	  if lamp_siz ge 900 then lab='..Other functions..' else lab='....Other functions....'
	  if sys_dep('MACHINE')   eq  'mac'                 then lab='..Functions..'
	  if GEORGE eq 1 then bas_geo2   =widget_button(lamp_ben(8),value=lab,font=ft_b_normal,menu=2)
	  if GEORGE eq 1 then lamp_ben(2)=bas_geo2
	endif
;micmac
	lamp_mac =widget_base (lamp_tmp2,/frame,resource_name='mic')
	lamp_ben(9)=lamp_mac
;did
	lamp_did   =lonarr(6)
	if  lamp_siz ge 900 then $
	sepdid	   =widget_draw (lamp_tmp2,xsize=600,ysize=4) else sepdid=0
	lamp_did(0)=widget_base (lamp_tmp2,/frame,resource_name='did')

	lamp_don(2)=lamp_ben(2)
	lamp_don(3)=lamp_did(0)

	if  lamp_siz ge 900 then $
	sepdon	   =widget_draw (lamp_tmp2,xsize=600,ysize=4) else sepdon=0

	widget_control,bad_id=ii,lamp_b1,default_font=ft_normal
	
;** Run  Selector Unit Creation
;** ---- -------- ---- --------
	if lamp_asite eq 'mic' then MIC,1

;** Workspace Manipulation Unit Creation
;** --------- ------------ ---- --------
	P_DON_CREATE ,(lamp_don(0))

;** File Selector Unit Creation
;** ---- -------- ---- --------
	P_MAC_CREATE ,(lamp_mac+0) ,mbar ,wread ,b33 ,b1,cque

;** Main Display Unit Creation
;** ---- ------- ---- --------
	P_DID_CREATE ,(lamp_did(0))

;** Specific Display Unit Creation
;** -------- ------- ---- --------
	P_BEN_CREATE ,(lamp_ben(2)),ben_f

;** General Functions Unit Creation
;** ------- --------- ---- --------
	P_FCT_CREATE ,(bas_ben), bas_geo2

;** Menu-bar Creation
;** -------- --------
	lamp_ben(15)=wread(0)
	lamp_ben(16)=wread(1)
	IF mbar gt 0 then begin	IF b33 gt 0 then widget_control,bad_id=i,b33, get_uvalue=uvbuti else uvbuti=0

	Show_version,0,lamp_version
	Mfile	=widget_button(mbar,font=ft_propor,/menu,value='File')
	Mcustom	=widget_button(mbar,font=ft_propor,/menu,value='Customize')
	Medit	=widget_button(mbar,font=ft_propor,/menu,value='Edit')
	Minfo	=widget_button(mbar,font=ft_propor,/menu,value='Info')
	Mtool	=widget_button(mbar,font=ft_propor,/menu,value='Tools')
	Mwind	=widget_button(mbar,font=ft_propor,/menu,value='Lamp/Layout')
	Mhelp	=widget_button(mbar,font=ft_propor,/menu,value='Help')

	Mfile1	=widget_button(Mfile	,font=ft_b_normal,value='EXPORT workspace'               ,uvalue=[-88,370,0,0,0,0])
	Mfile1	=widget_button(Mfile	,font=ft_b_normal,value='IMPORT file & workspace'        ,uvalue=[-88,380,0,0,0,0])
	Mfiler	=widget_button(Mfile	,font=ft_b_normal,value='Multiple READ (Selector access)',uvalue=uvbuti)
	Mfile1	=widget_button(Mfile	,value='--------')     & widget_control,Mfile1,sensitive=0
	Mfile1	=widget_button(Mfile	,font=ft_b_normal,value='BROWSE selected instrument path',uvalue=[-88,562,0,wread(0),wread(1),-2,-2,0])
	Mfilec	=widget_button(Mfile	,font=ft_b_normal,value='Instrument CALIBRATION files'   ,uvalue=[-88,307,11])
	Mfilet	=widget_button(Mfile	,font=ft_b_normal,value='CATALOG of data runs'           ,uvalue=[-88,331,0])
	Mfile1	=widget_button(Mfile	,value='--------')     & widget_control,Mfile1,sensitive=0
	Mfilem	=widget_button(Mfile	,font=ft_b_normal,value='MAKE a User MACRO'              ,uvalue=[-88,203,0,0])
	Mfile1	=widget_button(Mfile	,value='--------')     & widget_control,Mfile1,sensitive=0
	Mfile1	=widget_button(Mfile	,font=ft_b_normal,value='Save all'                       ,uvalue=[-88,397,0])
	Mfile1	=widget_button(Mfile	,value='--------')     & widget_control,Mfile1,sensitive=0
	Mfile1	=widget_button(Mfile	,font=ft_b_normal,value='Save and exit'                  ,uvalue=[-88,307,12])
	Mfile1	=widget_button(Mfile	,font=ft_b_normal,value='Exit'                           ,uvalue=[-88,398,0])

	Mcustom1=widget_button(Mcustom	,font=ft_b_normal,value='COLORS table'                   ,uvalue=[-88,347,0])
	Mcustom1=widget_button(Mcustom	,font=ft_b_normal,value='PLOTTING preferences'           ,uvalue=[-88,360,0])
	Mcustom1=widget_button(Mcustom	,font=ft_b_normal,value='Default PRINTER (Unix)'         ,uvalue=[-88,360,0])
	Mcustom1=widget_button(Mcustom	,value='--------')     & widget_control,Mcustom1,sensitive=0
	Mcustomf=widget_button(Mcustom	,font=ft_b_normal,value='Update data FORMAT TABLE (instruments)'   ,uvalue=[-88,560,0,0,0,-1])
	Mcustomp=widget_button(Mcustom	,font=ft_b_normal,value='Update PATH for instruments data base'    ,uvalue=[-88,560,0,0,0,-1])
	Mcustomr=widget_button(Mcustom	,font=ft_b_normal,value='Template for READ IN procedures: read_tmp',uvalue=[-88,560,0,0,0,-1])
	Mcustomm=widget_button(Mcustom	,font=ft_b_normal,value='User macros LOCATION'           ,uvalue=[-88,560,0,0,0,-1])
	Mcustom1=widget_button(Mcustom	,value='--------')     & widget_control,Mcustom1,sensitive=0
	Mcustoms=widget_button(Mcustom	,font=ft_b_normal,value='How to implement "INSTRUMENT MACROS" layout',uvalue=[-88,596,0,0])

	Medit1	=widget_button(Medit	,font=ft_b_normal,value='COLORS table'                   ,uvalue=[-88,347,0])
	Medit1	=widget_button(Medit	,font=ft_b_normal,value='DATA PARAMETERS values'         ,uvalue=[-88,204,0,0])
	Meditc	=widget_button(Medit	,font=ft_b_normal,value='Instrument CALIBRATION'         ,uvalue=[-88,307,11])
	Medit1	=widget_button(Medit	,font=ft_b_normal,value='User batch files , MACROS'      ,uvalue=[-88,203,0,0])
	Medit1	=widget_button(Medit	,value='--------')     & widget_control,Medit1,sensitive=0
	Meditw1	=widget_button(Medit	,font=ft_b_normal,value='STORE all workspaces in   secondary memory' ,uvalue=[-88,572,0,1])
	Meditw2	=widget_button(Medit	,font=ft_b_normal,value='RESTORE   workspaces from secondary memory' ,uvalue=[-88,572,0,2])
	Meditw3	=widget_button(Medit	,font=ft_b_normal,value='EXCHANGE  workspaces with secondary memory' ,uvalue=[-88,572,0,3])
	Meditw4	=widget_button(Medit	,font=ft_b_normal,value='CLEAR all workspaces and  memories'         ,uvalue=[-88,572,0,4])

	Minfo1	=widget_button(Minfo	,font=ft_b_normal,value='JOURNAL of current session'     ,uvalue=[-88,396,0,0])
	Minfo1	=widget_button(Minfo	,font=ft_b_normal,value='DATA PARAMETERS'                ,uvalue=[-88,204,0,0])
	Minfo1	=widget_button(Minfo	,value='--------')     & widget_control,Minfo1,sensitive=0
	Minfo1	=widget_button(Minfo	,font=ft_b_normal,value='Internal ROUTINES (lamp)'       ,uvalue=[-88,203,0,1])
	Minfo1	=widget_button(Minfo	,font=ft_b_normal,value='Available User BATCH files'     ,uvalue=[-88,203,1,0])
	Minfo1	=widget_button(Minfo	,value='--------')     & widget_control,Minfo1,sensitive=0
	Minfor	=widget_button(Minfo	,font=ft_b_normal,value='Template for READ IN procedures',uvalue=[-88,560,0,0,0,-1])

	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='OVERPLOTTING (superplot)'       ,uvalue=[-88,352])
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='SCROLL spectra within 2D,1D workspaces' ,uvalue=[-88,402,0])
	Mtool1	=widget_button(Mtool	,value='--------')     & widget_control,Mtool1,sensitive=0
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='SECTOR integration (small 2D wksp)'  ,uvalue=[-88,401,0])
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='RADIAL integration (small 2D wksp)'  ,uvalue=[-88,401,0])
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='UNROLLING for integration (large 2D wksp)' ,uvalue=[-88,401,-1])
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='TOMOGRAPHY'                          ,uvalue=[-88,401,-2])
	Mtool1	=widget_button(Mtool	,value='--------')     & widget_control,Mtool1,sensitive=0
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='FITTING with GKfit (gauss,lorentz)'  ,uvalue=[-88,580,0,0])
	Mtool1	=widget_button(Mtool	,value='--------')     & widget_control,Mtool1,sensitive=0
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='Qens_fit (gauss+lorentz+delta) for TOF '   ,uvalue=[-88,571,2])
								 widget_control,Mtool1,sensitive=0
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='Spectra GROUPING for TOF'            ,uvalue=[-88,403,0])
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='lampINX for TOF'                     ,uvalue=[-88,571,1])
	Mtool1	=widget_button(Mtool	,value='--------')     & widget_control,Mtool1,sensitive=0
;	Mtoolo	=widget_button(Mtool	,font=ft_b_normal,value='CRON tasks (toggle)')
	Mtool1	=widget_button(Mtool	,font=ft_b_normal,value='Multiple READ (Selector access)'     ,uvalue=uvbuti)
	Mtooln	=widget_button(Mtool	,font=ft_b_normal,value='Glory-Hole (scan)'                   ,uvalue=[-88,306,0,-1])
	
	Mwindf	=widget_button(Mwind	,font=ft_b_normal,value='Extend to classical Lamp')
	Mwindm	=widget_button(Mwind	,font=ft_b_normal,value='Light  configuration')
	Mwind1	=widget_button(Mwind	,value='--------')     & widget_control,Mwind1,sensitive=0
	Mwindl	=widget_button(Mwind	,font=ft_b_normal,value='LAMP   layout')
	Mwindg	=widget_button(Mwind	,font=ft_b_normal,value='GEORGE layout (dials)')
	Mwind1	=widget_button(Mwind	,value='--------')     & widget_control,Mwind1,sensitive=0
	Mwind1	=widget_button(Mwind	,font=ft_b_normal,value='DO input commands (...)'             ,uvalue=[-88,224])
	Mwindx	=widget_button(Mwind	,font=ft_b_normal,value='XBU input commands (...)'            ,uvalue=[-88,226])
	Mwind1	=widget_button(Mwind	,value='--------')     & widget_control,Mwind1,sensitive=0
	Mwinds	=widget_button(Mwind	,font=ft_b_normal,value='Help for INSTRUMENT MACROS',/menu)

	Mhelp1	=widget_button(Mhelp	,font=ft_b_normal,value='MANUAL  lamp/manual/front.html' ,uvalue=[-88,201,0,0])
	Mhelp1	=widget_button(Mhelp	,value='--------')     & widget_control,Minfo1,sensitive=0
	Mhelp1	=widget_button(Mhelp	,font=ft_b_normal,value='List  of ROUTINES (Idl)'        ,uvalue=[-88,573,0,0])
	Mhelp1	=widget_button(Mhelp	,font=ft_b_normal,value='Language HELP (Idl)'            ,uvalue=[-88,573,0,0])
	Mhelp1	=widget_button(Mhelp	,value='--------')     & widget_control,Minfo1,sensitive=0
	Mhelp1	=widget_button(Mhelp	,font=ft_b_normal,value='Manipulation syntax overview'   ,uvalue=[-88,588,0,0])
	Mhelp1	=widget_button(Mhelp	,font=ft_b_normal,value='Reading into workspaces'        ,uvalue=[-88,586,0,0])
	Mhelp1	=widget_button(Mhelp	,font=ft_b_normal,value='Displaying workspaces'          ,uvalue=[-88,587,0,0])
	Mhelp1	=widget_button(Mhelp	,value='--------')     & widget_control,Minfo1,sensitive=0
	Mhelpc	=widget_button(Mhelp	,value='About Lamp',/menu)
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='Start Lamp project: Apr 1994')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='This update: '+lamp_version)
	Mh	=widget_button(Mhelpc	,font=ft_propor,value=' ')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='Kernel Design:')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Didier   Richard   (ILL scientific computing engineer)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Don      Kearley   (ILL scientist -->1999 DELFT)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Michel   Ferrand   (ILL scientist -->1998 CEA)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value=' ')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='Scientific macros support:')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Ken      Andersen  (ISIS scientist -->2002 ILL)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Bob      Cubitt    (ILL  scientist    LSS)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Thomas   Hansen    (ILL  scientist    DIF)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Stephane Rols      (ANL  scientist -->2002 Montpellier Univ.)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        J.Ross   Stewart   (ILL  scientist    TOF)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value=' ')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='Complementary development:')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Arne     Dallmeyer (Thesis   in 1994)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Benjamin Vettier   (Grenoble university in 1994)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Romuald  Jouffrey  (Grenoble university in 1995,96)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='        Philippe Cuerq     (Grenoble university in 1996)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value=' ')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='Mail: lamp@ill.fr')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='http://www.ill.fr/data_treat/lamp/front.html (The Lamp Book)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='http://www.ill.fr/YellowBook/D7/home/D7_george_book (George on D7)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value='http://barns.ill.fr         (The applications web server)')
	Mh	=widget_button(Mhelpc	,font=ft_propor,value=' ftp://ftp.ill.fr/pub/cs    (Take-away sources & runtimes)')
	lamp_ben(12)=Mwindl
	lamp_ben(13)=Mwindg
	lamp_ben(14)=Mwindm
	if mmp                 then widget_control,Mwindf  ,sensitive=0
	if lamp_siz lt 600     then widget_control,Mwindf  ,sensitive=0
	if lamp_siz lt 600     then widget_control,Mwindm  ,sensitive=0
	if lamp_siz lt 800     then widget_control,Mtooln  ,sensitive=0
	if (sys_dep('EMBEDDED')  or sys_dep('RUNTIME')) then runt=1 else runt=0
	if runt                then widget_control,Mfilem  ,sensitive=0
	if runt                then widget_control,Mcustomr,sensitive=0
	if runt                then widget_control,Mcustomm,sensitive=0
	if runt                then widget_control,Minfor  ,sensitive=0
	if cque     eq 0       then widget_control,Mwindg  ,sensitive=0
	if cque     eq 0       then widget_control,Mwindl  ,sensitive=0
	if GEORGE   eq 2       then widget_control,Mwindl  ,sensitive=0
	if GEORGE   eq 1       then widget_control,Mwindg  ,sensitive=0
	if GEORGE   eq 1       then widget_control,Mfiler  ,sensitive=0
	if GEORGE   eq 1       then widget_control,Mcustomf,sensitive=0
	if GEORGE   eq 1       then widget_control,Mcustomp,sensitive=0
	if GEORGE   eq 1       then widget_control,Mcustoms,sensitive=0
	if GEORGE   eq 1       then widget_control,Mcustomr,sensitive=0
	if GEORGE   eq 1       then widget_control,Minfor  ,sensitive=0
	if GEORGE   eq 1       then widget_control,Mwinds  ,sensitive=0
	if GEORGE   eq 0       then widget_control,Mwindx  ,sensitive=0

	P_LAMBDA,pathmac
	pathcal =sys_dep('INSUB',pathmac,'CALIBRATION') & calfile =findfile(pathcal+'*',count=cnt)
	if cnt      eq 0       then widget_control,Mfilec  ,sensitive=0
	if cnt      eq 0       then widget_control,Meditc  ,sensitive=0
	
	toufile =findfile(lamp_touch+lamp_dvd+'*',count=cnt)
	if cnt      eq 0       then widget_control,Mfilet  ,sensitive=0

	widget_control,Meditw1,set_uvalue=[-88,572,0,1,Meditw2,Meditw3]
	widget_control,Meditw2,sensitive=0
	widget_control,Meditw3,sensitive=0
	empty & sby=0 & sp=0
	if sys_dep('MACHINE') eq 'win' then begin sby=30 & sp=1 & endif

	gml=widget_info(lamp_b1    ,/geometry) & sty=gml.scr_ysize+2*gml.margin + sby
	gmm=widget_info(lamp_don(0),/geometry) & ssy=gmm.scr_ysize+2*gmm.margin & ssy=sty-ssy

	if not mmp then widget_control,lamp_b1,scr_ysize=ssy
	if not mmp then b_labins(6)=1
	
	fullist=[''] & MAC_LIST,n_em,fullist,maclist,THISFILE='A_List_*.prox'
	if fullist(0) gt ' ' then begin
	   idx=sort(maclist) & maclist=maclist(idx) & fullist=fullist(idx)
	   idx=uniq(maclist) & maclist=maclist(idx) & fullist=fullist(idx)
	   for i=0,n_elements(maclist)-1 do $
	       bid =widget_button (Mwinds,font=ft_b_normal,value=maclist(i),uvalue=[-88,570,0,Mwinds,Mwindl,Mwindg,Mwindm,i])
	            widget_control,Mwinds,set_uvalue=fullist
	endif else  widget_control,Mwinds,sensitive =0
	endif

;** Event Loop
;** ----- ----

	P_MUS,''
	widget_control,bad_id=ii,basem	,/destroy
	P_MUS,'mus_shot'

	bid=sys_dep      ('DYNLAB',lamp_b1,1)
	widget_control,bad_id=ii  ,lamp_b1 ,/realize

	IF mbar gt 0 then begin
	   gml=widget_info(lamp_b1    ,/geometry) & stx=gml.scr_xsize+2*gml.margin
	   gmd=widget_info(lamp_ben(0),/geometry) & ssx=gmd.scr_xsize+2*gmd.margin & ssx=stx-ssx
	   widget_control,lamp_ben(10),set_uvalue=[-99,0L,mllab,stx]
	   widget_control,Mwindm      ,set_uvalue=[-88,307,62,ssx,ssy,stx,sty,Mwindf,Mwindm]
	   widget_control,Mwindf      ,set_uvalue=[-88,307,61,ssx,ssy,stx,sty,Mwindf,Mwindm]

	   widget_control,Mwindl,set_uvalue=[-88,557,0,b1,cque,-1,Mwindl,Mwindg,Mwindm,ssx,stx]
	   widget_control,Mwindg,set_uvalue=[-88,557,0,b1,cque, 1,Mwindl,Mwindg,Mwindm,ssx,stx]
;	   widget_control,Mtoolo,set_uvalue=[-88,557,0,b1,cque, 1,Mwindl,Mwindg,Mwindm,ssx,stx]
	endif
	if not mmp then begin	 widget_control,bad_id=ii,lamp_b1,scr_xsize=ssx
				 widget_control,bad_id=ii,Mwindm     ,sensitive=0
				 endif

	P_AFTER_REALIZE_DID,sepben,sepdon,sepdid

	if GEORGE ne 0 then GEORGEO,  /init $
	else if (lamp_siz lt 800) then put_logo, /TIO

	XMANAGER, 'LAMP' ,lamp_b1  ,event_handler='LAMP_EVENT_PARSER',CLEANUP='P_DYING',/just_reg
	if not runt then widget_control, lamp_b1, set_uvalue=[-85,700,1], TIMER=1.
	if sys_dep('VERSION') gt 5.0 then ii=execute('XMANAGER,/NO_BLOCK') else XMANAGER

    endif else begin
    	if lamp_asite eq 'mic'   then MIC,1
	if just       eq 'touch' then begin
	   TOUCH_B,1,'','+'
	   P_MUS,'' & widget_control,bad_id=ii,basem,/destroy & XMANAGER
	   if lamp_b1 eq -100  then  LAMP
	endif else $
	if just       eq 'tripx' then begin
	   ii=execute('TRIPX, /three_axis')
	   P_MUS,'' & widget_control,bad_id=ii,basem,/destroy & XMANAGER
	   if lamp_b1 eq -100  then  LAMP
	endif
    endelse

  endelse
; -------
; if just ne 'just'  then bid=sys_dep      ('EXIT')
end

pro P_LAYOUT, flag
;** ********
;**
@lamp.cbk

ev={WIDGET_BUTTON,id:0L,top:lamp_b1,handler:0L,select:1}

if n_elements(flag) eq 1 then case flag of
  'george': begin ev.id=lamp_ben(13)          & end
  'lamp':   begin ev.id=lamp_ben(12)          & end
  'light':  begin ev.id=lamp_ben(14)          & end
  'full':   begin widget_control,bad_id=ii,lamp_ben(14),get_uvalue=uv
                  if ii eq 0 then ev.id=uv(7) & end
   else:
endcase

if ev.id gt 0 then LAMP_EVENT_PARSER, ev
end

pro AFTER_RESTORE
;** *************
;**
@lamp.cbk
    	lamp_sys  =!version.os
	lamp_b1   = 0L & p_screen	& p_set_font, 0
	lamp_act  = 0L & lamp_focus =0	& l_message  =0L
	b_labins  = lonarr(9)		& b_labins(*)=0L

	jou_c     =['*******','SESSION','*******'] & jou_w=[' ',!stime,' ']
	last_form =''
	path      ='' & my_path   =[path,'','',!D.NAME]
end

pro P_ENVI ,cust
;** ******
;**
;** Track environments and custome variables.
@lamp.cbk
    p_screen

    lamp_user ='' & lamp_user =STRLOWCASE(sys_dep      ('GETENV','USER'))
    lamp_dir  ='' & lamp_dir  =sys_dep      ('GETENV','LAMP_DIR' ) & lamp_dir=strtrim(lamp_dir,2)
    lamp_exec ='' & lamp_exec =sys_dep      ('GETENV','LAMP_EXEC')
    lamp_host ='' & lamp_host =sys_dep      ('GETENV','HOST')
    		 if lamp_host eq '' then lamp_host=getenv('SYS$NODE')
    		 if lamp_dir  eq '' then cd,current=lamp_dir
    j=STRPOS (lamp_host, ':') & if j ge 0 then lamp_host=STRMID(lamp_host,0,j)
    lamp_host =STRLOWCASE(lamp_host)

    lamp_sys  =!version.os
    lamp_dvd  =sys_dep      ('DIVIDER')
    lamp_6    =6
    lamp_proxy=''

    lamp_ins  =['demo']
    lamp_wrti =[' '] & lamp_wrtp =[' ']
    lamp_grp  =[' ']
    lamp_ali  =['Current Path']
    lamp_path =['']
    lamp_asite= 'rdfilter'
    lamp_fsite= ' '

    lamp_proc    =strarr(n_elements(lamp_ins))
    lamp_proc(*) ='rdid'
    lamp_proc(0) ='read_tmp'
    lamp_touch   ='demo/TOUCH_BASE'
    lamp_macro   ='~lambda/macros'

    lamp_data = lamp_path(0)
    lamp_cyc  = [0L,0L]
    cycle     =lamp_ali(lamp_cyc(0))
    path_for_online=lamp_path(lamp_cyc(0))

    nld=strlen(lamp_dir)
    if (lamp_dir ne '')   then begin
     if strmid (lamp_dir,nld-1,1) eq lamp_dvd then lamp_dir=strmid(lamp_dir,0,nld-1)
     bid=sys_dep      ('IDLPATH',lamp_dir,nld)
    endif

    if n_elements(cust) eq 0 then P_NEWCUST

;   if lamp_sys eq 'vms' then lamp_touch=' '

    idx =reverse(sort(lamp_grp))
    lamp_grp =lamp_grp (idx)
    lamp_ins =lamp_ins (idx)
    lamp_proc=lamp_proc(idx)

    if n_elements(lamp_ins) eq 2 then begin inst_value=lamp_ins(0) & inst_group=lamp_grp(0)
    endif else  for i=0,n_elements(lamp_ins)-1 do begin
    		    if strpos(strlowcase(lamp_ins(i)),lamp_host) ge 0 then begin
    		       inst_value=lamp_ins(i) & inst_group=lamp_grp(i)  &  endif
    		endfor

    if  strpos(!path,"home") lt 0 then  begin	ta=findfile('home') & l_me=''
					;if n_elements(ta) ge 1 then   l_me=expand_path('+home')
					;if n_elements(ta) ge 1 then   l_me=expand_path( 'home')
					 if l_me ne '' then bid=sys_dep ('ADDPATH',l_me)
					endif
    meed=sys_dep ('NEWSUB',sys_dep ('HOME'),'../DIALS')

    if  strpos(!path,"DIAL") lt 0 then  begin	ta=findfile(meed)   & l_me=''
					if n_elements(ta) gt 1 then   l_me=expand_path('+'+meed)
					if l_me ne '' then bid=sys_dep ('ADDPATH',l_me)
					endif
    lamp_macro=expand_path(lamp_macro)
    if (lamp_macro gt ' ') then  begin      ta=findfile(lamp_macro+lamp_dvd+'*') & l_me=''
					if n_elements(ta) gt 1 then   l_me=expand_path('+'+lamp_macro)
					if l_me ne '' then bid=sys_dep ('ADDPATH',l_me)
					                   bid=sys_dep ('ADDPATH',lamp_macro)
    endif
;** Local or Remote
;** ----- -- ------
        lamp_loc  =0
	disp  ='' &      disp =sys_dep      ('GETENV','DISPLAY')
	if disp			  eq '' then lamp_loc=1 else $
	if strpos(disp,':')       eq 0	then lamp_loc=1 else $
	if strpos(disp,lamp_host) eq 0  then lamp_loc=1
return
end

FUNCTION P_LAMBDA, dummy
;******* ********
;**
@lamp.cbk

rst=lamp_macro
idx=strpos(lamp_macro,'macros')
if  idx gt 0 then begin rst= strmid(lamp_macro,0,idx-1) +lamp_dvd
			if   lamp_dvd eq "" then rst=rst+"]"
             endif else if   lamp_dvd ne "" then rst=rst+lamp_dvd

if   strmid     (rst,strlen(rst)-1,1) ne lamp_dvd then rst=rst+lamp_dvd
bid= findfile   (rst+'*',count=n)
if n eq 0 then rst=''
return, rst
end
PRO P_LAMBDA, rst
;** ********
;**
rst=P_LAMBDA (dum)
end

pro P_NEWCUST ,fromcust=fromcust
;** *********
;**
;** Update customisable tables.
@lamp.cbk

    datp      = ''    & init= 'init'
    stat=0 & catch,stat
    if stat  ne 0  then catch,/cancel else begin
			pth=sys_dep      ('NEWSUB',lamp_dir,'lamp_mac')
			t = findfile (pth + 'read_par.pro',count = exist)
			if (exist eq 0) then begin pth=!dir    +sys_dep("DIVIDER")
			t = findfile (pth + 'read_par.pro',count = exist) & endif
			if (exist eq 0) then begin status=23 & datp=0 & bid=0
			                ii=execute('bid=read_par("init","","",status,datp)')   & endif
			on_ioerror, mis_par & in=-1
			
			OPENR,in,pth+'read_par.pro',/get_lun
			on_ioerror, end_par
			      ligne=' '   & ttinst='' & ttproc ='' & ttgroup ='' & ttsymbol='' & ttpath=''
			      ttwall=''   & ttouch='' & ttmacro='' & ttaccess='' & ttsite  ='' & ttmagi='6'
			      ttpars='40'
			      WHILE (1) DO begin
            		          readf,in,ligne
            		          IF (strpos(ligne,';exec') gt 0) THEN r=execute(ligne)
        		      ENDWHILE
        		end_par:  datp={a:ttinst,  b:ttproc,  c:ttgroup,  $
              				d:ttsymbol,e:ttpath,  f:ttouch,   $
              				g:ttmacro, h:ttaccess,i:ttsite,j:ttmagi,k:ttwall,l:ttpars}
			mis_par:if in gt 0 then FREE_LUN,in
		    endelse

    if n_tags(datp) gt 0 then  begin
    	    nins = datp.a
    	    nproc= datp.b
    	    ngrp = datp.c
	    lamp_wrti =[' '] & lamp_wrtp =[' ']
;**
    	    for i= 0,n_elements(nins)-1 do begin  pos=-1
	      if strpos(nins(i),'.') ne strlen(nins(i))-1 then begin
    	        for j=0,n_elements(lamp_ins)-1 do if lamp_ins(j) eq nins(i) then pos=j
    	    	if  pos ge 0 then begin
    	    	    lamp_proc(pos)=nproc(i)
    	    	    lamp_grp (pos)=ngrp (i)
    	    	endif else begin
    	    	    lamp_ins	  =[lamp_ins ,nins (i)]
    	    	    lamp_proc	  =[lamp_proc,nproc(i)]
    	    	    lamp_grp	  =[lamp_grp ,ngrp (i)]

    	    	    if n_elements(b_labins) gt 0 then if b_labins(0) gt 0 then begin
		       n  =n_elements(lamp_ins)-1
	               bid=widget_button(b_labins(0),font=ft_b_normal,value=lamp_ins(n),$
	               			 uvalue=[-88,560,0,b_labins(0) , b_labins(1),n,0,0])
		    endif
    	    	endelse
	      endif else begin lamp_wrti=[lamp_wrti,strmid(nins(i),0,strlen(nins(i))-1)]
	                       lamp_wrtp=[lamp_wrtp,nproc(i)] & endelse
    	    endfor
;**
    	    nali =datp.d
    	    npath=datp.e
    	    for i= 0,n_elements(nali)-1 do begin  pos=-1
    	        for j=0,n_elements(lamp_ali)-1 do if lamp_ali(j) eq nali(i) then pos=j
    		nld=strlen(npath(i))
     		if  strmid(npath(i),nld-1,1) eq lamp_dvd then npath(i)=strmid(npath(i),0,nld-1)
     	    	if  pos ge 0 then begin
    	    	    lamp_path(pos)=npath(i)
    	    	endif else begin
    	    	    lamp_ali	  =[lamp_ali ,nali (i)]
    	    	    lamp_path	  =[lamp_path,npath(i)]

    	    	    if n_elements(b_labins) gt 0 then if b_labins(1) gt 0 then begin
		       n  =n_elements(lamp_ali)-1
	               bid=widget_button(b_labins(1),font=ft_b_normal,value=lamp_ali(n),$
	               			 uvalue=[-88,561,0,b_labins(0) , b_labins(1),n,0,0])
		    endif
    	    	endelse
    	    endfor
;**
	    if keyword_set(fromcust) then RDSET,inst=inst_value
    	    lamp_touch=datp.f
    	    lamp_macro=datp.g
    	    lamp_asite=datp.h
    	    lamp_fsite=datp.i
    	    lamp_6    =datp.j
    	    lamp_proxy=datp.k
	    if strpos(strupcase(lamp_fsite),'GEORGE') ge 0 then begin
	                                     GEORGE=1 & lamp_fsite=""
					     if strpos(strupcase(lamp_fsite),'LAMP') ge 0 then GEORGE=2
					     endif
	    npp	      =long(datp.l)<10000
	    if n_elements(par_txt) eq 0 then npars=npp else $
	    if npp gt  npars then begin nw=(size(par_txt))(1) & tmp=strarr(nw,npp)
					for i=0,nw-1 do   tmp(i,0)=par_txt(i,*) & par_txt    =tmp
    					tmp=strarr(npp) & tmp(0)  =par_txt_all  & par_txt_all=tmp
					npars=npp & endif
    	    lamp_path(0)=''
    	    if n_elements(b_labins) gt 0 then if b_labins(2) gt 0 then $
    	    				 widget_control,bad_id=i,b_labins(2),set_value=lamp_fsite
    	    nld=strlen(lamp_touch)
     	    if  strmid(lamp_touch,nld-1,1) eq lamp_dvd then lamp_touch=strmid(lamp_touch,0,nld-1)
     	    vg =strpos(lamp_macro,',')
     	    if  vg gt 0 then begin vg1=strmid(lamp_macro,  0 ,vg)
     	    			   vg2=strmid(lamp_macro,vg+1,50)
     	    			   vg =findfile(vg1,count=n)
     	    			   if  n gt 0 then lamp_macro=vg1 else lamp_macro=vg2 & endif
    	    nld=strlen(lamp_macro)
     	    if  strmid(lamp_macro,nld-1,1) eq lamp_dvd then lamp_macro=strmid(lamp_macro,0,nld-1)
    endif
return
end

pro P_SCREEN
;** ********
;**
@lamp.cbk
if !D.name eq 'Z'    then screen=[1024,1280] else $
if !D.name ne 'TEK'  then device,get_screen_size=screen $
                     else screen=[800,600]
   if (screen(0) ge 1024) then screen(1)=screen(1)>800
   if (screen(1) lt 800 ) and (screen(1) gt 750) then screen(1)=800
   if n_elements(lamp_ziz) eq 1 then screen(1)=min([screen(1),lamp_ziz])>480
   lamp_ziz=screen(1)
   lamp_siz=screen(1)

return
end

pro P_SET_FONT, n ,lamp_font
;** **********
;**
@lamp.cbk

    lamp_font = sys_dep('FONTS')
    fk=n
    if (lamp_ziz le 950 ) and (fk eq 0) then fk=1
    if (lamp_ziz lt 800 ) and (fk lt 2) then fk=2

    ft_propor    = lamp_font(0,fk)
    ft_biggest   = lamp_font(1,fk)
    ft_bigger    = lamp_font(2,fk)
    ft_b_bigger  = lamp_font(3,fk)
    ft_normal    = lamp_font(4,fk)
    ft_b_normal  = lamp_font(5,fk)
    ft_smaller   = lamp_font(6,fk)
    ft_smallest  = lamp_font(7,fk)


    if n_elements(lamp_b1) gt 0 then $
    if lamp_b1 gt 0 then widget_control,bad_id=ii,lamp_b1,default_font=ft_normal

    if n  eq 0 then lamp_siz=lamp_ziz else $
    if fk eq 1 then lamp_siz=900      else lamp_siz=780
return
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------



;*			******************************
;*			**                          **
			    PRO LAMP_EVENT_PARSER,ev
;*			**                          **
;*			******************************

;** User_Value for lamp's widgets having an event has the following structure:
;** [lampcode, sequence , 0 , 0 , 0 , 0 , 0 , 0 , 0]
;**     where:
;**  lampcode=-88  for widgets under lamp
;**          =-87  for widgets under scan
;**          =-86  for others  front windows
;**          =-85  lamp_b1 Timer
;**  sequence= 100 --> 199 for MIC   unit
;**	     = 200 --> 299 for DON   unit
;**	     = 300 --> 399 for DID   unit
;**	     = 400 --> 499 for BEN   unit
;**	     = 500 --> 599 for MAC   unit
;**	     = 600 --> 699 for GEO   unit
;**  others  = 7 free parameters

@lamp.cbk
	if ev.id gt 0 then begin  i=0
	   widget_control, ev.id ,bad_id=i, get_uvalue=uv

	   if i eq 0 then if n_elements(uv) gt 1 then begin

	         stat=0 & catch,stat
	         if stat  ne 0  then begin catch,/cancel
	         		catch,stat & if stat ne 0 then retall
				therror=strmid(!err_string,0,65)
	         		widget_control,bad_id=i,l_message,set_value=therror
	         		set_plot,my_path(3)
				P_MUS,'mus_cannon'
				if b_labins(6) then  if b_labins(7) gt 0 then $
				widget_control,bad_id=i,b_labins(7),set_value=therror
	         		return & endif

		 if  tag_names(ev,/structure_name) ne 'WIDGET_DRAW'  then  nodr=1 else nodr=0

		 if uv(0) eq -85  then begin if uv(2) ge 1 then widget_control,ev.id,TIMER=uv(2)
		    if uv(2) eq 1 then begin uv(2)=2  & widget_control,ev.id,set_uvalue=uv & retall & endif
		    if uv(2) eq 2 then begin uv(2)=0  & widget_control,ev.id,set_uvalue=uv
		    		print,'%Lamp successfully loaded' & print,''
		    		print,'Enter @lamp.cbk to define lamp variables at command line' & endif
		 endif else $
		 if uv(0) eq -87  then			       P_DID_EVENT,ev,uv  else $
		 if uv(0) eq -88  then  begin

		    if ( lamp_act eq 1) and (nodr)	     then P_DID_EVENT,ev,[-88,300]
		    if my_path(1) ne '' then if uv(1) ne 576 then P_SET_PATH

		    if (uv(1) ge 600)  and (uv(1) le 699) then P_GEO_EVENT,ev,uv $
		    else begin

		    if  nodr  then widget_control,/hourglass
		    if (uv(1) ge 100)  and (uv(1) le 199) then P_MIC_EVENT,ev,uv
		    if (uv(1) ge 200)  and (uv(1) le 299) then P_DON_EVENT,ev,uv
		    if (uv(1) ge 300)  and (uv(1) le 399) then P_DID_EVENT,ev,uv
		    if (uv(1) ge 400)  and (uv(1) le 499) then begin
		    					       P_BEN_EVENT,ev,uv
							       P_DID_SETWIN0
							       endif
		    if (uv(1) ge 500)  and (uv(1) le 599) then P_MAC_EVENT,ev,uv
		    if (uv(1) eq 222)  or  (uv(1) eq 342) or $
		    			   (uv(1) eq 422) then P_EXTEND   ,ev,uv

		    if lamp_b1 gt 0 then begin
			if (GEORGE eq 0) and (uv(1) ne 390) then that=lamp_b1 else that=long(lamp_don(0))
			evv=widget_event(that,/nowait,bad_id=i)
			widget_control,bad_id=i, that ,/clear_events
			if b_labins(8) gt 0 then begin
				evv=widget_event(b_labins(8),/nowait,bad_id=i)
				if i ne 0 then b_labins(8)=0 $
				          else widget_control,bad_id=i,b_labins(8),/clear_events
			endif
		    endif
		    endelse
		 endif
	   endif
	endif
return
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

pro	P_MAC_CREATE ,base ,mbar ,wread ,b33 ,b1,cque
;**	************
;**
@lamp.cbk
	b1   =	widget_base  (base,/column)
	b11  =  widget_base  (b1  ,/row)
;b11
	bid=' DATA COLLECTOR'
	btitl=	widget_label (b11 ,font=ft_biggest ,value=bid)
	bidon='' & CALL_PROCEDURE,'myhelp',bidon

          on_ioerror, mishlp & in=-1
	  OPENR,in,'myhelp.rt',/get_lun
	  line=''
	  WHILE (1) DO begin readf,in,line & bidon=[bidon,'line'] & ENDWHILE
	  mishlp: if in gt 0 then FREE_LUN,in
	if n_elements(bidon) gt 1  then begin
	  bhelp=widget_button(b11 ,font=ft_b_normal  ,value='my HELP?')
	  bidon=widget_label (b11 ,font=ft_biggest ,value='   ')
	endif else begin
	  bhelp=widget_button(b11 ,font=ft_normal  ,value='?')
	  bidon=widget_label (b11 ,font=ft_biggest ,value='            ')
	endelse
	cd,current =path
        pwd_l=  widget_label (b11 ,font=ft_b_normal,value=' Working path:  ')
	pwd_t=  widget_text  (b11 ,value=path,font=ft_b_bigger,xsize=24,ysize=1,/editable,$
										/all_events)
	sel    =[strlen(path),0]
 	my_path=[path,'',string(pwd_t),!D.NAME]

	laber=l_message
	cque =0
;b33
	if (n_elements(lamp_ins) le 1) or (lamp_asite eq 'customiz')  then text='Customize' $
	      else if lamp_siz ge 800 then text='Data...' else text='Data...'

	if GEORGE ne 0 then begin
	   bgeo2= laber

	   if GEORGE eq 1 then begin txt='Data' & map1=1 & map2=0 & b_labins(5)=1
	   endif          else begin txt='Pad'  & map1=0 & map2=1 & b_labins(5)=0 & endelse

	   aque = widget_base  (b1,/row)

	   if mbar eq 0 then begin
		bque = widget_button(aque ,font=ft_b_normal,value=txt)
		if sys_dep('MACHINE') eq 'win'  then val='  ' else val=' '
		bid  = widget_label (aque ,font=ft_propor  ,value=  val ) & endif

	   aque = widget_base  (aque )
;	   cque = widget_base  (aque ,/row,resource_name="geo",map=map1,frame=3)
	   cque = widget_base  (aque ,/row,resource_name="geo",map=map1)

	;	STACKER DATA ACCESS MODEL
;		b1= widget_base  (aque ,map=map2,/column,frame=3)
		b1= widget_base  (aque ,map=map2,/column)

	;	BESIDE  DATA ACCESS MODEL
	;	ttl='DATA Access....'
	;	if sys_dep('VERSION') lt 4.0 then $
	;	       ii=execute('b1=widget_base(title=ttl,/column,map=0,resource_name="lampmic"') $
	;	else   ii=execute('b1=widget_base(title=ttl,/column,map=0,resource_name="lampmic",tlb_frame_attr=8+2)')
	;	bu   = widget_base (b1 ,/row)
	;	put_logo,bu
	;	laber= widget_label(bu ,font=ft_b_normal,value='                  	',xsize=(lamp_siz/2)<600>300)
	;	Widget_Control, b1 ,group_leader=lamp_b1,/Realize & put_logo
	;	Xmanager,'Daccess' , b1 ,Event_Handler='LAMP_EVENT_PARSER',/just_reg

	  if mbar eq 0 then  widget_control,bque,set_uvalue=[-88,557,0,b1,cque,0]

	   dque = widget_base  (cque ,/column)
	   eque = widget_base  (dque ,/row)
	   bid  = widget_label (eque ,font=ft_b_bigger,value='COMMAND Control PAD')
	   bid  = widget_button(eque ,font=ft_b_bigger,value='?',uvalue=[-88,595,laber,0])
	   eque = widget_base  (dque ,/row)
	   bido = widget_text  (eque ,font=ft_propor  ,xsize=20,ysize=1,/editable)
	   bidu = widget_button(eque ,font=ft_b_bigger,value='Send')

	   ii   = findfile('dial_pad_init.prox',count=nn)
	   if  nn gt 0 then COMMSI,'dial_pad_init.prox', /EXEC $
	   else begin par1=''
		Mach=strlowcase(getenv('HOST')) & id=strpos(Mach,'.')
		if id gt 0 then Mach=strmid(Mach,0,id)
		if Mach ne '' then begin
		   catch,stat
	   	   if stat eq 0 then ii=execute('par1=dial_pad_init_'+(Mach)+'(dummy)') else catch,/cancel
	   	endif
	   	sz=SIZE(par1)
		if  sz(0) lt 2 then ii=execute("par1=dial_pad_init()")
	   endelse
	   sz=SIZE(par1)
	   if (sz(0) ne 2) or (sz(1) ne 5) then begin par1=strarr(5,17) & par1(*,*)='0'
	                                              par1(0,0)='' & par1(3,0)='lamp'  & endif
	   widget_control,bido,set_value=par1(0,0)
	   PROX=par1(*,0)
	   GEORGEO, duduch=PROX
	   uvv =[-88,540,0,cque]
	   widget_control,cque,set_uvalue=par1
	   widget_control,bido,bad_id=i,set_uvalue=[uvv,0,PROX,bido,0]
	   widget_control,bidu,bad_id=i,set_uvalue=[uvv,0,PROX,bido,0]

	   i=1 & k=0 & n_e=n_elements(par1)/5
	   par1=[[par1],['','','','','']]
	   while i  lt n_e do begin
		val=par1(0,i)
		j=k/2 & if j*2 eq k then dque = widget_base  (cque ,/column)
		eque  = widget_base  (dque ,/row)
		k=k+1
;**	level1
		if par1(2,i) eq '-' then begin
		  bid1= widget_button(eque ,font=ft_b_bigger,value=val ,menu=2)
		  i=i+1
		  while strpos(par1(0,i),'-') eq 0 do begin
		   val =strmid(par1(0,i),1,15)
;**	level2
		   if par1(2,i) eq '-' then begin
			bid2=widget_button(bid1 ,font=ft_b_bigger,value=val ,menu=2)
			i=i+1
			while strpos(par1(0,i),'--') eq 0 do begin
			  val =strmid(par1(0,i),2,15)
;**	level3
			  if par1(2,i) eq '-' then begin
				bid3=widget_button(bid2 ,font=ft_b_bigger,value=val ,menu=2)
				i=i+1
				while strpos(par1(0,i),'---') eq 0 do begin
				  val =strmid(par1(0,i),3,15)
				  SetDuduch,"_send", par1(3,i), PROX
				  bid4=widget_button(bid3 ,font=ft_b_bigger,value=val,uvalue=[uvv,i,PROX,bido,i])
				  i=i+1
				endwhile
			  endif else begin
				SetDuduch,"_send", par1(3,i), PROX
				bid3=widget_button(bid2 ,font=ft_b_bigger,value=val,uvalue=[uvv,i,PROX,bido,i])
				i=i+1 & endelse
			endwhile
		   endif else begin
			SetDuduch,"_send", par1(3,i), PROX
			bid2=widget_button(bid1 ,font=ft_b_bigger,value=val,uvalue=[uvv,i,PROX,bido,i])
			i=i+1 & endelse
		  endwhile
		endif else begin
		  SetDuduch,"_send", par1(3,i), PROX
		  bid = widget_button(eque ,font=ft_b_bigger,value=val,uvalue=[uvv,i,PROX,bido,i])
		  i=i+1 & endelse
	   endwhile
	endif
;b22
	if (lamp_siz ge 800) or (GEORGE ne 0) then $
	    laber=  widget_label (b1 ,font=ft_b_normal,value='Click below to access data from experiments',xsize=(lamp_siz*2/3)<600>300)
	    b_labins(7)=laber
;	b77=  widget_base  (b1  ,map=0)
	b88=  widget_base  (b1  ,/row)
	bac=  widget_button(b88 ,font=ft_b_normal,value=text)
	b33=  widget_base  (b88 ,/row, map=0)
;b44
	IF mbar eq 0 then begin
	  b44  =  widget_base  (b1  ,/row)
	  if GEORGE ne 0 then bid='IMPORT FILES, Workspaces' $
			 else bid='IMPORT FILES  or  RESTORE Workspaces'
	  butl =  widget_button(b44 ,font=ft_b_normal,value=bid)
	  buts =  widget_button(b44 ,font=ft_b_normal,value='EXPORT')
	  if (lamp_touch gt ' ') and (lamp_siz ge 800) then $
	    if sys_dep('MAP') ne -1 then $
	       butt =  widget_button(b44 ,font=ft_b_normal,value='Catalog...',uvalue=[-88,331,0],$
								resource_name='discret')  else $
	       butt =  widget_button(b44 ,font=ft_b_normal,value='Catalog...',uvalue=[-88,331,0])
	  if   sys_dep('MAP') ne -1 then $
	       butb =  widget_button(b44 ,font=ft_b_normal,value='Browse...'    , $
								resource_name='discret') else $
	       butb =  widget_button(b44 ,font=ft_b_normal,value='Browse...')
	     
	  widget_control,butl ,bad_id=i,set_uvalue=[-88,380,0    ,0,0,0,0,0,0]
	  widget_control,buts ,bad_id=i,set_uvalue=[-88,370,0    ,0,0,0,0,0,0]
	  
	  bs1f =widget_base  (b44,/row,/frame)
	  if sys_dep('MAP') ne -1 then $
	  bs1b1=widget_button(bs1f,font=ft_smaller ,value='<-',resource_name='discret') else $
	  bs1b1=widget_button(bs1f,font=ft_smaller ,value='<-')
	  wread=widget_label (bs1f,font=ft_b_normal,value='W1 ',xsize=29)
	  if sys_dep('MAP') ne -1 then $
	  bs1b2=widget_button(bs1f,font=ft_smaller ,value='->',resource_name='discret') else $
	  bs1b2=widget_button(bs1f,font=ft_smaller ,value='->')

	  widget_control, bs1b1  ,bad_id=i,set_uvalue=[-88,310,wread,0   ,0,0,0,0,0]
	  widget_control, bs1b2  ,bad_id=i,set_uvalue=[-88,311,wread,0   ,0,0,0,0,0]
	  widget_control, butb   ,bad_id=i,set_uvalue=[-88,562,0,0,wread ,-2,-2,0]
	  
	endif else begin butb=b88 & widget_control,butb,bad_id=i,set_uvalue=[-88,562,0,0,0 ,-1,-1,0] & endelse
	
	P_DATA_ACCESS, laber,b33,bac,butb,1
	
	if n_elements(lamp_wrd) ne 1 then lamp_wrd='W1'

	widget_control,butb ,bad_id=i,get_uvalue=uv & wread=[uv(3),uv(4)]
	widget_control,bhelp,bad_id=i,set_uvalue=[-88,586,laber,0,0,0,0,0,0]
	widget_control,pwd_t,bad_id=i,set_uvalue=[-88,576,laber,0,0,0,0,0,0],SET_TEXT_SELECT=sel

	widget_control,bac  ,bad_id=i,set_uvalue=[-88,558,laber,b33,bac,butb]
return
end

pro	P_MAC_EVENT  ,event ,uv
;**	************
@lamp.cbk

	if uv(2) gt 0 then widget_control,uv(2),bad_id=i,set_value='                   '
	icoco=0

;**PAD  Control
	if uv(1) eq 540 then begin                              ;uv(3)=cque  uv(7)=widget_text
		widget_control,uv(3),get_uvalue=PadTab          ;uv(8)=idx in padtab
		ncomm=-1
		bito = 0
		if uv(4) le 0 then begin                        ;*Comes from input widget_text
			widget_control,uv(7),get_value =comm
			comm =comm(0) & ncomm=uv(8)             ;   will xecute the command
		endif else begin                                ;*Comes from button
		   if PadTab(2,uv(8)) eq "t" then begin         ;   Put command to widget_text
			widget_control,uv(7),set_value =PadTab(1,uv(8))
			widget_control,uv(7),get_uvalue=uvv & uvv(8)=uv(8)
			widget_control,uv(7),set_uvalue=uvv     ;   and put index in uv
		   endif else $
		   if (uv(4) ne 100) and $
		     (PadTab(2,uv(8)) eq "c") then begin k=uv(8);   Create a GUI-input command
		   	str8="PAD_" +strtrim  (string(k),2)
			if xregistered(str8) le 0 then begin
			   padr=str_sep    (PadTab(1,k),"<cr>")
			   padb=widget_base(title=PadTab(0,k),resource_name='lamp',/column)
			   for r=0,n_elements(padr)-1 do begin
			   	padt =str_sep      (Padr(r),"~")
			   	n    =n_elements   (padt)
			   	if (n/2)*2 ne n then padt=[padt,' ']
			   	n    =n_elements   (padt) & biti =lonarr(n)
			   	padg =widget_base  (padb,/row,resource_name='geo',/frame)
			   	for i=0,n-1,2 do begin
			     	  bid=widget_base  (padg,/column)
			     	  bil=widget_label (bid ,value=padt(i)  ,font=ft_b_bigger) & biti(i)=bil
			     	  bit=widget_text  (bid ,value=padt(i+1),font=ft_propor ,/editable,$
			     			    xsize=strlen(padt(i+1))+3) & biti(i+1)=bit
			   	endfor
			   if r eq 0 then bito=biti else bito=[bito,-1,-1,biti]
			   endfor
			   ivv  =uv & ivv(4)=100
			   padg =widget_base  (padb,/row,resource_name='geo') & put_logo,padg
			   bid  =widget_button(padg,value='SEND ->',font=ft_b_bigger,uvalue=ivv)
			   bid  =widget_button(padg,value=' CLOSE ',font=ft_b_bigger,uvalue=[-88,399,0])
			   err  =widget_label (padg,value='       ',font=ft_b_normal,xsize=250)
			   bito =[bito,err]
			   widget_control,padb,group_leader =lamp_b1,set_uvalue=bito,/realize & put_logo
			   XMANAGER, str8,padb,event_handler='LAMP_EVENT_PARSER',/just_reg
			endif
		   endif else begin
		   	ncomm=uv(8)				;   will execute the command
			if (uv(4) ne 100) then  comm =PadTab(1,uv(8)) $		;from button
			else begin    como='' & comm =[''] & r=0 & sep=''		;from GUI
				widget_control, event.top, get_uvalue=bito
				nbito= n_elements(bito)-1
				for j=1,nbito-1,2 do begin	 comi=''
		  		   if bito(j) gt 0 then begin
					widget_control,bad_id=ii,bito(j-1),get_value=labi
					widget_control,bad_id=ii,bito(j)  ,get_value=comu
					comi   = strtrim (comu(0),2)
					comm(r)= comm(r) +comi+' '
					if comi eq '' then ncomm=-1
					como   = como+sep+labi(0)+"~"+comi & sep="~"
		  		   endif else begin r=r+1  & sep=""
					comm   =[comm,'']  & como=como+"<cr>" & endelse
				endfor
				PadTab(1,uv(8))=como
				widget_control,uv(3)    ,set_uvalue=PadTab
				if ncomm lt 0 then txt='!!! Cmd is incomplete.' else txt='Sending ...'
				bito=bito(nbito)
				widget_control,bito,bad_id=ii,set_value =txt
			endelse
		   endelse
		endelse
		if ncomm ge 0 then begin                        ;*Executes the command
			d0={GENERIC:PadTab(3,ncomm),NAME:PadTab(0,ncomm),TYPE:'PAD',PROS:[uv(5),uv(6)]}
			on_ioerror, mischk & check=0. & check=float(PadTab(4,ncomm)) & mischk:
			R=0L & ii=execute('R = DialControl(comm,  d=0, check=check)')
			if (R lt 0) or (R gt 1) then txt="error-code "+string(R) else txt= '- Sent -'
			if bito gt 0 then widget_control,bito,set_value =txt     else print,txt+': '+comm
		endif
		endif

;**Lamp and George configuration
	if uv(1) eq 557 then begin widget_control,event.id,get_value=labbut
				   widget_control,bad_id=i,l_message,set_value=''
				   if n_elements(uv) gt 6 then idx=uv(5) else idx=0
				   if   idx eq 0 then begin
				    idx=strpos(strupcase(labbut),'PAD')
				    if  idx ge 0 then widget_control,event.id,set_value="Data" $
				                 else widget_control,event.id,set_value="Pad"
				   endif else begin
				    if  idx gt 0 then widget_control,uv(6),sensitive=1 else widget_control,uv(6),sensitive=0
				    if  GEORGE gt  0  then $
				    if  idx gt 0 then widget_control,uv(7),sensitive=0 else widget_control,uv(7),sensitive=1
				   endelse
				   if idx eq -1 then begin
					widget_control,bad_id=i,uv(4),map=0
					widget_control,bad_id=i,uv(3),map=1 & b_labins(5)=0
					if GEORGE       eq 2 then widget_control,bad_id=i,lamp_ben(8) , map=0
					                          widget_control,bad_id=i,lamp_ben(7) , map=1
					if lamp_ben(10) gt 0 then widget_control,bad_id=i,lamp_ben(10), map=0
					
					if b_labins(6)  eq 1 then widget_control,bad_id=i,lamp_ben(0) , map=0
					if b_labins(6)  eq 1 then widget_control,bad_id=i,lamp_b1     , scr_xsize=uv(9)
					if b_labins(6)  eq 1 then widget_control,bad_id=i,uv(8)       , sensitive=0
					
				   endif else begin
					widget_control,bad_id=i,uv(3),map=0
					widget_control,bad_id=i,uv(4),map=1 & b_labins(5)=1
					if GEORGE       eq 2 then widget_control,bad_id=i,lamp_ben(7) , map=0
					if GEORGE       eq 2 then widget_control,bad_id=i,lamp_ben(8) , map=1
					if lamp_ben(10) gt 0 then widget_control,bad_id=i,lamp_ben(10), map=0
					
					if b_labins(6)  eq 1 then widget_control,bad_id=i,lamp_b1     , scr_xsize=uv(10)
					if b_labins(6)  eq 1 then widget_control,bad_id=i,lamp_ben(0) , map=1
					if b_labins(6)  eq 1 then widget_control,bad_id=i,uv(8)       , sensitive=1
				   endelse
				   endif
;**Data Access Creation
	if uv(1) eq 558 then begin if n_elements(lamp_ins) le 1 then customiz,1
				   P_DATA_ACCESS, uv(2),uv(3),uv(4),uv(5),0
				   widget_control,bad_id=i,uv(3),map=1 & endif
	if uv(1) eq 559 then begin if n_elements(lamp_ins) le 1 then customiz,1
				   P_DATA_ACCESS, uv(2),uv(3),uv(4),uv(5),2
				   widget_control,bad_id=i,uv(3),map=1 & endif
;**Change Instrument Name
	if uv(1) eq 560 then begin
	   if uv(5) ge 0 then begin
		inst_old  =strlowcase(inst_value)
		inst_value=lamp_ins(uv(5))
		inst_group=lamp_grp(uv(5))
		idx =where(lamp_entry eq inst_group)
		if idx(0) lt 0 then begin lamp_entry=[lamp_entry,inst_group]
				;plug_in
				VV=strtrim(string(sys_dep('VERSION')),2)
				VV=strmid (VV,0,1)+strmid (VV,2,1)
				plugpth=sys_dep ('NEWSUB',lamp_dir,'plug_in')
				plugfil=findfile(plugpth+'*'+strlowcase(inst_group)+'*'+VV+'*.rt',count=nn)
				if nn gt 0 then for i=0,nn-1 do begin
				   idx =where(lamp_entry eq plugfil(i))
				   if idx(0) lt 0 then begin lamp_entry=[lamp_entry,plugfil(i)]
					P_RESTORE,plugfil(i) ,rflg
					PRINT    ,plugfil(i) +' plug_in loaded ...'
					to_don_history,-1,0,plugfil(i) +' plug_in loaded ...'
				   endif
				endfor
		endif
		if uv(3) gt 0 then widget_control,uv(3)	,bad_id=i,set_value=inst_value
		if uv(7) gt 0 then widget_control,uv(7)	,bad_id=i,set_value=inst_value
		if lamp_cyc(1)  eq 0 then cycle = lamp_ali(lamp_cyc(0))
		p_ath=lamp_path(lamp_cyc(0))
		if  cycle ne 'Current Path' then $
		 if strpos(strlowcase(cycle),'cycle') lt 0   then $
			path_for_online=p_ath+lamp_dvd $
		 else begin
			if lamp_cyc(1) gt 0 then begin  p_ath=path_for_online & n=strlen(inst_old)
					pos1 =strpos(p_ath,inst_old)
					if pos1 gt 0 then p_ath=strmid(p_ath,0,pos1-1)+strmid(p_ath,pos1+n,10)
			endif else p_ath=p_ath+lamp_dvd
			path_for_online=sys_dep('INSUB',p_ath,strlowcase(inst_value))
		 endelse
		to_don_history,-1,0,'RDSET,inst="'+inst_value+'"'
	   endif else  customiz,1
	endif
;**Change Cycle
	if uv(1) eq 561 then begin
		lamp_cyc=[uv(5),uv(7)]
		if uv(7) gt 0 then begin YT=strtrim(string(uv(7)),2)
					 if strlen(YT) eq 1 then YT='0'+YT
					 if strlen(YT) eq 2 then YT='0'+YT & cycle='Cycle '+YT
		endif else cycle  =lamp_ali(lamp_cyc(0))
		tmps=''
		if uv(4) gt 0 then widget_control,uv(4)	,bad_id=i,set_value=cycle
		if uv(6) gt 0 then widget_control,uv(6)	,bad_id=i,set_value=cycle
		path_for_online=''
		if  cycle ne 'Current Path' then $
		 if strpos(strlowcase(cycle),'cycle') lt 0    then $
		      path_for_online=lamp_path(lamp_cyc(0))+lamp_dvd $
		 else begin
		      p_ath =  lamp_path(lamp_cyc(0))+lamp_dvd
		      bid   = 'Connecting '+p_ath+' ...' & n=0
		      if uv(2) gt 0 then widget_control,bad_id=i,uv(2) ,set_value=bid else print ,bid
		      if uv(7) gt 0 then begin tmps=',cycle='+YT
			 bid  =where(lamp_ali eq 'archive') & bid=bid(0)
			 if bid ge 0 then p_arc =lamp_path(bid) else p_arc= '?'
			 p_ath=sys_dep ('INSUB',p_ath,YT)
			 bid  =FINDFILE(p_ath,count=n)
			 if n le 0 then bid  =FINDFILE(p_ath+'*',count=n)
			 if n le 0 then begin	 ;Last chance !!!
					if p_arc ne '?' then begin p_ath=sys_dep ('INSUB',p_arc+lamp_dvd,YT)
					   bid =  'Connecting the archive system ...'
			 		   if uv(2) gt 0  then widget_control,bad_id=i,uv(2) ,set_value=bid else print,bid
			 		   bid =FINDFILE(p_ath,count=n)
					endif
			 endif
			 if n gt 0 then if strpos(p_ath,p_arc) ge 0 then bid =FINDFILE('/CDBOX',count=n)
		      endif else begin  catch,stat & if stat eq 0 then begin cd,p_ath,current=mee & cd,mee & n=1 & endif
		      endelse
		      if n le 0 then bid='Connection failed !!!' else bid =''
		      if uv(2) gt 0  then widget_control,bad_id=i,uv(2)   ,set_value=bid else print,bid
		      path_for_online=sys_dep('INSUB',p_ath,strlowcase(inst_value))
		 endelse
		to_don_history,-1,0,'RDSET,base="'+lamp_ali(lamp_cyc(0))+'"'+tmps
	endif
;**Browse
	if uv(1) eq 562 then begin
		lamp_man=findfile(path_for_online,count=n)
		if n eq 0 then lamp_man=findfile(path_for_online+'*',count=n)
		uvv=uv & uvv(1)=563
		if n gt 0 then begin cd,path_for_online,current=mee & cd,mee,current=p_f_o
				     if strmid(p_f_o,strlen(p_f_o)-1,1) ne lamp_dvd then p_f_o=p_f_o+lamp_dvd
				     ln=strpos(strupcase(lamp_man(0)),strupcase(p_f_o))
				     if ln ge 0 then lamp_man=strmid(lamp_man,ln+strlen(p_f_o),30)
		endif
		if uv(5) gt 0 then widget_control,bad_id=i,uv(5),/destroy
		base =widget_base  (title='Select File to Read',resource_name='lamptouch',/column)
		uv(5)=base
		bid  =widget_label (base,value="PATH="+path_for_online,font=ft_b_normal)
		lab  =widget_base  (base,/row)
		lub  =widget_list  (lab ,value=lamp_man,xsize=15,ysize=15,font=ft_propor)
		lab  =widget_base  (lab ,/column)
		lib  =widget_base  (lab,/row) & put_logo,lib
		bid  =widget_label (lib,value="sub",font=ft_b_normal)
		lyb  =widget_text  (lib,xsize=8,ysize=1,font=ft_propor,/editable,uvalue=[-88,564,lub])
		lob  =widget_draw  (lab ,retain=2  ,xsize=192,ysize=192)
		lib  =widget_base  (lab,/row)
		bid  =widget_button(lib,value='Done'   ,font=ft_b_normal,uvalue=[-88,399,0])
		bid  =widget_button(lib,value='Refresh',font=ft_b_normal,uvalue=uv)
		
		widget_control,bad_id=i,event.id,set_uvalue=uv
		widget_control,bad_id=i,base,group_leader=lamp_b1,set_uvalue=uv(1),/realize & put_logo
		widget_control,bad_id=i,lob,get_value = lob
		uvv(6)=lob & uvv(7)=lyb
		widget_control,bad_id=i,lub,set_uvalue= uvv
		XMANAGER, 'BROWS' ,base ,event_handler='LAMP_EVENT_PARSER',/just_reg
	endif
	if uv(1) eq 563 then begin
		runtxt=lamp_man(event.index)  & uv(1)=577
		if strpos(runtxt,strlowcase(inst_value)+'_') eq 0 then begin ;(Sub dir)
		  uv(1)=564
		  uv(2)=event.id
		  event.id=uv(7)
		  widget_control,bad_id=i,uv(7),set_value=runtxt
		endif else begin
		  icoco =1
		  idx   =strpos(runtxt,'.Z')    & if idx gt 0 then runtxt=strmid(runtxt,0,idx)
		  widget_control,bad_id=i,uv(3),set_value=runtxt
		endelse
	endif
	if uv(1) eq 564 then begin
		widget_control,bad_id=i,event.id,get_value=newsub   &   newsub=strcompress(newsub(0),/remove_all)
		if newsub ne '' then begin l_man=findfile(sys_dep('INSUB',path_for_online,newsub)    ,count=n)
		      if n eq 0 then	   l_man=findfile(sys_dep('INSUB',path_for_online,newsub)+'*',count=n)
		      if n gt 2 then begin lamp_man=l_man
					   poth=path_for_online+newsub & ln=strpos(strupcase(lamp_man(0)),strupcase(poth))
					   if ln lt 0 then begin poth=path_for_online & ln=strpos(strupcase(lamp_man(0)),strupcase(poth)) & endif
					   if ln ge 0 then lamp_man=strmid(lamp_man,ln+strlen(poth),30)
					   surp=strmid(lamp_man(0),0,1) & if surp eq lamp_dvd then lamp_man=strmid(lamp_man,1,30)
					   widget_control,bad_id=i,uv(2),set_value=lamp_man
		      endif
		endif
	endif

;**Browse URL
	if uv(1) eq 567 then begin widget_control,uv(3),bad_id=ii,get_uvalue=url
	                           bid=sys_dep('BROWSE',url) & endif

;**More for Instrument specific
	if uv(1) eq 568 then begin widget_control,uv(3),bad_id=ii,get_uvalue=stuc
	if (xregistered('A_'+stuc.macro)) eq 0 then begin
		bas  =widget_base  (title=stuc.macro,/column,resource_name='lampmic')
		llg  =widget_base  (bas,/row) & put_logo,llg
		bid  =widget_label (llg,value=stuc.macro   ,font=ft_biggest)
		bid  =widget_label (llg,value=stuc.author  ,font=ft_b_normal)
		if stuc.link gt ' ' then begin
		  bod=widget_button(llg,value='Url'        ,font=ft_b_normal,/menu)
		  widget_control,bod,bad_id=ii,set_uvalue = stuc.link
		  bid=widget_button(bod,value=stuc.link    ,font=ft_b_normal,uvalue=[-88,567,0,bod])
		endif
		bid  =widget_label (bas,value=stuc.purpose ,font=ft_b_bigger)
		for i=1,n_elements(stuc.more)-1 do $
		bid  =widget_label (bas,value=stuc.more(i) ,font=ft_b_normal)
		bss  =widget_base  (bas,/row)
		txt  =widget_text  (bss,value=stuc.call    ,font=ft_propor,/editable,xsize=50,ysize=1)
		bid  =widget_button(bss,value='Do'         ,font=ft_b_normal,uvalue=[-88,214,20,txt,0])

		widget_control,bas,group_leader=lamp_b1,/realize
		if stuc.logo gt ' ' then put_logo, file=stuc.logo else put_logo
		XMANAGER,'A_'+stuc.macro,bas,event_handler='LAMP_EVENT_PARSER',/just_reg
	endif & endif

;**Run macro for Instrument specific
	if uv(1) eq 569 then begin
		CASE uv(3) of
		-1:  begin spec_wplot,event,runtxt
		     if runtxt gt '' then begin
			uv=[-88,579,b_labins(7),lamp_ben(15),lamp_ben(16)]
			widget_control,bad_id=i,uv(3),set_value=runtxt & endif
		     end
		ELSE:begin
		     catch,stat & ii=0
		     if  stat eq 0 then begin widget_control,uv(4),get_uvalue=run & ii=execute(run) & endif
		     catch,/cancel
		     if (stat ne 0) or (ii ne 1) then $
		     if (b_labins(6)) and (b_labins(7) gt 0 ) then widget_control,b_labins(7),bad_id=ii,set_value=!err_string $
		                                              else widget_control,l_message  ,bad_id=ii,set_value=!err_string
		     end
		ENDCASE
	endif

;**Instrument specific
	if uv(1) eq 570 then if n_elements(lamp_ben) ge 15 then $
	                        if lamp_ben(10) gt 0 then begin
		                    widget_control,bad_id=ii,lamp_ben(10), get_uvalue=bu
		                    widget_control,bad_id=ii,lamp_ben(7) , map=0
		if GEORGE eq 2 then widget_control,bad_id=ii,lamp_ben(8) , map=0
		                    widget_control,bad_id=ii,lamp_b1     , scr_xsize=bu(3)
		                    widget_control,bad_id=ii,lamp_ben(0) , map=1
		                    widget_control,bad_id=ii,lamp_ben(10), map=1
		if GEORGE eq 2 then widget_control,bad_id=ii,lamp_ben(13), sensitive=1
		                    widget_control,bad_id=ii,lamp_ben(14), sensitive=1
		                    widget_control,bad_id=ii,lamp_ben(12), sensitive=1
		if uv(3) gt 0 then uv7=uv(7) else uv7=uv(3)
		if (bu(0) ne uv7) or (uv7 lt 0) then begin
		   widget_control,bu(1),bad_id=ii,/destroy & bu(0) =uv7
		   bu(1)=widget_base(lamp_ben(11),/column,y_scroll =fix(lamp_siz/2.1)<435)
		   tmpbase=0 & P_messi,tmpbase,bu(1)
		   widget_control,lamp_ben(10),bad_id=ii,set_uvalue=bu
		   widget_control,bu(2) ,bad_id=ii, set_value='-------------'

		   CASE uv7 of
		   -1:	begin widget_control,bu(2) ,bad_id=ii, set_value='SPEC contents'
			spec_wlist,bu(1)
			end
		   ELSE:begin
			widget_control,event.id    ,bad_id=ii, get_value=titl
			widget_control,bu(2)       ,bad_id=ii, set_value=strupcase(strmid(titl,0,15))
			widget_control,uv(3)       ,bad_id=ii,get_uvalue=fifis
			fifi=fifis(uv(7))
			topic='' & macr='' & purp='' & auth='' & coll='' & bmc=0 & more=[''] & icof='' & lnk=''
			on_ioerror,misfifi & out=-1  & lin=''
			OPENR,lun,fifi,/get_lun
			while not eof(lun) do begin
			READF,lun,lin & lon=strlowcase(lin)

			if (strpos(lon,'topic:') ge 0) or (strpos(lon,'macro:') ge 0) then $
						   if n_elements(more) gt 1 then if bmc gt 0 then begin
						      widget_control,bmc,set_uvalue=$
						      {topic:topic,macro:macr,purpose:purp,author:auth,call:coll,more:more,logo:icof,link:lnk}
						      bid=widget_button(bmc  ,value='More...',font=ft_propor,uvalue=[-88,568,b_labins(7),bmc])
						      endif
			c=strpos(lon,'topic:')	 & if c ge 0 then begin topic=strmid(strtrim(strmid(lin,c+6,150),2),0,13)
						   if bmc gt 0 then $
						   bid  =widget_label (bu(1),value='---')
						   bid  =widget_label (bu(1),value=topic ,font=ft_b_normal)
						   macr='' & purp='' & auth='' & coll='' & bmc=0 & more=[''] & lnk='' & endif

			c=strpos(lon,'macro:')	 & if c ge 0 then begin
						   macr =strmid(strtrim(strmid(lin,c+6,150),2),0,13)
						   bmc  =widget_button(bu(1),value=macr  ,font=ft_b_normal,menu=2)
						   purp='' & auth='' & coll='' & more=[''] & lnk='' & endif

			c=strpos(lon,'purpose:') & if c ge 0 then begin purp =strmid(strtrim(strmid(lin,c+8,150),2),0,80)
						   if bmc gt 0 then  if purp gt ' ' then $
						   bid  =widget_button(bmc  ,value='Purpose: '+purp,font=ft_propor) & endif

			c=strpos(lon,'author:')	 & if c ge 0 then begin auth =strmid(strtrim(strmid(lin,c+7,150),2),0,80)
						   if bmc gt 0 then  if auth gt ' ' then $
						   bid  =widget_button(bmc  ,value='Author:  '+auth,font=ft_propor) & endif

			c=strpos(lon,'call:')	 & if c ge 0 then begin coll =strmid(strtrim(strmid(lin,c+5,150),2),0,80)
						   if bmc gt 0 then  if coll gt ' ' then $
						   bid  =widget_button(bmc  ,value='Call:    '+coll,font=ft_propor) & endif

			c=strpos(lon,'more:')	 & if c ge 0 then more=[more, strmid(strtrim(strmid(lin,c+5,150),2),0,80)]

			c=strpos(lon,'run:')	 & if c ge 0 then begin run  =strmid(strtrim(strmid(lin,c+4,150),2),0,150)
						   if bmc gt 0 then  if run gt ' ' then begin
						   widget_control,bmc,set_uvalue=run
						   bid  =widget_button(bmc  ,value='Run....',font=ft_propor,uvalue=[-88,569,b_labins(7),0,bmc])
						   endif & endif
						   
			c=strpos(lon,'logo:')	 & if c ge 0 then begin icof =strmid(strtrim(strmid(lin,c+5,60 ),2),0,40)
						   p=strpos(strlowcase(fifi),'a_list') & cof =''
						   if p gt 0 then cof=strmid(fifi,0,p) & icof=cof+icof & endif

			c=strpos(lon,'link:')	 & if c ge 0 then lnk=strmid(strtrim(strmid(lin,c+5,150),2),0,150)

			endwhile
						   if n_elements(more) gt 1 then if bmc gt 0 then begin
						      widget_control,bmc,set_uvalue=$
						      {topic:topic,macro:macr,purpose:purp,author:auth,call:coll,more:more,logo:icof,link:lnk}
						      bid=widget_button(bmc  ,value='More...',font=ft_propor,uvalue=[-88,568,b_labins(7),bmc])
						   endif

			misfifi:if out gt 0 then FREE_LUN,out
			end
		   ENDCASE
		   P_messi, tmpbase,bu(1)
		endif
	endif
;**LampINX
	if uv(1) eq 571	then if uv(2) eq 1 then inx $
			else if uv(2) eq 2 then qens_fit

;**Workspace operations.
	if uv(1) eq 572 then case uv(3) of
		1: begin W_store   ,/ALL
			 widget_control,uv(4),bad_id=ii,sensitive=1
			 widget_control,uv(5),bad_id=ii,sensitive=1
		   end
		2:	 W_restore ,/ALL
		3:	 W_exchange,/ALL
		4:	 W_clear   ,/ALL
		else:
		endcase
;**Idl help
	if uv(1) eq 573 then language_help

;**Create display function site UI.
	if uv(1) eq 574 then $
		 if lamp_fsite gt ' ' then  iii=execute(lamp_fsite) $
		 else widget_control,bad_id=i,l_message,set_value=  $
		 		    'Well, indeed, so, good .. need to be customized!'
;**Create access site UI.
	if uv(1) eq 575 then $
		 if lamp_asite gt ' ' then  iii=execute(lamp_asite) $
		 else widget_control,bad_id=i,uv(2)    ,set_value=  $
		 		    'Well, indeed, so, good .. need to be customized!'

;**Change Path
	if uv(1) eq 576 then if event.type lt 3 then begin my_path(1)='1'
			     if event.type eq 0 then if event.ch eq byte(10) then P_SET_PATH
			     endif
;**Get Run
	if (uv(1) eq 577) or (uv(1) eq 578) or (uv(1) eq 579) then begin

		widget_control,bad_id=i,uv(4),get_value=wnumber
		i =strpos (wnumber(0),'W')
		ws=strtrim(strmid(wnumber(0),i+1,4),2)
		wi=fix(ws)

		widget_control,bad_id=i,uv(3),get_value=runtxt
		runtxt=strtrim(strcompress(runtxt(0)),2)
		i =-1
		k =strpos(runtxt,'+') + $
		   strpos(runtxt,'-') + $
		   strpos(runtxt,':') + $
		   strpos(runtxt,'>')
		if k eq -4 then begin
		 i=strpos(runtxt,'.htm')
		 if i lt 0 then       i=strpos(runtxt,'LAMP.hdf')
		 if i lt 0 then       i=strpos(runtxt,'LAMP.xml')
		 if i lt 0 then       i=strpos(runtxt,'.nxs')
		 if i lt 0 then begin i=strpos(runtxt,'.hdf')
		                           ;if i gt 0 then if strlowcase(inst_group) ne 'nexus' then i=-1
					    endif
		 if i lt 0 then begin i=strpos(runtxt,'.xml')    & endif
		 if i lt 0 then begin i=strpos(runtxt,'.xdr')
		                            if i gt 0 then runtxt=strmid(runtxt,0,i)+'.htm'  & endif
		 if i lt 0 then begin i=strpos(runtxt,'.zip')
		                            if i gt 0 then runtxt=strmid(runtxt,0,i)+'.htm'  & endif
		 if i lt 0 then begin i=strpos(runtxt,'_LAMP')
		                            if i gt 0 then begin
				                           runtxt=strmid(runtxt,0,i)+'_LAMP'
		                                           bid   =sys_dep('POT',runtxt)      & endif
		 endif
		endif
		if i gt 0 then begin
		    j = strpos(runtxt,'{')
		    if (j gt 0) and (uv(1) eq 578) then begin
		 	k=strpos(runtxt,'}')
			strun =strtrim(string(strmid(runtxt,j+1,k-j-1)+1),2)
			runtxt=strmid (runtxt,0,j+1)+strun+'}'
        	    endif
		    READ_LAMP,runtxt, w=wi, path=path_for_online
		    widget_control,bad_id=i,uv(3),set_value=runtxt
		    if (uv(1) gt 577) then P_DID_EVENT,0,[-88,301,0,wi,0]
		    
		endif else begin  if (strpos(runtxt,'.gif') le 0) and $
		                     (strpos(runtxt,'.jpg') le 0) and $
		                     (strpos(runtxt,'.png') le 0) then GMY_run, uv,runtxt, wi $
		                  else READ_myGIF,path_for_online+runtxt,w=wi
		endelse
		if icoco eq 1 then begin wset,uv(6) & erase,255 & xx=1L & yy=1L & wr=1L
				   i=execute('xx = x' +ws) & i=execute('yy = y' +ws)
				   p_did_makeicon, ws,xx,yy, 192,192 ,0 ,wr ,'i'
		                  widget_control,bad_id=i,l_message,set_value='Read in W'+ws & endif
	endif
;**FIT
	if (uv(1) eq 580) then iii=execute('gfit')

;**Helps
	if (uv(1) ge 585) and (uv(1) le 598) then show_helps,uv
return
end

pro	GMY_run   ,uv ,runtxt ,wi
;**	*******
		 status=12
		 j = strpos(runtxt,'{')
		 if (j gt 0) and (uv(1) eq 578) then begin
		 	k=strpos(runtxt,'}')
			strun =strtrim(string(strmid(runtxt,j+1,k-j-1)+1),2)
			runtxt=strmid (runtxt,0,j+1)+strun+'}'
			widget_control,bad_id=i,uv(3),set_value=runtxt
        	 endif
        	 
		 i =rstrpos(runtxt,'.')
		 k = strpos(runtxt,'+') + $
		     strpos(runtxt,'-') + $
		     strpos(runtxt,':') + $
		     strpos(runtxt,'>')
		 
		 if (i lt 0) or ((j gt 0) and (k gt -4)) then begin
		    on_ioerror,misrun
		    if k gt -4 then begin
		         P_DID_GET_IT,runtxt              ,wi,status,uv ,'opr'
			 if ((uv(1) gt 577) and (status eq 0)) then P_DID_EVENT,0,[-88,301,0,wi,0]
		         return
		    endif
		    
		                         j =strpos(strupcase(runtxt),' SUMTO ')
		    if j lt 0 then begin j =strpos(runtxt,'>')
		       			 if j gt 0 then j=j+100 & endif
		    if j lt 0 then	 i =strpos(strupcase(runtxt), ' TO ')

		    if (i le 0) and (j le 0) then begin
		      k=0 & for i=0,strlen(runtxt)-1 do begin car   =strmid(runtxt,i,1)
		                if (car lt '0') or (car gt '9') then k=1   &  endfor
		      if k eq 0 then begin
		         run=float(runtxt)
		         run=long (run)
		         if (uv(1) eq 578) then begin
		 	     run   =run+1
		 	     runtxt=strtrim(string(run),2)
			     widget_control,bad_id=i,uv(3),set_value=runtxt
			     endif

		         status=1
		         P_DID_GET_IT,run              ,wi,status,uv ,'run'
			 
			 if ((uv(1) gt 577) and (status eq 0)) then P_DID_EVENT,0,[-88,301,0,wi,0]
		         return
		      endif
		    endif else if (uv(1) lt 578) then begin
		       cmd='w'+strtrim(string(wi),2)
		       if i gt 0 then begin
		          run1=float(strtrim(strmid(runtxt,0  ,i     ),2)) & run1=long (run1)
		          run2=float(strtrim(strmid(runtxt,i+4,lamp_6),2)) & run2=long (run2)
		          cmd =cmd+'=RDAND('
		       endif     else begin
		          run1=float(strtrim(strmid(runtxt,0  ,j),2))        & run1=long (run1)
		          if j lt 100 then k=6 else begin k=1 & j=j-100      & endelse
		          run2=float(strtrim(strmid(runtxt,j+k,lamp_6+1),2)) & run2=long (run2)
		          cmd =cmd+'=RDSUM('
		       endelse
		       cmd =cmd+strtrim(string(run1),2)+','+strtrim(string(run2),2)+',z0)'
		       z0  =-1
		       xicuter ,cmd
		       if z0 gt 0 then begin prt='Missing '  +string(z0)
			  if uv(2) gt 0 then widget_control,uv(2),bad_id=i,set_value=prt else print,prt
 		       endif
		       return
		    endif
		 endif
		 misrun:
		 P_DID_GET_IT,runtxt,wi,status,uv ,'fil'
		 if ((uv(1) gt 577) and (status eq 0)) then P_DID_EVENT,0,[-88,301,0,wi,0]
end

pro	SHOW_HELPS, uv
;**	**********
@lamp.cbk

	 if uv(1) eq 585 then widget_control,bad_id=i,lamp_hlp,map=0 $
	 else begin
           iii=xregistered('HELPS')
	   if (iii gt 0) then begin
	   	widget_control,bad_id=i,lamp_hlp,get_uvalue=uval
	   	if uval eq uv(1) then widget_control,bad_id=i,lamp_hlp,map=1
	   	if uval ne uv(1) then widget_control,bad_id=i,lamp_hlp,/destroy
	   	if uval ne uv(1) then iii=0
	   endif
	   if (iii le 0) then begin
	    formu='' & formt='' & ttl=''

	    if uv(1) eq 586 then begin
	    			 ttl='READING INTO WORKSPACES'
				 if (not sys_dep('RUNTIME')) and (not sys_dep('EMBEDDED'))   then begin
				    iii= execute('myhelp_'+strlowcase(inst_value)+',formu')
				    if n_elements(formu) le 1 then iii= execute('myhelp,formu') & endif
				 if n_elements(formu) le 1 then p_did_help,uv(1),formu,formt $
				 else ttl='MYHELP.PRO'
	    endif
	    if uv(1) eq 587 then begin
	    			 ttl='DISPLAYING THE WORKSPACES'
	    			 p_did_help,uv(1),formu,formt
	    endif
	    if uv(1) eq 588 then begin
	    			 ttl='A few TIPS (See INTERNAL in User Macros?)'
	    			 p_did_help,uv(1),formu,formt
	    endif
	    if uv(1) eq 589 then begin
	    			 ttl='CALLING OTHER DISPLAY FACILITIES'
	    			 p_did_help,uv(1),formu,formt
	    endif
	    if uv(1) eq 590 then begin
	    			 ttl='GK_FIT FITTING FACILITY'
	    			 gfit_help ,uv(1),formu,formt
	    endif
	    if uv(1) eq 591 then begin
	    			 ttl='SUPERPLOT : The Multipurpose Superpose Plotting Tool for Lamp'
	    			 p_did_help,uv(1),formu,formt
	    endif
	    if uv(1) eq 592 then begin
	    			 ttl='SELECTING RUNS'
	    			 p_did_help,uv(1),formu,formt
	    endif
	    if uv(1) eq 593 then begin
	    			 ttl='3 AXES FACILITY'
	    			 ii=execute('tx_help,uv(1),formu,formt')
	    endif
	    if uv(1) eq 594 then begin
	    			 ttl='SELECTING RUNS'
	    			 p_did_help,uv(1),formu,formt
	    endif
	    if uv(1) eq 595 then begin
	    			 ttl='PAD for Command Control'
	    			 p_did_help,uv(1),formu,formt
	    endif
	    if uv(1) eq 596 then begin
	    			 ttl='HELP for Instrument macros'
	    			 p_did_help,uv(1),formu,formt
	    endif
	    if uv(1) gt 596 then return

	    lamp_hlp=widget_base   (title='Lamp helps those who help themselves',$
	    			    resource_name='lamptouch',/column)
	    lamp_hls=widget_base   (lamp_hlp,/column)
	    lab	    =widget_base   (lamp_hls,/row)
		     put_logo	   ,lab
	    lab	    =widget_label  (lab     ,value=ttl,font=ft_biggest)

	    if n_elements(formu) ne n_elements(formt) then $
		blab=widget_text   (lamp_hls,value=formu,xsize=80,ysize=30,font=ft_b_normal,/scroll)$
	    else for i=0,n_elements(formu)-1 do begin
	         blab=widget_base   (lamp_hls,/row)
	         lab=widget_label  (blab    ,value=formu(i)    ,font=ft_b_bigger)
	         lab=widget_label  (blab    ,value=formt(i)    ,font=ft_normal)
	    endfor

	    base_wel=widget_button (lamp_hlp,value='Hide'      ,font=ft_b_normal ,$
	   			    uvalue=[-88,585,0])
   	    bid=sys_dep      ('DYNLAB',lamp_hlp,0)
	    widget_control,bad_id=i,lamp_hlp,group_leader=lamp_b1,set_uvalue=uv(1),/realize & put_logo
	    XMANAGER, 'HELPS' ,lamp_hlp ,event_handler='LAMP_EVENT_PARSER',/just_reg
	   endif
	 endelse
return
end

pro	P_SET_PATH,ppth
;**	**********
@lamp.cbk
	   my_path(1)=''

	   if n_elements(ppth) eq 1 then  path=[ppth] $
	   else widget_control,long(my_path(2)),bad_id=i,get_value=path
	   path=path(0)
	   stat=0
	   catch,stat
	   if stat ne 0 then begin
	        catch,/cancel
		P_MUS,'mus_cannon'
	   	path=path+'???'
		if n_elements(ppth) eq 0 then $
	        widget_control,long(my_path(2)),bad_id=i,set_value=path,SET_TEXT_SELECT=[strlen(path),0]
 	        print,string(7b)+path
	   	return
	   	endif

	   if n_elements(ppth) eq 0 then DON_WRITE_PROG_MAC ,0
	   cd,path
	   if n_elements(ppth) eq 1 then $
		widget_control,long(my_path(2)),bad_id=i,set_value=path,SET_TEXT_SELECT=[strlen(path),0]
	   my_path(0)=path
	   DON_INIT_INST_MACS ,1
	   DON_INIT_PROG_MAC  ,1
return
end

pro	P_FCT_CREATE ,base ,bas_geo2
;**	************
@lamp.cbk
	if GEORGE eq 1	then   base0      =lamp_ben(2) $
	else		begin  lamp_ben(4)=widget_base(base ,/frame,resource_name='ben')
			       base0      =widget_base(lamp_ben(4),/column)
	endelse
	base1=widget_button(base0,font=ft_normal   ,value=lamp_fsite          ,uvalue=[-88,574,0,0])
	b_labins(2) =base1
	base1=widget_button(base0,font=ft_normal   ,value=' Load new Colors  ',uvalue=[-88,347])
	base1=widget_button(base0,font=ft_normal   ,value='     GK_Fit       ',uvalue=[-88,580,0,0])
	lamp_don   =[lamp_don,base1]
	base1=widget_button(base0,font=ft_normal   ,value='    SuperPlot     ',uvalue=[-88,352])
	lamp_don   =[lamp_don,base1]

	if GEORGE eq 2 then begin
	  ;base1=widget_button(bas_geo2,font=ft_normal   ,value=' Load new Colors  ',uvalue=[-88,347])     ;removed for george=2
	  ;base1=widget_button(bas_geo2,font=ft_normal   ,value='     GK_Fit       ',uvalue=[-88,580,0,0]) ;removed for george=2
	  ;base1=widget_button(bas_geo2,font=ft_normal   ,value='    SuperPlot     ',uvalue=[-88,352])     ;removed for george=2
	endif

	if GEORGE eq 1 then begin
	  bid=widget_button(base0,font=ft_normal   ,value='      ------      ')
;	  bid=widget_button(base0,font=ft_normal   ,value='   Dial Macros?   ',uvalue=[-88,203,0,0])
;	  bid=widget_button(base0,font=ft_normal   ,value='   The Journal    ',uvalue=[-88,396,0,0])
	  bid=widget_button(base0,font=ft_normal   ,value='   Data Params    ',uvalue=[-88,204,0,0])
	  if sys_dep('VERSION') ge 4.0 then widget_control,base0,/destroy
	endif

	if (lamp_siz ge 800) and (GEORGE ne 1) then begin
	if abs(sys_dep('MAP')) ne 1 then baba=base0 else baba=base
	brow =widget_base  (baba ,/row)
	lamp_don   =[lamp_don,brow]
	bs1bs=widget_button(brow ,font=ft_normal   ,value='SCAN W 1'          ,uvalue=[-88,306,0,0])
	if sys_dep('MAP') ne -1 then $
	bs1b1=widget_button(brow ,font=ft_smallest ,value='<',resource_name='discret') else $
	bs1b1=widget_button(brow ,font=ft_smallest ,value='<')
	if sys_dep('MAP') ne -1 then $
	bs1b2=widget_button(brow ,font=ft_smallest ,value='>',resource_name='discret') else $
	bs1b2=widget_button(brow ,font=ft_smallest ,value='>')
	widget_control,bad_id=i,bs1b1,set_uvalue=[-88,310,bs1bs,3,0,0,0,0,0]
	widget_control,bad_id=i,bs1b2,set_uvalue=[-88,311,bs1bs,3,0,0,0,0,0]
	endif

	if GEORGE eq 1 then begin GEORGEO, CONSTRUCT=base   &   base0=base & endif
	if GEORGE eq 2 then       GEORGEO, CONSTRUCT=lamp_ben(8)

	if GEORGE ne 1 then begin
	   lamp_ben(5)=widget_base  (base ,/frame,resource_name='ben')
	   base0=widget_base  (lamp_ben(5),/column)
	   if  (lamp_siz gt 950) or $
	      ((lamp_siz ge 900) and (sys_dep('MACHINE') eq 'mac')) then begin
      		      w0=2 & LOGO,w0 & pax1=size(w0)
      		      lamp_ben(6)=widget_draw (base0,retain=2,xsize=pax1(1),ysize=pax1(2),/button_event)
	   endif else begin  lamp_ben(3)=widget_label(base ,font=ft_smallest,value=' ' )
			     if lamp_siz lt 800  then put_logo,base
	   endelse
	   base1=widget_button(base0,font=ft_normal,value='    The Manual    '     ,uvalue=[-88,201,0])
	endif

	  base1=widget_button(base0      ,font=ft_normal,value='SAVE this Session ',uvalue=[-88,397])
	  lamp_don   =[lamp_don,base1]
	  base1=widget_button(base0      ,font=ft_normal,value='  SWITCH  OFF    ' ,uvalue=[-88,398])
	if GEORGE eq 2 then begin
	  base1=widget_button(lamp_ben(8),font=ft_normal,value='SAVE this Session ',uvalue=[-88,397])
	  base1=widget_button(lamp_ben(8),font=ft_normal,value='  SWITCH  OFF    ' ,uvalue=[-88,398])
	endif
return
end

pro MIC ,nocre
;** ***
;**
@lamp.cbk
common okitis, yo

	if n_elements(yo)    eq 0 then begin
	        keep_p=path_for_online & keep_i=inst_value  & keep_c=cycle
	   	P_RESTORE,lamp_dir+lamp_dvd+'mics.exe' ,cnt & ii=execute("MICS")
	   	yo=1
		ii=execute("P_MIC_CREATE ,0 ,'just'")
	        path_for_online=keep_p & inst_value=keep_i & cycle=keep_c
	endif

	if n_elements(nocre) eq 0 then begin
           i=xregistered('MIC')
	   if i gt 0 then widget_control,bad_id=i,lamp_mic,map=1 $
	   else begin
	        lamp_mic =widget_base (title='LAMP Data Instrument Access',resource_name='lampmic')
	   	ii=execute("P_MIC_CREATE ,(lamp_mic+0)")
   		bid=sys_dep      ('DYNLAB',lamp_mic,1)
		widget_control,bad_id=i   ,lamp_mic,group_leader=lamp_b1,/realize
		XMANAGER, 'MIC' ,lamp_mic ,event_handler='LAMP_EVENT_PARSER',/just_reg
	   endelse
	endif
return
end

pro	P_MUS  ,file
;**	*****
;**
@lamp.cbk
common  c_mus  ,mus_driv,mus_id,mus_fils

        if n_elements(mus_driv) lt 1 then begin
	   mus_driv =  ''
	   mus_id   =  0
	   if lamp_loc eq 1 then begin
		mus_driv=sys_dep      ('PLAYER')
;		cd,lamp_dir,current=mee
;		mus_fils=findfile     ('mus_*')
;		cd,mee
	   endif
        endif

        if n_elements(b_labins) ge 4 then if b_labins(3) eq 2 then mus_driv=''
	if mus_driv ne '' then begin
	   if file  eq '' then begin if mus_id gt 0 then bid=sys_dep      ('PLAY_OF',0,0,0,mus_id)
	   			        mus_id =0
	   endif else bid=sys_dep      ('PLAY_ON',mus_driv,lamp_dir,file,mus_id)
	endif
	if strpos(file,'cannon') ge 0 then p_tremble
return
end

pro	MANUAL ,res
;**	******
;**
@lamp.cbk
	res=''
	if lamp_dir eq '' then man_dir=sys_dep('HOME') else man_dir=lamp_dir
	res=sys_dep      ('MANUAL',man_dir,lamp_macro)
	if l_message gt 0 then widget_control,bad_id=iii,l_message,set_value=$
			 'See '+res
end

pro	P_MESSI , base ,topb
;**	*******
;**
@lamp.cbk
map=abs(sys_dep('MAP'))
if map eq 0 then return

if map eq 1 then begin
   if base  le 0 then begin
	     base =widget_base  ( title='Lamp')
	     bid  =widget_label ( base,value='LAMP RECONSTRUCTION ...',font=ft_b_bigger)
	     widget_control,topb ,bad_id=i,map=0
	     widget_control,base ,group_leader=topb   ,bad_id=i,/realize
   endif   else  begin
	     widget_control,base ,bad_id=i,/destroy
	     widget_control,topb ,bad_id=i,map=1
   endelse
endif
if map eq 2 then begin
   if base  le 0 then begin
   	    base=1 &  widget_control,bad_id=i,topb,UPDATE=0
   endif else 	      widget_control,bad_id=i,topb,UPDATE=1
endif
return
end

pro 	dynlabel_call, w
;**	*************
        type = WIDGET_INFO(w, /TYPE)
;       IF ((type EQ 1) OR (type EQ 5)) THEN BEGIN
        IF ((type EQ 1))  THEN BEGIN
                WIDGET_CONTROL, /DYNAMIC_RESIZE, w
        ENDIF ELSE IF (type EQ 0) THEN BEGIN
                child = WIDGET_INFO(W, /CHILD)
                WHILE (child NE 0) DO BEGIN
                  DYNLABEL_CALL, CHILD
                  CHILD = WIDGET_INFO(CHILD, /SIBLING)
                ENDWHILE
        ENDIF
END

pro	resizeButton_call, w ,val
;**	*****************

        type = WIDGET_INFO(w, /TYPE)
        IF (type EQ 1) THEN BEGIN
            geo = WIDGET_INFO(w, /GEOMETRY)
            WIDGET_CONTROL, w, XSIZE=(geo.scr_xsize+val)>15

        ENDIF ELSE IF (type EQ 0) THEN BEGIN
            child = WIDGET_INFO(W, /CHILD)
            WHILE ( child NE 0 ) DO BEGIN
            	    resizeButton_call, child,val
            	    child = WIDGET_INFO(child, /SIBLING) & ENDWHILE
        ENDIF
END

pro	P_DYING,id
;**	*******
@lamp.cbk
	if l_message eq 0 then exok=1 else exok=0
	lamp_b1 =0 ;<---  IMPORTANT
	if GEORGE  ne 0 then WebOff
	P_MUS,'mus_cannon'
	DON_WRITE_PROG_MAC ,1
	DID_WRITE_JOURNAL
	wait,.3 & EMPTY
	if (sys_dep('EMBEDDED')  or sys_dep('RUNTIME')) then EXIT
	rout=[0]
	if  sys_dep('VERSION') ge 5.1 then ii=execute('rout=widget_info(/managed)')
	if  rout(0)  gt 0 then exok=0 else exok=1
	if (sys_dep('MACHINE') eq 'unix') and (exok)    then EXIT
return
end

pro	P_LAMP_STOP
;**	***********
@lamp.cbk
@dons.cbk
	   stat=0
	   catch,stat
	   if stat eq 0 then widget_control,bad_id=i,lamp_b1,show=0
	   if stat eq 0 then bid=sys_dep('AFTES')
	   if stat eq 0 then print,string(7b),' Type RETALL & LAMP to continue'
	   if stat eq 0 then stop
return
end

pro	P_LAMP_INIT, lamp_size=lps, george=geo
;**	***********
@lamp.cbk
	if n_elements(lps) eq 1 then lamp_size=lps
	if n_elements(geo) eq 1 then george   =geo
return
end

pro	P_DO_THAT
;**	*********
;**
@lamp.cbk
 lamd_dir =           sys_dep('GETENV','LAMP_DIR' )
 lamd_wind=strlowcase(sys_dep('GETENV','LAMP_WIND'))

 while strpos(!path,'..') ge 0 do begin
    i1=strpos(!path,'..')
    if strpos(!path,'..\..') ge 0 then j1=5  else j1=2
    if strpos(!path,'../..') ge 0 then j1=5
    if i1 gt 0 then deb=strmid(!path,0,i1-1) else deb=''
    !path=deb+lamd_dir +strmid(!path,i1+j1,300)
 endwhile

 cd,current=mee
 if  strtrim  (!path,2)    eq '.'  then !path=mee else $
 while (strpos(!path,'.\') ge 0) or (strpos(!path,'./') ge 0) do begin
    i1= strpos(!path,'.\') & if i1 lt 0 then i1=strpos(!path,'./')
    if i1 gt 0 then deb=strmid(!path,0,i1-1) else deb=''
    !path=deb+ mee +strmid(!path,i1+1,300)
 endwhile

 if  strtrim  (!dir,2)     eq '.'  then !dir =mee

 catch,stat & if stat eq 0 then begin cd,!dir+sys_dep('DIVIDER')+".."         & cd,current=meelam
 				      cd,meelam+sys_dep('DIVIDER')+"lamp_mac" & cd,current=meemac
				      lamd_dir=meelam
				      bid=sys_dep("ADDPATH",meemac)
				      bid=sys_dep("ADDPATH",meelam) & lamp_dir=meelam
 				endif
 catch,stat & if stat eq 0 then begin pth=sys_dep("NEWSUB",lamd_dir,"work") & cd,pth & endif
 if sys_dep('MACHINE') eq 'unix' then cd,mee

 if strpos (!path,"lamp_mac") le 0 then begin
    pth  =sys_dep("NEWSUB" ,lamd_dir,"lamp_mac")
    bid  =sys_dep("ADDPATH",pth)
    endif

 if (!D.flags and 65536)   eq 0 then set_plot,'TEK' else $
 if strpos(lamd_wind,'nw') ge 0 then set_plot,'Z'
 if  sys_dep('STUDENT')                then lamp_ziz=600
 if (strpos(lamd_wind,'small'  ) ge 0) then lamp_ziz=480
 if (strpos(lamd_wind,'medium' ) ge 0) then lamp_ziz=600
 if (strpos(lamd_wind,'large'  ) ge 0) then lamp_ziz=800
 if (strpos(lamd_wind,'wide'   ) ge 0) then lamp_ziz=1024
 GEORGE=2
 if (strpos(lamd_wind,'lamp')    ge 0) then GEORGE  =0
 if (strpos(lamd_wind,'geo')     ge 0) then GEORGE  =1
 if (strpos(lamd_wind,'lampgeo') ge 0) then GEORGE  =2
 if (strpos(lamd_wind,'geolamp') ge 0) then GEORGE  =2
 if (strpos(lamd_wind,'full')    ge 0) then GEORGE  =3
end

pro     SL_RESTSCAN, file, cnt
;**     ***********
;**
@lamp.cbk
		P_RESTORE,file ,cnt
		if cnt eq 0  then  P_RESTORE,!dir+lamp_dvd+'lib'+lamp_dvd+'hook'+lamp_dvd+'scan.sav' ,cnt
		if cnt gt 0  then  begin
		   	sl_lampscan, 'test' ,did_scan,tso
		   	if did_scan ge 0 then ii=execute('scan,1') else cnt=0
      	endif
end
pro     SL_SCANLOAD, p1,p2
;**     ***********
;**
	if p1 eq -1 then begin
			   VV =strtrim(string(sys_dep('VERSION')),2)
			   VV =strmid (VV,0,1)+strmid (VV,2,1)
			   pth=sys_dep("NEWSUB" ,p2,"lamp_mac")
			   SL_RESTSCAN,pth+'scan'+VV+'.sav' ,cnt
			   if cnt le 0 then p1=-2 else p1=1
	endif
end
pro   LANGUAGE_HELP
;**   *************
	  if sys_dep('VERSION') ge 5.0 then ii=execute('online_help' ) $
	                               else ii=execute('man_proc,"?"')
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; Procedures for Mini-Lamp ****** END OF LAMP_UPD.PRO
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


;****************************** B.A.R.N.S interfaces **************************************
;****************************** B.A.R.N.S interfaces **************************************
;****************************** B.A.R.N.S interfaces **************************************
pro BARNS_R ,remove=rem
;** *******
@lamp.cbk
@dons.cbk
common  for_users
	fil=findfile('lamp.ses',count=true)
	if  true gt 0 then begin
			P_RESTORE, 'lamp.ses', cnt
			AFTER_RESTORE
			if keyword_set(rem) then bid=sys_dep('DELET','lamp.ses')
			print,'Previous Session is restored ....'
	endif
end
function BARNS_I	,val
;** **** *******
;**
common c_barns  ,wk_read ,wk_plot ,wk_year ,wk_raw ,wk_rot ,wk_repi ,wk_repc ,wk_reps $
		,wk_fu   ,wk_fil  ,wk_reg  ,wk_log ,wk_wr  ,wk_xr   ,wk_yr   ,wk_kef  $
		,wk_bx   ,wk_by   ,wk_bw   ,wk_fx  ,wk_fy  ,wk_fz   ,wk_px   ,wk_py   $
		,wk_pz   ,wk_save ,wk_fmt  ,wk_ins ,wk_cyc

eff=0
txt=strlowcase(val)
k  =strpos(txt,',	"')
if k gt 0 then begin
	if n_elements(wk_raw) eq 0 then begin
			 wk_raw =0		 & wk_fil ='Gif'	& wk_save= 1
			 wk_repi=''		 & wk_repc=''		& wk_plot='1'
			 wk_reps=''	 	 & wk_fu  =',/below'	& wk_fmt = 3
			 wk_reg =',regular=0'	 & wk_rot ='30'
			 wk_log =',log=0'        & wk_wr  = 0.
			 wk_xr	= [0.,0.]	 & wk_yr  = [0.,0.]	& wk_year=''
			 wk_bx  =  0		 & wk_by  =  0		& wk_bw  =0
			 wk_fx  =  0		 & wk_fy  =  0		& wk_fz  =0
			 wk_px  =  0		 & wk_py  =  0		& wk_pz  =0
			 wk_kef =  0		 & wk_ins = ''		& wk_cyc =''
			 endif
	eff=1
	txt=strmid(txt,0,k)	& cmd=strmid(val,k+2,100)
	ln =strlen(cmd)		& cmv=strmid(cmd,1,ln-2)

	CASE txt of
	"set_inst":if cmd ne wk_ins then begin XICUTER,'RDSET,inst='+cmd
			 wk_ins=cmd
			 cmv=strlowcase(cmv) & iii=EXECUTE('myinit_'+cmv)
			 hhh='' & iii=execute('myhelp_'+cmv+ ',hhh') & nn=n_elements(hhh)
			 if nn gt 1 then begin on_ioerror,misop      & u =0
					 openw ,u,'help_'+cmv+'.htm',/get_lun
					 printf,u,'<HTML><PRE>'
					 for i=0,nn-1 do printf,u,hhh(i)
					 printf,u,'</PRE></HTML>'
					 misop: if u gt 0 then free_lun,u & endif
	           endif
	"set_base":if wk_ins+wk_year+cmd ne wk_cyc then begin
			 wk_cyc =wk_ins +wk_year+cmd
			 IF strpos(cmd,'Cycle -') lt 0 then $
			 XICUTER	,'RDSET,base='+cmd else $
			 XICUTER	,'RDSET,base="C_Year '+wk_year+'",cycle='+strmid(wk_year,2,2)+strmid(cmv,7,1)
	           endif
	"set_year":	 wk_year	=	cmv
	;**********
	"do_filt" :begin on_ioerror,fsx_err  &  rdtmp=[0.,0.]	& reads	 , cmv+' 0 0',rdtmp
			 RDFILTER, xrange=rdtmp			& fsx_err:
			 if wk_fx eq 0 then RDFILTER,xrange=[0,0]
			 if wk_fy eq 0 then RDFILTER,yrange=[0,0]
			 if wk_fz eq 0 then RDFILTER,zrange=[0,0]
			 if wk_px eq 0 then RDFILTER,xproj = 0
			 if wk_py eq 0 then RDFILTER,yproj = 0
			 if wk_pz eq 0 then RDFILTER,zproj = 0
			 wk_kef=wk_fx + wk_fy + wk_fz + wk_px + wk_py + wk_pz
			 wk_fx =0 & wk_fy =0 & wk_fz =0 & wk_px =0 & wk_py =0 & wk_pz =0
			 end
	"r_scly"  :begin on_ioerror,fsy_err  &  rdtmp=[0.,0.]	& reads	 , cmv+' 0 0',rdtmp
			 RDFILTER, yrange=rdtmp			& fsy_err: & end
	"r_sclz"  :begin on_ioerror,fsz_err  &  rdtmp=[0.,0.]	& reads	 , cmv+' 0 0',rdtmp
			 RDFILTER, zrange=rdtmp			& fsz_err: & end
	"r_chkx"  :	 wk_fx = 1
	"r_chky"  :	 wk_fy = 1
	"r_chkz"  :	 wk_fz = 1
	"r_prjx"  :begin RDFILTER,/xproj & wk_px = 1 & end
	"r_prjy"  :begin RDFILTER,/yproj & wk_py = 1 & end
	"r_prjz"  :begin RDFILTER,/zproj & wk_pz = 1 & end
	;**********
	"do_read" :IF cmv gt " " then begin
			 IF wk_raw then RDSET,/raw else RDSET,/default
			 ffl=strpos(cmv,':')
			 if (wk_kef ge 1) or (ffl ge 1) then begin
						   if wk_raw then RDFILTER,monimod=2 $
			 				     else RDFILTER,monimod=1
			 		RDFILTER,selection=cmv,wksp=fix(wk_read)
			 endif else	GMY_run ,[0,0,0,0],cmv,     fix(wk_read)
			 wk_raw		=	0	 & wk_plot=wk_read  & ENDIF
	"r_raw"   :	 wk_raw		=	1
	"set_wks" :	 wk_read	=	cmv
	;**********
	"do_save" :	 WRITE_LAMP	,	cmv ,w=wk_save ,fmt=wk_fmt
	"s_wks"   :	 wk_save	=   fix(cmv)
	"s_fmt"   :	 wk_fmt		=   fix(cmv)
	;**********
	"do_plot" :begin wk_plot	=	cmv
			 if wk_bx eq 0 then wk_xr(*)=0. & if wk_by eq 0 then wk_yr(*)=0.
			 if wk_bw eq 0 then wk_wr   =0.
			 range=strcompress(',xrange=['+string(wk_xr(0))+','+string(wk_xr(1))+'],'+$
			 		    'yrange=['+string(wk_yr(0))+','+string(wk_yr(1))+'],'+$
			 		    'zlim='   +string(wk_wr),/remove_all)
			 cmd = 'SEEM, rot='+wk_rot +wk_reg +wk_fu +wk_log +range
			 XICUTER	,	cmd
			 if wk_fil eq 'Wrl'  then begin wk_reps=',/surface' & wk_repi='' & wk_repc=''
						  fil=',/vrml'  & endif else $
			 if wk_fil eq 'Ps'   then fil=',/pscript' else $
			 if wk_fil eq 'Java' then fil=',/htm'     else fil=''
			 cmd = 'SEE, w=' +wk_plot +wk_repi +wk_repc +wk_reps +fil
			 XICUTER	,	cmd
			 wk_repi	=	''		& wk_repc = ''
			 wk_reps	=	''		& wk_log  = ',log=0'
			 wk_reg		=	',regular=0'	& wk_wr   = 0.
			 wk_xr(*)	=	 0.		& wk_yr(*)= 0.
			 wk_bx		= 	 0		& wk_by   = 0	   & wk_bw = 0
		   end
	"d_fil"   :	 wk_fil		=	cmv
	"d_repi"  :	 wk_repi	=	',/image'
	"d_repc"  :	 wk_repc	=	',/contour'
	"d_reps"  :	 wk_reps	=	',/surface'
	"d_repr"  :	 wk_reg		=	',regular=1'
	"d_angl"  :begin on_ioerror,rot_err
			 wk_rot		=   	string(fix(cmv)) & rot_err: & end
	"d_befu"  :	 wk_fu		=	cmv
	"d_sclx"  :begin on_ioerror,chx_err
			 reads		,	cmv+' 0 0',wk_xr & chx_err: & end
	"d_scly"  :begin on_ioerror,chy_err
			 reads		,	cmv+' 0 0',wk_yr & chy_err: & end
	"d_sclw"  :begin on_ioerror,chw_err
			 reads		,	cmv+' 0 0',wk_wr & chw_err: & end
	"d_chkl"  :	 wk_log		=	',log=1'
	"d_chkx"  :	 wk_bx		=	1
	"d_chky"  :	 wk_by		=	1
	"d_chkw"  :	 wk_bw		=	1
	;**********
	"do_cmd"  :	 XICUTER	,	cmv
	"submit"  :CASE  cmv of
		   "The Journal": DID_WRITE_JOURNAL ,/htm
		   "Parameters" : DID_PARAM_HTM     ,wk_plot
		   "The Manual" :
		   else:
		   ENDCASE
	;**********
	"do_color":	 setcol		,   fix(cmv)
	;**********
	 else     :	 eff=0
	ENDCASE
endif
return,eff
end

PRO LAMP_B
;** ******
@lamp.cbk

if  b_labins(3) le 0 then begin
 if (strpos(strlowcase(sys_dep('GETENV','LAMP_WIND')),'nws') ge 0) or (!D.Name eq 'Z')  then begin
		  BARNS_R ,/remove & b_labins(3) = 2 & SET_PLOT,"Z"
 endif else begin BARNS_R	   & b_labins(3) = 1 & endelse
 P_DON_INIT_VAR
 P_DID_SETVAR
 P_DATA_IDOL
 RDFILTER
 GEORGEO,/nowin
 lamp_focus =-1
endif

ON_IOERROR,NO_MATTER

if b_labins(3) eq 1 then begin
 print,' **********************'
 print,' * Remember INTERNALS *'
 print,' **********************'
 print,'Available Bases       : ',lamp_ali(*)+','
 print,''
 print,'To set your instrument : RDSET   ,inst="D20"'
 print,'To set your base       : RDSET   ,base="C_Year 1996" ,cycle=964'
 print,'To use tektro,Win,X,Mac: SET_PLOT,"TEK" or "WIN" or "X" or "MAC"'
 print,'Actual display is '+!D.Name+' , Instrument is '+inst_value+' , Base is '+path_for_online
 print,''
endif

CATCH,stat & if stat ne 0 then print,!err_string
text=''
	while (1) do begin
		READ,'Lamp> ',text    &  text=strtrim(text,2)
		NO_MATTER: IF text ne '' then begin ;if b_labins(3) eq 2  then print,'Barns-sent'
						     if not barns_i(text) then XICUTER,text
						    ;if b_labins(3) eq 2  then print,'Barns-done'
						     ENDIF
	endwhile
return
end


PRO LAMP ,just ,NW=nw ,GEO=geo ,SMALL=small ,MEDIUM=medium ,LARGE=large ,WIDE=wide ,GEOLAMP=geolamp ,LAMPGEO=lampgeo ,LAMP=lamp ,FULL=full
;** ****
@lamp.cbk
    !quiet=1
    if  sys_dep ('VERSION') ge 5.1 then begin rout=['']
	ii=execute('rout=routine_info(/functions)') & dx=where(rout eq 'RLAMP')
	if dx(0) lt 0 then RESOLVE_ROUTINE,['dons','dids','bens','scan']
    endif
    if n_elements (GEORGE)  eq 0 then GEORGE  =2
    if keyword_set(LAMP)         then GEORGE  =0
    if keyword_set(GEO)          then GEORGE  =1
    if keyword_set(LAMPGEO)      then GEORGE  =2
    if keyword_set(GEOLAMP)      then GEORGE  =2
    if keyword_set(FULL)         then GEORGE  =3
    if  sys_dep   ('STUDENT')    then lamp_ziz=600
    if keyword_set(SMALL)        then lamp_ziz=480
    if keyword_set(MEDIUM)       then lamp_ziz=600
    if keyword_set(LARGE)        then lamp_ziz=800
    if keyword_set(WIDE)         then lamp_ziz=1024
    if (!D.flags and 65536) eq 0 then if (!D.Name ne 'Z') then set_plot,'TEK'
    if keyword_set(NW)           then LAMP_,   'just'
    if keyword_set(NW)           then LAMP_B    else   LAMP_,just
    return
    end
