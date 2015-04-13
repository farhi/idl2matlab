;pro scan_common
;** ***********
;@scan_com1.cbk
;@scan_com2.cbk
;end
;************************ SL_FUNC.NEW ****************************************
;************************ SL_FUNC.NEW ****************************************
;************************ SL_FUNC.NEW ****************************************
;
;
;****************************************************** SYS
;
function sl_sysget ,flg , val
;*******
;**
;**	Get variables.
;**	--- ---------
case flg of
	19:if sys_dep('VERSION') ge 5.0 then val=!Mouse.button else val = !err
	20:val = !c
	29:!err= val
	else:
endcase
return,1
end
;
function sl_cvsiz,	vsiz
;******* ********
;**
common my_funct, i,bb,ab
;**
	i=vsiz(vsiz(0)+1)
	if i eq  1 then i= 2 else if i eq  2 then i= 4 else $
	if i eq  3 then i=16 else if i eq  4 then i= 8 else $
	if i eq  5 then i=32 else if i eq  6 then i=64 else $
	if i eq  7 then i= 1 else if i eq  8 then i=80
	vsiz(vsiz(0)+1)=i
	return,vsiz
end
;
;
function sl_size,	area
;******* *******
;**
	return,sl_cvsiz(size(area))

end
;
;
function sl_getsym,	str
;******* *********
;**
	return, getenv (str)
end
;
;
function sl_getlog,	str,n
;******* *********
;**
common my_funct, i,bb,ab
;**
bb=1
	n=sys_dep('GETENV',str)
	if n eq '' then bb=0
return, bb
end
;
;
function sl_help,	data
;******* *******
;**
	help,		data
return,1
end
;
function sl_prompt,	str
;******* *********
;**
	prompt,		str
return, 1
end
;
;
function sl_execute,	n,i_rout,in_are,i_ps1,i_ps2,i_ps3,pcur,typ
;******* **********
;**
	common machin,	mc_sys,mc_sta
;**
bb=1
	if mc_sys eq 'vms'  then bb =execute("n="+i_rout+ $
			"(in_are,i_ps1,i_ps2,i_ps3,pcur,typ)") $
	else 	n=call_function(i_rout,in_are,i_ps1,i_ps2,i_ps3,pcur,typ)
return, bb
end
;
function sl_run,	cmd, str,ext,vers ,flg
;******* ******
;**
	common machin,	mc_sys,mc_sta
;**
	if  cmd eq 'd' then begin
	 if ext ne ''  then stre=str+'.'+ext else stre=str
	 if mc_sys eq 'vms' then begin
	   if vers eq 0  then spawn, 'Delete ' +stre+';*',/nowait else $
			      spawn, 'Delete ' +stre+';0',/nowait

	 endif else bid=sys_dep('DELET',stre)
	endif
return, 1
end
;
function sl_callt,	rout,file,p1,p2
 ;******* ********
;**
	common machin,	mc_sys,mc_sta
;**
	if mc_sys eq 'vms' then $
	bb=call_vms(file,rout,p1,p2)
return, 1
end
;
function sl_calll,	rout,file,p1,p2,p3,p4,p5,p6
;******* ********
;**
	common machin,	mc_sys,mc_sta
;**
	if mc_sys eq 'vms' then return,call_vms(file,rout,p1,p2,p3,p4,p5) $
			   else return,call_external(file,rout,p1,p2,p3,p4,p5,p6)
end
;
function sl_element,	area
;******* **********
;**
	return,n_elements(area)
end
;
function sl_sarr,	nd,d1,d2,d3,d4
;******* ******
;**
on_error,2
;%ow%	if nd lt 0 then return, strarr(d1)	 	else $
;%ow%	if nd eq 2 then return, strarr(d1,d2)	 	else $
;%ow%	if nd eq 3 then return, strarr(d1,d2,d3) 	else $
;%ow%	if nd eq 4 then return, strarr(d1,d2,d3,d4)
	if nd lt 0 then return, make_array(dimension=d1,string=1)	else $
	if nd eq 2 then return, make_array(d2          ,string=1)	else $
	if nd eq 3 then return, make_array(d2,d3       ,string=1)	else $
	if nd eq 4 then return, make_array(d2,d3,d4    ,string=1)
return, 0
end
;
function sl_barr,	nd,d1,d2,d3,d4
;******* ******
;**
on_error,2
;%ow%	if nd lt 0 then return, bytarr(d1)	 	else $
;%ow%	if nd eq 1 then return, bytarr(d1)	 	else $
;%ow%	if nd eq 2 then return, bytarr(d1,d2)	 	else $
;%ow%	if nd eq 3 then return, bytarr(d1,d2,d3) 	else $
;%ow%	if nd eq 4 then return, bytarr(d1,d2,d3,d4)
	if nd lt 0 then begin   if nd eq -2 then $
	   if d1(1) eq 1 then return,reform(make_array(dimension=d1,byte=1),d1) $
		   else	return, make_array(dimension=d1,byte=1) $
		   else	return, make_array(dimension=d1,byte=1)
	   endif   else $
	if nd eq 1 then return, make_array(d1          ,byte=1)	else $
	if nd eq 2 then begin
	   if d2 eq 1 	then return,reform(make_array(d1,d2,byte=1),d1,d2) $
		   else	return, make_array(d1,d2       ,byte=1)
	   endif   else $
	if nd eq 3 then return, make_array(d1,d2,d3    ,byte=1)	else $
	if nd eq 4 then return, make_array(d1,d2,d3,d4 ,byte=1)
return, 0
end
;
function sl_iarr,	nd,d1,d2,d3,d4
;******* ******
;**
on_error,2
;%ow%	if nd lt 0 then return, intarr(d1)	 	else $
;%ow%	if nd eq 1 then return, intarr(d1)	 	else $
;%ow%	if nd eq 2 then return, intarr(d1,d2)	 	else $
;%ow%	if nd eq 3 then return, intarr(d1,d2,d3) 	else $
;%ow%	if nd eq 4 then return, intarr(d1,d2,d3,d4)
	if nd lt 0 then begin   if nd eq -2 then $
	   if d1(1) eq 1 then return,reform(make_array(dimension=d1,int=1),d1) $
		   else	return, make_array(dimension=d1,int=1) $
		   else	return, make_array(dimension=d1,int=1)
	   endif   else $
	if nd eq 1 then return, make_array(d1          ,int=1)	else $
	if nd eq 2 then begin
	   if d2 eq 1 	then return,reform(make_array(d1,d2,int=1),d1,d2) $
		   else	return, make_array(d1,d2       ,int=1)
	   endif   else $
	if nd eq 3 then return, make_array(d1,d2,d3    ,int=1)	else $
	if nd eq 4 then return, make_array(d1,d2,d3,d4 ,int=1)
return, 0
end
;
function sl_larr,	nd,d1,d2,d3,d4
;******* ******
;**
on_error,2
;%ow%	if nd lt 0 then return, lonarr(d1)	 	else $
;%ow%	if nd eq 1 then return, lonarr(d1)	 	else $
;%ow%	if nd eq 2 then return, lonarr(d1,d2)	 	else $
;%ow%	if nd eq 3 then return, lonarr(d1,d2,d3) 	else $
;%ow%	if nd eq 4 then return, lonarr(d1,d2,d3,d4)
	if nd lt 0 then begin   if nd eq -2 then $
	   if d1(1) eq 1 then return,reform(make_array(dimension=d1,long=1),d1) $
		   else	return, make_array(dimension=d1,long=1) $
		   else	return, make_array(dimension=d1,long=1)
	   endif   else $
	if nd eq 1 then return, make_array(d1          ,long=1)	else $
	if nd eq 2 then begin
	   if d2 eq 1 	then return,reform(make_array(d1,d2,long=1),d1,d2) $
		   else	return, make_array(d1,d2       ,long=1)
	   endif   else $
	if nd eq 3 then return, make_array(d1,d2,d3    ,long=1)	else $
	if nd eq 4 then return, make_array(d1,d2,d3,d4 ,long=1)
return, 0
end
;
function sl_farr,	nd,d1,d2,d3,d4
;******* ******
;**
on_error,2
;%ow%	if nd lt 0 then return, fltarr(d1)	 	else $
;%ow%	if nd eq 1 then return, fltarr(d1)	 	else $
;%ow%	if nd eq 2 then return, fltarr(d1,d2)	 	else $
;%ow%	if nd eq 3 then return, fltarr(d1,d2,d3) 	else $
;%ow%	if nd eq 4 then return, fltarr(d1,d2,d3,d4)
	if nd lt 0 then begin   if nd eq -2 then $
	   if d1(1) eq 1 then return,reform(make_array(dimension=d1,float=1),d1)$
		   else	return, make_array(dimension=d1,float=1) $
		   else	return, make_array(dimension=d1,float=1)
	   endif   else $
	if nd eq 1 then return, make_array(d1          ,float=1)	else $
	if nd eq 2 then begin
	   if d2 eq 1 	then return,reform(make_array(d1,d2,float=1),d1,d2) $
		   else	return, make_array(d1,d2       ,float=1)
	   endif   else $
	if nd eq 3 then return, make_array(d1,d2,d3    ,float=1)	else $
	if nd eq 4 then return, make_array(d1,d2,d3,d4 ,float=1)
return, 0
end
;
function sl_darr,	nd,d1,d2,d3,d4
;******* ******
;**
on_error,2
;%ow%	if nd lt 0 then return, dblarr(d1)	 	else $
;%ow%	if nd eq 1 then return, dblarr(d1)	 	else $
;%ow%	if nd eq 2 then return, dblarr(d1,d2)	 	else $
;%ow%	if nd eq 3 then return, dblarr(d1,d2,d3) 	else $
;%ow%	if nd eq 4 then return, dblarr(d1,d2,d3,d4)
	if nd lt 0 then begin   if nd eq -2 then $
	   if d1(1) eq 1 then return,reform(make_array(dimension=d1,double=1),d1)$
		   else	return, make_array(dimension=d1,double=1) $
		   else	return, make_array(dimension=d1,double=1)
	   endif   else $
	if nd eq 1 then return, make_array(d1          ,double=1)	else $
	if nd eq 2 then begin
	   if d2 eq 1 	then return,reform(make_array(d1,d2,double=1),d1,d2) $
		   else	return, make_array(d1,d2       ,double=1)
	   endif   else $
	if nd eq 3 then return, make_array(d1,d2,d3    ,double=1)	else $
	if nd eq 4 then return, make_array(d1,d2,d3,d4 ,double=1)
return, 0
end
;
function sl_carr,	nd,d1,d2,d3,d4
;******* ******
;**
on_error,2
;%ow%	if nd lt 0 then return, complexarr(d1)	 	else $
;%ow%	if nd eq 1 then return, complexarr(d1)	 	else $
;%ow%	if nd eq 2 then return, complexarr(d1,d2)	else $
;%ow%	if nd eq 3 then return, complexarr(d1,d2,d3) 	else $
;%ow%	if nd eq 4 then return, complexarr(d1,d2,d3,d4)
	if nd lt 0 then begin   if nd eq -2 then $
	   if d1(1) eq 1 then return,reform(make_array(dimension=d1,complex=1),d1)$
		   else	return, make_array(dimension=d1,complex=1) $
		   else	return, make_array(dimension=d1,complex=1)
	   endif   else $
	if nd eq 1 then return, make_array(d1          ,complex=1)	else $
	if nd eq 2 then begin
	   if d2 eq 1 	then return,reform(make_array(d1,d2,complex=1),d1,d2) $
		   else	return, make_array(d1,d2       ,complex=1)
	   endif   else $
	if nd eq 3 then return, make_array(d1,d2,d3    ,complex=1)	else $
	if nd eq 4 then return, make_array(d1,d2,d3,d4 ,complex=1)
return, 0
end
;
;****************************************************** IOs
;
function sl_iotype,	str,typ,np,v1,v2,v3,v4
;******* *********
;**
on_ioerror,mis
bb=0
	if np eq 0 then print, str 		else $
	if np eq 1 then print, str,v1 		else $
	if np eq 2 then print, str,v1,v2 	else $
	if np eq 3 then print, str,v1,v2,v3 	else $
	if np ge 4 then print, str,v1,v2,v3,v4
	bb=1
mis:	return,bb
end
;
function sl_iofind	,specif,ext,vers ,names
;******* *********
;**
	common machin,	mc_sys,mc_sta
;**
on_ioerror,mis
bb=0
	if  ext  eq ''  then names = findfile (specif,count=bb)		else $
	if (vers eq 0) or (mc_sys ne 'vms') $
			then names = findfile (specif+'.'+ext	  ,count=bb) $
			else names = findfile (specif+'.'+ext+';0',count=bb)
mis:    return,bb
end
;
function sl_iolun,	u
;******* ********
;**
on_ioerror,mis
bb=0
u =0
	get_lun,u
	bb=1
mis:	return,bb
end
;
function sl_iofree,	u
;******* *********
;**
on_ioerror,mis
bb=0
	if u gt 0 then free_lun,u
	bb=1
mis:	return,bb
end
;
function sl_iopenw,	u,desc,ext,rec,struc
;******* *********
;**     struc=  0 for text , 1 for binary
	common machin,	mc_sys,mc_sta
;**
on_ioerror,mis
bb=0
	if mc_sys eq 'vms' then begin
	  if struc eq 1  then if rec le 0  then $
			 openw,u,desc+'.'+ext,/none	  else $
			 openw,u,desc+'.'+ext,rec,/fixed,/none $
	  else if rec le 0  then $
			 openw,u,desc+'.'+ext		  else $
			 openw,u,desc+'.'+ext,rec
	  bb=1

	endif else begin openw,u,desc+'.'+ext & bb=1 & endelse

mis:	if bb  eq 0 then u=0
	return,bb
end
;
function sl_iopenr,	u,desc,struc,frm
;******* *********
;**      Struc=  0 for edited text , 1 for binary
;**	 Frm  =  0 for fixed , 1 for text , 2 for tiff , 3 for stream
;**		 5 for segmented
	common machin,	mc_sys,mc_sta
;**
on_ioerror,mis
bb=0
	if mc_sys eq 'vms' then begin
	  if struc eq 1 then if frm eq 0 then openr,u,desc         else $
			     if frm eq 2 then openr,u,desc         else $
			     if frm eq 3 then openr,u,desc         else $
			     if frm eq 5 then openr,u,desc,/segm   $
				         else openr,u,desc         $
	  else openr,u,desc
	  bb=1
	endif else begin
			     if frm eq 5 then openr,u,desc,/f77_unformatted $
					 else openr,u,desc
	  bb=1
	endelse

mis:	if bb  eq 0 then u=0
	return,bb
end
;
function sl_iopoint,	u,n,rec
;******* **********
;**
on_ioerror,mis
bb=0
	point_lun, u,n*rec
	bb=1
mis:return,bb
end
;
function sl_iowrt,	u,area,vsiz,flg
;******* ********
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(0) ne vsiz(0)) or (test(1) ne vsiz(1)) or $
   (test(test(0))   ne vsiz(vsiz(0))) or $
   (test(test(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype('sl_iowrt size',0,0)
endif
;
on_ioerror,mis
bb=0
	if u gt 0 then  if flg eq 0 then writeu, u,area $
				    else printf, u,area
	bb=1
mis:	return,bb
end
;
function sl_ioread,	u,area,vsiz,flg
;******* *********
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if  flg eq 0 then $
if (test(0) ne vsiz(0)) or (test(1) ne vsiz(1)) or $
   (test(test(0))   ne vsiz(vsiz(0))) or $
   (test(test(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype('sl_ioread size',0,0)
endif
;
on_ioerror,mis
bb=0
	if u gt 0 then  if flg eq 0 then readu , u,area $
				    else readf , u,area
	bb=1
mis:	return,bb
end
;
function sl_ioreads,	str,typ,np,v1,v2,v3,v4
;******* **********
;**
on_ioerror,mis
bb=0
	if np eq 1 then reads, str ,v1		else $
	if np eq 2 then reads, str ,v1,v2 	else $
	if np eq 3 then reads, str ,v1,v2,v3 	else $
	if np ge 4 then reads, str ,v1,v2,v3,v4
	bb=1
mis:	return,bb
end
function sl_ioaccept,	str,typ,np,v1,v2,v3,v4
;******* ***********
;**
on_ioerror,mis
bb=0
	if np eq 1 then read, ' '+str+': ' ,v1		else $
	if np eq 2 then read, ' '+str+': ' ,v1,v2 	else $
	if np eq 3 then read, ' '+str+': ' ,v1,v2,v3 	else $
	if np ge 4 then read, ' '+str+': ' ,v1,v2,v3,v4
	bb=1
mis:	return,bb
end
;
function sl_ioclear,	dum
;******* **********
;**
	common machin,	mc_sys,mc_sta
;**
;	bb=sl_iotype('2J',0,0)
return, 1
end
;
function sl_iopage,	cout,flg
;******* *********
;**
	common machin,	mc_sys,mc_sta
;**
	if mc_sys eq 'vms' then bb=sl_iotype(cout,0,0)
return, 1
end
;
function sl_swapint,	area,d1,d2,d3, typ
;******* **********
;**
bb=1
	if typ eq 4  then byteorder,area, /Sswap else $
	if typ eq 16 then byteorder,area, /Lswap else $
	if typ ge 8  then begin
			  bb=sl_iotype('%Scan... Cant swap float data',0,0)
			  bb=0 & endif
return, bb
end
;
function sl_swapvms,	area,d1,d2,d3, typ ,flag
;******* **********
;**
	common machin,	mc_sys,mc_sta
;**
	if ((mc_sys eq 'vms') and ((flag lt 0) or (flag gt 255))) or   $
	   ((mc_sys ne 'vms') and  (flag gt 255))  then $
					return,sl_swapint(area,d1,d2,d3,typ)
return, 1
end
;
;****************************************************** MATH
;
function sl_sqrt	,area,dm,sz
;******* *******
;**
	if dm eq 1 then area       =sqrt(area) else $
	if sz eq 1 then area(0)    =sqrt(area) else $
	if sz eq 2 then area(0,0)  =sqrt(area) else $
	if sz eq 3 then area(0,0,0)=sqrt(area)
return, 1
end
;
function sl_abs		,are_in,are_out,nl,typ,sz
;******* ******
;**
	if sz eq 2 then are_out(0,0)  =abs(are_in) else $
	if sz eq 3 then are_out(0,0,0)=abs(are_in) $
		   else are_out       =abs(are_in)
return, 1
end
;
function sl_tang	,x
;******* *******
;**
	return, tan(x)
end
;
function sl_atang	,y,x
;******* ********
;**
	if (y ne 0) or (x ne 0) then return, atan(y,x) $
				else return, 0.
end
;
function sl_atangm	,are_in,are_out,nl,typ,sz
;******* *********
;**
	if sz eq 2 then begin
	   if typ eq 64 then are_out(0,0)=float(atan(are_in))  $
		        else are_out(0,0)=	atan(are_in)
	endif else begin
	   if typ eq 64 then are_out	 =float(atan(are_in))  $
		        else are_out	 =	atan(are_in)
	endelse
return, 1
end
;
function sl_pfix	,x
;******* *******
;**
common my_funct, i,bb,ab
;**
	bb=long(x)
	if (x-bb gt  0.5) then bb=bb+1 else $
	if (x-bb lt -0.5) then bb=bb-1
return, bb
end
;
function sl_maxf	,area,vsiz,cm
;******* ******
;**
common my_funct, i,bb,ab
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(0) ne vsiz(0)) or (test(1) ne vsiz(1)) or $
   (test(test(0))   ne vsiz(vsiz(0))) or $
   (test(test(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype('sl_maxf size',0,0)
endif
;
	bb=max      (area)
	ab=sl_sysget(20,cm)
return, bb
end
;
function sl_minf	,area,vsiz,cm
;******* ******
;**
common my_funct, i,bb,ab
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(0) ne vsiz(0)) or (test(1) ne vsiz(1)) or $
   (test(test(0))   ne vsiz(vsiz(0))) or $
   (test(test(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype('sl_minf size',0,0)
endif
;
	bb=min      (area)
	ab=sl_sysget(20,cm)
return, bb
end
;
function sl_maxim	,area,vsiz,cm,mv
;******* ********
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(0) ne vsiz(0)) or (test(1) ne vsiz(1)) or $
   (test(test(0))   ne vsiz(vsiz(0))) or $
   (test(test(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype('sl_maxf size',0,0)
endif
;
	return,max	(area,cm,min=mv)
end
;
function sl_correl,	area1,area2,dx,dy,typ
;******* *********
;**
if !quiet  eq 2 then begin
test=sl_size(area1)
if (test(1) ne dx) or (test(2) ne dy) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_correl size',0,0)
endif
;
	return,correlate(area1,area2)
end
;
function sl_deviat,	area,y1,dx,dy,typ
;******* *********
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or (test(2) ne dy) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_deviat size',0,0)
endif
;
	return,stdev   (area,y1)
end
;
function sl_logn,	area,vsiz
;******* *******
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(0) ne vsiz(0)) or (test(1) ne vsiz(1)) or $
   (test(test(0))   ne vsiz(vsiz(0))) or $
   (test(test(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype('sl_logn size',0,0)
endif
;
	if vsiz(0) eq 2 then area(0,0)  = alog (area) $
			else area	= alog (area)

return, 1
end
;
function sl_log1,	val,typ
;******* *******
;**
	return,alog    (val)
end
;
function sl_expn,	area,vsiz
;******* *******
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(0) ne vsiz(0)) or (test(1) ne vsiz(1)) or $
   (test(test(0))   ne vsiz(vsiz(0))) or $
   (test(test(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype('sl_expn size',0,0)
endif
;
	if vsiz(0) eq 2 then area(0,0)  = exp  (area) $
			else area	= exp  (area)
return, 1
end
;
function sl_cos,	ah
;******* ******
;**
	return,cos     (ah)
end
;
function sl_acos,	ah
;******* *******
;**
	return,acos    (ah)
end
;
function sl_sin,	ah
;******* ******
;**
	return,sin     (ah)
end
;
function sl_asin,	ah
;******* *******
;**
	return,asin    (ah)
end
;
;****************************************************** MISC
;
function sl_gfit	,x,y,vsiz,par
;******* *******
;**
	return,gaussfit (x,y,par)
end
;
;
function sl_polycoef	,x,y,vsiz,deg
;******* ***********
;**
	return,poly_fit (x,y,deg)
end
;
;
function sl_polyval	,x,vsiz,coef,deg
;******* **********
;**
	return,poly     (x,coef)
end
;
;
function sl_surfit	,area,vsiz,deg
;******* *********
;**
	return,surface_fit(area,deg)
end
;
function sl_hist	,area,dm,typ ,his ,mn,mx
;******* *******
;**
bb=0
	if mn ne mx then his=histogram(area,min=mn,max=mx,binsize=1) else his=1
	bb =sl_element(his)
return, bb
end
;
function sl_redim,	area,dx,dy,typ,nx,ny,flg
;******* ********
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or (test(2) ne dy) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_redim size',0,0)
endif
;
;%ow%	if flg eq 0 then return,congrid(area,nx,ny) $
;%ow%	else begin  i=nx/dx & i=i*dx & if i eq 0 then i=dx
;%ow%		    j=ny/dy & j=j*dy & if j eq 0 then j=dy
;%ow%		    if (i eq nx) and (j eq ny) then return,rebin(area,nx,ny) $
;%ow%		    else return,congrid(rebin(area,i,j),nx,ny) & endelse
	return,congrid( area,nx,ny ,interp=flg)
end
;
function sl_lis,	area,dx,dy,typ ,np ,fl
;******* ******
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or ((test(2) ne dy) and (dy gt 1)) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_lis size',0,0)
endif
;
	if fl eq 1 then begin
		if dy eq 1 then area(0,0)=  smooth(  area,np  ) $
			   else area	 =  smooth(  area,np  )
				return,1
	endif 	   else		return,smooth(  area,np  )
end
;
function sl_media,	area,dx,dy,typ ,np ,x1,y1
;******* ********
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or ((test(2) ne dy) and (dy gt 1)) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_media size',0,0)
endif
;
	return,median(  area,np  )
end
;
function sl_robt,	area,dx,dy,typ
;******* *******
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or (test(2) ne dy) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_robt size',0,0)
endif
;
	return,roberts( area )
end
;
function sl_sobl,	area,dx,dy,typ
;******* *******
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or (test(2) ne dy) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_sobel size',0,0)
endif
;
	return,sobel (  area )
end
;
function sl_fft,	area,dir,dx,dy ,fl
;******* ******
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or (test(2) ne dy) then bb=sl_iotype('sl_fft size',0,0)
endif
;
	if fl eq 1 then begin
		     if dy eq 1 then area(0,0)	=fft (area,dir) $
			        else area	=fft (area,dir)
				return,1
	endif	   else		return,fft   (  area,dir )
end
;
function sl_imaginary	,are_in,are_out,nl,typ,sz
;******* ************
;**
	if sz eq 2 then are_out(0,0)=imaginary(are_in) $
		   else are_out     =imaginary(are_in)
return, 1
end
;
function sl_rotat,	area,dx,dy,typ ,ang ,fl
;******* ********
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or (test(2) ne dy) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_rotat size',0,0)
endif
;
	i=ang/90
	if fl eq 1 then begin
	   if (i*90 eq ang) then area=	rotate (area,i  ) $
			    else area=	rot_int(area,ang)
	   return,1
	endif else begin
	   if (i*90 eq ang) then return,rotate (area,i  ) $
			    else return,rot_int(area,ang)
	endelse
end
;
function sl_rotscal,	area,vsiz,nf,ang,mag,cx,cy
;******* **********
;**
	if vsiz(0) lt 3 then    area	    =rot_int(area,ang,mag,cx,cy) else $
				area(0,0,nf)=rot_int(area(*,*,nf),ang,mag,cx,cy)
return, 1
end
;
function sl_pogons,  erey,vsiz,vl
;******* *********
;**
common my_pog,	tvare,tpare
;**
     	shade_volume,erey,vl, tvare,tpare

;	surface,fltarr(2,2),/nodata,/save,$
;		xrange=[0,vsiz(1)-1],yrange=[0,vsiz(2)-1],zrange=[0,vsiz(3)-1]
;			,xstyle=4,ystyle=4,zstyle=4

return, 1
end
;
function sl_shadoc, fl, area,xs,ys,  ax,ay,az
;******* *********
;**
common my_pog,	tvare,tpare
;**
   if fl eq 1 then begin
	zv=size(tvare)
	zp=size(tpare)
	if (zv(0) eq 2) and (zp(0) eq 1) then begin
;	t3d,/reset,translate=[-.5,-.5,-.5],rotate=[ax,ay,az]
;	t3d,       translate=[ .5, .5, .5]
	area(0,0)=polyshade(tvare,tpare,xsize=xs,ysize=ys)
	endif
   endif else begin
	tvare=1
	tpare=1
   endelse
return, 1
end
;
function sl_revs,	area,dx,dy,typ,flg
;******* *******
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) then bb=sl_iotype('sl_revs size',0,0)
endif
;
	if flg eq 0 then area = reverse( area ) $
		    else area = rotate ( area,flg)
return, 1
end
;
function sl_transp,	area,dx,dy,typ
;******* *********
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or (test(2) ne dy) or $
   (test(test(0)+1) ne typ) then  bb=sl_iotype('sl_transp size',0,0)
endif
;
	return,transpose(area)
end
;
function sl_scale,	area,dx,dy,typ ,mn,mx
;******* ********
;**
;**Scale  by  tv_nc  and return in byte.
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or (test(2) ne dy) or $
   (test(test(0)+1) ne typ) then  bb=sl_iotype('sl_scale size',0,0)
endif
;
	if mn eq mx then return, bytscl ( area ) $
		    else return, bytscl ( area,min=mn,max=mx )
end
;
function sl_shift,	area,dx,dy,typ ,nx,ny
;******* ********
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or ((test(2) ne dy) and (dy gt 0)) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_shift size',0,0)
endif
;
	if dy gt 0 then return, shift(area,nx,ny) $
		   else return, shift(area,nx)
end
;
function sl_shiff,	area,dx,dy,typ ,nx,ny
;******* ********
;**
	if dy eq 1 then area(0,0)= shift(area,nx,ny) else $
	if dy gt 0 then area	 = shift(area,nx,ny) $
		   else area	 = shift(area,nx)
return, 1
end
;
function sl_totf,	area,dx,dy,typ
;******* *******
;**
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(1) ne dx) or ((test(2) ne dy) and (dy gt 0)) or $
   (test(test(0)+1) ne typ) then bb=sl_iotype('sl_totf size',0,0)
endif
;
	return,total   (area)
end
;
;function sl_sum,	area,dm
;******* ******
;**
;	return, sum    (area,dm)
;end
;
function sl_fsum,	area,dm,siz,  areout
;******* *******
;**
bb=0
 if siz(0) gt dm then begin
  bb=1
  z=siz(0)
  if z eq 1 then begin
	areout=total(area)
  endif else if z eq 2 then begin
	if dm eq 0 then if  siz(2) eq 1 then areout     =total(area)  $
		   else for j=0,siz(2)-1  do areout(j)  =total(area(*,j))
	if dm eq 1 then if  siz(1) eq 1 then areout     =total(area)  $
		   else for i=0,siz(1)-1  do areout(i)  =total(area(i,*))
  endif else if z eq 3 then begin
	if dm eq 0 then if  siz(3) eq 1 then $
			if  siz(2) eq 1 then areout     =total(area) else   $
			for j=0,siz(2)-1  do areout(j)  =total(area(*,j,0)) $
		   else for k=0,siz(3)-1  do $
			for j=0,siz(2)-1  do areout(j,k)=total(area(*,j,k))
	if dm eq 1 then if  siz(3) eq 1 then $
			if  siz(1) eq 1 then areout     =total(area) else   $
			for i=0,siz(1)-1  do areout(i)  =total(area(i,*,0)) $
		   else for k=0,siz(3)-1  do $
			for i=0,siz(1)-1  do areout(i,k)=total(area(i,*,k))
	if dm eq 2 then if  siz(2) eq 1 then $
			if  siz(1) eq 1 then areout     =total(area) else   $
			for i=0,siz(1)-1  do areout(i)  =total(area(i,0,*)) $
		   else for j=0,siz(2)-1  do $
			for i=0,siz(1)-1  do areout(i,j)=total(area(i,j,*))
  endif
 endif
return,bb
end
;
function sl_tsum,	area,dm,  s2,areout
;******* *******
;**
bb=0
siz=sl_size(area)
 if siz(0) gt dm then begin
  bb=1
  z=siz(0)
  if z eq 1 then begin
	areout=total(area)
  endif else if z eq 2 then begin
	if dm eq 0 then if  siz(2) eq 1 then areout     =total(area)  $
		   else begin
			if s2 eq 1 then $
			for j=0,siz(2)-1  do areout(j)  =total(area(*,j))
			if s2 eq 2 then $
			for j=0,siz(2)-1  do areout(j,0)=total(area(*,j))
		   endelse
	if dm eq 1 then if  siz(1) eq 1 then areout     =total(area)  $
		   else begin
			if s2 eq 1 then $
			for i=0,siz(1)-1  do areout(i)  =total(area(i,*))
			if s2 eq 2 then $
			for i=0,siz(1)-1  do areout(i,0)=total(area(i,*))
		   endelse
  endif else if z eq 3 then begin
	if dm eq 0 then begin
		   if s2 eq 1 then $
			for k=0,siz(3)-1  do $
			for j=0,siz(2)-1  do areout(j)  =total(area(*,j,k))
		   if s2 eq 2 then $
			for k=0,siz(3)-1  do $
			for j=0,siz(2)-1  do areout(j,k)=total(area(*,j,k))
		   endif
	if dm eq 1 then begin
		   if s2 eq 1 then $
			for k=0,siz(3)-1  do $
			for i=0,siz(1)-1  do areout(i)  =total(area(i,*,k))
		   if s2 eq 2 then $
			for k=0,siz(3)-1  do $
			for i=0,siz(1)-1  do areout(i,k)=total(area(i,*,k))
		   endif
	if dm eq 2 then begin
		   if s2 eq 1 then $
			for j=0,siz(2)-1  do $
			for i=0,siz(1)-1  do areout(i)  =total(area(i,j,*))
		   if s2 eq 2 then $
			for j=0,siz(2)-1  do $
			for i=0,siz(1)-1  do areout(i,j)=total(area(i,j,*))
		   endif
  endif
 endif
return,bb
end
;
function sl_index,	dm,typ
;******* ********
;**
	if typ eq 8 then return, make_array(dm,float=1,/index) $
		    else return, make_array(dm,int=1  ,/index)
end
;
function sl_wait	,x
;******* *******
;**
	wait,x
return,1
end
;
function sl_where	,area,vsiz,opr,x,areout
;******* ********
;**
nl=0
	areout(0)=-1
	if opr   eq 'ne'  then areout(0)=where(area ne x) else $
	if opr   eq 'eq'  then areout(0)=where(area eq x) else $
	if opr   eq 'lt'  then areout(0)=where(area lt x)
	if areout(0) ge 0 then nl=n_elements(areout) else nl=0
return, nl
end
;
;****************************************************** STRINGs
;
function sl_str,	v,fmat
;******* ******
;**
	return,	 string(v,format=fmat)
end
;
function sl_strf,	area,vsiz
;******* ******
;**
	return,	 string(area)
end
;
function sl_sti,	c_out,c_in,pos
;******* ******
;**
	strput,		c_out,c_in,pos
return, 1
end
;
function sl_stx,	c_in,pos,len
;******* ******
;**
	return,	 strmid(c_in,pos,len)
end
;
function sl_stp,	c_in,c_sub,pos
;******* ******
;**
	return,	 strpos(c_in,c_sub,pos)
end
;
function sl_stbr,	c_in,flg
;******* *******
;**
	return, strtrim(c_in,flg)
end
;
function sl_stup,	c_in
;******* *******
;**
	return,strupcase(c_in)
end
;
function sl_stdim,	c_in,elm
;******* *******
;**
	bb =sl_element(c_in)
	tab=strlen(c_in)
	if  bb gt 1 then elm=max(tab) else elm=tab(0)
return, bb
end
;
function sl_stbyt,	str,area
;******* ********
;**
	area(0)=byte(str)
return, 1
end
;
;
;
;************************ SL_TV.NEW ****************************************
;************************ SL_TV.NEW ****************************************
;************************ SL_TV.NEW ****************************************
;
;;************************************************ Widget level ************
;
function sl_wggetuv, wg, uv
;******* **********
i=0
	if wg gt 0 then begin
	   widget_control, wg ,bad_id=i, get_uvalue=uv
	   if i eq 0 then begin
	      i =sl_element(uv)
	      if i  ge 4 then $
		if (uv(0) eq -87) or (uv(0) eq -88) then i=1 else i=0 $
	      else i=0
	   endif else  i=0
	endif
return, i
end
;
function sl_wgsens,wg, flg
;******* *********
;**
i=1
	if wg gt 0 then begin  i=0
		   if flg eq 0 then widget_control,bad_id=i,wg,sensitive =0
		   if flg eq 1 then widget_control,bad_id=i,wg,sensitive =1
		   if flg eq 2 then widget_control,bad_id=i,wg,set_button=0
		   if flg eq 3 then widget_control,bad_id=i,wg,set_button=1
	endif
return, 1-i
end
;
function sl_wghourglass, dum
;******* **************
;**
	widget_control,/hourglass
return, 1
end
;
function sl_wgdel, wg
;******* ********
;**
	widget_control,bad_id=i,wg, /destroy
return, 1
end
;
function sl_wgmotion, wd ,fl
;******* ***********
;**
	widget_control,bad_id=i,wd,DRAW_MOTION_EVENTS=fl
return, 1
end
;
pro sl_wgfocus, wn
;** **********
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur

	if wn gt 0 then $
	 ;if mot_wg(wn) gt 0 then widget_control,bad_id=i,mot_wd(wn)      ,/INPUT_FOCUS ;ok for Windows
	  if mot_wg(wn) gt 0 then widget_control,bad_id=i,mot_wghinf(6,wn),/INPUT_FOCUS
	  end
;
function sl_wgtimer, wd ,fl
;******* **********
;**
	widget_control,bad_id=i,wd,TIMER=fl
return, 1
end
;
function sl_wgvalid,wg
;******* **********
;**
	return,widget_info(wg,/valid_id)
end
;
function sl_wgjreg, wg
;******* *********
;**
	xmanager,string(wg),wg,event_handler='scan_event',/just_reg
return, 1
end
;
pro scan_event, ev
;** **********
;**
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur

;**
;**	Test if scan not running.
	if (mot_wdcur(7) gt 0) and (mot_wdcur(0) lt 0) then begin
	  i=sl_wggetuv(ev.id,mot_getuv)
	  if i eq 1 then begin
		widget_control,bad_id=i,mot_wdcur(7),set_uvalue=mot_getuv
		widget_control,bad_id=i,mot_wdcur(7),send_event=ev
	  endif
	endif else begin
;**
;**	scan is running.
	  mot_ev=ev
	  mot_wdcur(9)=mot_ev.top
	endelse
end
;
function sl_wglux, w,sx,sy,ttl,xp,yp ,seq ,t_sx,t_sy
;******* ********
;**     w is the scan  window number
;**	seq = 1	 Starting and pixmap (not mapped)
;**	seq = 2  Menu + ok
;**	seq = 3  Menu + cancel (list)
;**	seq = 4  Menu + input buffer
;**	seq = 5  Menu
;**	seq = 6  Glory_Hole
;**	seq = 7  Scan
;**	seq = 8  View
;**	seq = 9  Info
;**	seq = 10 Scan or View to be resized
;**	seq < 0  Scan or View in base -seq
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
x =xp & y =yp
	mot_t1=0
	mot_t3=0
	mot_t4=0
;**
;**     Bases
;**	-----
	if (seq lt 0) then begin
	   mot_t1=-seq
	   bb=sl_wgvalid(mot_t1)
	   if bb eq 1  then begin
		x=-1
;		mot_wn=widget_draw(mot_t1,/button_events,retain=2, $
;			           xsize=sx,ysize=sy,/scroll)
;		mot_t1=mot_wn
		mot_wn=mot_t1

		mot_wghinf(8 ,w)=-1
		mot_wghinf(9 ,w)=-1
		mot_wghinf(14,w)=mot_t1
	   endif else seq = 7
	endif
;********
	if (seq eq 10) or (seq eq 11) then begin
	   mot_t1=mot_wg(w)
	   mot_t3=mot_wghinf(14,w)
	   bb=sl_wgvalid(mot_t3)
	   if bb eq 1  then begin
		x=-1
		bb=sl_wgdel(mot_wd(w))
	   	i=sx
	   	j=sy
	   	if  sx gt t_sx-200 then i=t_sx-200
	   	if  sy gt t_sy-400 then j=t_sy-400

	   	if (sx eq i) and (sy eq j) then $
	     	    mot_wn= widget_draw(mot_t3,/button_events,retain=2, $
			   	   	xsize=sx,ysize=sy)        else  $
	     	    mot_wn= widget_draw(mot_t3,/button_events,retain=2, $
			   		xsize=sx,ysize=sy,x_scroll_size=i,y_scroll_size=j)
	   endif else seq = seq -3
	endif
;********
	if (seq eq 7) or (seq eq 8) then begin
	   y=0
	   mot_t1=widget_base(title=ttl,kill_notify='',/tlb_size_events,map=0,$
			      /column,xpad=5,ypad=5,resource_name='scan')
	   widget_control,bad_id=i,mot_t1,default_font=sys_dep('FONTD')
;			'-bitstream-charter-medium-r-normal--15-140-75-75-p-84-iso8859-1'
	   mot_t2=widget_base  (mot_t1,/row)
	   mot_setuv = [-87,0 ,w ,0,0, 0,0, sx,sy]

	   mot_t3=widget_base(mot_t2,/exclusive)
	   mot_wghinf(1,w)=mot_t3
	   mot_setuv(1) =21
	   mot_t4=widget_button(mot_t3,value='SCAN'	,uvalue=mot_setuv,$
				/frame)
	   mot_t3=widget_base(mot_t2,/row)
	   mot_wghinf(0,w)=mot_t3
	   mot_setuv(1) =22
	   mot_t4=widget_button(mot_t3,value='CLOSE'	,uvalue=mot_setuv)
	   mot_setuv(1) =23
	   mot_t4=widget_button(mot_t3,value='Save'	,uvalue=mot_setuv)
	   mot_setuv(1) =24
	   mot_t4=widget_button(mot_t3,value='Remove'	,uvalue=mot_setuv)
	   mot_setuv(1) =25
	   mot_t4=widget_button(mot_t3,value='Print'	,uvalue=mot_setuv)
	   mot_setuv(1) =26
	   mot_t4=widget_button(mot_t3,value='Duplic'   ,uvalue=mot_setuv)

	   mot_t4=widget_label (mot_t3,value='        .......... GLORY_HOLE ..........')
;	   mot_t4=widget_label (mot_t3,value='        GLORY_HOLE (For fine adjustment use Arrow_keys  ,'$
;	   				    +'  Page_up  ,  Page_down  ,  Home )')
;***
	   mot_t0=widget_base  (mot_t1,/column)
	   mot_wghinf(2,w)=mot_t0
	   mot_t2=widget_base  (mot_t0,/row)
;**	   Info..
	   mot_t3=widget_base  (mot_t2)
	   kpi   =widget_draw  (mot_t3,xsize=mot_wdcur(5),ysize=mot_wdcur(6),$
				retain=2)
	   mot_wghinf(3,w)=kpi

	   mot_t3=widget_base  (mot_t2,/column)
;**	   G_H...
	   mot_t4=widget_base  (mot_t3)
	   kpg   =widget_draw  (mot_t4,xsize=mot_wdcur(3),ysize=mot_wdcur(4),$
				retain=1)
	   mot_wghinf(4,w)=kpg
;***
	   mot_t4=widget_base  (mot_t3,/row)

	   mot_setuv(1) =33
	   mot_t5=widget_button(mot_t4, value=' ? ', uvalue=mot_setuv)

	   mot_t5=widget_base  (mot_t4,/row,/exclusive)
	   mot_wghinf(49,w)=mot_t5
	   mot_wghinf(15,w)=0
	   mot_setuv(1) =71
	   mot_t6=widget_button(mot_t5, value='i', uvalue=mot_setuv,/no_release)
	   mot_wghinf(16,w)=mot_t6
	   mot_setuv(1) =72
	   mot_t6=widget_button(mot_t5, value='l', uvalue=mot_setuv,/no_release)
	   mot_wghinf(17,w)=mot_t6
	   mot_setuv(1) =73
	   mot_t6=widget_button(mot_t5, value='s', uvalue=mot_setuv,/no_release)
	   mot_wghinf(18,w)=mot_t6
	   mot_setuv(1) =74
	   mot_t6=widget_button(mot_t5, value='x', uvalue=mot_setuv,/no_release)
	   mot_wghinf(19,w)=mot_t6
	   mot_setuv(1) =75
	   mot_t6=widget_button(mot_t5, value='y', uvalue=mot_setuv,/no_release)
	   mot_wghinf(20,w)=mot_t6

	   mot_t5=widget_label (mot_t4, value='  ')
	   mot_t5=widget_base  (mot_t4,/row,/exclusive)
	   mot_wghinf(21,w)=0
	   mot_wghinf(22,w)=0
	   mot_setuv(1) =80
	   mot_t6=widget_button(mot_t5, value='r', uvalue=mot_setuv,/no_release)
	   mot_wghinf(23,w)=mot_t6
	   mot_setuv(1) =81
	   mot_t6=widget_button(mot_t5, value='n', uvalue=mot_setuv,/no_release)
	   mot_wghinf(24,w)=mot_t6

	   mot_t5=widget_label (mot_t4, value='  ')
	   mot_t5=widget_base  (mot_t4,/row,/nonexclusive)
	   mot_wghinf(25,w)=0
	   mot_wghinf(26,w)=0
	   mot_wghinf(27,w)=0
	   mot_wghinf(28,w)=1
	   mot_setuv(1) =82
	   mot_t6=widget_button(mot_t5, value='o', uvalue=mot_setuv)
	   mot_wghinf(29,w)=mot_t6
	   mot_setuv(1) =83
	   mot_t6=widget_button(mot_t5, value='p', uvalue=mot_setuv)
	   mot_wghinf(30,w)=mot_t6
	   mot_setuv(1) =84
	   mot_t6=widget_button(mot_t5, value='e', uvalue=mot_setuv)
	   mot_wghinf(31,w)=mot_t6
	   mot_setuv(1) =85
	   mot_t6=widget_button(mot_t5, value='#', uvalue=mot_setuv)
	   mot_wghinf(32,w)=mot_t6

	   mot_t5=widget_label (mot_t4, value='  ')
	   mot_t5=widget_base  (mot_t4,/row,/exclusive)
	   mot_wghinf(50,w)=mot_t5
	   mot_wghinf(33,w)=0
	   mot_setuv(1) =86
	   mot_t6=widget_button(mot_t5, value='!', uvalue=mot_setuv,/no_release)
	   mot_wghinf(34,w)=mot_t6
	   mot_setuv(1) =87
	   mot_t6=widget_button(mot_t5, value='_', uvalue=mot_setuv,/no_release)
	   mot_wghinf(35,w)=mot_t6
;***
	   mot_t5=widget_base  (mot_t3,/row)
	   mot_setuv(1) =30
	   mot_t6=widget_button(mot_t5,value='Cut'	,uvalue=mot_setuv)
	   mot_setuv(1) =31
	   mot_t6=widget_button(mot_t5,value='Un_zoom'	,uvalue=mot_setuv)
;	   mot_setuv(1) =32
;	   mot_t6=widget_button(mot_t5,value='Misc.'	,uvalue=mot_setuv)
	   mot_setuv(1) =34
	   mot_t6=widget_button(mot_t5,value='Convol'	,uvalue=mot_setuv)
	   mot_setuv(1) =35
	   mot_t6=widget_button(mot_t5,value='Special'	,uvalue=mot_setuv)
	   mot_setuv(1) =36
	   mot_t6=widget_button(mot_t5,value='Math'	,uvalue=mot_setuv)
	   mot_setuv(1) =37
	   mot_t6=widget_button(mot_t5,value='Frame opr',uvalue=mot_setuv)
;	   mot_setuv(1) =33
;	   mot_t6=widget_button(mot_t5,value='Handies'	,uvalue=mot_setuv)
	   mot_setuv(1) =38
	   mot_t6=widget_button(mot_t5,value='Colors'	,uvalue=mot_setuv)
;***
	   mot_t6=widget_text  (mot_t5,value='',xsize=1,ysize=1,/EDITABLE)
	   mot_wghinf(6,w) =mot_t6
;***
	   mot_t2=widget_base  (mot_t0,/row)
	   mot_t3=widget_base  (mot_t2)
	   mot_wghinf(14,w)=mot_t3
	   i=sx
	   j=sy
	   if  sx gt t_sx-200 then i=t_sx-200
	   if  sy gt t_sy-440 then j=t_sy-440

	   if (sx eq i) and (sy eq j) then  $
	     mot_wn=widget_draw(mot_t3,/button_events,retain=2, $
			   xsize=sx,ysize=sy)             else  $
	     mot_wn=widget_draw(mot_t3,/button_events,retain=2, $
			   xsize=sx,ysize=sy,x_scroll_size=i,y_scroll_size=j)

	   if (sx lt t_sx*2/3) then mot_str='small'    else mot_str='wide'
	   if (sy lt t_sy/3)   or (t_sy lt 900) then $
				if  mot_str eq 'small' then mot_str='thin' $
						       else mot_str='limit'
;***
	   mot_t5=widget_base  (mot_t2,/column)
	   if mot_str eq 'thin' then mot_t3=widget_base  (mot_t5,/row,   /nonexclusive) $
				else mot_t3=widget_base  (mot_t5,/column,/nonexclusive)
	   mot_setuv(1) =40
	   mot_t4=widget_button(mot_t3,value='Log'	,uvalue=mot_setuv)
	   mot_wghinf(10,w)=mot_t4
	   mot_wghinf(11,w)=0

	   mot_setuv(1) =42
	   mot_t4=widget_button(mot_t3,value='Slice'	,uvalue=mot_setuv)
	   mot_wghinf(36,w)=mot_t4
	   mot_wghinf(37,w)=0

	   mot_setuv(1) =41
	   mot_t4=widget_button(mot_t3,value='Smooth'	,uvalue=mot_setuv)
	   mot_wghinf(12,w)=mot_t4
	   mot_wghinf(13,w)=0

	   mot_setuv(1) =43
	   mot_t4=widget_button(mot_t3,value='Square'	,uvalue=mot_setuv)
	   mot_wghinf(52,w)=mot_t4
	   mot_wghinf(53,w)=0

	   mot_t3=widget_label (mot_t5, value='  ')
	   if (mot_str eq 'small') or $
	      (mot_str eq 'thin') then mot_t3=widget_base(mot_t5,/row   , $
	   					         /exclusive)      $
			          else mot_t3=widget_base(mot_t5,/column, $
			     			         /exclusive)

	   mot_wghinf(51,w)=mot_t3
	   mot_wghinf(38,w)=100
	   mot_setuv(1) =44
	   mot_t4=widget_button(mot_t3,value='Surface',uvalue=mot_setuv,/no_release)
	   mot_wghinf(39,w)=mot_t4

	   mot_setuv(1) =45
	   mot_t4=widget_button(mot_t3,value='Levels' ,uvalue=mot_setuv,/no_release)
	   mot_wghinf(40,w)=mot_t4

	   mot_setuv(1) =46
	   mot_t4=widget_button(mot_t3,value='Image'  ,uvalue=mot_setuv,/no_release)
	   mot_wghinf(41,w)=mot_t4

	   mot_setuv(1) =48
	   mot_t4=widget_button(mot_t3,value='Other'  ,uvalue=mot_setuv,/no_release)
	   mot_wghinf(42,w)=mot_t4

	   mot_setuv(1) =49
	   mot_t4=widget_button(mot_t5,value='Params' ,uvalue=mot_setuv)

	   mot_setuv(1) =50
	   mot_t4=widget_button(mot_t5,value='Update' ,uvalue=mot_setuv)

	   i=mot_t5
	   if (mot_str eq 'small') or (mot_str eq 'thin') then sl_poslider,i,w
;***
	   mot_t2=widget_base  (mot_t0,/row)

	   mot_t3=widget_base  (mot_t2,/column)
	   mot_t6=widget_slider(mot_t3,value=0)
	   mot_wghinf(5,w)=mot_t6
	   mot_wghinf(8,w)=101
	   mot_t4=widget_base  (mot_t3,/row)
	   mot_t5=widget_label (mot_t4,value='low:')
	   mot_t5=widget_label (mot_t4,value='-------------')
	   mot_setuv(5)=mot_t5

	   mot_setuv(1) =61
	   mot_t3=widget_base  (mot_t2,/exclusive)
	   mot_t3=widget_button(mot_t3,value='Apply'	,uvalue=mot_setuv)

	   mot_setuv(1) =60
	   mot_setuv(4)=mot_t3
	   widget_control,mot_t6,bad_id=i,set_uvalue=mot_setuv

	   mot_t3=widget_base  (mot_t2,/column)
	   mot_t6=widget_slider(mot_t3,value=100)
	   mot_wghinf(7,w)=mot_t6
	   mot_wghinf(9,w)=101
	   mot_t4=widget_base  (mot_t3,/row)
	   mot_t5=widget_label (mot_t4,value='high:')
	   mot_t5=widget_label (mot_t4,value='-------------')
	   mot_setuv(1) =62  &  mot_setuv(5)=mot_t5
	   widget_control,mot_t6,bad_id=i,set_uvalue=mot_setuv

	   if (mot_str eq 'small') or (mot_str eq 'thin') then $
				       mot_t6=widget_label(mot_t2,value='               ')

	   mot_t3=widget_base  (mot_t2,/column)
	   mot_t6=widget_label (mot_t3,value=' DRAGGING THE MOUSE')
	   mot_t6=widget_label (mot_t3,value='  (button pressed) ')
	   mot_t6=widget_label (mot_t3,value='                   ')
	   mot_t3=widget_base  (mot_t2,/column)
	   mot_t6=widget_label (mot_t3,value=':Left   to  zoom  ')
	   mot_t6=widget_label (mot_t3,value=':Middle size box  ')
	   mot_t6=widget_label (mot_t3,value=':Right  to  mask  ')

	   i=mot_t2
	   if (mot_str eq 'wide') or (mot_str eq 'limit') then sl_poslider,i,w

	   bb=sl_wgsens(mot_wghinf(0,w),0)
	   bb=sl_wgsens(mot_wghinf(1,w),1)
	   bb=sl_wgsens(mot_wghinf(2,w),0)

;********Menu input
	endif else $
	if (seq eq 4) then begin
	   mot_t1=widget_base(title=ttl,kill_notify='',/tlb_size_events,map=0,$
			      /column,xpad=10,ypad=10,resource_name='scan')
	   mot_t2=widget_base (mot_t1,/row,/frame)
	   mot_t3=widget_label(mot_t2,value='                     ')
	   mot_t4=widget_text (mot_t2,/editable,xsize=25,ysize=1,/scroll)
	   mot_setuv = [-87,seq ,w ,0,0, mot_t3,mot_t4, sx,sy]
	   widget_control,mot_t4,bad_id=i,set_uvalue=mot_setuv
	   bb=sl_wgsens(mot_t4,0)

	   mot_wn=widget_draw(mot_t1,/button_events,retain=2, $
			      xsize=sx,ysize=sy)

	   mot_t2=widget_button(mot_t1,value='   Return   ')
	   mot_setuv(1)= 3
	   widget_control,mot_t2,bad_id=i,set_uvalue=mot_setuv
;********G_H
	endif else $
	if (seq eq 6) then begin
	   i=mot_wdcur(0)
	   if i ge 0 then $
	      if (mot_wg(i) gt 0) and (mot_wghinf(4,i) gt 0)	then  begin
		  bb=sl_wgvalid(mot_wghinf(4,i))
		  if bb ne 1  then i=-1 else $
		     mot_t1=mot_wghinf(4,i)
	      endif else  i=-1
	   if i ge 0 then begin x=-1 &	mot_wn=mot_t1
	   endif else begin
	    mot_t1=widget_base(title=ttl,kill_notify='',/tlb_size_events,map=0,$
							 resource_name='scan')
	    mot_wn=widget_draw(mot_t1,/button_events,retain=1, $
	      		       xsize=sx,ysize=sy)
	   endelse
;********Info
	endif else $
	if (seq eq 9) then begin
	   i=mot_wdcur(0)
	   if i ge 0 then $
	      if (mot_wg(i) gt 0) and (mot_wghinf(3,i) gt 0)	then  begin
		  bb=sl_wgvalid(mot_wghinf(3,i))
		  if bb ne 1  then i=-1 else $
		     mot_t1=mot_wghinf(3,i)
	      endif else  i=-1
	   if i ge 0 then begin x=-1 & mot_wn=mot_t1
	   endif else begin
	    mot_t1=widget_base(title=ttl,kill_notify='',/tlb_size_events,map=0,$
							 resource_name='scan')
	    mot_wn=widget_draw(mot_t1,/button_events,retain=2, $
			       xsize=sx,ysize=sy)
	   endelse
;********Menus
	endif else $
	if (seq eq 3) or (seq eq 2) then begin
	   mot_t1=widget_base(title=ttl,kill_notify='',/tlb_size_events,map=0,$
			      /column,xpad=10,ypad=10,resource_name='scan')

	   mot_wn=widget_draw(mot_t1,/button_events,retain=2, $
			      xsize=sx,ysize=sy)

	   if seq eq 3 then mot_t2=widget_button(mot_t1,value='   Return   ')
	   if seq eq 2 then mot_t2=widget_button(mot_t1,value='     Ok     ')
	   mot_setuv = [-87,seq ,w ,0,0, 0,0, sx,sy]
	   widget_control,mot_t2,bad_id=i,set_uvalue=mot_setuv
;********
	endif else if seq ge 0 then begin
	   mot_t1=widget_base(title=ttl,kill_notify='',/tlb_size_events,map=0,$
							resource_name='scan')
	   mot_wn=widget_draw(mot_t1,/button_events,retain=2, $
			      xsize=sx,ysize=sy)
	   mot_wghinf(14,w)=mot_t1
	endif

;**	Drawing area
;**	------- ----
	if (seq ne 10) and (seq ne 11) then $
	 if seq ge  0 then  if mot_wg(w) gt 0 then bb=sl_wgdel(mot_wg(w))
	mot_wg(w)=mot_t1
	mot_wd(w)=mot_wn

;**	Realize and place
;**	------- --- -----
	mot_sz(w,0)=sx
	mot_sz(w,1)=sy
	mot_sz(w,2)= 0
	mot_sz(w,3)= 0
	if (mot_wg(w) ne mot_wd(w)) then begin
	  i=0
	  bid=sys_dep      ('DYNLAB',mot_wg(w),0,-10)
	  widget_control, bad_id=i,mot_wg(w),/realize
	  if mot_wdcur(7) gt 0 then $
	  		   widget_control,bad_id=i,mot_wg(w),group_leader=mot_wdcur(7)
	  if seq ne 1 then $
	  if x   ge 0 then widget_control,bad_id=i,mot_wg(w),map=1, $
					  tlb_set_xoffset=x,tlb_set_yoffset=y>7 $
	  else		   widget_control,bad_id=i,mot_wg(w),map=1

	  if (seq eq 7) or (seq eq 8) then begin
;					   bb=sl_wgsens(mot_wghinf(0,w),0)
;					   bb=sl_wgsens(mot_wghinf(1,w),1)
;					   bb=sl_wgsens(mot_wghinf(2,w),0)
				   	   if mot_wdcur(7) gt 0 then $
				   	      bb=sl_wgjreg(mot_wg(w))
					   endif
;**	  Get size
;**	  --- ----
	  i=0
	  widget_control, bad_id=i,mot_wg(w),tlb_get_size=j
	  if i eq 0 then  begin  mot_sz(w,0)=j(0) & mot_sz(w,1)=j(1) & endif

	endif else begin
	endelse

;**	Get draw window id into mot_w(w)
;**	--- ---- ------ -- ---- -------
	j=0
	widget_control, bad_id=i,mot_wd(w),get_value=j
	i=where(mot_w eq j,mot_t2)
	if mot_t2 ge 1 then mot_w(i)=0

	mot_w(w) =j

;**	Set U_values for base and draw
;**	--- -------- --- ---- --- ----
	mot_setuv = [-87,seq ,w ,mot_wd(w),mot_w(w) ,mot_t3,mot_t4, sx,sy]
	widget_control,mot_wg(w),bad_id=i,set_uvalue=mot_setuv
	widget_control,mot_wd(w),bad_id=i,set_uvalue=mot_setuv

return, mot_wg(w)
end
;
;
pro sl_poslider,base,w
;** ***********
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;**
	   mot_setuv(1)    =63
	   if (mot_str eq 'small') or (mot_str eq 'thin') then $
	   mot_t6=widget_slider(base  ,title='Contour levels' ,minimum= 2,maximum=100,value=12,$
	   							xsize=180,uvalue=mot_setuv) else $
	   mot_t6=widget_slider(base  ,title='Contour levels' ,minimum= 2,maximum=100,value=12,$
									  uvalue=mot_setuv)
	   mot_wghinf(43,w)=12
	   mot_wghinf(44,w)=mot_t6

	   mot_setuv(1)    =64
	   if mot_str ne ' ' then $
	   mot_t6=widget_slider(base  ,title='Z axis rotation',minimum=-1,maximum=359,value=30,$
	   							xsize=180,uvalue=mot_setuv) else $
	   mot_t6=widget_slider(base  ,title='Z axis rotation',minimum=-1,maximum=359,value=30,$
	   							          uvalue=mot_setuv)
	   mot_wghinf(45,w)=30
	   mot_wghinf(46,w)=mot_t6

	   mot_setuv(1)    =65
	   if mot_str ne ' ' then $
	   mot_t6=widget_slider(base  ,title='X axis rotation',minimum= 0,maximum=359,value=65,$
	   							xsize=180,uvalue=mot_setuv) else $
	   mot_t6=widget_slider(base  ,title='X axis rotation',minimum= 0,maximum=359,value=65,$
	   							          uvalue=mot_setuv)
	   mot_wghinf(47,w)=65
	   mot_wghinf(48,w)=mot_t6
return
end
;
;
function sl_wghandy, flg
;******* **********
;**
;**	flg = 2   set new text value
;**	flg = 3   map widget
;**
common  my_handy, hand_ini,hand_wg,hand_txt,hand_ttl,hand_x,hand_y,hand_scr
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;**
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
;**
if hand_ini gt 0 then begin
	if  hand_wg  gt 0 then  i=sl_wgvalid(hand_wg) else i=0
	if  i eq 1 then i=sl_wggetuv(hand_wg, mot_getuv)
	if  i eq 1 then if mot_getuv(1) ne hand_ini then i=0
	if  i eq 0 then begin
	   mot_t1=widget_base(title=hand_ttl,kill_notify='',map=0,resource_name='scan')
	   mot_t2=widget_text(mot_t1,value=hand_txt,xsize=28,ysize=hand_scr,$
				     font=m_ft0,/scroll)
	   mot_wg(hand_ini) = mot_t1
	   hand_wg	    = mot_t2
	   widget_control, bad_id=i,mot_t1,/realize
	   if mot_wdcur(7) gt 0 then $
	  		   widget_control,bad_id=i,mot_t1,group_leader=mot_wdcur(7)

	   mot_setuv = [-87,hand_ini,hand_ini,mot_t1, 0 ,0,0, 28,hand_scr]
	   widget_control, bad_id=i,hand_wg,set_uvalue=mot_setuv
	endif
	if (flg eq 2) then widget_control,bad_id=i,hand_wg,set_value=hand_txt
	if (flg eq 3) then widget_control,bad_id=i,mot_wg(hand_ini), map=1, $
				tlb_set_xoffset=hand_x,tlb_set_yoffset=hand_y>7
endif
return, 1
end
;
;
function sl_wgshow,wg, flg
;******* *********
;**
	if flg eq 0 then widget_control,bad_id=i,wg, show=0	else $
	if flg eq 1 then widget_control,bad_id=i,wg, show=1	else $
	if flg eq 2 then widget_control,bad_id=i,wg, iconify=0	else $
	if flg eq 3 then widget_control,bad_id=i,wg, iconify=1
return, 1
end
;
function sl_wgclear,wg
;******* **********
;**
	if wg gt 0 then widget_control ,wg, bad_id=i ,/clear_events
return, 1
end
;
function sl_wgevent,wg, flg
;******* **********
;**
;** for scan: wg always > 0

	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;**
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
;**
ab=0  & i=0

;**	Ask for an event  (/YIELD_TO_TTY do not work !!!)
;**
	 if  flg eq 0 then begin
	   if wg le 0 then mot_resev=widget_event(/nowait) $
		      else mot_resev=widget_event(wg,bad_id=i,/nowait)
	 endif else   begin
	   if wg le 0 then mot_resev=widget_event() $
		      else mot_resev=widget_event(wg,bad_id=i)
	 endelse

;**	Check event from xmanager bye scan_event
;**
	if mot_wdcur(9) gt 0 then if (wg lt 0) or (wg eq mot_wdcur(9))    then begin
							 mot_resev=mot_ev
							 mot_wdcur(9)=0 & endif
	if i ne 0 then ab=-1 else $
	if mot_resev.id gt 0 then begin
	 i=sl_wggetuv(mot_resev.id , mot_getuv)
	 if i eq 1 then begin
	    if mot_getuv(0) eq -88 then begin
;**	    Lamp event
;**	    ---- -----
	       ab=-88
	    endif  else begin
;**	      Mouse button or motion
;**	      ----- ------ -- ------
	      if tag_names(mot_resev,/structure) eq 'WIDGET_DRAW'   then begin
	         if mot_resev.press gt 0 then begin m_err=mot_resev.press
						    ab   =1 & endif
		 if mot_resev.type  eq 3 then	    ab   =1
	      endif
;**	      Size changed
;**	      ---- -------
	      if tag_names(mot_resev,/structure) eq 'WIDGET_BASE'   then begin
	         j= mot_getuv(2)
		 if (mot_resev.x le 0) or (mot_resev.y le 0) then ab=1
;resize		 mot_sz(j,2)=mot_resev.x - mot_sz(j,0)
;  option	 mot_sz(j,3)=mot_resev.y - mot_sz(j,1)
;    take off	 mot_sz(j,0)=mot_resev.x
;		 mot_sz(j,1)=mot_resev.y
;		 ab=1
	      endif
	      if tag_names(mot_resev,/structure) eq 'WIDGET_BUTTON' then begin
	         j= mot_getuv(1)
;**		 Scan    button
;**		 ----    ------
		 if  j eq 21 then bb=sl_wgsens(mot_resev.id,3)
;**		 Handies button
;**		 ------- ------
		 if  j eq 33 then bb=sl_wghandy(3)
;**		 Apply scale
;**		 ----- -----
		 if  j eq 61 then bb=sl_wgsens(mot_resev.id,3)
;**		 Slice   button
;**		 -----   ------
		 if  j eq 42 then if mot_resev.select eq 0 then j=420
;**		 Other   button
;**		 -----   ------
		 if  j ne 33 then ab=j
	      endif
	      if tag_names(mot_resev,/structure) eq 'WIDGET_SLIDER' then begin
	         j= mot_getuv(1)
;**		 Low , High scale
;**		 ---   ---- -----
		 if (j eq 60) or (j eq 62) then begin
			bb=sl_wgsens(mot_getuv(4),2)
			widget_control,bad_id=i,mot_resev.id,get_value=ab
			if j eq 60 then ab=ab+1000 $
				   else ab=ab+2000
		 endif
;**		 Levels contour
;**		 ------ -------
		 if (j eq 63) then begin
			widget_control,bad_id=i,mot_resev.id,get_value=ab
			ab=ab+1000
			mot_wghinf(43,mot_getuv(2))=ab & ab=0
		 endif
;**		 Z axis rotation
;**		 - ---- --------
		 if (j eq 64) then begin
			widget_control,bad_id=i,mot_resev.id,get_value=ab
			ab=ab+1000
			mot_wghinf(45,mot_getuv(2))=ab & ab=0
		 endif
;**		 X axis rotation
;**		 - ---- --------
		 if (j eq 65) then begin
			widget_control,bad_id=i,mot_resev.id,get_value=ab
			ab=ab+1000
			mot_wghinf(47,mot_getuv(2))=ab & ab=0
		 endif
	      endif
	    endelse
	 endif
	endif
return, ab
end
;
;
function sl_wgaccept,w, lab,typ,nb, m1,m2,m3,m4
;******* ***********
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;**
on_ioerror,mis
mot_t1=0
bb=0
	i= sl_wggetuv(mot_wg(w) , mot_getuv)
	if i eq 1 then begin
	   mot_t3=mot_getuv(5)
	   mot_t4=mot_getuv(6)
	   i =sl_wgsens(mot_t4,1)
	   if i eq 1 then  begin
	    widget_control,mot_t3,bad_id=i, set_value=lab
	    if typ eq 16 then begin
				mot_str= string(m1,'(i8)')
		if nb gt 1 then mot_str= mot_str+ ' ,'+string(m2,'(i6)')
		if nb gt 2 then mot_str= mot_str+ ' ,'+string(m3,'(i5)')
		widget_control, mot_t4,bad_id=i, set_value=mot_str ,/input_focus
	    endif else $
		widget_control, mot_t4,bad_id=i, set_value=m1      ,/input_focus
;**
	    bb= sl_wgevent(mot_t4,1)
;**
	    widget_control,mot_t4,bad_id=i, get_value=mot_str
	    bb=1
	    if typ ne 1 then bb=sl_ioreads(mot_str(0),typ,nb,m1,m2,m3,m4) $
			else m1=mot_str(0)
	    widget_control,mot_t3,bad_id=i, set_value='                     '
	    mot_t1=1
	   endif
	   bb=sl_wgsens(mot_t4,0)
	endif

	if mot_t1    eq 0 then begin
		bb =sl_iotype  (lab,typ,nb, m1,m2,m3,m4)
		bb =sl_iotype  (' ' ,0,0)
	  	bb =sl_ioaccept(lab,typ,nb, m1,m2,m3,m4)
	endif
mis:return, bb
end
;
;
function sl_tvlamp_base, flg,base_event
;******* **************
;**
;** 1 = base
;** 2 = event

	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
	if flg eq 1 then  mot_wdcur(7)=base_event
	if flg eq 2 then  mot_wdcur(8)=base_event
return, 1
end
;
;
function sl_tvwmaj, w ,vf,vm,rvl,rvm,f_fg,f_vu,spt,f_ax,f_az
;******* *********
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
if mot_f then begin
;**	Scales
;**	------
	if (vm gt vf) and (rvm gt rvl) then begin
	   mot_t5=long( (rvl-vf)*100/(vm-vf) )
	   mot_t6=long( (rvm-vf)*100/(vm-vf) )
	   if mot_t5 ne mot_wghinf(8,w) then if mot_wghinf(8,w) ge 0 then begin
		mot_wghinf(8,w)=mot_t5
		i=sl_wggetuv(mot_wghinf(5,w) , mot_getuv)
		if i eq 1 then begin
		   widget_control,bad_id=i,mot_wghinf(5,w),set_value=mot_t5
		   mot_str=strtrim(string(rvl),1)
		   if (rvl ge 0) and (rvl le 255) then $
		       if fix(rvl) eq rvl then mot_str=strtrim(string(fix(rvl)),1)
                   widget_control,bad_id=i,mot_getuv(5)   ,set_value=mot_str
		endif
	   endif
	   if mot_t6 ne mot_wghinf(9,w) then if mot_wghinf(9,w) ge 0 then begin
		mot_wghinf(9,w)=mot_t6
		i=sl_wggetuv(mot_wghinf(7,w) , mot_getuv)
		if i eq 1 then begin
		   widget_control,bad_id=i,mot_wghinf(7,w),set_value=mot_t6
		   mot_str=strtrim(string(rvm),1)
		   if (rvm ge 0) and (rvm le 255) then $
		       if fix(rvm) eq rvm then mot_str=strtrim(string(fix(rvm)),1)
		   widget_control,bad_id=i,mot_getuv(5)   ,set_value=mot_str
		   bb=sl_wgsens(mot_getuv(4),2)
		endif
	   endif
	endif
;**	Levels contour
;**	------ -------
	val=f_fg(15)
	if val ne mot_wghinf(43,w) then begin
	       if mot_wghinf(43,w) gt 500 then begin
	       	  f_fg(15)=mot_wghinf(43,w)-1000   &  mot_wghinf(43,w)=f_fg(15)
	       endif else begin
		  mot_wghinf(43,w)=val
		  widget_control,bad_id=i,mot_wghinf(44,w),set_value=val
	       endelse
	endif
;**	Z axis rotation
;**	- ---- --------
	val=f_az
	if val ne mot_wghinf(45,w) then begin
	       if mot_wghinf(45,w) gt 500 then begin
	       	  f_az    =mot_wghinf(45,w)-1000   &  mot_wghinf(45,w)=f_az
	       endif else begin
		  mot_wghinf(45,w)=val
		  widget_control,bad_id=i,mot_wghinf(46,w),set_value=val
	       endelse
	endif
;**	X axis rotation
;**	- ---- --------
	val=f_ax
	if val ne mot_wghinf(47,w) then begin
	       if mot_wghinf(47,w) gt 500 then begin
	       	  f_ax    =mot_wghinf(47,w)-1000   &  mot_wghinf(47,w)=f_ax
	       endif else begin
		  mot_wghinf(47,w)=val
		  widget_control,bad_id=i,mot_wghinf(48,w),set_value=val
	       endelse
	endif
;**	Logarithm
;**	---------
	val=f_fg(0)
	if val  ne  mot_wghinf(11,w) then begin
	   mot_t4=  mot_wghinf(10,w)
	   if val eq 1  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(11,w)=val
	endif
;**	Smooth
;**	------
	val=f_fg(12)
	if val  ne  mot_wghinf(13,w) then begin
	   mot_t4=  mot_wghinf(12,w)
	   if val eq 1  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(13,w)=val
	endif
;**	Slice
;**	-----
	val=f_fg(24)
	if val  ne  mot_wghinf(37,w) then begin
	   mot_t4=  mot_wghinf(36,w)
	   if val eq 1  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(37,w)=val
	endif
;**	Square off
;**	------ ---
	val=f_fg(10)
	if val  ne  mot_wghinf(53,w) then begin
	   mot_t4=  mot_wghinf(52,w)
	   if val eq 1  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(53,w)=val
	endif
;**	Scale frames
;**	----- ------
	val=f_fg(22)
;**	Representation
;**	--------------
	val=spt
	if val  ne  mot_wghinf(38,w)  then begin
	   widget_control,bad_id=i ,  mot_wghinf(51,w),set_button=0
	   if val eq  1 then mot_t4=  mot_wghinf(39,w)
	   if val eq -1 then mot_t4=  mot_wghinf(40,w)
	   if val eq  0 then mot_t4=  mot_wghinf(41,w) $
	                else mot_t4=  mot_wghinf(42,w)
	   widget_control,bad_id=i ,  mot_t4,set_button=1
	   mot_wghinf(38,w)=val
	endif
;;**	G_H represent
;**	--- ---------
	val=f_vu
	if val  ne  mot_wghinf(15,w)  then begin
	   widget_control,bad_id=i ,  mot_wghinf(49,w),set_button=0
	   if val eq  3 then mot_t4=  mot_wghinf(16,w)
	   if val eq  2 then mot_t4=  mot_wghinf(17,w)
	   if val eq  1 then mot_t4=  mot_wghinf(18,w)
	   if val eq  5 then mot_t4=  mot_wghinf(19,w)
	   if val eq  4 then mot_t4=  mot_wghinf(20,w)
	   widget_control,bad_id=i ,  mot_t4	      ,set_button=1
	   mot_wghinf(15,w)=val
	endif
;**	G_H autoscale
;**	--- ---------
	val=f_fg(5)
	if val  ne  mot_wghinf(21,w) then begin
	   mot_t4=  mot_wghinf(23,w)
	   if val eq 1  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(21,w)=val
	endif
;**	G_H smooth
;**	--- ------
	val=f_fg(12)
	if val  ne  mot_wghinf(25,w) then begin
	   mot_t4=  mot_wghinf(29,w)
	   if val gt 0  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(25,w)=val
	endif
;**	G_H log
;**	--- ---
	val=f_fg(0)
	if f_fg(5) ne 0 then val=0
	if val  ne  mot_wghinf(22,w) then begin
	   mot_t4=  mot_wghinf(24,w)
	   if val gt 0  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(22,w)=val
	endif
;**	G_H sections
;**	--- --------
	val=f_fg(8)
	if val  ne  mot_wghinf(33,w) then begin
;	   widget_control,bad_id=i ,  mot_wghinf(50,w),set_button=0
	   mot_t4=  mot_wghinf(34,w)
	   if val eq 2  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_t4=  mot_wghinf(35,w)
	   if val eq 1  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(33,w)=val
	endif
;**	G_H project
;**	--- -------
	val=f_fg(9)
	if val  ne  mot_wghinf(26,w) then begin
	   mot_t4=  mot_wghinf(30,w)
	   if val eq 1  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(26,w)=val
	endif
;**	G_H arrows
;**	--- ------
	val=f_fg(27)
	if val  ne  mot_wghinf(28,w) then begin
	   mot_t4=  mot_wghinf(32,w)
	   if val eq 1  then widget_control,bad_id=i,mot_t4,set_button=0 $
			else widget_control,bad_id=i,mot_t4,set_button=1
	   mot_wghinf(28,w)=val
	endif
;**	G_H ellips
;**	--- ------
	val=f_fg(31)
	if val  ne  mot_wghinf(27,w) then begin
	   mot_t4=  mot_wghinf(31,w)
	   if val eq 1  then widget_control,bad_id=i,mot_t4,set_button=1 $
			else widget_control,bad_id=i,mot_t4,set_button=0
	   mot_wghinf(27,w)=val
	endif
endif
return, 1
end
;
;
;************************************************ FIRST  level ************
;
function sl_tvset ,flg , val ,v2,v3,v4,v5,v6,v7
;*******
;**
;**	Set graphic's variables.
;**	--- --------- ---------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
;string for mtitle...
case flg of
;	 plotting
;	 --------
	1:!P.color	=val
	4:!P.noerase	=val
	6:!P.font  	=val
	8:begin
	  !X.style	=val +V3   +V5*8 +V6*4
	  !Y.style	=v2  +V4   +V5*8 +V7*4
	  end
	9:!P.psym  	=val
	13:!X.ticks     =val
	14:!Y.ticks  	=val
	15:!P.title  	=val
	18:!P.linestyle	=val
	21:!P.noclip	=val
	35:!P.thick	=val
;	 contour
;	 -------
	 2:ms_bcolor	=val
	17:ms_ncount	=val
;	 image
;	 -----
	 7:!order 	=val
	22:!quiet	=val
	else:
endcase
return,1
end
;
;
function sl_tvget ,flg , val
;*******
;**
;**	Get graphic's variables.
;**	--- --------- ---------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;**
;string for stime...
case flg of
	1:val = !P.color
	3:begin
	  val = !D.window
	  if mot_f then if val gt 0 then begin
			mot_wn=where(mot_w eq val,i)
			if i eq 1 then val=mot_wn(0) else val=0
			endif
	  end
	4:val = !P.noerase
	6:val = !P.font
	7:val = !order
	8:val = !X.style
	9:val = !P.psym
	16:val = systime(0)
	17:val = ms_ncount
	18:val = !P.linestyle
	21:val = !P.noclip
	23:val = !D.flags
	24:val = !Version.arch
	25:val = !Version.os
	26:val = fix(!D.X_Px_cm)
	27:val = fix(!D.Y_Px_cm)
	28:begin
	   val = !D.X_Vsize
	   if mot_f then if !D.window  gt 0 then begin j=!D.window
		 mot_wn=where(mot_w eq j,i)
		 if i eq 1 then j= mot_wn(0) else j=0
	         if j gt 0 then if mot_wg(j) gt 0 then val=val+mot_sz(j,2)
	   endif
	   end
	29:begin
	   val = !D.Y_Vsize
	   if mot_f then if !D.window  gt 0 then begin j=!D.window
		 mot_wn=where(mot_w eq j,i)
		 if i eq 1 then j= mot_wn(0) else j=0
	         if j gt 0 then if mot_wg(j) gt 0 then val=val+mot_sz(j,3)
	   endif
	   end
;	30:val = !Display_size.x
; 	31:val = !Display_size.y
 	32:val = !D.N_Colors<256
 	33:val = !D.Name
	34:val = !stime
	35:val = !P.thick
	else:
endcase
return,1
end
;
function sl_x	,str
;******* ****
;**
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;**
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
;**
	common  machin,	mc_sys,mc_sta
;**
	common  my_x,	x_foc,x_st1,x_st2,x_tmp,x_fso,x_p0,x_p1,x_p2,x_bb
;**
	if x_foc eq -100 then begin
			x_foc=-1
	   		t_sx =long(0) & t_sy=long(0) & m_colo=long(0)
			x_bb =long(0)
			x_bb =call_external(x_fso,'sl_x_parse',long(3),$
							       t_sy,   $
							       t_sx,   $
							       m_colo)
			if x_bb eq 1 then begin
				t_sx =t_sx - 40
				t_sy =t_sy - 54
				x_foc=0
			endif
	endif

	if x_foc ge 0 then begin
	  case str of
	 'getkb':	x_bb=call_external(x_fso,'sl_x_parse',x_p0,x_st1,x_st2)
	 'open':
	 'focus_in':	if x_foc eq 0  then begin
			   x_foc   =call_external(x_fso,'sl_x_parse',x_p1)
			   if x_foc lt 0 then x_foc=0
			endif
	 'focus_out':	if x_foc ge 0  then begin
			   x_bb    =call_external(x_fso,'sl_x_parse',x_p2)
			   x_foc   =0  & endif
	 'focus_clear':	begin x_bb =call_external(x_fso,'sl_x_parse',x_p2)
			      x_foc=2  & end
	 'focus_reset':	begin x_bb =call_external(x_fso,'sl_x_parse',x_p2)
			      x_foc= 0 & end
	 'kb_check':	   x_bb    =call_external(x_fso,'sl_x_parse',long(5))
	 'close':	   x_bb    =call_external(x_fso,'sl_x_parse',long(4))
	  else:
	  endcase

	endif else begin ;*** get_kbrd ***
	  case str of
	 'focus_in':	if x_foc ne -2 then begin sl_wgfocus, t_w & x_foc=-2 & endif
;	 'focus_in':	x_foc=-2
	  else:		x_foc=-1
	  endcase
	endelse
return, x_bb
end
;
function sl_tviokey,	wt,nc
;******* **********
	common  my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;**
	common  my_x,	x_foc,x_st1,x_st2,x_tmp,x_fso,x_p0,x_p1,x_p2,x_bb

	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;**
	if x_foc ne 1 then begin
		x_tmp=''
		if x_foc eq -2 then begin
					widget_control,mot_wghinf(6,t_w),bad_id=ii,get_value=x_tmp & x_tmp=x_tmp(0)
					if x_tmp ne '' then begin
					widget_control,mot_wghinf(6,t_w),bad_id=ii,set_value=''
					nc=strlen(x_tmp) & if nc eq 1 then nc=0
					endif
					;x_tmp=get_kbrd(wt) & if x_tmp gt '' then sl_wgfocus, t_w ;ok for Windows
		endif
		return,x_tmp
	endif else begin
		x_st1	= byte(0)
		x_st2	= long(0)
		bb = sl_x('getkb')
		if x_st2 eq 0	then return,string(x_st1) $
		else begin
		     nc=5
		     case x_st2 of
;		F1
		65470:		x_tmp='[11~'

;		F2
		65471:		x_tmp='[12~'

;		F3
		65472:		x_tmp='[13~'

;		F4
		65473:		x_tmp='[14~'

;		F5
		65474:		x_tmp='[15~'

;		F6
		65475:		x_tmp='[17~'

;		F7
		65476:		x_tmp='[18~'

;		F8
		65477:		x_tmp='[19~'

;		F9
		65478:		x_tmp='[20~'

;		F10
		65479:		x_tmp='[21~'

;		F11
		65480:		x_tmp='[23~'
		268828432:	x_tmp='[23~'

;		F12
		65481:		x_tmp='[24~'
		268828433:	x_tmp='[24~'

;		F13
		65482:		x_tmp='[25~'

;		F14
		65483:		x_tmp='[26~'

;		F16 Copy
		65485:		x_tmp='D   '

;		F17
		65486:		x_tmp='[31~'

;		F18
		65487:		x_tmp='[32~'

;		F19
		65488:		x_tmp='[33~'

;		F20
		65489:		x_tmp='[34~'
		65513:		x_tmp='[34~'

;		Help
		65386:		x_tmp='[28~'

;		Do, Print
		65377:		x_tmp='[29~'
		65378:		x_tmp='[29~'
		65301:		x_tmp='[29~'
		65491:		x_tmp='[29~'

;		Break
		65387:		x_tmp='65387'
		65299:		x_tmp='65387'
		65490:		x_tmp='65387'

;		Menu
		65383:		x_tmp='65383'

;		Insert
		65379:		x_tmp='[2~ '
		268500850:	x_tmp='[2~ '

;		Find, Home
		65384:		x_tmp='[1~ '
		65360:		x_tmp='[1~ '

;		Remove, Delete
		268500786:	x_tmp='[3~ '
		268500851:	x_tmp='[3~ '

;		Select, End
		65376:		x_tmp='[4~ '
		65367:		x_tmp='[4~ '

;		PageUp
		65365:		x_tmp='[5~ '

;		PageDown
		65366:		x_tmp='[6~ '

;		Up
		65362:		x_tmp='[A  '

;		Down
		65364:		x_tmp='[B  '

;		Left
		65361:		x_tmp='[D  '

;		Right
		65363:		x_tmp='[C  '

;		Enter
		65421:		x_tmp='OM  '

;		PF1
		65425:		x_tmp='OP  '

;		PF2
		65426:		x_tmp='OQ  '

		else:		x_tmp=''
		endcase
		return,		x_tmp
		endelse
	endelse
end
;
;
function sl_tvldcol ,red,green,bleue
;******* **********
;**
;**	Load color vectors.
;**     ---- ----- -------
	tvlct ,red,green,bleue
return, 1
end
;
;
function sl_tvgtcol ,red,green,bleue
;******* **********
;**
;**	Load color vectors.
;**     ---- ----- -------
	tvlct ,red,green,bleue ,/get
return, 1
end
;
;
function sl_tvloadct ,id,red,green,bleue
;******* ***********
;**
;**	Load color table.
;**     ---- ----- -----
	if id ge 0 then begin   loadct,id,/silent
				bb=sl_tvgtcol (red,green,bleue)
				endif
return, 1
end
;
;
function sl_tvcur_w,	w,gh,inf ,fs,sx,sy
;******* **********
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
	if w   ge 0  then mot_wdcur(0)=w
	if gh  ge 0  then mot_wdcur(1)=gh
	if inf ge 0  then mot_wdcur(2)=inf

	if fs  eq 1  then begin mot_wdcur(3)=sx & mot_wdcur(4)=sy & endif
	if fs  eq 2  then begin mot_wdcur(5)=sx & mot_wdcur(6)=sy & endif
return, 1
end
;
;
function sl_tvwis  ,w,fl
;******* ********
;**
;**	Check if window exist
;**
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;**
	if fl eq 1 then $
	if sys_dep('MACHINE') ne 'mac'	then device,window_state=ms_tbwis

	i =sl_element(ms_tbwis)

	mot_wn=mot_w(w)
	if (i gt mot_wn) then bb=ms_tbwis(mot_wn) else bb=0
	if sys_dep('MACHINE') eq 'mac'	then bb=1
return, bb
end
;
;
function sl_tvshap ,fl
;******* *********
;**
;**	Cursor shape.
;**     ------ -----
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
;
	if fl lt 0 then i=46 else i=fl
	if ms_devs eq 1 then device, cursor_standard=i  else $
	if ms_devs eq 2 then device,/cursor_original
return, 1
end
;
;
function sl_tvclear, dummy
;******* **********
;**
;**	Clear current window.
;**	----- ------- ------
	erase
	return,1
end
;
;
function sl_tvdelwn ,wn
;******* **********
;**
;**	Suppress a window.
;**     -------- - ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;
	if mot_wg(wn) gt 0 then begin
	   bb=sl_wgdel(mot_wg(wn))
	   mot_wg(wn)=-1
	   mot_wghinf(*,wn)=-1
	endif else if mot_w(wn) gt 0 then wdelete  ,mot_w(wn)
	if t_w eq wn then t_w=-1
	ms_tbwin (wn)=0
	mot_w(wn)=0
return ,1
end
;
;
function sl_tvfreewn ,wn
;******* ***********
;**
;**	Suppress a window.
;**     -------- - ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur

	if mot_wg(wn) gt 0 then begin
	   mot_wg(wn)=-1
	   mot_wghinf(*,wn)=-1
	endif
	if t_w eq wn then t_w=-1
	ms_tbwin (wn)=0
	mot_w(wn)=0

;	bb=sl_tvdelwn(wn)

return ,1
end
;
;
function sl_tvsel ,w
;******* ********
;**
;**	Select a window.
;**	------ - ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
bb=0
	if w ge 0 then  if (ms_tbwin(w) gt 0) or (w eq 0) then begin
			  bb=sl_tvwis(w,1)
			  if (bb eq 1) and (ms_tbwin(w) ne 101) then begin
				  wset,mot_w(w) & t_w =w
			  endif
			endif
return,bb
end
;
;
function sl_tvsels ,w
;******* *********
;**
;**	Select a window.
;**	------ - ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
bb=0
	if w ge 0 then  if (ms_tbwin(w) gt 0) or (w eq 0) then begin
			  bb=sl_tvwis(w,0)
			  if (bb eq 1) and (ms_tbwin(w) ne 101) then begin
				  wset,mot_w(w) & t_w =w
			  endif
			endif
return,bb
end
;
;
function sl_tvtidy ,w ,flg
;******* *********
;**
;**	Tidy a window.
;**	---- - ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
bb=0
	if w ge 0 then  if ((ms_tbwin(w) gt 0) or (w eq 0)) and $
			    (ms_tbwin(w) ne 101)     then begin
			  bb=sl_tvwis(w,1)
			  if bb eq 1 then $
				if mot_wg(w) gt 0 then begin
					   if flg eq 1 then $
					    bb=sl_wgshow(mot_wg(w),3)  else $
					    bb=sl_wgshow(mot_wg(w),0)
					   bb= sl_wgsens(mot_wghinf(0,w),0)
					   bb= sl_wgsens(mot_wghinf(1,w),1)
					   bb= sl_wgsens(mot_wghinf(1,w),2)
					   bb= sl_wgsens(mot_wghinf(2,w),0)
				endif else if flg eq 1 then $
					    wshow,mot_w(w),0 ,iconic=1 else $
					    wshow,mot_w(w),0 ,iconic=0
			  mot_wdcur(0)=-1
			endif
return,bb
end
;
;
function sl_tvwake ,w
;******* *********
;**
;**	Expand a window.
;**	------ - ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
bb=0
	if w ge 0 then  if ((ms_tbwin(w) gt 0) or (w eq 0)) and $
			    (ms_tbwin(w) ne 101)     then begin
			  bb=sl_tvwis(w,1)
			  if bb eq 1 then $
				if mot_wg(w) gt 0 then begin
					    bb=sl_wgshow (mot_wg(w),2)
					    bb=sl_wgclear(mot_wg(w))
					    bb=sl_wgsens (mot_wghinf(0,w),1)
					    bb=sl_wgsens (mot_wghinf(1,w),3)
					    bb=sl_wgsens (mot_wghinf(2,w),1)
					    bb=sl_wgevent(mot_wg(w),0)
				endif  else wshow,mot_w(w),1 ,iconic=0
				mot_wdcur(8)=0
			endif
return,bb
end
;
;
function sl_tvlux ,w , sx,sy ,ttl ,op1,op2,op3,op4,op5,op6,op7,op8, x,y ,seq
;******* ********
;**
;**	Create a window.
;**	------ - ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common	my_tvi ,t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
bo=1
	opt=' '
	if  op1 eq 1  then opt=opt+'Top '	else $
	if  op1 eq 2  then opt=opt+'Bottom '
	if  op2 eq 1  then opt=opt+'Left '	else $
	if  op2 eq 2  then opt=opt+'Right '	else $
	if  op2 eq 3  then opt=opt+'Center '
	if  op3 eq 1  then opt=opt+'NoBanner '
	if  op4 eq 1  then opt=opt+'NoBorder '
	if  op5 eq 1  then opt=opt+'NoMenu '	else $
	if  op5 eq 2  then opt=opt+'Menu '
;
	if (seq ne 10) and (seq ne 11) then begin
;	 if seq ge  0 then  if mot_wg(w) gt 0 then bb=sl_wgdel(mot_wg(w))
	 mot_w(w) = w
;	 mot_wg(w)=-1
	 mot_wghinf(*,w)=-1
	endif

	if  op7 ne 0  then window , w,xsize=sx,ysize=sy,colors=op7  $
	else begin
	  if mot_f then begin
	    if x lt 0 then begin i=-1    & j=-1
	    endif     else begin i= x-25 & j= t_sy-sy-y
				 if j gt t_sy-100 then j=t_sy-100
				 if j lt 25 then j=25
				 if i gt t_sx-100 then i=t_sx-100
				 if i le 1  then i=2
			 endelse
	    bo=sl_wglux(w,sx,sy,ttl,i,j,(seq+0),t_sx,t_sy)
	  endif

	  if mot_wg(w) gt 0 then begin  ms_tbwin(w)=1
	  				bb=sl_tvsel(w)
					bb=sl_tvclear(dum)
	  endif else begin
	    if (op8 eq 1) and (ms_devs eq 1) $
		        then window , w,xsize=sx,ysize=sy,/pixmap     else    $
	    if x  lt 0  then window , w,xsize=sx,ysize=sy,title=ttl,retain=2  $
	    else window , w ,xsize=sx,ysize=sy,title=ttl,retain=2,xpos=x,ypos=y
	  endelse
	endelse
	if (op8 eq 1) then ms_tbwin(w)=101 else ms_tbwin(w)=1
	t_w	 = w
;
	return,bo
end
;
;
function sl_tvimag, area,vsiz,x,y
;******* *********
;**
;**	Display image video.
;**	------- ----- -----
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
if !quiet  eq 2 then begin
test=sl_size(area)
if (test(0) ne vsiz(0)) or (test(1) ne vsiz(1)) or $
   (test(test(0))   ne vsiz(vsiz(0))) or $
   (test(test(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype('sl_tvimag size',0,0)
endif
;
	if  (t_pix eq 1) and (t_piy eq 1) then tv,area,x,y $
	else tv,area,x,y,/device,xsize=vsiz(1)*t_pix,ysize=vsiz(2)*t_piy
return, 1
end
;
;
function sl_tvsimag, area,vsiz,x,y
;******* *********
;**
;**	Display image video.
;**	------- ----- -----
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;
	if  (t_pix eq 1) and (t_piy eq 1) then tvscl,area,x,y $
	else tvscl,area,x,y,/device,xsize=vsiz(1)*t_pix,ysize=vsiz(2)*t_piy
return, 1
end
;
;
function sl_tvmerr,   dum
;******* *********
;**
;**	Get the click button number.
;**	--- --- ----- ------ ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
;
	bb=m_err
	if  bb lt 0 then bb=-bb
	if (bb eq 1) or (bb eq 4) then bb=5-bb
	m_err= 0
return, bb
end
;
;
function sl_tvwinp ,x,y
;******* *********
;**
;**	Get window device position.
;**	--- ------ ------ --------
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;**
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
	x=0  & y=0
	if t_w ge 0  then if ms_tbwin(t_w) ne 101 then $
	  if ms_devs eq 1  then begin device,get_window_position=ms_v2
				      x=ms_v2(0)
				      y=ms_v2(1)
				      if mot_f then y=t_sy-y
				endif
	ms_v2(0)=0
	ms_v2(1)=0
return, 1
end
;
;
function sl_tvwait, tim ,motion,button,wind ,rflag ,rw
;******* *********
;**
;** TIM to wait in seconds
;** MOTION = 1 means  wakup if cursor moved.
;** BUTTON = 1 means  wakup if button pressed.
;** BUTTON = 2 means  wakup if keyboard pressed.
;** WIND   =-1 if event from any window else = Window ID.
;** RFLAG       return type of event to be processed by nacs.
;** RW		return window of event
;** in any case wakup if keyboard pressed is not faisable if MOTION=1.
;** if tim = 0  motion may be important.
;**
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;
	rflag=0 & rw=0
	sleep=tim
	if mot_f then begin
	 if motion+button ne 0 then begin
	  if  sleep gt 0  then begin
	   if (wind ge 0) then i=mot_wg(wind) else i=0
;**	   Check if an event in the queue
;**	   ----- -- -- ----- -- --- -----
	   ab=0
	   j =i
	   wait_flag= 0
	   wait_w   =-1
	   if (motion eq 1) and (button ne 2) then begin
;solution to   find	 wait_flag= 1
			 if wind lt 0 then wait_w=-wind-1 else wait_w=wind
			 bb=sl_wgmotion(mot_wd(wait_w),1)
	   endif

	   if i gt 0 then begin
			 ab=sl_wgevent(i,wait_flag)
	   endif else if wind lt 0 then begin
		   for   i=0,t_max-1 do begin
			 if  mot_wg(i) gt 0 then $
			  if mot_wg(i) ne mot_wd(i) then $
			     if i ne wait_w then ab=sl_wgevent(mot_wg(i),0) $
			     		    else ab=sl_wgevent(mot_wg(i),wait_flag)
			 if ab ne 0 then  begin
			    if ab eq -1 then mot_wg(i)=0
			    j =i  & i=t_max
			 endif
		   endfor
	   endif

	   if wait_flag eq 1 then bb=sl_wgmotion(mot_wd(wait_w),0)
;**
;**	   Test also for lamp event
	   if ab eq 0 then  $
		      if  mot_wdcur(7) gt 0 then $
		       if mot_wdcur(8) ne 0 then begin  ab=mot_wdcur(8)
							j =mot_wdcur(0)
					 if wind lt 0 then mot_wdcur(8)=0
		       endif else ab=sl_wgevent(mot_wdcur(7),0)
;**

	   if ab ne 0 then begin     sleep=0 & rflag=ab & rw=j
	   endif

	  endif
	 endif
	endif
	if sleep le 0 then			   return,1 else $
	if (not mot_f) or (mot_wdcur(7) le 0) then return,  sl_wait(tim) $
	else begin
;		ab=sl_wgtimer(mot_wdcur(7),tim)
;		ab=sl_wgevent(mot_wdcur(7),1)
						   return,  sl_wait(tim)
	endelse
end
;
;
function sl_tvgcur ,x,y, button ,wait
;******* *********
;**
;**	Get cursor device position.
;**	--- ------ ------ --------
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
	cursor  ,x,y,wait,/device
	if m_err le 0 then bb =sl_sysget(19,m_err)
	if mot_f then if m_err gt 0 then begin
		 bb=sl_tvget(3,w)
		 if w ge 0 then bb=sl_wgclear(mot_wd(w))
		 if w ge 0 then    sl_wgfocus,w
	endif
	button  =sl_tvmerr(0)

return, 1
end
;
;
function sl_tvmcur,  flg ,x,y
;******* *********
;**
;**	Manipulate the cursor.
;**	---------- --- ------
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;**
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	if t_w ge  0 then if ms_tbwin(t_w) ne 101 then $
	   if sys_dep('MACHINE') ne 'mac' then $
	      if flg eq 2 then tvcrs,x,y,/device else tvcrs,flg
return, 1
end
;
;
function sl_tvread ,x,y, dx,dy
;******* *********
;**
;**	Read pixels.
;**	---- ------
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;**
	if t_rd ne 0 then return,tvrd(x,y,dx,dy) else return,0
end
;
;
function sl_tvpop ,w,fl
;******* ********
;**
;**	put window to the front or to the back.
;**	--- ------ -- --- ----- -- -- --- ----
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;**
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;
	if w ge 0 then  if ms_tbwin(w) ne 101 then $
			if mot_wg(w) gt 0 then begin
			       bb=sl_wgshow(mot_wg(w),fl)
			endif  else  wshow ,mot_w(w) ,fl
return, 1
end
;
;
function sl_tvdmenu,  wm
;******* **********
;**
;**	Suppress  a menu.
;**	--------  - ----
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;
	if mot_wg(wm) gt 0 then begin
			   bb=sl_wgdel(mot_wg(wm))
			   mot_wg(wm)=-1
			   mot_wghinf(*,wm)=-1
	endif  else if ms_tbwin(wm) ne 100 then if mot_w(wm) gt 0 then wdelete ,mot_w(wm)
	ms_tbwin(wm)=100
	mot_w(wm)=0
return, 1
end
;
;
;
function sl_tvs, x,y, text, siz, deg, col
;******* ******
;**
;**	Output a text.
;**	------ - ----
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
;**
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
;**
	common  machin,	mc_sys,mc_sta
;**
	if (mc_sys eq 'unix') and (mc_sta eq 'dec') then $
	   	xyouts,x,y,text,/device,orient=deg,col=ms_ncol/2, $
			         charsize=1.+(siz-1.)/3.  $
	else $
	if col lt 0 then $
		xyouts,x,y,text,/device,orient=deg,col=m_colo-1-m_colo/4, $
				 charsize=1.+(siz-1.)/3.  $
	else 	xyouts,x,y,text,/device,orient=deg,col=col, $
				 charsize=1.+(siz-1.)/3.
return, 1
end
;
;
function sl_tvt, x,y, text, siz, deg, col
;******* ******
;**
;**	Output a text with special car.
;**	------ - ---- ---- ------- ---
bb=sl_tvget(6,ft)
bb=sl_tvset(6,-1)

if col lt 0 then $
	xyouts,x,y,text,/device,orient=deg, $
		    charsize=siz,charthick=siz $
else	xyouts,x,y,text,/device,orient=deg,col=col, $
		    charsize=siz,charthick=siz
bb=sl_tvset(6,ft)
return, 1
end
;
;
function sl_tvxyz,	xmin,xmax,ymin,ymax
;******* ********
;**
;**	Scale into the window region.(Care parameters type)
;**	----- ---- --- ------ ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common   my_eras  ,zr,zr_z,zrv2,zrv6,zx4,zy4
;**
	if (xmin ne xmax) and (ymin ne ymax)    then $
		 set_xy,xmin,xmax,ymin,ymax	else set_xy
;**
;	ms_xrg(0)= xmin
;	ms_xrg(1)= xmax
;	ms_yrg(0)= ymin
;	ms_yrg(1)= ymax
;	plot,zrv2,/nodata,/noerase,xst=4,yst=4 $
;			,xrange=ms_xrg,yrange=ms_yrg
return, 1
end
;
;
function sl_tvscreen,	xmin,xmax,ymin,ymax
;******* ***********
;**
;**	Map a window region.
;**	--- - ------ ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
;**
;	set_screen,     xmin,xmax,ymin,ymax
;**
	if (xmin ne xmax) and (ymin ne ymax) then begin
	ms_xypos(0)=xmin
	ms_xypos(1)=ymin
	ms_xypos(2)=xmax
	ms_xypos(3)=ymax
	plot,ms_v2,/nodata,/noerase,xst=4,yst=4   $
		       ,/device,position=ms_xypos,xmargin=ms_v2,ymargin=ms_v2
	endif
return, 1
end
;
;
function sl_tvaxis,	mn,mx,flg,lab,sz,ttl
;******* *********
;**
;**	Draw  axis.
;**	----  ----
	if flg eq 0 then axis,yaxis=0,yrange=[mn,mx],ytitle=lab, $
			      charsize=sz,charthick=sz     else  $
	if flg eq 2 then axis,yaxis=1,yrange=[mn,mx],ytitle=lab, $
			      charsize=sz,charthick=sz     else  $
	if flg eq 4 then axis,xaxis=0,xrange=[mn,mx],xtitle=lab, $
			      charsize=sz,charthick=sz     else  $
	if flg eq 6 then axis,xaxis=1,xrange=[mn,mx],xtitle=lab, $
			      charsize=sz,charthick=sz
return, 1
end
;
;
function sl_tvline, vx,vy,vs ,mode,col
;******* *********
;**
;**	Draw a vector. (vs=size)
;**	---- - ------
	if col lt 0 then  plots,vx,vy,/device  else $
			  plots,vx,vy,/device, color=col
return, 1
end
;
;
function sl_tvplt,	flg, nx,x , ny,y
;******* ********
;**
;**	Draw a vector.
;**	---- - ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	if flg eq -1 then begin if ny gt 0 then oplot  , x,y $
					   else oplot  , x
			  endif else $
	if flg eq -2 then begin if ny gt 0 then plot_io, x,y $
		,/device,position=ms_xypos,xmargin=ms_v2,ymargin=ms_v2 $
					   else plot_io, x   $
		,/device,position=ms_xypos,xmargin=ms_v2,ymargin=ms_v2
	endif
return, 1
end
;
;
function sl_tvfill,	x0,x,xn , y0,y,yn , col,inc,ang
;******* *********
;**
;**	Fill a region.
;**	---- - ------
	polyfill , [x0,x,xn],[y0,y,yn],/data,col=col,spac=1.*inc/30.,orient=ang
return, 1
end
;
function sl_tvpol,	n , vx , vy , colpat , cp
;******* ********
;**
;**	Fill a polygon
;**	---- - -------
	if cp gt 0 then polyfill,vx,vy,/dev,pattern=colpat
	if cp eq 0 then polyfill,vx,vy,/dev,col    =colpat
return, 1
end
;
;
function sl_tvdev,	dev
;******* ********
;**
;**	Choice output device.
;**	------ ------ ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	if dev eq -1 then begin
			  set_plot,ms_devm(ms_devp) & ms_devs=ms_devp
	endif  else begin set_plot,ms_devm(   dev ) & ms_devs=   dev & endelse
return, 1
end
;
;
function sl_tvfont ,n
;******* *********
;**
	common  machin,	mc_sys,mc_sta
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
;**
;sys_dep
;	if n eq 0 then device,font=m_ft0 else $
;	if n eq 1 then device,font=m_ft1
return, 1
end
;
;
function sl_tvclass ,n
;******* **********
;**
	if n eq 8 then ii=sys_dep('PSEUDO')
return, 1
end
;
;
function sl_tvsiz ,v2
;******* ********
;**
	device,get_screen_size=v2
return ,1
end
;
;
function sl_tvend ,dum
;******* ********
;**
	device,/close_display
	bb=sl_x('close')
return ,1
end
;
;
function sl_tvmod ,it,mode
;******* ********
;**
;**	Set writing mode.
;**     --- ------- ----
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
;
	if mode ge 0 then begin
		if mode eq 10 then ms_dmod(1)=sys_dep('INVERT') $
			      else ms_dmod(1)=mode
	endif	else		   ms_dmod(1)=ms_dmod(0)
	if ms_devs eq 0 then begin
	   if ms_dmod(1) eq 0 then opt ='ERAS' 	 else $
	   if ms_dmod(1) eq 2 then opt ='COMP'	 else $
	   if ms_dmod(1) eq 3 then opt ='REPL'	 else $
	   if ms_dmod(1) eq 6 then opt ='COMP'	 else $
	   if ms_dmod(1) eq 7 then opt ='OVER'   else $
				   opt ='REPL'
;Vers.1	   if it eq 1 then device,set_graphics=opt else device,set_image=opt
	endif  else $
	if ms_devs eq 1 then device,set_graphics  =ms_dmod(1)  else $
	if ms_devs eq 2 then device,set_graphics  =ms_dmod(1)
return, 1
end
;
;
function sl_tvmov,  int7
;******* ********
;**
;**	Move a region in current window.
;**	---- - ------ -- ------- ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;
	if ms_devs eq 0 then begin
	   int7(2)=int7(0)+int7(2)-1   &    int7(3)=int7(1)+int7(3)-1
;Vers.1	   device,move= int7
	endif else begin
	   int7(6)=mot_w(int7(6))
	   if ms_devs eq 1 then  device,copy = int7 else $
	   if ms_devs eq 2 then  device,copy = int7
	endelse
return, 1
end
;
;
function sl_tvhdfil,	flg,fil,ext,fhd,nocol
;******* **********
;**
;**	 Open or close a HD file
;**	 ---- -- ----- - -- ----
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
on_ioerror,mis
bb=0
	if (ms_devs eq 3) or (ms_devs eq 4) then $
	    if  flg eq 1 then begin
		if ms_devs  eq 3 then begin
		   if nocol eq 1 then $
			 device,filename=fil+'.'+ext,bits_per_pixel=8 $
		   else	 device,filename=fil+'.'+ext,bits_per_pixel=8,/color
		   if fhd eq 1 then device,/encapsulated
		   endif
		if ms_devs  eq 4 then device,filename=fil+'.'+ext
	    endif  else  device,/close_file
	bb=1
mis:return,bb
end
;
;
function sl_tvhdlct,	cr,cg,cb
;******* **********
;**
;**	 Put color in a HD file
;**	 --- ----- -- - -- ----
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
on_ioerror,mis
	if (ms_devs eq 3) or (ms_devs eq 4) then bb=sl_tvldcol(cr,cg,cb)
mis:return, 1
end
;
;
function sl_tvhdimg,	area,vsiz ,fil,ext
;******* **********
;**
;**	 Put image in a HD file
;**	 --- ----- -- - -- ----
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans

	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy

	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;**
on_ioerror,mis
bb=0
	bb=sl_tvget(7 ,ii)
	bb=sl_tvset(7 ,t_od)
	if (ms_devs eq 3) or (ms_devs eq 4) then begin
			 sx  =7.21 & sy=10.6 & bpi=300. & pi=85 & fx=1. & fy=1.
			 bord=0.5
			 lup =0.3
			 if vsiz(1) le vsiz(2) then begin
			    ix=float(vsiz(1))/pi & iy=float(vsiz(2))/pi
			 endif else begin
			    ix=float(vsiz(2))/pi & iy=float(vsiz(1))/pi & endelse
			 if ix gt sx then fx=sx/ix
			 if iy gt sy then fy=sy/iy
			 if fy lt fx then fx=fy
			 ix=ix*fx & iy=iy*fx
;
			 if vsiz(1) le vsiz(2) then begin
			    i=fix(ix*bpi/vsiz(1))
			    if i gt 0 then ix=float(i)*vsiz(1)/bpi
			    i=fix(iy*bpi/vsiz(2))
			    if i gt 0 then iy=float(i)*vsiz(2)/bpi
			 endif else begin
			    i=fix(ix*bpi/vsiz(2))
			    if i gt 0 then ix=float(i)*vsiz(2)/bpi
			    i=fix(iy*bpi/vsiz(1))
			    if i gt 0 then iy=float(i)*vsiz(1)/bpi
			 endelse

			 xo=(sx-ix)/2 & yo=(sy-iy)/2
;
			 if vsiz(1) le vsiz(2) then begin
			  if yo lt bord then yo=bord & if yo lt 0.8 then lup=0.1
			  if xo lt bord then xo=bord
			  if ms_devs eq 3 then begin
			    device,/inches,/portrait ,xsize=ix,ysize=iy,$
						    xoffset=xo,yoffset=yo
			    bb=sl_tvimag(area,vsiz,0,0)
			    if mot_wdcur(7) gt 0 then p_did_ps_header, iy+lup , 0 ,fil+'.'+ext
			    endif
			  if ms_devs eq 4 then begin
			    erase,0 & bb=sl_tvimag(area,vsiz,vsiz(1),vsiz(2))
			    endif
			 endif else begin
			  if xo lt bord then xo=bord & if xo lt 0.8 then lup=0.1
			  if ms_devs eq 3 then begin
			    device,/inches,/landscape,xsize=iy,ysize=ix,$
						    xoffset=xo,yoffset=sy-yo
			    bb=sl_tvimag(area,vsiz,0,0)
			    if mot_wdcur(7) gt 0 then p_did_ps_header, ix+lup , 0 ,fil+'.'+ext
			    endif
			  if ms_devs eq 4 then begin
			    erase,0 & bb=sl_tvimag(area,vsiz,vsiz(1),vsiz(2))
			    endif
			 endelse

	endif
	bb=sl_tvset(7 ,ii)
	bb=1
mis:return,bb
end
;
;
;
;************************************************ Second level ************
function sl_tvpix, fx,fy
;******* ********
;**
;**	Duplicate pixels. (tv_flg(0)=1)
;**	--------- ------   ******
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;
		t_pix =fx
		t_piy =fy
return, 1
end
;
;
function sl_tvras	,x,y,dx,dy,col,bx,by
;******* ********
;**
;**	Fill a region with col.
;**	---- - ------ ---- ---
	common  machin,	mc_sys,mc_sta
;**
	common	my_tvi,	 t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_eras ,zr,zr_z,zrv2,zrv6,zx4,zy4
;**
	if t_rep then begin
		zr(0,0)=col
		bb=sl_tvpix (dx,dy)
		bb=sl_tvimag(zr,zr_z,x,y)
		bb=sl_tvpix (1,1)
	endif else if mc_sta eq 'sun' then begin
;**		try fill_area.........
		zx4(0)=x & zx4(2)=x+dx & zx4(1)=zx4(0) & zx4(3)=zx4(2)
		zy4(0)=y & zy4(2)=y+dy & zy4(1)=zy4(2) & zy4(3)=zy4(0)
		if col lt 0 then bb=sl_tvpol(4,zx4,zy4,0  ,0) $
			    else bb=sl_tvpol(4,zx4,zy4,col,0)
	endif else begin
;**		try tvmove_area.........
		if col le 0 then bb=sl_tvmod(2,6) else bb=sl_tvmod(2,11)
		zrv6(0)=x  & zrv6(1)=y  & zrv6(2)=dx
		zrv6(3)=dy & zrv6(4)=x  & zrv6(5)=y
		bb=sl_tvget(3,w)	& zrv6(6)=w
		bb=sl_tvmov( zrv6)
		bb=sl_tvmod(0,-1)
	endelse
return, 1
end
;
;
function sl_tvnobut	,dum
;******* **********
	common	my_tvi ,t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur

	repeat   begin bb=sl_tvgcur(i,j,k,0) &  endrep  until k eq 0
	if mot_f then  if t_w ge 0 then bb=sl_wgclear(mot_wd(t_w))
return, 1
end
;
function sl_surfex	,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13
;******* *********
;**
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
;**
	common  machin,	mc_sys,mc_sta
;**
	if mc_sys eq 'vms'   then bb=call_external('surf_exe' ,'surf3',$
				  p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13) $
	else $
	if mc_sys eq 'win'   then bb=call_external('surf_exe' ,'surf3',$
				  p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13) $
	else $
	if mc_sys eq 'unix'  then $
	  if mc_sta eq 'hp'  then bb=call_external(ms_iodir+'surf_HP.so' ,'surf',$
				  p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13) $
	  else $
	  if mc_sta eq 'sgi' then bb=call_external(ms_iodir+'surf_SGI.so','surf_',$
				  p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13) $
	  else $
	  if mc_sta eq 'sun' then bb=call_external(ms_iodir+'surf_SUN.so','surf_',$
				  p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)
return, 1
end
;
function sl_deepex	,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,$
			 p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25
;******* *********
;**
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
;**
	common  machin,	mc_sys,mc_sta
;**
	if mc_sys eq 'vms'   then bb=call_external('fordeep_exe',  $
			'deepff'  ,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,$
			 p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)     $
	else $
	if mc_sys eq 'win'   then bb=call_external('fordeep_exe',  $
			'deepff'  ,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,$
			 p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)     $
	else $
	if mc_sys eq 'unix'  then $
	  if mc_sta eq 'hp'  then bb=call_external(ms_iodir+'fordeep_HP.so',  $
			'fordeep' ,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,$
			 p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)     $
	  else $
	  if mc_sta eq 'sgi' then bb=call_external(ms_iodir+'fordeep_SGI.so', $
			'fordeep_' ,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,$
			 p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)     $
	  else $
	  if mc_sta eq 'sun' then bb=call_external(ms_iodir+'fordeep_SUN.so', $
			'fordeep_',p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,$
			 p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25)
return, 1
end
;
function sl_surface	,area,az,ax,asp,skirt
;******* **********
;**
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
;
;	surface,area,ax=ax,az=az,bottom=ms_bcolor,zaxis=-1,xran=[0,0],yran=[0,0]$
;	     ,xst=4,yst=4,/device,position=ms_xypos,xmargin=ms_v2,ymargin=ms_v2
	shade_surf,area,ax=ax,az=az,bottom=ms_bcolor,zaxis=-1,xran=[0,0],yran=[0,0]$
	     ,xst=5,yst=5,/device,position=ms_xypos,xmargin=ms_v2,ymargin=ms_v2
return, 1
end
;
function sl_contour	,area,start,endd,lev
;******* **********
;**
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
;
	delta  =float(endd-start)/(lev-1)
	tmp    =ms_xypos
;	tmp(1) =min([ms_xypos(1),ms_xypos(3)])	;!!??
;	tmp(3) =max([ms_xypos(1),ms_xypos(3)])
	contour,area,levels=sl_index(lev,8)*delta+start,xran=[0,0],yran=[0,0],/fill $
	     ,xst=5,yst=5,/device,position=tmp,xmargin=ms_v2,ymargin=ms_v2
return, 1
end
;
;
;
function sl_tvhdout,	fil,ext,fhd
;******* **********
;**
;**	 Output a HD file
;**	 ------ - -- ----
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  machin,	mc_sys,mc_sta
;**
	if ms_devs  eq 3 then $
	    if fhd  ne 1 then begin
		    bb=sl_run('@',ms_iodir+'hard_out.com '+fil+'.'+ext,0,0,1)
	    endif
return, 1
end
;
;
function sl_tvscrl ,  txt , ms_ncol
;******* *********
;**
;**	Scroll menu text.
;**	------  --- ----
;**
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
	common  tmp_men,m_wd, m_wi,m_ht,m_ch,m_nl,m_px,m_py,m_ft,m_pp,m_rs ,$
			m_vx5,m_vy5,m_wd5,m_nl5,m_fun5,m_wm,m_xx,m_sp,m_soc,$
			m_losx,m_losy,m_bor,m_li5,m_lf5,m_ll5,m_lx5,m_ly5, $
			m_clop,m_bex1,m_bex2,m_filx,m_bey1,m_bey2,m_fily,m_od,$
			m_pat,m_siz,m_sel5,m_sta,m_str
;**
		wm=m_wm
		k1= ((m_nl5(wm)+1)*m_ch+m_bor)/m_ll5(wm)
		if  k1 eq 0 then k1=1
		k1= m_ll5(wm)-1-(m_wi / k1)
		k1= k1-m_nl5(wm)/2 & if k1 lt 0 then k1=0
		k2= k1+m_nl5(wm)-1 & if k2 ge m_ll5(wm) then begin
					   k2=m_ll5(wm)-1
					   k1=k2 - m_nl5(wm) +1 & endif
		if k1 ne m_li5(wm) then begin
		    	bb=sl_tvget(6,m_ft) & ft=m_ft
			if m_ft ge 0 then bb= sl_tvmod(1,6) else bb=sl_tvmod(1,3)
			if m_ft ge 0 then k3= m_colo/3      else k3=m_colo-1
			if sys_dep('MACHINE') eq 'win' then begin bb=sl_tvset(6,-1) & ft=-1 & endif
			if ft ge 0 then k4=1. else k4=2.

			m_pp=-2
			j=0
			for i=m_li5(wm),m_lf5(wm) do begin
			    bb=sl_tvs(m_bor+m_fun5(wm)*m_sp*j,$
			     (m_nl5(wm)-j-1)*m_ch+m_bor,txt(i)   ,k4,0,k3)
			    bb=sl_tvs(m_bor+m_fun5(wm)*m_sp*j,$
			     (m_nl5(wm)-j-1)*m_ch+m_bor,txt(k1+j),k4,0,m_colo/3)
			    j=j+1 & endfor
			m_li5(wm)=k1
			m_lf5(wm)=k2
		    	if ft ne m_ft then bb=sl_tvset(6,m_ft)
			m_ft=0

		if m_wi gt (m_nl5(wm)-1)*m_ch   then m_wi=(m_nl5(wm)-1)*m_ch
		if m_wi lt 10			then m_wi=10
		bb=sl_tvline(m_filx(*,wm),m_fily(*,wm) ,2 ,0,m_colo/2)
		bb=sl_tvline(m_bex1(*,wm),m_bey1(*,wm) ,7 ,0,m_colo/2)
		bb=sl_tvline(m_bex2(*,wm),m_bey2(*,wm) ,7 ,0,m_colo/2)
		m_fily(1,wm)=m_wi
		if (m_wi/2)*2 eq m_wi then j=m_od+1 else j=m_od
		if (m_wi/2)*2 eq m_wi then i=m_od   else i=m_od+1
		m_bey1(3,wm)=m_fily(1,wm)     & m_bey2(3,wm)=m_fily(1,wm)
		m_bey1(6,wm)=m_bey1(3,wm)+j*2 & m_bey2(6,wm)=m_bey1(3,wm)+i*2
		m_bey1(5,wm)=m_bey1(3,wm)+j*2 & m_bey2(5,wm)=m_bey1(3,wm)+i*2
		m_bey1(4,wm)=m_bey1(3,wm)+j   & m_bey2(4,wm)=m_bey1(3,wm)+i
		m_bey1(2,wm)=m_bey1(3,wm)-i   & m_bey2(2,wm)=m_bey1(3,wm)-j
		m_bey1(1,wm)=m_bey1(3,wm)-i*2 & m_bey2(1,wm)=m_bey1(3,wm)-j*2
		m_bey1(0,wm)=m_bey1(3,wm)-i*2 & m_bey2(0,wm)=m_bey1(3,wm)-j*2
		bb=sl_tvline(m_filx(*,wm),m_fily(*,wm) ,2 ,0,m_colo/2)
		bb=sl_tvline(m_bex1(*,wm),m_bey1(*,wm) ,7 ,0,m_colo/2)
		bb=sl_tvline(m_bex2(*,wm),m_bey2(*,wm) ,7 ,0,m_colo/2)

		bb=sl_tvmod(1,3)
		endif

return,1
end
;
;
function sl_tvmenub, wwm , flgg, txt , ttl , x,y ,seq
;******* **********
;**
;**	Use  a menu.
;**	---  - ----
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common	my_tvi,	 t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
	common  tmp_men,m_wd, m_wi,m_ht,m_ch,m_nl,m_px,m_py,m_ft,m_pp,m_rs ,$
			m_vx5,m_vy5,m_wd5,m_nl5,m_fun5,m_wm,m_xx,m_sp,m_soc,$
			m_losx,m_losy,m_bor,m_li5,m_lf5,m_ll5,m_lx5,m_ly5, $
			m_clop,m_bex1,m_bex2,m_filx,m_bey1,m_bey2,m_fily,m_od,$
			m_pat,m_siz,m_sel5,m_sta,m_str
	common  my_x,	x_foc,x_st1,x_st2,x_tmp,x_fso,x_p0,x_p1,x_p2,x_bb
	common  machin,	mc_sys,mc_sta
;**
ab=-1
bb=sl_tvget(3,m_id)
wm=wwm
fl=flgg
flg=flgg
if fl lt 0 then fl=0
if fl eq 7 then fl=1
if wm lt 0 then wm=-wm
;
;	Make Statique or Dynamique
;	---- -------- -- ---------
;	wm<0  then no item must be selected
;
;	fl=0  create menu (and return if wm # 0)
;	fl=1  menu exist  (statique)
;	fl=2  create menu  with previous info (wm=4) (obsolate)
;	fl=3  create menu                   and do'nt delete  (no return)
;	fl=4  menu exist   but change texte and do'nt delete  (no return)
;	fl=5  create menu  very simply     (and return if wm # 0)
;       fl=6  menu exist  (scroll and return when cursor out)
;       fl=7  menu exist  (choice and return when cursor out)
;       fl=-n same as fl=0 but maxi n lines
;
	if (fl eq 4) or  (fl eq 6) or (fl eq 7) then  begin
;	Verify existance and size
;	------ --------- --- ----
	   bb=sl_tvsel(wm)
	   if bb eq 1 then begin
		bb=sl_tvget(28,i)
		bb=sl_tvget(29,j)
		if (i ne m_siz(0,wm)) or (j ne m_siz(1,wm)) then begin
			if fl eq 6 then  flg= - (j/m_ch+1)
			if fl eq 7 then  flg= - (j/m_ch+1)
			if fl eq 4 then  fl=3 else fl=0
		endif
	   endif else fl=3
	endif
	if (fl ne 1) and (fl ne 6) then  begin

;	Init tables
;	---- ------
	     if fl ne 2 then begin m_nl= sl_stdim(ttl,i)
				   m_nl= sl_stdim(txt,m_wd)
	     endif      else begin m_nl= sl_stdim(m_ttl,i)
				   m_nl= sl_stdim(m_txt,m_wd)
	     endelse
	     if i lt 20   then i=20
	     if i gt m_wd then m_wd=i
	     m_li5(wm) =0
	     m_ll5(wm) =m_nl
	     if flg  lt 0 then m_nl=-flg
	     m_ht =  m_nl*m_ch+m_bor
	     if m_nl gt 1    then m_fun5(wm)=1    else   m_fun5(wm)=0
	     if fl   eq 5    then m_fun5(wm)=0
	     if fl   eq 2    then m_fun5(wm)=0
	     if flg  lt 0    then m_fun5(wm)=0
	     m_ht =  m_ht +  m_ch*m_fun5(wm)
	     if m_ht gt t_sy then begin m_ht=t_sy/2
					m_nl=(m_ht- m_bor)/m_ch -m_fun5(wm)
	     				m_ht= m_nl*m_ch+m_bor +  m_ch*m_fun5(wm)
					endif
	     if m_fun5(wm) eq 1    then  m_wd =(m_wd+m_nl+2) * m_sp + m_bor  $
				   else  m_wd =(m_wd       ) * m_sp + m_bor
	     if m_ll5(wm)  ne m_nl then  m_wi = 3*m_sp  else m_wi=0
	     if m_wd+m_wi  gt t_sx then begin   m_wd= t_sx-m_wi
						m_fun5(wm)=0  & endif
	     m_wd5(wm)=m_wd
	     m_nl5(wm)=m_nl
	     m_lf5(wm)=m_nl-1

;	Create menu
;	------ ----
	     if  fl ne 4   then begin
	      if fl ne 2   then begin
	       if x eq -2. then begin
		 bb=sl_tvwinp(m_px,m_py)
		 m_pp=0 & m_ft=0
		 if (m_px gt 0) and (m_py gt 0) then  $
			bb=sl_tvgcur(m_pp,m_ft, m_rs ,0)
		 m_px=(m_px+m_pp)
		 m_py=(m_py+m_ft)
	       endif else begin
		 m_px=m_dx * x
	   	 m_py=m_dy * y
	       endelse
	       if m_py+m_ht      gt t_sy then m_py=t_sy-m_ht
	       if m_px+m_wd+m_wi gt t_sx then m_px=t_sx-m_wd-m_wi
	       bb=sl_tvlux(wm,m_wd+m_wi,m_ht, ttl,0,0,0,0,0,0,0,0,m_px,m_py,seq)
	      endif  else $
	       bb=sl_tvlux(wm,m_wd+m_wi,m_ht,m_ttl,0,0,0,0,0,0,0,0,m_x,m_y ,seq)
              bb=sl_tvget(28,i) & m_siz(0,wm)=i
              bb=sl_tvget(29,i) & m_siz(1,wm)=i
	      ms_tbwin(wm)=wm
	     endif
;
	     if wwm lt 0 then m_sel5(wm)=0 else m_sel5(wm)=1
;
;	Fill window
;	---- ------
		m_lx5(0,wm)=0	   & m_lx5(1,wm)=m_wd-1 + m_wi
		m_lx5(3,wm)=0	   & m_lx5(2,wm)=m_wd-1 + m_wi
		m_lx5(4,wm)=0      & m_ly5(4,wm)=0
		m_ly5(0,wm)=0	   & m_ly5(1,wm)=0
		m_ly5(3,wm)=m_ht-1 & m_ly5(2,wm)=m_ht-1
;
	     if (m_fun5(wm) eq 1) then begin
		if (fl ne 4) then begin
;		background
		  m_pat(*,*)  =m_colo-1-m_colo/8
		  m_pat(6,3)  =m_colo-1-m_colo/4
		  m_pat(5,4)  =m_colo-1-m_colo/6
		  m_pat(4,5)  =m_colo-1-m_colo/7
		  m_pat(3,6)  =m_colo-1-m_colo/9
		  m_pat(2,7)  =m_colo-1-m_colo/10
		  if (mc_sys ne 'unix') or (mc_sta ne 'dec')      then $
		  bb=sl_tvpol(5,m_lx5(*,wm),m_ly5(*,wm),m_pat,10) else $
		  bb=sl_tvpol(5,m_lx5(*,wm),m_ly5(*,wm),m_colo-1-m_colo/8,0)
		endif
;		Page
		m_lx5(0,wm) =1	         & m_lx5(1,wm) = m_wd-1 - (m_nl-1)*m_sp
		m_lx5(3,wm) =m_wd-1      & m_lx5(4,wm) =(m_nl-1)*m_sp
		m_lx5(2,wm) =m_lx5(1,wm)
		m_ly5(0,wm) =m_ht-m_ch-2 & m_ly5(1,wm) =m_ly5(0,wm)
		m_ly5(3,wm) =m_bor-1	 & m_ly5(2,wm) =m_ly5(1,wm)
		m_ly5(4,wm) =m_ly5(3,wm)
;		with corner
		i=m_lx5(1,wm)/m_sp & if  i gt 40 then i=40
		m_lx5(1,wm) =m_lx5(1,wm)-i
		m_lx5(2,wm) =m_lx5(2,wm)+2*m_sp
		m_ly5(2,wm) =m_ly5(1,wm)-2*m_ch
		m_losx(0)   =m_lx5(1,wm) & m_losx(1)   =m_lx5(2,wm)-m_sp
		m_losx(2)   =m_lx5(2,wm) & m_losx(3)   =m_losx(1)
		m_losy(0)   =m_ly5(1,wm) & m_losy(1)   =m_ly5(2,wm)-m_ch
		m_losy(2)   =m_ly5(2,wm) & m_losy(3)   =m_losy(1)  -5
;
		if sys_dep('MACHINE') eq 'win' then j=10 else j=20
		bb=sl_tvpol(5,m_lx5(*,wm),m_ly5(*,wm),m_colo-1-m_colo/j,0)
		bb=sl_tvpol(4,m_losx,m_losy,m_colo-1-m_colo/4,0)
;
	     endif else begin
		bb=sl_tvpol(5,m_lx5(*,wm),m_ly5(*,wm),m_colo-1,0)
;		erase,ms_ncol-1
	     endelse

	     m_ly5(0,wm) =m_ht-m_ch-2

;	Fill base
;	---- ----
	     if (fl ne 4) and (m_fun5(wm) eq 1) then begin
;		as a socle
		m_losx(0)=m_lx5(0,wm)   & m_losx(1)=m_lx5(4,wm)
		m_losx(2)=m_losx(1)     & m_losx(3)=m_losx(0)
		m_losy(0)=m_ly5(0,wm)   & m_losy(1)=m_ly5(4,wm)
		m_losy(2)=1	        & m_losy(3)=m_ht /2

;		as a shade
                m_losy(0)=m_ly5(0,wm)/2 & m_losx(0)=m_lx5(0,wm)+(m_nl-1)*m_sp/2
		m_losy(3)=m_losy(0)

		bb=sl_tvpol(4,m_losx,m_losy,m_colo-1-m_colo/4,0)

		m_losx(0)=m_losx(1) & m_losx(1)=m_lx5(3,wm)
		m_losx(2)=m_losx(1) & m_losx(3)=m_losx(0)
		m_losy(0)=m_losy(1) & m_losy(1)=m_losy(0)
		m_losy(2)=m_losy(2) & m_losy(3)=m_losy(2)
		bb=sl_tvpol(4,m_losx,m_losy,m_colo-1-m_colo/4,0)
	     endif

;	Scroll bar
;	------ ---
	     if m_nl5(wm) ne m_ll5(wm) then begin
		m_vx5(0)=m_wd+m_wi-1 & m_vx5(1)= m_vx5(0)
		m_vx5(2)=m_vx5(1)    & m_vx5(3)= m_vx5(0)- 2*m_wi/3
		m_vx5(4)=m_vx5(0)-m_wi/2
		m_vx5(5)=m_vx5(4)    & m_vx5(6)= m_vx5(0)
		m_vy5(0)=1	     & m_vy5(1)= m_vy5(0)
		m_vy5(2)=m_ly5(0,wm)-m_wi/2 & m_vy5(3)= m_ly5(0,wm)
		m_vy5(4)=m_vy5(2)
		m_vy5(5)=m_vy5(4)    & m_vy5(6)= m_vy5(0)
		bb=sl_tvpol(7,m_vx5,m_vy5,0,0)

		m_od=4 & i=m_od
		m_fily(0,wm)=m_vy5(3)	      & m_filx(0,wm)=m_vx5(3)
		m_fily(1,wm)=m_vy5(3)-20      & m_filx(1,wm)=m_filx(0,wm)

		m_bex1(3,wm)=m_filx(1,wm)     & m_bex2(3,wm)=m_filx(1,wm)
		m_bey1(3,wm)=m_fily(1,wm)     & m_bey2(3,wm)=m_fily(1,wm)

		m_bex1(6,wm)=m_bex1(3,wm)-i+2 & m_bex2(6,wm)=m_bex1(3,wm)+i-2
		m_bex1(5,wm)=m_bex1(3,wm)-i+1 & m_bex2(5,wm)=m_bex1(3,wm)+i-1
		m_bex1(4,wm)=m_bex1(3,wm)-i+1 & m_bex2(4,wm)=m_bex1(3,wm)+i-1
		m_bex1(2,wm)=m_bex1(3,wm)-i   & m_bex2(2,wm)=m_bex1(3,wm)+i
		m_bex1(1,wm)=m_bex1(3,wm)-i   & m_bex2(1,wm)=m_bex1(3,wm)+i
		m_bex1(0,wm)=m_bex1(3,wm)-i-2 & m_bex2(0,wm)=m_bex1(3,wm)+i+2

		m_bey1(6,wm)=m_bey1(3,wm)+i*2 & m_bey2(6,wm)=m_bey1(3,wm)+i*2
		m_bey1(5,wm)=m_bey1(3,wm)+i*2 & m_bey2(5,wm)=m_bey1(3,wm)+i*2
		m_bey1(4,wm)=m_bey1(3,wm)+i   & m_bey2(4,wm)=m_bey1(3,wm)+i
		m_bey1(2,wm)=m_bey1(3,wm)-i   & m_bey2(2,wm)=m_bey1(3,wm)-i
		m_bey1(1,wm)=m_bey1(3,wm)-i*2 & m_bey2(1,wm)=m_bey1(3,wm)-i*2
		m_bey1(0,wm)=m_bey1(3,wm)-i*2 & m_bey2(0,wm)=m_bey1(3,wm)-i*2
	     endif
;	fill text
;	---- ----
	     bb=sl_tvget(6,m_ft) & ft=m_ft & j=3
	     if m_ft ge 0 then j=6
;	     if sys_dep('MACHINE') eq 'win' then begin bb=sl_tvset(6,-1) & ft=-1 & endif
	     if sys_dep('MACHINE') eq 'win' then j=3
	     bb=sl_tvmod(1,j) ;  bb=sl_tvmod(1,j)
	     if j eq 6 then j=40./(256./m_colo) else j=m_colo/10  ;m_colo-1

	     if fl ne 2 then begin
	      for i=m_li5(wm),m_lf5(wm) do $
		bb=sl_tvs(m_bor+m_fun5(wm)*m_sp*i,(m_nl-i-1)*m_ch+m_bor,$
			  txt(i),2.,0,j)
	      if  wm eq 4 then begin m_txt=txt   & m_ttl=ttl
				     m_x  =m_px  & m_y  =m_py  & endif
	     endif else $
	      for i=m_li5(wm),m_lf5(wm) do  $
		bb=sl_tvs(m_bor+m_fun5(wm)*m_sp*i,(m_nl-i-1)*m_ch+m_bor,$
			  m_txt(i),2.,0,j)
	     if m_nl5(wm) ne m_ll5(wm) then begin
		bb=sl_tvline(m_filx(*,wm),m_fily(*,wm) ,2 ,0,m_colo/2)
		bb=sl_tvline(m_bex1(*,wm),m_bey1(*,wm) ,7 ,0,m_colo/2)
		bb=sl_tvline(m_bex2(*,wm),m_bey2(*,wm) ,7 ,0,m_colo/2)
		endif
	     bb=sl_tvmod(1,3)
	     if ft ne m_ft then bb=sl_tvset(6,m_ft)
	     bb=sl_tvwait(.01,0,0,wm ,0,0)
	  endif

;	Scroll text.
;	------ ----
	  if (fl eq 6) then begin
	     bb=sl_tvshap(42)
	     bb =sl_tvpop(wm,1)
	     m_wm =wm
	     m_px =0
	     m_py =0
	     m_pp =-1
	     m_clop=0 & i=0
	     bb = sl_x('focus_in')
	     while (m_px ge 0) do begin

	        m_wi= m_py
		m_py= m_py-m_bor & if m_py lt 0 then m_py=0
		m_py= m_py/m_ch
		if m_py ge m_nl5(m_wm) then m_py=m_nl5(m_wm)-1

		if  (m_nl5(m_wm) ne m_ll5(m_wm)) $
		     and (m_px ge m_wd5(m_wm)-3*m_sp-m_fun5(m_wm)*m_py*m_sp) $
			then bb=sl_tvscrl(txt , ms_ncol)
		bb=sl_tvgcur(m_px,m_py, m_rs ,0)
		if m_py eq m_pp then begin
			if m_clop ge 500 then begin
					 bb=sl_tvwait(.5,1,2,m_wm ,i,kw)
					 if (i eq 2) or (i eq 3) then m_px=-1
			endif else m_clop=m_clop+1
			k= m_clop/25
			if m_clop eq k*25  then $
			 if x_foc ge  0    then begin
						bb=  sl_x('kb_check')
						if bb eq 1 then begin m_px=-1
								ab=1  & endif
			 endif
		endif else m_clop=0
		m_pp=m_py
	     endwhile

	     bb =sl_tvnobut(0)
	     bb =sl_tvshap(-1)

;	Get Statique or Dynamique
;	--- -------- -- ---------
	  endif else $
	  if (wm eq 0) or (fl eq 1) or (fl eq 3) or (fl eq 4) then begin
	    bb=sl_tvsel(wm)
	    if bb eq 1  then begin
	     if flgg eq 7 then m_sel5(wm)=2
	     bb =sl_tvshap(58)
	     bb =sl_tvpop(wm,1)
	     bb =sl_tvmod(2,10)
	     bb =sl_sysget(29,0)
	     bb =sl_x('focus_out')
	     m_sta=0
	     m_clop=0
	     m_ft =0
	     m_rs =0
	     m_xx =0
	     m_pp =-1
	     m_py =-1
	     m_nl =m_nl5(wm)
	     m_wd =m_wd5(wm)-1
	     m_wm =wm
	     i    =0
;	Loop
;	----
	     if (flgg ne 1) and (flgg ne 4) and (flgg ne 7) then $
		bb=sl_tvmcur(2,m_wd/2,m_nl*m_ch/2)
	     bb =sl_tvnobut(0)
;
	     while (m_rs eq 0) or (m_py lt 0) do begin
	      bb=sl_tvgcur(m_px,m_py, m_rs ,m_ft)
	      m_ft=0
	      if m_sel5(m_wm) eq 0 then m_py=-1
	      m_wi=m_py
	      if m_py ge 0 then begin
		m_py= m_py-m_bor & if m_py lt 0 then m_py=0
		m_py= m_py/m_ch
		if m_py ge m_nl then m_py=m_nl-1

;		Fill selection
;		---- ---------
		if m_py ne m_pp then begin
			m_vx5(0)=m_fun5(m_wm)*(m_nl-m_py-1)*m_sp +m_bor
			m_vx5(4)=m_vx5(0)  -   m_fun5(m_wm)*m_sp
			if m_px ge m_ch then m_vx5(1)=m_px -m_ch $
					else m_vx5(1)=m_px
			if m_vx5(1) lt  m_vx5(4) then m_vx5(1)=m_vx5(0)
			if m_vx5(1) gt  m_wd-m_fun5(m_wm)*m_py*m_sp  then begin
						      m_vx5(1)=m_vx5(0)
						      m_rs    =0        & endif
			m_vx5(2)= m_vx5(1)+m_ch     & m_vx5(3)=m_vx5(1)
			m_vx5(5)= m_lx5(0,wm)+(m_nl-1)*m_sp/2
			m_vx5(6)= m_vx5(0)

			m_vy5(0)= m_py*m_ch+m_bor-2 & m_vy5(1)=m_vy5(0)
			m_vy5(2)= m_vy5(0)+m_ch/2   & m_vy5(3)=m_vy5(0)+m_ch
			m_vy5(4)= m_vy5(3)	    & m_vy5(5)=m_ly5(0,wm)/3
			m_vy5(6)= m_vy5(0)
			bb=m_nl-1-m_py+m_li5(m_wm)
			if (flgg ne 1) and (flgg ne 2) and (bb ge 0) then $
			    m_str=sl_stx(txt(bb),0,1) else m_str=' '
;			if  m_str ne '.' then bb=sl_tvpol(7,m_vx5,m_vy5,m_colo-1,0)
			bb=sl_tvset(35,2)
			if  m_str ne '.' then bb=sl_tvline(m_vx5,m_vy5,7,0,m_colo-1)
			bb=sl_tvset(35,1)
		endif else begin

;			Scroll if necessary
;			------ -- ---------
			if  (m_nl5(m_wm) ne m_ll5(m_wm)) $
			and (m_px gt m_wd5(m_wm)-3*m_sp-m_fun5(m_wm)*m_py*m_sp) $
				then begin
				m_rs=0
				bb=sl_tvscrl(txt , ms_ncol)
				bb=sl_tvmod(2,10)
			endif
		endelse
	      endif
	      if (m_pp ge 0) and ((m_pp ne m_py) or (m_rs ne 0)) then begin
;		Clear selection
;		----- ---------
			m_vx5(0)=m_fun5(m_wm)*(m_nl-m_pp-1)*m_sp +m_bor
			m_vx5(4)=m_vx5(0)  -   m_fun5(m_wm)*m_sp
			if m_xx ge m_ch then m_vx5(1)=m_xx -m_ch $
					else m_vx5(1)=m_xx
			if m_vx5(1) lt  m_vx5(4) then m_vx5(1)=m_vx5(0)
			if m_vx5(1) gt  m_wd-m_fun5(m_wm)*m_pp*m_sp  then begin
						      m_vx5(1)=m_vx5(0)
						      m_rs    =0        & endif
			m_vx5(2)= m_vx5(1)+m_ch     & m_vx5(3)=m_vx5(1)
			m_vx5(5)= m_lx5(0,wm)+(m_nl-1)*m_sp/2
			m_vx5(6)= m_vx5(0)

		        m_vy5(0)= m_pp*m_ch+m_bor-2 & m_vy5(1)=m_vy5(0)
		        m_vy5(2)= m_vy5(0)+m_ch/2   & m_vy5(3)=m_vy5(0)+m_ch
			m_vy5(4)= m_vy5(3)	    & m_vy5(5)=m_ly5(0,wm)/3
			m_vy5(6)= m_vy5(0)
			bb=m_nl-1-m_pp+m_li5(m_wm)
			if (flgg ne 1) and (flgg ne 2) and (bb ge 0) then $
			    m_str=sl_stx(txt(bb),0,1) else m_str=' '
;			if  m_str ne '.' then bb=sl_tvpol(7,m_vx5,m_vy5,m_colo-1,0)
			bb=sl_tvset(35,2)
			if  m_str ne '.' then bb=sl_tvline(m_vx5,m_vy5,7,0,m_colo-1)
			bb=sl_tvset(35,1)
	      endif

;	      Wait
;	      ----
	      if (m_py eq m_pp)  then begin
			if (m_py lt 0) then begin
				 if (m_fun5(m_wm) eq 1) and $
					  (m_clop gt 180) then begin
				   m_wi	    =m_nl*m_sp/8
				   if m_wi  gt 10 then m_wi=10
				   m_losx(0)   =m_lx5(1,m_wm)
				   m_losx(1)   =m_lx5(2,m_wm)-m_sp
				   m_losx(2)   =m_lx5(2,m_wm)
				   m_losx(3)   =m_losx(1)
				   m_losy(0)   =m_ly5(1,m_wm)
				   m_losy(1)   =m_ly5(2,m_wm)-m_ch/3
				   m_losy(2)   =m_ly5(2,m_wm)
				   m_losy(3)   =m_ly5(2,m_wm)-m_ch-5
				   bb=sl_tvmod(2,6)
				   bb=sl_tvpol(4,m_losx,m_losy,m_colo-1,0)
				   bb=sl_tvmod(2,10)
				endif
				if m_sel5(m_wm) eq 1  then begin
				   if m_clop ge 500 then begin
				    bb=sl_tvpop(m_wm,1)
				    bb=sl_tvwait(1.,1,0,m_wm ,i,kw)
				   endif  else begin
				    m_clop=m_clop+1
				    bb=sl_tvwait(.05,1,0,m_wm ,i,kw)
				   endelse
				   if (i lt 0) or (i eq 2) or (i eq 3) then $
					       begin m_rs=-1 & m_py=0 & endif
				endif
			endif   else begin
				if x_foc   ge   0 then begin
				 if m_sta  eq   0 then begin
					bb=sl_x('focus_in') & m_sta=1 & endif
				 k= m_clop/25
				 if m_clop eq k*25 then begin
					bb=sl_x('kb_check')
					if bb eq 1  then begin
					   m_clop=20
					   m_str =sl_tviokey(0,k)
					   case   m_str of
;*					    Print
					    '[29~':  m_rs=1
;*					    Return
;*					    '!!!':      m_rs=2 ; 'x0D'
					    string(13b):m_rs=2
;*					    Up
					    '[B  ': begin
						     if m_wi-m_ch ge 0 then $
							m_wi=m_wi-m_ch
						     bb=sl_tvmcur(2,m_px,m_wi)
						     end
;*					    Down
					    '[A  ': begin
						     if (m_wi+m_ch)/m_ch lt m_nl $
							 then m_wi =m_wi+m_ch
						     bb=sl_tvmcur(2,m_px,m_wi)
						     end
;					    Left
					    '[D  ': m_rs=4
;					    Right
					    '[C  ': m_rs=1
					    else:
					   endcase
					endif
				 endif
				endif
 				if m_clop ge 800 then $
				 bb=sl_tvwait(.5,1,2,m_wm, i,kw) $
 				else   m_clop=m_clop+1
			endelse
	      endif	else    begin   m_clop=0
			if m_pp ne -2   then  m_xx=m_px
			endelse
;	      Try other statics or verify close or size changed
;	      --- ----- ------- -- ------ ----- -- ---- -------
	      if   m_py lt 0 then begin
	       if m_sta eq 1 then begin bb=sl_x('focus_out') & m_sta=0 & endif
	       k=0
               while (k le 5)  do begin
		if m_wm gt 0 then m_wm=m_wm+1
		if m_wm gt 5 then m_wm=1
		if ms_tbwin(m_wm) lt 100 then begin
		   bb=sl_tvsel(m_wm)
		   if (bb ne 1) then begin
			bb=sl_tvdmenu(m_wm)
			if m_wm eq 0 then begin m_rs=-1 & m_py=0 & endif
		   endif else begin
			bb=sl_tvget(28,i)
			bb=sl_tvget(29,j)
			if (i ne m_siz(0,m_wm)) or (j ne m_siz(1,m_wm)) then begin
			   if m_wm ne 0 then bb=sl_tvdmenu(m_wm)
			   m_py=0 & m_rs=-1
			endif else begin
			   bb=sl_tvgcur(i,j, j ,m_ft)
			   if i ge 0 then k =5
			   m_nl =m_nl5(m_wm)
			   m_wd =m_wd5(m_wm)-1
			endelse
		   endelse
    		endif
		k=k+1
	       endwhile
		if flgg eq 7 then begin m_py=0 & m_rs=-1 & endif
		if m_rs eq 0 then begin
		   m_rs=-1 & m_py=0
		   for  i=0,5 do $
			if (ms_tbwin(i) lt 100) and (m_sel5(i) eq 1) then begin
						     m_py=-1 & m_rs=0  &  endif
		endif
	      endif
	      m_pp=m_py
	     endwhile

;	End loop
;	--- ----
	     if m_sta eq 1 then begin bb=sl_x('focus_out') & m_sta=0 & endif
	     bb=sl_tvmod(2,3)
	     bb=sl_tvnobut(0)
	     bb=sl_tvshap(-1)
	     if (wm eq  0) and ((fl eq 0) or (fl eq 2) $
					  or (fl eq 5)) then bb=sl_tvdmenu(wm)
	     if m_rs ge 0 then ab=m_nl-1-m_pp+m_li5(m_wm) + m_wm*100
;**	     re-init m_err
	     if (m_rs eq 1) or (m_rs eq 4) then m_rs=5-m_rs
	     if m_rs ge 0 then m_err=-m_rs
;**
	    endif
	  endif
bb=sl_tvset(6,0)
if m_id gt 0 then  begin bb=sl_tvsel(m_id)
			 if (wm lt 0) or  (fl ne 0) then $
			 if (fl ne 4) and (bb eq 1) and (seq eq 5) then $
						     bb=sl_tvpop(m_id,1)
			 endif
return,ab
end
;
function sl_tvmenu ,  wwm , flgg, txt , ttl , x,y
;******* *********
;**
return,  sl_tvmenub  (wwm , flgg, txt , ttl , x,y ,5)
end
;
function sl_tvmenuc,  wwm , flgg, txt , ttl , x,y
;******* *********
;**      Ok to continu
;**
return,  sl_tvmenub  (wwm , flgg, txt , ttl , x,y ,2)
end
;
function sl_tvmenuh , wwm , flgg, txt , ttl , x,y
;******* **********
;**
common  my_handy, hand_ini,hand_wg,hand_txt,hand_ttl,hand_x,hand_y,hand_scr
;**
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
;**
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
;**
	if mot_f then begin
		 if wwm gt 0 then hand_ini=wwm else hand_ini=-wwm
		 hand_txt=txt
		 hand_ttl=ttl
		 hand_x  =m_dx*x-100 & hand_y  =t_sy-m_dy*y
		 if hand_y lt  25 then hand_y=25
		 if wwm lt 0 then hand_scr=-flgg
		 bb=sl_wghandy(2)
;		 bb=sl_wghandy(3)
		 return,0
	endif else $
		 return,  sl_tvmenub  (wwm , flgg, txt , ttl , x,y ,5)
end
;
function sl_tvmenui , wwm , flgg, txt , ttl , x,y
;******* **********
;**      With input buffer
;**
return,  sl_tvmenub  (wwm , flgg, txt , ttl , x,y ,4)
end
;
function sl_tvmenul , wwm , flgg, txt , ttl , x,y
;******* **********
;**      A list
;**
return,  sl_tvmenub  (wwm , flgg, txt , ttl , x,y ,3)
end
;
function sl_tvmenun , wwm , flgg, txt , ttl , x,y
;******* **********
;**      Notify
;**
return,  sl_tvmenub  (wwm , flgg, txt , ttl , x,y ,5)
end
;
;
function sl_tvmenunw, wwm , flgg, txt , ttl , x,y
;******* ***********
;**      Notify for hourglass
;**
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur

	if not mot_f then return,  sl_tvmenub  (wwm , flgg, txt , ttl , x,y ,5)$
	else bb=sl_wghourglass(0)
return, 1
end
;
function sl_tvdmenunw, wwm
;******* ************
;**
;**      Clear  hourglass
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur

	if not mot_f then return,  sl_tvdmenu(wwm) $
	else bb=sl_wgevent(-1 , 0)
return, 1
end
;
;
function sl_tvgetwn ,wn
;******* **********
;**
;**	Get a free window number (or -1).
;**     --- - ---- ------ ------
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	wn=-1
	i = 1
	while ms_tbwin(i) ge 0 do  begin
	   if ms_tbwin(i) eq 0 then wn=i
	   i=i+1 &  endwhile
return, 1
end
;
;
function sl_tvfirst ,	devs,replic,tv_nc,tv_x,tv_y,tv_dx,tv_dy,$
			tv_od,tv_rd,io_rec,io_dir,mo_tif,tv_vcol,tv_swap,tv_extso
;******* **********
;**
;**	Initialisations.
;**     ---------------
	common  machin,	mc_sys,mc_sta
;**
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
	common  tmp_men,m_wd, m_wi,m_ht,m_ch,m_nl,m_px,m_py,m_ft,m_pp,m_rs ,$
			m_vx5,m_vy5,m_wd5,m_nl5,m_fun5,m_wm,m_xx,m_sp,m_soc,$
			m_losx,m_losy,m_bor,m_li5,m_lf5,m_ll5,m_lx5,m_ly5, $
			m_clop,m_bex1,m_bex2,m_filx,m_bey1,m_bey2,m_fily,m_od,$
			m_pat,m_siz,m_sel5,m_sta,m_str
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_eras,zr,zr_z,zrv2,zrv6,zx4,zy4
	common  my_x,	x_foc,x_st1,x_st2,x_tmp,x_fso,x_p0,x_p1,x_p2,x_bb
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
common  my_handy, hand_ini,hand_wg,hand_txt,hand_ttl,hand_x,hand_y,hand_scr
;**
if devs eq -1 then begin
	m_wd5	= sl_iarr(1,6)
	m_nl5	= sl_iarr(1,6)
	m_fun5	= sl_iarr(1,6)
	m_sel5	= sl_iarr(1,6)
	m_vx5	= sl_iarr(1,7)
	m_vy5	= sl_iarr(1,7)
	m_soc	= sl_iarr(1,7)
	m_losx	= sl_iarr(1,4)
	m_losy	= sl_iarr(1,4)
	m_li5	= sl_iarr(1,6)
	m_lf5	= sl_iarr(1,6)
	m_ll5	= sl_iarr(1,6)
	m_lx5	= sl_iarr(2,5,6)
	m_ly5	= sl_iarr(2,5,6)
	m_bex1	= sl_iarr(2,7,6)
	m_bex2	= sl_iarr(2,7,6)
	m_filx	= sl_iarr(2,2,6)
	m_bey1	= sl_iarr(2,7,6)
	m_bey2	= sl_iarr(2,7,6)
	m_fily	= sl_iarr(2,2,6)
	m_siz	= sl_iarr(2,2,6)
	m_pat	= sl_barr(2,10,10)
	m_str	= '     '

	ms_dmod = sl_iarr(1,2)
	ms_devm = sl_sarr(2,5,6)
	t_max   = 32
	ms_tbwin= sl_iarr(1,t_max+1)
	ms_tbwis= sl_iarr(1,t_max)
	ms_trans= sl_barr(1,256)

	ms_v2	=sl_larr(1, 2)
	ms_xypos=sl_iarr(1, 4)
	ms_xrg  =sl_farr(1, 2)
	ms_yrg  =sl_farr(1, 2)

	zr	=sl_iarr(2,1,1)
	zr_z	=sl_larr(1,17)
	zrv2	=sl_iarr(1,2)
	zrv6	=sl_iarr(1,7)
	zx4	=sl_iarr(1,4)
	zy4	=sl_iarr(1,4)
;
	ms_ini	   =20
	for i=0,255 do ms_trans(i)=ms_ini + i *(255.-ms_ini)/255.

	ms_tbwin(t_max)=-1
	ms_tbwin(0)=100
	ms_tbwin(1)=100
	ms_tbwin(2)=100
	ms_tbwin(3)=100
	ms_tbwin(4)=100
	ms_tbwin(5)=100
;**
;**my_eras
;*********
	zr_z(0) =2
	zr_z(1) =1
	zr_z(2) =1
	zr_z(3) =4
;**
;**my_menu
;*********
	m_err	= 0
	m_ch	= 15
	m_sp	= long(7)
	m_bor	= 5
	m_ft0   ='a14'
	m_ft1	='6x13bold'
	m_colo	= long(0)

	t_sx	= long(0)
	t_sy	= long(0)

	x_tmp	= ''
	x_st1	= byte(0)
	x_st2	= long(0)
	x_p0	= long(0)
	x_p1	= long(1)
	x_p2	= long(2)
	x_bb	= long(0)
;**
;**my_motif
;**********
	mot_f	  = 0
	mot_w	  = sl_larr(1,t_max)
	mot_wg	  = sl_larr(1,t_max)
	mot_wd	  = sl_larr(1,t_max)
	mot_sz	  = sl_larr(2,t_max,4)
	mot_setuv = sl_larr(1,9)
	mot_getuv = sl_larr(1,9)
	mot_wghinf= sl_larr(2,54,t_max) & mot_wghinf(*,*)=-1
	mot_wdcur = sl_larr(1,10)        & mot_wdcur (*)  =-1 & mot_wdcur(8)=0
;**
;**my_handy
;**********
	hand_ini =-1
	hand_wg  = 0
	hand_scr = 24
;
endif
;
;**
;**machin mc_sys=!version.os    device=!d.name (X)
;********
	bb=sl_tvget(24,mc_stt)
	bb=sl_tvget(25,mc_sys)
;
	if mc_sys eq 'vms'    then mc_sta='dec'  else $
	if mc_sys eq 'ultrix' then mc_sta='dec'  else $
	if mc_sys eq 'sunos'  then mc_sta='sun'  else $
	if mc_sys eq 'IRIX'   then mc_sta='sgi'  else $
	if mc_sys eq 'Win32'  then mc_sta='pc'   else $
	if mc_sys eq 'hp-ux'  then mc_sta='hp'   else mc_sta='unk'
;
	mc_sys=sys_dep('MACHINE')
;
	dev	= 1
;**
;**my_sys
;********
	t_so	=  3
	x_foc	= -1
	bb=sl_getlog('SCAN_DIR',ms_iodir)
	if ms_iodir eq '' then bb=sl_getlog('LAMP_DIR',ms_iodir)
	if (mc_sys eq 'unix') or (mc_sys eq 'win') then begin  io_rec=long(10)^10
	      if ms_iodir ne ''  then ms_iodir=ms_iodir+sys_dep('DIVIDER')
	endif
	      x_foc=0
	      sufx =''
	      if mc_sys eq 'vms' then $
	      if mc_stt eq 'vax' then sufx='VAX' else $
	      			      sufx='AXP' else $
	      if mc_sta eq 'hp'  then sufx='HP'  else $
	      if mc_sta eq 'pc'  then sufx='PC'  else $
	      if mc_sta eq 'sgi' then sufx='SGI' else $
	      if mc_sta eq 'sun' then sufx='SUN' else x_foc=-1
	      if x_foc  eq  0    then begin
	      			 x_foc=-100
	      			 x_fso=ms_iodir+'sl_sx_'+sufx+'.so'
				 bb=sl_iofind(x_fso,'',0,mot_str)
				 if bb lt 1 then x_foc=-1 else $
				 if mc_sys eq 'vms' then x_fso='sl_sx_exe'
	      endif
	      bb=sl_iofind(ms_iodir+'surf_'   +sufx+'.so' ,'',0,mot_str)
	      if bb ge 1 then t_so=1 else t_so=0
	      
	      t_so=1	;Use the Idl sl_surf
	      
	      bb=sl_iofind(ms_iodir+'fordeep_'+sufx+'.so' ,'',0,mot_str)
	      if bb ge 1 then t_so=t_so+2

	      if (sys_dep("RUNTIME") or sys_dep("EMBEDDED") or sys_dep("DEMO")) then begin
	                                                         x_foc=-1 & t_so=1 & endif
	if devs ne -1 then begin
;**	   Set t_sx,t_sy,m_colo
;**	   --- ---- ---- ------
	   bb  = sl_x('open')
	   bb  = sl_tvgetwn(i)
	   if m_colo gt 64  then bb=sl_tvclass(8)
	   bb  = sl_tvget(3,dwin)
	   if dwin lt 0 then begin
	    if m_colo gt 256 then $
	 	 bb = sl_tvlux(i,15,15,'SCAN starting',0,0,0,0,0,0,-6 ,0,0,0, 1) $
	    else bb = sl_tvlux(i,15,15,'SCAN starting',0,0,0,0,0,0,-6 ,0,0,0, 1)
	   endif
;	   Font
	   m_ft0   = sys_dep      ('FONTD')
	   m_ft1   = m_ft0
	   if sys_dep('MACHINE') eq 'win' then m_sp = long(6)
	   m_ch	   = 13
	   bb  = sl_tvfont(0)
	   if dwin lt 0 then bb  = sl_tvdelwn(i)
	endif
;
		  bb= sl_iolun(m_ft)
		  if  m_ft gt 0 then bb=sl_iopenr(m_ft,ms_iodir+'100.CTB',1,0)
		  bb= sl_iofree(m_ft)
;
	bb=sl_tvget(33,X)
	ms_devm( 0)='UIS'
	ms_devm( 1)= X
	ms_devm( 2)='SUN'
	ms_devm( 3)='PS'
	ms_devm( 4)='CGM'
	ms_devm( 5)=''
;
	ms_dmod(0) =3
	ms_devp	   =dev
	ms_devs	   =dev
	ms_bcolor  =0
	ms_ncount  =0
;**
;**my_tvi
;********
	t_od	= 0	;!!??
	t_w	=-1
	t_pix	= 1
	t_piy	= 1
;**
;**Get device dependencies     tv_nc=!d.n_colors < 256
;*************************
	bb=sl_tvdev(dev)

;size
	if (t_sy le 0) or (t_sy eq 863) then begin
	t_sx	=1280
	t_sy	=1024
	if mc_stt eq 'vax'   then begin t_sx=1024     & t_sy=863      & endif

;no.pvi
;	if devs ne -1 then begin
	   bb=sl_tvsiz(ms_v2)
	   if ms_v2(0) gt 0 then begin
	      t_sx=ms_v2(0) & t_sy=ms_v2(1) & ms_v2(*)=0 & endif
;	endif
;borders
	t_sx=t_sx - 40
	t_sy=t_sy - 54
	endif
;px-cm
	m_dx=-1  &  m_dy=-1
	bb=sl_tvget(26,m_dx)
	bb=sl_tvget(27,m_dy)
	if m_dx le 0 then begin m_dx = 31 & m_dy = 31 & endif
;misc
	bb=sl_tvget(23,flag)
	t_rep	=(flag and 1)
 	t_rd	=(flag and 128)    & if t_rd  ne 0 then t_rd =1
 	mot_f	=(flag and 65536)  & if mot_f ne 0 then mot_f=1
;colors
	ms_ncol	= 256
	bb=sl_tvget(32,m_colo)
	if m_colo le 0   then m_colo=ms_ncol else $
	if m_colo gt 256 then if devs ne -1 then bb=sl_tvclass(8)
	if m_colo gt 256 then m_colo=ms_ncol
;**
;**Return parameters
;*******************
	io_dir   = ms_iodir
	tv_x	 = t_sx
	tv_y	 = t_sy
	tv_rd	 = t_rd
	tv_od	 = t_od
	tv_nc	 = ms_ncol
	tv_vcol	 = m_colo
	tv_dx	 = m_dx
	tv_dy	 = m_dy
	tv_extso = t_so
	replic	 = t_rep
	mo_tif	 = mot_f
	tv_swap  = 255
	if mc_sys eq 'vms' then tv_swap=1
	if mc_sys eq 'win' then tv_swap=1
return, 1
end
;
;
pro tvstop, dummy
;** ******
	common  machin,	mc_sys,mc_sta
;**
	common	my_tvi,	t_w ,t_rep ,t_max ,t_pix ,t_piy ,t_od ,t_rd ,t_sx ,t_sy
	common  my_menu,m_err,m_dx,m_dy,m_txt,m_ttl,m_x,m_y,m_colo,m_ft0,m_ft1
	common  tmp_men,m_wd, m_wi,m_ht,m_ch,m_nl,m_px,m_py,m_ft,m_pp,m_rs ,$
			m_vx5,m_vy5,m_wd5,m_nl5,m_fun5,m_wm,m_xx,m_sp,m_soc,$
			m_losx,m_losy,m_bor,m_li5,m_lf5,m_ll5,m_lx5,m_ly5, $
			m_clop,m_bex1,m_bex2,m_filx,m_bey1,m_bey2,m_fily,m_od,$
			m_pat,m_siz,m_sel5,m_sta,m_str
	common  my_sys ,ms_devs,ms_tbwin,ms_bcolor,ms_ncount,ms_xrg,ms_yrg,$
			ms_xypos,ms_devm,ms_devp,ms_dmod,ms_ncol,ms_iodir ,$
			ms_tbwis,ms_v2,ms_ini,ms_trans
	common  my_eras,zr,zr_z,zrv2,zrv6,zx4,zy4
	common  my_x,	x_foc,x_st1,x_st2,x_tmp,x_fso,x_p0,x_p1,x_p2,x_bb
	common  my_motif, mot_f,mot_w,mot_wg,mot_wd,mot_setuv,mot_getuv,$
			  mot_resev,mot_t1,mot_t2,mot_sz,mot_wn,mot_str,mot_ev,$
			  mot_t3,mot_t4,mot_t5,mot_t6,mot_wghinf,mot_wdcur
common  my_handy, hand_ini,hand_wg,hand_txt,hand_ttl,hand_x,hand_y,hand_scr
;**
stop
return
end
;
;
;************************ NACS.TS ********************************************
;************************ NACS.TS ********************************************
;************************ NACS.TS ********************************************
;
;
pro sl_grafin,devs
;************
;**Init station
;**---- -------
;  Care tv_nc=256 --> r,g,b,cr,cg,cb
;       my_cl     --> cl_i=tv_nc
;	my_box	  --> bx_tb(*,*),bx_ty(*,*)=tv_nc-1
;	my_glor	  --> f_pl=sl_index(tv_nc,4)
;
	common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		      	tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common	my_tvg,	w_co,w_cw,w_fl,w_fy,w_hi,w_lo,w_lt,w_nc,w_no,w_od,$
			w_ps,w_ty,w_ig,w_wk
;**
	common my_vcol,	r,g,b,	cr,cg,cb
;**
	common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
			io_cur,io_ext,io_seq,io_str
;**
	common my_cl ,	cl_i,cl_cold,cl_ctb,cl_ttl,cl_hlp,cl_colm,cl_v2,cl_v3
;**
	replic=0
	mo_tif=0
	swap  =0
	bb = sl_tvfirst(devs,replic ,tv_nc,tv_x,tv_y,tv_dx,tv_dy,$
			tv_od,tv_rd, io_rec,io_dir,mo_tif,tv_vcol,swap,tv_extso)
	r  = sl_index(tv_nc,4)
	g  = sl_index(tv_nc,4)
	b  = sl_index(tv_nc,4)
	cr = sl_iarr(1,tv_nc)
	cg = sl_iarr(1,tv_nc)
	cb = sl_iarr(1,tv_nc)
	cl_i = tv_nc
;**Set	pixf etc.
;**---  ---- ----
	tv_flg(0) = replic
	tv_flg(1) = mo_tif
	tv_flg(2) = tv_vcol
	tv_flg(3) = -1
	tv_flg(4) = swap
	tv_flg(5) = -1
	if tv_x  gt 800   then tv_flg(6)= 0 else tv_flg(6)=1
	if tv_x  lt 600   then tv_flg(6)= 2
	if tv_flg(6) ne 0 then tv_flg(7)= 1 else tv_flg(7)=0
	tv_flg(8) = tv_x
	tv_flg(9) = tv_y
	tv_flg(17)= tv_extso
;**
return
end
;
;
pro sl_comi  ,dummy
;** *******
;**
;**Init commons
;**---- -------
	common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
			io_cur,io_ext,io_seq,io_str
;**
	common	my_cl,	cl_i,cl_cold,cl_ctb,cl_ttl,cl_hlp,cl_colm,cl_v2,cl_v3
;**
	common  my_tty,	esc,osc,stt,csi,cout
;**
	common	my_kb,	kb_tb,kb_cs,kb_es,kb_ls,kb_gh,kb_bx,kb_by,kb_kk,kb_car
;**
	common  my_box,	bx_tb,bx_ty,bx_fl,bx_dc,bx_pc ,bx_pl ,bx_f,$
			bx_c1,bx_c2,bx_l1,bx_l2,bx_cl1,bx_cl2,bx_lc1,bx_lc2,$
			bx_cx,bx_cy,bx_dx,bx_dy
;**
	common	my_gf,	gf_v,gf_v1,gf_fm
;**
	common	my_xred,xr_is,xr_js,xr_ns,xr_xvl,xr_xvm,xr_rex
;**
	common	my_surf,dms,dm1,dm2,dii,djj,hoo,fxx,fyy,coo,sii,ndd,sv2,sbox,$
			flx,flz,su_aa,su_ah,su_b,su_bb,su_di,su_dj,su_dlx,su_dm,$
			su_fgg,su_ho,su_j,su_nd,su_ni,su_sco,su_ssi,$
			su_sdi,su_sdj,su_bz,su_bh,su_fj,su_fj2,su_mav,su_mnv
;**
	common  my_insert,  i_txt,i_idx,i_fil,i_rout,i_ps,i_rs,$
			    i_trout,i_tfil,i_tlang,i_tdx,i_enter,i_rcall
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
	common  my_conv,care  ,care_z ,cv_k
;**
	common	my_refl,arefl,arefl_z,rf_cur,rf_t
;**
common  my_fun,	a,b,d,e,bfx,bfy,c1,c2,c3,c4,cj,ez,fmf,ifu,jfu,l1,l2,vp,vh,$
		rbx,rby,mn,mx,mni,mxi,h,p,rvmm,rvmi,sp,int7,fmi4,fmf9,fsmo,$
		st1,st2,st3,st4,st5,st6,st7,st8,st9,st10,st11,st12,st13,$
		st14,st15,st16,st17,st18,f24,tap,tip,mnj,mxj,c5,c6,c7,c8,c9,c10
;**
common	my_glor,f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
;**my_cl
;*******
	cl_i	   =    20
	cl_ctb	   =    cl_i-6
	cl_colm	   =	sl_sarr(2,29,cl_i)
	cl_hlp	   =	sl_sarr(2,41,3)
	cl_v2	   =	sl_larr(1,2)
	cl_v3      =    sl_larr(1,3)
	cl_cold	   =    cl_i-1
	cl_i	   =	tv_nc
	cl_ttl	   =   'Palettes'
	cl_colm(0) = '  Black->White  Linear       '
	cl_colm(1) = '  Blue-->White'
	cl_colm(2) = '  Green->Red--->Blue->White'
	cl_colm(3) = '  Red    Temperature'
	cl_colm(4) = '  Blue-->Green->Red-->Yellow'
	cl_colm(5) = '  Standard      Gamma-11'
	cl_colm(6) = '  Prism'
	cl_colm(7) = '  Red--->Purple'
	cl_colm(8) = '  Green->White  Linear'
	cl_colm(9) = '  Green->White  Exponential'
	cl_colm(10)= '  Blue-->Pink'
	cl_colm(11)= '  Blue-->Red'
	cl_colm(12)= '  16  levels'
	cl_colm(13)= '  Col wheel'
	cl_colm(14)= '.'
	cl_colm(15)= '  {Levels} {Rotate} {Inverse}'
	cl_colm(16)= '  Bgrd: {Dark} {Grey} {Light}'
	cl_colm(17)= '  Specify a ".CTB"  file'
	cl_colm(18)= '  Save in a ".CTB"  file'
	cl_colm(19)= '  Return'
	cl_hlp(0)  =   'Use   <Hold Screen> to take time to think'
	cl_hlp(1)  =   ' '
	cl_hlp(2)  =   'Press <Return> key  to stop'
;**
;**my_io
;*******
	io_ext	   = sl_sarr(2,6,18)
	io_ext(0)  = 'SLICE'
	io_ext(1)  = 'REFLEX'
	io_ext(2)  = 'DATA'
	io_ext(3)  = 'RAD'
	io_ext(4)  = 'WIND'
	io_ext(5)  = 'SCAN'
	io_ext(6)  = 'DEEP'
	io_ext(7)  = 'TMP_'
	io_ext(8)  = 'IMG'
	io_ext(9)  = 'CTB'
	io_ext(10) = 'HLP'
	io_ext(11) = 'PRO'
	io_ext(12) = 'FUNC'
	io_ext(13) = 'SIM'
	io_ext(14) = 'WDG'
	io_ext(15) = 'pro'
	io_ext(16) = 'PS'
	io_ext(17) = 'CGM'

	io_cur     = ''
	io_seq	   = 0
;**
;**my_tty
;*******
	esc	=''
	osc     =esc+']'
	stt	=esc+'\'
	csi	=esc+'['
	cout	='                                             '
;**
;**my_kb
;*******
kb_tb='[28~[29~OM  OQ  [1~ [2~ [3~ [4~ [5~ [6~ [A  [D  [B  [C  '
kb_cs=[1   ,2   ,100 ,102 ,101 ,4   ,5   ,6   ,7   ,8   ,9   ,10  ,11  ,12  ]
;
;---------F1---F2---F3---F4---F5---F6---F7---F8---F9--F10--F11--F12--F13--->SGI
kb_tb='[001[002[003[004[005[006[007[008[009[010[011[012[013'+kb_tb
kb_cs=[   24  ,25  ,26  ,27 ,101   ,0   ,0 ,103 ,104 ,105  ,20  ,21  ,22 ,kb_cs]
;--------------------------------------------------------------------------> HP
;
;--------F7---F1---F2---F3---F4---delete-------------insert------------F5--> HP
kb_tb='[18~[11~[12~[13~[14~'+string(127b)+'    '+string(9b)+'    [15~'+kb_tb
kb_cs=[0   ,24  ,25  ,26  ,27  ,    5       ,           4            ,101   ,kb_cs]
;
;      control_B          control-C         control-Y
kb_tb= string(2b) +'    '+string(3b) +'    '+string(25b)+'    '+kb_tb
kb_cs=[      140  ,             131  ,              131        ,kb_cs]
;
;      control_F          control-P         control-E
kb_tb= string(6b) +'    '+string(16b)+'    '+string(5b) +'    '+kb_tb
kb_cs=[       23  ,             102  ,              22	       ,kb_cs]
;
;      control_K          control-L         control-
kb_tb= string(11b)+'    '+string(12b)+'    '                   +kb_tb
kb_cs=[       20  ,             21                             ,kb_cs]
;
;      control_X          control-R         control-G
kb_tb= string(24b)+'    '+string(18b)+'    '+string(7b) +'    '+kb_tb
kb_cs=[       0   ,             142  ,              3 	       ,kb_cs]
;
;      control_Z          control_O	      Break   Menu
kb_tb= string(26b)+'    '+string(15b)+'    '+'65387'+'65383'   +kb_tb
kb_cs=[       131 ,             15   ,        140   , 141      ,kb_cs]
;
kb_tb='?    (    )    ((((()))))OP  [19~[20~[21~'+kb_tb
kb_cs=[1   ,13  ,14  ,13  ,14  ,15  ,103 ,104 ,105   ,kb_cs]
kb_tb='((   ))   (((  )))  (((( )))) '+kb_tb
kb_cs=[13  ,14  ,13  ,14  ,13  ,14    ,kb_cs]
;
kb_tb='[23~[24~[25~[26~[31~[32~[33~[34~'+string(13b)+'    '+kb_tb
kb_cs=[20  ,21  ,22  ,23  ,24  ,25  ,26  ,27  ,         33         ,kb_cs]
;
kb_tb='+    <    >    -    I    C    R    P    G    L    S    T    B    '+kb_tb
kb_cs=[16  ,17  ,18  ,19  ,29  ,30  ,31  ,32  ,34  ,35  ,36  ,37  ,38    ,kb_cs]
;
kb_tb='J    K    V    M    @    /    E    W    A    j    k    '+kb_tb
kb_cs=[28  ,39  ,80  ,81  ,82  ,83  ,84  ,85  ,86  ,89  ,199 ,  kb_cs]
;
kb_tb='a    s    l    i    y    x    c    d    f    r    $    g    O    '+kb_tb
kb_cs=[40  ,41  ,42  ,43  ,44  ,45  ,46  ,47  ,48  ,49  ,50  ,51  ,53    ,kb_cs]
;
kb_tb='w    q    b    N    _    !    p    =    ~    m    #    %    .    '+kb_tb
kb_cs=[54  ,55  ,56  ,57  ,60  ,63  ,62  ,65  ,66  ,90  ,91  ,92  ,93  ,  kb_cs]
;
kb_tb='*    ^    t    u    e    :    ;    |    h    o    n    v    z    '+kb_tb
kb_cs=[94  ,95  ,96  ,97  ,98  ,99  ,100 ,61  ,67  ,68  ,69  ,58  ,87  ,  kb_cs]
;
kb_tb='0    1    2    3    4    5    6    7    8    9    '+kb_tb
kb_cs=[200 ,201 ,202 ,203 ,204 ,205 ,206 ,207 ,208 ,209 ,  kb_cs]
;
kb_tb='8    4    2    6    88   44   22   66   888  444  222  666  '+kb_tb
kb_cs=[9   ,10  ,11  ,12  ,9   ,10  ,11  ,12  ,9   ,10  ,11  ,12    ,kb_cs]
kb_tb='8888 4444 2222 6666 88888444442222266666'+kb_tb
kb_cs=[9   ,10  ,11  ,12  ,9   ,10  ,11  ,12    ,kb_cs]
;
kb_tb='Z    Y    X    U    Q    H    F    D    [    ]    \    {    }    '+kb_tb
kb_cs=[106 ,107 ,108 ,109 ,110 ,111 ,112 ,113 ,114 ,115 ,116 ,117 ,118 ,  kb_cs]
;
kb_tb='     '+kb_tb
kb_cs=[0     ,kb_cs]
;
;Free  F7  Tab w
;****  **  *** *
;
kb_gh=['a','s','l','i','y','x','c','d',' ','_','|','r','!','p','#','n','o','v']
kb_es=esc
kb_ls= 0
kb_kk= 0
kb_car=''
;**
;**my_box
;********
if tv_ini lt 2400 then bx_tb = sl_iarr(2,2400  ,8) $
		  else bx_tb = sl_iarr(2,tv_ini,8)
if tv_y   lt 2400 then bx_ty = sl_iarr(2,2,2400)   $
		  else bx_ty = sl_iarr(2,2,tv_y)
bx_tb(*,*)=tv_nc-1
bx_ty(*,*)=tv_nc-1
bx_dc = sl_iarr(2,3,10)
bx_fl = sl_iarr(1,4)
bx_pc = sl_iarr(1,2)
bx_pl = sl_iarr(1,2)
;**
;**my_gf
;*******
	gf_v    =0.0
	gf_v1	=sl_farr(1,4)
	gf_fm	=sl_sarr(2,6,6)
	gf_v1(0)=1E+08
	gf_v1(1)=1E+07
	gf_v1(2)=1E+05
	gf_v1(3)=1E+03
	gf_fm(0)	='(I9)  '
	gf_fm(1)	='(E9.2)'
	gf_fm(2)	='(F9.0)'
	gf_fm(3)	='(F9.2)'
	gf_fm(4)	='(F9.4)'
	gf_fm(5)	='(E9.2)'
;**
;**my_xred
;*********
;
	xr_xvl	=0.0
	xr_xvm	=0.0
	xr_rex	=sl_larr(1,3)
;**
;**my_surf
;*********
	dms	=sl_larr(1,17)
	sv2	=sl_larr(1,2)
	sbox	=sl_larr(2,4,3)
	fxx	=0.0
	fyy	=0.0
	coo	=0.0 & sii    =0.0
	su_sco  =0.0 & su_ssi =0.0
	su_bh	=0.0
	su_fj	=0.0
	su_fj2	=2. & bb=sl_sqrt(su_fj2,1)
;**
;**my_insert
;***********
	i_ps	  = sl_larr(1,3)
	i_idx	  = sl_larr(1,2)
	i_idx(0)  = 60
	i_tlang	  = sl_iarr(1,i_idx(0))
	i_trout	  = sl_sarr(2,28,i_idx(0))
	i_tfil	  = sl_sarr(2,50,i_idx(0))
	i_txt	  = sl_sarr(2,57,i_idx(0))
	i_txt(0)  ='.EXTERNAL FUNCTION CALL                                  '
	i_txt(1)  ='.-------- -------- ----                                  '
	i_txt(2)  ='.B=Your_function (PASSAREA ,DI,DJ,DK, CURIJK ,TYPE)      '
	i_txt(3)  ='.'
	i_txt(4)  ='.  PASSAREA is current data image of dimensions DI,DJ,DK '
	i_txt(5)  ='.  CURIJK(3)is current pointer coordinates  '
	i_txt(6)  ='.  TYPE     of data  : 1=byte 2=i*2 3=long 4=f 5=df 6=cpx'
	i_txt(7)  ='.'
	i_txt(8)  ='.  B:if  1  the new image is created from PASSAREA '
	i_txt(9)  ='.    if  0  current image remains unchanged        '
	i_txt(10) ='.'
	i_txt(11) ='.CHOOSE YOUR FUNCTION TO INSERT IN HANDY-KEYS '
	i_txt(12) ='.------ ---- -------- -- ------ -- ----------'
	i_txt(13) ='.>'
	i_idx(1)  = 14
	i_trout(0)='External Functions Recall=^R'
	i_trout(1)='******** *********          '
	i_tdx	  = 2
	i_fil	  ='      '
	i_rout	  ='      '
	i_enter   ='k'
	i_rcall   = 0
;**
;**my_area
;*********
	ares_z	= sl_larr(1,17)
	areb_z	= sl_larr(1,17)
	arec_z	= sl_larr(1,17)
	ared_z	= sl_larr(1,17)
	arev_z	= sl_larr(1,17) & arev_z(0) = [2,0,11,8,0,0,0]
	aref_z	= sl_larr(1,17)
	arer_z	= sl_larr(1,17)
	sare_z	= sl_larr(1,17)
	tare_z	= sl_larr(1,17)
	vare_z	= sl_larr(1,17)
	areu_z	= sl_larr(1,17)
	arei_z	= sl_larr(1,17)
	arex_z	= sl_larr(1,17)
	arey_z	= sl_larr(1,17)
	arel_z	= sl_larr(1,17)
	aregx_z	= sl_larr(1,17)
	aregy_z	= sl_larr(1,17)
	areo_z	= sl_larr(1,17)
;**
;**my_conv
;*********
	care_z	= sl_larr(1,17)
;**
;**my_refl
;*********
	arefl_z	= sl_larr(1,17)
	rf_cur	= 0
;**
;**my_fun
;********
		h	= sl_farr(1,2)
		p	= sl_farr(1,2)
		vh	= sl_farr(1,2)
		vp	= sl_farr(1,2)
		int7	= sl_iarr(1,7)
		ifu	= 0
		jfu	= 0
		f24	= 0
		fsmo	= 0
		fmf	= '      '
		fmi4	= '(i4)'
		fmf9	= '(f9.4)'
		st1	= '                '
		st2	= '                '
		st3	= '                '
		st4	= 'dx*dy:     *    '
		st5	= 'V min:          '
		st6	= 'V max:          '
		st7	= 'Angl:Z     X    '
		st8	= 'S_Angle         '
		st9	= 'Deviat:         '
		st10	= 'Y=         *X          '
		st11	= 'Cut X=          Y=         '
		st12	= 'Ellips Ang:     '
		st13	= ' SUM/AVG '
		st14	= '  SIGNAL '
		st15	= ' SG/NOISE'
		st16	= '  STDEV  '
		st17	= 'N pts:          '
		st18	= ' BGRD AVG'
		c4	=  0. & c5=0. & c6=0  & c7=0. & c8=0. & c9=0. & c10=0.
;**
;**my_glor
;*********
;**             help
;**             ----
		f_ib	= 60
		f_h3    = sl_sarr(2,80,f_ib+4)
		f_h2    = sl_sarr(2,80,2)
		f_h1    = sl_sarr(2,80,f_ib)
		f_h2(0) =''
		f_h2(1) ='Click here to continue...'
;**
		f_tt	= sl_sarr(2,15,16)
		f_tt(0) ='Current Set    '
		f_tt(1) ='Z axis  Angle  '
		f_tt(2) ='Viewing Angle  '
		f_tt(3) ='N Levels'
		f_tt(4) ='New X Size'
		f_tt(5) ='New Y Size'
		f_tt(6) ='XY Factor'
		f_tt(7) ='X Size Box'
		f_tt(8) ='Y Size Box'
		f_tt(9) ='N ------->'
		f_tt(10)='Square Box Size'
		f_tt(11)='Starting Frame'
		f_tt(12)='Frame number'
		f_tt(13)='Reflex number'
		f_tt(14)='Lower  limit'
		f_tt(15)='Upper  limit'
;**		G_H
;**		---
			f_ab	 =sl_larr(2,2,3) & f_ab(0,0)=-1
			f_pl     =sl_iarr(2,tv_nc,1)
			f_pl(0,0)=sl_index (tv_nc,4)
			f_fg  = sl_larr(1,53)
			f_cn  = long(0)
			f_ln  = long(0)
			f_zn  = long(0)
			f_vu  = 3  &    f_w1 =-1
			f_wx  = long(tv_x/2.69)
			f_wy  =(long(tv_y/21.2)+1)/2	& f_wy=f_wy*2
			if f_wx gt 380 then f_wx=383
			if f_wy gt  40 then f_wy= 40
			f_wp  =(f_wx*2/3 +1)/2		& f_wp=f_wp*2
			f_wx  = f_wp*3/2
;
			f_fg(3 )=1
			if f_fg(3) eq 1 then f_wy=f_wy*4
			if f_fg(3) eq 2 then f_wy=f_wy*8

			tv_w  =f_wy + 30
			f_py  =tv_y - tv_w + 2
;
			f_ax  = 65 &    f_az =30 & f_ic= 0
			f_el  = 0. &    f_sh = 1
			f_fg(0 )=0 & f_fg(1 )=40
			f_fg(4 )=1 & f_fg(2 )=f_fg(1) * f_wy / f_wp
			f_fg(6 )=1 & f_fg(11)=-1
			f_fg(12)=0
			f_fg(14)=2 ;surface
			f_fg(15)=12
			f_fg(16)=12;contour
			f_fg(17)=0
			f_fg(18)=0
			f_fg(19)=1
			f_fg(20)=1 ;surface in box (3=none 2=fun)
			f_fg(27)=1
			f_fg(44)=-1
			f_fg(45)=1
			if tv_mps  lt 2 then f_fg(1 )=16
			if tv_mps  lt 2 then f_fg(2 )=16
			if tv_mps  lt 1 then f_fg(4 )= 0
			if tv_mps  lt 8 then f_fg(12)= 0
;**
	sl_comi2,0
return
end
;
pro sl_comi2	,dummy
;** ********
;**
	common  my_matx,zstring,zimg,excor,excnc,exmat,extyp,exconf,exmit,$
			exfrm,exfri,m_frm,m_fri ,m_v6,m_pos,m_bo,$
			m_dm1,m_dm2,m_my,m_i,m_j,m_nc,m_rec,m_sr,m_typ,m_u,m_x1
;**
	common my_rotfun,wf,azt,axt,fx1,fx2,fy1,fy2,px,py,stepz,stepx,s1,s2,s3,$
		    	 sso,fpp,fpb,fum,fux,ndx,ndy,ndz,rtyp,wcw,wno,wod
;**
	common my_opview,o_xdm1,o_ydm1,o_zdm1,o_typ1, $
			 o_xdm2,o_ydm2,o_zdm2,o_typ2,o_tip,o_xi,o_yi,o_zi
;**
	common	my_tvg,	w_co,w_cw,w_fl,w_fy,w_hi,w_lo,w_lt,w_nc,w_no,w_od,$
			w_ps,w_ty,w_ig,w_wk
;**
	common	my_err,	err_1 ,err_2 ,err_3 ,err_4 ,err_5 ,err_6 ,err_7 ,err_8
;**
common my_click, tc_bb,tc_nb,tc_n2,tc_rti,tc_st,tc_tmtl,tc_x,tc_xc,tc_xd,$
		 tc_xs,tc_y,tc_yp,tc_yl,tc_zerr,tc_w_cw,tc_w_no,tc_w_ft,tc_7,$
		 tc_bo,tc_ttl,tc_st2,tc_x03,tc_y03,tc_x13,tc_y13,tc_x04,tc_y04,$
		 tc_sz,tc_are,tc_vsz,tc_sel
;**
;**my_matx
;*********
       		zstring		= sl_sarr(2,1,112)
		exconf		= sl_sarr(2,71,3)
		exmat    	= sl_sarr(2,71,10)
		exmit    	= sl_sarr(2,21,8)
;
		excor	   	= [0,2,4,5,16,8,64,32,0,6]
		excnc	   	= [0,1,2,2, 4,4,8 , 8,0,0]
		extyp    	= sl_sarr(2,25,10)
;
		exfrm    	= sl_sarr(2,22,14)
		exfri    	= sl_iarr(1,14)
		m_my		= sl_sarr(2,50,1)
		m_x1		= sl_sarr(2,20,1)
		m_v6		= sl_larr(1,6)
		zimg		= 1
		exmit(0)	='1} Change file-name :'
		exmit(1)	='2} Dimensions x,y,z :'
		exmit(2)	='3} Change the type  :'
		exmit(3)	='4} Record size(byte):'
		exmit(4)	='5} Starting record  :'
		exmit(5)	='5} Byte offset 1->n :'
		exmit(6)	='6} Format  of data  :'
		exmit(7)	='   Data Description  '
		exmat(6)   	='.  ----'
		exmat(7)   	='7} Read the file'
		exmat(8)   	='8} Delete the file'
		exmat(9)   	='9} Quit'
;
		exconf(1)	='.'
		exconf(2)	='Preserve the file'
;
		extyp(0) 	='No change                '
		extyp(1) 	='Byte                     '
		extyp(2) 	='Integer 2                '
		extyp(3) 	='Integer 2 positive       '
		extyp(4) 	='Integer 4 long           '
		extyp(5) 	='Floating_point'
		extyp(6) 	='Complex floating'
		extyp(7) 	='Double_precision floating'
		extyp(8) 	='.'
		extyp(9) 	='+swap byte'
;
		exfrm(0)        ='.       IMAGES        '
		exfrm(1)        ='.       ------        '
		exfrm(2)        ='Unformatted   Fortran '
		exfrm(3)        ='Stream  vms   binary  '
		exfrm(4)        ='Tiff g,p Uncompressed '
		exfrm(5)        ='Ccp4   (.map  binary) '
		exfrm(6)        ='Stream unix ,Fixed vms'
		exfrm(7)        ='Mar image plate       '
		exfrm(8)        ='Formatted     Ascii   '
		exfrm(9)        ='No change             '
		exfrm(10)       ='.     COORDINATES'
		exfrm(11)       ='.     -----------'
		exfrm(12)       ='Formatted ix,iy,value '
		exfrm(13)       ='Formatted val,ix,iy,iz'
;
		exfri(0)	=[-1,-1, 5,3,2,6,0,7,1, -1,-1,-1,4, 8]
		m_sr		= long(1)
		m_fri		= 6
		m_pos		= 0
		m_bo		= 0
		m_frm		= exfri(m_fri)
;
		m_x1 (0)  	='Reading the file ...'
;**
;**my_tvg
;********
		w_wk		= 0
;**
;**my_err
;********
		err_1		='%Scan... No file found.'
		err_2		='%Scan... Quota Disk or privilege error.'
		err_3		='%Scan... File open error.'
		err_4		='%Scan... Command unsatisfied in this context.'
		err_5		='%Scan... Operation finds bad dimensions.'
;**
;**my_click
;**********
		tc_x03		= sl_iarr(1,3)
		tc_y03		= sl_iarr(1,3)
		tc_x13		= sl_iarr(1,3)
		tc_y13		= sl_iarr(1,3)
		tc_x04		= sl_iarr(1,4)
		tc_y04		= sl_iarr(1,4)
		tc_7		= sl_iarr(1,7)
		tc_sel		= sl_sarr(2,7,2)
		tc_sel(0)	='Select '
		tc_sel(1)	='Desktop'
		tc_st		='     '
		tc_st2		='     '
		tc_tmtl		='                                           '
		tc_ttl		=' Use Arrows and <Return> Keys'
		tc_sz		= 52
		tc_are		= sl_barr(2,tc_sz,tc_sz/2+1)
		tc_vsz		= [2, tc_sz,tc_sz/2+1 ,2,0,0,tc_sz*(tc_sz/2+1)]
;**
	sl_comj,0
return
end
;
pro sl_comj	,dummy
;** *******
;**
	common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common  my_which,wh_d,wh_m,wh_n,wh_s,wh_t,wh_spc,wh_tb,wh_ti
;**
	common  my_space,sp_si,sp_sj,sp_sx,sp_sy,sp_sz,sp_px1,sp_px2,sp_py1,$
			sp_py2,sp_fdx,sp_fdy,sp_fdz,sp_dx,sp_dy,$
			sp_vssz,sp_res,sp_stt
;**
	common  my_trsig,tr_bb,tr_dirc,tr_i,tr_n,tr_u,tr_v2,tr_v3
;**
	common  my_sr,	sr_bb,sr_dirc,sr_dwn,sr_num,sr_spdl,sr_spt,sr_u,$
			sr_winc,sr_v2,sr_v3,sr_typ
;**
	common  my_ovs,	ov_sum1,ov_sum2,ov_sum3,ov_sum4,ov_sum5,ov_sum6,ov_sum7,$
			ovs1_z ,ovs2_z ,ovs3_z ,ovs4_z ,ovs5_z ,ovs6_z ,$
			ov_pmx,ov_f,ov_z,ov_l,ov_m
;**
	common my_fit , fi_ez,fi_typ,fi_f,fi_nx,fi_ny,fi_nz,fi_ne,fi_l,fi_coef,$
			fi_pcoef,fi_min,fi_max
;**
	common	my_geto,go_v7,go_v2,go_v3,go_rql,go_rqm,go_x5,go_y5
;**
common my_views,  abt,az,ax,bbx,bby,bcx,bcy,bti,btj,btx,bty,btw,bwx,bwy,c1,cc ,$
		  cf,cm,ck1,dif,dif3,fc,fcg,fic,fil,fmt,four,fxy,fx,fy,f_0,f_1,$
		  f_2,f_3,f_4,f_5,f_6,f_7,hh,ii2,ii3,ii6,k1,k2,kk,lc,lk2,mn,mx
common my_views2, mx1,mx2,nx,ny,nz,o,op4,op5,plx,ply,pp,rot,spc,spm,spt,stc   ,$
		  stf,stl,spm_t,spt_t,tip,tite,titx,vsis,vsx,vsy,vsz,w,xsiz ,$
		  xdm,ydm,zdm,vxl,vxm,km,bxa,bya,v_vx4,v_vy4
;**
;**my_which
;**********
	wh_d	  =' Dim: ('
	wh_m	  =') * ('
	wh_n	  ='None'
	wh_s	  =' Scan: '
	wh_t	  ='Views currently loaded'
;**
	wh_spc	  =  sl_sarr(2,10,9)
	wh_spc(0) = 'Molecule  '
	wh_spc(1) = ' '
	wh_spc(2) = 'Surf proj.'
	wh_spc(3) = 'Project.*3'
	wh_spc(4) = 'Deep proj.'
	wh_spc(5) = 'Levels'
	wh_spc(6) = 'Image '
	wh_spc(7) = 'Surface'
	wh_spc(8) = 'Vectors'
;**
	wh_tb	  =  sl_sarr(2,71,tv_wsz(1)+2)
	wh_ti	  =  sl_larr(1,tv_wsz(1)+2)
;**
;**my_space
;**********
	sp_vssz	= sl_larr(1,17)
	sp_res	= sl_larr(1,3)
	sp_stt	= sl_larr(1,3)
;**
;**my_trsig
;***********
	tr_v2	= sl_larr(1,2)
	tr_v3	= sl_larr(1,3)
	tr_dirc = '          '
;**
;**my_sr
;*******
	sr_winc	= sl_larr(1,tv_wsz(0))
	sr_v2	= sl_larr(1,2)
	sr_v3	= sl_larr(1,3)
	sr_typ	= sl_sarr(2,6,7)
	sr_dirc = '          '
	sr_dwn  = '          '
	sr_spdl = '          '
	sr_typ(0)='string'
	sr_typ(1)='integ1'
	sr_typ(2)='integ2'
	sr_typ(3)='float4'
	sr_typ(4)='integ4'
	sr_typ(5)='float8'
	sr_typ(6)='complx'
;**
;**my_ovs
;********
	ov_sum7 = 0
	ovs1_z	= sl_larr(1,17)
	ovs2_z	= sl_larr(1,17)
	ovs3_z	= sl_larr(1,17)
	ovs4_z	= sl_larr(1,17)
	ovs5_z	= sl_larr(1,17)
	ovs6_z	= sl_larr(1,17)
;**
;**my_fit
;********
	fi_coef = sl_farr(1,6)
	fi_pcoef= sl_farr(2,1,21)
;**
;**my_geto
;********
	go_v7	= sl_larr(1,7)
	go_v2	= sl_iarr(1,2)
	go_v3	= sl_iarr(1,3)
	go_x5	= sl_iarr(1,5)
	go_y5	= sl_iarr(1,5)
;**my_views
;**********
		fcg	= sl_larr(1,3)
		vsis	= sl_larr(1,17)
		xsiz	= sl_larr(1,17)
		abt	= sl_larr(1,3)
		pp	= sl_larr(1,2)
		hh	= sl_larr(1,2)
		v_vx4	= sl_larr(1,4)
		v_vy4	= sl_larr(1,4)
		spm_t	= sl_sarr(2,10,10)
		spt_t	= sl_sarr(2,14, 7)
		spt_t(0)='   Scan       '
		spt_t(1)='   Frm:   V   '
		spt_t(2)='   Deep       '
		spt_t(3)='   Proj       '
		spt_t(4)='   Pile       '
		spt_t(5)=' '
		spt_t(6)='   Mole       '
		spm_t(0)='   Corr   '
		spm_t(1)='   Sdev   '
		spm_t(2)='   SumZ   '
		spm_t(3)='   Tran   '
		spm_t(4)='   View   '
		spm_t(5)='   From   '
		spm_t(6)='   SumF   '
		spm_t(7)='   SumX   '
		spm_t(8)='   SumY   '
		spm_t(9)='   Dist   '
		tite	='              '
		titx	='              '
		fmt	='      '
		ii2	='(i2)'
		ii3	='(i3)'
		ii6	='(i6)'
;**
return
end
;
pro sl_inview  ,dummy
;** *********
;** Init viewer tables.
;** ---- ------ ------
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
       		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
;**
;**my_keep
;*********
		ex_gh	=  sl_sarr(2,16,3)
		expc	=  sl_sarr(2,43,22)
		expd	=  sl_sarr(2,50,18)
		expf	=  sl_sarr(2,43,22)
		expg	=  sl_sarr(2,43,15)
		expgs	=  sl_sarr(2,38,18)
		exph	=  sl_sarr(2,43, 7)
		explm	=  sl_sarr(2,43, 2)
		expm	=  sl_sarr(2,33, 2)
		expn	=  sl_sarr(2,38, 4)
		expo	=  sl_sarr(2,50,19)
		expp	=  sl_sarr(2,50,27)
		expr	=  sl_sarr(2,35, 5)
		expy	=  sl_sarr(2,49,11)
		expex	=  sl_sarr(2,28, 7)
		exphc	=  sl_sarr(2,28, 9)
		exsph	=  sl_sarr(2,28,125)
		exspc	=  sl_sarr(2,28, 20)
		exadj	=  sl_sarr(2,43, 6)
		exrad	=  sl_sarr(2,43, 6)
		exci	=  sl_sarr(2,43, 6)
		exfi	=  sl_sarr(2,43, 6)
		expb	=  sl_sarr(2,43, 6)
		expe	=  sl_sarr(2,43, 6)
		expfl	=  sl_sarr(2,43, 6)
		expk	=  sl_sarr(2,43, 6)
		expl	=  sl_sarr(2,43, 6)
		expo1	=  sl_sarr(2,43, 6)
		expo2	=  sl_sarr(2,43, 6)
		expo3	=  sl_sarr(2,43, 6)
		exps	=  sl_sarr(2,43, 6)
		expw	=  sl_sarr(2,43, 6)
		expx	=  sl_sarr(2,43, 6)
		exsf	=  sl_sarr(2,43, 6)
		exff	=  sl_sarr(2,43, 6)
		exprs	=  sl_sarr(2,43, 6)
;**
		exy	=  sl_iarr(3,30, 4,4)
		exz	=  sl_iarr(2,30, 6)
		exz(29,*)=-1
;**
		ex_gh(0)= '   From    Radial Sum '
		ex_gh(1)= '   From    Reflex Sum '
		ex_gh(2)= '   From    Slice'
;**
		ex_c	= 'Scan: contents'
		expc(0) = '{C}Load a  new  color  table               '
		expc(1) = '   Adjust color limit {Reset}{Adjust}{None}'
		expc(2) = '{R}Rescale image       (not the data)      '
		expc(3) = '.'
		expc(4) = '{/}Slice (interactive process)'
		expc(5) = '{?}                                  {Help}'
		expc(6) = '{E}Back to non expanded image: un_zoom     '
		expc(7) = '   Output  display    see "h"       {Print}'
		expc(8) = '   Select  profiles , options  MENU{Select}'
		expc(9) = '   Insert  external functions  MENU{Insert}'
		expc(10)= '.'
		expc(11)= '   Other graphical representation MENU{F11}'
		expc(12)= '   Image processing: convolutions MENU{F12}'
		expc(13)= '   Data  processing:miscellaneous MENU {^E}'
		expc(14)= '   Simple mathematical  functions MENU {^F}'
		expc(15)= '   Frames operations              MENU {J} '
		expc(16)= '.'
		expc(17)= '{H}Save data and  display construction     '
		expc(18)= '   Remove    this view       {Del} {Remove}'
		expc(19)= '{D}Duplicate this view '
		expc(20)= '{Q}Quit      this view '
		expc(21)= '   None'
;**		Case entry...
		exz(0 ,0)=0
		exz(1 ,0)=1
		exz(2 ,0)=2
		exz(3 ,0)=3
		exz(4 ,0)=4
		exz(5 ,0)=5
		exz(6 ,0)=6
		exz(7 ,0)=7
		exz(8 ,0)=8
		exz(9 ,0)=20
		exz(10,0)=9
		exz(11,0)=10
		exz(12,0)=11
		exz(13,0)=12
		exz(14,0)=13
		exz(15,0)=14
		exz(16,0)=15
		exz(17,0)=16
		exz(18,0)=19
		exz(19,0)=17
		exz(20,0)=22
		exz(21,0)=18
;**
		ex_f	= 'Scan: profiles'
		expf(0) = '{a}Glory_Hole- Put G_Hole  aside        '
		expf(1) = '{s}         -- Surface profile          '
		expf(2) = '{l}         -- Levels  profile          '
		expf(3) = '{i}         -- Image   profile          '
		expf(4) = '{y}         -- Y       profile          '
		expf(5) = '{x}         -- X       profile          '
		expf(6) = '{c}         -- Show  Color palette      '
		expf(7) = '{d}         -- Avrg,frames Deviation    '
		expf(8) = '{f}            Viewfinder size     *    '
		expf(9) = '{r}            Rescale   data profil    '
		expf(10)= '{$}            Integrate data profil    '
		expf(11)= '{g}            Other  G_Hole  size      '
		expf(12)= '               Other  G_H  options      '
		expf(13)= '{O}Views-----> Smooth stretched view    '
		expf(14)= '{w}            Window Borders           '
		expf(15)= '{q}            Square off the frames    '
		expf(16)= '{=}            Scale  frm separately    '
		expf(17)= '{N}            Logarithmic   display    '
		expf(18)= '{b}Data -----> Stack modified data      '
		expf(19)= '{h}Hard-copy-> Options                  '
		expf(20)= '.'
		expf(21)= '   Return'
;**		Case entry...
		exz(0 ,1)=0
		exz(1 ,1)=1
		exz(2 ,1)=2
		exz(3 ,1)=3
		exz(4 ,1)=4
		exz(5 ,1)=5
		exz(6 ,1)=6
		exz(7 ,1)=7
		exz(8 ,1)=8
		exz(9 ,1)=9
		exz(10,1)=10
		exz(11,1)=11
		exz(12,1)=12
		exz(13,1)=13
		exz(14,1)=14
		exz(15,1)=15
		exz(16,1)=25
		exz(17,1)=17
		exz(18,1)=16
		exz(19,1)=27
		exz(20,1)=18
		exz(21,1)=19
;**
;**
		exph(0) = '{_}      Horiz. section over  frames    '
		exph(1) = '{|}      Verti. section over  frames    '
		exph(2) = '{p}      Sum over suitable direction    '
		exph(3) = '{!}      Enhance  profile               '
		exph(4) = '{0}      Remove enhancement             '
		exph(5) = '.'
		exph(6) = '         Return'
;**		Case entry...
		exz(0 ,2)=20
		exz(1 ,2)=21
		exz(2 ,2)=22
		exz(3 ,2)=23
		exz(4 ,2)=24
		exz(5 ,2)=18
		exz(6 ,2)=19
;**
		ex_d	= 'Scan: Data Processing'
		expd(0) = '   Rescale the data with  new limits             '
		expd(1) = '{K}Reduce  the data according to the view        '
		expd(2) = '   Data compression or stretching'
    		expd(3) = '   Conversion: {float}   {long}   {byte}'
    		expd(4) = '   Conversion: {integ2}  {double} {none}'
		expd(5) = '.'
		expd(6) = '{W}Extract data to produce a file or an image'
		expd(7) = '   Extract reflections coordinates'
		expd(8) = '.'
		expd(9) = '{F} Fit  procedures '
		expd(10)= '   Show vertices'
;		expd(11)= '   Show frequencies {Power} {Phase angle} {Imagi}'
		expd(11)= '   Make densities (use contour parameters)'
		expd(12)= '.'
		expd(13)= '   Radial Integrations'
		expd(14)= '.'
		expd(15)= '   Simple Mathematical Functions menu'
		expd(16)= '{B}Back  to  previous data      (if stacked)'
		expd(17)= '   None    '
;**		Save before...   Cut before...   D_P entry....    Case entry...
		exy (0 ,0,1)=0 & exy(0 ,1,1)=0 & exy(0 ,2,1)=0  & exy(0 ,3,1)=4
		exy (1 ,0,1)=0 & exy(1 ,1,1)=1 & exy(1 ,2,1)=0  & exy(1 ,3,1)=2
		exy (2 ,0,1)=1 & exy(2 ,1,1)=1 & exy(2 ,2,1)=48 & exy(2 ,3,1)=13
		exy (3 ,0,1)=1 & exy(3 ,1,1)=0 & exy(3 ,2,1)=38 & exy(3 ,3,1)=0
		exy (4 ,0,1)=1 & exy(4 ,1,1)=0 & exy(4 ,2,1)=39 & exy(4 ,3,1)=0
;
		exy (6 ,0,1)=0 & exy(6 ,1,1)=0 & exy(6 ,2,1)=0  & exy(6 ,3,1)=21
		exy (7 ,0,1)=0 & exy(7 ,1,1)=0 & exy(7 ,2,1)=0  & exy(7,3,1)=27
;
		exy (9 ,0,1)=1 & exy(9 ,1,1)=0 & exy(9 ,2,1)=0  & exy(9 ,3,1)=23
		exy (10,0,1)=1 & exy(10,1,1)=0 & exy(10,2,1)=29 & exy(10,3,1)=0
;		exy (11,0,1)=1 & exy(11,1,1)=1 & exy(11,2,1)=11 & exy(11,3,1)=6
		exy (11,0,1)=0 & exy(11,1,1)=0 & exy(11,2,1)=11 & exy(11,3,1)=6
;
		exy (13,0,1)=0 & exy(13,1,1)=0 & exy(13,2,1)=0  & exy(13,3,1)=29
;
		exy (15,0,1)=0 & exy(15,1,1)=0 & exy(15,2,1)=0  & exy(15,3,1)=7
		exy (16,0,1)=0 & exy(16,1,1)=0 & exy(16,2,1)=0  & exy(16,3,1)=1
;**
		expp( 0)= '   X Derivative                           '
		expp( 1)= '   Y Derivative '
		expp( 2)= '     Gradient   '
    		expp( 3)= '     Standard  deviation  over the frames '
		expp( 4)= '     Transpose second and third  dimension'
		expp( 5)= '     Transpose first  and third  dimension'
    		expp( 6)= '     Transpose first  and second dimension'
		expp( 7)= '     Equalize  using  cumulat.distribution'
    		expp( 8)= '.'
    		expp( 9)= '     Natural Logarithm'
    		expp(10)= '     Natural Exponential'
    		expp(11)= '     Square--root'
    		expp(12)= '     Square'
    		expp(13)= '     f(I)=1/I      [=1   if  I=0 ]'
    		expp(14)= '     f(I)=I/n      [ 1 < n < 300 ]'
    		expp(15)= '     f(I)=I*n      [ 1 < n < 300 ]'
    		expp(16)= '     f(I)=I-n      [min< n < max ]'
    		expp(17)= '     f(I)=I>1      [=1   if  I<1 ]'
		expp(18)= '     f(I)=  abs(I) [  Magnitude  ]'
    		expp(19)= '     f(I)=I/max(I) [  Normalize  ]'
    		expp(20)= '     f(I)=Unsig(I) [ >=0 <=32767] '
    		expp(21)= '.'
    		expp(22)= '     Show distribution curve'
    		expp(23)= '.'
    		expp(24)= '     Data processing (miscellaneous) menu'
		expp(25)= '{B}  Back  to  previous data (if stacked)'
		expp(26)= '     None'
;**		Save before...   Cut before...   D_P entry....    Case entry...
		exy (0 ,0,2)=1 & exy(0 ,1,2)=0 & exy(0 ,2,2)=20 & exy(0 ,3,2)=0
		exy (1 ,0,2)=1 & exy(1 ,1,2)=0 & exy(1 ,2,2)=21 & exy(1 ,3,2)=0
		exy (2 ,0,2)=1 & exy(2 ,1,2)=0 & exy(2 ,2,2)=22 & exy(2 ,3,2)=0
		exy (3 ,0,2)=0 & exy(3 ,1,2)=0 & exy(3 ,2,2)=23 & exy(3 ,3,2)=12
		exy (4 ,0,2)=0 & exy(4 ,1,2)=0 & exy(4 ,2,2)=24 & exy(4 ,3,2)=8
		exy (5 ,0,2)=0 & exy(5 ,1,2)=0 & exy(5 ,2,2)=25 & exy(5 ,3,2)=8
		exy (6 ,0,2)=0 & exy(6 ,1,2)=0 & exy(6 ,2,2)=28 & exy(6 ,3,2)=9
		exy (7 ,0,2)=1 & exy(7 ,1,2)=1 & exy(7 ,2,2)=2  & exy(7 ,3,2)=0
;
		exy (9 ,0,2)=1 & exy(9 ,1,2)=0 & exy(9 ,2,2)=30 & exy(9 ,3,2)=0
		exy (10,0,2)=1 & exy(10,1,2)=0 & exy(10,2,2)=31 & exy(10,3,2)=0
		exy (11,0,2)=1 & exy(11,1,2)=0 & exy(11,2,2)=32 & exy(11,3,2)=0
		exy (12,0,2)=1 & exy(12,1,2)=0 & exy(12,2,2)=33 & exy(12,3,2)=0
		exy (13,0,2)=1 & exy(13,1,2)=0 & exy(13,2,2)=34 & exy(13,3,2)=0
		exy (14,0,2)=1 & exy(14,1,2)=0 & exy(14,2,2)=35 & exy(14,3,2)=0
		exy (15,0,2)=1 & exy(15,1,2)=0 & exy(15,2,2)=36 & exy(15,3,2)=0
		exy (16,0,2)=1 & exy(16,1,2)=0 & exy(16,2,2)=50 & exy(16,3,2)=0
		exy (17,0,2)=1 & exy(17,1,2)=0 & exy(17,2,2)=51 & exy(17,3,2)=0
		exy (18,0,2)=1 & exy(18,1,2)=0 & exy(18,2,2)=37 & exy(18,3,2)=0
		exy (19,0,2)=1 & exy(19,1,2)=0 & exy(19,2,2)=49 & exy(19,3,2)=0
		exy (20,0,2)=1 & exy(20,1,2)=0 & exy(20,2,2)=14 & exy(20,3,2)=0
;
		exy (22,0,2)=0 & exy(22,1,2)=0 & exy(22,2,2)=19 & exy(22,3,2)=28
;
		exy (24,0,2)=0 & exy(24,1,2)=0 & exy(24,2,2)=0  & exy(24,3,2)=11
		exy (25,0,2)=0 & exy(25,1,2)=0 & exy(25,2,2)=0  & exy(25,3,2)=1
;**
		sl_inkeep1,dummy
return
end
;
;**
pro sl_inkeep1 ,dummy
;** *********
;** Init viewer tables.
;** ---- ------ ------
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
       		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
;**my_keep
;*********
;**
		ex_o	= 'Frame operations'
		expo(0) = '{Z}Sum over the frames                            '
		expo(1) = '   Sum each of the frames'
		expo(2) = '   Make all the frames a single frame'
		expo(3) = '{X}Project each of the frames along X'
		expo(4) = '{Y}Project each of the frames along Y'
		expo(5) = '.'
		expo(6) = '   Line up  all frames according to  X proj.values'
		expo(7) = '   Line up  all frames according to  Y proj.values'
		expo(8) = '{>}Shift    the frames manually'
		expo(9) = '{U}Inverse   a  frame  (up-down)'
		expo(10)= '.'
		expo(11)= '{+}Add      two frames'
		expo(12)= '{-}Subtract two frames'
		expo(13)= '   Correlations between all frames'
		expo(14)= '.'
		expo(15)= '   Joint  a new frame'
		expo(16)= '   Rotate  and  scale a frame'
		expo(17)= '.'
		expo(18 )= '  None '
;**		Save before...   Cut before...   D_P entry....    Case entry...
		exy (0 ,0,3)=0 & exy(0 ,1,3)=0 & exy(0 ,2,3)=26 & exy(0 ,3,3)=10
		exy (1 ,0,3)=0 & exy(1 ,1,3)=0 & exy(1 ,2,3)=27 & exy(1 ,3,3)=14
		exy (2 ,0,3)=0 & exy(2 ,1,3)=0 & exy(2 ,2,3)=47 & exy(2 ,3,3)=8
		exy (3 ,0,3)=0 & exy(3 ,1,3)=0 & exy(3 ,2,3)=43 & exy(3 ,3,3)=10
		exy (4 ,0,3)=0 & exy(4 ,1,3)=0 & exy(4 ,2,3)=44 & exy(4 ,3,3)=10
;
		exy (6 ,0,3)=1 & exy(6 ,1,3)=0 & exy(6 ,2,3)=0  & exy(6 ,3,3)=18
		exy (7 ,0,3)=1 & exy(7 ,1,3)=0 & exy(7 ,2,3)=0  & exy(7 ,3,3)=19
		exy (8 ,0,3)=0 & exy(8 ,1,3)=0 & exy(8 ,2,3)=0  & exy(8 ,3,3)=15
		exy (9 ,0,3)=0 & exy(9 ,1,3)=0 & exy(9 ,2,3)=10 & exy(9 ,3,3)=24
;
		exy (11,0,3)=0 & exy(11,1,3)=0 & exy(11,2,3)=46 & exy(11,3,3)=17
		exy (12,0,3)=0 & exy(12,1,3)=0 & exy(12,2,3)=45 & exy(12,3,3)=16
		exy (13,0,3)=0 & exy(13,1,3)=0 & exy(13,2,3)=3  & exy(13,3,3)=5
;
		exy (15,0,3)=0 & exy(15,1,3)=0 & exy(15,2,3)=13 & exy(15,3,3)=20
		exy (16,0,3)=1 & exy(16,1,3)=0 & exy(16,2,3)=0  & exy(16,3,3)=26
;**
		ex_y	= 'Scan: Image processing'
		expy(0) = '   Sobel     edge-enhancement'
		expy(1) = '   Roberts  |D(ij)-D(i+1 j+1)|+|D(i+1 j)-D(i j+1)|'
		expy(2) = '   Mean      smoothing  (boxcar average)'
		expy(3) = '   Median    smoothing  (boxcar median )'
		expy(4) = '   D-Mean    unsharp masking (data-mean)'
		expy(5) = '.'
		expy(6) = '   Selective data-filter Viewport       '
		expy(7) = '   Filtering in frequency domain'
		expy(8) = '.'
		expy(9) = '{B}Back  to  previous data     (stacked)'
		expy(10)= '   None    '
;**		Save before...   Cut before...   D_P entry....    Case entry...
		exy (0 ,0,0)=1 & exy(0 ,1,0)=0 & exy(0 ,2,0)=6  & exy(0 ,3,0)=0
		exy (1 ,0,0)=1 & exy(1 ,1,0)=0 & exy(1 ,2,0)=5  & exy(1 ,3,0)=0
		exy (2 ,0,0)=1 & exy(2 ,1,0)=0 & exy(2 ,2,0)=7  & exy(2 ,3,0)=3
		exy (3 ,0,0)=1 & exy(3 ,1,0)=0 & exy(3 ,2,0)=8  & exy(3 ,3,0)=3
		exy (4 ,0,0)=1 & exy(4 ,1,0)=0 & exy(4 ,2,0)=9  & exy(4 ,3,0)=3
		exy (6 ,0,0)=1 & exy(6 ,1,0)=0 & exy(6 ,2,0)=0  & exy(6 ,3,0)=22
		exy (7 ,0,0)=1 & exy(7 ,1,0)=1 & exy(7 ,2,0)=11 & exy(7 ,3,0)=25
		exy (9 ,0,0)=0 & exy(9 ,1,0)=0 & exy(9 ,2,0)=0  & exy(9 ,3,0)=1
;**
		ex_ex	= ' Scan:Wayside ...'
		expex(0)= ' ---> GET MENUS             '
		expex(1)= '.'
		expex(2)= ' ---> REMOVE CURRENT VIEW   '
		expex(3)= '.'
		expex(4)= ' ---> SELECT AN OTHER VIEW  '
		expex(5)= '.'
		expex(6)= ' ---> RETURN TO THE DESKTOP '
;**
		exspc(0) = ' Logarithmic Scaling   --> N'
		exspc(1) = ' Square   Shape        --> q'
		exspc(2) = ' Smoothed image        --> O'
		exspc(3) = ' Frm separately scaled --> ='
		exspc(4) = ' Arrow-Keys for Move   --> #'
		exspc(5) = ' Arrow-Keys for Resize --> #'
		exspc(6) = ' Rescaled image        --> R'
		exspc(7) = ' Panning  mode         -->^P'
		exspc(8) = ' '
		exspc(9) = 'CURRENT SETTINGS            '
		exspc(10)= '******* ********            '
		exspc(11)= '                            '
		exspc(12)= '                            '
		exspc(13)= '                            '
		exspc(14)= '                            '
		exspc(15)= '                            '
		exspc(16)= '                            '
		exspc(17)= '                            '
		exspc(18)= '                            '
		exspc(19)= '                            '
		exsi	 = 9
		exsj	 = 11
;**
		sl_inkeep2,dummy
return
end
;
pro sl_inkeep2, dummy
;** **********
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
       		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
;**my_keep
;*********
;**
		ex_g	= 'Scan: Graphical Representations'
		expg(0) = '{L}Levels   contour mapped to the data     '
		expg(1) = '{S}Surface  view in perspective'
		expg(2) = '{P}Project  image   of projections   on/off'
		expg(3) = '{ }  "      image   of sections (frames)'
		expg(4) = '{ }  "      volume  aspect      (frames)'
		expg(5) = '{ }  "      '
		expg(6) = '{V}Vectors  X vect. within  the possibility'
		expg(7) = '{I}Image    is the  default representation'
		expg(8) = '.'
		expg(9) = '{M}MENU Parameters: default options'
		expg(10)= '{A}Annotations'
		expg(11)= '.'
		expg(12)= '{T}Turn 180 deg        about X   (up-down) '
		expg(13)= '{G}Resize  :Default view is fully stretched'
		expg(14)= '   None    '
;**		Case entry...
		exz(0 ,3)=0
		exz(1 ,3)=1
		exz(2 ,3)=2
		exz(3 ,3)=5
		exz(4 ,3)=3
		exz(5 ,3)=4
		exz(6 ,3)=7
		exz(7 ,3)=6
		exz(8 ,3)=10
		exz(9 ,3)=9
		exz(10,3)=16
		exz(11,3)=10
		exz(12,3)=20
		exz(13,3)=14
		exz(14,3)=10
;**
		ex_gs	 = 'Scan: Graphical parameters'
		expgs(0) = 'Surface:filled with colors and lines  '
		expgs(1) = '       :filled with colors            '
		expgs(2) = '       :represented by  colors lines  '
		expgs(3) = '       :stacked from a  3 dim matrix  '
		expgs(4) = '       :4D surf from a (x,y,2)matrix  '
		expgs(5) = '       :as a solid                    '
		expgs(6) = '       :shaded                        '
		expgs(7) = 'Surface angles: rot         view      '
		expgs(8) = '        bk_grd {None }  {Box }  {fun }'
		expgs(9) = '.'
		expgs(10)= 'Levels :filled  with colors           '
		expgs(11)= '       :contour with colors lines     '
		expgs(12)= '       :as a surface (same angles)    '
		expgs(13)= '       :set number of intervals       '
		expgs(14)= '.'
		expgs(15)= 'Image  :smooth well-stretched image   '
		expgs(16)= '.'
		expgs(17)= 'Return'
;**		Case entry...
		exz(0 ,4)=1
		exz(1 ,4)=5
		exz(2 ,4)=3
		exz(3 ,4)=7
		exz(4 ,4)=15
		exz(5 ,4)=4
		exz(6 ,4)=12
		exz(7 ,4)=6
		exz(8 ,4)=14
		exz(9 ,4)=9
		exz(10,4)=10
		exz(11,4)=11
		exz(12,4)=16
		exz(13,4)=8
		exz(14,4)=9
		exz(15,4)=13
		exz(16,4)=9
		exz(17,4)=0
;**
		ex_r	= 'Use "z" for a quick zoom'
		expr(0) = '   Reduce horizontally the frame(s)'
		expr(1) = '   Reduce vertically   the frame(s)'
    		expr(2) = '   Reduce the number   of  frames'
		expr(3) = '   Expand  a  region             '
		expr(4) = '   None                          '
;**
		ex_m	= 'Map or not to map'
		expm(0) = '   Map the data to current view  '
		expm(1) = '   Remake  the  view             '
;**
		ex_n	= 'Bound type'
		expn(0) = '   Set outside values to lower , upper'
		expn(1) = '   Set outside values to zeros '
		expn(2) = '   Set inside  values to zeros '
		expn(3) = '   Set none'
;**
		ex_l	= 'Mouse Action (Left-Mid-Right)              '
		ex_l	= 'Mouse:Left      Middle       Right         '
		expl(0) = '   {F1 }         {F2 &}         {F3 }      '
		expl(1) = '  {Close}     {Cut image}    {Get Menus}   '
;**
		expb(0) = '               -->  Stop  <--              '
		expb(1) = '--> Removing                    Adding  <--'
;**
		ex_s1	= 'Rescale the View'
		ex_s2	= 'Rescale the Data'
		exps(0) = '   {F1 }           {F2 &}          {F3 }   '
		exps(1) = '{Set lower}      {Rescale}     { Set upper}'
		exps(2) = '                Cancel->{F4 space}'
		exps(3) = '"[" or left   button to set the lower value'
		exps(4)	= '"]" or right  button to set the upper value'
		exps(5)	= '"R" or middle button to rescale            '
;**
		ex_w	= 'Extracting data: values ,coordinates'
		expw(0) = '   {F1 }          {F2 &}           {F3 }   '
		expw(1) = '{Write extr} {Show selection}  {Make image}'
		expw(2) = '                Abort->{F4 space}'
		expw(3) = '"^" =This is center   | Record:           '
		expw(4) = '"%" =This is radius   | ";"*"=value ,region'
		expw(5) = '"-" =Back  1 record   | "m"+"=average , sum'
;**
		ex_x1	= 'Scan Reduction'
		ex_x2	= 'Verti. Frame Reduction'
		ex_x3	= 'Horiz. Frame Reduction'
		expx(0) = '   {F1 }          {F2 &}           {F3 }   '
		expx(1) = '{Set lower}      {Reduce}      { Set upper}'
		expx(2) = '                Cancel->{F4 space}'
		expx(3) = '{F1}or left   button to set the lower limit'
		expx(4)	= '{F3}or right  button to set the upper limit'
		expx(5)	= '{F2 &}or middle button to reduce           '
;**
		ex_i	= 'Slicing...'
		exci(0) = '   {F1 }          {F2 &}           {F3 }   '
		exci(1) = '{Set pivot pt} {Write vector} {Show vector}'
		exci(2) = ' '
		exci(3) = ' m =3 points  average slice * : =fix  slice'
		exci(4)	= ' % =move line keeping angle * / =stop slice'
		exci(5)	= ' . =set  pivot  point       * '
;**
		ex_fi	= 'Fitting...'
		exfi(0) = '   {F1 }          {F2 &}           {F3 }   '
		exfi(1) = '{Stop fit}   {Update image}  {Direct. X,Y }'
		exfi(2) = ' '
		exfi(3) = ' Fit functions: 0=Gauss      3=Poly-Surface'
		exfi(4)	= ' --- ---------  1=Poly       5='
		exfi(5)	= '"+"-"=change degree'
;**
		ex_x4	= 'Expand a region'
		expk(0) = '   {F1 }          {F2 &}           {F3 }   '
		expk(1) = '{Decrease reg}  {Expand}    { Increase reg}'
		expk(2) = '                Cancel->{F4 space}'
		expk(3) = ' Use   arrow-keys   for fine   adjustment  '
		expk(4)	= ' Use  {F1 } / {F3 } for coarse adjustment  '
		expk(5)	= ' Click middle button to expand             '
;**
		ex_sf	= 'Selective data-filter Viewport'
		exsf(0) = '   {F1 }          {F2 &}           {F3 }   '
		exsf(1) = '{Decrease size}   {Stop}    {Increase size}'
		exsf(2) = ' '
		exsf(3) = '"0"  =Clear    region  * "%" =Clear   point'
		exsf(4)	= '"1"  =Average  region  * "u" =Update  image'
		exsf(5)	= '"3,5"=Put,Subtract  t  * "t" =Take  average'
;**
		ex_ff	= 'Filtering in frequencies'
		exff(0) = '   {F1 }          {F2 &}           {F3 }   '
		exff(1) = '{Phase/Power}  {F inverse}   {Save in file}'
		exff(2) = '                Cancel->{F4 space}'
		exff(3) = '"0"  =Clear  region    * "%"  =Clear  point'
		exff(4)	= '"1,3"=Low,High pass    * "u"  =Update image'
		exff(5)	= '"5,7"=Clear Line,Colm  *Insert=Break       '
;**
		ex_rs	= 'Rotate and scale'
		exprs(0)= '   {F1 }          {F2 &}           {F3 }   '
		exprs(1)= '{From position}  {Rotate}     {To position}'
		exprs(2)= '                 Cancel->{F4 space}'
		exprs(3)= '"^" =This is the center of rotation'
		exprs(4)= '"0" =Set  angle to zero'
		exprs(5)= '"1" =Set  magification factor to one'
;**
		ex_o1	= 'Subtraction'
		ex_o2	= 'Addition'
		ex_o3	= 'Shift'
		expo1(0)= '   {F1 }           {F2 &}          {F3 }   '
		expo1(1)= '  {Frame}        {Subtract}    {From frame}'
		expo1(2)= '                 Cancel->{F4 space}'
		expo1(3)= ' Click left   button  for subtracted frame '
		expo1(4)= '       right  button  for giving     frame '
		expo1(5)= '       middle button  to  subtract         '
		expo2(0)= '   {F1 }           {F2 &}          {F3 }   '
		expo2(1)= '  {Frame}          {Add}        {To frame} '
		expo2(2)= '                 Cancel->{F4 space}'
		expo2(3)= ' Click left   button   for added    frame  '
		expo2(4)= '       right  button   for giving   frame  '
		expo2(5)= '       middle button   to  add             '
		expo3(0)= '   {F1 }           {F2 &}          {F3 }   '
		expo3(1)= '{From position}   {Shift}     {To position}'
		expo3(2)= '                 Cancel->{F4 space}'
		expo3(3)= ' Click left   button  to  set old position '
		expo3(4)= '       right  button  to  set new position '
		expo3(5)= '       middle button  to  shift            '
;**
		ex_e	= 'Rotation Control'
		expe(0) = '   {F1 }           {F2 &}          {F3 }   '
		expe(1) = ' {Set angles}    {Stop rot}    {Z or X rot}'
		expe(2) = ' '
		expe(3) = ' Use  {F1 } to set    Z and X axis angles  '
		expe(4) = '      {F2 &}to stop   rotation'
		expe(5) = '      {F3 } to switch Z for X axis rotation'
;**
		ex_fl	= 'Flick Control'
		expfl(0)= '   {F1 }           {F2 &}          {F3 }   '
		expfl(1)= '  {Resize image}  {Stop }  {Pause/continue}'
		expfl(2)= ' '
		expfl(3)= ' Use  {F1 } to resize image (pick up speed)'
		expfl(4)= '      {F2 &}to stop   flicking             '
		expfl(5)= '      {F3 } to pause and continue          '
;**
		ex_ad	= 'Extracting reflex parameters'
		exadj(0)= '   {F1 }           {F2 &}          {F3 }   '
		exadj(1)= '{Write ref} {Show selection}{Show a reflex}'
		exadj(2)= '                Abort->{F4 space}'
		exadj(3)= '"^"=Set radial center|";"=Record reflexion '
		exadj(4)= '"%"=Clear radial mode|"-"=Back  one record '
		exadj(5)= '"\"=Adjust ellipse   |"+"=Get a reflex file'
;**
		ex_ra	= 'Radial integrations'
		exrad(0)= '   {F1 }           {F2 &}          {F3 }   '
		exrad(1)= '{Stop process} {Write vector} {Show vector}'
		exrad(2)= ' '
		exrad(3)= '" ^ "=This is center   | Integrate:        '
		exrad(4)= '"{ }"=Low,Upper radius | ";"=current radius'
		exrad(5)= '"< >"=Low,Upper angle  | "*"=L to U  radius'
;**
		exphc(0)= ' PostScript from Image    **'
		exphc(1)= ' Encapsulated  PostScript   '
		exphc(2)= ' PostScript from the data   '
		exphc(3)= ' Byte  binary    Image      '
		exphc(4)= ' CGM Metafile               '
		exphc(5)= '.'
		exphc(6)= ' Black & White toggle       '
		exphc(7)= '.'
		exphc(8)= ' Return'
		exz(0,5)= 0
		exz(1,5)= 1
		exz(2,5)= 2
		exz(3,5)= 3
		exz(4,5)= 4
		exz(5,5)= 99
		exz(6,5)= 51
		exz(7,5)= 99
		exz(8,5)= 100
;**
	sl_inkeep3,dummy
return
end
;
;
pro sl_inkeep3, dummy
;** **********
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
       		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;
;
		ex_sp	 = 'Scan: Handy keys'
;
 		exsph(0) = 'Cursor handies              '
 		exsph(1) = '****** *******          /\  '
 		exsph(2) = ' Move   the pointer   <-||->'
 		exsph(3) = ' Resize the region.     \/  '
    		exsph(4) = '_Toggle move/resize region #'
 		exsph(5) = ' Next     frame  ---> Next S'
 		exsph(6) = ' Previous frame  ---> Prev S'
    		exsph(7) = '_(un)Free the mouse --->  F7'
    		exsph(7) = ' '
    		exsph(8) = '_Toggle   Pan mode  --->  ^P'
 		exsph(9) = ' '
 		exsph(10)= 'Mouse buttons'
 		exsph(11)= '***** *******'
 		exsph(12)= ' Left  (ex: Close    ) or F1'
 		exsph(13)= ' Middle(ex: Cut image) or F2'
 		exsph(14)= ' Right (ex: Get menus) or F3'
 		exsph(15)= ' '
 		exsph(16)= 'Viewfinder (Glory Hole)     '
 		exsph(17)= '**********  ***** ****      '
    		exsph(18)= '_Show region,integration-> $'
 		exsph(19)= ' Resize  the region    --> f'
 		exsph(20)= ' Search maxi in region->HOME'
    		exsph(21)= '_Toggle  Box/ellipsoid --> e'
 		exsph(22)= ' Adjust      ellipse ----> \'
 		exsph(23)= ' Rotate      ellipse ---->()'
 		exsph(24)= ' Clear ellipse angle ----> 9'
 		exsph(25)= ' '
 		exsph(26)= 'Menus'
 		exsph(27)= '*****'
 		exsph(28)= ' View    Representions ->F11'
 		exsph(29)= '         "" parameters ->  M'
 		exsph(30)= '         Rescale     --->  R'
 		exsph(31)= '         Color  tables ->  C'
 		exsph(32)= ' Profile Options    ->Select'
 		exsph(33)= '         Slicing    ---->  /'
 		exsph(34)= ' Data    Convolutions  ->F12'
 		exsph(35)= '         Miscellaneous -> ^E'
 		exsph(36)= '         Simple funct. -> ^F'
 		exsph(37)= ' Frames  Operations ---->  J'
 		exsph(38)= '         Additions   --->  +'
 		exsph(39)= '         Subtractions -->  -'
 		exsph(40)= '         Shift         ->  >'
 		exsph(41)= ' External functions ->Insert'
 		exsph(42)= ' Fit      functions ->     F'
 		exsph(43)= ' Extract regions of data-> W'
 		exsph(44)= ' Annotations            -> A'
 		exsph(45)= ' '
 		exsph(46)= 'Hard-copy'
 		exsph(47)= '*********'
 		exsph(48)= ' Options             ->    h'
 		exsph(49)= ' Image   hard-copy   ->Print'
 		exsph(50)= ' Glory_H hard-copy   ->^G ,g'
 		exsph(51)= ' '
 		exsph(52)= 'Window'
 		exsph(53)= '******'
;    		exsph(54)= '_Window borders       ->   w'
 		exsph(54)= ' Resize G_H or Image  ->   G'
 		exsph(55)= ' Duplicate window     ->   D'
 		exsph(56)= ' Save data and window ->   H'
 		exsph(57)= ' Quit and tidy window ->   Q'
 		exsph(58)= ' Remove   this view ->Remove'
 		exsph(59)= ' '
 		exsph(60)= ' '
 		exsph(61)= 'Profiles (Glory Hole)'
 		exsph(62)= '********  ***** **** '
 		exsph(63)= ' Image           ------>   i'
 		exsph(64)= ' Contour  levels  ----->   l'
 		exsph(65)= ' Surface           ---->   s'
 		exsph(66)= ' x vector profile    -->xv x'
 		exsph(67)= ' y vector profile     ->yv y'
    		exsph(68)= '_Toggle Auto-Rescale  ->   r'
 		exsph(69)= ' Enhance  profile     -> 0 !'
 		exsph(70)= ' Show  deviations     ->   d'
 		exsph(71)= ' Show  current colors ->   c'
    		exsph(72)= '_Show  Horiz. section ->   _'
    		exsph(73)= '_Show  Verti. section ->   |'
    		exsph(74)= '_Show  projections    ->   p'
    		exsph(75)= '_Toggle Slicing       ->   /'
 		exsph(76)= ' Set   pivot for slice->   .'
 		exsph(77)= ' suppress profiles    ->   a'
 		exsph(78)= ' '
 		exsph(79)= 'Representations'
 		exsph(80)= '***************'
 		exsph(81)= ' Image           ------>   I'
 		exsph(82)= ' Contour  levels  ----->   L'
 		exsph(83)= ' Surface           ---->   S'
 		exsph(84)= ' Projections        --->   P'
 		exsph(85)= ' x vector profile    -->   V'
 		exsph(86)= ' Annotations          ->   A'
 		exsph(87)= ' '
 		exsph(88)= 'Display         _ for Toggle'
 		exsph(89)= '*******         * *** ******'
		exsph(90)= '_Smoothed            ----> O'
		exsph(91)= '_Logarithmic           --> N'
		exsph(92)= '_Square off the frames  -> q'
		exsph(93)= '_Scale frames separately-> ='
 		exsph(94)= ' Up is down           ---> T'
 		exsph(95)= ' Set current value low  -> ['
 		exsph(96)= ' Set current value high -> ]'
 		exsph(97)= '     and Rescale        -> R'
 		exsph(98)= ' Flick over the frames --> @'
 		exsph(99)= ' Rotate a surface      --> @'
    		exsph(100)='_Rotate other direction--> ~'
 		exsph(101)=' E <--- Quick Zoom ------> z'
 		exsph(102)=' '
 		exsph(103)='Data  (modify)'
 		exsph(104)='****   ****** '
 		exsph(105)=' Simple Functions.   ---> ^F'
 		exsph(106)=' Convolutions,Filters -->F12'
 		exsph(107)=' Shifts and Operations ->  J'
 		exsph(108)=' Rescale,Miscellaneous -> ^E'
    		exsph(109)='_Stack data on modify   -> b'
 		exsph(110)=' Back to previous one   -> B'
 		exsph(111)=' Reduce  as the view    -> K'
 		exsph(112)=' Inverse (up is down)   -> U'
 		exsph(113)=' Show the sum on x    ---> X'
 		exsph(114)=' Show the sum on y    ---> Y'
 		exsph(115)=' Show the sum on z    ---> Z'
 		exsph(116)=' '
 		exsph(117)='Coordinates'
 		exsph(118)='***********'
 		exsph(119)=' Get a file of coord->F9 , "'
 		exsph(120)=' Choose a coordinate->F8 , 5'
 		exsph(121)=' Position next coord->F10, 1'
 		exsph(122)=' '
 		exsph(123)='I.D.L. Command level-> Break'
 		exsph(124)='*****  ******* *****   or ^B'
;**
		ex_p1	= 'Feed the mouse.Limit depends up the pointer'
		ex_p2	= 'Squeak'
		ex_p6	= '* Pt *   Signal  *  Center at      ,      *'
		ex_p4	= '* Pt *   Center at      ,     Ray Lg:      '
		ex_p3	= '     :          at      ,     frame        '
		ex_p5	= 'Stored Value:                              '
		ex_p7	= '* Pt * Center:    ,     Radius:     to     '
		ex_p8	= '     :          Rd:     Angle :     to     '
		explv	= 'Value:          at      ,      frame       '
		explz	= 'Max:            at      ,      frame       '
		explc	= 'Pivot point:     ,     Avg    Move line    '
		expld	= 'Dev:            Avg:          frame        '
		explb	= 'Bounds:      >     ,      >     ,     >    '
		explk	= 'Size of region  X:       Y:       Z:       '
		explr	= 'Limit setting low:          high:          '
		explo1	= 'Apply for frame       and      giving      '
		explo2	= 'C at      ,     Mag:          Ang:         '
		explo3	= 'For   frm     from     ,      to     ,     '
		expfi	= 'Fit along:X   Poly-degree:                 '
		explp	= '% Data  processing -> working ......       '
		expli	= '% Image processing -> working ......       '
;**
		ex_t1	= '*****************TAMPON********************'
		ex_t3	= ' Move the cursor inside active view'
;		ex_t3	= ' Hit <F7>  key to unfreeze the mouse :     '
;**
		i3	= '(i3)'
		i4	= '(i4)'
		i5	= '(i5)'
		i6	= '(i6)'
		f6	= '(f6.2)'
		s_o	= '  ** '
		wayt	=  0.5
		rvm	=  0
		rvl	=  0
		vmt	=  0
		vlt	=  0
;**
	invier,dummy
return
end
;
;
pro invier, dum
;** ******
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
common my_viewer, v_xsiz,v_loop,v_wndn,v_wntv,v_rec,v_wcw,v_x1,v_x5
;**
common my_vecfun, vf_w,vf_cw,vf_wy,vf_bx,vf_py,vf_l1,vf_l2,vf_ch,vf_st,vf_ft,$
		  vf_x41,vf_x46,vf_y41,vf_y46,vf_y51,vf_y52,vf_xb4,vf_yb4,$
		  vf_g,vf_tt4,vf_mm4,vf_tt5,vf_mm5,vf_tmp,iare,jare,xare,yare,$
		  iare_z,jare_z,xare_z,yare_z,$
		  vf_mxy,vf_mny,vf_mxx,vf_mnx,vf_a,vf_b,vf_d,vf_e,vf_p,vf_h,vf_z
;**
common my_tif,	tf_a,tf_b,tf_c,tf_cnt,tf_flg,tf_inv,tf_nt,tf_tag,tf_typ,tf_rect,$
		tf_val,tf_x,tf_nbuf,tf_off0,tf_off1,tf_offr,tf_rec,tf_uni,tf_sz
;**
common my_ccp4, ccsiz,ccrect,ccskp,ccnc
;**
common my_radia,rad_vtm,rad_bb,rad_6,rad_57
;**
common  my_annot, an_gm,an_gh,an_gf,an_ttl1,an_ttl2,an_xlab,an_ylab,an_ttm, $
		  an_zlab,an_com1,an_com2,an_unit,an_offs,an_i,an_r,an_f6
;**
;**my_viewr
;**********
		bxy	= sl_larr(1,14)
		clfc	= sl_larr(1,3)
		csiz	= sl_larr(1,17)
		fcg	= sl_larr(1,3)
		dxy	= sl_larr(1,2)
		fct	= sl_larr(1,3)
		mfi	= sl_larr(1,2)
		res	= sl_larr(1,3)
		ired	= sl_larr(1,3)   &   ired(0)= -1
		vsiz	= sl_larr(1,17)
		w_num	= sl_larr(1,3)
;**
		vik	= 0 & vin = -1
		inc	= 0
		vtm	= 0.
		rti	= 0
		ros	= 0
		entitl	= '                                           '
		fma	= '      '
		fmt	= '      '
		fmx	= '      '
		f_o	= '    '
		l_o	= '    '
		c_o	= '    '
		ttl	= '*****************FREE'
;**
;**my_viewer
;***********
		v_xsiz	 = sl_larr(1,17)
		v_wntv	 = sl_larr(1,3 )
		v_x1	 = sl_sarr(2,20, 1)
		v_x5	 = sl_sarr(2,20, 1)
		v_x1(0)  = 'Reading the data ...'
		v_x5(0)  = ' Saving the data ...'
		v_wntv(2)=-1
;**
;**my_vecfun
;***********
		iare_z	 = sl_larr(1,17)
		jare_z	 = sl_larr(1,17)
		xare_z	 = sl_larr(1,17)
		yare_z	 = sl_larr(1,17)
		vf_x41	 = sl_iarr(1,4)
		vf_x46	 = sl_iarr(1,4)
		vf_y41	 = sl_iarr(1,4)
		vf_y46	 = sl_iarr(1,4)
		vf_y51	 = sl_iarr(1,4)
		vf_y52	 = sl_iarr(1,4)
		vf_xb4	 = sl_iarr(1,4)
		vf_yb4	 = sl_iarr(1,4)
		vf_g	 = sl_iarr(1,9)
		vf_g(0)  =-99
		vf_p	 = sl_iarr(1,2)
		vf_h	 = sl_iarr(1,2)
		vf_tt4	 = '                                   '
		vf_mm4	 = sl_sarr(2,43,6)
		vf_tt5	 = '                                   '
		vf_mm5	 = sl_sarr(2,43,2)
		vf_ch	 = 15
		vf_w	 =-1
		vf_z	 = 360
;**my_tif
;********
		tf_a	 = long(2)^8
		tf_b	 = long(2)^16
		tf_c	 = long(2)^24
		tf_sz	 = sl_larr(1,17)
		tf_rect	 = sl_iarr(1,2)
;**
;**my_ccp4
;*********
		ccsiz	 = sl_larr(1,17)
		ccrect	 = sl_iarr(1,2)
;**
;**my_radia
;**********
		rad_vtm	 = 0.
		rad_6	 = 6.2832
		rad_57	 = 57.2956
;**
;**my_annot
;**********
	an_gm	 = sl_sarr(2,93 ,20)
	an_gh	 = sl_sarr(2,14 ,20)
	an_gf	 = sl_sarr(2,142,12)
	an_unit  = sl_farr(1,3)
	an_offs  = sl_farr(1,3)
	an_gm(0) = '.                                                    ' + $
		   '                                        '
	an_gh(1) = '  Title_1   : '
	an_gm(2) = '.'
	an_gh(3) = '  Title_2   : '
	an_gm(4) = '.'
	an_gh(5) = '  label X   : '
	an_gm(6) = '.'
	an_gh(7) = '  label Y   : '
	an_gm(8) = '.'
	an_gh(9) = '  label Z   : '
	an_gh(10)= ' Comment_1  : '
	an_gh(11)= ' Comment_2  : '
	an_gm(12)= '.'
	an_gh(13)= 'Unit   x,y,z: '
	an_gh(14)= 'Offset x,y,z: '
	an_gm(15)= '.   -------'
	an_gm(16)= '   Return'
	an_gm(17)= '  Remove'
	an_gm(18)= ' Apply '
	an_gm(19)= '.'

	an_f6	 = '!6'
	an_gf(0) = '!!7     !7ABCDEFGHIJKLMNOPQRSTUVWXYZ [\]^_, '+ $
			    ' abcdefghijklmnopqrstuvwxyz   ."$%&'+ $
			    ' 0123456789 :;<=>?@ ()#+-*/'
	an_gf(1) = '!6Font !!6'
	an_gf(2) = '!!6     !6ABCDEFGHIJKLMNOPQRSTUVWXYZ [\]^_, '+ $
			    ' abcdefghijklmnopqrstuvwxyz    ."$%&'+ $
			    ' 0123456789 :;<=>?@ ()#+-*/'
	an_gf(3) = '!6Font !!9'
	an_gf(4) = '!!9     !9ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_,'+ $
			    ' abcdefghijklmnopqrstuvwxyz."$%&'+ $
			    ' 0123456789 :;<=>?@ ()#+-*/'
	an_gf(5) = ' '
	an_gf(6) = '!6!!S  save position   !!R restore position'
	an_gf(7) = ' '
	an_gf(8) = ' '
	an_gf(9) = '!6!L!!L!S!E!!Exponent!R!I!!Index!N!!N!S!E!!E!R!I!!I!N'+ $
		    ' !S!U!!U!S!E!!E!R!I!!I!R!D!!D!S!E!!E!R!I!!I!N'       + $
		    ' !S!A!!A!S!E!!E!R!I!!I!R!B!!B!S!E!!E!R!I!!I'
	an_gf(10)= ' '
	an_gf(11)= ' '

	an_ttm   = ' Annotations '
	an_ttl1  = ' '
	an_ttl2  = ' '
	an_xlab  = ' '
	an_ylab  = ' '
	an_zlab  = ' '
	an_com1  = ' '
	an_com2  = ' '
return
end
;
;
;
;
pro sl_super  ,dummy
;** ********
;** Init Scan Desktop.
;** ---- ---- -------
;**
	common c_scan,	c_ini,c_x0,c_t0,c_t2,c_t3,c_t4,c_t5,c_x1,c_x2,c_x3 ,$
			c_x4,c_x5,c_x6,c_matdm,c_matrl,c_matdy,c_siz,c_area,$
			c_w,c_win,c_k,c_dirc,c_frm,c_pos,c_bo,c_sr
;**
;**c_scan
;********
			c_x0	 = sl_sarr(2,70,10)
			c_x1	 = sl_sarr(2,20, 1)
			c_x2	 = sl_sarr(2,27, 9)
			c_x3	 = sl_sarr(2,27, 1)
			c_x4	 = sl_sarr(2,27, 1)
			c_x5	 = sl_sarr(2,20, 1)
			c_x6	 = sl_sarr(2,21, 3)
			c_siz	 = sl_larr(1,17)
			c_matdm  = sl_larr(2,2,2)
			c_matrl  = sl_larr(2,2,2)
			c_matdy  = sl_sarr(3,50,3,1)
			c_area	 = 1
		c_dirc	 = ' '
		c_t4	 = 'New Directory ex:/users/b/richard/myself/  '
		c_t5	 = '  Default Directory '
;
		c_t0	 = 'Scan: Desktop'
		c_x0(0)	 = '  Restore a display you have previously saved' + $
			   '                         '
		c_x0(1)	 = '  Specify a new data file to read'
		c_x0(2)	 = '.'
		c_x0(3)	 = '  Apply  an operation between displayed data'
		c_x0(4)	 = '  Remove  a currently loaded  view '
		c_x0(5)	 =    c_t5
		c_x0(6)	 = '.'
		c_x0(7)	 = '  Product overview'
		c_x0(8)	 = '  Close   display'
		c_x0(9)	 = '.                       ---------------------------'
			c_x1(0)  = ' Read Process ...   '
			c_x5(0)  = ' Saving the data ...'
			c_x6(0)  = ' Restore a display   '
			c_x6(1)  = ' Specify new  data   '
			c_x6(2)  = ' Exit                '
			c_t2	 = 'A {operator} B --> C'
			c_t3	 = 'A            B --> C'
			c_x2(0)  = '.Choose an operator        '
			c_x2(1)  = '.------ -- --------        '
			c_x2(2)  = 'Add          +    '
			c_x2(3)  = 'Subtract     -    '
			c_x2(4)  = 'Multiply     *    '
			c_x2(5)  = 'Join         |    '
			c_x2(6)  = 'Mat.mult     #    '
			c_x2(7)  = 'B.ground     -bg  '
			c_x2(8)  = 'None'
			c_x3(0)	 = 'Choose view A              '
			c_x4(0)	 = 'Choose view B              '
			c_matdm(0,0)  = [512 ,  512]
			c_matdy(0,0)  = '*'
			c_matrl(0,0)  = 0
			c_matrl(1,0)  = 8
			c_win	      = 0
			c_ini	      = 0
			c_w	      = 0
;**
;'create    y      t_y,y '
;'free   y         t_y,y '
;'transfert y-->a  t_y,y '
;'Cellput Size'
;'Return'
;'Scan: Glory_Hole'
;'SCAN"lab: Version III ...'
;'Scan:print options'
;'Front'
;'All'
;'.Created file: '
;'---->  Click here to continue  <---- '
;'Saved colors'
;'> on'
;'>off'
;'Your choice'
;'Remove '
;'.View definition: '
;'.Data   file:     '
;'Save'
;'Saved values'
;csi+'80$|'+csi+'24t'+csi+'24t'
;csi+'69$|'+csi+'11t'+osc+'24;[0.0,22.5]'+stt
;'I.D.L> '
;'Type .CONTINUE to return (or RETALL & SCAN to restart)'
;'Snooper> '
;'External functions'
;'Dev'
;'Avg '
;'scan_'
;'Copy Output'
;'Scan: Info'
;'Current set'
;'OFF'
;' ON'
;'Viewr Size'
;'Display '
;'SCAN'
;	bb=sl_str_to_long( 1,'From ',tv_win,win,70 ,64)
;	bb=sl_str_to_long( 1,'<-X->',tv_win,win,102,64)
;	bb=sl_str_to_long( 1,'<-Y->',tv_win,win,118,64)
;	bb=sl_str_to_long( 1,'<-Z->',tv_win,win,134,40)
;	bb=sl_str_to_long( 1,'Read by SCAN on',tv_win,win,144,40)
;**
	sl_super2,0
;**
return
end
;
;
pro sl_super2, dummy
;** *********
;**
	common suprv,	s_vw,s_ns,s_x1,s_x2,s_x3,s_x4,s_x5,s_x6,s_x7,s_x8,s_x9,$
			s_rn,s_sc,t_x1,t_x2,t_x3,t_x4,t_x5,t_x6,t_x7,t_x8,t_x9,$
			s_x0,t_x0,s_x10,t_x10,s_x11,t_x11,s_x12,s_fl,s_siz,s_sz0
;**
;**suprv
;*******
			s_x0	 = sl_sarr(2,33, 7)
			s_x1	 = sl_sarr(2,25, 6)
			s_x2	 = sl_sarr(2,24, 6)
			s_x3	 = sl_sarr(2,23, 6)
			s_x4	 = sl_sarr(2,25, 9)
			s_x5	 = sl_sarr(2,41,13)
			s_x6	 = sl_sarr(2,38, 1)
			s_x7	 = sl_sarr(2,18, 1)
			s_x8	 = sl_sarr(2,3 , 4)
			s_x9	 = sl_sarr(2,28, 4)
			s_x10	 = sl_sarr(2,29, 5)
			s_x11	 = sl_sarr(2,18, 6)
			s_x12	 = sl_sarr(2,10, 7)
			s_vw	 = sl_iarr(1,  sl_element(s_x4)-3)
			s_fl	 = sl_larr(1,3) &  s_fl(0)=-1
			s_siz	 = sl_larr(1,17)
			s_sz0	 = sl_larr(1,2)
			s_x0(0)  = '.                                '
			s_x0(1)  = '  Access  a data base or a matrix'
			s_x0(2)  = '.'
			s_x0(3)  = '  Restore a display'
			s_x0(4)  = '.'
			s_x0(5)  = '  Exit'
			s_x0(6)  = '.'
;**
			s_x1(0)  = ' Access a data base      '
			s_x1(1)  = ' Restore  a display      '
			s_x1(2)  = ' Mailing                 '
			s_x1(3)  = ' Switch  off             '
			s_x1(4)  = ' Overview                '
			s_x1(5)  = ' Exit                    '
;**
			s_x2(0)  = ' Specify a scan(file)   '
			s_x2(1)  = ' Get next  scan         '
			s_x2(2)  = ' Get next  Non-stop     '
			s_x2(3)  = ' Refresh current scan   '
			s_x2(4)  = '.      --------         '
			s_x2(5)  = ' Data  analysis         '
;**
			s_x3(0)  = ' Setup default  views  '
			s_x3(1)  = ' Auto. frame correction'
			s_x3(2)  = ' Purge     some views  '
			s_x3(3)  = ' Put  an access aside  '
			s_x3(4)  = ' Change  colors        '
			s_x3(5)  = ' other   options       '
;**
			s_x4(0)  = ' Show:all  the data   on '
			s_x4(1)  = '      maxi frame      off'
			s_x4(2)  = '  Sum over frames   z off'
			s_x4(3)  = '  Sum over anodes   y off'
			s_x4(4)  = '  Sum over cathodes x off'
			s_x4(5)  = ' Board projections    on '
			s_x4(6)  = ' Set all off        '
			s_x4(7)  = ' Re_paint           '
			s_x4(8)  = ' Return             '
;**
			s_x5(0)  = ' Normalize                               '
			s_x5(1)  = ' Set     current frame as the back_ground'
			s_x5(2)  = ' Specify a detector  correction file '
			s_x5(3)  = ' Bound   over  1/3 of maximum value'
			s_x5(4)  = ' Bound   from  input values'
			s_x5(5)  = ' Show    limit values'
			s_x5(6)  = ' Show    back  ground    '
			s_x5(7)  = ' Show    detector efficiency'
			s_x5(8)  = ' Turn    back  ground       on/off'
			s_x5(9)  = ' Turn    detec.correction   on/off'
			s_x5(10) = ' Turn    boundaries         on/off'
			s_x5(11) = ' Re_paint'
			s_x5(12) = ' Return  '
;**
			s_x6(0)  = '----> Waiting for next acquisition ...'
;**
			s_x7(0)  = '----> Working ...                     '
;**
			s_x8(0)  = '* 1'
			s_x8(1)  = '* 2'
			s_x8(2)  = '* 3'
			s_x8(3)  = '* 4'
;**
			s_x9(0)	 = ' Show next part     '
			s_x9(1)	 = ' Show all  the data '
			s_x9(2)	 = ' Set  starting frame'
			s_x9(3)	 = ' Return'
;**
			s_x10(0) = ' Window Borders              '
			s_x10(1) = ' Smooth expanded views       '
			s_x10(2) = ' Square off the frames       '
			s_x10(3) = ' Allow to specify many runs  '
			s_x10(4) = ' Return'
;**
			s_x11(0) = '  SPECIFY  A  FILE  '
			s_x11(1) = '  RESTORE  A  VIEW  '
			s_x11(2) = '.      -----'
			s_x11(3) = '  GO INTO THE VIEW  '
			s_x11(4) = '.      -----'
			s_x11(5) = '  EXIT'
;**
			s_x12(0) = 'Surf proj.'
			s_x12(1) = 'Project.*3'
			s_x12(2) = 'Deep proj.'
			s_x12(3) = 'Levels'
			s_x12(4) = 'Image '
			s_x12(5) = 'Surface'
			s_x12(6) = 'Vectors'
;**
			t_x0	 = 'You"re welcome to any view I can give you'
			t_x1	 = 'CHANGE CONTEXT'
			t_x2	 = 'GET DATA , VIEWS'
			t_x3	 = 'CUSTOMIZE'
			t_x4	 = 'Toggle'
			t_x5	 = 'Back_grd OFF * Cell_fit OFF * Bound OFF'
			t_x6	 = 'Type <CR> to stop'
			t_x7	 = 'Read '
			t_x8	 = 'Size '
			t_x9	 = 'Starting frame:    '
			t_x10	 = 'Options'
			t_x11	 = 'Get Data , View'
			s_vw(0)  = 1
			s_vw(5)  = 1
return
end
;
;
;Scan colors	-11200 noir
;---- ------	     0 bleu clair	140
;		    80 jaune		  1
;		   240 vert clair	  2
;		   320 tres vert	  1
;		   800 vert fonce	  6
;		  1840 sapin		 13
;		  2480 marron		  8
;		  3040 gris		  7
;		  3520 blanc bleu	  6
;		  4800 blanc		 16
;		  8800 b sature		 50
;dvlmt
;-----
;**---	fft	: exponantial filter
;**---	ellips	: position
;	radial	: integration in fortran for all loops.
;
;	geo.trf : congridi,bilinear, finir FIT
;*	surface : histogram mode, vecteurs transparence, solide densite.
;		: surface(1) val> 255
;		: shade comme filled (ni)
;		: vectors + solide horizon possible.
;	axis	: surf , ccp4
;**---	units	: x,y,z + comments (dans "h")  (care bwx,bwy)
;	fxterm	: rm=unalias
;
;	Mare	: read header for 2000
;	read	: sl_kb(wait) until <cr> (matx,super + print)
;
;**---	molecule:
;**---	s volume:
;	neuron	:
;
;
;
function sl_handerr, n ,str
;******* **********
;**
	common	my_err,	err_1 ,err_2 ,err_3 ,err_4 ,err_5 ,err_6 ,err_7 ,err_8
;**
	case n of
	1:	bb=sl_iotype(err_1+' '+str,0,0)
	2:      bb=sl_iotype(err_2+' '+str,0,0)
	3:      bb=sl_iotype(err_3+' '+str,0,0)
	4:      bb=sl_iotype(err_4        ,0,0)
	5:      bb=sl_iotype(err_5        ,0,0)
	else:	bb=0
	endcase
return, bb
end
;
;
function sl_tog		,i
;******* ******
;**
if i eq 0 then return,1 else return,0
end
;
;
function sl_min2,	x1,x2
;******* *******
;**
	if x1 le x2 then return,x1 $
		    else return,x2
end
;
;
function sl_max2,	x1,x2
;******* *******
;**
	if x1 ge x2 then return,x1 $
		    else return,x2
end
;
;
function sl_dd, j,erey,vsizy,area,vsiza
;******* *****  * **** ***** **** *****
;**
;** Data dynamic.
;** ---- -------
common tmp_dd,	bb,dim,typ
;**
;carez + erey
on_error,2
bb=1
	case j of
;** Transfert.
;** ---------
;    0:	begin
;**prov
;	i=sl_element(erey)
;	if i ne vsizy(6) then if (i gt 1) or (vsizy(6) gt 1) then $
;			 bb=sl_iotype('transfert y-->a  t_y,y ',16,2,i,vsizy(6))
;	i=sl_element(area)
;	if i ne vsiza(6) then if (i gt 1) or (vsiza(6) gt 1) then $
;			 bb=sl_iotype('transfert y-->a  t_a,a ',16,2,i,vsiza(6))
;
;	if vsiza(6) ne vsizy(6) then begin
;				if vsiza(6) gt 0 then area=0
;				area=erey
;				i   =sl_element(area)
;				if i eq vsizy(6) then vsiza(0)=vsizy(*) $
;				else begin
;				     bb  = 0
;				     area= sl_iarr(2,2,2)
;				     vsiza(0)=[2,2,2,4,4,0,4]
;				     endelse
;	endif else  begin	area=erey
;				vsiza(0)=vsizy(*) & endelse
;	end
;** Create erey.
;** ------ ----
    1:	if   vsizy(0) gt 0 then begin
	dim= vsizy(1:vsizy(0))
	typ= vsizy  (vsizy(0)+1)
;**prov
;	i=sl_element(erey)
;	if i ne vsizy(6) then   if typ ne 1 then $
;				if (i gt 1) or (vsizy(6) gt 1) then $
;			 bb=sl_iotype('create    y      t_y,y ',16,2,i,vsizy(6))
;
	if   vsizy(6) ne 0 then erey=0
	if   typ eq  1 then erey=sl_sarr(2,dim(0),dim(1)) else $
	if   typ eq  2 then erey=sl_barr(-vsizy(0),dim)   else $
	if   typ eq  4 then erey=sl_iarr(-vsizy(0),dim)   else $
	if   typ eq  8 then erey=sl_farr(-vsizy(0),dim)   else $
	if   typ eq 16 then erey=sl_larr(-vsizy(0),dim)   else $
	if   typ eq 32 then erey=sl_darr(-vsizy(0),dim)   else $
	if   typ eq 64 then erey=sl_carr(-vsizy(0),dim)
	vsizy(6)=1
	for  k=1,vsizy(0) do vsizy(6)=vsizy(6)*vsizy(k)
	i  = sl_element(erey)
	if i ne vsizy(6) then if typ ne 1 then begin
				     bb  = 0
				     erey= sl_iarr(2,2,2)
				     vsizy(0)=[2,2,2,4,4,0,4] & endif
	endif
;** Free erey.
;** ---- ----
    2:	begin
;**prov
;	i=sl_element(erey)
;	if i ne vsizy(6) then if (i gt 1) or (vsizy(6) gt 1) then $
;			 bb=sl_iotype('free   y         t_y,y ',16,2,i,vsizy(6))

	erey=0
	vsizy(*)=0
	end
;** Copy erey wavely.
;** ---- ---- ------
    3:	begin   area=erey
		end
;**
    else:
    endcase
return ,bb
end
;
;
function sl_pp, j,erey,vsizy,area,vsiza
;******* *****  * **** ***** **** *****
;**
bb=1
	case j of
;** Transfert.
;** ---------
    0:	begin
;**prov
;	i=sl_element(erey)
;	if i ne vsizy(6) then if (i gt 1) or (vsizy(6) gt 1) then $
;			 bb=sl_iotype('transfert y-->a  t_y,y ',16,2,i,vsizy(6))

	vsiza(0)=vsizy(0:5)
	bb=sl_dd(1,area,vsiza)
	if bb then begin
		if vsiza(0) eq 1 then	area(0)    =erey else $
		if vsiza(0) eq 2 then	area(0,0)  =erey else $
		if vsiza(0) eq 3 then	area(0,0,0)=erey else $
					area       =erey
	endif
	end
;**
    else:
    endcase
return, bb
end
;
;
;
;
function sl_psiz,	psiz,s0,s1,s2,s3,s4,s5
;******* *******
;**
	if s0 ge 0 then psiz(0) = s0
	if s1 ge 0 then psiz(1) = s1
	if s2 ge 0 then psiz(2) = s2
	if s3 ge 0 then psiz(3) = s3
	if s4 ge 0 then psiz(4) = s4
	if s5 ge 0 then psiz(5) = s5

	psiz(6)=1
	for i=1,psiz(0) do psiz(6)=psiz(6)*psiz(i)
return, 1
end
;
function sl_psiz0,	psiz,s0,s1,s2,s3,s4,s5
;******* *******
;**
	if s0 ge 0 then psiz(0) = s0
	if s1 ge 0 then psiz(1) = s1
	if s2 ge 0 then psiz(2) = s2
	if s3 ge 0 then psiz(3) = s3
	if s4 ge 0 then psiz(4) = s4
	if s5 ge 0 then psiz(5) = s5
return, 1
end
;
;
function sl_psizm ,area,psiz,s0,s1,s2,s3,s4,s5
;******* ********
;**
	bb=sl_psiz0(    psiz,s0,s1,s2,s3,s4,s5)
	bb=sl_dd(1,area,psiz)
return,	bb
end
;
;
;
;
pro sl_stron,	extab,j1,j2,plc,pll,char1,char2
;** ********
;**
	tmp	 = extab(j1)
	bb	 = sl_sti(tmp,char1,plc)
	extab(j1)= tmp
	tmp	 = extab(j2)
	bb	 = sl_sti(tmp,char2,pll)
	extab(j2)= tmp
return
end
;
;
;
;
function sl_typb, typ
;******* *******  ***
;**
			      nc = 1
	if typ	   eq 4  then nc = 2 else $
	if typ	   eq 8  then nc = 4 else $
	if typ	   eq 16 then nc = 4 else $
	if typ	   eq 32 then nc = 8 else $
	if typ	   eq 64 then nc = 8
return, nc
end
;
;
;
;
function sl_str_to_long, flg,str,areout,y,offset,maxl
;******* **************
;**
	common tmp_strlng, bb,nb,j,k,k2
;**
	common my_area ,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;** String to long
;** ------ -- ----
bb=0
	if flg eq  1 then begin
	   areout(offset:offset+maxl/4-1,y)=0
	   k2=sl_stdim(str,nb)
	   bb=nb
;
	   if nb gt 0 then begin
		k2=nb/4
		if k2*4 lt nb then nb=(k2+1)*4
		k2=sl_psizm(arei,arei_z,1,nb,2,-1,-1,-1)
		k2=sl_stbyt(str,arei)
		if nb gt maxl then nb=maxl
		j =offset
;
		for i=0,nb-1,4 do begin
			k= long(arei(i))
			k=k*256+arei(i+1)
			k=k*256+arei(i+2)
			k=k*256+arei(i+3)
			areout(j,y)=k
			j=j+1
		endfor
		k2=sl_dd(2,arei,arei_z)
	   endif
	endif
;**
;** Long to string
;** ---- -- ------
	if (flg eq -1) and (maxl gt 0) then begin
		k2=sl_psizm(arei,arei_z,1,maxl,2,-1,-1,-1)
		j =offset
;
		for i=0,maxl-1,4 do begin
			k =areout(j,y)
			k2=k /256
			arei(i+3)=k-k2*256
			k2=k2/256
			arei(i+2)=k/256 -k2*256
			k2=k2/256
			arei(i+1)=k/256/256 -k2*256
			arei(i)  =k2
			j=j+1
		endfor
		str=sl_strf(arei,arei_z)
		k2 =sl_stbr(str,0)
		k2 =sl_dd(2,arei,arei_z)
		k2=sl_stdim(str,nb)
		bb=nb
	endif
return, bb
end
;
;
function sl_put_strfile, n,win,dirc,dm1,dm2,dm3,recl,typ,form,posit,swap,start
;******* **************
;**
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common  my_insert,  i_txt,i_idx,i_fil,i_rout,i_ps,i_rs,$
			    i_trout,i_tfil,i_tlang,i_tdx,i_enter,i_rcall
;**
	common tmp_strlng, bb,nb,j,k,k2
;**
	tv_win(50,win)=n
	tv_win(51,win)=dm1
	tv_win(52,win)=dm2
	tv_win(53,win)=dm3
	tv_win(54,win)=recl
	tv_win(55,win)=typ
	tv_win(56,win)=form
	tv_win(57,win)=posit
	tv_win(58,win)=swap
	tv_win(59,win)=start
;	      VMS
	      j    =sl_stp(dirc,':',0)+1
	      k    =sl_stp(dirc,']',0)+1
	      if j gt 1 then if k lt j then k=j
	      if k le 1 then begin
;	         UNIX
		 j=0
		 i_rout=dirc
		 while j ge 0 do begin
			k=j+1
			bb=sl_sti(i_rout,' ',j)
			j =sl_stp(i_rout,sys_dep('DIVIDER'),0)
		 endwhile
;		 FILE ONLY
		 if k le 1 then k=0
		 endif
	bb=sl_stdim(dirc,j)
	i_rout=sl_stx(dirc,k,j)
	bb=sl_str_to_long( 1,i_rout,tv_win,win,60,32)
	if n eq 1 then begin
	bb=sl_tvget(34,i_fil)
	bb=sl_str_to_long( 1,'From ',tv_win,win,70 ,64)
	bb=sl_str_to_long( 1,i_rout ,tv_win,win,86 ,64)
	bb=sl_str_to_long( 1,'<-X->',tv_win,win,102,64)
	bb=sl_str_to_long( 1,'<-Y->',tv_win,win,118,64)
	bb=sl_str_to_long( 1,'<-Z->',tv_win,win,134,40)
	bb=sl_str_to_long( 1,'Read by SCAN on',tv_win,win,144,40)
	bb=sl_str_to_long( 1,i_fil  ,tv_win,win,154,40)
	endif
return, 1
end
;
;
function sl_get_strfile, n,win,dirc,dm1,dm2,dm3,recl,typ,form,posit,swap,start
;******* **************
;**
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common  my_insert,  i_txt,i_idx,i_fil,i_rout,i_ps,i_rs,$
			    i_trout,i_tfil,i_tlang,i_tdx,i_enter,i_rcall
;**
	common tmp_strlng, bb,nb,j,k,k2
;**
	n	=1
	dm1	=tv_win(51,win)
	dm2	=tv_win(52,win)
	dm3	=tv_win(53,win)
	recl	=tv_win(54,win)
	typ	=tv_win(55,win)
	form	=tv_win(56,win)
	posit	=tv_win(57,win)
	swap	=tv_win(58,win)
	start	=tv_win(59,win)
return, 1
end
;
;
;
function sl_cellget ,u,dim,rl_ty ,a,fl
;******* **********  * *** *****  * **
;**	 Read  next block.  fl  > 0 then get  memory
;**	 ----  ---- -----  |fl| > 1 then skip records
;**			   recl < 0 calculate max record size
;**			   recl = 0 read(data)
;carez
;touti	 dim(3) rl_ty(2) a:area b:area
;**
common  tmp_cellget,	bb,n,nc,nx,ny,nz,p,recl,varl,typ
;**
common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
		io_cur,io_ext,io_seq,io_str
;**
ab=0
if u ne 0 then  begin
	n    =  dim(0)
	typ  =  rl_ty(1)
	recl =  rl_ty(0)
	if n gt 0 then  nx = dim(1)  else nx = 0
	if n gt 1 then  ny = dim(2)  else ny = 1
	if n gt 2 then  nz = dim(3)  else nz = 1
;**
	nc   =  sl_typb(typ)
;**
	if recl gt io_rec then recl=io_rec
	if recl lt 0 then if  nx*ny*nz*nc le io_rec then recl=nx*ny*nz*nc else $
			  if  nx*ny*nc	  le io_rec then recl=nx*ny*nc    else $
							 recl=nx*nc
;**
	p= fl
	if p lt 0 then p=-p
;**	Skip records
;**	---- -------
	if p gt 1 then begin n =p
	   if recl eq 0 then begin
			     bb=sl_psizm(b,io_dim,1,n-1 , 2 ,-1,-1,-1)
			      n =1
		  endif else  bb=sl_psizm(b,io_dim,1,recl,typ,-1,-1,-1)
	   for x=long(1),n do if u   lt 0 then bb=sl_ioread(-u,b,io_dim,1) $
			else  if typ eq 1 then bb=sl_ioread( u,b,io_dim,1) $
					  else bb=sl_ioread( u,b,io_dim,0)
	   bb=sl_dd(2,b,io_dim)
	endif

	if recl eq 0 then begin varl=1 & recl=nx & endif else varl=0

;**	Get memory
;**	--- ------
	io_dima(0)  =dim(*)
	io_dima(io_dima(0)+1)=typ
	io_dima(6)  =0
	ab	    =1
	if fl gt 0 then ab= sl_dd(1,a,io_dima)
;**
	if ab then begin
	if    u   lt 0		  then ab=sl_ioread(-u,a,io_dima,1) else $
	if   typ  eq 1		  then ab=sl_ioread( u,a,io_dima,1) else $
	if ((nx*ny*nz*nc) le recl) or (varl eq 1) $
				  then ab=sl_ioread( u,a,io_dima,0) else $
	if   nx*ny*nc     le recl then begin
				bb=sl_psizm(b,io_dim,2,io_dima(1),io_dima(2),$
						       typ,-1,-1)
				b    =   a(*,*,0)
				for z=0 ,nz-1 do begin
					 bb=sl_ioread(u,b,io_dim,0)
					 if not bb  then z=nz    $
					 else a(0,0, z)= b
				endfor
		  		bb=sl_dd(2,b,io_dim) & endif $
	else begin if nx*nc  lt recl then  p=recl/(nx*nc) else p=1
			        if   p gt 0 then begin
				 if (n eq 1) or (p eq 1) then $
				      bb=sl_psizm(b,io_dim,1,recl/nc,typ,-1,-1,-1)$
				 else bb=sl_psizm(b,io_dim,2,nx,p,typ,-1,-1)

                                 if  n eq 1 then b = a(0:recl/nc-1)    else $
				 if  p eq 1 then begin
                                  if n eq 2 then b = a(0:recl/nc-1,0)  else $
                                  if n eq 3 then b = a(0:recl/nc-1,0,0)
				 endif else begin
                                  if n eq 2 then b = a(0:nx-1,0:p-1)   else $
                                  if n eq 3 then b = a(0:nx-1,0:p-1,0)
				 endelse
				 bb=1
			         for z = long(0) ,nz-1     do  $
				    for y = long(0),ny-1,p do  $
				      for x = long(0),nx-1,recl/nc do begin
					 bb=sl_ioread(u,b,io_dim,0)
					 if not bb then begin x=nx & y=ny & z=nz
					 endif else   $
				         if n eq 1 then  a(x) = b      else $
				         if n eq 2 then  a(x,y) = b    else $
				         if n eq 3 then  a(x,y,z) = b
 				 endfor
				 bb=sl_dd(2,b,io_dim)
				endif
	endelse & endif
endif
return, ab
end
;
;


function sl_stream  ,u,dim,rl_ty ,a,fl
;******* **********  * *** *****  * **
;**	 Read  stream matrix.  fl  > 0 then get  memory
;**	 ----  ------ ------  |fl| > 1 then skip bytes
;carez
;touti	 dim(3) rl_ty(2) a:area
;**
common  tmp_stream,	bb,k,kr,l,n,nx,ny,nz,p,px,recl,typ
;**
common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
		io_cur,io_ext,io_seq,io_str
;**
	common my_area ,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
bb=0
if u ne 0 then  begin
	n    =  dim(0)
	typ  =  rl_ty(1)
	recl =  rl_ty(0)
	if recl le 0 then  rstream=1 else rstream=0
	if n gt 0 then  nx = dim(1)  else nx = 1
	if n gt 1 then  ny = dim(2)  else ny = 1
	if n gt 2 then  nz = dim(3)  else nz = 1
;**
	recl=recl/sl_typb(typ)
;**
	if recl le 0	   then   recl= nx*ny*nz
	if recl gt io_rec  then   recl= nx*ny
	if recl gt io_rec  then   recl= nx
;**
;**	Get memory
;**	--- ------
	io_dima(0)  =dim(*)
	io_dima(n+1)=typ
	io_dima(6)  =0
	bb	    =1
	if fl gt 0 then bb= sl_dd(1,a,io_dima)

	if bb eq 1 then begin
	 p  = fl
	 if p lt 0 then p=-p
	 if rstream eq 0 then bb=  sl_psizm(areb,areb_z,1,recl,typ,-1,-1,-1) $
	    else if p gt 1 then bb=sl_psizm(areb,areb_z,1,p-1 ,2  ,-1,-1,-1)

	 k  = recl
	 l  = k-1
;**	Skip bytes
;**	---- -----
	 if p gt 1 then begin
	  if rstream eq 0 then begin
	   z  =(p-1)/sl_typb(typ)
	   p  =0
	     while (bb)  do  begin
		kl=recl-k
		px=z   -p
		if kl lt px  then begin
		   if k le l then p=p+kl
		   k =sl_ioread(u,areb ,areb_z,0)
		   k =0
		endif else  begin bb=0 & k=k+px & endelse
	     endwhile
	  endif else k =sl_ioread(u,areb ,areb_z,0)
	 endif
;**
;**	Read data
;**	---- ----
	 if rstream eq 0 then begin
	   for z=0,nz-1 do begin
	     p  =0
	     n  =0
	     while (n lt ny) and (bb) do  begin
		kl=recl-k
		px=nx  -p
		if kl lt px  then begin
		   if k le l then begin if nz eq 1 then a(p,n  )=areb(k:l) $
						   else a(p,n,z)=areb(k:l)
					p=p+kl & endif
		   bb=sl_ioread(u,areb ,areb_z,0)
		   k =0
		endif else begin	if nz eq 1 then a(p,n  )=areb(k:k+px-1)$
						   else a(p,n,z)=areb(k:k+px-1)
					k=k+px
					n=n+1
					p=0
		endelse
	     endwhile
	   endfor
	 endif else bb=sl_ioread(u,a ,io_dima,0)
	endif
	bb=sl_dd(2,areb,areb_z)
endif
return, bb
end
;
;
;
;function imagget ,u,dim,typ
;;******* *******  * *** ***
;;**	 Read  next block.
;;**	 ----  ---- -----
;common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
;		io_cur,io_ext,io_seq,io_str
;;**
;;**
;a=0
;if u gt 0 then begin
;;**
;	n=dim(0)
;	if n gt 2 then  nz = dim(3)  else nz = 1
;;**
;	io_dim(0)  =dim
;	io_dim(n+1)=typ
;	io_dim(6)  =0
;	bb	   =sl_dd(1,a,io_dim)
;;**
;	if nz  gt 1	then begin ass = assoc(u,a(*,*,0))
;				   for i= 0,nz-1 do    a(0,0,i) = ass(i)
;	endif		else begin ass = assoc(u,a) &  a=ass(0) & endelse
;endif
;return, a
;end
;
;
;
function sl_cellput ,erey, u, vsiz
;******* **********  ****  *  ****
;**	 Write a scan block.
;**	 ----- - ---- -----
common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
		io_cur,io_ext,io_seq,io_str
;**
common  tmp_cellput,	bb,nc,nx,ny,nz,typ,xsiz
;**
;carez + erey + b
;**
bb=0
if u ne 0 then begin
;prov
	xsiz=sl_size(erey)
	if u gt 0 then $
	if (xsiz(0) ne vsiz(0)) or (xsiz(1) ne vsiz(1)) or $
	   (xsiz(2) ne vsiz(2)) or $
	   (xsiz(xsiz(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype( $
			'Cellput Size',16,2,xsiz(0:3),vsiz(0:3))
;**
	typ  = xsiz(xsiz(0)+1)
;**
	if xsiz(0) eq 0  then nx = 1 else nx = xsiz(1)
	if xsiz(0) lt 2  then ny = 1 else ny = xsiz(2)
	if xsiz(0) lt 3  then nz = 1 else nz = xsiz(3)
;**
	nc   = sl_typb(typ)
;**
	if (u lt 0) or (nc eq 0)  then  begin
					if u lt 0 then z=-u else z=u
					bb=sl_iowrt(z,erey,xsiz,1)
				  endif else $
	if nx*ny*nz*nc  le io_rec then  bb=sl_iowrt(u,erey,xsiz,0) else $
	if nx*ny*nc     le io_rec then  begin
				  xsiz(0)=2 & xsiz(3)= typ & bb=1
				  for	z=0,nz-1  do   begin
					b= erey(*,*,z)
					bb=sl_iowrt(u,b,xsiz,0)
					if not bb  then z=nz
					endfor
	endif else begin
				  xsiz(0)=1 & xsiz(2)= typ & bb=1
				  for   z=0,nz-1  do   $
				   for  y=0,ny-1  do   begin
				        if nz gt 1 then b= erey(*,y,z) $
						   else b= erey(*,y  )
					bb=sl_iowrt(u,b,xsiz,0)
					if not bb  then begin y=ny & z=nz & endif
				   endfor
	endelse
if bb eq 0 then nc=sl_handerr(2,' ')
endif
return,bb
end
;
;
;
;
function  sl_filr, diry,ext,vers ,dirc ,frm
;*******  *******  *************  ****  ***
;** Open a file.
;** ---- - ----
;**
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
		io_cur,io_ext,io_seq,io_str
;**
common  tmp_sl_filr,	bb,i,n,pp,u
;**
	u	= 0
	pp	= 1
	n	= sl_iofind(diry,ext,vers,io_nam)
	i= n-1
	if n le 0 then bb=sl_handerr(1,diry+' '+ext) $
	else if  (n gt 1) or (dirc eq '?') then begin
		  if n gt 1 then io_nam   = ([io_nam(0),io_nam]) $
			    else io_nam   = ([io_nam,io_nam   ])
	    	  io_nam(0)=  'None'
		  i	   =  sl_tvmenul(0,0,io_nam,' ',tv_xp,tv_yp/2)
		  if i le 0 then i=-1
	     endif
	if (i ge 0) and (i le n) then begin
		  if  (sl_stp(io_nam(i),sl_stbr(io_ext(10),0),0) ge 0) then pp=0
		  bb = sl_iolun(u)
		  if u  gt 0 then bb=sl_iopenr(u , io_nam(i),pp,frm)
		  if bb eq 0 then begin bb=sl_iofree(u) & u=0
					bb=sl_handerr(3,io_nam(i))
					dirc=''
		  endif else dirc=io_nam(i)
        endif
return, u
end
;
;
;
function  sl_filw, dim ,extc,ext ,name ,typ ,xsiz ,ppf ,nbf
;*******  *******  ***  ********  ****  ***  ****  ***  ***
;** Open  write a scan file.
;** ----  ----- - ---- ----
common  tmp_sl_filw,	bb,pp,u,nc,nx,ny,nz
;**
common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
		io_cur,io_ext,io_seq,io_str
;**
	if name eq '?'   then name = io_cur else name =''

	if dim(0)   ge 0 then begin
			      name = name+     sl_stbr(sl_str(dim(0),'(i6)'),1)
	 if dim(1)  ge 0 then begin
			      name = name+'_' +sl_stbr(sl_str(dim(1),'(i6)'),1)
	  if dim(2) ge 0 then name = name+'_' +sl_stbr(sl_str(dim(2),'(i6)'),1)
	endif &  endif
	if extc ne ' '	 then name = name+'_' +sl_stbr(extc,1)
;**
	pp = ppf
	if (typ gt 0) then begin
		if xsiz(0) lt 1 then nx=1 else nx=xsiz(1)
		if xsiz(0) lt 2 then ny=1 else ny=xsiz(2)
		if xsiz(0) lt 3 then nz=1 else nz=xsiz(3)
		nc=sl_typb(typ)
		bb=nx*ny*nz*nc
		if bb gt io_rec then bb=nx*ny*nc
		if bb gt io_rec then bb=nx*nc
		if bb gt io_rec then nc=0  else nc=bb
	endif   else  nc=0
;
	if nbf gt 0 then nbf =  sl_iofind(name,ext,1,io_nam)
;
	if nbf le 0 then begin
	   bb =sl_iolun(u)
	   if u  gt 0 then bb=sl_iopenw(u , name,ext,nc,pp)
	   if bb eq 0 then begin bb=sl_iofree(u) & u=0
				 bb=sl_handerr(2,name+' '+ext)
	   endif
	endif else u=0
return, u
end
;
;
;
;
pro sl_colexp	,x,y
;** *********
;**
;**	Modify the color table.
;**	------ --- ----- -----
;
	common	my_cl,	cl_i,cl_cold,cl_ctb,cl_ttl,cl_hlp,cl_colm,cl_v2,cl_v3
;**
	common my_vcol,	r,g,b,	cr,cg,cb
;**
	common tmp_col, di,dj,k
bb=1
	if x lt 0 then begin
		cr(0)= r(*)
		cg(0)= g(*)
		cb(0)= b(*)
		bb =sl_tvldcol(cr,cg,cb)
	endif else if (x ge 0) and (y lt cl_i) then begin
	   if (y-x ge cl_i/10) or $
	      (x-y ge cl_i/10) then begin
		di=float(y-x+1)/cl_i
		dj=0.
		for i=0,cl_i-1 do begin
		    k =x+dj
		    cr(i)=r(k)
		    cg(i)=g(k)
		    cb(i)=b(k)
		    dj= dj+di  & endfor
		bb =sl_tvldcol(cr,cg,cb)
	   endif
	endif
return
end
;
;
;
;
pro sl_level, n
;** ********
;**
;**	 Edit color table to see levels.
;**	 ---- ----- ----- -- --- ------
;
	common	my_cl,	cl_i,cl_cold,cl_ctb,cl_ttl,cl_hlp,cl_colm,cl_v2,cl_v3
;**
	common my_vcol,	r,g,b,	cr,cg,cb
;**
if n gt 1 then begin
	i=cl_i / n
	if i gt 0 then $
	for j=cl_i-1 ,0, -i do begin
	    l= j-i+1
	    if l lt 0 then l=0
	    for  k=j ,l, -1 do begin
		 cr(k)=r(j)
		 cg(k)=g(j)
		 cb(k)=b(j)
	endfor & endfor
endif else if n le 0 then begin
	i=-n*2
	j= i+cl_i/10
	if i ge cl_i then i=cl_i -1
	if j ge cl_i then j=cl_i -1
	k=cl_i/2
	cr(*)= k & cg(*)= k & cb(*)= k
	for k=i,j do begin
		 cr(k)=r(k)
		 cg(k)=g(k)
		 cb(k)=b(k)
	endfor
endif
;
bb =sl_tvldcol(cr,cg,cb)
;
return
end
;
;
;
pro sl_kbcar ,st
;** ********
;**
	common	my_kb,	kb_tb,kb_cs,kb_es,kb_ls,kb_gh,kb_bx,kb_by,kb_kk,kb_car
	kb_car=st
;**
end
;
function sl_kb	,in_st
;******* *****
;**	 get a user signal.
;**	 *** * **** ******
	common	tmp_kb,	i,n,st
;**
	common	my_kb,	kb_tb,kb_cs,kb_es,kb_ls,kb_gh,kb_bx,kb_by,kb_kk,kb_car
;**
	i    =long(0)
	if kb_ls then st= kb_es else if kb_car ne '' then begin st=kb_car
								kb_car=''
				     endif else st= sl_tviokey(0,i)
	kb_ls=0
	in_st=st
;**
	if i eq 0 then $
	while st ne '' do    begin ;print,st,byte(st)
	      i  = i+1
	      if (st eq 'k') and (i eq 1) then kb_kk=2 else $
	      if  kb_kk gt 0 then begin  kb_kk=kb_kk-1 & st=''
	      endif else begin
	         st = sl_tviokey(0,n)
	         if st eq kb_es  then begin st='' & kb_ls=1
		 	  endif  else in_st=in_st+st
	      endelse
	endwhile
;**
	if in_st ne '' then  begin
				if i gt 5 then in_st=sl_stx(in_st,0,4) else $
				if i eq 1 then begin
				 case in_st of
				  '&':		in_st='[12~' ;F2
				  ' ':		in_st='[14~' ;F4
				  '5':		in_st='[19~' ;F8
				  '"':		in_st='[20~' ;F9
				  '1':		in_st='[21~' ;F10
				  '0':		in_st='[2~ ' ;Insert
				  '7':		in_st='[1~ ' ;Home
				  '9':		in_st='[5~ ' ;Prev P
				  '3':		in_st='[6~ ' ;Next P
				  'g':		in_st=string(7b)+'    ' ;Print G_H
				 else:		in_st=in_st     +'    '
				 endcase
				endif else if i eq 2 then in_st=in_st+'   ' $
				      else if i eq 3 then in_st=in_st+'  '  $
				      else if i eq 4 then in_st=in_st+' '
				n=sl_stp(kb_tb,in_st,0)
				if n ge 0 then n=kb_cs(n/5) & endif $
	else n=-1
return, n
end
;
;
;
pro sl_signal	,x,y ,fcx,fcy ,rti,reg,ot
;** *********
;**
common tmp_signal,bb,n,tx,ty
;**
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	  f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		  f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_viewr,  bxy
;**
	n=sl_kb(indm)
	while (n ge 9) and (n le 14) do begin
	  if reg eq 0  then   begin
;**
;**	   arrows for cursor
		  case  n of
		  9:	begin
			if fcy ge 1 then y=y+fcy else  if tv_od eq 0 then begin
						 bxy(5) =bxy(5)+1
					         if bxy(5) *fcy ge 1 then begin
					         y=y+1  &   bxy(5)=0   &  endif
						 endif else begin
						 bxy(5) =bxy(5)-1
						 if bxy(5)	lt 0 then begin
						 y=y+1 & bxy(5)=1/fcy-1 & endif
						 endelse
				if y ge  bxy(13) then begin
						 y=bxy(13)-1
						 bxy(5)  =0 & endif & end
		  10:	begin
			if fcx ge 1 then x=x-fcx else begin bxy(4)=bxy(4)-1
						 if bxy(4)	lt 0 then begin
						 x=x-1  &   bxy(4)=1/fcx -1
						 endif  &   endelse
				if x lt 0   then begin
						 x=0 & bxy(4)=0 & endif & end
		  11:	begin
			if fcy ge 1 then y=y-fcy else  if tv_od eq 0 then begin
						 bxy(5) =bxy(5)-1
						 if bxy(5)	lt 0 then begin
						 y=y-1 & bxy(5)=1/fcy-1 & endif
						 endif else begin
						 bxy(5) =bxy(5)+1
					         if bxy(5) *fcy ge 1 then begin
					         y=y-1 & bxy(5)=0       & endif
						 endelse
				if y lt 0   then begin
						 y=0 & bxy(5)=0 & endif & end
	  	  12:	begin
			if fcx ge 1 then x=x+fcx else begin bxy(4)=bxy(4)+1
						 if bxy(4) *fcx ge 1 then begin
						 x=x+1  &   bxy(4)=0
						 endif  &   endelse
				if x ge  bxy(12) then begin
						 x=bxy(12)-1
						 bxy(4)  =0 & endif & end
		  else:
		  endcase
		  if (n ge 9) and (n le 12) then bb=sl_tvmcur(2,x,y)
	  endif else if reg eq 1 then begin
;**
;**	   arrows for region
		  case  n of
		  9:	if f_fg(2) lt bxy(3) then f_fg(2)=f_fg(2)+1
		  10:	if f_fg(1) gt 2	     then f_fg(1)=f_fg(1)-1
		  11:	if f_fg(2) gt 2	     then f_fg(2)=f_fg(2)-1
		  12:	if f_fg(1) lt bxy(2) then f_fg(1)=f_fg(1)+1
		  else:
		  endcase
		  ot=2
	  endif
;**
;**	   ellips rotation
		  case  n of
		  13:	if f_fg(31) then begin tx= sl_asin(1./f_fg(1))
					       ty= sl_asin(1./f_fg(2))
				    if  tx  gt ty    then tx=ty
				    if indm eq '(((((' then ot=10 else $
				    if indm eq '(((('  then ot=8  else $
				    if indm eq '((('   then ot=6  else $
				    if indm eq '(('    then ot=4  else ot=1
				    f_el=f_el- tx*180./3.1416*ot     & ot=2
				    if f_el lt  -45. then begin
				       f_el=f_el+90.   & tx=f_fg(1)
				       f_fg(1)=f_fg(2) & f_fg(2)=tx
			endif   &   endif
		  14:	if f_fg(31) then begin tx= sl_asin(1./f_fg(1))
					       ty= sl_asin(1./f_fg(2))
				    if  tx  gt ty    then tx=ty
				    if indm eq ')))))' then ot=10 else $
				    if indm eq '))))'  then ot=8  else $
				    if indm eq ')))'   then ot=6  else $
				    if indm eq '))'    then ot=4  else ot=1
				    f_el=f_el+ tx*180./3.1416*ot     & ot=2
				    if f_el gt   45. then begin
				       f_el=f_el-90.   & tx=f_fg(1)
				       f_fg(1)=f_fg(2) & f_fg(2)=tx
			endif   &   endif
		  else:
		  endcase
	n=sl_kb(indm)
	endwhile
;
        rti=n
return
end
;
;
;
;
function sl_glory, flin
;******* ********
;**
common	my_glor,f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
	common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_vecfun, vf_w,vf_cw,vf_wy,vf_bx,vf_py,vf_l1,vf_l2,vf_ch,vf_st,vf_ft,$
		  vf_x41,vf_x46,vf_y41,vf_y46,vf_y51,vf_y52,vf_xb4,vf_yb4,$
		  vf_g,vf_tt4,vf_mm4,vf_tt5,vf_mm5,vf_tmp,iare,jare,xare,yare,$
		  iare_z,jare_z,xare_z,yare_z,$
		  vf_mxy,vf_mny,vf_mxx,vf_mnx,vf_a,vf_b,vf_d,vf_e,vf_p,vf_h,vf_z
;**
   bb=0
   if flin eq -1 then begin
;**Init device.
;**---- ------
	sl_grafin,  0
	tv_win(*,*)=0
;**pb save,/var.
;**-- ---- ----
	f_pl     =sl_iarr(2,tv_flg(2),1)
	f_pl(0,0)=sl_index (tv_flg(2),4)
;**
;**			Size Glory_Hole
;**			---- ----------
			f_wx  = long(tv_x/2.69)
			f_wy  =(long(tv_y/21.2)+1)/2	& f_wy=f_wy*2
			if f_wx gt 380 then f_wx=383
			if f_wx lt 300 then f_wx=300
			if f_wy gt  40 then f_wy= 40
			if f_wy lt  33 then f_wy= 33
			f_wp  =(f_wx*2/3 +1)/2		& f_wp=f_wp*2
			f_wx  = f_wp*3/2
;
			tv_w  = 4*f_wy
			if tv_flg(6) eq 2  then f_fg(3)=0
			if tv_flg(6) ne 0  then tv_w=f_wy
			f_py  =tv_y - tv_w/2
			tv_xp = 0.
			tv_yp =float(tv_y-tv_w)/tv_dy

			f_bx=f_wx+f_wx*2/3
			if f_fg(3) eq 1 then f_wy=f_wy*4
			if f_fg(3) eq 2 then f_wy=f_wy*8

		   	bb=sl_tvcur_w(-1,-1,-1,1,f_bx,f_wy)
;**
;**			Size Vecfun
;**			---- ------

			if tv_flg(6) ne 0 then vf_z =320 else vf_z=360
			vf_wy=230  &  vf_bx=vf_z
			if tv_flg(6) ne 0 then vf_bx=vf_z*4/5
			if tv_flg(6) ne 0 then vf_wy=160

			bb=sl_tvcur_w(-1,-1,-1,2,vf_bx,vf_wy)

;**Set  color , bcolor , hard font
;**---  -----   ------   ---------
	bb	 = sl_tvset(1,tv_nc-1)
	bb	 = sl_tvset(2,tv_nc/2)
	bb	 = sl_tvset(6,0)
;**	debug
;**	-----
	bb	 = sl_tvset(22,1)
	bb	 = sl_prompt('Idl> ')
	bb	 = 1
;**     First window and Keep in pixmap
;**	----- ------ --- ---- -- ------
	bb  = sl_tvgetwn(f_w1)
 	bb  = sl_tvlux(f_w1,60,60,'SCAN',0,0,0,0,2,0,0,1,0,0 ,1)
	bb  = sl_tvmod(0,3)
	bb  = sl_tvshap(-1)
	bb  = sl_tvget(32,k)
	if k gt 0 then tv_flg(2)=k
	tv_flg(3)=f_w1
	sl_colexp,-1
	k   = tv_col
	sl_manycol,k
	f_w1=-1
 	i= (tv_flg(17) and 1) & if i eq 0 then begin f_fg(14)= 2
				  		     f_fg(16)=12 & endif
  endif
;**
   if flin eq -2 then begin
	if f_w1 ge 0 then begin
	   if tv_flg(1) ne 1 then bb = sl_tvdelwn (f_w1) $
			     else bb = sl_tvfreewn(f_w1)
	   f_w1=-1
   endif & endif
;**
   if flin eq 0 then begin
	if f_w1 ge 0 then if tv_flg(1) ne 0 then begin  bb = sl_tvfreewn(f_w1)
							f_w1=-1 &  endif
	if f_w1 lt 0 then bb = sl_tvgetwn(f_w1)
	if f_w1 ge 0 then begin
		if tv_flg(6) ne 0 then k=(320+10) else k=(380+10)
		if tv_flg(1) eq 1 then f_fg(13)=1
		f_ic=0
		f_bx=f_wx+f_wx*f_fg(13)*2/3
		bb=sl_tvmod(0,3)
		bb=sl_tvget(3,kw)
		bb=sl_tvcur_w(-1,f_w1,-1,1,f_wx+f_wx*2/3,f_wy)
 		bb=sl_tvlux(f_w1,f_bx,f_wy,'Scan: Glory_Hole',$
			    0,0,0,0,2,0,0,0,k,f_py, 6)
		if bb gt 0 then begin
 		   bb=sl_tvget(28,k)     & bb=sl_tvget(29,j)
		   if k gt 0 then f_bx=k & if j gt 0 then f_wy=j
		   bb=sl_tvs(1,4,'SCAN"lab: Version III ...',1.,0,-1)
		   if kw gt 0 then   bb=sl_tvsel(kw)
		   bb=1
		endif else f_w1=-1
	endif   else bb=0
   endif
return, bb
end
;
;
;
function sl_vecfun, pw,pfl,pmen,pttl,px,py	,erey,vsiz,c,l,f,bx,by
;******* *********
;**
common my_vecfun, vf_w,vf_cw,vf_wy,vf_bx,vf_py,vf_l1,vf_l2,vf_ch,vf_st,vf_ft,$
		  vf_x41,vf_x46,vf_y41,vf_y46,vf_y51,vf_y52,vf_xb4,vf_yb4,$
		  vf_g,vf_tt4,vf_mm4,vf_tt5,vf_mm5,vf_tmp,iare,jare,xare,yare,$
		  iare_z,jare_z,xare_z,yare_z,mxy,mny,mxx,mnx,a,b,d,e,p,h,vf_z
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
;** Delete
;** ------
   if (pw  eq -2) then  if vf_w ge 0 then begin
			if tv_flg(1) ne 1  then bb=sl_tvdelwn (vf_w) $
					   else bb=sl_tvfreewn(vf_w)
			vf_w=-1 & tv_flg(5)=-1 & endif
   if (pw  eq -2)  or (pw eq 0) then vf_g(0)=-99 $
   else begin
;**
;** Check existence & size
;** ----- --------- - ----
    bb =sl_tvget(3,vf_cw)
    vf_tmp=0
    if vf_w ge 0 then if f_fg(45) ne 0  then bb=sl_tvsel(vf_w) $
					else bb=sl_tvsels(vf_w)
    if bb   ne 1 then vf_w=-1
    if vf_w lt 0 then begin if tv_flg(6) ne 0 then vf_z =320 else vf_z=360
			    vf_wy=230  &  vf_bx=vf_z
			    if tv_flg(6) ne 0 then vf_bx=vf_z*4/5
			    if tv_flg(6) ne 0 then vf_wy=160
		      endif
    if f_fg(45) ne 0 then if vf_w ge 0 then if tv_flg(1) ne 1 then begin
	bb =sl_tvget(28,vf_l1) & bb=sl_tvget(29,vf_l2)
	if (vf_l1 gt 0)   then $
	if (vf_l1 ne vf_bx) or (vf_l2 ne vf_wy) then begin
		if vf_l1 lt vf_z*4/5 then vf_bx=vf_z*4/5 else vf_bx=vf_l1
		if vf_l2 lt 160      then vf_wy=160      else vf_wy=vf_l2
		bb=sl_tvclear(dummy)
		bb=sl_tvdelwn(vf_w) &  vf_w =-1
	endif
    endif
    bo=1
    if vf_w lt 0 then begin
	bo=0
	bb=sl_tvgetwn(vf_w)
	if vf_w ge 0 then begin
	   vf_tmp = 1
	   vf_py=tv_y - vf_wy/2
	   bb=sl_tvcur_w(-1,-1,vf_w,2,vf_bx,vf_wy)
 	   bo=sl_tvlux(vf_w,vf_bx,vf_wy,'Scan: Info',0,0,0,0,2,0,0,0,0,vf_py, 9)
	   if bo gt 0 then begin
	      bb=sl_tvget(28,a)      & bb=sl_tvget(29,b)
	      if a gt 0 then vf_bx=a & if b gt 0 then vf_wy=b
	   endif else vf_w=-1
	endif
        tv_flg(5)=vf_w
    endif

    if bo gt 0 then begin

    if vf_tmp eq 1 then begin
	   vf_st=vf_bx- vf_z*4/5
	   i =4
	   vf_x41(0)=vf_st    +   vf_ch  &  vf_x41(1)=vf_bx-i
	   vf_x41(2)=vf_x41(1)-   vf_ch  &  vf_x41(3)=vf_x41(0)-vf_ch
	   vf_y41(0)=vf_wy -i -   vf_ch  &  vf_y41(1)=vf_y41(0)
	   vf_y41(2)=vf_wy -i            &  vf_y41(3)=vf_y41(2)

	   vf_x46(0)=vf_x41(0)           &  vf_x46(1)=vf_x41(1)
	   vf_x46(2)=vf_x46(1)           &  vf_x46(3)=vf_x46(0)
	   vf_y46(0)=vf_y41(0)- 6*vf_ch  &  vf_y46(1)=vf_y46(0)
	   vf_y46(2)=vf_y41(1)	         &  vf_y46(3)=vf_y46(2)

	   vf_y51(*)=vf_y41(*)- 7*vf_ch -vf_ch/2

	   vf_y52(0)=vf_y51(0)- 2*vf_ch  &  vf_y52(1)=vf_y52(0)
	   vf_y52(2)=vf_y51(1)           &  vf_y52(3)=vf_y52(2)

	   vf_xb4(0)=vf_x41(3)           &  vf_xb4(1)=vf_x41(0)
	   vf_xb4(2)=vf_xb4(1)           &  vf_xb4(3)=vf_xb4(0)
	   vf_yb4(0)=vf_y46(0)+   vf_ch  &  vf_yb4(1)=vf_y46(0)
	   vf_yb4(2)=vf_y41(0)           &  vf_yb4(3)=vf_y41(3)

	   bb=sl_tvpol(4,vf_xb4,vf_yb4,tv_flg(2)-1-tv_flg(2)/4,0)

	   vf_yb4(0)=vf_y52(0)+   vf_ch  &  vf_yb4(1)=vf_y52(0)
	   vf_yb4(2)=vf_y51(0)           &  vf_yb4(3)=vf_y51(3)

	   bb=sl_tvpol(4,vf_xb4,vf_yb4,tv_flg(2)-1-tv_flg(2)/4,0)
    endif
;**
;**
    if (pw  eq -1) then bb=sl_tvpop(vf_w,pfl)
;**
;** Clear Text
;** ----- ----
    if (pw  eq -3) then begin
;		bb=sl_tvpol(4,vf_x41,vf_y41,tv_flg(2)-1-tv_flg(2)/8,0)
		bb=sl_tvpol(4,vf_x46,vf_y46,tv_flg(2)-1,0)
		bb=sl_tvpol(4,vf_x41,vf_y51,tv_flg(2)-1-tv_flg(2)/8,0)
		bb=sl_tvpol(4,vf_x46,vf_y52,tv_flg(2)-1,0)
		if pfl eq 1 then begin
		   bb=sl_tvget(6,vf_ft)
		   if sys_dep('MACHINE') eq 'win' then bb=sl_tvset(6,-1) else bb=sl_tvset(6,0)
		   vf_l1=vf_x41(0)+3    &    vf_l2=vf_y51(0)+3
		   bb=sl_tvmod(1,6)
		   bb=sl_tvs(vf_l1,vf_l2-2*vf_ch,pttl,2.,0, 40./(256./tv_flg(2)) )  ;(tv_flg(2)/2)
		   bb=sl_tvset(6,vf_ft)
		   bb=sl_tvmod(1,3)
		endif
    endif
;**
;** Fill Text
;** ---- ----
    if (pw  gt  0) or (vf_tmp) eq 1 then begin
	bb=sl_tvpop(vf_w,1)
	bb=sl_tvget(6,vf_ft)
	bb=sl_tvset(6,0)
	vf_l1=vf_x41(0)+3
	if (pw eq 4) or (vf_tmp eq 1) then begin
		if (pw eq 4) and (pfl ne 2) then vf_tt4=pttl
		if (pw eq 4) and (pfl ne 2) then vf_mm4(0)=pmen(*)
		bb=sl_tvpol(4,vf_x41,vf_y41,tv_flg(2)-1-tv_flg(2)/8,0)
		bb=sl_tvpol(4,vf_x46,vf_y46,tv_flg(2)-1,0)
		vf_l2=vf_y41(0)+3
		   bb=sl_tvmod(1,6)
		if sys_dep('MACHINE') eq 'win' then bb=sl_tvset(6,-1)
;		   bb=sl_tvt(vf_l1,vf_l2   ,vf_tt4,1.5,0,tv_flg(2)-1)
		   bb=sl_tvs(vf_l1,vf_l2   ,vf_tt4,2.8,0,tv_flg(2)-1)
		   bb=sl_tvfont(1)
		if sys_dep('MACHINE') eq 'win' then begin bb=sl_tvmod(1,3) & bb=sl_tvset(6,0) & j=10
							 endif else j=2
		for i=0,5 do $
		   bb=sl_tvs(vf_l1,vf_l2-(i+1)*vf_ch,vf_mm4(i),2.,0,40./(256./tv_flg(2)) ) ;(tv_flg(2)/j)
		   bb=sl_tvfont(0)
		   bb=sl_tvmod(1,3)
	endif
	if (pw eq 5) or (vf_tmp eq 1) then begin
		if (pw eq 5) and (pfl ne 2) then vf_tt5=pttl
		if (pw eq 5) and (pfl ne 2) then vf_mm5(0)=pmen(*)
		bb=sl_tvpol(4,vf_x41,vf_y51,tv_flg(2)-1-tv_flg(2)/8,0)
		bb=sl_tvpol(4,vf_x46,vf_y52,tv_flg(2)-1,0)
		vf_l2=vf_y51(0)+3
		if sys_dep('MACHINE') eq 'win' then j=tv_flg(2)/10 $
						 else begin j=tv_flg(2) & bb=sl_tvmod(1,6) & endelse
		   bb=sl_tvt(vf_l1,vf_l2   ,'!3'+vf_tt5,1.5,0,j-1)
		   bb=sl_tvfont(1)
		for i=0,1 do $
		   bb=sl_tvs(vf_l1,vf_l2-(i+1)*vf_ch,vf_mm5(i),2.,0,40./(256./tv_flg(2)) ) ;(j/2)
		   bb=sl_tvfont(0)
		   bb=sl_tvmod(1,3)
	endif
	bb=sl_tvset(6,vf_ft)
    endif
;**
;** Vectors
;** -------

    if (pw  eq -4) then begin

	a = c - bx & d = a+2*bx	   & if   a lt vsiz(7)  then a=vsiz(7)
	b = l - by & e = b+2*by    & if   b lt vsiz(8)  then b=vsiz(8)
	d = d - a -1		   & if a+d gt vsiz(13) then d=vsiz(13)-a
	e = e - b -1		   & if b+e gt vsiz(14) then e=vsiz(14)-b

	vf_l1=0
	vf_l2=0

	if (vf_y52(0) gt 40) and (d gt 0) then begin
	    vf_l1=a+1
	    if xare_z(1) ne d+1 then begin
	       bb=sl_psizm(xare,xare_z,1,d+1,vsiz(vsiz(0)+1),-1,-1,-1)
	       bb=sl_psizm(jare,jare_z,1,xare_z(1),4,-1,-1,-1)
	       jare(0)=sl_index(xare_z(1),4)
	       endif
	    if vsiz(0) lt 3 then  xare(0)=erey(a:a+d,l) $
			    else  xare(0)=erey(a:a+d,l,f)
	    mxx=sl_maxim(xare,xare_z,i,mnx)
	    p(0)=c-bx/2 & p(1)= p(0)+bx & if p(0) lt vsiz(7) then p(0)=vsiz(7)
	    p(1)=p(1)-p(0) -1 & if p(0)+p(1) gt vsiz(13) then p(1)=vsiz(13)-p(0)
	    d=p(0)+p(1)-a
	    a=p(0)-a
	endif

	if (vf_st     gt 40) and (e gt 0) then begin
	    vf_l2=b+1
	    if yare_z(2) ne e+1 then $
	       bb=sl_psizm(yare,yare_z,2,1,e+1,vsiz(vsiz(0)+1),-1,-1)
	    if iare_z(1) ne e+1 then $
	       bb=sl_psizm(iare,iare_z,1,e+1,4,-1,-1,-1)
	    if	((tv_od eq 1) and (iare(0) eq 0)) or $
		((tv_od eq 0) and (iare(0) eq 0)) then begin
	       if tv_od eq 1 then for i= 0 , e  do iare(i)=e-i $
			     else for i= 0 , e  do iare(i)=  i
	       endif
	    if vsiz(0) lt 3 then  yare(0,0)=erey(c,b:b+e) $
			    else  yare(0,0)=erey(c,b:b+e,f)
	    mxy=sl_maxim(yare,yare_z,i,mny)
	    h(0)=l-by/2 & h(1)= h(0)+by & if h(0) lt vsiz(8) then h(0)=vsiz(8)
	    h(1)=h(1)-h(0) -1 & if h(0)+h(1) gt vsiz(14) then h(1)=vsiz(14)-h(0)
	    e=h(0)+h(1)-b
	    b=h(0)-b
	endif

	if  vf_l2 gt 0 then begin
	    bb=sl_tvscreen(0,vf_st-1,0,vf_wy-1)
	    bb=sl_tvset(8 ,0,0,0,0,0,1,1)
	    bb=sl_tvxyz(mny,mxy,0,iare_z(1)-1)
	    h(0)=iare(l-vf_l2+1) & h(1)=h(0)
	    p(0)=0
	    if tv_od then p(1)=yare(0,yare_z(2)-1-h(0)) else p(1)=yare(0,h(0))
	    bb=sl_tvras(0,0,vf_st,vf_wy, 0 ,vf_st-1,vf_wy-1)
	    bb=sl_tvget(18,vf_l2)
	    bb=sl_tvset(18,0)
	    bb=sl_tvset(1,tv_flg(2)-1-tv_flg(2)/10)
	    bb=sl_tvplt(-1,e-b+1	,yare(0,b:e),e-b+1	,iare(b:e))
	    bb=sl_tvset(1,tv_flg(2)-1-tv_flg(2)/4)
	    bb=sl_tvplt(-1,b+1		,yare(0,0:b),b+1	,iare(0:b))
	    bb=sl_tvplt(-1,yare_z(2)-e	,yare(0,e:*),yare_z(2)-e,iare(e:*))
	    bb=sl_tvset(18,1)
	    bb=sl_tvset(1,tv_flg(2)-1-tv_flg(2)/8)
	    bb=sl_tvplt(-1,2,p,2,h)
	    bb=sl_tvset(18,vf_l2)
	    bb=sl_tvset(1,tv_flg(2)-1-tv_flg(2)/10)
	    bb=sl_tvset(8 ,1,1,0,0,0,0,0)
	endif else if vf_st gt 40 then $
	    bb=sl_tvras(0,0,vf_st,vf_wy ,0, vf_st-1,vf_wy-1)

	if  vf_l1 gt 0 then begin
	    bb=sl_tvscreen(vf_st,vf_bx-1,0,vf_y52(0)-1)
	    bb=sl_tvset(8 ,0,0,0,0,0,1,1)
	    bb=sl_tvxyz(0,xare_z(1)-1,mnx,mxx)
	    p(0)=c-vf_l1+1 & p(1)=p(0)
	    h(0)=0	   & h(1)=xare(p(0))
	    bb=sl_tvras(vf_st,0,vf_bx-vf_st,vf_y52(0),0,vf_bx-vf_st-1,vf_y52(0)-1)
	    bb=sl_tvget(18,vf_l1)
	    bb=sl_tvset(18,0)
	    bb=sl_tvset(1,tv_flg(2)-1-tv_flg(2)/10)
	    bb=sl_tvplt(-1,d-a+1	,jare(a:d),d-a+1	,xare(a:d))
	    bb=sl_tvset(1,tv_flg(2)-1-tv_flg(2)/4)
	    bb=sl_tvplt(-1,a+1		,jare(0:a),a+1		,xare(0:a))
	    bb=sl_tvplt(-1,xare_z(1)-d	,jare(d:*),xare_z(1)-d	,xare(d:*))
	    bb=sl_tvset(18,1)
	    bb=sl_tvset(1,tv_flg(2)-1-tv_flg(2)/8)
	    bb=sl_tvplt(-1,2,p,2,h)
	    bb=sl_tvset(18,vf_l1)
	    bb=sl_tvset(1,tv_flg(2)-1)
	    bb=sl_tvset(8 ,1,1,0,0,0,0,0)
	endif

    endif

    if vf_cw gt 0 then bb=sl_tvsels(vf_cw)
    endif
   endelse
return, 1
end
;
;
;
function sl_click  ,b1,b2,b3,ttl,flg
;******* ********   ** ** ** *** ***
;**	 input a number using the smallest device.
;**	 ----- - ------ ----- --- -------- ------
	common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_click, bb,nb,n2,rti,st,tmtl,x,xc,xd,xs,y,yp,yl,zerr,w_cw,w_no,w_ft,$
		 tc_7,bo,tc_ttl,st2,tc_x03,tc_y03,tc_x13,tc_y13,tc_x04,tc_y04,$
		 tc_sz,tc_are,tc_vsz,tc_sel
;**
common my_glor, f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
	nb  =b3
	bb  =sl_tvget(3,w_cw)
	bb  =sl_tvget(4,w_no)
	bo  =sl_tvsel(f_w1)
	if bo ne 1 then begin	bb=sl_glory(0) & bo=bb
				if bo gt  0 then bb=sl_tvsel(f_w1)
			endif
	if bo gt 0 then begin
	bb  =sl_tvget(6,w_ft)
;	bb  =sl_tvwake(f_w1)
	bb  =sl_tvpop(f_w1,1)
	bb  =sl_tvmcur(1,0,0)
	bb  =sl_tvclear(dummy)
	rti =0
	bb  =sl_tvnobut(0)
	fwx =f_wx + f_fg(13)*(f_wp) -40
	ot  =0   & yp=f_wy/2+3 &  xd=float(fwx)/(b2-b1+1)
	yl  =f_wy- yp & if  yl gt 15 then yl=15
	x   =fix(xd*(b3-b1) +1)
	xs  =fwx
	if x lt 0  then x=0
	if x gt xs then x=xs else xs=x
	xc  =x & xp=x
	y   =yp
	n2  =0

	tc_x03(0)=fwx*2/3   & tc_y03(0)=f_wy*4/5
	tc_x03(1)=x         & tc_y03(1)=yp-2+yl
	tc_x03(2)=x+35      & tc_y03(2)=tc_y03(1)

	ki = 35
	if x+35 gt tc_x03(0) then if x gt tc_x03(0) then ki=0 else ki=1
	tc_x13(0)=tc_x03(0) & tc_y13(0)=tc_y03(0)
	tc_x13(1)=x+ki      & tc_y13(1)=tc_y03(1)
	tc_x13(2)=tc_x13(1) & tc_y13(2)=tc_y03(1)-yl

	tc_x04(0)=tc_x03(1) & tc_y04(0)=tc_y03(1)
	tc_x04(1)=tc_x03(2) & tc_y04(1)=tc_y03(1)
	tc_x04(2)=tc_x03(2) & tc_y04(2)=tc_y13(2)
	tc_x04(3)=tc_x03(1) & tc_y04(3)=tc_y13(2)

	tmtl='    '+ f_tt(ttl) + '  "' + sl_str(b3,'(i5)') + ' "'
	st  =sl_str(b1,'(i5)')
	if b1 lt 0  then if ttl eq 1  then st='Front' $
		    else if ttl eq 12 then st='All'
	bb  =sl_tvs(1  ,f_wy-16,tc_ttl,2.5,0,-1)
	bb  =sl_tvs(1  ,4      ,tmtl  ,2.5,0,-1)
	bb  =sl_tvs(1  ,yp+yl+6,st    ,2.5,0,-1)
	st  =sl_str(b2,'(i5)')
	bb  =sl_tvs(fwx,yp+yl+6,st    ,2.5,0,-1)
	bb  =sl_tvmcur(2,x,y)

	bb  =sl_tvmod(1,6)
	if sys_dep('MACHINE') eq 'win' then bb=sl_tvset(6,-1) else bb=sl_tvset(6,0)

	st  =sl_str(nb,'(i5)')
	if nb lt 0  then if ttl eq 1  then st='Front' $
		    else if ttl eq 12 then st='All'
	bb=sl_tvs(x,yp,st,2.,0,tv_nc-1)
	bb=sl_tvpol(3,tc_x03,tc_y03,tv_flg(2)-1-tv_flg(2)/8,0)
	if ki ne 1 then $
	bb=sl_tvpol(3,tc_x13,tc_y13,tv_flg(2)-1-tv_flg(2)/4,0)
	bb=sl_tvpol(4,tc_x04,tc_y04,tv_nc-1,0)

	bb=sl_x('focus_in')

	repeat  begin
		if ot ne 0  then begin
		   if ot eq  1 then nb=sl_pfix(x/xd)+b1
		   if nb gt b2 then nb=b2
	 	   if nb lt b1 then nb=b1
		   if flg eq 2 then if xd lt 1.  then if ot eq 1 then      begin
				    n2=nb/2 & if nb ne n2*2 then nb=nb+1 & endif
		   st2=sl_str(nb,'(i5)')
		   if nb lt 0  then $
			 if ttl  eq 1  then begin nb=-1 & st2='Front'
			 endif else $
			 if ttl  eq 12 then begin nb=-1 & st2='All'      & endif

		   if ki ne 1 then $
		   bb=sl_tvpol(3,tc_x13,tc_y13,tv_flg(2)-1-tv_flg(2)/4,0)
		   bb=sl_tvpol(3,tc_x03,tc_y03,tv_flg(2)-1-tv_flg(2)/8,0)
		   bb=sl_tvs(xp,yp,st,2.,0,tv_nc-1)
		   bb=sl_tvpol(4,tc_x04,tc_y04,tv_nc-1,0)

		   st=st2
		   if x le fwx then xp=x else xp=fwx
		   ki = 35
		   if xp+35 gt tc_x03(0) then if xp gt tc_x03(0) then ki=0 else ki=1
		   tc_x03(1)=xp        & tc_x03(2)=xp+35
		   tc_x13(1)=xp+ki     & tc_x13(2)=tc_x13(1)
		   tc_x04(0)=tc_x03(1) & tc_x04(1)=tc_x03(2)
		   tc_x04(2)=tc_x03(2) & tc_x04(3)=tc_x03(1)
		   bb=sl_tvpol(4,tc_x04,tc_y04,tv_nc-1,0)
		   bb=sl_tvs(xp,yp,st,2.,0,tv_nc-1)

		   bb=sl_tvpol(3,tc_x03,tc_y03,tv_flg(2)-1-tv_flg(2)/8,0)
		   if ki ne 1 then $
		   bb=sl_tvpol(3,tc_x13,tc_y13,tv_flg(2)-1-tv_flg(2)/4,0)

		   if flg eq 1 then sl_level,nb
		   ot=0
		   n2=0
		endif else if n2 gt 200  then   bb=sl_tvwait(1., 1,2,f_w1 ,ki,0) $
			   else begin		bb=sl_tvwait(.1, 1,2,f_w1 ,ki,0)
						n2=n2+1  &  endelse
;**
		xc=x
		sl_signal,x ,y ,1,1,rti ,0
		if x ne xc then begin  nb=nb + (x-xc) & xs=x & ot =2 & endif
		bb=sl_tvgcur(x,y,zerr,0)
;**
		y =yp
		if x lt 0  then begin x=xc & bb =sl_tvmcur(2,x,y)
				bb=sl_tvsel(f_w1)
				if bb eq 1 then  bb=sl_tvpop(f_w1,1) else zerr=1
				bb=sl_tvgcur(x,y,zerr,0)
				if x  lt 0 then  begin zerr=1 & nb=b3 & endif
				endif
		if x ne xs then begin xs=x & ot =1
				endif
;**pb compil	if (rti eq 1) and (flg ge 0)  then bb=sl_trsig(1,10,0,0,0,rti)
	endrep  until (zerr gt 0) or (rti eq 33)
	bb  =sl_x('focus_out')
	bb  =sl_tvmod(1,3)
	bb  =sl_tvnobut(0)
	bb  =sl_tvclear(dummy)
	bb  =sl_tvset(6,w_ft)
	if bo   gt 0  then bb=sl_tvpop(f_w1,0) $
	else begin         bb=sl_tvdelwn(f_w1) & f_w1=-1 & endelse
	f_sh=1   & f_ic=0
	if w_cw gt 0  then bb=sl_tvsel(w_cw)
	endif
return,nb
end
;
;
;
pro sl_manycol,	wcol
;** **********
;** Get a color table.
;** *** * ****  *****
;
	common tmp_manycol, bb,dirc,i,rti,u,mis
;**
	common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common my_cl ,	cl_i,cl_cold,cl_ctb,cl_ttl,cl_hlp,cl_colm,cl_v2,cl_v3
;**
	common my_vcol,	r,g,b,	cr,cg,cb
;**
	common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
			io_cur,io_ext,io_seq,io_str
;**
if wcol ne 0   then begin
if wcol lt 100 then i =cl_ctb else i=-1
if i    ne -1  then begin i =sl_tvmenul(0,3,cl_colm,cl_ttl,tv_xp,tv_yp)
			  if i lt 0 then i =cl_cold+1 $
			  else begin bb=sl_tvgtcol(tr,tg,tb)
			  	     r(0)=tr & g(0)=tg & b(0)=tb
			  	     sl_colexp,-1
			  endelse
		    endif
repeat	begin
	if (i lt cl_ctb) or (i eq cl_ctb+3) then begin
;**		Load a color table.
			 if i eq -1 then begin
				    tv_col=wcol & i=cl_cold+1 & endif else $
			 if i ne cl_ctb+3 then tv_col=i+100
			 if i eq cl_ctb+3 then $
				 dirc=io_cur+'*' else $
			 	 dirc=sl_stbr(io_dir,2)+sl_str(tv_col,'(i3)')
;			 u  = sl_filr(dirc,io_ext(9),0,dirc,1)
			 u  =-1
			 if u gt 0  then begin
			      cl_v2(0)=1   & cl_v2(1)=cl_i
			      cl_v3(0)=2*cl_i & cl_v3(1)=4
			      bb =  sl_cellget(-u,cl_v2,cl_v3,r,0)
			      bb =  sl_cellget(-u,cl_v2,cl_v3,g,0)
			      bb =  sl_cellget(-u,cl_v2,cl_v3,b,0)
			      bb =  sl_iofree ( u)
;               	      and adjust with available entries.
			      k  =  cl_i-1
			      if i ne cl_ctb+3 then begin
					r(0)=0 & g(0)=0 & b(0)=0
					r(k)=k & g(k)=k & b(k)=k & endif
			      if (cl_i gt tv_flg(2)) and (i ne cl_ctb+3) then begin
				kf= float(cl_i) / tv_flg(2)
				l= 1
				while (l lt tv_flg(2)) do begin
					m=fix(l*kf)
					r(l)=r(m) & g(l)=g(m) & b(l)=b(m)
					l=l+1
				endwhile
				r(l-1)=k & g(l-1)=k & b(l-1)=k
				for j = l , k do begin
					r(j)=r(k) & g(j)=g(k) & b(j)=b(k)
				endfor
			      endif
			 endif else begin bb  =sl_tvloadct(tv_col-100,tr,tg,tb)
			  	     	  r(0)=tr & g(0)=tg & b(0)=tb & endelse
			 sl_colexp,-1
	endif else $
	if i eq cl_ctb+1 then begin u=sl_tvmerr (0)
	       if u eq 4 then begin
;**	       Levels
			 j=cl_i/2 +1
			 j=sl_click (-j,j,j,3,1) & endif else $
	       if u eq 2 then begin
;**	       Rotate
			 j =  sl_tvmenunw(5,0,cl_hlp ,'    ',tv_xp,tv_yp)
			 sl_colexp,-1
			 bb=  sl_x('focus_in')
			 for j=1,cl_i-1,1  do begin
			  cr(1)=sl_shift(cr(1:cl_i-1),cl_i-1,0,4  ,1,0)
       			  cg(1)=sl_shift(cg(1:cl_i-1),cl_i-1,0,4  ,1,0)
			  cb(1)=sl_shift(cb(1:cl_i-1),cl_i-1,0,4  ,1,0)
			  bb   =sl_tvldcol(cr,cg,cb)
			  rti  =sl_kb(s_rep)
			  if rti eq 33 then j=cl_i
			 endfor
			 bb=  sl_x('focus_out')
;			 bb  =sl_tvdmenunw(5)
	       endif else begin
;**	       Inverse
			 r(0)=(cl_i-1) - r
			 g(0)=(cl_i-1) - g
			 b(0)=(cl_i-1) - b
			 sl_colexp,-1 & endelse
	       endif else $
	if i eq cl_ctb+2 then begin u=sl_tvmerr (0)
;**	Set background
	       if u eq 4 then j=0 	else $
	       if u eq 2 then j=cl_i/2  else $
			      j=cl_i-1
		r(0)=j	 & g(0)=j  & b(0)=j
		r(1)=j	 & g(1)=j  & b(1)=j
		r(2)=j	 & g(2)=j  & b(2)=j
;prov(menu)
;		if j ne cl_i/2 then begin r(cl_i-7:cl_i-1)=cl_i-j-1
;					  g(cl_i-7:cl_i-1)=cl_i-j-1
;					  b(cl_i-7:cl_i-1)=cl_i-j-1 & endif
		sl_colexp,-1
	       endif else $
	if i eq cl_ctb+4 then begin
;**	Save colors
			 cl_v3(0)=cl_i & cl_v3(1)=3 & cl_v3(2)=tv_col
			 mis='?' & k=1
			 while k gt 0 do begin
			    u=sl_filw(cl_v3,sl_stbr(sl_str(io_seq,'(i3)'),2),$
					                io_ext(9),mis,0,0,0,k)

			    io_seq=io_seq+1
			 endwhile
			 if u gt 0  then begin
			      cl_v3(0)=1 & cl_v3(1)=cl_i & cl_v3(2)=4
			      bb=sl_cellput(cr,-u ,cl_v3)
			      bb=sl_cellput(cg,-u ,cl_v3)
			      bb=sl_cellput(cb,-u ,cl_v3)
			      bb=sl_iofree (u)
			      io_txt(0) ='.Created file: '+ mis +'.'+io_ext(9)
			      io_txt(1) ='.'
			      io_txt(2) ='---->  Click here to continue  <---- '
			      bb=sl_tvmenuc(5,3,io_txt,'Saved colors',tv_xp,tv_yp)
			      bb=sl_tvdmenu(5)
			      endif & endif
	if i lt cl_cold then  begin i=sl_tvmenu(0,1)
				    if i lt 0 then i =cl_cold+1
				    endif
	endrep   until  i ge cl_cold
	if i ge cl_cold then bb=sl_tvdmenu(0)
endif
return
end
;
;
;
;
;
pro sl_dc, ci,li,xi,yi
;** *****
;**	From index, return device coordinates.
;**
common tmp_dc,   k
;**
common my_tv,	 tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		 tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
		xd  =  nf / ny
		yd  =  nf - ny*xd
		xi  =  xd*plx + fix(fcx*ci)
		k   =  fix(fcy*li)
		yi  =  plny   - yd * ply - k - 1
		if tv_od eq 0 then yi=yi+2*k - ply + 1
return
end
;
;
;
;
;
pro sl_ellip, flg,erey,vsiz,nf,alph,cx,cy,ddx,ddy, itg,moy,nlz, sigma,sum
;** ********
;**
common  tmp_ellip,ccl,ja,jb,i1,i2,j1,j2,j3,bj,lcl,nz,typ,rx2,ry2,rxy2,ryx2,vtm,$
		  tr,n,ip,ri,rs,dx,dy,ang,coo,sii,co2,si2,cosi,a1,b1,b2,c1,d1,a4
;**
	common my_area ,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;**	(x,y)| (1/rx2 * cos2 + 1/ry2 * sin2), (1/rx2 - 1/ry2) * sin * cos  |(x)=1
;**	     |                                                             |(y)
;**	     | (1/rx2 - 1/ry2) * sin * cos  , (1/ry2 * cos2 + 1/rx2 * sin2)|
	n  = 0
	tr = 0.
	ccl =vsiz(1)-1
	lcl =vsiz(2)-1
	if   vsiz(0) lt 3 then nz=1 else nz=vsiz(3)
	typ= vsiz(vsiz(0)+1)
;
	dx  =ddx
	dy  =ddy
	ang =alph
	if dx  eq dy  then ang=0.
	if ang ne 0.  then begin
;**	Normalize angle.
;**	--------- -----
	   ip =fix(ang/360.)
	   ang=ang- ip*360.
	   if  ang  lt   0.  then ang=ang +360.
	   if  ang  ge 180.  then ang=ang -180.
	   if (ang  gt  45.) and (ang  lt  135.) then begin
	       dx =ddy & dy =ddx
	       if ang lt 90. then ang=ang + 90.  else $
	       if ang gt 90. then ang=ang - 90.  else ang=0.
	   endif
	   if  ang  ge 135.  then ang=ang - 180.
	endif
;
	rs=dx/2  & if rs*2 ne dx then rs=1   else rs=0
	ri=dy/2  & if ri*2 ne dy then ri=0.5 else ri=1.
;
	if ang ne 0.  then begin
	       ang=ang * 3.1416 / 180.
	       coo=sl_cos(ang)  & sii=sl_sin(ang)
     	       i1 =dx/2		& j1 =dy/2
	       i2 =sl_pfix(coo*i1 - sii*j1)
	       j2 =sl_pfix(sii*i1 + coo*j1)
	       if (i1 eq i2) and (j1 eq j2)	 then ang=0. $
	       else begin
			rx2 = float(dx)    /2 & rx2 = rx2*rx2
			ry2 =(float(dy))   /2 & ry2 = ry2*ry2
			co2 = coo*coo	      & si2 = sii*sii
			cosi= coo*sii
			a1  =(co2/rx2 + si2/ry2)       * 2.
			a4  = a1 *  2.
			c1  = co2/ry2 + si2/rx2
			b1  = (1./rx2 - 1./ry2) * cosi * 2.
			b2  = b1 * b1
	       endelse
	endif
;
	if ang eq 0.  then begin
	ja =   cy -dy/2
	jb =   ja +dy
	if ja	 lt  0  then ja=0
	if jb	 gt lcl then jb=lcl
;
	j1 =(1.-ri)+cy-ja
	rx2= float(dx)    /2 & rx2 =rx2*rx2
	ry2=(float(dy)+ri)/2 & ry2 =ry2*ry2
	if ry2  gt 0   then    rxy2=rx2/ry2 else rxy2=0.
	j3 =1.
;
	if (flg eq 1) then begin
;**	Low pass
;**	--- ----
		ip  =0
		if  ja gt 0  then  begin
				 ip=ip + (ccl+1)*ja
				 if nz eq 1 then erey(*,0:ja-1)   =nlz $
					    else erey(*,0:ja-1,nf)=nlz & endif
		for jj= ja , jb do begin
		    vtm=rx2 -j1*j1*rxy2
		    if vtm gt 0 then bb=sl_sqrt(vtm,1) else vtm=0.
		    i1 =sl_pfix(vtm)
		    i2 =cx+i1+1+rs
		    i1 =cx-i1-1
		    if  i1 ge 0    then begin
			ip=ip+i1+1
			if nz eq 1 then begin erey(0:i1,jj)     =nlz
			endif	   else begin erey(0:i1,jj,nf)  =nlz  & endelse
			endif
		    if  i2 le ccl  then begin
			ip=ip+ccl-i2+1
			if nz eq 1 then begin erey(i2:ccl,jj)   =nlz
			endif	   else begin erey(i2:ccl,jj,nf)=nlz  & endelse
			endif
		    j1= j1-j3
		endfor
		if  jb lt lcl then begin
				   ip=ip + (ccl+1)*(lcl-jb)
				   if nz eq 1 then erey(*,jb+1:lcl)   =nlz $
					      else erey(*,jb+1:lcl,nf)=nlz & endif
		itg =vsiz(1)*vsiz(2) -ip
	endif
	if (flg eq 2) then begin
;**	High pass
;**	---- ----
		for jj= ja , jb do begin
		    vtm=rx2 -j1*j1*rxy2
		    if vtm gt 0 then bb=sl_sqrt(vtm,1) else vtm=0.
		    i1 =sl_pfix(vtm)
		    i2 =cx+i1+rs
		    i1 =cx-i1
		    if  i1 lt 0   then i1=0
		    if  i2 gt ccl then i2=ccl
		    if  i1 le i2  then $
		    if  nz eq 1   then erey(i1:i2,jj)   =nlz $
				  else erey(i1:i2,jj,nf)=nlz
		    j1= j1-j3
		endfor
	endif
	if (flg eq 3) then begin
;**	Circ Vector
;	best if ry ge rx
;	----------------
		j2= (jb-ja)*2 +1
		if aregx_z(1) ne j2+1 then $
		bb=sl_psizm(aregx,aregx_z,1,j2+1,4,-1,-1,-1)
		if aregy_z(1) ne j2+1 then $
		bb=sl_psizm(aregy,aregy_z,1,j2+1,4,-1,-1,-1)
		for jj= ja , jb do begin
		    vtm=rx2 -j1*j1*rxy2
		    if vtm gt 0 then bb=sl_sqrt(vtm,1) else vtm=0.
		    i1 =sl_pfix(vtm)
		    i2 =cx+i1+1+rs
		    i1 =cx-i1-1
		    if  i1 lt 0   then i1=0
		    if  i2 gt ccl then i2=ccl
			aregx(n)   =i1
			aregx(j2-n)=i2
			aregy(n)   =jj
			aregy(j2-n)=jj
		    n  =n +1
		    j1= j1-j3
		endfor
	endif
	if (flg eq 4) then begin
;**	Circ integration :background, np , low pass , I , sigma(I)
;**	---- -----------
		ip  =0
		if  ja gt 0   then begin tr=tr+erey(cx,ja-1) & n=  1
		    ip=ip + (ccl+1)*ja
		    if nlz ne -1 then erey(*,0:ja-1)=nlz
		endif
;
		for jj= ja , jb do begin
		    vtm=rx2 -j1*j1*rxy2
		    if vtm gt 0 then bb=sl_sqrt(vtm,1) else vtm=0.
		    i1 =sl_pfix(vtm)
		    i2 =cx+i1+1+rs
		    i1 =cx-i1-1
		    if  i1 ge 0      then begin
			if i1 le ccl then begin
				n =n +1
				tr=tr+erey(i1,jj)
			endif else i1=ccl
			ip=ip+i1+1
			if nlz ne -1 then erey(0:i1,jj)  =nlz
		    endif
		    if  i2 le ccl    then begin
			if i2 ge  0  then begin
				n =n +1
				tr=tr+erey(i2,jj)
			endif else  i2=0
			ip=ip+ccl-i2+1
			if nlz ne -1 then erey(i2:ccl,jj)=nlz
		    endif
		    j1= j1-j3
		endfor
		if  jb lt lcl then begin tr=tr+erey(cx,jb+1) & n=n+1
		    ip=ip + (ccl+1)*(lcl-jb)
		    if nlz ne -1 then erey(*,jb+1:lcl)=nlz
		endif
		itg =vsiz(1)*vsiz(2) -ip
		if n gt 0 then moy= tr / n else moy=0.001
;
		sum  =0.
		sigma=0.
		for jj= jb , ja , -1 do begin
		    j1= j1+j3
		    vtm=rx2 -j1*j1*rxy2
		    if vtm gt 0 then bb=sl_sqrt(vtm,1) else vtm=0.
		    i1 =sl_pfix(vtm)
		    i2 =cx+i1+rs
		    i1 =cx-i1
		    if  i1 lt 0   then  i1=0
		    if  i2 gt ccl then  i2=ccl
		    if  i1 le i2  then  for k =i2,i1,-1 do begin
					 vtm  =erey(k,jj)
					 sum  =sum  +vtm
					 vtm  =vtm  -moy
					 sigma=sigma+vtm*vtm
					endfor
		endfor
		if itg gt 1 then  begin
			    sigma=sigma/(itg-1)
			    bb=sl_sqrt(sigma,1) & endif
	endif
;************ angle
	endif else begin
;	(a11)X*X + (a21+a12)Y*X + (a22)Y*Y -1 =0
;	-----a     ----------b    -----------c
;	ja,jb for  b*b-4ac=0  with b*b  = b2 * y*y
;				   4*a  = a4
;				   c	= c1 * y*y -1
;       y ~ j1
;	i = (-b +-sqrt(b*b-4ac)) / 2a
		j1 = -a4 / (b2  -  a4*c1)
		bb = sl_sqrt(j1,1)
		j2 = sl_pfix(j1)
		ja = cy - sl_pfix(j1-0.2)
		jb = cy + sl_pfix(j1+0.2)
		if ja lt 0   then begin  j1=j1+ja & ja=0   &  endif
		if jb gt lcl then jb=lcl
;	correcteur Y
		j1=j1- 0.5
		j3=1.- 1.0/(jb-ja)
;
	if (flg eq 1) then begin
;**	Low pass
;**	--- ----
		ip  =0
		if  ja gt 0  then  begin
				 ip=ip + (ccl+1)*ja
				 if nz eq 1 then erey(*,0:ja-1)   =nlz $
					    else erey(*,0:ja-1,nf)=nlz & endif
		for jj= ja , jb do begin
		    d1 =(b2 - a4*c1)* j1*j1 + a4
		    bj =-b1*j1
		    if d1 gt 0 then bb = sl_sqrt(d1,1) else d1=0.
		    i1 =sl_pfix((bj -d1) / a1) + cx-1
		    i2 =sl_pfix((bj +d1) / a1) + cx+1 +rs
		    if  i1 ge 0    then begin
			if i1 gt ccl then  i1=ccl
			ip=ip+i1+1
			if nz eq 1 then begin erey(0:i1,jj)     =nlz
			endif	   else begin erey(0:i1,jj,nf)  =nlz  & endelse
			endif
		    if  i2 le ccl  then begin
			if i2 lt  0  then  i2=0
			ip=ip+ccl-i2+1
			if nz eq 1 then begin erey(i2:ccl,jj)   =nlz
			endif	   else begin erey(i2:ccl,jj,nf)=nlz  & endelse
			endif
		    j1= j1-j3
		endfor
		if  jb lt lcl then begin
				   ip=ip + (ccl+1)*(lcl-jb)
				   if nz eq 1 then erey(*,jb+1:lcl)   =nlz $
					      else erey(*,jb+1:lcl,nf)=nlz & endif
		itg =vsiz(1)*vsiz(2) -ip
	endif
	if (flg eq 2) then begin
;**	High pass angle
;**	---- ---- -----
		for jj= ja , jb do begin
		    d1 =(b2 - a4*c1)* j1*j1 + a4
		    bj =-b1*j1
		    if d1 gt 0 then bb = sl_sqrt(d1,1) else d1=0.
		    i1 =sl_pfix((bj -d1) / a1) + cx
		    i2 =sl_pfix((bj +d1) / a1) + cx +rs
		    if  i1 lt 0   then i1=0
		    if  i2 gt ccl then i2=ccl
		    if  i1 le i2  then $
		    if  nz eq 1   then erey(i1:i2,jj)   =nlz $
				  else erey(i1:i2,jj,nf)=nlz
		    j1= j1-j3
		endfor
	endif
	if (flg eq 3) then begin
;**	Circ Vector angle
;	---- ------ -----
		j2= (jb-ja)*2 +1
		if aregx_z(1) ne j2+1 then $
		bb=sl_psizm(aregx,aregx_z,1,j2+1,4,-1,-1,-1)
		if aregy_z(1) ne j2+1 then $
		bb=sl_psizm(aregy,aregy_z,1,j2+1,4,-1,-1,-1)
		for jj= ja , jb do begin
		    d1 =(b2 - a4*c1)* j1*j1 + a4
		    bj =-b1*j1
		    if d1 gt 0 then bb = sl_sqrt(d1,1) else d1=0.
		    i1 =sl_pfix((bj -d1) / a1) + cx-1
		    i2 =sl_pfix((bj +d1) / a1) + cx+1 +rs
		    if  i1 lt 0   then i1=0
		    if  i2 gt ccl then i2=ccl
		    if  i1 gt i2  then if i2 lt 0 then i2=i1 else i1=i2
			aregx(n)   =i1
			aregx(j2-n)=i2
			aregy(n)   =jj
			aregy(j2-n)=jj
		    n  =n +1
		    j1= j1-j3
		endfor
	endif
	if (flg eq 4) then begin
;**	Circ integration angle :background, np , low pass , I , sigma(I)
;**	---- ----------- -----
		ip  =0
		    d1 =(b2 - a4*c1)* j1*j1 + a4
		    bj =-b1*j1
		    if d1 gt 0 then bb = sl_sqrt(d1,1) else d1=0.
		    i1 =sl_pfix((bj -d1) / a1) + cx-1
		    i2 =sl_pfix((bj +d1) / a1) + cx+1 +rs
		if  ja gt 0   then begin i1=(i1+i2)/2
		    if (i1 ge 0) and (i1 le ccl) then begin n=1
				tr=tr+erey(i1,ja-1) & endif
;
		    ip=ip + (ccl+1)*ja
		    if nlz ne -1 then erey(*,0:ja-1)=nlz
		endif
;
		for jj= ja , jb do begin
		    d1 =(b2 - a4*c1)* j1*j1 + a4
		    bj =-b1*j1
		    if d1 gt 0 then bb = sl_sqrt(d1,1) else d1=0.
		    i1 =sl_pfix((bj -d1) / a1) + cx-1
		    i2 =sl_pfix((bj +d1) / a1) + cx+1 +rs
		    if  i1 ge 0      then begin
			if i1 le ccl then begin
				n =n +1
				tr=tr+erey(i1,jj)
			endif else i1=ccl
			ip=ip+i1+1
			if nlz ne -1 then erey(0:i1  ,jj)=nlz
		    endif
		    if  i2 le ccl    then begin
			if i2 ge  0  then begin
				n =n +1
				tr=tr+erey(i2,jj)
			endif else  i2=0
			ip=ip+ccl-i2+1
			if nlz ne -1 then erey(i2:ccl,jj)=nlz
		    endif
		    j1= j1-j3
		endfor
;
		if  jb lt lcl then begin i1=(i1+i2)/2
		    if (i1 ge 0) and (i1 le ccl) then begin n=n+1
				tr=tr+erey(i1,jb+1) & endif
;
		    ip=ip + (ccl+1)*(lcl-jb)
		    if nlz ne -1 then erey(*,jb+1:lcl)   =nlz
		endif
		itg =vsiz(1)*vsiz(2) -ip
		if n gt 0 then moy= tr / n else moy=0.001
;
		sum  =0.
		sigma=0.
		for jj= jb , ja , -1 do begin
		    j1= j1+j3
		    d1 =(b2 - a4*c1)* j1*j1 + a4
		    bj =-b1*j1
		    if d1 gt 0 then bb = sl_sqrt(d1,1) else d1=0.
		    i1 =sl_pfix((bj -d1) / a1) + cx
		    i2 =sl_pfix((bj +d1) / a1) + cx +rs
		    if  i1 lt 0   then i1=0
		    if  i2 gt ccl then i2=ccl
		    if  i1 le i2  then  for k =i2,i1,-1 do begin
					 vtm  =erey(k,jj)
					 sum  =sum  +vtm
					 vtm  =vtm  -moy
					 sigma=sigma+vtm*vtm
					endfor
		endfor
		if itg gt 1 then  begin
			    sigma=sigma/(itg-1)
			    bb=sl_sqrt(sigma,1) & endif
	endif
	endelse
return
end
;
;
function sl_radi_a, iy,ix ,fang,lang
;******* *********
;**
;**	test point inside  angle
;**
	common  my_radia, vtm,bb,rad_6,rad_57
;**
		vtm=0.
	   	if (iy ne 0) or (ix ne 0) then vtm=sl_atang(iy,ix)
		if vtm lt 0 then vtm= rad_6 + vtm
		vtm=vtm*rad_57
		if  lang ge fang  then begin
		 if (vtm ge fang) and (vtm le lang) then bb=1 else bb=0
		endif else $
		 if (vtm ge fang) or  (vtm le lang) then bb=1 else bb=0
return, bb
end
;
function sl_radies, erey,vsiz,nf,alph,cx,cy,ddx,ddy, sum,np ,fang,lang
;******* *********
;**
	common  tmp_rad, oz,ez,i,j,pi,bb,tr
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey	,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
	if (fang eq 0.) and (lang eq 360.) then tr=0 else tr=1
	sum=0.
	np =0
	if vsiz(0) gt 2 then ez=1 else ez=0

	sl_ellip,3, erey,vsiz,nf,alph,cx,cy,ddx,ddy, 0,0,0

	oz =aregx_z(1)-1
	if oz gt 0 then begin
	 pi=aregx(0)
	 if tr eq 1 then begin
	   for k1=0,oz-1 do begin
		i=aregx(k1)
		j=aregy(k1)
		if i eq pi then pi= pi+1
		if i lt pi then for k2=i,pi-1  do begin
				 bb=sl_radi_a( cy-j, k2-cx, fang,lang)
				 if bb eq 1 then begin
				    np=np+1
				    if ez eq 1  then sum=sum+erey(k2,j,nf) $
						else sum=sum+erey(k2,j)
				 endif
				endfor $
		else		for k2 =pi+1,i  do begin
				 bb=sl_radi_a( cy-j, k2-cx, fang,lang)
				 if bb eq 1 then begin
				    np=np+1
				    if ez eq 1  then sum=sum+erey(k2,j,nf) $
						else sum=sum+erey(k2,j)
				 endif
				endfor
		pi=i
	   endfor
	 endif else begin
	   for k1=0,oz-1 do begin
		i=aregx(k1)
		j=aregy(k1)
		if i eq pi then pi= pi+1
		if i lt pi then for k2=i,pi-1  do begin
				    np=np+1
				    if ez eq 1  then sum=sum+erey(k2,j,nf) $
						else sum=sum+erey(k2,j)
				endfor $
		else		for k2 =pi+1,i  do begin
				    np=np+1
				    if ez eq 1  then sum=sum+erey(k2,j,nf) $
						else sum=sum+erey(k2,j)
				endfor
		pi=i
	   endfor
	 endelse
;**	Care same coord. for down.
;**	---- ---- -----  --- ----
	   k1=aregx((oz+1)/2 -1)  & k2=aregx((oz+1)/2)
	   if k1 eq k2 then begin   j =aregy((oz+1)/2)
				 bb=sl_radi_a( cy-j, k2-cx, fang,lang)
				 if bb eq 1 then begin
				    np=np-1
				    if ez eq 1  then sum=sum-erey(k2,j,nf) $
						else sum=sum-erey(k2,j)
				 endif
	   endif
;**	Close coord. for up.
;**	----- -----  --- --
	   k1=aregx(0) & k3=aregx(oz) & j=aregy(0)
	   if k1 lt k3-1 then   for k2 =k1+1,k3-1 do begin
				 bb=sl_radi_a( cy-j, k2-cx, fang,lang)
				 if bb eq 1 then begin
				    np=np+1
				    if ez eq 1  then sum=sum+erey(k2,j,nf) $
						else sum=sum+erey(k2,j)
				 endif
				endfor
	endif
return,1
end
;
;
pro sl_box ,flg
;** ******  ***
;**
common my_box,	  bx_tb,bx_ty,bx_fl,bx_dc,bx_pc ,bx_pl ,bx_f,$
		  bx_c1,bx_c2,bx_l1,bx_l2,bx_cl1,bx_cl2,bx_lc1,bx_lc2,$
		  bx_cx,bx_cy,bx_dx,bx_dy
;**
common my_glor,	  f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		  f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common	my_tvg,	w_co,w_cw,w_fl,w_fy,w_hi,w_lo,w_lt,w_nc,w_no,w_od,$
			w_ps,w_ty,w_ig,w_wk
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey	,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;**	repare.
;**	------
if (flg eq 1) or (flg eq 2) then begin
    bb =sl_tvscreen(0,plnx-1,0,plny-1)
    bb =sl_tvxyz   (0,plnx-1,0,plny-1)
    if (bx_fl(0) gt 0) then $
	if (bx_fl(0) ne 3) or (f_fg(31) ne 1) then begin
;**	repare box.
;**	------ ---
	   bb=sl_tvmod(1,6)

	   sl_box_draw ,0

;	   if (bx_fl(0) eq 2)  or (bx_fl(0) eq 3) then  begin
;			 if arex_z(6) ne 0 then bb=sl_dd(2,arex,arex_z)
;			 bb=sl_psiz(arex_z, 1,bx_dc(0,2)+1,4,-1,-1,-1)
;			 arex_z(6)=0
;;			 bb=sl_dd(1,arex,arex_z)
;;			 arex(0)     =bx_tb(0:bx_dc(0,2),2)
;			 bb=sl_tvimag(bx_tb(0:bx_dc(0,2),2),$
;				      arex_z,bx_dc(1,2),bx_dc(2,2))
;		      if bx_fl(3) gt 0 then for kk=0,bx_fl(3)-1 $
;			          do bb=sl_tvimag(bx_tb(0:bx_dc(0,2),2),$
;				  arex_z,bx_dc(1,2)+arev(kk,9),bx_dc(2,2)+arev(kk,10))
;			 endif
;	   if (bx_fl(0) eq 1) or   (bx_fl(0) eq 3) then  begin
;			 if arey_z(6) ne 0 then bb=sl_dd(2,arey,arey_z)
;			 bb=sl_psiz(arey_z, 2,1,bx_dc(0,0)+1,4,-1,-1)
;			 arey_z(6)=0
;;			 bb=sl_dd(1,arey,arey_z)
;	    		 for k=0,1 do begin
;;			     arey(0,0)   =bx_ty(k,0:bx_dc(0,0))
;			     bb=sl_tvimag(bx_ty(k,0:bx_dc(0,0)),$
;				          arey_z,bx_dc(1,k),bx_dc(2,k))
;			 endfor
;		      if bx_fl(3) gt 0 then for kk=0,bx_fl(3)-1 $
;			          do for k=0,1 do bb=sl_tvimag(bx_ty(k,0:bx_dc(0,0)),$
;				  arey_z,bx_dc(1,k)+arev(kk,9),bx_dc(2,k)+arev(kk,10))
;			 endif
;	   if (bx_fl(0) eq 2)  or (bx_fl(0) eq 3) then  begin
;			 arex(0)     =bx_tb(0:bx_dc(0,2),3)
;			 bb=sl_tvimag(bx_tb(0:bx_dc(0,2),3),$
;				      arex_z,bx_dc(1,3),bx_dc(2,3))
;		      if bx_fl(3) gt 0 then for kk=0,bx_fl(3)-1 $
;			          do bb=sl_tvimag(bx_tb(0:bx_dc(0,2),3),$
;				  arex_z,bx_dc(1,3)+arev(kk,9),bx_dc(2,3)+arev(kk,10))
;			 endif
	   bb=sl_tvmod(1,3)
	endif
	bx_fl(0)=0
    if  bx_fl(2) gt 0  then begin
;**	repare ellip.
;**	------ -----
		  bb=sl_tvmod(1,6)
		  bb=sl_tvget(18,w_lt)
		  bb=sl_tvset(18,0)
		  bb=sl_tvset(35,2)
		  bb=sl_tvline(aregx,aregy,aregx_z(1),-1,-1)
		  if  bx_fl(3) gt 0 then for k=0,bx_fl(3)-1 do $
			  bb=sl_tvline(aregx+arev(k,9),aregy+arev(k,10),2,0,-1)
		  bb=sl_tvset(35,1)
		  bb=sl_tvset(18,w_lt)
		  bb=sl_tvmod(1,3)
		  bx_fl(2)=0
		  if flg eq 1 then bb=sl_dd(2,aregx,aregx_z)
		  if flg eq 1 then bb=sl_dd(2,aregy,aregy_z)
		  endif
    if  bx_fl(1) gt 0  then begin
;**	repare slice.
;**	------ -----
		  bx_pc(0)=bx_dc(0,8)  &   bx_pl(0)=bx_dc(1,8)
		  bx_pc(1)=bx_dc(0,9)  &   bx_pl(1)=bx_dc(1,9)
		  bb=sl_tvmod(1,6)
		  bb=sl_tvget(18,w_lt)
		  bb=sl_tvset(18,0)
		  bb=sl_tvset(35,2)
		  bb=sl_tvline(bx_pc,bx_pl,2,0,-1)
		  if  bx_fl(3) gt 0 then for k=0,bx_fl(3)-1 do $
			  bb=sl_tvline(bx_pc+arev(k,9),bx_pl+arev(k,10),2,0,-1)
		  bb=sl_tvset(35,1)
		  bb=sl_tvset(18,w_lt)
		  bb=sl_tvmod(1,3)
		  bx_fl(1)=0 & endif
    bx_fl(3)=0
endif
;**
;**	slice & box.
;**	----- - ---
if (flg eq 2) then begin
;   Check many frames
    bx_fl(3)=fcm-nf
    if bx_fl(3) gt 0 then begin
	sl_dc,  c   ,  l    ,bx_cx ,bx_cy
	bx_f=nf+1
	for k =bx_f,fcm do begin
	    nf=k
	    sl_dc,  c,  l    ,bx_c1 ,bx_l1
	    arev(k-bx_f, 9)=bx_c1-bx_cx
	    arev(k-bx_f,10)=bx_l1-bx_cy
	endfor
	nf=bx_f-1
    endif
    if (f_fg(24) eq 1)  then begin
;		  Slice.
		  sl_dc ,bxy(6)-cp,bxy(7)-lp,bx_c1,bx_l1
		  bx_dc(0,8)=bx_c1	&  bx_dc(1,8)=bx_l1
		  sl_dc,c,l ,bx_c1,bx_l1
		  bx_dc(0,9)=bx_c1	&  bx_dc(1,9)=bx_l1
		  bx_pc(0)  =bx_dc(0,8) &  bx_pl(0)  =bx_dc(1,8)
		  bx_pc(1)  =bx_dc(0,9) &  bx_pl(1)  =bx_dc(1,9)
		  bb=sl_tvmod(1,6)
		  bb=sl_tvget(18,w_lt)
		  bb=sl_tvset(18,0)
		  bb=sl_tvset(35,2)
		  bb=sl_tvline(bx_pc,bx_pl,2,0,-1)
		  if  bx_fl(3) gt 0 then for k=0,bx_fl(3)-1 do $
			  bb=sl_tvline(bx_pc+arev(k,9),bx_pl+arev(k,10),2,0,-1)
		  bb=sl_tvset(35,1)
		  bb=sl_tvset(18,w_lt)
		  bb=sl_tvmod(1,3)
		  bx_fl(1)=1 & endif
    if (f_vu gt 0) and ((f_vu lt 6) or (f_vu eq 7))  then begin
	bxy(0)  =f_fg(1) & bxy(1)=f_fg(2)
	bx_fl(0)=3
	if (f_vu eq 5) or (lcl eq 0) or (f_fg(8) eq 1) then begin bx_fl(0)=1
						     bxy(1)=1 & endif else $
	if (f_vu eq 4) or (ccl eq 0) or (f_fg(8) eq 2) then begin bx_fl(0)=2
						     bxy(0)=1 & endif
;	data coord.
	bx_f  = 3
	bx_c1 = c-bxy(0)/2 & bx_c2 = bx_c1 +bxy(0) & if bx_c1 lt 0  then bx_c1=0
	if bx_c2 gt ccl then bx_c2 = ccl    else   bx_f=bx_f -1
	bx_l1 = l-bxy(1)/2 & bx_l2 = bx_l1 +bxy(1) & if bx_l1 lt 0  then bx_l1=0
	if bx_l2 gt lcl then bx_l2 = lcl    else   bx_f=bx_f -2
;
	bx_cl2=(bx_l2 -bx_l1)/4
	bx_cl1= bx_l1 +bx_cl2
	bx_cl2= bx_l2 -bx_cl2
;
	bx_lc2=(bx_c2 -bx_c1)/4
	bx_lc1= bx_c1 +bx_lc2
	bx_lc2= bx_c2 -bx_lc2
;
;**	       c1       c2    x
;**	     l1	 _______
;**		|       |cl1  y
;**		|       |cl2  y
;**	     l2	 -------
;**	        lc1   lc2     x
;**
;	Device coord.
		if (bx_fl(0) eq 1) or (bx_fl(0) eq 3) then begin
;		Vertical bar
		  sl_dc,bx_c1 ,bx_cl1 ,bx_c1 ,bx_cl1
		  sl_dc,bx_c2 ,bx_cl2 ,bx_c2 ,bx_cl2
;
		  if fcx lt 1 then begin
		     if bx_c1  lt 0    then  bx_c1=0
		     if bx_c2  ge plnx then  bx_c2=plnx-1
		  endif
		  if fcy lt 1 then begin
		     if bx_cl1 lt 0    then  bx_cl1=0 else $
		     if bx_cl1 ge plny then  bx_cl1=plny-1
		     if bx_cl2 lt 0    then  bx_cl2=0 else $
		     if bx_cl2 ge plny then  bx_cl2=plny-1
		  endif
;
		  if bx_cl1 eq bx_cl2     then begin
			if bx_cl1 gt 0      then bx_cl1=bx_cl1-1
			if bx_cl2 lt plny-1 then bx_cl2=bx_cl2+1  & endif
		  bx_dc(0,0)=  bx_cl1 -bx_cl2
		  if ((bx_f eq 1)  or (bx_f eq 3)) and (fcx gt 1) then $
						   bx_c2 =bx_c2+fcx -1
		  bx_dc(1,0)=bx_c1  &  bx_dc(2,0)= bx_cl2
		  bx_dc(1,1)=bx_c2  &  bx_dc(2,1)= bx_cl2
		  if bx_dc(0,0) lt  0  then begin  bx_dc(0,0)= -bx_dc(0,0)
		  			           bx_dc(2,0)=  bx_cl1
					           bx_dc(2,1)=  bx_cl1
		endif & endif
		if (bx_fl(0) eq 2) or (bx_fl(0) eq 3) then begin
;		Horizontal bar
		  sl_dc,bx_lc1,bx_l1  ,bx_lc1,bx_l1
		  sl_dc,bx_lc2,bx_l2  ,bx_lc2,bx_l2
;
		  if fcx lt 1 then begin
		     if bx_lc1 lt 0    then  bx_lc1=0
		     if bx_lc2 ge plnx then  bx_lc2=plnx-1
		     endif
		  if fcy lt 1 then begin
		     if bx_l1  lt 0    then  bx_l1=0 else $
		     if bx_l1  ge plny then  bx_l1=plny-1
		     if bx_l2  lt 0    then  bx_l2=0 else $
		     if bx_l2  ge plny then  bx_l2=plny-1
		     endif
;
		  if bx_lc1 eq bx_lc2     then begin
			if bx_lc1 gt 0      then bx_lc1=bx_lc1-1
			if bx_lc2 lt plnx-1 then bx_lc2=bx_lc2+1  & endif
		  bx_dc(0,2)=  bx_lc2 -bx_lc1
		  if ((bx_f eq 2)  or (bx_f eq 3)) and (fcy gt 1) then $
		   if bx_l1 gt bx_l2   then bx_l2= bx_l2-fcy+1  $
				       else bx_l2= bx_l2+fcy-1
		  bx_dc(1,2)=bx_lc1 &  bx_dc(2,2)= bx_l1
		  bx_dc(1,3)=bx_lc1 &  bx_dc(2,3)= bx_l2
		endif
;       Save screen and plot.
		bb=sl_tvget(18,w_lt)
;
		if (f_fg(31) ne 0) and (bx_fl(0) eq 3) then begin
;		Elliptic
		    sl_dc,  c   ,  l    ,bx_cx ,bx_cy
		    bx_dx=fix(bxy(0)*fcx)
		    bx_dy=fix(bxy(1)*fcy)
		    if bx_l1 le bx_l2 then bx_f =1 else bx_f=-1
		    if fcx gt 1 then	   bx_cx=bx_cx + fix(fcx/2)
		    if fcy gt 1 then  $
		       if bx_f eq  1  then bx_cy=bx_cy + fix(fcy/2) $
				      else bx_cy=bx_cy - fix(fcy/2)
		    bb=sl_psiz(csiz,2,plnx,plny,2,-1,-1)
		    sl_ellip,3,0,csiz,0,f_el*bx_f,bx_cx,bx_cy,bx_dx,bx_dy, 0,0,0
		    bb=sl_tvmod(1,6)
		    bb=sl_tvset(18,0)
		    bb=sl_tvset(35,2)
		    bb=sl_tvline(aregx,aregy,aregx_z(1),-1,-1)
		    if    bx_fl(3) gt 0 then for k=0,bx_fl(3)-1 do $
			  bb=sl_tvline(aregx+arev(k,9),aregy+arev(k,10),2,0,-1)
		    bb=sl_tvset(35,1)
		    bb=sl_tvmod(1,3)
		    bx_fl(2)=1
		endif
		if (f_fg(31) ne 1) or (bx_fl(0) ne 3) then begin
;		Box
		bb=sl_tvmod(1,6)
		if  bx_fl(0) eq 3 then bb=sl_tvset(18,0)

		sl_box_draw ,0

		bb=sl_tvmod(1,3)
		endif
		bb=sl_tvset(18,w_lt)
    endif
;**
;**	Fix slice.
;**	--- -----
endif else $
if (flg eq 3)   then  bx_fl(1)=0 else $
if (flg eq 0)   then  begin
;**
;**	Init.
;**	----
	bx_fl(*)   =0
	bx_dc(0,4) =plny-1 & bx_dc(0,5) =plny-1
	bx_dc(0,6) =plnx-1 & bx_dc(0,7) =plnx-1 & endif
;**
return
end
;
pro sl_box_draw, dum
;** ***********
;**
common my_box,	  bx_tb,bx_ty,bx_fl,bx_dc,bx_pc ,bx_pl ,bx_f,$
		  bx_c1,bx_c2,bx_l1,bx_l2,bx_cl1,bx_cl2,bx_lc1,bx_lc2,$
		  bx_cx,bx_cy,bx_dx,bx_dy
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey	,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
		if (bx_fl(0) eq 2) or (bx_fl(0) eq 3) then begin
		    bx_pc(0)=bx_lc1 &  bx_pl(0)=bx_l1
		    bx_pc(1)=bx_lc2 &  bx_pl(1)=bx_l1

		    if bx_fl(0) eq 2 then begin bx_pc(1)=bx_lc1 &  bx_pl(1)=bx_l2 & endif

		    bb=sl_tvline(bx_pc,bx_pl,2,0,-1)
		    if  bx_fl(3) gt 0 then for k=0,bx_fl(3)-1 $
		             do bb=sl_tvline(bx_pc+arev(k,9),bx_pl+arev(k,10),2,0,-1)
		    endif
		if (bx_fl(0) eq 1) or (bx_fl(0) eq 3) then begin
		    bx_pc(0)=bx_c1  &  bx_pl(0)=bx_cl1
		    bx_pc(1)=bx_c1  &  bx_pl(1)=bx_cl2

		    if bx_fl(0) eq 1 then begin bx_pc(1)=bx_c2 &  bx_pl(1)=bx_cl1 & endif

		    bb=sl_tvline(bx_pc,bx_pl,2,0,-1)
		    if  bx_fl(3) gt 0 then for k=0,bx_fl(3)-1 $
		             do bb=sl_tvline(bx_pc+arev(k,9),bx_pl+arev(k,10),2,0,-1)
		    bx_pc(0)=bx_c2  &  bx_pc(1)=bx_c2

		    if bx_fl(0) eq 1 then begin bx_pc(1)=bx_c1 &  bx_pl(0)=bx_cl2
								  bx_pl(1)=bx_cl2 & endif

		    bb=sl_tvline(bx_pc,bx_pl,2,0,-1)
		    if  bx_fl(3) gt 0 then for k=0,bx_fl(3)-1 $
		             do bb=sl_tvline(bx_pc+arev(k,9),bx_pl+arev(k,10),2,0,-1)
		    endif
		if (bx_fl(0) eq 2) or (bx_fl(0) eq 3) then begin
		    bx_pc(0)=bx_lc1 &  bx_pl(0)=bx_l2
		    bx_pc(1)=bx_lc2 &  bx_pl(1)=bx_l2

		    if bx_fl(0) eq 2 then begin bx_pc(0)=bx_lc2 &  bx_pl(1)=bx_l1 & endif

		    bb=sl_tvline(bx_pc,bx_pl,2,0,-1)
		    if  bx_fl(3) gt 0 then for k=0,bx_fl(3)-1 $
		             do bb=sl_tvline(bx_pc+arev(k,9),bx_pl+arev(k,10),2,0,-1)
		    endif
return
end
;
;
;
function sl_settings,	mfi,xu,yu,zerr,inc,flg
;******* ***********
;**
common tmp_settings,	bb,i,j,j1,jm,plc,tmp,w_cw,m4
;**
common my_tv,	 tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		 tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_glor,	  f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		  f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
	plc	 =36
	j	 =0
	j1	 =0
	jm	 =0
	m4	 =3
	while j   ne 100  do begin
;**
;**care tmp
	 tmp	 =exph(3)
	 bb=sl_sti(tmp,sl_str(f_fg(7),'(i3)'),plc)
	 exph(3) =tmp
	 tmp	 =expf(8)
	 bb=sl_sti(tmp,sl_str(f_fg(1),'(i4)'),plc-5)
	 bb=sl_sti(tmp,sl_str(f_fg(2),'(i4)'),plc)
	 expf(8) =tmp
	 if flg lt 0 then begin if m4 ne 4  then bb=sl_tvmcur(2,xu,yu)
	 			if jm eq 0  then begin
					    j =sl_tvmenul(0,m4,expf,ex_f,-2.,-2.)
					    if j lt 0 then begin j=100 & j1=-1
					    endif else j1=exz(j,1)
				endif	    else begin
					    j =sl_tvmenul(0,m4,exph,ex_f,-2.,-2.)
					    if j lt 0 then begin j=100 & j1=-1
					    endif else j1=exz(j,2)
				endelse
				m4=4
	 endif	     else begin j1=0 & i=0 & j=0
				while (exz(i,1) ge 0) and (j1 eq 0) do begin
					if (exz(i,1) eq flg) or $
					   (exz(i,2) eq flg) then begin
							     j=i & j1=-1 & endif
					i= i+1 & endwhile
				j1=flg
				endelse
;**Representation
	 if (j1 le 7) and (j1 ge 0) then begin
				sl_stron,expf,f_vu,j1,plc,plc,'    ','> on'
				f_fg(47)=0
				if (j1 eq 7) and (f_vu eq 7) then f_vu=3 $
			 	else f_vu = j1 &  endif
;**
	 case j1 of
;**Tidy
	0: if f_ic ne 0 then begin
			     bb=sl_tvget(3,w_cw)
			     bb=sl_tvsel(f_w1)
			     if bb   eq 1 then   bb=sl_tvpop(f_w1,0)
			     if w_cw ne 0 then   bb=sl_tvsels(w_cw)
			     f_sh=1 & f_ic=0   & end
;**Viewfinder size
	8: begin	 i=2
			 f_fg(1)=sl_click(i,mfi(0),f_fg(1),7,-1)
			 f_fg(2)=f_fg(1)*f_wy/f_wp
			 f_fg(2)=sl_click(i,mfi(1),f_fg(2),8,-1) & end
;**Rescale
	9: begin	 tmp	=expf(j)
			 if f_fg(5)  then bb=sl_sti(tmp,'>off',plc) $
				     else bb=sl_sti(tmp,'> on',plc)
			 expf(j)=tmp
			 f_fg(5)=sl_tog(f_fg(5)) & end
;**Integrate
	10:	begin	 tmp	=expf(j)
			 if f_fg(13)  then sl_box,1
			 if f_fg(13)  then bb=sl_sti(tmp,'>off',plc) $
				      else bb=sl_sti(tmp,'> on',plc)
			 expf(j)=tmp
			 f_fg(13)=sl_tog(f_fg(13))
			 bb=sl_glory(0)
			 end
;**Size G_H
	11:begin	 tmp	=expf(j)
			 if f_fg(3) eq 1  then  begin f_wy=f_wy/4
						bb=sl_sti(tmp,' 1/3',plc)
						f_fg(3)=0 & endif else $
			 if f_fg(3) eq 0  then  begin f_wy=f_wy*8
						if f_wy gt tv_y then f_wy=tv_y
						bb=sl_sti(tmp,' 3/3',plc)
						f_fg(3)=2 & endif else $
			 if f_fg(3) eq 2  then  begin f_wy=f_wy/2
						bb=sl_sti(tmp,' 2/3',plc)
						f_fg(3)=1 & endif
			 expf(j)=tmp
			 bb=sl_glory(0)
			 end
;**More
	12:		 begin jm=1 & m4=3 & end
;**Smooth
	13: if f_fg(37) eq 1  then begin
			 f_fg(35)=3
;			 f_fg(6)=2
			 tmp	=expf(j)
			 if f_fg(12) eq 1 then bb=sl_sti(tmp,'>off',plc) $
				          else bb=sl_sti(tmp,'> on',plc)
			 expf(j)=tmp
			 if f_fg(12) eq 1 then f_fg(12)=0 $
					  else f_fg(12)=1
			 endif
;**Border
	14: if f_fg(37) eq 1  then begin
			 f_fg(35)=3
			 f_fg(6)=2
			 tmp	=expf(j)
			 if f_fg(4)  then bb=sl_sti(tmp,'>off',plc) $
				     else bb=sl_sti(tmp,'> on',plc)
			 expf(j)=tmp
			 f_fg(4)=sl_tog(f_fg(4)) & endif
;**Square  off
	15: if f_fg(37) eq 1  then begin
			 f_fg(35)=3
			 f_fg(6)=2
			 tmp	=expf(j)
			 if f_fg(10) then bb=sl_sti(tmp,'>off',plc) $
				     else bb=sl_sti(tmp,'> on',plc)
			 expf(j)=tmp
			 f_fg(10)=sl_tog(f_fg(10)) & endif
;**Save data
	16:begin	 tmp	=expf(j)
			 if f_fg(17)  then bb=sl_sti(tmp,'>off',plc) $
				      else bb=sl_sti(tmp,'> on',plc)
			 expf(j)=tmp
			 f_fg(17)=sl_tog(f_fg(17))  & end
;**Log dispay
	17: if (inc eq 18) or (f_fg(37) ne 0) then begin
			 f_fg(35)=3
;			 f_fg(6)=2
			 rvm=rvl
			 tmp	=expf(j)
			 if f_fg(0) eq 1 then bb=sl_sti(tmp,'>off',plc) $
					 else bb=sl_sti(tmp,'> on',plc)
			 expf(j)=tmp
			 if f_fg(0) eq 1 then f_fg(0)=-1 else f_fg(0)=1
	      endif
;**Vectors
	18:   begin
		if f_fg(47) ne 0 then f_fg(47)=0 $
		else begin  if f_vu eq 4 then f_fg(47)=4
			    if f_vu eq 5 then f_fg(47)=5 & endelse
	      end
;**Return
	19:		 j=100
;**Sections
	20:   begin	 tmp	=exph(j)
			 if f_fg(8) eq 1 then bb=sl_sti(tmp,'>off',plc) $
				         else bb=sl_sti(tmp,'> on',plc)
			 exph(j)=tmp
			 tmp = exph(j+1)
			 bb=sl_sti(tmp,'    ', plc)
			 exph(j+1)=tmp
			 if f_fg(8)  ne 1 then f_fg(8)=1 else f_fg(8)=0	& end
	21:   begin	 tmp	=exph(j)
			 if f_fg(8) eq 2 then bb=sl_sti(tmp,'>off',plc) $
				         else bb=sl_sti(tmp,'> on',plc)
			 exph(j)=tmp
			 tmp = exph(j-1)
			 bb=sl_sti(tmp,'    ', plc)
			 exph(j-1)=tmp
			 if f_fg(8)  ne 2 then f_fg(8)=2 else f_fg(8)=0	& end
;**Projection
	22:   begin	 tmp	=exph(j)
			 if f_fg(9)   then bb=sl_sti(tmp,'>off',plc) $
				      else bb=sl_sti(tmp,'> on',plc)
			 exph(j)=tmp
			 f_fg(9) =sl_tog(f_fg(9))  & end
;**Enhance
	23:	begin	 f_fg(7) = f_fg(7)+1	   & end
;**Remove enhancmt
	24:	begin	 f_fg(7) = 0		   & end
;**Scale separate frames
	25: if f_fg(37) eq 1  then begin
			 f_fg(35)=3
			 tmp =expf(j)
			 if f_fg(22) then bb=sl_sti(tmp,'>off',plc) $
				     else bb=sl_sti(tmp,'> on',plc)
			 expf(j)=tmp
			 f_fg(22)=sl_tog(f_fg(22)) & endif
;**Rotate other angle
	26:		 f_fg(18)=sl_tog(f_fg(18))
;**Hard copy
	27:	begin	 i =0
			 m4=3
			 bb=sl_tvmcur(2,xu,yu)
			 while  i lt 100 do begin
				j =sl_tvmenul(0,m4,exphc,'Scan:print options',-2.,-2.)
				if j lt 0    then i=100 else i =exz(j,5)
				if i lt 99   then begin
				  if i lt 50 then begin
				    sl_stron,exphc,f_fg(33),j,26,26,'  ','**'
			 	    f_fg(33)=i  & endif
				  if i eq 51 then begin
				     f_fg(51)=sl_tog(f_fg(51))
				     if f_fg(51) eq 1 then $
					  sl_stron,exphc,j,j,26,26,'  ','**' $
				     else sl_stron,exphc,j,j,26,26,'  ','  '
				  endif
				endif
				m4=4
			 endwhile
			 if flg  ge 0 then bb=sl_tvdmenu(0)
			 inc=inc+50 & zerr=1
			 j=100
		end
;**Smooth in G.H.
	28:		 if f_fg(12) eq 0 then f_fg(12)=2 else $
			 if f_fg(12) eq 2 then f_fg(12)=0
;**Logarithm in G.H.
	29:		 if f_fg(0)  le 0 then f_fg(0) =2 else $
			 if f_fg(0)  eq 2 then f_fg(0) =-1
	else:
	endcase
	if flg ge 0 then j =100
	endwhile
	if flg lt 0 then bb=sl_tvdmenu(0)
return,1
end
;
;
;
function sl_trsig, zerr,inc,vin,mfi,ot,rti
;******* ********  **** *** *** *** ** ***
;**
common my_trsig,  bb,dirc,i,n,u,tr_v2,tr_v3
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
		io_cur,io_ext,io_seq,io_str
;**
common my_viewr, bxy
;**
	common  my_insert,  i_txt,i_idx,i_fil,i_rout,i_ps,i_rs,$
			    i_trout,i_tfil,i_tlang,i_tdx,i_enter,i_rcall
;**
	n   =rti
	rti =0
;**
	if (n ge 40) and (n le 69) then begin if n eq 47 then ot=1 else ot=2
			    bb=sl_settings(mfi,0,0,zerr,inc,n-40) & endif else $
		  case  n of
;**	Help
		  1:	begin
			i=zerr & if inc lt 50 then i=i+10*inc
			dirc=sl_stbr(io_dir,2)	+sl_stbr(io_ext(5),0) $
						+sl_stbr(sl_str(i,'(i3)'),1)
			u=sl_filr(dirc,io_ext(10),1,dirc,1)
			if u gt 0 then  begin
			     i=f_ib-1
			     tr_v2(0)=f_ib & tr_v2(1)=1
			     tr_v3(0)=2    & tr_v3(1)=80 & tr_v3(2)=f_ib
			     while i ne 0  do begin
			  	bb      =sl_cellget(u,tr_v3,tr_v2,f_h1,1)
				while (i gt 0) and (f_h1(i) eq f_h2(0)) do i=i-1
				if i gt 0 then begin f_h3=[f_h1(0:i),f_h2]
					       bb=sl_tvmenuc(0,0,f_h3,' ',9.,9.)
					       if bb eq i+2 then i=f_ib-1 $
							    else i=0
					       ot=2 & inc=inc+50 & zerr=1
				endif
			     endwhile & bb=sl_iofree (u)
			endif
			end
;**	Insert
		  4 :	if zerr+10*inc  eq 180  then  zerr=9   else $
	        	if zerr+inc eq 0   then begin zerr=1 & vin=20
			endif else bb=sl_handerr(4)
;**	Remove
		  5:	if zerr+inc eq 0   then begin zerr=1 & vin=19
			endif else bb=sl_handerr(4)
;**	Select
		  6:	if zerr+inc eq 0   then begin zerr=1 & vin=8
			endif else bb=sl_handerr(4)
;**	Frame ops (care vin position in menu)
;	     or take sum "+"
;	     or get reflex
;	     or add degree
		  16:	if zerr+10*inc  eq 130  then  begin
					zerr=3    &   f_fg(25)=3 & endif else $
			if zerr+10*inc  eq 160  then  zerr=6   else $
			if zerr+10*inc  eq 210  then  zerr=8   else $
			if zerr+inc eq 0   then begin zerr=9 & vin=11
			endif else bb=sl_handerr(4)
;**	     Upper angle
;	     or Shift ">"
		  18:	if zerr+10*inc  eq 220  then  begin
						      zerr=5 & f_fg(23)=2
						      endif    else $
			if zerr+inc eq 0   then begin zerr=9 & vin=8
			endif else bb=sl_handerr(4)
;**	     back one "-"
;	     or sub degree
		  19:	if zerr+10*inc  eq 130  then  zerr=9   else $
			if zerr+10*inc  eq 160  then  zerr=7   else $
			if zerr+10*inc  eq 210  then  zerr=9   else $
			if zerr+inc eq 0   then begin zerr=9 & vin=12	& endif
;**	Menus
		  20:	if zerr+inc eq 0   then begin zerr=1 & vin=10	& endif
		  21:	if zerr+inc eq 0   then begin zerr=1 & vin=11	& endif
		  22:	if zerr+inc eq 0   then begin zerr=1 & vin=12	& endif
		  23:	if zerr+inc eq 0   then begin zerr=1 & vin=13	& endif
		  28:	if zerr+inc eq 0   then begin zerr=1 & vin=14	& endif
;**	Mouse
		  24:	zerr=4
		  25:	zerr=2
		  26:	zerr=1
		  27:	if zerr+10*inc  eq 170  then  zerr=2 else $
			if zerr+10*inc  eq 180  then  zerr=6 else $
			if zerr+10*inc  eq 181  then  zerr=8 $
				  else begin  inc=0 & zerr=3 & endelse
;**	Break  ^B
		 140:	if zerr+inc eq 0   then begin zerr=1 & vin=20
						      f_fg(42)=1        & endif
;**	Menu
		 141:	if zerr+inc eq 0   then       zerr=1
;**	Recall ^R
		 142:	if f_fg(37) eq 1 then f_fg(42)=2
;**	Control Y,Z
		 131:	bb=sl_x('focus_clear')
;**	Colors  C
		  30:	if zerr+inc eq 0   then begin zerr=1 & vin=0
			endif  else begin  sl_manycol,-1 & inc=inc+50
						  zerr=1 & ot=2 & endelse
;**	Rescale R
		  31:	if (zerr+inc eq 0) and (f_fg(41) ne 1)  then begin
						  zerr=1 & vin=2
			endif  else  if inc eq 3  then zerr=2 $
			       else  if f_fg(37)  eq 1 then f_fg(40)=3
;**	Slice   /
		  83:	if inc eq 14 then zerr=3 $
		  	else if inc ne 19 then begin
			bxy(8)=-1
			if zerr+inc eq 0   then begin zerr=1 & vin=4
			endif  else if  f_fg(24) ne 1 then begin
					f_fg(24)=1 & f_fg(29)=1
			endif  else	f_fg(24)=0
			endif  else bb=sl_handerr(4)
;**	Un-Expand "E"
		  84:	if (zerr eq  0) and (f_fg(37) ne 0) then f_fg(35)=1 $
			else bb=sl_handerr(4)
;**	Quick expand "z"
		  87:	if (zerr eq  0) and (f_fg(37) ne 0) then f_fg(35)=2 $
			else bb=sl_handerr(4)
;**	area for FFT "j"
		  89:	if  zerr+inc eq 0  then begin zerr=1 	 & vin=89 & endif
;**	Graphics
;	"I"
		  29:	if (inc eq 18) or  $
			   (f_fg(37) eq 1) then begin f_fg(35)=4 & vin=6  & endif
;	"P"
		  32:	if  zerr+inc eq 0  then begin zerr=5 	 & vin=2  & endif
;	"G"
		  34:	if  f_fg(37) ne 0  then begin f_fg(35)=4 & vin=14 & endif
;	"L"
		  35:	if (inc eq 18) or  $
			   (f_fg(37) eq 1) then begin f_fg(35)=4 & vin=0  & endif
;	"S"
		  36:	if (inc eq 18) or  $
			   (f_fg(37) eq 1) then begin f_fg(35)=4 & vin=1  & endif
;	"T"
		  37:	if  f_fg(37) ne 0  then begin f_fg(35)=4 & vin=20 & endif
;	"V"
		  80:	if  f_fg(37) eq 1  then begin f_fg(35)=4 & vin=7  & endif
;	"M"
		  81:				begin f_fg(35)=4 & vin=9  & end
;	"@"
 		  82:	if zerr+inc eq 0   then begin zerr=5	 & vin=15 & endif
;**	"A"
		  86:	if (inc eq 18) or  $
			   (f_fg(37) eq 1) then begin f_fg(35)=4 & vin=16 & endif
;**	Back to B (care vin position in menu)
		  38:	if zerr+inc eq 0   then begin zerr=7	 & vin=16 & endif
;**	Reduce  K (care vin position in menu)
		  39:	if zerr+inc eq 0   then begin zerr=7	 & vin=1  & endif
;**	Extract W (care vin position in menu)
		  85:	if zerr+inc eq 0   then begin zerr=7	 & vin=6  & endif
;**	Project Z (care vin position in menu)
		 106:	if zerr+inc eq 0   then begin zerr=9	 & vin=0  & endif
;**	Project Y (care vin position in menu)
		 107:	if zerr+inc eq 0   then begin zerr=9	 & vin=4  & endif
;**	Project X (care vin position in menu)
		 108:	if zerr+inc eq 0   then begin zerr=9	 & vin=3  & endif
;**	Up down U (care vin position in menu)
		 109:	if zerr+inc eq 0   then begin zerr=9	 & vin=9  & endif
;**	Fit     F (care vin position in menu)
		 112:	if zerr+inc eq 0   then begin zerr=7	 & vin=9  & endif
;**	Quit    Q
		 110:	if zerr+inc eq 0   then       zerr=4     $
			else bb=sl_handerr(4)
;**	Save    H
		 111:	if zerr+inc eq 0   then begin zerr=1	 & vin=16
			endif else bb=sl_handerr(4)
;**	Duplic  D
		 113:	if zerr+inc eq 0   then begin zerr=1	 & vin=17
			endif else bb=sl_handerr(4)
;**	set low [
		 114:	if zerr+10*inc  eq 30   then  begin
					zerr=4    &   f_fg(48)=1 & endif else $
			if f_fg(37)	eq 1    then  f_fg(40)=1
;**	set hig ]
		 115:	if zerr+10*inc  eq 30   then  begin
					zerr=1    &   f_fg(48)=1 & endif else $
			if f_fg(37)	eq 1    then  f_fg(40)=2
;**	Adjust    "\"
		 116:	f_fg(36)=1
;**	Min point "<"
;	or Lower angle
		  17:	if zerr+10*inc  eq 150  then  zerr=6 else $
			if zerr+10*inc  eq 220  then  begin
						      zerr=5 & f_fg(23)=1 & endif
;**	Take mean "m"
;**	Average point
;**	mean slice
		  90:	if zerr+10*inc  eq 130  then  begin
					zerr=3    &   f_fg(25)=1 & endif else $
			if zerr+10*inc  eq 150  then  zerr=3 $
						else  f_fg(25)=sl_tog(f_fg(25))
;**	Toggle "#"
		  91:	begin f_fg(27)=sl_tog(f_fg(27))
			ot=2 & end
;**	Radius "%"
;**	Rubber
;**	Clear point
;**	Clear radial mode
;**	Mole
		  92:	if zerr+10*inc  eq 130  then  zerr=5 else $
			if zerr+10*inc  eq 140  then  f_fg(26)=sl_tog(f_fg(26)) else $
			if zerr+10*inc  eq 150  then  zerr=5 else $
			if zerr+10*inc  eq 180  then  zerr=5 else $
			if zerr+10*inc  eq 210  then  zerr=5 else $
			if zerr+inc eq 0   then begin zerr=1 & vin=21	& endif
;**	Pivot point "."
		  93:   if zerr+10*inc  eq 140  then  zerr=4 else f_fg(29)=1
;**	Take  region "*"
;	Record all radial int.
		  94:	if zerr+10*inc  eq 130  then  zerr=7  else $
			if zerr+10*inc  eq 220  then  zerr=8
;**	Center "^"
		  95:	if zerr+10*inc  eq 130  then  zerr=6  else $
		  	if zerr+10*inc  eq 190  then  zerr=3  else $
		  	if zerr+10*inc  eq 210  then  zerr=6  else $
		  	if zerr+10*inc  eq 220  then  zerr=3
;**	Store avg "t"
		  96:	if zerr+10*inc  eq 150  then  zerr=9
;**	Update "u"
		  97:	if zerr+10*inc  eq   0  then  zerr=37 else $
			if zerr+10*inc  eq 130  then  zerr=8  else $
			if zerr+10*inc  eq 140  then  zerr=8  else $
			if zerr+10*inc  eq 150  then  zerr=8  else $
			if zerr+10*inc  eq 160  then  zerr=2  else $
			if zerr+10*inc  eq 180  then  zerr=8  else $
						      bb=sl_handerr(4)
;**	Elliptic "e"
		  98:	begin
			if f_fg(13)  then sl_box,1
			if f_vu gt 3 then begin  f_vu=3 & f_fg(31)=1
			endif	     else f_fg(31)=sl_tog(f_fg(31))
			ot=2
			end
;**	Fix slice ":"
		  99:	f_fg(29)=2
;**	Take reflect ; <Enter>
;**	Take  point ";"
;	Record current radial int.
		  100:  if zerr+10*inc  eq 130  then  begin
					zerr=3    &   f_fg(25)=2  & endif else $
			if zerr+10*inc  eq 210  then  zerr=3 else $
			if zerr+10*inc  eq 220  then  zerr=7
;**	Position maxi "Find"
		  101:	begin f_fg(32)=1 & ot=2 & end
;**	toggle pan "^P"
		  102:  if f_fg(37) ne 0 then if f_fg(38) eq 0  then f_fg(38)=1 $
								else f_fg(38)=0
;**	Reflex coord "f8 f9 f10"
		  103:	if f_fg(37) ne 0 then f_fg(39)=1
		  104: 	if f_fg(37) ne 0 then f_fg(39)=2
		  105:	if f_fg(37) ne 0 then f_fg(39)=3

;**	Lower radius "{"
		  117:	if zerr+10*inc  eq 220  then  begin
					f_fg(23)=1 &  zerr=6  & endif
;**	Upper radius "}"
		  118:	if zerr+10*inc  eq 220  then  begin
					f_fg(23)=2 &  zerr=6  & endif
;**	External F   "k"
		  199:	if f_fg(37) eq 1 then f_fg(42)=9
;**     A number
		  else: if (n ge 200) and (n le 209) then begin
			f_fg(23)=n-200
;**			External F
			if  f_fg(42) ge 9  then begin
			 if f_fg(42) eq 9  then f_fg(42)=f_fg(23)*10 $
			 else begin
				i_rcall=f_fg(42)+f_fg(23)
				f_fg(42)=2
			 endelse
			 f_fg(23)=0
			endif else begin
;**
			 if zerr+10*inc  eq 150  then  zerr=7  else $
			 if zerr+10*inc  eq 160  then  zerr=5  else $
			 if zerr+10*inc  eq 180  then  zerr=7  else $
			 if zerr+10*inc  eq 190  then  zerr=5  else $
			 if f_fg(23)	 eq 9 	 then  begin f_fg(7)=0
					 f_fg(23)=10 & f_el=0. & ot=2  & endif
;			 if f_fg(23) eq 8 then if f_fg(2) lt bxy(3) then begin
;			  		 f_fg(2) =f_fg(2)+1    & ot=2  & endif
;			 if f_fg(23) eq 4 then if f_fg(1) gt 2      then begin
;					 f_fg(1) =f_fg(1)-1    & ot=2  & endif
;			 if f_fg(23) eq 2 then if f_fg(2) gt 2      then begin
;					 f_fg(2) =f_fg(2)-1    & ot=2  & endif
;			 if f_fg(23) eq 6 then if f_fg(1) lt bxy(2) then begin
;					 f_fg(1) =f_fg(1)+1    & ot=2  & endif
			endelse
			endif else  rti=n
		  endcase
return,1
end
;
;
;
function sl_conv, erey,vsiz,typ
;******* *******  **** **** ***
;**
;** Convert to byte fix float long double complex
;**    typ  =  2    4   8     16   32     64

    common my_conv,care ,care_z ,k
;**
	k=0
	if vsiz(vsiz(0)) gt 1 then begin
		if typ eq 2  then erey=byte(erey)
		if typ eq 4  then erey=fix  (erey)
		if typ eq 8  then erey=float (erey)
		if typ eq 16 then erey=long  (erey)
		if typ eq 32 then erey=double (erey)
		if typ eq 64 then erey=complex (erey)
		vsiz(vsiz(0)+1)=typ
	endif else if vsiz(0) eq 1 then begin
		k=1
		bb=sl_psizm(care,care_z,1,vsiz(1),typ,-1,-1,-1)
		care(0)=erey
	endif else if vsiz(0) eq 2 then begin
		k=1
		bb=sl_psizm(care,care_z,2,vsiz(1),vsiz(2),typ,-1,-1)
		care(0,0)=erey
	endif else if vsiz(0) eq 3 then begin
		k=1
		bb=sl_psizm(care,care_z,2,vsiz(1),vsiz(2),vsiz(3),typ,-1)
		care(0,0,0)=erey
	endif
	if k eq 1 then begin
		bb=sl_pp(0,care,care_z,erey,vsiz)
		bb=sl_dd(2,care,care_z)
	endif
return,1
end
;
;
function sl_b_fix, erey,vsiz,typ
;******* ********  **** **** ***
;**
;** Byte --> fix
;**
  if (vsiz(vsiz(0)+1) eq 2) then begin
	typ=4
	bb=sl_conv(erey,vsiz,typ)
  endif
return,1
end
;
;
;
function sl_scalf,	area,vsiz ,mn,mx, mnx ,flg,areout,sby
;******* ********
;**
;**Scale  by  sby.      mnx= 1 means values already 0< >255
;**			flg=-1 means return result
;**			flg= 0 means use    areout
;**			flg= 1 means use    area
;**			flg= 2 means use    area + force byte
;**
	common tmp_scalf, tip,mini,maxi,bid1,manix
bb=1
	if mn eq mx then maxi=sl_maxim(area,vsiz,bid1,mini) $
	else  begin	 maxi=mx &  mini=mn & endelse
;
	tip=vsiz(vsiz(0)+1)
	if maxi gt mini then begin
	   if  sby gt 1 then manix=1.*sby -1.    else manix=255.
	   if  ((mnx   eq 1) or  (tip  eq 2))    and  $
		(mini  eq 0) and (maxi eq manix) then begin
		if flg lt 0 then return  ,       area
		if flg eq 0 then areout(0,0) =   area
	   endif else begin
	    manix=manix/(maxi-mini)
	    if (mn ne mx) and (mnx eq 0) then  begin
		if flg lt 0 then return  ,    ((area > mini < maxi) -mini)*manix
		if flg eq 0 then areout(0,0) =((area > mini < maxi) -mini)*manix
		if flg gt 0 then area(0,0)   =((area > mini < maxi) -mini)*manix
	    endif else begin
		if flg lt 0 then return  ,     (area-mini)  *manix
		if flg eq 0 then areout(0,0) = (area-mini)  *manix
		if flg gt 0 then area(0,0)   = (area-mini)  *manix
	    endelse
	   endelse
	endif
;
	if flg eq 2 then if tip ne 2 then bb=sl_conv(area,vsiz,2)
return, bb
end
;
;
;
;
function sl_d_p1,  j,erey,vsiz,dif,stc ,x1,y1
;******* ******    * **** **** *** ***  ** **
;**
;**
	common tmp_dp,	bb,cpx,his,n,nk,nl,nz,typ,fval
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
	common my_ovs,	sum1   ,sum2   ,sum3   ,sum4   ,sum5   ,sum6   ,sum7,$
			ovs1_z ,ovs2_z ,ovs3_z ,ovs4_z ,ovs5_z ,ovs6_z ,$
			ov_pmx,ov_f,ov_z,ov_l,ov_m
;**
ab=1
    case j of
;** Rescale.
;** -------
    0:	if dif eq 0   then $
	   if not cpx then if nz gt 1 then erey(0,0,0)=erey  > x1 <y1 $
				      else erey(0,0)  =erey  > x1 <y1 $
	   else begin      erey(where(float(erey) lt x1 ))=complex(x1 )
			   erey(where(float(erey) gt y1 ))=complex(y1 )
			   endelse  else $
  	if  dif eq 1  then $
	   if not cpx then if nz gt 1 then erey(0,0,0)=erey  > x1     $
				      else erey(0,0)  =erey  > x1     $
	   	      else erey(where(float(erey) lt x1 ))=complex(x1 )   else $
	if  dif eq 2  then $
	   if not cpx then if nz gt 1 then erey(0,0,0)=erey  < y1     $
				      else erey(0,0)  =erey  < y1     $
		      else erey(where(float(erey) gt y1 ))=complex(y1 )   else $
	if  dif eq 3  then $
	   if not cpx then erey(where(      (erey  lt x1 ) or (erey gt y1 )))=0 $
		      else erey(where((float(erey) lt x1 ) or  $
				      (float(erey) gt y1 )))=complex(0)   else $
	if  dif eq 4  then $
	   if not cpx then erey(where(       erey  lt x1 )) =0 $
	   	      else erey(where( float(erey) lt x1 )) =complex(0)   else $
	if  dif eq 5  then $
	   if not cpx then erey(where(       erey  gt y1 )) =0 $
		      else erey(where( float(erey) gt y1 )) =complex(0)   else $
	if  dif eq 6  then $
	   if not cpx then erey(where(      (erey  gt x1 ) and (erey lt y1 )))=0 $
		      else erey(where((float(erey) gt x1 ) and $
				      (float(erey) lt y1 )))=complex(0)   else $
	if  dif eq 7  then $
	   if not cpx then erey(where(       erey  gt x1 )) =0 $
	   	      else erey(where( float(erey) gt x1 )) =complex(0)   else $
	if  dif eq 8  then $
	   if not cpx then erey(where(       erey  lt y1 )) =0 $
		      else erey(where( float(erey) lt y1 )) =complex(0)   else $
	if  dif eq 10 then $
	   if not cpx then erey(where(       erey  eq 0  )) =x1 $
		      else erey(where( float(erey) eq 0  )) =complex(x1)  else $
	if  dif eq 11 then $
	   if not cpx then erey(where(       erey  ne x1 )) =y1 $
		      else erey(where( float(erey) ne x1 )) =complex(y1)
;** X Derivative.
;** - ----------
    20:	if vsiz(1) gt 1 then begin
	bb=sl_b_fix(erey,vsiz,typ)
	if nz eq 1 then erey(0,0)  =erey-sl_shift(erey,vsiz(1),vsiz(2),typ,-1,0)$
	else begin
		bb=sl_psizm(sare,sare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
		   for l=0,nz-1 do begin
			sare(0,0)  =erey(*,*,l)
			erey(0,0,l)=sare-sl_shift(sare,vsiz(1),vsiz(2),typ,-1,0)
		   endfor
		bb=sl_dd(2,sare,sare_z)
	endelse
	end
;** Y Derivative.
;** - ----------
    21:	if vsiz(2) gt 1 then begin
	bb=sl_b_fix(erey,vsiz,typ)
	if nz eq 1 then erey(0,0)  =erey-sl_shift(erey,vsiz(1),vsiz(2),typ,0,-1)$
	else begin
		bb=sl_psizm(sare,sare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
		   for l=0,nz-1 do begin
			sare(0,0)  =erey(*,*,l)
			erey(0,0,l)=sare-sl_shift(sare,vsiz(1),vsiz(2),typ,0,-1)
		   endfor
		bb=sl_dd(2,sare,sare_z)
	endelse
	end
;** Gradient.
;** --------
    22:	if (vsiz(1) gt 1)  and  (vsiz(2) gt 1) then begin
	bb=sl_b_fix(erey,vsiz,typ)
	bb=sl_psizm(tare,tare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
	bb=sl_psizm(vare,vare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
	if nz eq 1 then begin
			tare(0,0)=erey-sl_shift(erey,vsiz(1),vsiz(2),typ,-1, 0)
			bb=sl_abs(tare,tare,nl,typ,2)
			vare(0,0)=erey-sl_shift(erey,vsiz(1),vsiz(2),typ, 0,-1)
			bb=sl_abs(vare,vare,nl,typ,2)
			erey(0,0)=tare+vare
	endif else begin
		   bb=sl_psizm(sare,sare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
		   for l=0,nz-1 do begin
			sare(0,0)=erey(*,*,l)
			tare(0,0)=sare-sl_shift(sare,vsiz(1),vsiz(2),typ,-1, 0)
			bb=sl_abs(tare,tare,nk,typ,2)
			vare(0,0)=sare-sl_shift(sare,vsiz(1),vsiz(2),typ, 0,-1)
			bb=sl_abs(vare,vare,nk,typ,2)
			erey(0,0,l)=tare+vare
		   endfor
		   bb=sl_dd(2,sare,sare_z)
	endelse
	bb=sl_dd(2,tare,tare_z)
	bb=sl_dd(2,vare,vare_z)
	endif
;** Standard deviation over frames.
;** -------- --------- ---- ------
    23: if nz gt 1 then begin
	   bb=sl_psizm(ares,ares_z,2,vsiz(1),vsiz(2),8,-1,-1)
	   bb=sl_psizm(sare,sare_z,2,vsiz(1),vsiz(2),8,-1,-1)
	   bb=sl_fsum(erey,2,vsiz,sare)
	   sare(0,0)=sare/nz
	   for l =0,nz-1 do begin
		ares(0,0) =(erey(*,*,l)-sare) ^ 2 + ares
		endfor
	   bb=sl_dd(2,sare,sare_z)
	   ares(0,0) =ares/(nz-1)
	   bb=sl_sqrt(ares, nk,2)
	   endif
;** X mixed salad.
;** - ----- -----
    24:	if vsiz(0) eq 3 then begin
	bb=sl_psizm(ares,ares_z,3,vsiz(1),vsiz(3),vsiz(2),typ,-1)
	for k1=0,vsiz(2)-1 do $
	for k2=0,nz-1	   do ares(0,k2,k1)=erey(*,k1,k2)
	endif
;** Y mixed salad.
;** - ----- -----
    25:	if vsiz(0) eq 3 then begin
	bb=sl_psizm(ares,ares_z,3,vsiz(3),vsiz(2),vsiz(1),typ,-1)
	for k1=0,vsiz(1)-1 do $
	for k2=0,nz-1	   do ares(k2,0,k1)=erey(k1,*,k2)
	endif
;** Sum over frames.
;** --- ---- ------
    26: begin
	if  typ eq 2 then k=4 else k=8
	if (nz gt 1) and (vsiz(12) ne 0) then $
		     bb=sl_psizm(ares,ares_z,2,vsiz(10)+1,vsiz(11)+1,k,-1,-1) $
	else         bb=sl_psizm(ares,ares_z,2,vsiz(10)+1,	   1,k,-1,-1)
	if nz eq 1 then begin
	   if vsiz(11) ne 0 then if not dif then bb=sl_tsum(erey,1,2,ares) else $
	      bb=sl_tsum(erey(vsiz(7):vsiz(7)+vsiz(10) ,$
				    vsiz(8):vsiz(8)+vsiz(11)),1   ,2,ares)
	endif else $
	   if vsiz(12) ne 0 then begin
	      if not dif then  bb=sl_fsum(erey,2,vsiz,ares) else $
	      bb=sl_tsum (erey(vsiz(7):vsiz(7)+vsiz(10) ,$
				     vsiz(8):vsiz(8)+vsiz(11) ,$
				     vsiz(9):vsiz(9)+vsiz(12)),2  ,2,ares)
	   endif else begin
	      bb=sl_tsum (erey(vsiz(7):vsiz(7)+vsiz(10) ,$
				     vsiz(8):vsiz(8)+vsiz(11),vsiz(9)),1,2,ares)
	   endelse
	end
;** Sum each frames.
;** --- ---- ------
    27: if vsiz(12)  gt 0 then begin
	   bb=sl_psizm(ares,ares_z,2,vsiz(12)+1,1,8,-1,-1)
	   for l=0,vsiz(12) do  $
	     if not dif then ares(l,0)=sl_totf(erey(*,*,l),vsiz(1),vsiz(2),typ)$
			else ares(l,0)=sl_totf(erey(vsiz(7):vsiz(7)+vsiz(10) , $
					  vsiz(8):vsiz(8)+vsiz(11),vsiz(9)+l), $
					  vsiz(10)+1,vsiz(11)+1,typ)
	   endif
;** Transpose x y.
;** --------- ---
    28:	if vsiz(0) eq 3 then begin
	   bb=  sl_psizm(ares,ares_z,3,vsiz(2),vsiz(1),vsiz(3),typ,-1)
	   for  k1=0,vsiz(3)-1 do $
		ares(0,0,k1)=sl_transp(erey(*,*,k1),vsiz(1),vsiz(2),typ)
	endif   else begin
	   bb=  sl_psizm(ares,ares_z,2,vsiz(2),vsiz(1),typ,-1,-1)
		ares(0,0)   =sl_transp(erey   ,vsiz(1),vsiz(2),typ)
	endelse
;** Peaks only.
;** ---- ----
    29:	if (vsiz(1) gt 1)    and  (vsiz(2) gt 1) then begin
	bb=sl_b_fix(erey,vsiz,typ)
	bb=sl_psizm(tare,tare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
	bb=sl_psizm(vare,vare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
	if nz eq 1  then     begin
	      tare(0,0)=((erey- sl_shift(erey,vsiz(1),vsiz(2),typ,-1, 0)) gt 0) $
		       *((erey- sl_shift(erey,vsiz(1),vsiz(2),typ,+1, 0)) gt 0)
	      vare(0,0)=((erey- sl_shift(erey,vsiz(1),vsiz(2),typ, 0,-1)) gt 0) $
		       *((erey- sl_shift(erey,vsiz(1),vsiz(2),typ, 0,+1)) gt 0)
	      erey(0,0)=  erey * ((tare+vare )  gt 0)
	endif else begin
	  bb=sl_psizm(sare,sare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
	  for l=0,nz-1 do begin
	      sare(0,0)=  erey (*,*,l)
	      tare(0,0)=((sare- sl_shift(sare,vsiz(1),vsiz(2),typ,-1, 0)) gt 0) $
		       *((sare- sl_shift(sare,vsiz(1),vsiz(2),typ,+1, 0)) gt 0)
	      vare(0,0)=((sare- sl_shift(sare,vsiz(1),vsiz(2),typ, 0,-1)) gt 0) $
		       *((sare- sl_shift(sare,vsiz(1),vsiz(2),typ, 0,+1)) gt 0)
	      erey(0,0,l)=sare * ((tare+vare )  gt 0)
	  endfor
	  bb=sl_dd(2,sare,sare_z)
	endelse
	bb  =sl_dd(2,tare,tare_z)
	bb  =sl_dd(2,vare,vare_z)
	endif
;** Logarithm N.
;** -----------
    30:	begin if x1 le 0 then $
	          if vsiz(0) eq 2	then  erey(0,0) =erey > 1 $
					else  erey	=erey > 1
	      if (typ le 16) and (typ ne 8) then   begin
	      typ=8  & bb=sl_conv(erey,vsiz,typ) & endif
	      bb=sl_logn(erey,vsiz)
;	      if vsiz(0) eq 2	then erey(0,0)=sl_logn(erey,vsiz) $
;				else erey     =sl_logn(erey,vsiz)
	end
;** Exponential N.
;** -------------
    31:	begin
	      if (typ le 16) and (typ ne 8) then   begin
	      typ=8  & bb=sl_conv(erey,vsiz,typ) & endif
	      bb=sl_expn(erey,vsiz)
;	      if vsiz(0) eq 2	then erey(0,0)=sl_expn(erey,vsiz) $
;				else erey     =sl_expn(erey,vsiz)
	end
;** Square-root.
;** -----------
    32: begin
	      if (typ le 16) and (typ ne 8) then   begin
	      typ=8  & bb=sl_conv(erey,vsiz,typ) & endif
	      bb=sl_sqrt(erey,nl,vsiz(0))
	end
;** Square.
;** ------
    33: begin if typ le 4  then begin typ=16 & bb=sl_conv(erey,vsiz,typ)
	      endif  else  $
	      if typ eq 16 then begin typ=8  & bb=sl_conv(erey,vsiz,typ)
	      endif
	      if vsiz(0) eq 2	then  erey(0,0) =erey*erey $
				else  erey	=erey*erey
	end
;** 1/z.
;** ---
    34: begin if (float(x1 ) le 0) and (float(y1 ) ge 0) then $
	          if vsiz(0) eq 2	then  erey(0,0) =erey > 1 $
					else  erey	=erey > 1
	      if (typ le 16) and (typ ne 8) then   begin
	      typ=8  & bb=sl_conv(erey,vsiz,typ) & endif
	      if vsiz(0) eq 2	then  erey(0,0) =1./erey $
				else  erey	=1./erey
	end
;** z/n.
;** ---
    35: begin    n = sl_click(1,300 ,nz,9,0) & ab=0
	      if n gt 1 then if nz eq 1 then erey(0,0  ) = erey / n $
					else erey(0,0,0) = erey / n
	end
;** z*n.
;** ---
    36: begin    n = sl_click(1,300 ,nz,9,0) & ab=0
	   if n gt 1    then begin
	      bb=sl_b_fix(erey,vsiz,typ)
	      if vsiz(0) eq 2	then  erey(0,0) =erey*n $
				else  erey	=erey*n
	   endif
	end
;** Magnitude.
;** ---------
    37:	begin	bb=sl_abs(erey, erey,nl,typ,vsiz(0))
		if typ eq 64 then begin typ=8 & vsiz(vsiz(0)+1)=typ & endif
		end
;** Convert type.
;** ------- ----
    38: begin	if y1-x1 gt 1 then n=1 else n=0
		ab=0
		if (stc(0) eq 2) and (typ gt 2) and (n) then begin
;		Byte.
;		----
		   if (x1 ge 0) and (y1 le 255) then bb=sl_conv(erey,vsiz,2) $
		   else if (nz eq 1) then begin
			    bb=sl_scalf(erey,vsiz,x1,y1,0,2,dummy,256)
;			    erey=sl_scale(erey,vsiz(1),vsiz(2),typ,x1,y1)
;			    vsiz(vsiz(0)+1)= 2
		   endif else begin
	   	     bb=sl_psizm(sare,sare_z,3,vsiz(1),vsiz(2),vsiz(3),2,-1)
		     for l=0,nz-1 do $
		     sare(0,0,l)=sl_scale(erey(*,*,l) ,vsiz(1),vsiz(2),typ,x1,y1)
		     bb=sl_pp(0,sare,sare_z,erey,vsiz)
		     bb=sl_dd(2,sare,sare_z)
		   endelse
		   typ =2
		endif else if  (stc(0) eq 4 ) and (typ ne 4 ) and (n) then begin
;		Int2.
;		----
		   typ=4
		   bb =sl_conv(erey,vsiz,typ)
		endif else if  (stc(0) eq 16) and (typ ne 16) and (n) then begin
;		Long.
;		----
		   typ=16
		   bb =sl_conv(erey,vsiz,typ)
		endif else if  (stc(0) eq 8 ) and (typ ne 8 ) then begin
;		Float.
;		-----
		   typ=8
		   bb=sl_conv(erey,vsiz,typ)
		endif else if  (stc(0) eq 32) and (typ ne 32) then begin
;		Double.
;		------
		   typ=32
		   bb =sl_conv(erey,vsiz,typ)
		endif else if  (stc(0) eq 64) and (typ ne 64) then begin
;		Complex.
;		-------
		   typ=64
		   bb =sl_conv(erey,vsiz,typ)
		endif & end
;** Convert type.
;** ------- ----
    39:	begin ab=0
	if  (stc(0) eq 8 ) and (typ ne 4 ) and (y1-x1 gt 1) then begin
;		Int2.
;		----
		   typ=4
		   bb =sl_conv(erey,vsiz,typ)
	endif else if  (stc(0) eq 16) and (typ ne 32) then begin
;		Double.
;		------
		   typ=32
		   bb =sl_conv(erey,vsiz,typ)
	endif  &   end
;** Data Projections.
;** ---- -----------
    40: begin
	if nz eq 1 then begin
	   bb=sl_psizm(sum1,ovs1_z,1,vsiz(11)+1,8,-1,-1,-1)
	   if not  dif then bb=sl_fsum(erey,0,vsiz,sum1) else   $
	      		    bb=sl_tsum(erey(vsiz(7):vsiz(7)+vsiz(10),$
					    vsiz(8):vsiz(8)+vsiz(11)),0,1,sum1)
	   if vsiz(11) ne 0 then begin
	    bb=sl_psizm(sum2,ovs2_z,1,vsiz(10)+1,8,-1,-1,-1)
	    if not dif then bb=sl_fsum(erey,1,vsiz,sum2) else   $
	      		    bb=sl_tsum(erey(vsiz(7):vsiz(7)+vsiz(10),$
					    vsiz(8):vsiz(8)+vsiz(11)),1,1,sum2)
	    endif
	   sum4   =sl_totf(sum1,ovs1_z(1),0,8)
	   sum5	  =sum4
	   sum6	  =sum4
	   sum7	  =sum4
	endif else begin
	 bb=sl_psizm(sum1,ovs1_z,2,vsiz(11)+1,vsiz(12)+1,8,-1,-1)
	 bb=sl_psizm(sum2,ovs2_z,2,vsiz(10)+1,vsiz(12)+1,8,-1,-1)
	 bb=sl_psizm(sum3,ovs3_z,2,vsiz(10)+1,vsiz(11)+1,8,-1,-1)
	 bb=sl_psizm(sum4,ovs4_z,1,ovs1_z(2),8,-1,-1,-1)
	 bb=sl_psizm(sum5,ovs5_z,1,ovs1_z(1),8,-1,-1,-1)
	 bb=sl_psizm(sum6,ovs6_z,1,ovs2_z(1),8,-1,-1,-1)
	 if stc(0) ne 1 then begin
	    if not dif then bb=sl_tsum(erey,0,2,sum1) else   $
			    bb=sl_tsum(erey(vsiz(7):vsiz(7)+vsiz(10),$
			    vsiz(8):vsiz(8)+vsiz(11),vsiz(9):vsiz(9)+vsiz(12)),$
							   0,2,sum1)
	    if not dif then bb=sl_tsum(erey,1,2,sum2) else   $
			    bb=sl_tsum(erey(vsiz(7):vsiz(7)+vsiz(10),$
			    vsiz(8):vsiz(8)+vsiz(11),vsiz(9):vsiz(9)+vsiz(12)),$
							   1,2,sum2)
	    if not dif then bb=sl_tsum(erey,2,2,sum3) else   $
			    bb=sl_tsum(erey(vsiz(7):vsiz(7)+vsiz(10),$
			    vsiz(8):vsiz(8)+vsiz(11),vsiz(9):vsiz(9)+vsiz(12)),$
							   2,2,sum3)
	 endif else begin
	    if not dif then bb=sl_tsum(erey(vsiz(1)-1,*,*),0,2,sum1) else   $
			    bb=sl_tsum(erey(vsiz(7)+vsiz(10), $
			    vsiz(8):vsiz(8)+vsiz(11),vsiz(9):vsiz(9)+vsiz(12)),$
							   0,2,sum1)
	    if not dif then bb=sl_tsum(erey(*,vsiz(2)-1,*),1,2,sum2) else   $
			    bb=sl_tsum(erey(vsiz(7):vsiz(7)+vsiz(10),$
			    	    vsiz(8)+vsiz(11),vsiz(9):vsiz(9)+vsiz(12)),$
							   1,2,sum2)
	    if not dif then sum3(0,0)=erey(*,*,0) else   $
			    sum3(0,0)=erey(vsiz(7):vsiz(7)+vsiz(10),$
			    		   vsiz(8):vsiz(8)+vsiz(11),vsiz(9))
	 endelse
	 bb =sl_fsum  (sum1,0,ovs1_z,sum4)
	 bb =sl_fsum  (sum1,1,ovs1_z,sum5)
	 bb =sl_fsum  (sum2,1,ovs2_z,sum6)
	 sum7    =sl_totf (sum4,ovs4_z(1),0,8)
	endelse & end
    else:
    endcase
return,ab
end
;
function sl_d_p, j,erey,vsiz,dif,stc ,x1,y1
;******* ******  * **** **** *** ***  *****
;**
;** Data Processing.
;** ---- ----------
;**
	common tmp_dp,	bb,cpx,his,n,nk,nl,nz,typ,fval
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
	common my_ovs,	sum1   ,sum2   ,sum3   ,sum4   ,sum5   ,sum6   ,sum7,$
			ovs1_z ,ovs2_z ,ovs3_z ,ovs4_z ,ovs5_z ,ovs6_z ,$
			ov_pmx,ov_f,ov_z,ov_l,ov_m
;**
;carez sl_hist return a long vector.
;care his
ab=1
	typ= vsiz(vsiz(0)+1)
        if  (typ eq 64)     then cpx=1	    else cpx=0
	if   vsiz(0)  eq  3 then nz=vsiz(3) else nz =1
	nk = vsiz(1)
	if   vsiz(0)  gt  1 then nk=nk*vsiz(2)
	nl = nk*nz
;**
	case j of
;** Rescale.
;** -------
;** 0:
;** Reduce.
;** ------
    1:	begin
	if nz eq 1 then begin
	 if   vsiz(10)*vsiz(11) eq 0  then begin
	      bb=sl_psizm(sare,sare_z,2,vsiz(10)+1,vsiz(11)+1,typ,-1,-1)
	      sare(0,0)=erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11))
	      bb=sl_pp(0,sare,sare_z,erey,vsiz)
	      bb=sl_dd(2,sare,sare_z)
	 endif else begin
	      erey=erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11))
	      vsiz( 1) =vsiz(10)+1   &  vsiz( 2)=vsiz(11)+1
	 endelse
	endif else begin
	      erey=erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11), $
						 vsiz(9):vsiz(9)+vsiz(12))
	      vsiz( 1) =vsiz(10)+1   &  vsiz( 2)=vsiz(11)+1
	      if  vsiz(12) eq 0 then begin
				vsiz( 0) =2	     & vsiz( 3)=typ
	      endif  else       vsiz( 3) =vsiz(12)+1
	endelse
	vsiz(13) =vsiz(10)   &  vsiz(14) =vsiz(11)   & vsiz(15)=vsiz(12)
	vsiz( 7) =0	     &  vsiz( 8) =0	     & vsiz(9)=0
	vsiz( 6) =1
	for k=1,vsiz(0) do vsiz(6)=vsiz(6)*vsiz(k)
	end
;** Equalize.
;** --------
    2:	begin
	if typ lt 8 then begin typ=16 & bb =sl_conv(erey,vsiz,typ) & endif
	if nz  eq 1 then begin
	   if typ ne 64  then n=sl_hist(      erey ,nl,typ	  ,his,x1,y1)-1 $
			 else n=sl_hist(float(erey),nl,16	  ,his,x1,y1)-1
	   if n gt 0 then  for k = long(1),n do his(k)=his(k-1)+his(k)
	   erey(0,0)    =  his(erey) & endif $
	else for   l=0,nz-1  do begin
	   if typ ne 64  then n=sl_hist(      erey(*,*,l) ,nk,typ,his,x1,y1)-1 $
			 else n=sl_hist(float(erey(*,*,l)),nk,16 ,his,x1,y1)-1
	   if n gt 0 then  for k = long(1),n do his(k)=his(k-1)+his(k)
	   erey(0,0,l)  =  his(erey(*,*,l))  &  endfor
	his=1 & end
;** Correlate.
;** ---------
    3:	begin
	bb=sl_psizm(ares,ares_z,2,vsiz(12)+1,vsiz(12)+1,8,-1,-1)
	for k1=0,vsiz(12)   do $
	for k2=0,vsiz(12)   do $
		 if (k1 eq k2) then ares(k1 ,k1)=1.   else $
		 if dif eq 0   then ares(k1 ,k2)=sl_correl($
		     erey(*,*, k1), erey(*,*,k2),vsiz(1),vsiz(2),typ)     else $
				    ares(k1 ,k2)=sl_correl($
		     erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11)   ,$
				  				   vsiz(9)+k1),$
		     erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11)   ,$
					  vsiz(9)+k2),vsiz(10)+1,vsiz(11)+1,typ)
	end
;** Roberts.
;** -------
   5:	begin
	if typ lt 8 then begin typ=16 & bb =sl_conv(erey,vsiz,typ) & endif
	if nz  eq 1 then erey(0,0)  = sl_robt(erey,vsiz(1),vsiz(2),typ) $
		    else for l =0 ,nz-1 do  $
		    	 erey(0,0,l)= sl_robt(erey(*,*,l) ,vsiz(1),vsiz(2) ,typ)
	end
;** Sobel.
;** -----
   6:	begin
	if typ lt 8 then begin typ=16 & bb =sl_conv(erey,vsiz,typ) & endif
	if nz  eq 1 then erey(0,0)  = sl_sobl(erey,vsiz(1),vsiz(2),typ) $
		    else for l =0 ,nz-1 do  $
		    	 erey(0,0,l)= sl_sobl(erey(*,*,l) ,vsiz(1),vsiz(2) ,typ)
	end
;** Smooth.
;** ------
    7:	if (vsiz(1) gt dif) and ((vsiz(2) gt dif) or (stc(0) eq 1)) then begin
	if nz eq 1 then begin
	      if stc(0) eq 0 then $
		 bb	    = sl_lis  (erey,vsiz(1),vsiz(2),typ,dif,1)  $
	      else for l=0,vsiz(2)-1 do $
		 erey(0,l)  = sl_lis  (erey(*,l),vsiz(1),1 ,typ,dif,0)
	endif else for l=0,nz-1 do $
		 erey(0,0,l)= sl_lis  (erey (*,*,l),vsiz(1),vsiz(2),typ,dif,0)
	endif else ab=0
;** Median.
;** ------
    8:	if (vsiz(1) gt dif) and ((vsiz(2) gt dif) or (stc(0) eq 1)) then begin
	if nz eq 1 then begin
;	      if (typ ne 2) and (y1-x1 le 255) then begin
;		 bb = sl_scalf(erey,vsiz,x1,y1,0,2,dummy,256) & typ=2  & endif
	      if stc(0) eq 0 then $
		 erey(0,0)  = sl_media(erey,vsiz(1),vsiz(2),typ,dif,x1,y1) $
	      else for l=0,vsiz(2)-1 do $
		 erey(0,l)  = sl_media(erey(*,l) ,vsiz(1),1,typ,dif,x1,y1)
	endif else for l=0,nz-1  do begin
;		 if (typ ne 2) and (y1-x1 le 255) then $
;		 erey(0,0,l)= sl_scale(erey (*,*,l),vsiz(1),vsiz(2),typ,x1,y1)
		 erey(0,0,l)= sl_media(erey (*,*,l),vsiz(1),vsiz(2),typ,dif,x1,y1)
	     endfor
	endif else ab=0
;** Erey-smooth.
;** -----------
    9:	if (vsiz(1) gt dif) and ((vsiz(2) gt dif) or (stc(0) eq 1)) then begin
	bb=sl_b_fix(erey,vsiz,typ)
	if nz eq 1  then begin
	      if stc(0) eq 0 then $
		 erey(0,0)  = (erey-sl_lis(erey,vsiz(1),vsiz(2)    ,typ,dif,0)) $
	      else for l=0,vsiz(2)-1 do $
		 erey(0,l)  = (erey(*,l)-sl_lis(erey(*,l),vsiz(1),1,typ,dif,0))
	endif else for l=0,nz-1 do begin
			     bb=sl_psizm(sare,sare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
			     sare(0,0)	= erey(*,*,l)
			     erey(0,0,l)=(sare-sl_lis(sare,vsiz(1), vsiz(2),$
								    typ,dif,0))
			     endfor
	     bb=sl_dd(2,sare,sare_z)
	endif else ab=0
;** Inverse frames.
;** ------- ------
    10:	begin
	bb=sl_psizm(ares,ares_z,2,vsiz(1),1,typ,-1,-1)
	if nz eq 1  then begin
			 for k=0,(vsiz(2)-1)/2 do begin
				l=vsiz(2)-k-1
				ares(0,0)   =erey(*,k)
				erey(0,k)   =erey(*,l)
				erey(0,l)   =ares(*,0)
			 endfor
	endif else  begin
		if stc(0) eq -1 then begin n=0 & k2=nz-1
		endif		else begin n=stc(0)-1 & k2=n & endelse
			for k1=n, k2 do $
			 for k=0,(vsiz(2)-1)/2 do begin
			    	l=vsiz(2)-k-1
				ares(0,0)   =erey(*,k,k1)
				erey(0,k,k1)=erey(*,l,k1)
				erey(0,l,k1)=ares(*,0)
			 endfor
	endelse
	bb=sl_dd(2 ,ares,ares_z)
	end
;
;** Frequency.
;** ---------
    11:	begin
	if cpx then bb=sl_pp(0,erey,vsiz,arel,arel_z) $
	else if nz eq 1 then begin
		bb=sl_psizm(arel,arel_z,2,vsiz(1),vsiz(2),64,-1,-1)
		arel(0,0)  =erey
	endif else begin
		bb=sl_psizm(arel,arel_z,3,vsiz(1),vsiz(2),nz,64,-1)
		arel(0,0,0)=erey
	endelse
	if nz eq 1 then begin
		bb	 =sl_fft  (arel,-1,vsiz(1),vsiz(2),1)
		bb	 =sl_shiff(arel,vsiz(1),vsiz(2),64,vsiz(1)/2,vsiz(2)/2)
	endif else for l=0 ,nz-1 do begin
		arel(0,0,l)=sl_fft(arel(*,*,l),-1,vsiz(1),vsiz(2),0)
		arel(0,0,l)=sl_shift(arel(*,*,l),vsiz(1),vsiz(2),64,vsiz(1)/2,vsiz(2)/2)
		endfor
	end
;** Spacial.
;** -------
    12:	if arel_z(arel_z(0)+1) eq 64 then begin
	if arel_z(0) lt 3 then nz =1 else nz =arel_z(3)
	if dif       le 2 then typ=4 else typ=dif
	if nz eq 1 then bb=sl_psizm(erey,vsiz,2,arel_z(1),arel_z(2),typ,-1,-1) else $
			bb=sl_psizm(erey,vsiz,3,arel_z(1),arel_z(2),nz,typ,-1)
	if nz eq 1 then begin
		   bb	       =sl_shiff(arel,arel_z(1),arel_z(2),64,$
					-arel_z(1)/2,  -arel_z(2)/2)
		   erey (0,0)  =sl_fft(arel,1,arel_z(1),arel_z(2),0)
		   bb	       =sl_shiff(arel,arel_z(1),arel_z(2),64,$
					 arel_z(1)/2,   arel_z(2)/2)
	endif else for l=0 ,nz-1 do begin
	     	   arel (0,0,l)=sl_shift(arel(*,*,l),   arel_z(1),arel_z(2),64,$
					-arel_z(1)/2,  -arel_z(2)/2)
	     	   erey (0,0,l)=sl_fft(arel(*,*,l),1,   arel_z(1),arel_z(2),0)
	     	   arel (0,0,l)=sl_shift(arel(*,*,l),   arel_z(1),arel_z(2),64,$
					 arel_z(1)/2,   arel_z(2)/2)
		   endfor
	endif else ab=0
;** Increase erey dimension.
;** -------- ---- ---------
    13: begin
	bb=sl_pp(0,erey,vsiz,sare,sare_z)
	k1=vsiz(1)
	if vsiz(0) gt 1 then k2=vsiz(2) else k2=0
	if (k2 eq 0) and  (stc(1) eq 0) then begin
	  bb=sl_psizm(erey,vsiz,1,k1+stc(0),typ,-1,-1,-1)
	  erey(0) = sare
	endif else if (nz eq 1) and  (stc(2) eq 0) then begin
	  bb=sl_psizm(erey,vsiz,2,k1+stc(0),k2+stc(1),typ,-1,-1)
	  erey(0,0) = sare
	endif else begin
	  bb=sl_psizm(erey,vsiz,3,k1+stc(0),k2+stc(1),nz+stc(2),typ,-1)
	  erey(0,0,0)=sare
	endelse
	bb=sl_dd(2,sare,sare_z)
	end
;** overflows.
;** ---------
    14:	if (typ eq 4) then erey(where(erey lt 0)) =32767 else ab=0
;** Duplicate.
;** ---------
    15:	begin
	if vsiz(12) eq 0 then $
	     bb=sl_psizm(ares,ares_z,2,vsiz(10)+1,vsiz(11)+1,typ,-1,-1) else $
	     bb=sl_psizm(ares,ares_z,3,vsiz(10)+1,vsiz(11)+1,vsiz(12)+1,typ,-1)
	if dif eq 0 then if vsiz(12) eq 0 then ares(0,0)  =erey $
					  else ares(0,0,0)=erey $
	else if nz  eq 1 then $
	   ares(0,0  )=erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11)) $
	else if vsiz(12) eq 0 then $
	   ares(0,0  )=erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11), $
						     vsiz(9):vsiz(9)+vsiz(12)) $
	else $
	   ares(0,0,0)=erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11), $
						     vsiz(9):vsiz(9)+vsiz(12))
	end
;** Complex arel ---> power erey.
;** ------- ----      ----- ----
    16:	if  arel_z(arel_z(0)+1) eq 64 then begin
		nl=arel_z(1)*arel_z(2)
		if arel_z(0) lt 3 then $
		   bb=sl_psizm(erey,vsiz,2,arel_z(1),arel_z(2),8,-1,-1) $
		else begin
		   bb=sl_psizm(erey,vsiz,3,arel_z(1),arel_z(2),arel_z(3),8,-1)
		   nl=nl*arel_z(3) & endelse
		bb=sl_abs(arel, erey,nl,64,vsiz(0))
		if dif then bb=sl_dd(2,arel,arel_z)
	endif   else ab=0
;** Complex arel ---> phase erey.
;** ------- ----      ----- ----
    17:	if  arel_z(arel_z(0)+1) eq 64 then begin
		nl=arel_z(1)*arel_z(2)
		if arel_z(0) lt 3 then $
		   bb=sl_psizm(erey,vsiz,2,arel_z(1),arel_z(2),8,-1,-1) $
		else begin
		   bb=sl_psizm(erey,vsiz,3,arel_z(1),arel_z(2),arel_z(3),8,-1)
		   nl=nl*arel_z(3) & endelse
		bb=sl_atangm(arel, erey,nl,64,vsiz(0))
		if dif then bb=sl_dd(2,arel,arel_z)
	endif   else ab=0
;** Complex arel ---> imaginary erey.
;** ------- ----      --------- ----
    18:	if  arel_z(arel_z(0)+1) eq 64 then begin
		nl=arel_z(1)*arel_z(2)
		if arel_z(0) lt 3 then $
		   bb=sl_psizm(erey,vsiz,2,arel_z(1),arel_z(2),8,-1,-1) $
		else begin
		   bb=sl_psizm(erey,vsiz,3,arel_z(1),arel_z(2),arel_z(3),8,-1)
		   nl=nl*arel_z(3) & endelse
		bb=sl_imaginary(arel, erey,nl,64,vsiz(0))
		if dif then bb=sl_dd(2,arel,arel_z)
	endif   else ab=0
;** Distribution.
;** ------------
    19:	begin
	bb=sl_dd(2,ares,ares_z)
	if not dif then if nz eq 1  then $
	  n=sl_hist(erey,nk,typ,his,x1,y1)   else $
	  n=sl_hist(erey(*,*,stc(0)),nk,typ,his,x1,y1) $
	else if nz eq 1 then  $
	  n=sl_hist(erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11)),$
			(vsiz(10)+1)*(vsiz(11)+1),typ,his,x1,y1) 	  else $
	  n=sl_hist(erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11) ,$
		         vsiz(9)+stc(0)),(vsiz(10)+1)*(vsiz(11)+1),typ,his,x1,y1)
	if n gt 0 then begin
	  bb=sl_psizm(ares,ares_z,2,n,1,16,-1,-1)
	  ares(0,0)=his
	  his=1 & endif
	end
;** X Derivative.
;** - ----------
;** 20:
;** Y Derivative.
;** - ----------
;** 21:
;** Gradient.
;** --------
;** 22:
;** Standard deviation over frames.
;** -------- --------- ---- ------
;** 23:
;** X mixed salad.
;** - ----- -----
;** 24:
;** Y mixed salad.
;** - ----- -----
;** 25:
;** Sum over frames.
;** --- ---- ------
;** 26:
;** Sum each frames.
;** --- ---- ------
;** 27:
;** Transpose x y.
;** --------- ---
;** 28:
;** Pics only.
;** ---- ----
;** 29:
;** Logarithm N.
;** -----------
;** 30:
;** Exponential N.
;** -------------
;** 31:
;** Square-root.
;** -----------
;** 32:
;** Square.
;** ------
;** 33:
;** 1/z.
;** ---
;** 34:
;** z/n.
;** ---
;** 35:
;** z*n.
;** ---
;** 36:
;** Magnitude.
;** ---------
;** 37:
;** Convert type.
;** ------- ----
;** 38:
;** Convert type.
;** ------- ----
;** 39:
;** Projections.
;** -----------
;** 40:
;** Deviation.
;** ---------
    41:	if nk gt 1 then begin
	if not dif then if nz eq 1  then $
	  x1=sl_deviat(erey,		y1,vsiz(1),vsiz(2),typ) else $
	  x1=sl_deviat(erey(*,*,stc(0)),y1,vsiz(1),vsiz(2),typ) $
	else if nz eq 1 then  $
	  x1=sl_deviat(erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11)),$
				         y1,vsiz(10)+1,vsiz(11)+1,typ) 	  else $
	  x1=sl_deviat(erey(vsiz(7):vsiz(7)+vsiz(10),vsiz(8):vsiz(8)+vsiz(11) ,$
			 vsiz(9)+stc(0)),y1,vsiz(10)+1,vsiz(11)+1,typ)
	endif else x1=0.
;** Shift.
;** -----
    42: if nz eq 1 then bb  =  sl_shiff(erey,vsiz(1),vsiz(2),typ,stc(0),stc(1))$
		   else erey(0,0,stc(2))= $
		   sl_shift(erey(*,*,stc(2)),vsiz(1),vsiz(2),typ,stc(0),stc(1))
;** X Projections.
;** -------------
    43: begin
	      bb=sl_psizm(ares,ares_z,2,vsiz(10)+1,vsiz(12)+1,8,-1,-1)
	      if vsiz(11) ne 0 then if not dif then bb=sl_fsum(erey,1,vsiz,ares) else $
		if nz eq 1 then  $
		bb=sl_tsum (erey(vsiz(7):vsiz(7)+vsiz(10)  $
				,vsiz(8):vsiz(8)+vsiz(11)),1,2,ares) else $
	        bb=sl_tsum (erey(vsiz(7):vsiz(7)+vsiz(10) ,$
		   vsiz(8):vsiz(8)+vsiz(11),vsiz(9):vsiz(9)+vsiz(12)),1,2,ares)
	end
;** Y Projections.
;** -------------
    44: begin
	      bb=sl_psizm(ares,ares_z,2,vsiz(11)+1,vsiz(12)+1,8,-1,-1)
	      if vsiz(11) ne 0 then if not dif then bb=sl_fsum(erey,0,vsiz,ares) else $
		if nz eq 1 then  $
		bb=sl_tsum (erey(vsiz(7):vsiz(7)+vsiz(10)  $
				,vsiz(8):vsiz(8)+vsiz(11)),0,2,ares) else $
	        bb=sl_tsum (erey(vsiz(7):vsiz(7)+vsiz(10) ,$
		   vsiz(8):vsiz(8)+vsiz(11),vsiz(9):vsiz(9)+vsiz(12)),0,2,ares)
	end
;** Subtract.
;** --------
    45: begin
	bb=sl_b_fix(erey,vsiz,typ)
	erey(0,0,stc(2))=erey(*,*,stc(1))-erey(*,*,stc(0))
	end
;** Add.
;** ---
    46: begin
	bb=sl_b_fix(erey,vsiz,typ)
	erey(0,0,stc(2))=erey(*,*,stc(1))+erey(*,*,stc(0))
	end
;** Make a unique frame.
;** ---- - ------ -----
    47: if nz gt 1 then $
	 if vsiz(10) gt vsiz(11) then begin
	   bb=sl_psizm(ares,ares_z,2,vsiz(10)+1,(vsiz(11)+1)*(vsiz(12)+1),typ,-1,-1)
	   for l=0,vsiz(12) do $
		   ares(0,l*(vsiz(11)+1))=(erey(vsiz(7):vsiz(7)+vsiz(10), $
			     vsiz(8):vsiz(8)+vsiz(11) , vsiz(9)+l))
	 endif else begin
	   bb=sl_psizm(ares,ares_z,2,(vsiz(10)+1)*(vsiz(12)+1),vsiz(11)+1,typ,-1,-1)
	   for l=0,vsiz(12) do $
		   ares(l*(vsiz(10)+1),0)=(erey(vsiz(7):vsiz(7)+vsiz(10), $
			     vsiz(8):vsiz(8)+vsiz(11) , vsiz(9)+l))
	 endelse
;** Data compaction.
;** ---- ----------
    48: begin
	if (stc(0)/vsiz(1)) ge 2 then l=1 else l=0
	if (stc(1)/vsiz(2)) ge 2 then l=1
	if nz gt 1  then begin
	      bb=sl_psizm(sare,sare_z,3,stc(0),stc(1),nz,typ,-1)
	      for k=0,nz-1 do sare(0,0,k)=sl_redim(erey(*,*,k),vsiz(1),vsiz(2) $
							  ,typ,stc (0),stc(1),l)
	      bb=sl_pp(0,sare,sare_z,erey,vsiz)
	      bb=sl_dd(2,sare,sare_z)
	endif else  begin
	      bb=sl_psizm(sare,sare_z,2,stc(0),stc(1),typ,-1,-1)
	      sare(0,0)	=sl_redim(erey,vsiz(1),vsiz(2),typ,stc(0),stc(1),l)
	      bb=sl_pp(0,sare,sare_z,erey,vsiz)
	      bb=sl_dd(2,sare,sare_z)
	endelse
	end
;** Normalize.
;** ---------
    49: begin ab=0
	if (typ le 16) and (typ ne 8) then   begin
	typ=8  & bb=sl_conv(erey,vsiz,typ) & endif
	if (nz eq 1) or (stc(1) eq 0) then begin
	    if y1 ne 0 then begin
	      if vsiz(0) eq 2	then  erey(0,0) =(1./y1)*erey $
				else  erey	=(1./y1)*erey
	      endif
	endif else begin
	    bb =sl_psizm(sare,sare_z,2,vsiz(1),vsiz(2),typ,-1,-1)
	    for k=0,nz-1 do  begin
		   sare(0,0)= erey(*,*,k)
		   fval     = sl_maxf(sare,sare_z,l)
		   if fval ne 0  then erey(0,0,k)=sare/fval
		   endfor
	    bb=sl_dd(2,sare,sare_z)
	endelse
	end
;** z-n.
;** ---
    50: begin
	  if (y1-x1) gt 1   then begin
	   n = sl_click(long(x1),long(y1),long(x1),9,0)
	   if n ne 0 then if nz eq 1 then erey(0,0  ) = erey - n $
				     else erey(0,0,0) = erey - n
	  endif
	end
;** z>0.
;** ---
    51: if y1 gt 1 then $
	      if vsiz(0) eq 2	then  erey(0,0) =erey > 1 $
				else  erey	=erey > 1 $
	else  ab=0
;**
;** integer positive.
;** ------  --------
    52:	if (typ eq 4) then begin
		his= where(erey lt 0)
		if his(0) ge 0 then begin
			typ=16 & bb =sl_conv(erey,vsiz,typ)
			erey(his)=65536 + erey(his)
		endif
	endif   else ab=0
;**
;** get area.
;** --- ----
    89:	begin   bb=sl_psizm(ares,ares_z,2,stc(0),stc(1),dif,-1,-1)
		ares(*,*)=x1
		end
;**
    else:ab=sl_d_p1(j,erey,vsiz,dif,stc ,x1,y1)
    endcase
return,ab
end
;
;
;
function sl_gauss, x,siz,p
;******* ********
;**
bb=0
	if p(2) ne 0. then begin
;
	 z =(x-p(1))/p(2)
	 z = z*z/(-2)
	 bb= sl_expn(z,siz)
;
	 return, p(0) * z + p(3) + p(4)*x + p(5)*x*x
;
	endif
return,	bb
end
;
;
function sl_fit, erey,vsiz,f,f_ab,fct,sens,deg
;******* ******
;**     fct:  0=gauss   1=poly 2=poly(null) 3=poly surface
;**	sens: 0=X	1=Y
;**
	common my_fit , fi_ez,fi_typ,fi_f,fi_nx,fi_ny,fi_nz,fi_ne,fi_l,fi_coef,$
			fi_pcoef,fi_min,fi_max
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
ab=0
;
	if (fct eq 0) or (fct eq 1) or (fct eq 2) then begin
;**	Gauss , Poly
;**	-----   ----
	   if fct  eq 0 then fi_l=8 else fi_l=deg+1
	   if sens eq 0 then if fi_nx ge fi_l then begin
		if fi_f then k= fi_typ else k=8
		bb=sl_psizm(arex,arex_z,1,fi_nx,k,-1,-1,-1)
		bb=sl_psizm(arey,arey_z,2,fi_nx,1, k,-1,-1)
		bb=sl_psizm(arei,arei_z,1,fi_nx,8,-1,-1,-1)
		if fct eq 1 then begin
		   bb=sl_psizm(sare,sare_z,1,fi_nx,4,-1,-1,-1)
		   sare=sl_index(fi_nx,4)
		endif
		for jj=f_ab(0,1),f_ab(1,1) do begin
			if fi_ez eq 1 then arex(0)=erey(f_ab(0,0):f_ab(1,0),jj) $
				      else arex(0)=erey(f_ab(0,0):f_ab(1,0),jj,f)
			fi_ne=sl_where(arex,arex_z,'ne',0,arei)
			if fi_ne ge fi_l then begin
;**			Fit known points
;**			--- ----- ------
			   bb=sl_psizm(areu,areu_z,1,fi_ne,8,-1,-1,-1)
			   areu(0)  =arei(0:fi_ne-1)
;**care arex(areu)
			   if fct eq 0 then begin
			      fi_coef(2)=0.
			      arey(0,0)=sl_gfit(areu,arex(areu),areu_z,fi_coef)
			      if fi_coef(2) ne 0. then ab=1
			   endif else if (fct eq 1) or (fct eq 2) then begin
			      fi_pcoef(0,0)=sl_polycoef(areu,arex(areu),areu_z,deg)
			      ab=1 & endif
			   if fct eq 1 then $
			      arey  (0,0)=sl_polyval(sare,sare_z,$
						     fi_pcoef(0,0:deg),deg)
			   if ab eq 1 then begin
;**			    Put them
;**			    --- ----
			    if fct ne 2 then if fi_typ eq 2 then $
					bb=sl_d_p(0,arey,arey_z,0,0,0.,255.)
			    if fct eq 0 then begin
			       if     fi_ez  eq 1 then $
				      for ii=long(0),fi_ne-1 do $
				      erey(f_ab(0,0)+areu(ii),jj)  =arey(ii,0) $
			       else   for ii=long(0),fi_ne-1 do $
				      erey(f_ab(0,0)+areu(ii),jj,f)=arey(ii,0)
			    endif else if fct eq 1 then begin
			       if     fi_ez  eq 1 then $
				      erey(f_ab(0,0),jj)  =arey(*,0) $
			       else   erey(f_ab(0,0),jj,f)=arey(*,0)
			    endif
			    if fct eq 1 then fi_ne=0 $
			    else   fi_ne=sl_where(arex,arex_z,'eq',0,arei)
			    if fi_ne gt 0 then begin
;**			      Fit others (null)
;**			      --- ------
			      bb=sl_psizm(areu,areu_z,1,fi_ne,8,-1,-1,-1)
			      areu(0)  =arei(0:fi_ne-1)
			      if  fct eq 0 then $
			        arey(0,0)=sl_gauss(areu,areu_z,fi_coef)
			      if (fct eq 1) or (fct eq 2) then $
			        arey(0,0)=sl_polyval(sare,sare_z,$
						     fi_pcoef(0,0:deg),deg)
			      if fi_typ eq 2 then $
				 bb =sl_d_p(0,arey,arey_z,0,0,0.,255.)
;**			      Put them
;**			      --- ----
			      if fi_ez  eq 1 then $
				      for ii=long(0),fi_ne-1 do $
				      erey(f_ab(0,0)+areu(ii),jj)  =arey(ii,0) $
			      else    for ii=long(0),fi_ne-1 do $
				      erey(f_ab(0,0)+areu(ii),jj,f)=arey(ii,0)
			    endif
			   endif
			endif
		endfor
		bb=sl_dd(2,arei,arei_z)
		bb=sl_dd(2,areu,areu_z)
		bb=sl_dd(2,arex,arex_z)
		bb=sl_dd(2,arey,arey_z)
		if fct eq 1 then bb=sl_dd(2,sare,sare_z)
	   endif
	endif else if fct eq 3 then begin
		sare(0,0)=sl_surfit(sare,sare_z,deg)
		if fi_typ eq 2 then bb=sl_d_p(0,sare,sare_z,0,0, 0.,255.)
		if fi_ez eq 1 then  erey(f_ab(0,0),f_ab(0,1))  =sare(*,*) $
			      else  erey(f_ab(0,0),f_ab(0,1),f)=sare(*,*)
		ab=1
	endif
return, ab
end
;
;
function sl_prefit, erey,vsiz,f,f_ab,fct,sens,deg
;******* *********
;**     fct:  0=gauss   1=poly 2=poly(null) 3=poly surface
;**
	common my_fit , fi_ez,fi_typ,fi_f,fi_nx,fi_ny,fi_nz,fi_ne,fi_l,fi_coef,$
			fi_pcoef,fi_min,fi_max
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
bb=0
	if vsiz(0) ge 3 then fi_ez=vsiz(3) else fi_ez=1
	fi_typ=vsiz(vsiz(0)+1)
	if (fi_typ gt 16) or (fi_typ eq 8) then fi_f =1 else fi_f=0
	fi_nx =f_ab(1,0)-f_ab(0,0)+1
	fi_ny =f_ab(1,1)-f_ab(0,1)+1
	fi_nz =f_ab(1,2)-f_ab(0,2)+1
;**
	if fct eq 3 then begin
	   fi_l=deg+1
	   if  (fi_nx ge fi_l) and (fi_ny ge fi_l) then begin
		if fi_f then k= fi_typ else k=8
		bb=sl_psizm(sare,sare_z,2,fi_nx,fi_ny,k,-1,-1)
		if fi_ez eq 1 then  sare(0,0)=erey(f_ab(0,0):f_ab(1,0), $
						   f_ab(0,1):f_ab(1,1)) $
			      else  sare(0,0)=erey(f_ab(0,0):f_ab(1,0), $
						   f_ab(0,1):f_ab(1,1),f)
		fi_max=sl_maxim(sare,sare_z,k,fi_min)
		if (fi_min eq 0) or ((fi_min lt 0) and (fi_max ge 0)) then $
		   bb=sl_fit(erey,vsiz,f,f_ab,2  ,sens,deg)
		   bb=sl_fit(erey,vsiz,f,f_ab,fct,sens,deg)
		   k =sl_dd(2 ,sare,sare_z)
	   endif
	endif else bb=sl_fit(erey,vsiz,f,f_ab,fct,sens,deg)
return, bb
end
;
;
;
function sl_opcheck,oprt
;******* **********
;**	Check concistancies for view operations.
;**	----- ------------- --- ---- ----------
;**	Matrix operations: are1 {oprt} are2 --> are3  2:+ 3:- 4:* 5:| 6:# 7:>-
;**
	common my_opview,xdm1,ydm1,zdm1,typ1,xdm2,ydm2,zdm2,typ2,tip,xi,yi,zi
	zi=-1 & xi=-1 & yi=-1
	if ((zdm1 le  1 ) and (zdm2 le  1 )) then zi= 0 $
		      else if (zdm1 eq zdm2) then zi= 3 $
		      else if (zdm2 eq  1  ) then zi= 2 $
		      else if (zdm1 eq  1  ) then zi= 1 $
		      else			  zi= 4
;
	if ((ydm1 eq  1 ) and (ydm2 eq  1 )) then yi= 0 $
		      else if (ydm1 eq ydm2) then yi= 3 $
		      else if (ydm2 eq  1  ) then yi= 2 $
		      else if (ydm1 eq  1  ) then yi= 1 $
		      else if (ydm1 gt ydm2) then yi= 4 $
		      else if (ydm1 lt ydm2) then yi= 5
;
	if  (xdm1 eq xdm2)		     then xi= 3 $
		      else if (xdm2 eq  1  ) then xi= 2 $
		      else if (xdm1 eq  1  ) then xi= 1 $
		      else if (xdm1 gt xdm2) then xi= 4 $
		      else if (xdm1 lt xdm2) then xi= 5
;
	if  oprt eq 6 then begin
			   if (ydm1 eq xdm2) then begin	     xi= 3
							     yi= 3
			   endif else $
			   if (xi ne 3) or   (yi ne 0)	then xi=-1
			   if (zi   eq  4  ) 		then zi=-1
	endif else $
	if  oprt eq 5 then begin
			   if (yi eq 4)  then yi=3
			   if (yi eq 5)  then begin
			       yi=ydm1 & ydm1=ydm2  & ydm2=yi & yi=3
			       endif
			   if (xi eq 4)  then xi=3
			   if (xi eq 5)  then begin
			       xi=xdm1 & xdm1=xdm2  & xdm2=xi & xi=3
			       endif
			   if (xi ne 3) or  ((yi ne 0)  and (yi ne 3)) then $
			   if (zi ne 3) and  (zi ne 0)  then zi=-1
	endif	      else begin
			   if (xi eq 1)			then xi=-1
			   if (yi eq 1)			then yi=-1
			   if (xi eq 5)			then xi=-1
			   if (yi eq 5)			then yi=-1
			   if (zi eq 1)			then zi=-1
			   if (zi eq 4)			then zi=-1
	endelse
;
	if (xi eq 4) or (yi eq 4) then begin
		if (oprt eq 2) or (oprt eq 3) or (oprt eq 7) then begin
			if xi eq 4 then xi=3
			if yi eq 4 then  begin yi=3 & ydm2=ydm1 & endif
		endif else xi=-1
	endif
;
	if (xi ne 3)  and (yi ne 3) and (yi ne 0)	then xi=-1
return,1
end
;
function sl_d_pm,	are1,siz1 ,oprt, are2,siz2  ,are3,siz3
;******* ********
;**
;**	Matrix operations: are1 {oprt} are2 --> are3  2:+ 3:- 4:* 5:| 6:# 7:>-
;**	------ ----------  -------------------------    -   -   -   -   -   --
	common my_opview,xdm1,ydm1,zdm1,typ1,xdm2,ydm2,zdm2,typ2,tip,xi,yi,zi
;**
bb=0
	xdm1=siz1(1)
	ydm1=siz1(2)
	xdm2=siz2(1)
	ydm2=siz2(2)
	if   siz1(0) ge 3 then zdm1=siz1(3) else zdm1=1
	if   siz2(0) ge 3 then zdm2=siz2(3) else zdm2=1
	typ1=siz1(siz1(0)+1)
	typ2=siz2(siz2(0)+1)
;**
	bb  =sl_opcheck(oprt)
;**
;
	if (zi ge 0)  and (xi ge 0) and (yi ge 0) then  begin
;**
	    if (typ1 ge typ2) then  tip= typ1    else tip=typ2
	    if (tip  eq 16)   and ((typ1 eq  8)  or  (typ2 eq 8)) then tip=8
	    if (tip  eq 4 )   and ((oprt eq  4)  or  (oprt eq 6)) then tip=16
	    if (oprt ne 5 )   and  (tip  eq  2)  then tip= 4
	    if (oprt eq 6 )   and  (tip  le 16)  then tip= 8
	    if (oprt ne 5 )   and  (typ1 ne tip) then $
				    bb= sl_d_p(38,are1,siz1,0,[tip,0],0,2)

;** + - * # >-
;*************
	    if (oprt eq 2) or (oprt eq 3) or (oprt eq 4) $
			   or (oprt eq 6) or (oprt eq 7)   then begin
		if  (xi eq 3) and ((yi eq 3) or (yi eq 0)) then begin
;**		(x,y) {opr} (x,y)
;**		*****************
		 if (zi eq 0) then  begin
			if (oprt eq 6) and (yi eq 0) then $
		  	    bb=sl_psizm(are3, siz3,2,xdm1,xdm1,tip,-1,-1) else $
		  	    bb=sl_psizm(are3, siz3,2,xdm1,ydm2,tip,-1,-1)
			if oprt eq 2 then are3(0,0)  =are1	  + are2
			if oprt eq 3 then are3(0,0)  =are1	  - are2
			if oprt eq 4 then are3(0,0)  =are1	  * are2
			if oprt eq 6 then if yi eq 0  then $
					  are3(0,0)  =are1(*,0)   # are2(*,0)  $
				     else are3(0,0)  =are1	  # are2
			if oprt eq 7 then are3(0,0)  =are1	  - are2
		 endif  else  begin
		  bb=sl_psizm(are3, siz3,3,xdm1,ydm2,zdm1,tip,-1)
;**		(x,y,z) {opr} (x,y,z)
;**		*********************
		  if (zi eq 3) then begin
			for k=0,zdm1-1 do begin
			if oprt eq 2 then are3(0,0,k)=are1(*,*,k) + are2(*,*,k)
			if oprt eq 3 then are3(0,0,k)=are1(*,*,k) - are2(*,*,k)
			if oprt eq 4 then are3(0,0,k)=are1(*,*,k) * are2(*,*,k)
			if oprt eq 6 then are3(0,0,k)=are1(*,*,k) # are2(*,*,k)
			if oprt eq 7 then are3(0,0,k)=are1(*,*,k) - are2(*,*,k)
			endfor
;**		(x,y,z) {opr} (x,y,1)
;**		*********************
		  endif else if (zi eq 2) then begin
			for k=0,zdm1-1 do begin
			if oprt eq 2 then are3(0,0,k)=are1(*,*,k) + are2
			if oprt eq 3 then are3(0,0,k)=are1(*,*,k) - are2
			if oprt eq 4 then are3(0,0,k)=are1(*,*,k) * are2
			if oprt eq 6 then are3(0,0,k)=are1(*,*,k) # are2
			if oprt eq 7 then are3(0,0,k)=are1(*,*,k) - are2
			endfor
		  endif
		 endelse
;**		(x,y) {opr} (1,y)
;**		*****************
		endif else if xi eq 2 then begin
		 if (zi eq 0) then  begin
		  	bb=sl_psizm(are3, siz3,2,xdm1,ydm1,tip,-1,-1)
			if oprt eq 2 then for k=0,xdm1-1 do $
					 are3(k,0)   =are1(k,*)	  + are2
			if oprt eq 3 then for k=0,xdm1-1 do $
					 are3(k,0)   =are1(k,*)	  - are2
			if oprt eq 4 then for k=0,xdm1-1 do $
					 are3(k,0)   =are1(k,*)	  * are2
			if oprt eq 7 then for k=0,xdm1-1 do $
					 are3(k,0)   =are1(k,*)	  - are2
		 endif  else  begin
		  bb=sl_psizm(are3, siz3,3,xdm1,ydm1,zdm1,tip,-1)
;**		(x,y,z) {opr} (1,y,z)
;**		*********************
		  if (zi eq 3) then begin
			for k=0,zdm1-1 do begin
			if oprt eq 2 then for x=0,xdm1-1 do $
					 are3(x,0,k) =are1(x,*,k) + are2(0,*,k)
			if oprt eq 3 then for x=0,xdm1-1 do $
					 are3(x,0,k) =are1(x,*,k) - are2(0,*,k)
			if oprt eq 4 then for x=0,xdm1-1 do $
					 are3(x,0,k) =are1(x,*,k) * are2(0,*,k)
			if oprt eq 7 then for x=0,xdm1-1 do $
					 are3(x,0,k) =are1(x,*,k) - are2(0,*,k)
			endfor
;**		(x,y,z) {opr} (1,y,1)
;**		*********************
		  endif else if (zi eq 2) then begin
			for k=0,zdm1-1 do begin
			if oprt eq 2 then for x=0,xdm1-1 do $
					 are3(x,0,k) =are1(x,*,k) + are2
			if oprt eq 3 then for x=0,xdm1-1 do $
					 are3(x,0,k) =are1(x,*,k) - are2
			if oprt eq 4 then for x=0,xdm1-1 do $
					 are3(x,0,k) =are1(x,*,k) * are2
			if oprt eq 7 then for x=0,xdm1-1 do $
					 are3(x,0,k) =are1(x,*,k) - are2
			endfor
		  endif
		 endelse
;**		(x,y) {opr} (x,1)
;**		*****************
		endif else if yi eq 2 then begin
		 if (zi eq 0) then  begin
		  	bb=sl_psizm(are3, siz3,2,xdm1,ydm1,tip,-1,-1)
			if oprt eq 2 then for k=0,ydm1-1 do $
					 are3(0,k)   =are1(*,k)	  + are2
			if oprt eq 3 then for k=0,ydm1-1 do $
					 are3(0,k)   =are1(*,k)	  - are2
			if oprt eq 4 then for k=0,ydm1-1 do $
					 are3(0,k)   =are1(*,k)	  * are2
			if oprt eq 7 then for k=0,ydm1-1 do $
					 are3(0,k)   =are1(*,k)	  - are2
		 endif  else  begin
		  bb=sl_psizm(are3, siz3,3,xdm1,ydm1,zdm1,tip,-1)
;**		(x,y,z) {opr} (x,1,z)
;**		*********************
		  if (zi eq 3) then begin
			for k=0,zdm1-1 do begin
			if oprt eq 2 then for y=0,ydm1-1 do $
					 are3(0,y,k) =are1(*,y,k) + are2(*,0,k)
			if oprt eq 3 then for y=0,ydm1-1 do $
					 are3(0,y,k) =are1(*,y,k) - are2(*,0,k)
			if oprt eq 4 then for y=0,ydm1-1 do $
					 are3(0,y,k) =are1(*,y,k) * are2(*,0,k)
			if oprt eq 7 then for y=0,ydm1-1 do $
					 are3(0,y,k) =are1(*,y,k) - are2(*,0,k)
			endfor
;**		(x,y,z) {opr} (x,1,1)
;**		*********************
		  endif else if (zi eq 2) then begin
			for k=0,zdm1-1 do begin
			if oprt eq 2 then for y=0,ydm1-1 do $
					 are3(0,y,k) =are1(*,y,k) + are2
			if oprt eq 3 then for y=0,ydm1-1 do $
					 are3(0,y,k) =are1(*,y,k) - are2
			if oprt eq 4 then for y=0,ydm1-1 do $
					 are3(0,y,k) =are1(*,y,k) * are2
			if oprt eq 7 then for y=0,ydm1-1 do $
					 are3(0,y,k) =are1(*,y,k) - are2
			endfor
		  endif
		 endelse
		endif
		if oprt eq 7 then bb=sl_d_p(0,are3,siz3,1,0,0)
		bb=1
	    endif
;** |
;****
	    if (oprt eq 5) then begin
		if  (xi eq 3) and ((yi eq 3) or (yi eq 0))   then begin
;**		(x,1) {|} (x,1)
;**		***************
		 if (zi eq 0) and  (yi eq 0) then  begin
			bb=sl_psizm(are3,siz3,2,xdm1,2,tip,-1,-1)
			are3(0,0)     =are1
			are3(0,1)     =are2
;**		(x,y,z) {|} (x,y,z)
;**		*******************
		 endif else begin
			bb=sl_psizm(are3,siz3,3,xdm1,ydm1,zdm1+zdm2,tip,-1)
			are3(0,0,0)   =are1
			are3(0,0,zdm1)=are2
		 endelse
		endif else if (xi eq 3)  then begin
;**		(x,y) {|} (x,?)
;**		***************
		 if (zi eq 0) then  begin
			bb=sl_psizm(are3,siz3,2,xdm1,ydm1+ydm2  ,tip,-1,-1)
			are3(0,0)     =are1
			are3(0,ydm1)  =are2
;**		(x,y,z) {|} (x,?,z)
;**		*******************
		 endif else if (zi eq 3) then begin
			bb=sl_psizm(are3,siz3,3,xdm1,ydm1+ydm2,zdm1,tip,-1)
			for k=0,zdm1  do begin
			are3(0,0,k)   =are1
			are3(0,ydm1,k)=are2
			endfor
		 endif
		endif else if (yi eq 3) or (yi eq 0) then begin
;**		(x,y) {|} (?,y)
;**		***************
		 if (zi eq 0) then  begin
			bb=sl_psizm(are3,siz3,2,xdm1+xdm2,ydm1  ,tip,-1,-1)
			are3(0,0)     =are1
			are3(xdm1,0)  =are2
;**		(x,y,z) {|} (?,y,z)
;**		*******************
		 endif else if (zi eq 3) then begin
			bb=sl_psizm(are3,siz3,3,xdm1+xdm2,ydm1,zdm1,tip,-1)
			for k=0,zdm1  do begin
			are3(0,0,k)   =are1
			are3(xdm1,0,k)=are2
			endfor
		 endif
		endif
	    endif
	endif
return, bb
end
;
;
;
function sl_lstframe,	erey,xsiz,flg  ,fout,areout,areout_z
;******* ***********
;**
;**	 Matrix construction from vectors list.    ni , cx , cy , radius
;**	 ------ ------------ ---- ------- ----     val, rx , ry , rz
;**                                                "    "    "    "
;**	 ni  = # of points
;**	 flg = 0 to keep only sub_area
;**	 fout= 1 use areout and areout_z=[0,typ,0,0,0,0..]
;**	 fout= 0 use ares
;**	 fout= 2 use ares and lisse

common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common tmp_lstf,mnx,mx, mny,my, mnz,mz, ci,ni
;**
b  =0
ni =erey(0,0)
if (ni gt 1) then begin
	bb =sl_psizm(arei,arei_z,2,1,ni,4,-1,-1)
	arei(0,0)=erey(3,1:ni)-1
	mz =sl_maxim(arei,arei_z,ci,mnz)
;
	bb =sl_psizm(arey,arey_z,2,1,ni,4,-1,-1)
	arey(0,0)=erey(2,1:ni)-1
	my =sl_maxim(arey,arey_z,ci,mny)
;
	bb =sl_psizm(arex,arex_z,2,1,ni,4,-1,-1)
	arex(0,0)=erey(1,1:ni)-1
	mx =sl_maxim(arex,arex_z,ci,mnx)
;
	if flg eq 0 then begin
		arei(0,0)=arei-mnz
		arex(0,0)=arex-mnx
		arey(0,0)=arey-mny
		mx =mx-mnx & my =my-mny & mz =mz-mnz & endif
;
	if fout eq 1 then ci=areout_z(areout_z(0)+1) else ci =xsiz(xsiz(0)+1)
	if fout eq 1 then  $
	     if mz eq 0 then bb=sl_psizm(areout,areout_z,2,mx+1,my+1  ,ci,-1,-1)$
			else bb=sl_psizm(areout,areout_z,3,mx+1,my+1,mz+1,ci,-1)$
	else if mz eq 0 then bb=sl_psizm(ares  ,ares_z  ,2,mx+1,my+1  ,ci,-1,-1)$
			else bb=sl_psizm(ares  ,ares_z  ,3,mx+1,my+1,mz+1,ci,-1)
;
	if bb eq 1 then begin
	   if fout eq 1 then begin
	     if mz eq 0 then for i=0,ni-1 do $
			areout(arex(0,i),arey(0,i))	     =erey(0,i+1) $
		   	else for i=0,ni-1 do $
			areout(arex(0,i),arey(0,i),arei(0,i))=erey(0,i+1)
	   endif   else begin
	     if mz eq 0 then for i=0,ni-1 do $
			ares  (arex(0,i),arey(0,i))	     =erey(0,i+1) $
		   	else for i=0,ni-1 do $
			ares  (arex(0,i),arey(0,i),arei(0,i))=erey(0,i+1)
	   endelse
;**	   Lisse.
;**	   -----
	   if fout eq 2 then bb=sl_d_p(7,ares,ares_z,3 ,[0,0],0,0)
	   if fout eq 2 then bb=sl_d_p(0,ares,ares_z,11,  0  ,0,255)
	b =1
	endif
	bb=sl_dd(2,arei,arei_z)
	bb=sl_dd(2,arey,arey_z)
	bb=sl_dd(2,arex,arex_z)
endif
return,b
end
;
;
;
function sl_getord, m_u,k,dm_ins,rl_ins,areins,zimg,m_sr ,m_frm
;******* *********
;**
;**	 Read (X , Y , values) and make image. --> m_frm=4
;**	 Read (value ,  X,Y,Z) and make image. --> m_frm=8
;**	 ----                  --- ---- -----
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
	common	my_geto,go_v7,go_v2,go_v3,go_rql,go_rqm,go_x5,go_y5
;**
	go_v7(0)=1
	go_v7(2)=0 & go_v7(3)=0
	go_v7(1)=3
	if m_frm eq 8 then go_v7(1)=4
;
	go_v2(0)=go_v7(1)
	go_v2(1)=8
;
	bb =sl_psizm(arei,arei_z,1,go_v2(0),go_v2(1),-1,-1,-1)
	bb =sl_cellget(m_u,go_v7,go_v2,arei,m_sr*(-1))
	if bb then begin
		go_rql=1  & go_rqm=100
		aa =sl_psizm(areo,areo_z,2,4,go_rqm,go_v2(1),-1,-1)
		while (aa) do begin
			if go_rql ge go_rqm then  begin
				   go_v3(0)=0  & go_v3(1)=100  & go_v3(2)=0
				   bb=sl_d_p(13,areo,areo_z,0,go_v3)
				   go_rqm=go_rqm+go_v3(1)
				   endif
			if m_frm eq 4 then begin
                           areo(0,go_rql)=arei(2)
                           areo(1,go_rql)=arei(0)
                           areo(2,go_rql)=arei(1)
			endif else if m_frm eq 8 then begin
                           areo(0,go_rql)=arei(0)
                           areo(1,go_rql)=arei(1)
                           areo(2,go_rql)=arei(2)
                           areo(3,go_rql)=arei(3)
			endif
			aa =sl_cellget(m_u,go_v7,go_v2,arei,0)
			go_rql=go_rql+1
		endwhile
		areo(0,0)=go_rql -1
		go_v7(0) =2
		go_v7(3) =8
;		go_v7(1)=dm_ins(0,k) & go_v7(2)=dm_ins(1,k)
;		go_v7(3) =rl_ins(1,k)
		go_v7(6) =0

		bb =sl_lstframe(areo,areo_z,0  ,1,areins,go_v7)
;
		rl_ins(1,k)=go_v7(go_v7(0)+1)
		dm_ins(0,k)=go_v7(1)
		dm_ins(1,k)=go_v7(2)
		if go_v7(0) eq 3 then zimg=go_v7(3) else zimg=1

		bb =sl_dd(2,areo,areo_z)
	endif
return,bb
end
;
;
;
;
function sl_getrflx, arel,arel_z,dm1,dirc
;******* **********
;**
	common	my_geto,go_v7,go_v2,go_v3,go_rql,go_rqm,go_x5,go_y5
;**
	common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
			io_cur,io_ext,io_seq,io_str
;**
b=0
	go_rqm=dm1
	if go_rqm le 0 then begin
		    dirc='?'
		    go_rql=sl_filr(io_cur+'*',io_ext(1) ,0,dirc,1)
		    if go_rql gt 0 then go_rqm=9 else  begin
		    dirc='?'
		    go_rql=sl_filr(io_cur+'*',io_ext(13),0,dirc,1)
		    if go_rql gt 0 then go_rqm=6 &  endelse
	endif else  go_rql=sl_filr(io_cur+'*',io_ext(1) ,0,dirc,1)
	if go_rql gt 0 then begin
		go_v2(0)=-1
		go_v2(1)= 8
		bb=sl_psizm(arel,arel_z,1,go_rqm,go_v2(1),-1,-1,-1)
		bb=sl_cellget(-go_rql,arel_z,go_v2,arel,0)
		if (bb) and (arel(0) gt 0) and (arel(0) lt 100000) $
			and (arel(1) ge 0) and (arel(1) lt 100000) $
			and (arel(2) ge 0) and (arel(2) lt 100000) then begin
			bb=sl_psizm(arel,arel_z,2,go_rqm,arel(0)+1,go_v2(1),-1,-1)
			if bb then begin
			   bb=sl_iopoint( go_rql,0,0)
			   bb=sl_cellget(-go_rql,arel_z,go_v2,arel,0)
			   b =1
		endif   &  endif
		if not b then bb=sl_dd(2,   arel,arel_z)
		bb=sl_iofree(go_rql)
	endif
return, b
end
;
;
function sl_getccp4, m_u,idk,dm_ins,rl_ins,erey,zimg,m_sr,m_bo
;******* **********
;**
common	my_ccp4, ccsiz,ccrect,ccskp,ccnc
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**	Get Header
;**	--- ------
	ccrect(0)=rl_ins(0,idk)
	ccrect(1)=16
	bb=sl_psizm(areb ,areb_z,1,1024/4,ccrect(1),-1,-1,-1)
	bb=sl_cellget(m_u,areb_z,ccrect,areb,0)
;**
	ia0=areb(0) & ia1=areb(1) & ia2=areb(2) & ia3=areb(3) & ia23=areb(23)
	if m_bo ne 0 then begin bb= sl_swapint(ia0 ,1,0,0,16)
				bb= sl_swapint(ia1 ,1,0,0,16)
				bb= sl_swapint(ia2 ,1,0,0,16)
				bb= sl_swapint(ia3 ,1,0,0,16)
				bb= sl_swapint(ia23,1,0,0,16) & endif
	if (bb) and (ia0 gt 1) and (ia3 ge 0) and (ia3 le 4)  $
		and (ia1 gt 1) and (ia2 ge 1) $
		and (ia3 ne 3) then begin
		dm_ins(0,idk)= ia0
		dm_ins(1,idk)= ia1
		zimg	     = ia2
		if m_sr gt 1  then zimg=zimg-m_sr+1
		if zimg le 0  then zimg=1
		if ia3 eq 0 then rl_ins(1,idk)=2  else $
		if ia3 eq 1 then rl_ins(1,idk)=4  else $
		if ia3 eq 2 then rl_ins(1,idk)=8  else $
		if ia3 eq 4 then rl_ins(1,idk)=32
		ccskp=ia23  +1
		bb=sl_dd(2,areb,areb_z)
;**	Get Data
;**	--- ----
		ccsiz(*) =0
		ccrect(1)=rl_ins(1,idk)
		if m_sr gt 1 then begin
;**		   Skip Frames
;**		   ---- ------
;		   bb=sl_psizm(erey,ccsiz,3,dm_ins(0,idk),dm_ins(1,idk),m_sr-1,$
;					    rl_ins(1,idk),-1)
;		   bb=sl_cellget(m_u,ccsiz,ccrect,erey,0)
		   ccnc =sl_typb(rl_ins(1,idk))
		   ccskp=ccskp + dm_ins(0,idk)*dm_ins(1,idk)*(m_sr-1)*ccnc
		   endif
		if bb then begin
;**		   Read data
;**		   ---- ----
		 if zimg gt 1 then $
		   bb=sl_psizm(erey,ccsiz,3,dm_ins(0,idk),dm_ins(1,idk),zimg,$
					    rl_ins(1,idk),-1)	  else  $
		   bb=sl_psizm(erey,ccsiz,2,dm_ins(0,idk),dm_ins(1,idk),$
					    rl_ins(1,idk),-1,-1)
		 bb=sl_stream  (m_u,ccsiz,ccrect,erey,-ccskp)
		 if m_bo ne 0 then $
		   bb=sl_swapint(erey,ccsiz(1),ccsiz(2),zimg,rl_ins(1,idk))
		endif else ab=sl_dd(2,erey,ccsiz)
	endif else bb=0
return,	bb
end
;
;
;
function sl_tifb,	idx
;******* *******
;**
common my_tif,	a,b,c,cnt,flg,inv,nt,tag,typ,rect,val,x,$
		nbuf,off0,off1,offr,rec,uni,sz
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
	if (idx lt off0) or (idx gt off1) then begin
	    if off1 lt 0 then bb=sl_psizm(areb,areb_z,1,nbuf*rec,rect(1),-1,-1,-1)
	    i   =idx/rec
	    if  (i ne offr+nbuf) then bb=sl_iopoint(uni,i,rec)
	    offr=i
	    off0=i * rec
	    off1=(i+nbuf)*rec -1
	    bb  =sl_cellget(uni,areb_z,rect,areb,0)
	    if not bb then areb(*)=0
	endif
;**
return, areb(idx-off0)
;**
end
;
function sl_w,		idx,flw
;******* ****
;**
common my_tif,	a,b,c,cnt,flg,inv,nt,tag,typ,rect,val,x,$
		nbuf,off0,off1,offr,rec,uni,sz
;**
	if  flw eq 3 then $
	 if inv eq 0 then return,sl_tifb(idx) + sl_tifb(idx+1)*a $
		     else return,sl_tifb(idx+1) + sl_tifb(idx)*a else $
	if (flw eq 1) or (flw eq 2) or (flw eq 6) or (flw eq 7)  then $
			  return,sl_tifb(idx) else $
	if  flw eq 8 then $
	 if inv eq 0 then return,fix(sl_tifb(idx) + sl_tifb(idx+1)*a) $
		     else return,fix(sl_tifb(idx+1) + sl_tifb(idx)*a) $
	else if inv eq 0 then $
	 return,sl_tifb(idx)+sl_tifb(idx+1)*a+sl_tifb(idx+2)*b+sl_tifb(idx+3)*c $
	     else $
	 return,sl_tifb(idx+3)+sl_tifb(idx+2)*a+sl_tifb(idx+1)*b+sl_tifb(idx)*c
end
;
pro sl_tiftag,	idx
;** *********
;**
common my_tif,	a,b,c,cnt,flg,inv,nt,tag,typ,rect,val,x,$
		nbuf,off0,off1,offr,rec,uni,sz
;**
common my_tag,  t_bps,t_map,t_cps,t_grc,t_gru,t_imx,t_imy,t_pho,t_spp,t_pnc,t_ids,$
		t_fms,t_fmc,t_nbs,t_ort,t_rps,t_str,t_mda,t_mdn,t_mdd,t_mdt,t_tim,$
		t_inf,t_cn1,t_cn2,t_cn3
;**
bb=1
	val=sl_w(idx,typ)
	case tag of
;** Image width
	256:	t_imx=val
;** Image lenght
	257:  	t_imy=val
;** Bits/sample
	258:	t_bps=val
;** Compression
	259:	t_cps=val
;** Photometrie
	262:	t_pho=val
;** Image Description
	270:	begin if cnt gt 4 then t_ids=sl_w(idx,4) else t_ids=idx
		      t_cn1=cnt & end
;** Strip offset
	273:	begin  t_fms=typ & t_nbs=cnt
		if cnt gt 1 then t_str=sl_w(idx,4) else t_str=val  & end
;** Orientation
	274:	t_ort=val
;** Samples/pixel
	277:	t_spp=val
;** Rows/strip
	278:	t_rps=val
;** Strip byte count
	279:	begin  t_fmc=typ
		if cnt gt 1 then t_sbs=sl_w(idx,4) else t_sbs=val  & end
;** Planar Configuration
	284:	t_pnc=val
;** Gray resp unit
	290:	t_gru=val
;** Gray resp curve
	291:	t_grc=sl_w(idx,4)
;** Date Time
	306:	begin t_tim=sl_w(idx,4) & t_cn2=cnt & end
;** Color map
	320:	t_map=sl_w(idx,4)
;** Data  type Mol.Dyn.
	33445:	t_mdt=val
;** Scale addr Mol.Dyn.
	33446:	t_mda=val
;** Sampleinfo Mol.Dyn.
	33449:	begin if cnt gt 4 then t_inf=sl_w(idx,4) else t_inf=idx
		      t_cn3=cnt & end
;**
	else:	bb=0
	endcase
return
end
;
function sl_tifcell, m_u,idk,dm_ins,rl_ins,erey,zimg,m_sr,m_bo,m_hyst
;******* **********
;**
common my_tif,	a,b,c,cnt,flg,inv,nt,tag,typ,rect,val,x,$
		nbuf,off0,off1,offr,rec,uni,sz
;**
common my_tag,  t_bps,t_map,t_cps,t_grc,t_gru,t_imx,t_imy,t_pho,t_spp,t_pnc,t_ids,$
		t_fms,t_fmc,t_nbs,t_ort,t_rps,t_str,t_mda,t_mdn,t_mdd,t_mdt,t_tim,$
		t_inf,t_cn1,t_cn2,t_cn3
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
flg =0
rec =rl_ins(0,idk)
if rec le 0 then rec=512
rect(0)=rec
rect(1)=2
nbuf=1024/rec & if nbuf eq 0 then nbuf=1
off0= 0
off1=-1
offr= 0
uni =m_u
inv = 0
inv =sl_w(0,1)
if inv eq 73 then inv=0 else $
if inv eq 77 then inv=1 else inv=-1
x   =sl_w(2,3)
;**
if (x eq 42) and (inv ge 0) then begin
	x =sl_w(4,4)
	if m_sr gt 1 then begin
;**	Skip images.
;**	---- ------
	   i=2
	   while (i le m_sr) and (x gt 0) do begin
		  x=x+ sl_w(x,3)*12 +2
		  x=   sl_w(x,4)
		  i=i+1
	   endwhile
	endif
	if x gt 0 then begin
;**	Init params.
;**	---- ------
	   t_bps= 1	& t_map= 0	& t_cps= 1	& t_grc= 0
	   t_gru= 2	& t_imx= 0	& t_imy= 0	& t_pho=-1
	   t_spp= 1	& t_str= 0	& t_rps= long(2)^24
	   t_ort= 1     & t_nbs= 0	& t_pnc= 1
	   t_ids=-1	& t_tim=-1
;
	   t_inf=-1	& t_mdt=-1	& t_mda= 0      & t_mdn= 1.     & t_mdd= 1
;**	Get  tags.
;**	---  ----
	   nt=sl_w(x,3)
	   x =x+2
  	   for  i=1,nt do begin
		tag=sl_w(x,3)
		typ=sl_w(x+2,3)
		cnt=sl_w(x+4,4)
		sl_tiftag, x+8
		x  =x+12
	   endfor
	   x= sl_w(x,4)
;**	   Get  private tags.
;**	   ---  ------- ----
	   if x gt 0 then begin
	   	nt=sl_w(x,3)
	   	x =x+2
		tag=sl_w(x,3)
		if tag ge 32768 then begin
	  	   for  i=1,nt do begin
			tag=sl_w(x,3)
			typ=sl_w(x+2,3)
			cnt=sl_w(x+4,4)
			sl_tiftag, x+8
			x  =x+12
	   		endfor
		   if t_mda gt 0 then begin t_mdn=1.*sl_w(t_mda,4)
					    t_mdd=   sl_w(t_mda+4,4) & endif
           endif & endif
	   if m_sr lt 0 then begin
;**	Just get header.
;**	---- --- ------
		tmp1='' & tmp2='' & tmp3=''
		if t_ids gt 0 then begin tmp1=bytarr(t_cn1)
					 for i=0,t_cn1-1 do tmp1(i)=sl_w(t_ids+i ,1) & endif
		if t_tim gt 0 then begin tmp2=bytarr(t_cn2)
					 for i=0,t_cn2-1 do tmp2(i)=sl_w(t_tim+i ,1) & endif
		if t_inf gt 0 then begin tmp3=bytarr(t_cn3)
					 for i=0,t_cn3-1 do tmp3(i)=sl_w(t_inf+i ,1) & endif
		m_hyst=[' '+sl_strf(tmp1)+' on '+sl_strf(tmp2),sl_strf(tmp3),' --------------------']
		m_hyst=[m_hyst,' Image width        = '+sl_stbr(sl_strf(t_imx),2)]
		m_hyst=[m_hyst,' Image lenght       = '+sl_stbr(sl_strf(t_imy),2)]
		m_hyst=[m_hyst,' ']
		m_hyst=[m_hyst,' Bits/sample        = '+sl_stbr(sl_strf(t_bps),2)]
		m_hyst=[m_hyst,' Samples/pixel      = '+sl_stbr(sl_strf(t_spp),2)]
		m_hyst=[m_hyst,' Number of Strips   = '+sl_stbr(sl_strf(t_nbs),2)]
		m_hyst=[m_hyst,' Rows  per Strip    = '+sl_stbr(sl_strf(t_rps),2)]
		m_hyst=[m_hyst,' Orientation        = '+sl_stbr(sl_strf(t_ort),2)]

		if (t_nbs eq 1)  then m_hyst=[m_hyst,' ',' Data Byte Offset   ='+sl_stbr(sl_strf(t_str),2)]

		if (t_cps eq 1)     then tmp1=' No Compression     ( 1 )' else $
		if (t_cps eq 2)     then tmp1=' CCITT Compression  ( 2 )' else $
		if (t_cps eq 5)     then tmp1=' LZW Compression    ( 5 )' else $
		if (t_cps eq 32773) then tmp1=' PackBit Compression( 32773 )'
		if (t_pho eq 0)     then tmp2=' Grayscale Image    ( 0 )' else $
		if (t_pho eq 1)     then tmp2=' Grayscale Image    ( 1 )' else $
		if (t_pho eq 2)     then tmp2=' RGB model          ( 2 )' else $
		if (t_pho eq 3)     then tmp2=' Sample color Index ( 3 )' else $
		if (t_pho eq 4)     then tmp2=' Sample Mask        ( 4 )'
		tmp3=' ' & tmp4=' '
		if (t_mdt eq 0)     then tmp3=' PhosphorImager Logarithmic'
		if (t_mdt eq 1)     then tmp3=' PhosphorImager Linear'
		if (t_mdt eq 2)     then tmp3=' PhosphorImager Square Root'
		if (t_mdt eq 128)   then tmp3=' Densitometer   Logarithmic'
		if (t_mda gt 0)     then tmp4=' Scale Factor       = '+sl_stbr(sl_strf(t_mdn),2)+ ' / ' $
								      +sl_stbr(sl_strf(t_mdd),2)

		m_hyst=[m_hyst,' ',tmp1,tmp2,' ',tmp3,tmp4]

		if (t_grc ne 0)   then m_hyst=[m_hyst,' ',' Response Curve exists !']
	   endif
;**	Validate.
;**	--------
	   if ((t_bps eq 8) or  (t_bps eq 16) or  (t_bps eq 32))   and $
	       (t_cps eq 1) and (t_imx gt 0 ) and (t_imy gt 0 )    and $
	      ((t_pho eq 0) or  (t_pho eq 1 ) or  (t_pho eq 3 ))   and $
	       (t_spp eq 1) and (t_nbs ge 1 ) and (m_sr  ge 0 )    then begin

;**	Get data buffer.
;**	--- ---- ------
		zimg = 1
		dm_ins(0,idk)=t_imx
		dm_ins(1,idk)=t_imy
		if t_bps eq 8   then begin
			typ=1 & cnt=1 & rl_ins(1,idk)=2  & endif else $
		if t_bps eq 16  then begin
			typ=3 & cnt=2 & rl_ins(1,idk)=4  & endif else $
		if t_bps eq 32  then begin
			typ=4 & cnt=4 & rl_ins(1,idk)=16 & endif
;
		flg=sl_psizm(erey,sz,2,t_imx,t_imy,rl_ins(1,idk),-1,-1)

;**	many strips. (test)
;**	---- ------
		if t_nbs gt 1 then if t_rps le t_img/t_nbs+1 then begin
		   flg=sl_psizm(arei,arei_z,2,t_nbs,2,16,-1,-1)
		   idx=t_str
		   if t_fms eq 3 then j=2 else j=4
		   for i=0,t_nbs-1 do arei(i,0)=sl_w(idx+i*j,t_fms)
		   idx=t_sbs
		   if t_fmc eq 3 then j=2 else j=4
		   for i=0,t_nbs-1 do arei(i,1)=sl_w(idx+i*j,t_fmc)
		endif else t_rps = t_img/t_nbs

;**	Read.
;**	----
		if t_nbs gt 1  then begin
		 for k=0,t_nbs-1 do begin
		  idx=arei(k,0)
		  for j=t_rps*k , t_rps*(k+1)-1 do $
		   for i=0,t_imx-1 do begin
			   erey(i,j)=sl_w(idx,typ)
			   idx=idx+cnt
		   endfor
		 endfor
;
		endif else begin
;		 i  =t_str/rec
;		 bb =sl_iopoint(uni,i,rec)
;		 flg=sl_stream (uni,sz,rl_ins(*,idk),erey,-(t_str - i*rec +1))
		 bb =sl_iopoint(uni,t_str,1)
		 flg=sl_stream (uni,sz,rl_ins(*,idk),erey,-1)
;		 Swap
		 if (m_bo ne 0) and (inv eq 0) and ((sz(sz(0)+1) eq 4) or (sz(sz(0)+1) eq 16))$
			 then bb=sl_swapint(erey,sz(1),sz(2),0,sz(sz(0)+1))
;
		endelse
;**	Positive.
;**	--------
		if (rl_ins(1,idk) eq 4) then begin
		   his=where(erey lt 0)
		   if his(0) ge 0 then begin
		      bb=sl_d_p(52,erey,sz,0,0,0,0)
		      rl_ins(1,idk)= sz(sz(0)+1)
		   endif
		endif
;**	Orientation.
;**     -----------
		if (t_ort ge 2) and (t_ort le 8) then begin
			if t_ort eq 2 then i= 5  else $
			if t_ort eq 3 then i= 2  else $
			if t_ort eq 4 then i= 7  else $
			if t_ort eq 5 then i= 4  else $
			if t_ort eq 6 then i= 1  else $
			if t_ort eq 7 then i= 6  else $
			if t_ort eq 8 then i= 3
			bb=sl_revs(erey,sz(1),sz(2),sz(sz(0)+1), i)
		endif
;**	Private data type.
;**	------- ---- ----
		if t_mdt ge 0 then begin	rect(0)=8
;			Float
			bb=sl_d_p(38,erey,sz,0, rect,0,0)
			rl_ins(1,idk)=8
;			Square
			if (t_mdt eq 2) then bb=sl_d_p(33,erey,sz,0)
;			Exponential
			if (t_mdt eq 0)  or (t_mdt eq 128) then $
					     bb=sl_d_p(31,erey,sz,0)
;			Scale
			if (t_mdn ne 1) then erey(0,0)=erey*t_mdn
			if (t_mdd ne 1) then erey(0,0)=erey/t_mdd
		endif
		sz(*)=0
	   endif
	endif
endif
return, flg
end
;
;
;
function sl_annot,win
;******* ********
;**
common  my_annot, an_gm,an_gh,an_gf,an_ttl1,an_ttl2,an_xlab,an_ylab,an_ttm, $
		  an_zlab,an_com1,an_com2,an_unit,an_offs,an_i,an_r,an_f6
;**
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
an_r=0
	bb=sl_str_to_long(-1,an_ttl1,tv_win,win,70 ,64)
	bb=sl_str_to_long(-1,an_ttl2,tv_win,win,86 ,64)
	bb=sl_str_to_long(-1,an_xlab,tv_win,win,102,64)
	bb=sl_str_to_long(-1,an_ylab,tv_win,win,118,64)
	bb=sl_str_to_long(-1,an_zlab,tv_win,win,134,40)
	bb=sl_str_to_long(-1,an_com1,tv_win,win,144,40)
	bb=sl_str_to_long(-1,an_com2,tv_win,win,154,40)
;
	bb=sl_tvset(6,-1)
	bb=sl_tvmenun(5,5,an_gf ,an_ttm,tv_xp,tv_yp/4)
;
	an_i=0
	m=3
	while an_i ge 0 do begin
	  an_gm(1) = an_gh(1)  + an_ttl1 +'!6'
	  an_gm(3) = an_gh(3)  + an_ttl2 +'!6'
	  an_gm(5) = an_gh(5)  + an_xlab +'!6'
	  an_gm(7) = an_gh(7)  + an_ylab +'!6'
	  an_gm(9) = an_gh(9)  + an_zlab +'!6'
	  an_gm(10)= an_gh(10) + an_com1 +'!6'
	  an_gm(11)= an_gh(11) + an_com2 +'!6'
	  an_gm(13)= an_gh(13)
	  an_gm(14)= an_gh(14)
	  bb	   = sl_tvset(6,-1)
	  an_i	   = sl_tvmenui(0,m,an_gm,an_ttm,tv_xp,tv_yp/2)
	  m=4
	  case an_i of
	  1 :	begin
	  	bb =sl_wgaccept(0,an_gh(an_i),1,1,an_ttl1)
		bb =sl_str_to_long( 1,an_ttl1,tv_win,win,70 ,64)
		bb =sl_str_to_long(-1,an_ttl1,tv_win,win,70 ,64)
		end
	  3 :   begin
		bb =sl_wgaccept(0,an_gh(an_i),1,1,an_ttl2)
		bb =sl_str_to_long( 1,an_ttl2,tv_win,win,86 ,64)
		bb =sl_str_to_long(-1,an_ttl2,tv_win,win,86 ,64)
		end
	  5 :   begin
		bb =sl_wgaccept(0,an_gh(an_i),1,1,an_xlab)
		bb =sl_str_to_long( 1,an_xlab,tv_win,win,102,64)
		bb =sl_str_to_long(-1,an_xlab,tv_win,win,102,64)
		end
	  7 :   begin
		bb =sl_wgaccept(0,an_gh(an_i),1,1,an_ylab)
		bb =sl_str_to_long( 1,an_ylab,tv_win,win,118,64)
		bb =sl_str_to_long(-1,an_ylab,tv_win,win,118,64)
		end
	  9 :   begin
		bb =sl_wgaccept(0,an_gh(an_i),1,1,an_zlab)
		bb =sl_str_to_long( 1,an_zlab,tv_win,win,134,40)
		bb =sl_str_to_long(-1,an_zlab,tv_win,win,134,40)
		end
	  10:   begin
		bb =sl_wgaccept(0,an_gh(an_i),1,1,an_com1)
		bb =sl_str_to_long( 1,an_com1,tv_win,win,144,40)
		bb =sl_str_to_long(-1,an_com1,tv_win,win,144,40)
		end
	  11:   begin
		bb =sl_wgaccept(0,an_gh(an_i),1,1,an_com2)
		bb =sl_str_to_long( 1,an_com2,tv_win,win,154,40)
		bb =sl_str_to_long(-1,an_com2,tv_win,win,154,40)
		end
	  13:   begin
		end
	  14:   begin
		end
	  16:	an_i=-1
	  17:   begin an_i=-1 & an_r=-1 & tv_win(69,win)=0 & end
	  18:   begin an_i=-1 & an_r= 1 & tv_win(69,win)=1 & end
	else:
	  endcase
	endwhile
;
	bb=sl_tvset(6,0)
	bb=sl_tvdmenu(0)
	bb=sl_tvdmenu(5)
;
return ,an_r
end
;
;
;
function sl_matx ,k,dirc,areins,dm_ins,zd,dy_ins,rl_ins ,menfrm,z_pos,z_bo,z_sr, z_hyst
;******* *******
;** k	   index
;** dirc   returned filename
;** areins data buffer
;** dm_ins [dim1,dim2]
;** zd	    dim3
;** dy_ins ['filespec for input','returned filename']
;** rl_ins [recl , type]
;** menfrm input format or -1 to get menu.
;** z_pos  integer positif
;** z_bo   byte order 1 to reverse
;** z_sr   byte offset or record offset

common  my_matx,zstring,zimg,excor,excnc,exmat,extyp,exconf,exmit,$
		exfrm,exfri,m_frm,m_fri ,m_v6,m_pos,m_bo,$
		m_dm1,m_dm2,m_my,m_i,m_j,m_nc,m_rec,m_sr,m_typ,m_u,m_x1
;**
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	m_my	   = dy_ins(0,k)
	m_dm1	   = dm_ins(0,k)
	m_dm2	   = dm_ins(1,k)
	m_typ	   = rl_ins(1,k)
	m_rec	   = rl_ins(0,k)
	repeat begin
	flg	   = 0
	if menfrm ge 0 then begin m_frm=menfrm & zimg =zd   & m_pos=z_pos
				  m_bo =z_bo   & m_sr =z_sr
	endif else begin
;**
	 m=3
	 repeat begin
	  if  m_typ eq  2 then  m_j=1  else if m_typ eq  4 then m_j=2 else $
	  if  m_typ eq  8 then  m_j=5  else if m_typ eq 16 then m_j=4 else $
	  if  m_typ eq 32 then  m_j=7  else if m_typ eq 64 then m_j=6
	  if  m_pos eq  1 then  begin
				m_j=3 & m_typ=4 & rl_ins(1,k) = m_typ & endif
	  m_nc	   = excnc(m_j)
;
;	'1} Change file-name :'
	  exmat(0) =exmit(0) + m_my + $
		    '                                                  '
;	'2} Dimensions x,y,z :'
	  exmat(1) =exmit(1) +'(' +    sl_str(m_dm1 ,'(i5)')+' , ' $
				  +    sl_str(m_dm2 ,'(i5)')+' , ' $
				  +    sl_str(zimg  ,'(i3)')+' )'
;	'3} Change the type  :'
	  if m_bo eq 0 then $
	  exmat(2) =exmit(2) +extyp (m_j)   else $
	  exmat(2) =exmit(2) +extyp (m_j)  +extyp(m_bo)
;
;	'4} Record size(byte):'
	  exmat(3) =exmit(3) +sl_str(rl_ins(0,k),'(i5)')
;
;	'5} Starting record  :'
	  exmat(4) =exmit(4) +sl_str(m_sr       ,'(i5)')
;
;	'5} Byte offset 1->n :'
	  if (rl_ins(0,k) eq 0) or (m_frm eq 3) then $
	  exmat(4) =exmit(5) +sl_str(m_sr       ,'(i5)')
;
;	'6} Format  of data  :'
	  exmat(5) =exmit(6) +exfrm (m_fri)

	  m_i	   = sl_tvmenui(0,m,exmat,exmit(7),tv_xp,tv_yp/2)
	  m=4
	  case  m_i of
		0:  begin bb=sl_wgaccept(0,exmit(0),1,1,m_my)
			  if bb eq 1 then dy_ins(0,k)=m_my  & end
		1:  begin bb=sl_wgaccept(0,exmit(1),16,3,m_dm1,m_dm2,zimg)
			  if bb eq 1 then begin
			   if m_dm1 eq 0 then m_dm1=1
			   if m_dm2 eq 0 then m_dm2=1
			   if zimg  eq 0 then zimg =1
			   dm_ins(0,k) = m_dm1 & dm_ins(1,k) = m_dm2
			   if   (m_frm eq 1) or  (m_frm eq 4) then $
				 rl_ins(0,k)= dm_ins(0,k)     else $
			   if	(m_frm ne 0) and (m_frm ne 2) and  $
				(m_frm ne 3) and (m_frm ne 6) then $
				 rl_ins(0,k)= dm_ins(0,k) * m_nc
			  endif & end
		2:  begin m_j = sl_tvmenul(5,3,extyp,'Your choice',tv_xp,tv_yp/2)
			  m_j=m_j-500
			  bb  = sl_tvdmenu(5)
			  if m_j ge 0 then j = excor(m_j) else j =0
			  if  j ne 0 then begin
			   if j ne 6 then begin
			      m_bo  = 0
			      m_typ = j
			      m_nc  = excnc(m_j)
			      if m_typ eq 5 then begin m_typ=4 & m_pos=1
			  			 endif else	 m_pos=0
			      rl_ins(1,k) = m_typ
			      if (m_frm ne 0) and (m_frm ne 2) and  $
				 (m_frm ne 3) and (m_frm ne 6) then $
			          rl_ins(0,k) = m_nc * dm_ins(0,k)
			   endif else m_bo= m_j
			  endif
			  end
		3:  begin bb=sl_wgaccept(0,exmit(3),16,1,m_rec)
			  if bb eq 1 then rl_ins(0,k)=m_rec & end
		4:  begin if (rl_ins(0,k) eq 0) or (m_frm eq 3) then $
				 bb=sl_wgaccept(0,exmit(5),16,1,m_sr) $
			  else   bb=sl_wgaccept(0,exmit(4),16,1,m_sr)
			  if m_sr le 0 then m_sr=long(1)
			  end
		5:  begin i =sl_tvmenul(5,3,exfrm,'Your choice',tv_xp,tv_yp/2)
			  i =i-500
			  bb=sl_tvdmenu(5)
			  if i ge 0 then j =exfri(i) else j=-1
			  if j ge 0 then begin m_frm=j  & m_fri=i & endif
			  if (m_frm eq 0)		  then  rl_ins(0,k)=0
			  if (m_frm eq 2)		  then  m_bo       =9
			  if (m_frm eq 2) or (m_frm eq 3) then  rl_ins(0,k)=512
			  if (m_frm eq 5)		  then  rl_ins(0,k)=-1
			  if (m_frm eq 6)		  then  rl_ins(0,k)=1024
			  if (m_frm eq 7) then  begin	  m_dm1=1200
							  m_dm2=1200
							  zimg =1
			   				  dm_ins(0,k)=m_dm1
							  dm_ins(1,k)=m_dm2
							  m_typ= 4 & m_pos=1
							  rl_ins(1,k)=m_typ
							  rl_ins(0,k)=0
							  m_sr =long(2401)
							  m_frm=0
						endif
			  end
		7:  m_i=-1
		8:  begin m_i=-1 & flg=-1 & end
		9:  begin bb =sl_tvdmenu(0) & return ,flg & end
		else: if m_i lt 0 then begin
			  bb =sl_tvdmenu(0) & return ,flg & endif
	  endcase
	  bb=sl_ioclear(0)
	 endrep  until m_i  lt 0
	 bb =sl_tvdmenu(0)
	endelse
;**
;**	Open
	if (m_frm eq 1) or (m_frm eq 4) or (m_frm eq 8) then $
		m_u  =  sl_filr (dy_ins(0,k),'',0,dirc,1) else $
	if (m_frm eq 6) then  $
		m_u  =  sl_filr (dy_ins(0,k),'',0,dirc,0)   $
	else	m_u  =  sl_filr (dy_ins(0,k),'',0,dirc,m_frm)
	if m_u gt 0 then  begin
	 if flg lt 0 then begin
;**	   Delete
	   bb =sl_iofree (m_u)
	   exconf(0)='Remove ' + dirc + '     '
	   bb =sl_tvmenul(5,3,exconf,' ',tv_xp,tv_yp/2)
	   bb=bb-500
	   if bb eq 0 then bb =sl_run('d',dirc,'',0,1)
	   bb=sl_tvdmenu(5)
	 endif else begin
;**	   Read
	   bb=sl_tvmenunw(5,0,m_x1,' ',tv_xp,tv_yp/2)
	   if zimg le 1  then    m_v6(0)=2        else m_v6(0)=3
	   m_v6(1)=dm_ins(0,k) & m_v6(2)=dm_ins(1,k) & m_v6(3)=zimg
	   if m_frm eq 0 then flg=sl_cellget( m_u,m_v6,rl_ins(*,k),areins,m_sr)
	   if m_frm eq 5 then flg=sl_cellget( m_u,m_v6,rl_ins(*,k),areins,m_sr)
	   if m_frm eq 1 then flg=sl_cellget(-m_u,m_v6,rl_ins(*,k),areins,m_sr)
	   if m_frm eq 2 then flg=sl_tifcell( m_u,k,dm_ins,rl_ins,areins,zimg,$
						    m_sr,m_bo,z_hyst)
	   if m_frm eq 3 then flg=sl_stream ( m_u,m_v6,rl_ins(*,k),areins,m_sr)
	   if m_frm eq 4 then flg=sl_getord (-m_u,k,dm_ins,rl_ins,areins,zimg,$
						    m_sr,m_frm)
	   if m_frm eq 8 then flg=sl_getord (-m_u,k,dm_ins,rl_ins,areins,zimg,$
						    m_sr,m_frm)
	   if m_frm eq 6 then flg=sl_getccp4( m_u,k,dm_ins,rl_ins,areins,zimg,$
						    m_sr,m_bo)
	   bb =sl_iofree (m_u)
	   if zimg le 1  then    m_v6(0)=2        else m_v6(0)=3
	   m_v6(1)=dm_ins(0,k) & m_v6(2)=dm_ins(1,k) & m_v6(3)=zimg
	   m_v6(m_v6(0)+1)=rl_ins(1,k)

	   if m_sr ge 0 then begin
;**
;**	   Swap integers
	   if (m_bo  ne 0) then $
		if (m_frm ne 2) and (m_frm ne 4) and (m_frm ne 6) $
				and (m_frm ne 8) then $
		bb=sl_swapint(areins,m_v6(1),m_v6(2),zimg,m_v6(m_v6(0)+1))
;**
;**	   Positive
	   if (m_pos eq 1) then if (rl_ins(1,k) eq 4)  then begin
		bb=sl_d_p(52,areins,m_v6,0,0,0,0)
		rl_ins(1,k)=m_v6(m_v6(0)+1)
		endif
	   bb =sl_tvdmenunw(5)

	   endif
	 endelse
	endif
	endrep  until flg  ge 0
	dy_ins(1,k)= dirc
	zd	   = zimg
	if menfrm lt 0 then begin menfrm=m_frm & z_pos=m_pos
				  z_bo =m_bo   & z_sr =m_sr  & endif
;**
return, flg
end
;
;
;
;
function sl_chk_win,  w,xx,yy ,fl ,wex
;******* **********   * ** **  **  ***
;**
;** Check where is the cursor.
;** ----- ----- -- --- ------
;**
common my_click, bb,nb,n2,rti,st,tmtl,x,xc,xd,xs,y,yp,yl,zerr,w_cw,w_no,w_ft,$
		 tc_7,bo,tc_ttl,st2,tc_x03,tc_y03,tc_x13,tc_y13,tc_x04,tc_y04,$
		 tc_sz,tc_are,tc_vsz,tc_sel
;**
	common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
;**	TVSELS assumes windows exist.
;**	BO  ge  0	       means tv_win(*,BO) selected.
;**     BO  ge  0  but XX lt 0 means size changed.
;**	BO  eq -1  	       means WEX  selected.
;**     FL  eq  1              means something selected
	bb=sl_tvget(3,w_no)
	bb=sl_tvget(7,yp)
	bo=-2
	fl= 0
	nb=tv_wsz(1)
	if w   eq -2 then nb= 0
	if wex le  0 then n2= 0  else n2=-1
	yl=-1
	while n2 lt nb do begin
	   if n2 ge 0 then w_cw=tv_win(0,n2) else w_cw=wex
	   if (w_cw gt 0) and (w_cw ne w) then begin
		bb=sl_tvsel(w_cw)
		if bb eq 1 then begin
 		   bb=sl_tvget(28,x)
		   bb=sl_tvget(29,y)
		   if n2 ge 0 then $
			if (x gt 0) then $
			   if (tv_win(28,n2) ne x) or $
			      (tv_win(29,n2) ne y) then begin fl=1 & xx=-1
                           endif
		   if fl ne 1 then begin
			bb=sl_tvgcur(xx,yy,i,0)
			if (  xx ge 0 ) or (w eq -2) then begin  xs=tc_sz
			   if n2 ge 0 then st= tc_sel(0) else st=tc_sel(1)
			   if n2 ge 0 then tc_x03(0)=tv_win(28,n2)/20 $
				      else tc_x03(0)=x-320/2-xs/2
			   tc_x03(1)=tc_x03(0)*20/17   & tc_x03(2)=tc_x03(1)+xs
			   tc_x13(0)=tc_x03(0)         & tc_x13(1)=tc_x03(1)
			   tc_x13(2)=tc_x13(1)
			   if n2 ge 0 then tc_y03(0)=tv_win(29,n2)*19/20 $
				      else tc_y03(0)=y-160/3+xs/2
			   tc_y03(1)=tc_y03(0)*16/17   & tc_y03(2)=tc_y03(1)
			   tc_y13(0)=tc_y03(0)         & tc_y13(1)=tc_y03(1)-2
			   tc_y13(2)=tc_y13(1)-xs/2
			   tc_x04(0)=tc_x03(1)+1       & tc_x04(1)=tc_x03(2)
			   tc_x04(2)=tc_x04(1)         & tc_x04(3)=tc_x04(0)
			   tc_y04(0)=tc_y13(1)         & tc_y04(1)=tc_y04(0)
			   tc_y04(2)=tc_y13(2)         & tc_y04(3)=tc_y04(2)
			   bb=sl_tvmod(0,6)
			   bb=sl_tvpol(3,tc_x03,tc_y03,tv_nc-1,0)
			   bb=sl_tvpol(3,tc_x13,tc_y13,tv_nc-1,0)
			   xc=tv_nc-1
			   j =0
			   if tv_flg(3) ge 0 then begin
;				j=sl_tvsel(tv_flg(3))
				j=1
				if j gt 0 then  begin
				  bb=sl_tvmod(0,3)
;				  tc_7(0)=tc_x04(0) &  tc_7(1)=tc_y04(2)
;				  tc_7(2)=tc_x04(1)-tc_x04(0)+1
;				  tc_7(3)=tc_y04(1)-tc_y04(2)+1
;				  tc_7(4)=0      &  tc_7(5)=0   &  tc_7(6)=w_cw
;				  bb=sl_tvmov(tc_7)
;				  bb=sl_tvsels(w_cw)
				  bb=sl_tvset(7,0)
		   		  tc_are(0,0)=sl_tvread(tc_x04(0),tc_y04(2),$
						        xs,xs/2+1)
				  bb=sl_tvset(7,yp)
				  bb=sl_tvline( tc_x04,tc_y04,4,0,1)
			          bb=sl_tvpol(4,tc_x04,tc_y04,tv_flg(2)-2,0)
				  bb=sl_tvmod(0,6)
				  xc=tv_flg(2)-2
				endif
			   endif
			   if j  le  0 then bb=sl_tvpol(4,tc_x04,tc_y04,tv_nc-1,0)
			   bb=sl_tvs(tc_x04(0)+1,tc_y04(2)+xs/6 ,st,1.,0,xc)
			   bb=sl_tvmod(0,3)
			   if yl eq -1 then bb=sl_tvshap(38)
			   yl=0
			   xc=xx
			   while (xx ge 0) and (fl eq 0) do begin
			          if xx ne xc then xc=xx else $
				  if yl eq 0 then bb=sl_tvwait(1.,1,1,w_cw ,i,0) $
					     else bb=sl_tvwait(.1,1,1,w_cw ,i,0)
			          bb=sl_tvgcur(xx,yy,i,0)
					if (xx ge tc_x04(0)) and  $
					   (xx le tc_x04(1)) and  $
					   (yy le tc_y04(0)) and  $
					   (yy ge tc_y04(2)) then begin
					    if i  gt 0 then  fl=1
					    if yl eq 0 then  bb=sl_tvshap(58)
					    yl=1
					endif else begin
					    if yl eq 1 then  bb=sl_tvshap(38)
					    yl=0
					endelse
			   endwhile

			   if n2 ge 0 then begin
			    if j  le 0 then begin
			      bb=sl_tvmod(0,6)
			      bb=sl_tvs(tc_x04(0)+1,tc_y04(2)+xs/6 ,st,1.,0,tv_nc-1)
			      bb=sl_tvpol(4,tc_x04,tc_y04,tv_nc-1,0)
			    endif else begin
;				 tc_7(0)=0 & tc_7(1)=0   &  tc_7(6)=tv_flg(3)
;				 tc_7(2)=tc_x04(1)-tc_x04(0)+1
;				 tc_7(3)=tc_y04(1)-tc_y04(2)+1
;				 tc_7(4)=tc_x04(0) &  tc_7(5)=tc_y04(2)
;				 bb=sl_tvmov(tc_7)
				 bb=sl_tvset(7,0)
				 bb=sl_tvimag(tc_are,tc_vsz,tc_x04(0),tc_y04(2))
				 bb=sl_tvset(7,yp)
			         bb=sl_tvmod(0,6)
			    endelse
			    bb=sl_tvpol(3,tc_x13,tc_y13,tv_nc-1,0)
			    bb=sl_tvpol(3,tc_x03,tc_y03,tv_nc-1,0)
			    bb=sl_tvmod(0,3)
			   endif
			endif
		   endif
		   if (fl eq 1)   then begin bo=n2 & n2=nb
		   endif    else  n2=n2+1
		endif else begin
		      if n2 eq -1 then begin bo=n2 & n2=nb & fl=1
		      endif else  n2=n2+1
		endelse
	   endif else n2=n2+1
	endwhile
	if yl   ge 0 then bb=sl_tvshap(-1)
	if w_no gt 0 then bb=sl_tvsel(w_no)
return, bo
end
;
;
;
;
function sl_purge,  windn
;******* ********   *****
;**
;** Delete some windows.
;** ------ ---- -------
;**
	common tmp_purge, bb,k,n,spt
;**
	common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
			io_cur,io_ext,io_seq,io_str
;**
	n =tv_wsz(1)
	if windn lt 0   then k=-windn	else k=windn
;
	if k	 ge n   then begin
			if tv_flg(3) ge 0 then bb = sl_tvdelwn(tv_flg(3))
			tv_flg(3)=-1
			k=0
	endif	   else n=k+1
;
	for i = k , n-1 do begin
		        if (tv_win(0,i) gt 0) or (tv_win(0,i) eq -2) then begin
			         if  tv_lst   eq  tv_win(0,i) then tv_lst=0
				 if (tv_win(0,i) gt 0) then $
				 bb =sl_tvdelwn(tv_win(0,i))
				 spt=tv_win(13,i)/10
				 if (tv_win(20,i) gt 0) or (spt eq -2) then $
					bb=sl_run('d',sl_str(i,'(i2)')+ $
					   '_*',sl_stbr(io_ext(7),0),0,1)
				 tv_win(*,i) = 0
				 endif  & endfor
;**	And duplicated
	if  k gt 0 then if  windn  lt 0 then for  i = 0,tv_wsz(1)-1  do begin
		if (tv_win(10,i) eq k)  and (tv_win(20,i) eq 0)	   then begin
				 if tv_lst eq  tv_win(0 ,i)        then tv_lst=0
				 bb=sl_tvdelwn(tv_win(0 ,i))
				 spt=tv_win(13,i)/10
				 if (spt eq -2) then bb=sl_run('d',$
			         sl_str(i,'(i2)')+'_*',sl_stbr(io_ext(7),0),0,1)
				 tv_win(*,i)   =0
				 endif    &    endfor
return,1
end
;
;
;
;
function sl_cv, vl,vo,f,g,cpx
;******* *****  ** ** * * ***
	if g or cpx then vo=float(vl) $
			 else if  not f then vo=long(vl) $
				  else  vo=vl
return,1
end
;
;
;
;
function sl_xred, erey, clf, siz, fct , cpx
;******* *******  ****  ***  ***  ***   ***
;**	 get indices from a max in a reduced sub_view.
;**	 --- ------- ---- - --- -- - ------- --------
;**
common	my_xred, is,js,ns,xvl,xvm,rex
;**
;carez + erey
;care xvl,xvm
rex(0)= clf
	is=clf(0)+siz(7)
	js=clf(1)+siz(8)
	ns=clf(2)+siz(9)
	if siz(0) eq  3  then xvl = erey(is,js,ns) else xvl = erey(is,js)
	if cpx then bb=sl_cv(xvl,xvl,1,0,cpx)
;**
	for nt= ns , ns+fct(2)-1 do if nt le siz(13) then $
	for jt= js , js+fct(1)-1 do if jt le siz(14) then $
	for it= is , is+fct(0)-1 do if it le siz(15) then begin
		if siz(0) eq 3 then xvm = erey(it,jt,nt)  else xvm = erey(it,jt)
		if cpx then bb=sl_cv(xvm,xvm,1,0,cpx)
		if xvm gt xvl then begin  xvl=xvm
		   rex(0)=it-siz(7) & rex(1)=jt-siz(8) & rex(2)=nt-siz(9)
	endif & endif
return, rex
end
;
;
;
;
function sl_spacial, erey, flg, clf, vsis, rot , sxy
;******* **********  ****  ***  ***  ****  ***   ***
;**	 spacial  view for scan.
;**	 -------  ---- --- ----
common	 tmp_spacial,bb,cpx,ez,i,ii,is,j,jj,js,n,nn,ns,nt,nz,$
		     sx1,sx2,sy1,sy2,typ,vl,vm,yv
;**
common	 my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
	 tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common   my_space, si,sj,sx,sy,sz,px1,px2,py1,py2,fdx,fdy,fdz,dx,dy,vssz,res,stt
;**
common   my_keep,  rvl,rvm,vlt,vmt
;**
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;carez + erey + rvl,rvm,vl,vm
res(0)=-1 & res(1)=-1 & res(2)=-1
;**
typ=vsis(vsis(0)+1)
    if  (vsis(0) gt 1) and (vsis(0) lt 4) then begin
     if  vsis(0) eq 3  then  nz   =  vsis(3) else nz   =  1
     ez  =   nz
     if  flg gt 0  then begin
;**   Construction.
;**   ------------
        stt(0) =vsis(7) & stt(1)=vsis(8) & stt(2)=vsis(9)
        vssz(0)=vsis &    nz    =vsis(12)+1
;**	Size result.
;**	---- ------
	sx   =  vssz(10)+1  &  sy   =  vssz(11)+1 &  sz   =  nz
	fdx  =  1	    &  fdy  =  1          &  fdz  =  1
;**
	if rot lt 0 then rot=9
;		sx2=sl_min2(sl_max2(2*(tv_x     -3*sz)/(3*sx),1),sx)
;		sy2=sl_min2(sl_max2(2*(tv_y-tv_w-3*sz)/(3*sy),1),sy)
		sx2=1	   & sy2=1
		sx1=1      & sy1=0
;		sx1=sxy(0) & sx2=sxy(1) & sy1=sxy(2) & sy2=sxy(3)
	if nz eq 1 then begin   sy1=0 & rot=9 & endif
;**	Must absolutly fit the screen.
;**	---- --------- --- --- ------
;**	reduce  vssz.
;**	------  ----
	while   sx/fdx gt tv_x	    do fdx = fdx+1
	while   sy/fdy gt tv_y-tv_w do fdy = fdy+1
	while   sz/fdz gt tv_x	    do fdz = fdz+1
	while   sz/fdz gt tv_y-tv_w do fdz = fdz+1
	sx   =  sx/fdx + fdx-1
	sy   =  sy/fdy + fdy-1
	sz   =  sz/fdz + fdz-1
	si   =  sx1*(sx-1)  +  sx2*(sz-1)  +  1  + (sz+1)
	sj   =  sy1*(sx-1)  +  sy2*(sz-1)  +  sy + (sz+1)
;**	reduce  steps.
;**	------  -----
	if si gt tv_x then begin
		if sx1 gt 1 then sx1=1
		if sx2 gt 1 then sx2=1
		si    = sx1*(sx-1)   + sx2*(sz-1)  +  1  + (sz+1) & endif
	if sj gt tv_y-tv_w then begin
		if sy1 gt 1 then sy1=1
		if sy2 gt 1 then sy2=1
		sj    = sy1*(sx-1)   + sy2*(sz-1)  +  sy + (sz+1) & endif
;**	reduce  vssz again.
;**	------  ---- -----
	while si gt tv_x do begin
		fdx=fdx+1
		sx =(vssz(10)+1)/fdx+1
		si    = sx1*(sx-1)  +  sx2*(sz-1)  +  1  + (sz+1)
		sj    = sy1*(sx-1)  +  sy2*(sz-1)  +  sy + (sz+1) & endwhile
	while sj gt tv_y-tv_w  do begin
		if sy eq 1   then begin
			fdx= fdx+1
			sx = (vssz(10)+1)/fdx+1
		endif else   begin
			fdy= fdy+1
			sy = (vssz(11)+1)/fdy+1    & endelse
		si    = sx1*(sx-1)  +  sx2*(sz-1)  +  1  + (sz+1)
		sj    = sy1*(sx-1)  +  sy2*(sz-1)  +  sy + (sz+1) & endwhile
	if nz eq 1 then si=si-2
	if nz eq 1 then sj=sj-2
;**	ouf
;**	---
;**	Position into space.
;**     -------- ---- -----
	if rot le  9 then  px1= sx1 else if rot eq 10 then px1=0 else px1=-sx1
;**
	if rot eq  0 then begin px2=-sx2 & py1= 0   & py2=-sy2 & endif else $
	if rot eq  1 then begin px2= 0   & py1= 0   & py2=-sy2 & endif else $
	if rot eq  2 then begin px2=-sx2 & py1= 0   & py2=-sy2 & endif else $
	if rot eq  3 then begin px2=-sx2 & py1= 0   & py2= 0   & endif else $
	if rot eq  4 then begin px2=-sx2 & py1= 0   & py2= sy2 & endif else $
	if rot eq  5 then begin px2= 0   & py1= 0   & py2= sy2 & endif else $
	if rot eq  6 then begin px2= sx2 & py1= 0   & py2= sy2 & endif else $
	if rot eq  7 then begin px2= sx2 & py1= 0   & py2= 0   & endif else $
	if rot eq  8 then begin px2= 0   & py1= 0   & py2= 0   & endif else $
	if rot eq  9 then begin px2= sx2 & py1= 0   & py2= 0   & endif else $
	if rot eq 10 then begin px2= sx2 & py1= 0   & py2= 0   & endif else $
	if rot eq 11 then begin px2= 0   & py1= 0   & py2= 0   & endif else $
	if rot eq 12 then begin px2=-sx2 & py1= 0   & py2= 0   & endif else $
	if rot eq 13 then begin px2=-sx2 & py1= 0   & py2= sy2 & endif else $
	if rot eq 14 then begin px2= 0   & py1= 0   & py2= sy2 & endif else $
	if rot eq 15 then begin px2= sx2 & py1= 0   & py2= sy2 & endif else $
	if rot eq 16 then begin px2= sx2 & py1= 0   & py2= 0   & endif
;**	Starting point.
;**	-------- -----
	if px1 gt 0 then dx = 0			else $
	if px1 eq 0 then dx = sx1*(sx-1)/2	else $
	if px1 lt 0 then dx = sx1*(sx-1)
;
	if px2 eq 0 then dx = dx + sx2*(sz-1)/2 else $
	if px2 lt 0 then dx = dx + sx2*(sz-1)
;
	if py1 gt 0 then dy = 0			else $
	if py1 eq 0 then dy = sy1*(sx-1)/2	else $
	if py1 lt 0 then dy = sy1*(sx-1)
;
	if py2 eq 0 then dy = dy + sy2*(sz-1)/2	else $
	if py2 lt 0 then dy = dy + sy2*(sz-1)
;
	if flg eq 1 then return  , res
;**
;**	Spacial view.
;**	------- ----
	bb=sl_psizm(arec,arec_z,3,si,sj,2,4,-1)
;	arec(*,*,0)=  0
	arec(*,*,1)= -1
	if (rvl eq rvm) then rvm=sl_maxim(erey,vssz,nn,rvl)
	nn	  = nz-1 &   jj=vssz(11) &  ii=vssz(10)
;**
	if (rvl eq vlt) and (rvm eq vmt) then vl=rvl+(rvm-rvl)/2 else vl=rvl
	if (typ ge 4) and (typ le 16) then begin
	 if tv_flg(17) gt 1 then begin
	  if typ ne 8 then begin vl =long(vl)   &   rvm=long(rvm)   &  endif
	  bb=sl_deepex(long(nn) ,long(jj) ,long(ii) ,long(ez),vsis(2),vsis(1),$
		long(sz),long(si) ,long(sj) ,long(dx) ,long(dy) ,long(fdx)  ,$
		long(fdy),long(fdz),long(px1),long(py1),long(px2),long(py2) ,$
		long(tv_nc),long(typ), vl ,rvm ,long(stt),arec,erey)
	  endif
	endif	else begin
;**
	vlm	  =rvm-vl
	for nk= long(0) ,nn  do begin
	    nt= nk/fdz
	    xt= nt*px2 + dx
	    yv= nt*py2 + dy
	    ns= nk+stt(2)
	    pz= sj-nt-1
	    pw= si-nt-1
	    for jk= long(0) ,jj do begin
		yt= yv+ jk/fdy
		js= jk+ stt(1)
		for ik= long(0),ii do begin
		   xi = xt + ik/fdx*px1
		   yi = yt + ik/fdx*py1
		   is = ik + stt(0)
		   if   ez gt  1  then vm= erey(is,js,ns) else  vm= erey(is,js)
		   if   vm gt rvm then vm= vl
		   vm = long(sl_max2(vm-vl ,0))*tv_nc/vlm
		   if   arec(xi,yi,0) le vm then begin
			arec(xi,yi,0)  = vm
			arec(xi,yi,1)  = nk
			endif
		   if sz gt 1 then if px1 ne 0   then	  begin
		   	if   vm gt arec(xi,pz,0) then     begin
				   arec(xi,pz,0) = vm
				   arec(xi,pz,1) =-jk-2 & endif
		   	if   vm gt arec(pw,yi,0) then     begin
				   arec(pw,yi,0) = vm
				   arec(pw,yi,1) =-ik-2 & endif & endif
	endfor & endfor & endfor
;...
;...	deep colors adjustment.
;...
;	if nz lt tv_nc/4 then begin
;	nt=tv_nc/nz
;	for  jk = 0 , sj-1  do $
;	  for  ik = 0  , si-1  do begin
;		nk=arec(ik,jk,1)
;		if (nk ge 0) then begin
;			ns = arec(ik,jk,0)
;			if (ns gt 0) then ns = ns/nz + nt*(nn-nk)
;			arec(ik,jk,0)= ns
;			endif
;	  endfor
;	endif
	endelse
	return , res
      endif  else  begin
;**	To get coordinates?
;**	-- --- -----------
	if  typ eq 64 then cpx=1 else cpx=0
	clf(2) =arec(clf(0),clf(1),1)
	if (clf(2) ge 0) and (clf(2) le vssz(12)) then begin
;**	    From the space.
;**	    ---- --- -----
	    nt= clf(2) / fdz
	    ns= clf(2) + vssz(9)
	    yv= nt*py2 + dy
	    i = 0
;**	    More difficult invers ...
;**	    ---- --------- ------
	    if px1 eq 0 then if py1 eq 0 then begin
	    			j   = (clf(1) - yv) *fdy
				  if ( j ge 0) and (j le vssz(11)) then  begin
				   js=vssz(8)+j
				   is=vssz(7)
				   if ez gt 1 then vl =  erey(is,js,ns) $
					      else vl =  erey(is,js   )
				     bb=sl_cv(vl,vl,1,0,cpx)
				     for nk = long(0) ,  vssz(10)  do    begin
					is  = vssz(7)+nk
					if ez gt 1 then vm =  erey(is,js,ns) $
						   else vm =  erey(is,js   )
					bb=sl_cv(vm,vm,1,0,cpx)
					if vm gt vl   then    begin
					   vl=vm & i=nk/fdx & endif &   endfor
				  endif else j=-1
			     endif else j=0  $
	    else    begin
;**		Normal invers ...
;**		------ ------
		i = ( clf(0) - nt*px2 - dx) / px1  * fdx
		j = ((clf(1) - i /fdx *py1) - yv)  * fdy   &   endelse
	        if (j ge 0) and (j le vssz(11)) and (i ge 0) and (i le vssz(10))$
	    	then  begin
;**	        And may be reduced ...
;**	        --- --- -- -------
		res(0)=i & res(1)=j & res(2)=clf(2)
		if fdx*fdy gt 1 then  begin
		   stt(0)= fdx & stt(1)=fdy & stt(2)=1
		   res(0)= sl_xred(erey,res,vssz,stt,cpx) & endif
		endif
	  endif else if clf(2)  lt  -1 then begin
	    i=-1 & j=-1 & n=-1
;**	    From the Y projection.
;**	    ---- --- - ----------
	    if clf(0) ge si-vssz(12)-1 then begin
		i = -clf(2)-2
		nt=  si-clf(0)-1
		yv=  nt*py2 + dy
		j =((clf(1) - i/fdx *py1) - yv) * fdy
		n =  nt*fdz
		res(0)=i & res(1)=j & res(2)=n
;**	        And may be reduced ...
;**	        --- --- -- -------
		if fdy*fdz gt 1 then begin
		   stt(0)= 1 & stt(1)=fdy & stt(2)=fdz
		   res(0)= sl_xred(erey,res,vssz,stt,cpx) & endif
	    endif else begin
;**	    From the X projection.
;**	    ---- --- - ----------
		j = -clf(2)-2
		nt=  sj-clf(1)-1
		if px1 eq 0 then begin
			    i =nt
			    nt=si-clf(0)-1
		endif else  i =  (clf(0)-nt*px2 -dx) /px1  * fdx
		n =  nt*fdz
		res(0)=i & res(1)=j & res(2)=n
;**	        And may be reduced ...
;**	        --- --- -- -------
		if fdx*fdz gt 1 then  begin
		 i=vssz(7)+i
		 n=vssz(9)+n
		 if ez gt 1     then  vl = erey(i ,j ,n )  else vl = erey(i ,j )
		 bb=sl_cv(vl,vl,1,0,cpx)
	    	 for nk = n , n+fdz-1 do if nk lt ez	   then $
		 for ik = i , i+fdx-1 do if ik le vssz(10) then begin
		    if ez gt 1  then  vm = erey(ik,j ,nk)  else vm = erey(ik,j )
		    bb=sl_cv(vm,vm,1,0,cpx)
		    if  vm gt vl then  begin  vl=vm
		       res(0)=ik-vssz(7) & res(1)=j & res(2)=nk-vssz(9)
		endif & endif  & endif
	    endelse
	  endif
      endelse
    endif
if (res(0) lt 0) or (res(1) lt 0) then begin res(0)=0 & res(1)=0 & res(2)=0
				       endif
return,  res
end
;
;
;
function sl_savarea, cd, erey, fcg ,xsiz, windn ,icon ,icon_z
;******* **********  **  ****  ***  ****  *****  ****  ******
;**
	common  my_sr,	bb,dirc,dwn,num,spdl,spt,u,winc,v2,v3,sr_typ
;**
	common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
			io_cur,io_ext,io_seq,io_str
;**
	common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;carez + erey
;**
bb=0
    if cd eq 0 then begin
;**	Window definition
;**	------ ----------                   ----
	winc(0 )=tv_win(*,windn)
	winc(0 )= -1
	winc(20)=  0
	num=winc(11)
	spt=winc(13)/10
	if num gt 0 then dwn=sl_str(num,'(i6)') $
	else  begin num=xsiz(xsiz(0)+1)/2
		    if  num eq 4  then num=3 else if num eq 8  then num=4 else $
		    if  num eq 16 then num=5 else if num ge 32 then num=6
		    dwn=sr_typ(num)
		    endelse
;	WIND
	dirc='?'
	v3(0)=1 & v3(1)=tv_wsz(0) & v3(2)=16
	k= 1
	while k gt 0 do begin
		u= sl_filw(fcg,dwn+'_'+sl_stbr(sl_str(io_seq,'(i3)'),2), $
		   io_ext(4),dirc,v3(2),v3,1,k)
		io_seq=io_seq+1
	endwhile
	if u gt 0 then begin
		bb=sl_cellput(winc,u,v3)
		bb=sl_iofree (u)
		  io_txt(0) ='.View definition: '+ dirc +'.'+io_ext(4)
		  io_txt(1) ='.Data   file:     '+ dirc +'.'+io_ext(5)
		  dwn=dwn+'_'+sl_stbr(sl_str(io_seq-1,'(i3)'),2)
;**	Write icon in .WINDimg
;**	----- ---- -- --------              ----
		u= sl_filw(fcg,dwn,io_ext(4)+'img',dirc,icon_z(icon_z(0)+1),icon_z,1,k)
			if u gt 0 then begin
			   bb=sl_cellput(icon,u,icon_z)
			   bb=sl_iofree (u)
			   endif
;**	Data file SCAN.
;**	---- ----                           ----
		dirc='?' & k=-1
		u= sl_filw(fcg,dwn,io_ext(5),dirc,xsiz(xsiz(0)+1),xsiz,1,k)
		if u gt 0 then begin
		  io_txt(2) ='---->  Click here to continue  <---- '
			bb=sl_cellput(erey,u,xsiz)
			bb=sl_iofree (u)
			if spt  eq -2  then begin
;			DEEP
			dirc='?' & k=-1
			u= sl_filw(fcg,dwn,io_ext(6),dirc,$
					arec_z(arec_z(0)+1),arec_z,1,k)
     			if u gt 0 then begin
				bb=sl_cellput(arec,u,arec_z)
				bb=sl_iofree (u)
			endif & endif
		  bb=sl_tvmenuc(0,0,io_txt,'Save',-2.,-2.)
	endif &   endif
;**
    endif else if (cd eq 1) or (cd eq 4) then begin
;**	Data file only.
;**	---- ---- ---- -----
;**
	winc(0)=tv_win(*,windn)
	spt    =winc (13)/10
;	TMP
	dirc=' ' & k=-1
	v3(0)=1 & v3(1)=tv_wsz(0) & v3(2)=16
	u= sl_filw(fcg,'t',io_ext(7),dirc,v3(2),v3,1,k)
	if u gt 0 then  begin
		bb=sl_cellput (winc,u,v3)
		bb=sl_iofree  (u)
		if (spt eq -2) and (cd eq 4)   then begin k=-1
			u= sl_filw(fcg,'s',io_ext(7),dirc,$
					arec_z(arec_z(0)+1),arec_z,1,k)
			if u gt 0  then begin
				bb=sl_cellput(arec ,u ,arec_z)
				bb=sl_iofree (u)
			endif & endif
		k=-1
		u= sl_filw(fcg,'d',io_ext(7),dirc,xsiz(xsiz(0)+1),xsiz,1,k)
		if u gt 0 then  begin
			bb=sl_cellput(erey ,u, xsiz)
			bb=sl_iofree (u)
		endif & endif
;**	Saved in memory.
;**	----- -- ------
;**
    endif else if cd eq 2 then begin
;**futur
    endif else if cd eq 3 then begin
;**	Deep data only.
;**	---- ---- -----
;**
	if xsiz(0) gt 1 then begin
;	TMP
	dirc=' ' & k=-1
	u= sl_filw(fcg,'s',io_ext(7),dirc,xsiz(xsiz(0)+1),xsiz,1,k)
	if u gt 0  then begin
		bb=sl_cellput (erey ,u, xsiz)
		bb=sl_iofree  (u)
	endif & endif
    endif else if cd eq 5 then begin
;**	Formated values
;**	-------- ------                     ----
	if fcg(0) ge 0 then begin
	   num=xsiz(xsiz(0)+1)
	   v3(0)=1 & v3(1)=xsiz(1) & v3(2)=num
	   endif
	dirc='?' & k= 1
	while k gt 0 do begin
		u= sl_filw(fcg,sl_stbr(sl_str(io_seq,'(i3)'),2),$
		   io_ext(windn),dirc,0,0,0,k)
		io_seq=io_seq+1
	endwhile
	if u gt 0 then begin
	   if fcg(0) ge 0 then begin
		if fcg(1)  lt 0 then  fcg(0)=fcg(0)+1
		for j=long(0),fcg(0)-1 do bb=sl_cellput(erey(*,j),-u,v3)
		io_txt(0) ='.Created file: '+ dirc +'.'+io_ext(windn)
		io_txt(1) ='.'
		io_txt(2) ='---->  Click here to continue  <---- '
		bb=sl_tvmenuc(0,0,io_txt,'Saved values',-2.,-2.)
	   endif
	   bb=sl_iofree (u)
	endif
    endif else if cd eq 6 then begin
;**	Binary image IMG
;**	------ -----                        ----
	dirc='?' & k=-1
	dwn = sl_stbr(sl_str(io_seq,'(i3)'),2)
	u= sl_filw(fcg,dwn,io_ext(8),dirc,xsiz(xsiz(0)+1),xsiz,1,k)
	if u gt 0 then  begin
		io_seq=io_seq+1
		bb=sl_cellput(erey ,u, xsiz)
		bb=sl_iofree (u)
		io_txt(0) ='.Created file: '+ dirc +'.'+ io_ext(8)
		io_txt(1) ='.'
		io_txt(2) ='---->  Click here to continue  <---- '
		bb=sl_tvmenuc(0,0,io_txt,'Saved values',-2.,-2.)
		endif
    endif
return,bb
end
;
;
function sl_resarea, cd, erey,xsiz ,fdl ,fl ,filename
;******* **********  **  **** ****  ***  **  ********
;**
	common  my_sr,	bb,dirc,dwn,num,spdl,spt,u,winc,v2,v3,sr_typ
;**
	common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
			io_cur,io_ext,io_seq,io_str
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
	common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
;carez + erey + fdl (scal. or v(2))
bb=0
;**
    dirc=' '
    if cd le 0 then begin
;**	window definition WIND.
;**	------ ----------
	if filename ne '' then u=sl_filr(filename  ,''       ,0,dirc,0) $
			  else u=sl_filr(io_cur+'*',io_ext(4),0,dirc,0)
	if u   gt 0  then    begin
	   bb=sl_tvmenunw(5,0,['reading ...',' '],' ',tv_xp,tv_yp/2)
	   v2(0)=-1 & v2(1)=16 & v3(0)=1 & v3(1)=tv_wsz(0)
	   bb=sl_cellget(u,v3,v2,winc,0)
	   num=winc(42)
	   bb=sl_swapvms(winc,tv_wsz(0),0,0,16,num)
	   if bb then tv_win(0,fdl)=winc(*)
	   bb=sl_iofree (u)
	   bb=sl_sti(dirc,sl_stbr(io_ext(5),0),sl_stp(dirc,sl_stbr(io_ext(4),0),0))
;**	   Data file SCAN.
;**	   ---- ----
	   bb  =0
	   u   =sl_filr(dirc,'',0,dirc,0)
	   if (u gt 0) and (winc(1) gt 0) then begin
	     bb= 1
	     if cd eq 0 then begin

	      spt = winc(13)/10
	      if winc(5) gt 1 then $
		 bb=sl_psizm(erey,xsiz,3,winc(1),winc(2) , $
					 winc(5),winc(16),-1) else $
		 bb=sl_psizm(erey,xsiz,2,winc(1),winc(2),winc(16),-1,-1)
	      if winc(42) ne num then v2(0)=0 else v2(0)=-1
	      v2(1)=winc(16)
	      bb=sl_cellget(u,xsiz,v2,erey,0)
	      bb=sl_swapvms(erey,winc(1),winc(2),winc(5),winc(16),num)
	      bb=sl_put_strfile(2,fdl,dirc,winc(1),winc(2),winc(5),$
				v2(0),v2(1),0,0,0,1)
	      filename=dirc
	      bb=sl_iofree (u)
	      if spt  eq  1 then begin  f_az=winc(30)
					f_ax=winc(31) & endif
	      if spt  eq -2 then begin
		 bb=sl_sti(dirc,'       ',sl_stp(dirc,sl_stbr(io_ext(5),0),0))
;		 DEEP
		 u   =sl_filr(dirc,io_ext(6),1,dirc,0)
	   	 if u gt 0 then begin
		    bb=sl_psizm(arec,arec_z,3,winc(24),winc(25),2,4,-1)
		    v2(0)=-1 & v2(1)=arec_z(4)
		    bb=sl_cellget(u,arec_z,v2,arec,0)
		    bb=sl_swapvms(arec,winc(24),winc(25),2,4,num)
		    bb=sl_iofree (u)
	         endif
	      endif
	     endif
	     if u gt 0 then bb=sl_iofree (u)
	   endif
	endif
;**
    endif else if (cd eq 1) or (cd eq 4) then begin
;**	Data file only.
;**	---- ---- ----
;**
	dwn = sl_stbr(sl_str(fl(0),'(i6)'),1)+'_'+sl_stbr(sl_str(fl(1),'(i6)'),1)
;	TMP
	u=sl_filr(dwn+'_t',io_ext(7),1,dirc,0)
	if u gt 0 then  begin
	   spdl = dirc
	   v2(0)=-1 & v2(1)=16 & v3(0)=1 & v3(1)=tv_wsz(0)
	   bb=sl_cellget(u,v3,v2,winc,0)
	   bb=sl_iofree (u)
	   if fdl  then   bb=sl_run('d',spdl,'',0,1)
	   spt=winc(13)/10
	   if (spt  eq -2) and (cd eq 4) then begin
	   	u   =sl_filr(dwn+'_s',io_ext(7),1,dirc,0)
	   	if u gt 0 then begin
		     spdl = dirc
		     bb=sl_psizm(arec,arec_z,3,winc(24),winc(25),2,4,-1)
		     v2(0)=-1 & v2(1)=arec_z(4)
		     bb=sl_cellget(u,arec_z,v2,arec,0)
		     bb=sl_iofree (u)
		     if fdl  then   bb=sl_run('d',spdl,'',0,1)
		endif & endif
	   bb  =0
	   u   =sl_filr(dwn+'_d',io_ext(7),1,dirc,0)
	   if u gt 0 then begin
		spdl = dirc
		if winc(23) gt 1 then $
		   bb=sl_psizm(erey,xsiz,3,winc(21),winc(22),winc(23),winc(16),-1) $
				 else $
		   bb=sl_psizm(erey,xsiz,2,winc(21),winc(22),winc(16),-1,-1)
		   v2(0)=-1 & v2(1)=winc(16)
		   bb=sl_cellget(u ,xsiz,v2,erey,0)
		   bb=sl_iofree (u)
		   if fdl  then   bb=sl_run('d',spdl,'',0,1)
		   tv_win(18,fl(0))=winc(18)
		   tv_win(27,fl(0))=winc(27)
		   endif
	   endif
    endif  else if cd eq 2 then begin
;**	From memory.
;**	---- ------
;**
;**futur
    endif  else if cd eq 3 then begin
;**	Deep data only.
;**	---- ---- ----
;**
	dwn =sl_stbr(sl_str(fl(0),'(i6)'),1)+'_'+sl_stbr(sl_str(fl(1),'(i6)'),1)
;	TMP
	u   =sl_filr(dwn+'_s',io_ext(7),1,dirc,0)
	if u gt 0 then begin
		  bb=sl_psizm(erey,xsiz,3,fdl(0),fdl(1),2,4,-1)
		  v2(0)=-1 & v2(1)=xsiz(4)
		  bb=sl_cellget(u ,xsiz,v2,erey,0)
		  bb=sl_iofree (u)
		  endif
    endif
return,bb
end
;
;
;
;
function sl_tty   ,  flg
;******* ******
;**
common  my_tty,	esc,osc,stt,csi,cout
;**
;	if flg ge 0 then return,1
;Large  window
;-----  ------
	if flg eq 0 then cout=csi+'80$|'+csi+'24t'+csi+'24t'
;Small  window
;-----  ------
	if flg eq 1 then cout=csi+'69$|'+csi+'11t'+osc+'24;[0.0,22.5]'+stt
;**
	bb=sl_iopage(cout,flg)
return,1
end
;
;
;
;
function sl_break,	DATA ,VSIZ, CURIJK, ARES ,ARES_Z
;******* ********	****  ****  ******  ****  ******
;**
;**
on_error,0
on_ioerror,mis
	return_break=0
	xsiz=vsiz
	bb=sl_tty (0)

	bb=sl_iotype(' ',0,0)
	bb=sl_help(DATA)
	bb=sl_iotype(' ',0,0)
	bb=sl_prompt('I.D.L> ')
	stop,'Type .CONTINUE to return (or RETALL & SCAN to restart)'

mis: bb=sl_prompt('Idl> ')
     bb=sl_tty(1)

     vsiz(0)=sl_size (data)
     vsiz(6)=vsiz(vsiz(0)+2)
     if vsiz(0) le 1  then begin
		if vsiz(0) eq 0 then data=sl_iarr(2,1,1)
		if vsiz(0) eq 1 then begin
		   bb=sl_pp(0,data,vsiz,ares,ares_z)
		   bb=sl_psizm(data,vsiz,2,vsiz(1),1,vsiz(2),-1,-1)
		   data(0,0)=ares(*)
		   bb=sl_dd(2,ares,ares_z) & endif
		vsiz(0)=sl_size(data)
	        vsiz(6)=vsiz(vsiz(0)+2)
     endif
     if (xsiz(0) ne vsiz(0)) or (xsiz(1) ne vsiz(1)) or $
	(xsiz(2) ne vsiz(2)) or (xsiz(3) ne vsiz(3)) or $
	(xsiz(xsiz(0)+1)     ne  vsiz(vsiz(0)+1))    then return_break=4

     return ,return_break
end
;
;
;
function sl_insert, w,in_are,vsiz, pcur
;******* *********  * ****** ****  ****
;**
;**Call an external function.
;**
	common  my_insert,  i_txt,i_idx,i_fil,i_rout,i_ps,i_rs,$
			    i_trout,i_tfil,i_tlang,i_tdx,i_enter,i_rcall
;**
	common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
			io_cur,io_ext,io_seq,io_str
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;carez
n   =0
on_ioerror,mis
;
;
	i_txt(i_idx(1):*)=' '
;**	Find PRO  procedures.
	i    = sl_iofind(io_dir+'*.'+io_ext(11),'',0,io_nam)
	if i gt 0 then  begin
			if   i+i_idx(1) ge i_idx(0) then i=i_idx(0)-i_idx(1)-1
			for k=0,i-1 do i_txt(i_idx(1)+k  )='  '+io_nam(k) & endif
;**	Find .pro procedures.
	h    = sl_iofind(io_cur+'*.'+io_ext(15),'',0,io_nam)
	if h gt 0 then  begin
			if h+i+i_idx(1) gt i_idx(0) then h=i_idx(0)-i_idx(1)-i
			for k=0,h-1 do i_txt(i_idx(1)+k+i)='  '+io_nam(k)
			i=i+h & endif
;**	Find WDG  procedures.
	h    = sl_iofind(io_dir+'*.'+io_ext(14),'',0,io_nam)
	if h gt 0 then  begin
			if h+i+i_idx(1) gt i_idx(0) then h=i_idx(0)-i_idx(1)-i
			for k=0,h-1 do i_txt(i_idx(1)+k+i)='  '+io_nam(k)
			i=i+h & endif
;**	Find FUNC procedures.
	j    = sl_iofind(io_cur+'*.'+io_ext(12),'',0,io_nam)
	if j gt 0 then  begin
			if j+i+i_idx(1) gt i_idx(0) then j=i_idx(0)-i_idx(1)-i
			for k=0,j-1 do i_txt(i_idx(1)+k+i)='  '+io_nam(k) & endif
	i    =i_idx(0)-1
	while i_txt(i) eq i_txt(i-1) do i=i-1
	i_txt(i)='.   '
	i_txt(i+1)='  '+'Return'
	i_rs =i+1
	k    =0
;
;	Choice.
	m   = 3
	while k ge 0 do begin
	k   = sl_tvmenul(0,m,i_txt(0:i_rs),'External functions',15.,15.)
	m   = 4
;
	if k  eq i_rs then k=-1
;
	if k  ge i_idx(1)  then  begin
	      i_fil=i_txt(k)
;	      VMS
	      j    =sl_stp(i_fil,':',0)+1
	      i    =sl_stp(i_fil,']',0)+1
	      if j gt 1 then if i lt j then i=j
	      if i le 1 then begin
;	         UNIX
		 j=0
		 i_rout=i_fil
		 while j ge 0 do begin
			i=j+1
			bb=sl_sti(i_rout,' ',j)
			j =sl_stp(i_rout,sys_dep('DIVIDER'),0)
		 endwhile
		 if i le 1 then  begin
;		    FILE ONLY
			i_rout=i_fil
			i=2
		 endif
	      endif
	      if    i gt 1 then  begin
		 h=0
		 i_ps(0)=vsiz(1)
		 i_ps(1)=vsiz(2)
		 if vsiz(0) gt 2 then i_ps(2)=vsiz(3) else i_ps(2)=1
;
		 j =sl_stp(i_fil,sl_stbr(io_ext(11),0),0) -1
		 if (j gt i) then  begin
;**		   PRO IDL
		   h=1
		   i_rout = sl_stx(i_fil,i,j-i)
		 endif else begin
		 j =sl_stp(i_fil,sl_stbr(io_ext(15),0),0) -1
		 if (j gt i) then  begin
;**		   pro IDL
		   h=1
		   i_rout = sl_stx(i_fil,i,j-i)
		   endif else begin
		    j =sl_stp(i_fil,sl_stbr(io_ext(14),0),0) -1
		    if (j gt i) then  begin
;**		      WDG IDL
		      h=2
		      i_rout = sl_stx(i_fil,i,j-i)
		      endif else begin
		       j = sl_stp(i_fil,sl_stbr(io_ext(12),0),0) -1
		       if (j gt i) then  begin
;**		         FUNC EXTERN
			 h=3
		         i_rout = sl_stx(i_fil,i,j-i)
		         endif
		      endelse
		   endelse
		 endelse
		 if (h gt 0) and (i_tdx lt i_idx(0)-1) then begin
		    bb=sl_sti(i_fil,'.*',0)
		    i_txt(k)= i_fil
		    bb=sl_sti(i_fil,'  ',0)
		    bb=sl_stbr(i_fil,2)
;
		    i_tfil (i_tdx)=i_fil
		    i_fil='                            '
		    bb=sl_sti(i_fil,i_rout,1)
		    bb=sl_sti(i_fil,i_enter+sl_str(i_tdx-2+10,'(i2)'),25)
		    i_trout(i_tdx)=i_fil
		    i_tlang(i_tdx)=h
		    i_tdx=i_tdx+1
		 endif
	      endif
	endif
	endwhile
	bb=sl_tvdmenu(0)
;
mis:bb=sl_ioclear(0)
    if (n ne 1) then n=0
    return, n
end
;
;
;
function sl_gf, vl,f,g,fmt
;******* *****  ** * * ***
;**
;** Find a format.
;**
common	my_gf,	gf_v,gf_v1,gf_fm
;**
		if vl lt 0 then  gf_v=-vl else gf_v=vl
		if f then begin
			    if gf_v ge gf_v1(1) then  fmt=gf_fm(1)      else  $
			    if gf_v ge gf_v1(2) then  fmt=gf_fm(2)      else  $
			    if gf_v ge gf_v1(3) then  fmt=gf_fm(3) $
			    			else  fmt=gf_fm(4) &    endif $
		     else   if gf_v lt gf_v1(0) then  fmt=gf_fm(0) else begin
						g=1 & fmt=gf_fm(5) &    endelse
return,1
end
;
;
;
function sl_dislog, are,are_z,vxl,vxm
;******* *********
;**
	if vxl le 0 then begin
		if are_z(0) eq 1 then are(0)    =are(*)    -vxl+1 else $
		if are_z(0) eq 2 then are(0,0)  =are(*,*)  -vxl+1 else $
		if are_z(0) eq 3 then are(0,0,0)=are(*,*,*)-vxl+1
		vxm=vxm - vxl + 1
		vxl=vxl - vxl + 1
	endif
	bb=sl_d_p(30,are,are_z,0,0,vxl,vxm)
	vxl=sl_log1(vxl,are_z(are_z(0)+1)) & vxm=sl_log1(vxm,are_z(are_z(0)+1))
return,1
end
;
;
;
function sl_surf ,scl,erey,vsx,vsy,vsz,typ,plx,ply,miv,mxv,az,ax,lev,flg,bg,smo
;******* *******  *** **** *** *** *** *** *** *** *** *** ** ** *** *** ** ***
;**
;** Tranform an image to a surface video
;** ******** ** ***** ** * ******* *****
;**
	common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common	my_surf,dms,dm1,dm2,dii,djj,hoo,fxx,fyy,coo,sii,ndd,sv2,sbox,$
			flx,flz,aa,ah,b,bb,di,dj,dlx,dm,fgg,ho,j,nd,ni,$
			sco,ssi,sdi,sdj,bz,bh,fj,fj2,mav,mnv
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey	,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
b =0
;** Get coordinates from a surface.
;** --- ----------- ---- - -------
if (scl eq -3)  then  begin
;		Index to dc..
		if ndd gt 0 then begin miv =miv*ndd/(vsx+1)
				       mxv =mxv*ndd/(vsy+1) & endif
		miv = miv -dm1 + dii
		mxv = mxv -dm2 + djj
		di  = fix( miv*coo - mxv*sii) +dm1
		dj  = fix( miv*sii + mxv*coo) +dm2
		miv = fix( di /fxx)
		mxv = fix( dj /fyy)+hoo
;??		if tv_od eq 0  then mxv= ply -mxv -1
		if miv lt 0 then miv=0 else if miv ge plx then miv=plx-1
		if mxv lt 0 then mxv=0 else if mxv ge ply then mxv=ply-1
		endif else  $
if (scl eq -2)  then  begin
;		DC to index..
		di  = fix( plx*fxx)
		dj  = fix((ply-hoo)*fyy)
		di  = di-dm1
		dj  = dj-dm2
		miv = fix(di *coo -dj *sii +dm1 -dii)
		mxv = fix(di *sii +dj *coo +dm2 -djj)
		if ndd gt 0 then begin miv =miv*(vsx+1)/ndd
				       mxv =mxv*(vsy+1)/ndd  & endif
		b   = 1
		if (miv lt  0 ) or (mxv lt  0 ) then begin  miv=0
						     b=0 &  mxv=0   & endif
		if (miv ge vsx) or (mxv ge vsy) then begin  miv=vsx-1
						     b=0 &  mxv=vsy-1 & endif

		if tv_od eq 0  then miv= vsx -miv -1
		endif else  $
if (scl eq -1)  then  begin
;		Init invers..
		ndd = fix((plx -3) /fj2)
		if (vsx le ndd) and (vsy le ndd) and (vsx le vsy*4) then ndd=0
		if  ndd gt 0    then begin  ndd=ndd/2  &   dm=plx /2
				 dii = (dm -ndd)/2 & djj =(dm-ndd)/2
		endif else begin
				 fj  = float(vsx*vsx + vsy*vsy)
				 bb  = sl_sqrt(fj,1)
				 dm  = fix(fj) +3
				 ni  = plx/ dm
				 if ni eq 0 then dm = plx $
					    else dm =(plx -dm*ni)/ni+dm
				 dii = (dm -vsx)/2 & djj =(dm-vsy)/2 & endelse
		dm1 = (dm -1)  /2
		dm2 =  dm1
;
		aa  =  ax
		if (flg eq 10) or (flg eq 11) or (flg eq 12) then aa =90
		if (aa gt 90) and (aa lt 270) then j=1 else j=0
		if  aa gt 180 then ah=360-aa  else ah=aa
		if  ah gt  90 then ah=180-ah
		bh  = float( ply)  / dm    *(ah/90.)
        	ho  = fix  ( dm * bh)  &   if  ho  eq 0 then  ho =1
		hoo = ply -ho
		if  aa gt 180 then hoo=hoo-(ply-1 -(dm-1)*bh)/2
		if flg eq 4   then hoo=hoo/2
		if flg eq 2   then begin
				   hoo=fix (hoo/(90./(90.-ah+1)))
				   ho =ply -hoo  & endif
;
		ah  =  az
		if (flg eq 10) or (flg eq 11) or (flg eq 12) then ah =-1
		if ah eq  -1  then begin dii=0   & djj=0  & ah=0
				         ndd=0   & dm1=vsx/2  &  dm2=vsy/2
					 fxx=float(vsx)/plx
					 fyy=float(vsy)/ho
		endif	      else begin
					 fxx=float(dm)/plx
					 fyy=float(dm)/ho     &  endelse
;
		ah  = (ah+(1-tv_od+j)*180.) * 3.1416 / 180.
		coo = sl_cos(ah) & sii = sl_sin(ah)
		b   = 1
;**
endif else if (vsx gt 1) and (vsy gt 1) and (plx gt 4) and (ply gt 4) then begin
 if flg eq 2  then begin
;** Surface plot
;** ------- ----
	if tv_od eq 1 then begin vare=erey & bb=sl_psiz(vare_z,2,vsx,vsy,typ, -1,-1)
					 bb=sl_d_p(10, vare,vare_z,0,0) ;!!??
	                         b =sl_surface(vare,az,ax,1,0) & endif else $
	                         b =sl_surface(erey,az,ax,1,0) ; (az-(180*tv_od))
    endif else if flg eq 12 then begin
;** Contour plot
;** ------- ----
	if tv_od eq 1 then begin vare=erey & bb=sl_psiz(vare_z,2,vsx,vsy,typ, -1,-1)
					 bb=sl_d_p(10, vare,vare_z,0,0) ;!!??
	                         b =sl_contour(vare,miv,mxv,lev) & endif else $
	                         b =sl_contour(erey,miv,mxv,lev)
    endif else begin
;** Surface video
;** ------- -----
;**	Scale & re-form
;**	-----   -------
	if tv_od eq 0 then erey=reverse(erey,1)
	bz =az
	ah =ax
;
	if vsz eq 1 then bb=sl_psiz(dms,2,vsx,vsy,typ, -1,-1) $
		    else bb=sl_psiz(dms,3,vsx,vsy,vsz,typ,-1)
	mnv=miv & mav =mxv
	km =0
	if  scl ne 2   then begin
	 if mnv eq mav then begin mav =sl_maxim (erey,dms,ni,mnv) & km=1 & endif
	 if smo gt 1   then 	  bb  =sl_dislog(erey,dms,mnv,mav)

	 if (dms(dms(0)+1) eq 2) and ((mav -mnv) gt 210) then mnv=mav
;**
	 if ((flg eq 10) or (flg eq 11) or (flg eq 13)) then begin
	   if dms(dms(0)+1) ne 2 then begin
			    bb=sl_scalf(erey,dms,mnv,mav,km,2,dummy,256)
;			    erey=sl_scale(erey,dms(1),dms(2),dms(3),mnv,mav)
;	   		    dms(3)=2
			    mnv =mav & km  = 0  &  endif
	   if  (flg ne 13)  then begin
	   	bz  =-1
	   	ah  =90
		if ((dms(1) lt plx/2) or (dms(2) lt ply/2)) then begin
				 sv2(0) = plx/2      &   sv2(1)= ply/2
				 bb=sl_d_p(48,erey,dms,0,sv2)
				 endif
	   endif
	   bb=sl_lis (erey,dms(1),dms(2),dms(3),3,1)
	 endif
	endif
;**
	fgg=flg
	if (fgg eq 13) then  fgg=10
	if (vsz gt  2) then  fgg= 6
	if (vsz eq  2) and  (fgg ne 6) then fgg= 8
	if (vsz eq  1) and ((fgg eq 6)  or (fgg eq 8)) then fgg=1
;**
	if (fgg eq 8) and (scl ne 2)   then begin   mnv=mav
	    erey(0,0,0)=sl_scale(erey(*,*,0),dms(1),dms(2),dms(4),mnv,mav)
	    erey(0,0,1)=sl_scale(erey(*,*,1),dms(1),dms(2),dms(4),mnv,mav)
	    endif
;**
	ah =ah - fix(ah/360) *360
	if  ah lt 0   then ah=360  +  ah
	aa =tv_nc-1
	nd =fix((plx-3)/fj2)
	ni = plx/dms(1)
	if ((dms(1) le nd) and (dms(2) le nd) and (dms(1) le dms(2)*4)) and $
	   ((plx - ni*dms(1)) lt plx/6) then nd=0
	dm =plx
	dlx=plx
;**	Special angle...
	if bz  eq -1  then begin
	   if ni eq 0 then begin sv2(0)=plx & sv2(1)=dms(2)
				 bb=sl_d_p(48,erey,dms,0,sv2)
	   endif else begin
		if (plx - ni*dms(1)) gt plx/6 then  begin
				 sv2(0)=(plx-dms(1)*ni)/ni+dms(1)
				 sv2(1)= dms(2)
				 bb=sl_d_p(48,erey,dms,0,sv2)
				 endif
	   	ni =plx/dms(1)-1
		dlx=dms(1)
	   endelse
	   dm=dms(2)
	   if (smo eq 1) or $
	      (smo eq 2) then bb=sl_d_p(7,erey,dms,3 ,[0,0],0,0)
	   if (vsz eq 1) then begin
			bb=sl_psizm(tare,tare_z,2,plx/(ni+1),dms(2),4 ,-1,-1)
			if (ah gt 180) and (fgg ne 4) then tare(*,*)   =aa
			if mnv eq mav then  tare(0,0)=erey else $
			  bb=sl_scalf(erey,dms,mnv,mav,km,0,tare,256)
;			  tare(0,0)=sl_scale (erey,dms(1),dms(2),dms(3),mnv,mav)
	   endif  else  begin
			bb=sl_psizm(tare,tare_z,3,plx/(ni+1),dms(2),vsz,4,-1)
			if (fgg eq 6) then begin
			    for k=1,vsz-1 do $
				erey(0,0,k)= erey(*,*,k)+erey(*,*,k-1)
			    mav=sl_maxim(erey,dms,k,mnv)
			    endif
			if (ah gt 180) then  tare(*,*,*) =aa
			for k=0,vsz-1 do $
			if mnv eq mav  then  tare(0,0,k)=erey(*,*,k)   else $
			tare(0,0,k)=sl_scale(erey(*,*,k),dms(1),dms(2),dms(4),mnv,mav)
	   endelse
	   flx=-1
	   flz=-1
;**	Normal angle...
	endif else begin
	   if   nd eq 0  then   begin
				fj  = float(dms(1)*dms(1) + dms(2)*dms(2))
				bb  = sl_sqrt(fj,1)
				dm  = fix(fj) +3
				ni  = plx/ dm
				if ni eq 0 then  dm = plx $
				      else begin dm =(plx-dm*ni)/ni+dm
						 dlx= dm & ni=plx/dm-1 & endelse
				di  = (dm - dms(1))/2
				dj  = (dm - dms(2))/2
	   endif	 else begin
				nnd=nd/2 & dm=dm/2 & dlx=dm & ni=1
				di  = (dm - nnd)/2
				dj  = (dm - nnd)/2 & endelse
;
	   if   scl ne  2 then   begin
	     if nd  gt  0 then   begin
		if vsz eq 1 then begin
		   bb=sl_psizm(vare,vare_z,2,nnd,nnd,2,-1,-1)
		   if mnv eq mav then $
		   vare(0,0)=sl_redim(erey,dms(1),dms(2),dms(3),nnd,nnd,0) else $
 		   vare(0,0)=sl_redim(sl_scalf(erey,dms,mnv,mav,km,-1,dummy,256), $
						 dms(1),dms(2),dms(3),nnd,nnd,0)
;		   vare(0,0)=sl_redim(sl_scale(erey,dms(1),dms(2),dms(3),$
;				       mnv,mav),dms(1),dms(2),2,nnd,nnd,0)
		endif else begin
		   bb=sl_psizm(vare,vare_z,3,nnd,nnd,vsz,2,-1)
		   if (fgg eq 6) then begin
			    for k=1,vsz-1 do $
				erey(0,0,k)= erey(*,*,k)+erey(*,*,k-1)
			    mav=sl_maxim(erey,dms,k,mnv)
			    endif
		   for  k=0,vsz-1  do $
		   if mnv eq mav then $
		   vare(0,0,k)=sl_redim(erey(*,*,k),dms(1),dms(2),dms(4),$
				 nnd,nnd,0) else $
		   vare(0,0,k)=sl_redim(sl_scale(erey(*,*,k),dms(1),dms(2),$
				 dms(4),mnv,mav),dms(1),dms(2),2,nnd,nnd,0)
		endelse
	     endif else begin
		if vsz eq 1 then begin
		   bb=sl_psizm(vare,vare_z,2,dms(1),dms(2),2,-1,-1)
		   if mnv eq mav  then  vare(0,0)=erey else $
		   bb=sl_scalf(erey,dms,mnv,mav,km,0,vare,256)
;		   vare(0,0)  =sl_scale(erey,dms(1),dms(2),dms(3),mnv,mav)
		endif else begin
		   bb=sl_psizm(vare,vare_z,3,dms(1),dms(2),vsz,2,-1)
		   if (fgg eq 6) then begin
			    for k=1,vsz-1 do $
				erey(0,0,k)= erey(*,*,k)+erey(*,*,k-1)
			    mav=sl_maxim(erey,dms,k,mnv)
			    endif
		   for  k=0,vsz-1 do $
		   if mnv eq mav then $
		   vare(0,0,k)=erey(*,*,k) else $
		   vare(0,0,k)=sl_scale(erey(*,*,k),dms(1),dms(2),dms(4),mnv,mav)
		endelse
	     endelse
	     if  scl eq 1  then bb=sl_d_p(0,vare,vare_z,0,0,1,aa-1)
	     if (smo eq 1) or $
		(smo eq 2) then bb=sl_d_p(7,vare,vare_z,3 ,[0,0],0,0)
	     flx=-1
	     flz=-1
	     if vsz eq 1  then begin
		bb=sl_psizm(tare,tare_z,2,dm,dm,4,-1,-1)
		if (ah gt 180)  and (fgg ne 4)  then tare(*,*)=aa
		tare(di,dj)=vare
	     endif else begin
		bb=sl_psizm(tare,tare_z,3,dm,dm,vsz,4,-1)
		if (ah gt 180) then tare(*,*,*)  =aa
		for k=0,vsz-1  do   tare(di,dj,k)=vare(*,*,k)
	     endelse
	     bb=sl_dd(2,vare,vare_z)
	   endif
	endelse
;
	if scl eq 1 then bb=sl_pp(0 ,tare,tare_z,aref,aref_z)
;
;**	Rotate
;**	------
	if (ah gt 90) and (ah lt 270) then j=1 else j=0
;
	ho = bz+180*(1-tv_od+j)
	ho = ho- fix(ho/360) *360
;
	if bz ne -1 then begin
	   if scl ne 2  then begin
	      if vsz eq 1 then  bb=sl_rotat(tare,tare_z(1),tare_z(2),4,ho,1) $
		   else for k=0,vsz-1 do $
		   tare(0,0,k)=sl_rotat(tare(*,*,k), tare_z(1),tare_z(2),4,ho,0)
	   endif else if flz ne ho then begin
	      if vsz eq 1 then begin
		   bb=sl_psizm(tare,tare_z,2,aref_z(1),aref_z(2),4, -1,-1)
			     tare(0,0)=sl_rotat(aref,aref_z(1),aref_z(2),4,ho,0)
	      endif	  else begin
		   bb=sl_psizm(tare,tare_z,3,aref_z(1),aref_z(2),aref_z(3),4,-1)
		   for k=0,vsz-1 do $
		   tare(0,0,k)=sl_rotat(aref(*,*,k), aref_z(1),aref_z(2),4,ho,0)
		   endelse
	      flx=-1 & flz=ho  &  endif
	endif else if tv_od eq j then begin
	      if vsz eq 1 then bb=sl_rotat(tare,tare_z(1),tare_z(2),4,180,1) $
		   else for k=0,vsz-1 do $
		   tare(0,0,k)=sl_rotat(tare(*,*,k),tare_z(1),tare_z(2),4,180,0)
	endif
;
;**	Normalize heights angle  (0 -->90 )
;**	--------- ------- -----
	if (fgg ne 4) then $
	if (scl eq 2) then begin
		if  ((ah gt 180) and (flx eq -1)) or   $
		    ((ah le 180) and (flx eq  0)) then begin
				 if vsz eq 1 then tare(0,0)   =aa-tare $
					     else tare(0,0,0) =aa-tare
				 bb=sl_d_p(0,tare,tare_z,5,0,0,aa-1) & endif
		if    ah gt 180  then  flx=0 else flx=-1
	endif else if ah gt 180  then  $
				 if vsz eq 1 then tare(0,0)   =aa-tare $
					     else tare(0,0,0) =aa-tare
	if  ah gt 180 then ah  = 360 -ah
	if  ah gt  90 then ah  = 180 -ah
;**	Prepare the loop
;**	------- --- ----
	bh =float( ply)  / dm   *(ah/90.)
	if (bz ne -1) and (bg eq 1) then begin
;		Box calculation.
;		--- -----------
		     if nd eq 0 then  sdi=dms(1) else sdi=nnd
		     if nd eq 0 then  sdj=dms(2) else sdj=nnd
		     fj =-float(ho) * 3.1416 / 180.
		     sco= sl_cos(fj) & ssi=sl_sin(fj)
		     fj = float(dm)/2
		     sbox(0,0) =di-1     & sbox(3,0)=di-1
		     sbox(0,1) =dj-1     & sbox(1,1)=dj-1
		     sbox(1,0) =di+sdi+1 & sbox(2,0)=di+sdi+1
		     sbox(3,1) =dj+sdj+1 & sbox(2,1)=dj+sdj+1
		     for k=0,3  do begin
			 j	  =sl_pfix(sco*(-fj+sbox(k,0)) $
					  -ssi*(-fj+sbox(k,1))+fj)
			 sbox(k,1)=sl_pfix(ssi*(-fj+sbox(k,0)) $
					  +sco*(-fj+sbox(k,1))+fj)
			 j	  =j*(ni+1)
			 if j lt 0 then j=0 else if j ge plx   then j=plx-1
			 sbox(k,0)=j
;
			 j	  =(ply-1) - bh* ((dm-1)-sbox(k,1))
			 if j le 0 then j=0 else if j ge ply-1 then j=ply-1
			 sbox(k,1)=j
			 sbox(k,2)=k & endfor
		     if (ho gt  90) and (ho le 180)  then begin sbox(0,2)=1
			 sbox(1,2)=2 & sbox(2,2)=3 & sbox(3,2)=0    & endif
		     if (ho gt 180) and (ho le 270)  then begin sbox(0,2)=2
			 sbox(1,2)=3 & sbox(2,2)=0 & sbox(3,2)=1    & endif
		     if (ho gt 270)		     then begin sbox(0,2)=3
			 sbox(1,2)=0 & sbox(2,2)=1 & sbox(3,2)=2    & endif
	endif else sbox(0,2)=-1
;
;**	Output matrix
;**	------ ------
	bb=1
	if scl ne 2 then bb=sl_psizm(erey,dms,2,plx,ply,4,-1,-1) $
		    else erey(*,*)=0
	if bb  eq 0 then return,0
	aa =tv_flg(2)-1

	if  fgg eq 1 then if (ni le 2) and (bh lt 4) then fgg=3

	surf,long(plx),long(ply),long(dm) ,long(vsz),long(ni),long(ah), $
		long(aa) ,long(lev),long(fgg),long(bg) ,tare,erey,sbox

;	bb=sl_surfex(long(plx),long(ply),long(dm) ,long(vsz),long(ni),long(ah),$
;	   	     long(aa) ,long(lev),long(fgg),long(bg),tare,erey,sbox)

	if scl eq 0   then bb=sl_dd(2,tare,tare_z)
 endelse
 b=1
endif
return,b
end
;
;
;
;
function sl_rotfun, flg,w,flp,flpb,erey,vsx,vsy,vsz,typ,az,ax,smo, x1,x2,y1,y2
;******* *********  *** * *** **** **** *** *** *** *** ** ** ***  ** ** ** **
;**
common my_rotfun   ,wf,azt,axt,fx1,fx2,fy1,fy2,px,py,stepz,stepx,s1,s2,s3,$
		    sso,fpp,fpb,fum,fux,ndx,ndy,ndz,rtyp,w_cw,w_no,w_od
;**
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common my_tv	   ,tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		    tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;carez + erey
;
	if (flg eq 0) then begin
		azt   =az
		axt   =ax
		if azt lt 0 then azt=0
		fx1   =x1
		fx2   =x2-1
		fy1   =y1
		fy2   =y2-1
		px    =fx2-fx1+1
		py    =fy2-fy1+1
		fpp   =flp
		fpb   =flpb
		sso   =smo
		ndx   =vsx
		ndy   =vsy
		ndz   =vsz
		rtyp  =typ
		if fpp  eq 2  then  begin
			s3=(px+py)/8
			bb=sl_psizm(arer,arer_z,2,s3,s3,typ,-1,-1)
			arer(0,0)=sl_redim(erey,vsx,vsy,typ,s3,s3,0)
		endif else begin
		   if vsz eq 1 then begin
			bb=sl_psizm(arer,arer_z,2,vsx,vsy,typ,-1 ,-1)
			arer(0,0) = erey
		   endif else begin
			bb=sl_psizm(arer,arer_z,3,vsx,vsy,vsz,typ,-1)
			arer(0,0,0)=erey
		endelse & endelse
		fux=sl_maxim(arer,arer_z,s1,fum)
		if  fum ne fux then wf=w else wf=-1
		s1   =30  / tv_mps & if s1 eq 0 then s1=1
		stepz=360
		s2   =axt/90 & s2=s2*90
		stepx=s2 +90
		endif
	if   (w eq wf) then begin
	  if (flg eq 1) then $
	     if flp eq 0 then begin if  s1  lt 0     then s1 =-s1
				    azt=azt+s1
				    if  azt gt stepz then azt=0
	     endif	 else begin
				    if  axt+s1 gt stepx then s1 =-s1 else $
				    if  axt+s1 le s2    then s1 =-s1
				    axt=axt+s1  & endelse
	  if (flg eq 3)  then begin azt=az
                                    axt=ax
				    if azt lt 0 then azt=0
				    s2 =axt /90 & s2=s2*90
				    stepx=s2+90 & endif
	  if (flg ne 2)  then begin
	     if fpp eq 2  then begin
;cici		bb=sl_tvras   (fx1,fy1,px,py,0,fx2,fy2)
		bb=sl_tvscreen(fx1,fx2,fy1,fy2)
		bb=sl_tvget(4,w_no)
		bb=sl_tvset(4,1)
		bb=sl_surf (0,arer,s3,s3,1,rtyp,px,py,fum,fux, $
						azt,axt,1,2,fpb,sso)
		bb=sl_tvset(4,w_no)
	     endif else begin
		s3=flg+1 & if flg eq 3 then s3=2
		bb=sl_surf (s3,arer,ndx,ndy,ndz,rtyp,px,py,fum,fux, $
						azt,axt,-1,fpp,fpb,sso)
		bb=sl_psiz(arer_z,2,px,py,4,-1,-1)
		bb=sl_tvget(7,w_od)
		bb=sl_tvset(7,1)
		bb=sl_tvimag(arer,arer_z,fx1,fy1)
		bb=sl_tvset(7,w_od)  & endelse
	  endif else  begin
		az=azt & ax=axt      & endelse
	endif
return,1
end
;
;
;
;
function sl_getentry,	w,windn
;******* ***********
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
				  bb=sl_tvgetwn(w)
				  if w  le 0    then  $
				     for i=0,tv_wsz(1)-1  do $
					 if (tv_win(0 ,i) gt 0) and $
					    (tv_win(20,i) ne 0) then begin
							  w= tv_win(0,i)
							  tv_win(0,i)=-2
							  if tv_lst eq w then $
							     tv_lst=0
							  bb=sl_tvdelwn(w)
							  i =tv_wsz(1)
							  endif
				  if  w le 0 then $
					if tv_lst gt 1 then w=tv_lst else w=1
				  if windn lt 0 then begin
				     for i = 1 ,  tv_wsz(1)-1 , 1   do $
					 if (tv_win(0,i) le  0) and $
					    (tv_win(0,i) ne -2) then windn = i
				     if  windn lt 0 then windn=0
				  endif
return,1
end
;
;
;
;
function sl_views,  erey, windn, ttl , c_c,l_c,sp_c,f_c,fcs ,vsiz
;******* ********   ****  *****  ***   ********************  ****
;**
;** Or just an image for Loafers.
;** -- ---- -- ----- --- -------
;**
common my_space,si,sj,sx,sy,sz,px1,px2,py1,py2,fdx,fdy,fdz,dx,dy,vssz,res,stt
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common	my_tvg,	w_co,w_cw,w_fl,w_fy,w_hi,w_lo,w_lt,w_nc,w_no,w_od,$
		w_ps,w_ty,w_ig,w_wk
;**
common my_keep,	rvl,rvm,vlt,vmt
;**
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_ovs,	ov_sum1,ov_sum2,ov_sum3,ov_sum4,ov_sum5,ov_sum6,ov_sum7,$
		ovs1_z ,ovs2_z ,ovs3_z ,ovs4_z ,ovs5_z ,ovs6_z ,$
		ov_pmx,ov_f,ov_z,ov_l,ov_m
;**
common my_views,  abt,az,ax,bbx,bby,bcx,bcy,bti,btj,btx,bty,btw,bwx,bwy,c1,cc ,$
		  cf,cm,ck1,dif,dif3,fc,fcg,fic,fil,fmt,four,fxy,fx,fy,f_0,f_1,$
		  f_2,f_3,f_4,f_5,f_6,f_7,hh,ii2,ii3,ii6,k1,k2,kk,lc,lk2,mn,mx
common my_views2, mx1,mx2,nx,ny,nz,o,op4,op5,plx,ply,pp,rot,spc,spm,spt,stc   ,$
		  stf,stl,spm_t,spt_t,tip,tite,titx,vsis,vsx,vsy,vsz,w,xsiz ,$
		  xdm,ydm,zdm,vxl,vxm,km,bxa,bya,v_vx4,v_vy4
;**
common  my_annot, an_gm,an_gh,an_gf,an_ttl1,an_ttl2,an_xlab,an_ylab,an_ttm, $
		  an_zlab,an_com1,an_com2,an_unit,an_offs,an_i,an_r,an_f6
;**
;carez + erey
;care rvl,rvm,vl ,fx,fy,c1,mn,mx,mx1,mx2
;
;pro int
;pro vid sl_tv*
;pro sl  sl_d_p,sl_spacial,sl_savarea,sl_redim,sl_lis,sl_revs,sl_scale,sl_str,
;	 sl_sqrt,sl_index,sl_sti
;**	Any param. must be ok
;**	--- -----  ---- -- --
	bb  =sl_tvget(1 ,w_co)
	bb  =sl_tvget(3 ,w_cw)
	bb  =sl_tvget(4 ,w_no)
	bb  =sl_tvget(7 ,w_od)
	bb  =sl_tvget(8 ,w_ty)
	bb  =sl_tvget(9 ,w_ps)
	bb  =sl_tvget(17,w_nc)
	bb  =sl_tvget(18,w_lt)
	bb  =sl_tvget(21,w_ig)
	fcg(*)=0
	kk    =0
	if c_c lt 0 then begin  cc =0 & lc =-1 & spc= 0     & tite=spm_t(4)
				fc =0 & rvm= 0 & rvl= 0     & fcg(0)=-1 & kk=-1
	endif	    else begin  cc =c_c  & lc =l_c & fc=f_c & tite=ttl
				spc=sp_c & fcg=fcs & endelse
;**
	sl_lampscan, 'get_size', 0,0, baseview, 0
;**
      repeat begin
      recurs =0
;**
      ab  =0
      if  fcg(0)  lt 0  then xsiz(0)=sl_size(erey) else xsiz(0)=vsiz(*)
      if (xsiz(0) gt 1) and (xsiz(0) le 4) and (tv_wsz(1) gt windn) then begin
	if (xsiz(0) ge 3) then nz= xsiz(3)  else  nz = 1
	vsis = xsiz(*)
	four = xsiz(0)
	xdm  = xsiz(1)
	ydm  = xsiz(2)
	zdm  = nz
	dif  = 0
	stc  = 0     & stl = 0     & stf = fc
	fic  = xdm-1 & fil = ydm-1
	if fcg(0)  ne -1  then  begin
	   vsz    =  fcg(2)
	   if (xsiz(1) ne fcg(0)) or (xsiz(2) ne fcg(1))  then     begin
	       xsiz(1) =  fcg(0)  &   xsiz(2) =  fcg(1) & dif = 1
	       fic     =  fcg(0)+cc-1 & fil   =  fcg(1)+lc-1
	       stc     =  cc	  &   stl     =  lc     & endif
	endif  else vsz=  nz-stf
	tip  = xsiz(xsiz(0)+1)
	spt  = spc/10
	spm  = spc-10*spt  & if spm  lt 0 then spm =-spm
	rot  = -1
	btw  = windn
	vsx  = xsiz(1)
	vsy  = xsiz(2)
	nz   = vsz
	xsiz(0 )=2	   & xsiz( 3)=tip
	vsis( 7)=stc	   & vsis( 8)=stl	& vsis( 9)=stf
	vsis(10)=vsx-1     & vsis(11)=vsy-1	& vsis(12)=vsz-1
	vsis(13)=vsx-1+stc & vsis(14)=vsy-1+stl & vsis(15)=vsz-1+stf
	dif3 = dif & if zdm ne nz then dif3=1
;**	Min Max.
;**	--- ---
	km =0
	if (rvm eq rvl) then begin
			km=1
			if  (not dif3) then begin
			     rvl = sl_minf(erey,vsis,cf)
			     rvm = sl_maxf(erey,vsis,cm) & endif $
			else if (zdm eq 1) then begin
			     if (ydm gt 1) then $
			     rvl = sl_minf(erey(stc:fic,stl:fil),xsiz,cf) else $
			     rvl = sl_minf(erey(stc:fic,*)	,xsiz,cf)
			     if (ydm gt 1) then $
			     rvm = sl_maxf(erey(stc:fic,stl:fil),xsiz,cm) else $
			     rvm = sl_maxf(erey(stc:fic,*)	,xsiz,cm)
			     k   = cf/vsx & j = cf - vsx*k
			     k	 = k +stl & j =  j + stc
			     cf  = k *xdm + j
			     k   = cm/vsx & j = cm - vsx*k
			     k	 = k +stl & j =  j + stc
			     cm  = k *xdm + j
			     endif else begin
				rvl=sl_minf(erey,vsis,cf)
				rvm=rvl
				for i=stf,stf+nz-1 do begin
				    if not dif then $
				      vl=sl_maxf(erey(*,*,i),xsiz,k1)    else $
				      vl=sl_maxf(erey(stc:fic,stl:fil,i),xsiz,k1)
				    if float(vl) ge float(rvm)	then begin
					k   = k1/vsx & j = k1 - vsx*k
					k   = k +stl & j =  j + stc
					cm  = k *xdm + j +(xdm* ydm*i)
					rvm=vl & endif
				endfor
			     endelse
	endif else cf  =-1
;**	Fill the screen anyway.
;**	---- --- ------ ------
	if f_fg(43) eq 1 then begin f_fg(43)=tv_w & tv_w=0 & endif
	nx   = (tv_x)     / xsiz(1)
	ny   = (tv_y-tv_w)/ xsiz(2)
;**			Special view.
;**			------- ----
	if (spt eq -3)  or (spm eq 6) or (spt eq -4)  then begin
			if spt eq -3 then abt(0)=spm else abt(0)=0
			if (fcg(0) eq -1) or (f_fg(44) ne 1) then bb=sl_d_p(40,erey,vsis,dif3,abt)
			f_fg(44)=1
			endif
	if (spt le -2)  then   begin
			if spt eq -2 then begin
			 if (fcg(0) eq -1) or (arec_z(6) eq 0)   then begin
			    if windn lt 0 then i=-windn-1 else   i=windn
			    rot= tv_win(26,i) & rot=2
			    ret= sl_spacial(erey,2,0,vsis,rot)
			    if fcg(0) eq - 1 then begin
				   abt(0)= i & abt(1)=cc & abt(2) =-1
				   bb =sl_savarea(3 ,arec,abt,arec_z) & endif
			 endif
			 xsiz(0)=arec_z  & nz=1
;**
			endif else if spt eq -3  then begin
			 xsiz(0)=2       & nz=1
			 xsiz(1)=vsx+vsz & xsiz(2)=vsy+vsz
;**
			endif else if spt eq -4  then begin
			 xsiz(0)=2       & nz=1
;**
			endif else if spt eq -6  then begin
			 xsiz(2)=xsiz(1)
			endif
;**
			nx    = (tv_x)      / xsiz(1)
			ny    = (tv_y-tv_w) / xsiz(2)
			endif
	k=6
	if (spt eq  2) and (vsy lt vsx) and (nx*ny gt 15*nz) then k=4
	if (spt eq  1)  or (spt eq  -4)  or (f_fg(10))	     then k=1
	bty  = xsiz(1)/(xsiz(2)*k) & if bty lt 1 then bty=1
	btx  = xsiz(2)/(xsiz(1)*k) & if btx lt 1 then btx=1
	bby  = bty & bbx=btx
	if  (nx*ny eq 0) or ((nx*ny lt nz) and ((fcg(0) ne -1) or (spm ne 0))) $
			then begin
;**			Reduce the view.
;**			------ --- ----
			plx  = xsiz(1)*btx
			ply  = xsiz(2)*bty
			fxy  = 1
			while (tv_x/(plx/fxy))*((tv_y-tv_w)/(ply/fxy)) lt nz $
			   do  fxy = fxy+1
			plx  = plx / fxy
			ply  = ply / fxy
			fxy  =-fxy
			nx   = tv_x	 /plx
			ny   =(tv_y-tv_w)/ply
			while (nx-1)* ny ge nz  do   nx = nx - 1
			while (ny-1)* nx ge nz  do   ny = ny - 1
	endif else begin
		if nz gt 1 then begin	vsz = nx*ny
					if nz gt vsz then nz=vsz else vsz=nz
					endif
;**	  square off a little bit.
;**       ------ --- - ------ ---
		if (btx*bty gt 1) then begin
			  if  btx gt 1 then while nx/btx*ny lt nz do btx=btx-1
			  if  bty gt 1 then while ny/bty*nx lt nz do bty=bty-1
			  nx   =  nx / btx
			  ny   =  ny / bty
			  endif
;**
;**	  goog good
;**	  ---- ----
	  c1  =  float ((nx*ny)/nz)
	  bb  =  sl_sqrt( c1,1)
	  fxy =  fix(c1)
	  if fxy gt  nx then fxy =nx
	  if fxy gt  ny then fxy =ny
	  if fxy lt  1  then fxy =1
	  if (fxy gt 12) and (spt ne -4) $
		         and (spt gt -6) and (f_fg(19) ) then begin
;**	  but not to big.
;**	  --- --- -- ---
		k   = nz
		i   = ny/fxy & if  k gt i then k=i
		i   = nx/fxy & if  k gt i then k=i & if  k lt 1 then k=1
;
		i   = ny*2/3/k  &  k  = nx*2/3/k
		if  k  gt i then   k=i  & if k gt 30  then k  =30
		if fxy gt k then fxy=k  & if fxy lt 1 then fxy=1
		endif
;**	  Choice  nx ny
;**       ------  -----
	  if (nx ge  ny) or (spt eq 2)   then begin
		   nx= nx/fxy & if nx gt nz   then  nx=nz
		   ny= nz/nx  & if ny lt 1    then  ny=1
		   if (nx eq nz) and (nz gt 1) and (ny gt 1) then nx=nx-1
	           if  nx*ny lt nz then  ny=ny+1 &  endif $
	  else begin ny= ny/fxy & if ny gt nz then  ny=nz
		   nx= nz/ny  & if nx lt 1    then  nx=1
		   if (ny eq nz) and (nz gt 1) and (nx gt 1) then ny=ny-1
	           if nx*ny lt nz then nx=nx + 1 &  endelse
	  while  (nx-1) *ny ge nz  do  nx=nx - 1
	  while  (ny-1) *nx ge nz  do  ny=ny - 1
	  i   =   tv_x  /(xsiz(1)*nx*btx)      & if fxy gt i then fxy=i
	  i   =  (tv_y-tv_w)/(xsiz(2)*ny*bty)  & if fxy gt i then fxy=i
					         if fxy lt 1 then fxy=1
	endelse
;**
	if fxy gt 0 then begin     plx= xsiz(1)*btx*  fxy
				   ply= xsiz(2)*bty*  fxy  & endif $
		    else begin     plx= xsiz(1)*btx/(-fxy)
				   ply= xsiz(2)*bty/(-fxy) & endelse
	bwx = plx*nx
	bwy = ply*ny
	bxa = 0
	bya = 0
;**	Specialities.
;**	------------
	if (spm eq 6) or (spt eq -3) or (f_fg(46) gt 0)  then begin
	    k= (bwx+bwy)/2
	    if (bwx+k gt tv_x) or (bwy+k gt (tv_y-tv_w) or $
		(f_fg(46) gt 0))    then begin
		if   fxy lt 0  then fxy=fxy*2 else fxy=fxy/2
		if   fxy eq 0  then fxy=-2
		if   fxy gt 0  then begin  plx= xsiz(1)*btx*  fxy
					   ply= xsiz(2)*bty*  fxy  & endif $
			       else begin  plx= xsiz(1)*btx/(-fxy)
					   ply= xsiz(2)*bty/(-fxy) & endelse
		bwx = plx*nx
		bwy = ply*ny
	    endif
	endif
;**
;**	Decrement if wanted.
;**	--------- -- ------
	if windn ge 0 then begin
	   if (tv_win(3 ,windn) gt fxy) then tv_win(19,windn)=0
	   if (tv_win(19,windn) ne  0 ) or  (f_fg(28) gt 0) then begin
			   fxy=fxy-tv_win(19,windn)
			   if  f_fg(28) gt 0 then $
				if tv_mps lt 7 then fxy=fxy-2
			   if (fxy eq 0) or (fxy eq -1) then fxy=-2
			   if  fxy gt 0 then begin plx= xsiz(1)*btx*  fxy
						   ply= xsiz(2)*bty*  fxy
		    	   endif	else begin plx= xsiz(1)*btx/(-fxy)
				   		   ply= xsiz(2)*bty/(-fxy)
						   endelse
			   bwx = plx*nx
			   bwy = ply*ny
			   endif
	endif
;**	Place for projections.
;**	----- --- -----------
	if (spm eq 6) or (spt eq -3) or (f_fg(46) gt 0) then begin
		k= (bwx+bwy)/2
		if  (bwx+k le tv_x)  and (bwy+k le (tv_y-tv_w)) then  begin
		     			  bwx=bwx+k & bwy=bwy+k
		endif else begin          bwx=tv_x  & bwy=tv_y-tv_w & endelse
	endif else $
	if (spt eq 2) then begin
;**	Place for vectors.
;**	----- --- -------
	  if (vsy gt 1) then begin
	    if fxy gt 0 then k= (tv_y-tv_w -bwy)/(vsy*  fxy * ny) $
			else k= (tv_y-tv_w -bwy)/(vsy/(-fxy)* ny)
	    if (k ge 1) then if  ply/bty*(bty+k)/ vsy gt 15 then begin
		ply=ply/bty*(bty+k)
		bty=bty+k
		bwy=ply*ny  & endif
	  endif else if  (nz eq 1) then begin
	   k=60
	   if (windn ge 0) then if (tv_win(69,windn) eq 1) then k=0
	   if  k gt 0 then begin
	    while (tv_x-bwx lt k) and  (btx gt 1)  do begin
					plx=plx/btx & btx=btx-1 & plx=plx*btx
					bwx=plx     & endwhile
	    if tv_x-bwx ge  k  then bwx=bwx+k
	    if bty gt tv_y - 3*tv_w then begin  bty= tv_y - 3*tv_w
						ply= bty  & bwy= bty & endif
	   endif
	  endif
	  if fxy gt 0 then if (bty*  fxy  le 15)  then spt=0
	  if fxy lt 0 then if (bty/(-fxy) le 15)  then spt=0
	endif else $
	if (spt eq 0) and (f_fg(28) gt 0) and (tv_mps ge 7) then begin
;**	Place for Flick image.
;**	----- --- ----- -----
;		if  (bwx*2 le tv_x)  and (bwy*2 le (tv_y-tv_w)) then begin
;		     			  bwx=bwx*2 & bwy=bwy*2  &   endif
	endif
;**	Place for annotation.
;**	----- --- ----------
	if windn ge 0 then if tv_win(69,windn) eq 1 then begin
		bxa=0 & bya=0
		bb=sl_str_to_long(-1,an_ttl1,tv_win,windn,70 ,64)
		if bb gt bxa then bxa=bb
		bb=sl_str_to_long(-1,an_ttl2,tv_win,windn,86 ,64)
		if bb gt bxa then bxa=bb
		bb=sl_str_to_long(-1,an_xlab,tv_win,windn,102,64)
		if bb gt bxa then bxa=bb
		bb=sl_str_to_long(-1,an_ylab,tv_win,windn,118,64)
		if bb gt bya then bya=bb
		bb=sl_str_to_long(-1,an_zlab,tv_win,windn,134,40)
		bb=sl_str_to_long(-1,an_com1,tv_win,windn,144,40)
		bb=sl_str_to_long(-1,an_com2,tv_win,windn,154,40)
		bya=120
		if bwy+bya gt tv_y  then bya=tv_y-bwy
                if bwx+160 lt bxa*8 then bxa=bxa*8+160-bwx else bxa=160
		if bwx+bxa gt tv_x  then bxa=tv_x-bwx
	endif
;**	Place for title.
;**	----- --- -----
	if bwx+bxa lt 96 then bwx=96
	if bwy+bya lt 96 then bwy=96
;**	Impose logarithmic display.
;**	------ ----------- -------
	if (f_fg(0) eq 0) and (rvm-rvl gt 10000) and (spt ne 2) then f_fg(0)=1
;**
;**	Get a window
;**	--- - ------
	w   =-1
	if windn  ge 0  then   begin
	 w  =tv_win(0,windn)
	 if (tv_win(1,windn) eq vsx) and (tv_win(2,windn) eq vsy  ) and  $
	    (tv_win(3,windn) eq fxy) and (tv_win(5,windn) eq vsz  ) and  $
	    (fcg(0) ne -1)   and (w gt 0)  then kk=1
	 if (f_fg(6) eq 2)		   then kk=0
	endif else kk=-1
;	if (tv_lst gt 0) and (tv_lst ne w) then	bb=sl_tvtidy(tv_lst,1)

			btw  =   windn
			if w le 0 then bb=sl_getentry(w,windn)
;**			Save info.
;**			---- ----
			tv_win(0 ,windn) = w
			tv_win(1 ,windn) = vsx
			tv_win(2 ,windn) = vsy
			tv_win(3 ,windn) = fxy
			tv_win(4 ,windn) = ny
			tv_win(5 ,windn) = vsz
			tv_win(6 ,windn) = cc
			tv_win(7 ,windn) = lc
			tv_win(8 ,windn) = fc
					   tv_win(9 ,windn) = btx
			if bty gt 1  then  tv_win(9 ,windn) =-bty
			tv_win(10,windn) =-1
			if spt le 0  then  tv_win(13,windn) = 10*spt-spm $
				     else  tv_win(13,windn) = 10*spt+spm
			if (cf ge 0) then  begin
					   tv_win(14,windn) = cf
					   tv_win(15,windn) = cm    & endif
			tv_win(16,windn) = tip
			tv_win(17,windn) = tv_od
			tv_win(18,windn) = tv_col
			tv_win(21,windn) = xdm
			tv_win(22,windn) = ydm
			tv_win(23,windn) = zdm
			tv_win(24,windn) = xsiz(1)
			tv_win(25,windn) = xsiz(2)
			if rot ge 0 then   tv_win(26,windn) = rot
			tv_win(28,windn) = bwx+bxa
			tv_win(29,windn) = bwy+bya
			tv_win(30,windn) = f_az
			tv_win(31,windn) = f_ax
			tv_win(32,windn) = f_fg(14)
			tv_win(33,windn) = f_fg(15)
			tv_win(34,windn) = f_fg(16)
			tv_win(35,windn) = f_fg(20)
			j=(tv_x-bwx)/2    & if btw lt 0 then j=0
			tv_win(36,windn) = j
			j=(tv_y-2*tv_w-bwy) & if j lt 0 then j=(tv_y-tv_w-bwy)
			tv_win(37,windn) = j/2
			tv_win(38,windn) = f_fg(0)
			tv_win(39,windn) = f_fg(10)
			tv_win(40,windn) = f_fg(12)
			tv_win(41,windn) = f_fg(22)
			tv_win(42,windn) = tv_flg(4)
			if fcg(0) eq -1    then begin
					   tv_win(6 ,windn) = 0
					   tv_win(7 ,windn) = 0
					   tv_win(11,windn) = cc
					   tv_win(12,windn) = lc  &  endif
			if btw    lt  0    then begin  btw  =-btw-1
					   tv_win(10,windn) = btw
					   tv_win(11,windn) = tv_win(11, btw)
					   tv_win(12,windn) = tv_win(12, btw)
			   		if cf lt 0 then begin
					   tv_win(14,windn) = tv_win(14, btw)
					   tv_win(15,windn) = tv_win(15, btw)
					endif
;					   tv_win(20,windn) = tv_win(20, btw)
					   tv_win(26,windn) = tv_win(26, btw)
					   tv_win(27,windn) = tv_win(27, btw)
;
				     tv_win(70:169,windn)=tv_win(70:169, btw)
			endif
;**		Verify size of previous display
;**		------ ---- -- -------- -------
		if kk gt 0 then begin bb=sl_tvsel(w)
				   if not bb then kk=0 else begin
;					bb=sl_tvwake(w)
					bb=sl_tvget(28,i)
					bb=sl_tvget(29,j)
					if (i ne bwx+bxa) or $
					   (j ne bwy+bya) then kk=0
				   endelse
		endif
                if kk le 0 then begin
;**			Make a title.
;**			---- - -----
			if (vsz eq 1) and (zdm gt 1)   then begin
					   tite  = spt_t (1)
					   btw   = tv_win(10,windn)
					   if btw lt 0 then btw=windn
					   bb=sl_sti(tite,sl_str(fc+1,ii3),7 )
					   bb=sl_sti(tite,sl_str(btw ,ii2),12)
			endif else begin   btw   = tv_win(10,windn)
			      if btw  ge 0 then    begin
				 if   tite eq 'x'  then $
				      if spm eq  1 then tite=spm_t(6)  else $
				      if spm eq  2 then tite=spm_t(7)  else $
				      if spm eq  3 then tite=spm_t(8)  else $
				      if spm eq  4 then tite=spm_t(9)  else $
				      if spm eq  5 then tite=spm_t(0)  else $
				      if spm eq  7 then tite=spm_t(1)  else $
				      if spm eq  8 then tite=spm_t(2)  else $
				      if spm eq  9 then tite=spm_t(3)  else $
							tite=spm_t(5)
				 bb=sl_sti(tite,sl_str(btw,ii2),8)
			      endif else   begin
			       j  = tv_win(11,windn)
			       if   tite eq 'x' then begin
				  if spt gt -2  then	tite=spt_t(0) $
						else	tite=spt_t(-spt)
				  endif
			       if j gt 0 then bb=sl_sti(tite,sl_str(j,ii6),8)
			       if tv_win(50,windn) le 0 then bb=0 else $
			          bb=sl_str_to_long(-1,titx,tv_win,windn,60,32)
			       if bb gt 0 then tite=tite+' ('+titx+')'
			       if tv_flg(16) gt 0 then tite=tite+ ' --> W' + sl_str(tv_flg(16),ii2)
			endelse &   endelse
			bb=sl_sti(tite,sl_str(windn,ii2),0)
;**
			op5=0 & op4=0
			if f_fg(4) and (bwx+bxa lt tv_x-30) $
				   and (bwy+bya lt tv_y-45-tv_w) then op5=2 $
				   else op4=1
		endif
;
	if baseview le 0 then tv_lst  = w
	bb  =sl_tvset(4 ,1)
	bb  =sl_tvset(7 ,tv_od)
	bb  =sl_tvset(8 ,0,0,0,0,0,1,1)
	bb  =sl_tvset(17,1)
	ax	= f_ax
	az	= f_az
	f_0	= 0
	f_2	= 0
	f_1	= 1
	f_3	= 0
	fx	= float(fxy*btx)
	fy	= float(fxy*bty)
	if  fxy lt 0  then  begin f_1=0 & o=1
			fx =float(btx)/(-fxy) & fy=float(bty)/(-fxy) & endif
	if tip eq 2  then if (rvl eq 0) and (rvm eq tv_flg(2)-1) then f_2=1
	if tip ne 64 then if (cf  ge 0) and (rvl ge 0) and (rvm le tv_flg(2)) $
		      and (rvm-rvl ge tv_flg(2)/2)		 then f_2=1
;**	Specials
	if (f_fg(0) eq 1)		   then f_2=0
	if (spt eq -2) or (spt eq -3)	   then f_2=1
	if (spt eq -1)			   then f_4=1   else    f_4=0
	if (spt eq  1) or (spt eq -4)	   then f_5=1   else    f_5=0
	if (spt eq  0) or (spt eq -2)  or (spt eq -3)	then	f_0=1
	if (spt eq  2) then if (fy  gt 15) then f_3=1	else	f_0=1
	if (spc ne  0) or (dif) or (zdm ne 1) or (not f_2) then f_7=0 $
							else	f_7=1
	if  f_4 and (vsy eq 1) then begin       f_3=1 & f_4=0 & endif
	if  f_5 and (vsy eq 1) then begin       f_3=1 & f_5=0 & endif
	if  f_4 then begin c1  =(float(rvm)-float(rvl))/ f_fg(15) & endif
	if  f_3 then begin bb  =sl_psizm(arei,arei_z,1,vsx,4,-1,-1,-1)
			   arei=sl_index(vsx,4)	      & f_2=1 & endif
	if  f_5 then begin
		     f_6 = 1
		     bcx = xsiz(1)/32 & if bcx lt 1 then bcx=1
		     bcy = xsiz(2)/32 & if bcy lt 1 then bcy=1
		     i	 = bcy*bby
		     bcx = bcx*bbx    & if bcx gt i then bcx=i
		     i	 = bcx*bby
		     bcy = bcy*bbx    & if bcy gt i then bcy=i
		     if  bcx*bcy eq 1  then begin bcx=xsiz(1) & bcy=xsiz(2)
						  f_6=0	      & endif  $
				       else begin bcx=xsiz(1)/bcx
						  bcy=xsiz(2)/bcy
						  if bcx lt 2 then bcx=xsiz(1)
						  if bcy lt 2 then bcy=xsiz(2)
						  endelse
		     endif else f_6=0
	if  btx*bty*fxy gt 1 then begin  f_1=0
		if  fxy gt 9 then o=9 else o=fxy
		if (f_0) and tv_flg(0) and ((f_fg(12) ne 1) or (o lt 3)) $
		then f_1=2 & endif
	vsis(16)=0
	if f_fg(6)  ne 0 then  $
	if nz  gt 1 then begin
;**	View a scan.
;**     ---- - ----
		if f_fg(19) then $
		if kk le 0  then begin
			if baseview gt 0 then i=-baseview else i=7

			if (kk eq 0) and (tv_flg(1) ne 0) and (i eq 7) then begin
			   bb=sl_glory(-2)
			   bb=sl_vecfun(-2,0)
;			   i =i+3
			endif
			bb=sl_tvlux(w,bwx+bxa,bwy+bya,tite,0,0,0,op4,op5,$
			            0,0,0,tv_win(36,windn),tv_win(37,windn), i)
 			bb=sl_tvget(28,i)     & bb=sl_tvget(29,j)
			if i gt 0 then tv_win(28,windn)=i
			if j gt 0 then tv_win(29,windn)=j
		endif else if ((not f_0) or (tv_win(69,windn) eq 1)) and $
			       (f_fg(11) eq -1) then bb=sl_tvclear(dummy)
		if f_1 eq 2 then    bb=sl_tvpix(fix(fx),fix(fy))
		bti      = 0
		k        = stf
		for i=1,nx do begin
;**		--- ------
		btj  = ply*(ny-1)
		for j=1,ny do begin
;**		--- ------
		   if (k lt nz+stf) and ((f_fg(11) eq -1) or (f_fg(11) eq k))$
		   then begin
;**
;prov
		    vxl=rvl & vxm=rvm
		    provw0,erey,k,w_co,w_lt
		    bb=sl_tvs(bti,btj,sl_str(k+1,ii3),1.,0,-1)
;**
		   endif
		   btj  = btj - ply
		   k    = k   + 1
		endfor
		pp(0)=bti  & pp(1) =bti+plx
		if f_fg(19)  then   begin
		   bb=sl_tvset(18,1)
		   bb=sl_tvscreen(0,(plx*nx)-1,0,(ply*ny)-1)
		   bb=sl_tvxyz   (0,(plx*nx)-1,0,(ply*ny)-1)
		   for j=ply*ny-1,0,-ply do begin
				    hh(0)=j
				    hh(1)=j
				    bb	 =sl_tvline(pp,hh,2,0,-1) & endfor
		   bti  =bti + plx
		   pp(0)=bti-1   &  pp(1)=pp (0)
		   hh(0)=0	 &  hh(1)=ply*ny-1
		   bb   =sl_tvline (pp,hh,  2,0,-1)
		   bb=sl_tvset(18,0)
		endif
		endfor
		bb=sl_tvpix(1,1)
		bb=sl_dd(2,ared,ared_z)

		if f_fg(46) ne 0 then if f_fg(19) then begin
			if rvm ge vmt then vxm=rvl+(rvm-rvl)/2 else vxm=rvm
			bti=bwx - plx*nx
			btj=bwy - ply*ny
			bb=sl_tvscreen(0,bti-1,0,btj-1)
;			bb=sl_tvxyz   (0,bti-1,0,btj-1)
			if dif3 eq 0 then bb =sl_pogons(erey,vsiz,vxm) $
			else bb =sl_pogons(erey(stc:fic,stl:fil,stf:stf+nz-1),vsiz,vxm)
			bb =sl_psizm(ared,ared_z,2,bti,btj,2,-1,-1)
			bb =sl_shadoc(1,ared,bti,btj, 30.,30.,0.)
			bb =sl_shadoc(0)
			bb =sl_tvimag(ared,ared_z,plx*nx,ply*ny)
			bb =sl_dd(2,ared,ared_z)
		endif

	endif else begin
;**	View a frame.
;**     ---- - -----
;**
;prov
;**
		    vxl=rvl & vxm=rvm
		    k=stf
		    provw1,erey,k,w_co,windn ,baseview
		endelse
;**
	if tv_win(69,windn) eq 1 then begin
		bti=160-bxa & btj=120-bya
		if bti lt 0 then bti=0
		v_vx4(0)=0           & v_vx4(1)=bwx+bxa-1
		v_vx4(2)=v_vx4(1)    & v_vx4(3)=0
		v_vy4(0)=bwy+bya-1   & v_vy4(1)=v_vy4(0)
		v_vy4(2)=bwy-btj     & v_vy4(3)=v_vy4(2)
		bb =sl_tvpol(4,v_vx4,v_vy4,tv_nc-1,0)
;
		v_vx4(0)=bwx+bxa-1   & v_vx4(1)=v_vx4(0)
		v_vx4(2)=bwx-bti     & v_vx4(3)=v_vx4(2)
		v_vy4(0)=bwy-btj     & v_vy4(1)=0
		v_vy4(2)=0           & v_vy4(3)=v_vy4(0)
		bb =sl_tvpol(4,v_vx4,v_vy4,tv_nc-1,0)
;               Draw a black border for window.
		v_vx4(0)=0           & v_vx4(1)=bwx+bxa-1
		v_vx4(2)=v_vx4(1)    & v_vx4(3)=0
		v_vy4(0)=0           & v_vy4(1)=v_vy4(0)
		v_vy4(2)=bwy+bya-1   & v_vy4(3)=v_vy4(2)
		bb =sl_tvline (v_vx4,v_vy4,4,0,tv_nc/25)
;
;		Image in a white border.
		pp(0)=1 & pp(1)=1   & hh(0)=bwy & hh(1)=1
		bb =sl_tvline (pp,hh,2,0,tv_nc-1)
		pp(0)=1 & pp(1)=bwx & hh(0)=1   & hh(1)=1
		bb =sl_tvline (pp,hh,2,0,tv_nc-1)
;		+ a black border
;		v_vx4(0)=2           & v_vx4(1)=bwx-bti-1
;		v_vx4(2)=v_vx4(1)    & v_vx4(3)=2
;		v_vy4(0)=2           & v_vy4(1)=v_vy4(0)
;		v_vy4(2)=bwy-btj-1   & v_vy4(3)=v_vy4(2)
;		bb =sl_tvline (v_vx4,v_vy4,4,0,0)
;
		if spt eq -2 then begin
			bbx=px1*(sx-1)  & if bbx eq 0 then bbx=px2*(sz-1)
			bbx=fix(fx*(bbx+1))-1
			k  =fix(fy*(sz +1))
			bby=     sy-1
			bby=fix(fy*(bby+1))-1 +k
		endif else $
		if spt eq -3 then begin
			bbx=fix(fx*vsx)-1
			k  =0
			bby=fix(fy*vsy)-1 +k
		endif else begin
			bbx=plx-1
			k  =0
			bby=ply-1 +k
		endelse
		if bbx ge bwx-bti then ck1=bwx-bti-1 else ck1=bbx
		if bby ge bwy-btj then lk2=bwy-btj-1 else lk2=bby
		k1 =vsx- (bbx-ck1)/fx
		k2 =     (bby-lk2)/fy+1

		bb =sl_tvset(8 ,0,0,1,1,0,0,0)
		bb =sl_tvset(1 ,tv_nc/20)
		bb =sl_tvget(6 ,w_fy)
		bb =sl_tvset(6 ,-1)
		bb =sl_tvset(13,3)
		bb =sl_tvt(10,bwy+bya-20,an_f6+an_ttl1,1.8,0,tv_nc/15)
		bb =sl_tvt(10,bwy+bya-45,an_f6+an_ttl2,1.8,0,tv_nc/15)
		bb =sl_tvscreen(2,ck1,0, bwy-btj+15)
		bb =sl_tvaxis (1 ,k1  ,6,an_f6+an_xlab+an_f6,1.5,'')

		bb =sl_tvset(14,3)
		bb=sl_tvscreen(0,bwx-bti+15, k+2,lk2)
		if spt   eq 2 then $
			bb =sl_tvaxis(vxl,vxm,2,an_ylab+an_f6,1.5) else $
		if tv_od eq 1 then $
			bb =sl_tvaxis(vsy,k2 ,2,an_ylab+an_f6,1.5) $
		else	bb =sl_tvaxis(k2,vsy ,2,an_ylab+an_f6,1.5)
		bb =sl_tvset(6 ,w_fy)
	endif
;**
	f_fg(6 )= 1
	f_fg(11)=-1
	if f_fg(21) eq 1 then f_fg(21)= 0
	if f_fg(43) gt 1 then if bwy le  tv_y-f_fg(43) then begin
			      tv_w=f_fg(43) & f_fg(43) =0 & endif
	ab	= 1
;**
;**
	endif
	endrep until (not recurs)
;**
	sl_lampscan, 'clear_size', 0,0, 0,0
;**
	if w_cw gt 0 then i=sl_tvsels(w_cw)
	bb  =sl_tvxyz(0,0,0,0)
	bb  =sl_tvset(1,w_co)
	bb  =sl_tvset(4,w_no)
	bb  =sl_tvset(7,w_od)
	bb  =sl_tvset(8,1,1,0,0,0,0,0)
	bb  =sl_tvset(9,w_ps)
	bb  =sl_tvset(13,0)
	bb  =sl_tvset(14,0)
	bb  =sl_tvset(17,w_nc)
	bb  =sl_tvset(18,w_lt)
	bb  =sl_tvset(21,w_ig)
return, ab
end
;
;
pro provw0, erey,kf,w_co,w_lt
;** ******
;**
common my_space,si,sj,sx,sy,sz,px1,px2,py1,py2,fdx,fdy,fdz,dx,dy,vssz,res,stt
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_ovs,	ov_sum1,ov_sum2,ov_sum3,ov_sum4,ov_sum5,ov_sum6,ov_sum7,$
		ovs1_z ,ovs2_z ,ovs3_z ,ovs4_z ,ovs5_z ,ovs6_z ,$
		ov_pmx,ov_f,ov_z,ov_l,ov_m
;**
common my_views,  abt,az,ax,bbx,bby,bcx,bcy,bti,btj,btx,bty,btw,bwx,bwy,c1,cc ,$
		  cf,cm,ck1,dif,dif3,fc,fcg,fic,fil,fmt,four,fxy,fx,fy,f_0,f_1,$
		  f_2,f_3,f_4,f_5,f_6,f_7,hh,ii2,ii3,ii6,k1,k2,kk,lc,lk2,mn,mx
common my_views2, mx1,mx2,nx,ny,nz,o,op4,op5,plx,ply,pp,rot,spc,spm,spt,stc   ,$
		  stf,stl,spm_t,spt_t,tip,tite,titx,vsis,vsx,vsy,vsz,w,xsiz ,$
		  xdm,ydm,zdm,vxl,vxm,km,bxa,bya,v_vx4,v_vy4
;**
		    if  spt ne -6   then begin
		        k1=tip
			if (f_fg(0) eq 1) and (tip lt 32) then tip=8
			bb=sl_psizm(ared,ared_z,2,vsx,vsy,tip,-1,-1)
			if dif  then ared(0,0)=erey(stc:fic,stl:fil,kf)   $
				else ared(0,0)=erey(   *   ,   *   ,kf)
;
			if (f_fg(0) eq 1) then bb=sl_dislog(ared,ared_z,vxl,vxm)
;
			if (f_fg(22) eq 1) then begin km=1
			        vxm=sl_maxim(ared,ared_z,cm,vxl) & endif
		    endif else begin
			bb=sl_molprep(plx,ax,az,kf)
		    endelse
;**
		       if  f_fg(19) then $
		       if (f_fg(11) eq  kf) and (not f_0) then $
			   bb=sl_tvras(bti,btj,plx,ply,0,bwx-1,bwy-1)
;**
		       bb=sl_tvscreen(bti,bti+plx-1,btj,btj+ply-1)
		       if f_3 then bb=sl_tvxyz(0,vsx-1 ,vxl,vxm) $
			      else bb=sl_tvxyz(0,vsx-1 ,0,vsy-1)
;**
		    if	  f_0 then begin
		       k1=tip
		       if not f_2  then	begin
			  bb=sl_scalf(ared,ared_z,vxl,vxm,km,2,dummy,tv_flg(2))
;			  ared=sl_scale(ared,ared_z(1),ared_z(2),tip,vxl,vxm)
;			  ared_z(3)=2
			  tip =2 & endif
		       if f_1 eq 0 then begin
			  ared=sl_redim(ared,ared_z(1),ared_z(2),tip,plx,ply,0)
			  bb  =sl_psiz(ared_z, -1,plx,ply,-1,-1,-1)
		    	  if (f_fg(12) eq 1) and (o gt 2) then $
					bb=sl_lis   (ared   ,plx,ply,tip,o,1)
			  endif
		       if f_fg(19) then bb=sl_tvimag(ared,ared_z,bti,btj)
		       tip=k1
		       endif else $
		    if	  f_3 then begin
			  if cf lt 0 then bb=sl_d_p(0,ared,ared_z,0,0,vxl,vxm)
			  k1= bti+plx-1
			  k2= btj+ply
			  bb= sl_tvset(1,tv_nc/1.1)
			  bb= sl_tvset(18,w_lt)
			  bb= sl_psizm(tare,tare_z,1,vsx,tip,-1,-1,-1)
			  if f_fg(19)  then $
			  for l=long(0),vsy-1 do begin
				if tv_od eq 0 then $
				  bb=sl_tvscreen(bti,k1,btj+fy*l,btj+fy*(l+1)-1)$
				  else $
				  bb=sl_tvscreen(bti,k1,k2 -fy*(l+1),k2- fy*l-1)
				tare(0)=ared(*,l)
				bb=sl_tvfill(0,arei,vsx-1,vxl,tare,vxl,tv_nc/2,3,0)
				bb=sl_tvplt (-1,vsx,tare,0)
				endfor
			  bb=sl_dd(2,tare,tare_z)
			  bb=sl_tvset(1,w_co)
			  endif else $
		    if	  f_4 then begin
		      if  f_fg(16) eq 12  then begin
;				   if tv_od eq 1 then bb=sl_tvscreen(bti,bti+plx-1,btj+ply-1,btj) ;!!??

				   if f_fg(19)   then $
				   bb=sl_surf(0,ared,vsx,vsy,1,tip,plx,ply,vxl,$
				   vxm,-1,90,f_fg(15),f_fg(16),f_fg(20),f_fg(12))
		      endif  else  begin
				   bb=sl_surf(0,ared,vsx,vsy,1,tip,plx,ply,vxl,$
				   vxm,az,ax,f_fg(15),f_fg(16),f_fg(20),f_fg(12))
		        	   bb=sl_tvset(7,1)
				   bb=sl_psiz(ared_z,2,plx,ply,4,-1,-1)
				   if f_fg(19)   then $
				   bb=sl_tvimag(ared,ared_z,bti,btj)
		      endelse
		      endif else $
		    if	  f_5 then begin
		     if   f_fg(21) ne 1 then begin
		      if  f_fg(14) eq 2 then begin
		        if cf  lt 0 then bb=sl_d_p(0,ared,ared_z,0,0,vxl,vxm)
		        ared(ared_z(1)-1,0)=vxm
		        if f_6  then begin
			   ared=sl_redim(ared,ared_z(1),ared_z(2),tip,bcx,bcy,0)
			   bb  =sl_psiz(ared_z,-1,bcx,bcy,-1,-1,-1) & endif
;			if tv_od eq 1 then bb=sl_tvscreen(bti+plx-1,bti,btj,btj+ply-1) ;!!??
			if f_fg(19) then $
		        bb=sl_surf    (0,ared,bcx,bcy,1,tip,plx,ply,$
				     vxl,vxm,az,ax,f_fg(15),f_fg(14),f_fg(20),1)
		      endif else begin
		        bb=sl_surf    (0,ared,vsx,vsy,1,tip,plx,ply,$
			      vxl,vxm,az,ax,f_fg(15),f_fg(14),f_fg(20),1)
		        bb=sl_tvset(7,1)
			bb=sl_psiz(ared_z,2,plx,ply,4,-1,-1)
			if f_fg(19) then $
			bb=sl_tvimag(ared,ared_z,bti,btj)
		      endelse
		     endif else if f_fg(11) ge 0  then  $
			bb=sl_rotfun(0,w,f_fg(14),f_fg(20),ared,vsx,vsy,1,tip,$
					 az,ax,1,bti,bti+plx,btj,btj+ply)
		     endif else $
		    if	spt  eq -6  then begin
			if f_fg(19) then bb=sl_molout(kf,bti,btj)
		    endif
return
end
;
;
;
pro provw1, erey,kf,w_co,windn ,baseview
;** ******
;**
common my_space,si,sj,sx,sy,sz,px1,px2,py1,py2,fdx,fdy,fdz,dx,dy,vssz,res,stt
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_ovs,	ov_sum1,ov_sum2,ov_sum3,ov_sum4,ov_sum5,ov_sum6,ov_sum7,$
		ovs1_z ,ovs2_z ,ovs3_z ,ovs4_z ,ovs5_z ,ovs6_z ,$
		ov_pmx,ov_f,ov_z,ov_l,ov_m
;**
common my_views,  abt,az,ax,bbx,bby,bcx,bcy,bti,btj,btx,bty,btw,bwx,bwy,c1,cc ,$
		  cf,cm,ck1,dif,dif3,fc,fcg,fic,fil,fmt,four,fxy,fx,fy,f_0,f_1,$
		  f_2,f_3,f_4,f_5,f_6,f_7,hh,ii2,ii3,ii6,k1,k2,kk,lc,lk2,mn,mx
common my_views2, mx1,mx2,nx,ny,nz,o,op4,op5,plx,ply,pp,rot,spc,spm,spt,stc   ,$
		  stf,stl,spm_t,spt_t,tip,tite,titx,vsis,vsx,vsy,vsz,w,xsiz ,$
		  xdm,ydm,zdm,vxl,vxm,km,bxa,bya,v_vx4,v_vy4
;**		Make output matrix...
;**		---- ------ ------
		k1= tip
		if  spt gt -2 then begin
		  if (f_fg(0) eq 1) and (tip lt 32) then tip=8
		  if f_7 eq 0 then bb=sl_psizm(ared,ared_z,2,vsx,vsy,tip,-1,-1)$
			      else bb=sl_dd(2 ,ared,ared_z)
;
		  if zdm eq 1 then begin
		         if   dif  then if vsy eq 1 then begin ared=erey
					  bb=sl_d_p(1,ared,vsis,dif)
					endif else			       $
					  ared(0,0)=erey(stc:fic,stl:fil) else $
			 if f_7 eq 0 then ared(0,0)=erey & endif	       $
		  else if dif then	  ared(0,0)=erey(stc:fic,stl:fil, kf)  $
			      else	  ared(0,0)=erey(   *   ,   *   , kf)
;
		  if (f_fg(0) eq 1) then bb=sl_dislog(ared,ared_z,vxl,vxm)
;
;			 vxm=sl_maxim(ared,ared_z,cm,vxl) & km=1 & endif
;**		Specials..
;**		--------
		endif else if spt eq -2 then begin
		     bb=sl_psizm(ared,ared_z,2,xsiz(1),xsiz(2),xsiz(4),-1,-1)
		     ared(0,0)   = arec (*,*,0)
		     bb=sl_scalf(ared,ared_z,0,0,0,2,dummy,tv_flg(2))
;		     ared=sl_scale(ared,ared_z(1),ared_z(2),ared_z(3),0,0)
;		     ared_z(3)=tip
		     tip =2
;**
		endif else if spt eq -3 then begin
		     if (f_fg(0) eq 1) and (tip lt 32) then tip=8 else tip=2
		     bb=sl_psizm(ared,ared_z,2,xsiz(1),xsiz(2),  2 ,-1,-1)
		     bb=sl_psizm(vare,vare_z,2,vsx,vsz,		tip,-1,-1)
		     bb=sl_psizm(tare,tare_z,2,vsy,vsz,		tip,-1,-1)
;
		     if (f_fg(0) ne 1) then  begin
			bb=sl_scalf(ov_sum3,ovs3_z,0,0,0,0,ared,tv_flg(2))
			bb=sl_scalf(ov_sum2,ovs2_z,0,0,0,0,vare,tv_flg(2))
			bb=sl_scalf(ov_sum1,ovs1_z,0,0,0,0,tare,tv_flg(2))
;			ared(0,0)=sl_scale(ov_sum3,vsx,vsy    ,8,0,0)
;			vare(0,0)=sl_scale(ov_sum2,vsx,vsz    ,8,0,0)
;			tare(0,0)=sl_scale(ov_sum1,vsy,vsz    ,8,0,0)
		     endif else begin
		        bb=sl_psizm(sare,sare_z,2 ,vsx,vsy,	tip,-1,-1)
			sare(0,0)=ov_sum3 & vare(0,0)=ov_sum2 & tare(0,0)=ov_sum1
			bb=sl_d_p(30, sare,sare_z,0,0,vxl,vxm)
			bb=sl_d_p(30, vare,vare_z,0,0,vxl,vxm)
			bb=sl_d_p(30, tare,tare_z,0,0,vxl,vxm)
			ared(0,0)=sl_scale(sare,vsx,vsy,tip,0,0)
			bb=sl_scalf  (vare,vare_z,0,0,0,2,dummy,256)
			bb=sl_scalf  (tare,tare_z,0,0,0,2,dummy,256)
;			vare=sl_scale(vare,vsx,vsz,tip,0,0)
;			tare=sl_scale(tare,vsy,vsz,tip,0,0)
;				vare (vare(0)+1)=2 & tare(tare(0)+1)=2
			tip=2
			bb=sl_dd(2,   sare,sare_z)
		     endelse
;
			     for   i=long(0),vsz-1 do begin
				     ared(i ,vsy+i) = vare(*,i)
			       for j=long(0),vsy-1 do $
				     ared(vsx+i,i+j)= tare(j,i)     & endfor
			     bb=sl_dd(2,vare,vare_z)
			     bb=sl_dd(2,tare,tare_z)
;**
		endif else if spt eq -4 then begin
		      if (f_fg(0) eq 1) and (tip lt 32) then tip =8
		      bb=sl_psizm(ared,ared_z,3,vsx,vsy,vsz ,tip,-1)
		      if dif then ared(0,0,0)=erey(stc:fic,stl:fil,kf:kf+vsz-1) $
			     else ared(0,0,0)=erey(   *   ,   *   ,kf:kf+vsz-1)
;
		      if (f_fg(0) eq 1) then bb=sl_dislog(ared,ared_z,vxl,vxm)
;
;			 vxm=sl_maxim(ared,ared_z,cm,vxl) & km=1 & endif
;
		endif else if spt eq -6 then begin
		      bb=sl_molprep(plx,ax,az,kf)
		      endif
;**
;**		Bound the matrix....
		    if (f_3 or f_4 or f_5) and (cf lt 0) then $
				     bb=sl_d_p(0,ared,ared_z,0,0,vxl,vxm)
		    if spm eq 6 then begin
		       if f_0   then bb=sl_d_p(0,ared,ared_z,0,0,vxl,vxm)
			       bb=sl_psizm(arex,arex_z,1,ared_z(1),8,-1,-1,-1)
			  bb=sl_fsum(ared,1,ared_z,arex)
			       bb=sl_psizm(arey,arey_z,1,ared_z(2),8,-1,-1,-1)
			  bb=sl_fsum(ared,0,ared_z,arey) & endif
;**
		    if f_fg(19) then $
		    if kk le 0  then begin
			if baseview gt 0 then i=-baseview else i=8

			if (kk eq 0) and (tv_flg(1) ne 0) and (i eq 8) then begin
			   bb=sl_glory(-2)
			   bb=sl_vecfun(-2,0)
;			   i =i+3
			endif
			bb=sl_tvlux(w,bwx+bxa,bwy+bya,tite,0,0,0,op4,op5,0,0,0,$
				    tv_win(36,windn),tv_win(37,windn) ,i)
 			bb=sl_tvget(28,i)     & bb=sl_tvget(29,j)
			if i gt 0 then tv_win(28,windn)=i
			if j gt 0 then tv_win(29,windn)=j
		    endif else $
		       if  f_3 or (spm eq 6) or (spt eq -3) or (spt eq -6) $
			 or (f_4 and (f_fg(16) eq 12)) $
			 or (tv_win(69,windn)  eq 1)   $
			 or (f_5 and (f_fg(14) eq 2 )) then bb=sl_tvclear(dummy)
;**
		    if f_fg(19) then begin
		       j=2*tv_mps
		       if f_fg(14) eq 2 then i=2 else i=5
                       if (spm eq  6) then $
			bb=sl_rotfun(0,w,i,f_fg(20),ared,vsx,vsy,1,tip,az,ax,1,$
				     plx+(bwx-plx)/j,bwx-1,ply+(bwy-ply)/j,bwy-1)
                       if (spt eq -3) then $
			bb=sl_rotfun(0,w,i,f_fg(20),ared(0:vsx-1,0:vsy-1),vsx,vsy,1,tip,$
			     az,ax,1,plx+(bwx-plx)/j,bwx-1,ply+(bwy-ply)/j,bwy-1)
		    endif
;**
		    bb=sl_tvscreen(0,plx-1 , 0,ply-1)
		    bb=sl_tvxyz   (0,vsx-1  ,0,vsy-1)
;**
		    if	   f_0  then begin
		        if not   f_2 then begin
			    if   f_7 then begin
			     bb=sl_psizm(ared,ared_z,2,vsx,vsy,tip,-1,-1)
			     bb=sl_scalf(erey,xsiz,vxl,vxm,km,0,ared,tv_flg(2))
;			     ared(0,0)=sl_scale(erey,vsx,vsy,tip,vxl,vxm)
			    endif else $
			     bb=sl_scalf(ared,ared_z,vxl,vxm,km,2,dummy,tv_flg(2))
;			     ared=sl_scale(ared,ared_z(1),ared_z(2),tip,vxl,vxm)
;			    bb  =sl_psiz(ared_z,2,vsx,vsy,2,-1,-1)
			    tip =2 & f_7=0
			endif
		        if f_1 eq 0  then begin
			    if   f_7 then $
			      ared=sl_redim(erey,vsx,vsy,tip,plx,ply,0) else $
			      ared=sl_redim(ared,ared_z(1),ared_z(2),tip,plx,ply,0)
			    bb  =sl_psiz(ared_z,2,plx,ply,tip,-1,-1)
		    	    if (o gt 2) and ((f_fg(12) eq 1) or (spt eq -2) $
						       or (spt eq -3)) then $
					bb=sl_lis(ared,plx,ply,tip,o,1)
			    f_7=0 & endif
			if f_1 eq 2 then bb=sl_tvpix(fix(fx),fix(fy))
			if (f_fg(28)  gt 0) and  (tv_mps ge 7) then begin
;				if fx gt 1 then i=fix(fx) else i=1
;				if fy gt 1 then j=fix(fy) else j=1
;				if f_fg(28) eq 2 then begin i=-i & j=-j & endif
;				bb=sl_tvmov([0,0,bwx,bwy,i,j,w])
				endif
			if f_fg(19) then $
			     if f_7 then bb=sl_tvimag(erey,vsis  ,0,0) $
				    else bb=sl_tvimag(ared,ared_z,0,0)
			if f_1 eq 2 then begin
					 bb=sl_tvpix(1,1)
					 bb=sl_dd(2,ared,ared_z) & endif
			endif else $
		    if	   f_3	then begin
			   if f_fg(19) then begin
				if vsy eq 1  then  begin
				   if tv_win(69,windn) eq 0 then i=30 else i=0
				   bb=sl_tvscreen(0,plx-1 ,i,ply-i/3)
				   bb=sl_tvxyz(0,vsx-1 ,vxl,vxm)
				   bb=sl_tvset(8 ,0,0,1,1,0,0,0)
				   bb=sl_tvfill(0,arei,vsx-1,vxl,ared,vxl,tv_nc/2,3,0)
				   bb=sl_tvplt (-1,vsx,ared,0)
				   bb=sl_tvset(1,tv_nc/3)
;				   if tip gt 4 then begin
;				    for  i=long(1) ,vsx-1 do  $
;					   ared(i)=ared(i)+ared(i-1)
;				    bb=sl_tvxyz(0 ,vsx-1,ared(0),ared(vsx-1))
;				    bb=sl_tvplt(-1,vsx  ,ared,0) &  endif
				   if tv_win(69,windn) eq 0 then begin
				    bb=sl_tvset(14, 3)
				    bb=sl_tvaxis  (vxl  ,vxm   ,2,' ',1.)
;				    bb=sl_tvset(13, 1)
;				    bb=sl_tvaxis  (stc+1,fic+1 ,4,' ',1.,'')
				    pp(0)=0   &  pp(1)=plx-1
				    hh(0)=30  &  hh(1)=hh(0)
				    bb   =sl_tvline (pp,hh,  2,0,tv_nc/3)
				    bb=sl_gf (fic,0,0,fmt)
				    bb=sl_tvs(5,15,sl_stbr(sl_str(stc+1,fmt),1),$
				              1.,0,tv_nc/3)
				    bb=sl_tvs(plx-15,15,sl_stbr(sl_str(fic+1,fmt),1),$
					      1.,0,tv_nc/3)
				   endif
				endif
				if vsy gt 1  then    begin
				 bb= sl_psizm(tare,tare_z,1,vsx,tip,-1,-1,-1)
				 for i=long(0),vsy-1  do begin
				   if tv_od eq 0 then $
				   bb=sl_tvscreen(0,plx-1,i*fy,(i+1)*fy-1) $
				      else $
				   bb=sl_tvscreen(0,plx-1,ply-fy*(i+1),ply-fy*i-1)
				   bb=sl_tvxyz(0,vsx-1 ,vxl,vxm)
				   tare(0)=  ared(*,i)
				   bb=sl_tvfill(0,arei,vsx-1,vxl,tare,vxl,tv_nc/2,3,0)
				   bb=sl_tvplt (-1,vsx,tare,0)
				   endfor
				bb=sl_dd(2,tare,tare_z)
				endif
			   endif
			   endif else $
		    if	   f_4  then begin
		      if   f_fg(16) eq 12 then begin
;		           if tv_od  eq 1 then bb=sl_tvscreen(0,plx-1,ply-1,0) ;!!??
			   if f_fg(19)    then $
				bb=sl_surf(0,ared,vsx,vsy,1,tip,plx,ply,vxl,$
				  vxm,-1,90,f_fg(15),f_fg(16),f_fg(20),f_fg(12))
		      endif else begin
				bb=sl_surf(0,ared,vsx,vsy,1,tip,plx,ply,vxl,$
				  vxm,az,ax,f_fg(15),f_fg(16),f_fg(20),f_fg(12))
				bb=sl_tvset(7,1)
				bb=sl_psiz(ared_z,2,plx,ply,4,-1,-1)
				if f_fg(19)  then $
				bb=sl_tvimag(ared,ared_z,0,0)
		      endelse
		     endif else $
		    if	   f_5  then begin
		     if    f_fg(21) ne 1  then begin
		      if   f_fg(14) eq 2  then begin
;		        if tv_od  eq 1 then bb=sl_tvscreen(plx-1,0,0,ply-1) ;!!??
			if f_6	then begin
			   ared=sl_redim(ared,vsx,vsy,tip,bcx,bcy,0)
			   bb  =sl_psiz(ared_z,-1,bcx,bcy,-1,-1,-1) & endif
			if f_fg(19)  then $
			bb=sl_surf(0,ared,bcx,bcy,1,tip,plx,ply,vxl,vxm,$
				   az,ax,f_fg(15),f_fg(14),f_fg(20),1)
		      endif else begin
			bb=sl_surf(0,ared,vsx,vsy,vsz,tip,plx,ply,vxl,vxm,$
				   az,ax,f_fg(15),f_fg(14),f_fg(20),1)
		        bb=sl_tvset(7,1)
			bb=sl_psiz(ared_z,2,plx,ply,4,-1,-1)
			if f_fg(19)  then bb=sl_tvimag(ared,ared_z,0,0)
		      endelse
		     endif else $
			bb=sl_rotfun(0,w,f_fg(14),f_fg(20),ared,vsx,vsy,vsz,tip,$
						  az,ax,1,0,bwx,0,bwy)
		     endif else $
		    if	spt  eq -6  then begin
			if f_fg(19) then bb=sl_molout(kf,0,0)
		    endif
;**Special
		    provw2,w_co
;**
return
end
;
;
pro provw2, w_co
;** ******
;**
common my_space,si,sj,sx,sy,sz,px1,px2,py1,py2,fdx,fdy,fdz,dx,dy,vssz,res,stt
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_ovs,	ov_sum1,ov_sum2,ov_sum3,ov_sum4,ov_sum5,ov_sum6,ov_sum7,$
		ovs1_z ,ovs2_z ,ovs3_z ,ovs4_z ,ovs5_z ,ovs6_z ,$
		ov_pmx,ov_f,ov_z,ov_l,ov_m
;**
common my_views,  abt,az,ax,bbx,bby,bcx,bcy,bti,btj,btx,bty,btw,bwx,bwy,c1,cc ,$
		  cf,cm,ck1,dif,dif3,fc,fcg,fic,fil,fmt,four,fxy,fx,fy,f_0,f_1,$
		  f_2,f_3,f_4,f_5,f_6,f_7,hh,ii2,ii3,ii6,k1,k2,kk,lc,lk2,mn,mx
common my_views2, mx1,mx2,nx,ny,nz,o,op4,op5,plx,ply,pp,rot,spc,spm,spt,stc   ,$
		  stf,stl,spm_t,spt_t,tip,tite,titx,vsis,vsx,vsy,vsz,w,xsiz ,$
		  xdm,ydm,zdm,vxl,vxm,km,bxa,bya,v_vx4,v_vy4
;** Specials
		    if (spt eq -2) and (f_fg(19)) then begin
			   if tv_od eq 1 then bb=sl_tvxyz(0,plx-1,ply-1,0) else $
					      bb=sl_tvxyz(0,plx-1,0,ply-1)
			   i  =px1*(sx-1)    & if i   eq 0 then i =px2*(sz-1)
			   j  =     sy-1
			   i  =fix(fx*(i +1))     & j =fix(fy*(j+1))
			   ck1=fix(fx*px2*(sz-1)) & if px1 eq 0 then ck1=0
			   k2 =fix(fy*py2*(sz-1))
			   k  =fix(fx*dx)         & l =fix(fy*dy)
			   ck1=ck1  +k	     & lk2=k2   +l
			   bb=sl_tvset(18,2)
			   bb=sl_tvset(1,tv_nc/1.1)
			   bb=sl_tvmod(0,2)
			   bb=sl_tvplt(-1,5,[ck1,ck1+i,ck1+i,ck1,ck1], $
					  5,[lk2,lk2,lk2+j,lk2+j,lk2])
			   bb=sl_tvplt(-1,5,[k  ,k+i,k+i  ,k    ,k  ], $
					  5,[l  ,l  ,l+j  ,l+j  ,l  ])
			   bb=sl_tvplt(-1,2,[k  ,ck1  ],2,[l  ,lk2  ])
			   bb=sl_tvplt(-1,2,[k  ,ck1  ],2,[l+j,lk2+j])
			   bb=sl_tvplt(-1,2,[k+i,ck1+i],2,[l  ,lk2  ])
			   bb=sl_tvplt(-1,2,[k+i,ck1+i],2,[l+j,lk2+j])
;
			   if k2 lt 0 then k2=-k2
			   bb=sl_tvplt(-1,2,[k  ,ck1  ],2,[ply-1,ply-1-k2 ])
			   bb=sl_tvplt(-1,2,[k+i,ck1+i],2,[ply-1,ply-1-k2 ])
;
			   k=fix (fx*(si-sz))
;
			   bb=sl_tvplt(-1,2,[plx-1,k  ],2,[l  ,lk2   ])
			   bb=sl_tvplt(-1,2,[plx-1,k  ],2,[l+j,lk2+j ])
;
			   l=fix (fy*(sj-sz))
			   bb=sl_tvset(18,0)
			   bb=sl_tvset(1,w_co)
;;;;			   bb=sl_tvplt(-1,3,[0,k,k    ],3,[l,l,0])
;;;;			   bb=sl_tvplt(-1,2,[k,plx-1  ],2,[l,ply-1])
			   bb=sl_tvmod(0,3)
			   endif
;**
		    if (spt eq -3) and (f_fg(19)) then begin
			   if tv_od eq 1 then bb=sl_tvxyz(0,plx-1,ply-1,0) else $
					      bb=sl_tvxyz(0,plx-1,0,ply-1)
			   k1 =fix(fx*vsx)-1 & k2 =fix(fy*vsy)-1
			   ck1=fix(fx*vsz)-1 & lk2=fix(fy*vsz)-1
			   bb=sl_tvplt(-1,5,[0,0,k1,k1,0]    ,5,[k2,0,0,k2,k2])
			   bb=sl_tvplt(-1,2,[k1+1 ,plx-1]    ,2,[0    ,lk2   ])
			   bb=sl_tvplt(-1,2,[k1   ,plx-1]    ,2,[k2   ,ply-1 ])
			   bb=sl_tvplt(-1,2,[0    ,ck1  ]    ,2,[k2+1 ,ply-1 ])
			   bb=sl_tvplt(-1,3,[plx-1,plx-1,ck1],3,[lk2,ply-1,ply-1])
;			   bb=sl_tvset(18,2)
;			   bb=sl_tvplt(-1,3,[0,ck1,ck1  ]    ,3,[0,lk2,ply-1])
;			   bb=sl_tvplt(-1,2,[ck1 ,plx -1]    ,2,[lk2  ,lk2  ])
;**			SumX...6.
			   ck1=ply+(bwy-ply)/6
			   bb =sl_tvscreen(0,k1-1 ,ck1,bwy-1)
			   mx =sl_maxim(ov_sum6,ovs6_z,k,mn)
			   bb=sl_tvxyz(0,vsx-1,mn,mx)
		    	   bb=sl_tvset(18,0)
			   bb=sl_tvplt(-1,2,[vsx,vsx],2,[mn,mx/6])
			   bb=sl_tvset(1,tv_nc/2)
			   bb=sl_tvset(9 ,10)
			   bb=sl_tvplt(-1,vsx,ov_sum6,0)
;**			SumF...4.
			   bb=sl_tvscreen(k1,plx-1,ck1,bwy-1)
			   mx=sl_maxim(ov_sum4,ovs4_z,k,mn)
			   bb=sl_tvxyz(0,vsz-1,mn,mx)
		    	   bb=sl_tvset(18,1)
			   bb=sl_tvset(1,tv_nc/1.5)
			   bb=sl_tvplt(-1,vsz,ov_sum4,0)
			   bb=sl_tvset(9,0)
			   bb=sl_tvset(1,w_co)
			   bb=sl_tvplt(-1,2,[vsz-1,vsz-1],2,[mn,mx/6])
;**			SumY...5.
			   bb  =sl_psizm(arei,arei_z,1,vsy,4,-1,-1,-1)
			   arei=sl_index(vsy,4)
			   k1  =plx+(bwx-plx)/6
			   if tv_od eq 1 then  begin
					 bb=sl_revs	(arei,vsy,0,4,0)
			   		 bb=sl_tvscreen (k1,bwx-1,lk2+1,ply-1)
			   endif  else   bb=sl_tvscreen (k1,bwx-1,0,k2)
			   mx=sl_maxim(ov_sum5,ovs5_z,k,mn)
			   bb=sl_tvxyz(mn,mx,0,vsy-1)
		    	   bb=sl_tvset(18,0)
			   bb=sl_tvset(1,tv_nc/1.1)
			   bb=sl_tvplt(-1,vsy,ov_sum5,vsy,arei)
			   bb=sl_tvset(1,w_co)
			   bb=sl_tvplt(-1,2,[mn,mx/6],2,[arei(vsy-1),arei(vsy-1)])
			   bb=sl_tvplt(-1,2,[mn,mx/6],2,[arei  (0)  ,arei  (0)  ])
;
			   bb=sl_tvscreen(0,bwx-1,0,bwy-1)
			   bb=sl_tvxyz(0,bwx-1,0,bwy-1)
			   bb=sl_tvplt(-1,3,[0,k1,k1],3,[ck1,ck1,0])
			   bb=sl_dd(2,arei,arei_z)
			   endif
;**
		    if (spm eq 6)   then begin
			if f_fg(19) then begin
			   bb	  =sl_psizm(arei,arei_z,1,vsy,4,-1,-1,-1)
			   arei	  =sl_index(vsy,4)
			   mx1	  =sl_maxim(arey,arey_z,k,mn)
			   mx2	  =sl_maxim(arex,arex_z,k,mx)
			   if mx   lt mn  then mn=mx
			   if mx1  gt mx2 then mx=mx2 else mx=mx1
;**		    proj.x.....
			   bb=sl_tvscreen(0,plx-1 , ply,bwy-1)
			   if (f_5) and (az gt 90)  and (az lt 270) then $
				bb=sl_revs(arex,vsx,0,arex_z(arex_z(0)+1),0)
			   bb=sl_tvset(18,0)
			   bb=sl_tvset(21,0)
			   bb=sl_tvxyz(0,vsx-1,mn,mx)
			   bb=sl_tvplt(-1,2,[0,vsx-1],2,[mn,mn])
			   bb=sl_tvset(1,tv_nc/2)
			   bb=sl_tvset(9,10)
			   bb=sl_tvplt (-1,vsx,arex,0)
			   if mx lt mx2 then begin bb=sl_tvset(18,2)
					bb=sl_tvset(1,tv_nc/1.1)
					bb=sl_tvxyz(0 ,vsx-1,mx,mx2)
					bb=sl_tvplt(-1,vsx,arex,0)
					bb=sl_tvset(18,0) & endif
;**		    proj.y.....
			   bb=sl_tvscreen(plx,bwx-1,0,ply-1)
			   if f_5 then begin
			      if (ax gt 90)   and  (ax lt 270) then i=1 else i=0
			      if (az gt 90)   and  (az lt 270) then j=1 else j=0
			      if (tv_od eq 1) then j=sl_tog(j)
			      if (i-j   ne 0) then bb=sl_revs(arei,vsy,0,4,0)
			   endif  else  if tv_od eq 1 then $
						   bb=sl_revs(arei,vsy,0,4,0)
			   bb=sl_tvset(1,tv_nc/3)
			   bb=sl_tvset(9,0)
			   bb=sl_tvxyz(mn,mx,0,vsy-1)
			   bb=sl_tvplt(-1,vsy,arey,vsy,arei)
			   if mx lt mx1 then begin bb=sl_tvset(18,2)
					bb=sl_tvset(1,tv_nc/1.1)
					bb=sl_tvxyz(mx,mx1,0 ,vsy-1)
					bb=sl_tvplt(-1,vsy,arey,vsy,arei)
					bb=sl_tvset(18,0) & endif
			   bb=sl_dd(2,arei,arei_z)
			   bb=sl_tvset(1,w_co)
			   bb=sl_tvplt(-1,2,[mn,mn],2,[0,vsy-1])
;**
			   bb=sl_tvscreen(0,bwx-1,0,bwy-1)
			   bb=sl_tvxyz   (0,bwx-1,0,bwy-1)
;			   bb=sl_tvplt   (-1,2,[plx,bwx-1],2,[ply,bwy-1])
			   if  cf ge 0  then  begin
			      bb=sl_gf (mx2,1,0,fmt)
			      bb=sl_tvs(plx,bwy-15,sl_str(mx2,fmt),2.,0,tv_nc/2)
			      bb=sl_gf (mx1,1,0,fmt)
			      bb=sl_tvs(bwx-15,ply+(bwy-ply)/3,sl_str(mx1,fmt),$
					 2.,-90,tv_nc/3)
			   endif
			endif
			bb=sl_dd(2,arex,arex_z)
			bb=sl_dd(2,arey,arey_z)
		    endif
return
end
;
;
;
;
pro sl_region,	c,l, a,b,d,e, ifu,jfu, vsiz
;** *********
	 a = c - ifu/2 & d = a+ifu & if   a lt vsiz(7)  then a=vsiz(7)
	 b = l - jfu/2 & e = b+jfu & if   b lt vsiz(8)  then b=vsiz(8)
	 d = d - a -1		   & if a+d gt vsiz(13) then d=vsiz(13)-a
	 e = e - b -1		   & if b+e gt vsiz(14) then e=vsiz(14)-b
return
end
;
;
function sl_slice, erey,vsiz,c,l,f1,f2,bxy6,bxy7,flg,f_sa
;******* ********
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common  my_fun,	a,b,d,e,bfx,bfy,c1,c2,c3,c4,cj,ez,fmf,ifu,jfu,l1,l2,vp,vh,$
		rbx,rby,mn,mx,mni,mxi,h,p,rvmm,rvmi,sp,int7,fmi4,fmf9,fsmo,$
		st1,st2,st3,st4,st5,st6,st7,st8,st9,st10,st11,st12,st13,$
		st14,st15,st16,st17,st18,f24,tap,tip,mnj,mxj,c5,c6,c7,c8,c9,c10
;**
	   if   vsiz(0)  eq  3 then ez=vsiz(3) else ez =1
	   tip= vsiz(vsiz(0)+1)
;
	   d=c-bxy6    & if d lt 0 then a=-d+1 else a=d+1
	   e=l-bxy7    & if e lt 0 then b=-e+1 else b=e+1
	   c2= 0.      & c3=0
	   if  a ge b  then begin
		bb=sl_psizm(arei,arei_z,2,a,2,4,-1,-1)
		if d lt 0   then i=-1 else i =1
		if d eq 0   then c1=0 else c1=float(e) /d *i
		for k=bxy6  ,c ,i do  begin
				  arei(c3,0)=k
				  arei(c3,1)=bxy7  +sl_pfix(c2)
				  c3 = c3+1 & c2 =  c2+c1   & endfor
	   endif else begin
		bb=sl_psizm(arei,arei_z,2,b,2,4,-1,-1)
		if e lt 0   then i=-1 else i =1
		c1=float(d) /e *i
		for k=bxy7  ,l ,i do  begin
				  arei(c3,1)=k
				  arei(c3,0)=bxy6  +sl_pfix(c2)
				  c3 = c3+1 & c2 =  c2+c1   & endfor
	   endelse
	   bb  =sl_psizm(ares,ares_z,2,arei_z(1),f2-f1+1,tip,-1,-1)
	   if (not flg)  then begin
	    for f=f1,f2   do  begin
	     l1=f-f1
	     if ez gt 1   then $
		  for k=0,ares_z(1)-1 do ares(k,l1)=erey(arei(k,0),arei(k,1),f)$
	     else for k=0,ares_z(1)-1 do ares(k,l1)=erey(arei(k,0),arei(k,1))
	    endfor
	   endif else begin
	    if a ge b then begin
	     for f=f1,f2   do  begin
		 l1=f-f1
		 for k=0,ares_z(1)-1 do begin
			 c3=3 & i=arei(k,0)  & j=arei(k,1)
			 if ez gt 1 then c1=0.+erey(i,j,f) else c1=0.+erey(i,j)
			 j =j+1 & if j ge vsiz(14) then c3=c3-1 else $
			 if ez gt 1 then c1=c1+erey(i,j,f) else c1=c1+erey(i,j)
			 j =j-2 & if j lt vsiz( 8) then c3=c3-1 else $
			 if ez gt 1 then c1=c1+erey(i,j,f) else c1=c1+erey(i,j)
			 if (tip lt 8) or (tip eq 16) then $
				 ares(k,l1)=sl_pfix(float(c1)/c3) $
			 else    ares(k,l1)=c1/c3
		 endfor
	     endfor
	    endif     else begin
	     for f=f1,f2   do  begin
		 l1=f-f1
		 for k=0,ares_z(1)-1 do begin
			 c3=3 & i=arei(k,0)  & j=arei(k,1)
			 if ez gt 1 then c1=0.+erey(i,j,f) else c1=0.+erey(i,j)
			 i =i+1 & if i ge vsiz(13) then c3=c3-1 else $
			 if ez gt 1 then c1=c1+erey(i,j,f) else c1=c1+erey(i,j)
			 i =i-2 & if i lt vsiz( 7) then c3=c3-1 else $
			 if ez gt 1 then c1=c1+erey(i,j,f) else c1=c1+erey(i,j)
			 if (tip lt 8) or (tip eq 16) then $
				 ares(k,l1)=sl_pfix(float(c1)/c3) $
			 else    ares(k,l1)=c1/c3
		 endfor
	     endfor
	    endelse
	   endelse
	   c1  = float(a*a + b*b)
	   bb  = sl_sqrt (c1,1)
	   f_sa=-sl_atang(e,d)
	   i   = sl_pfix (c1)
return,	   i-1
end
;
;
;
function sl_ellipos,	erey,vsiz,cx,cy,cf,f_fg,f_el,rad,ccx,ccy
;******* **********
;**
	common my_elpos,a,b,d,e,ife,jfe,kfe,il,ki,kj,tg,typ,v1,v2,v3,v4,v5
;**
	common my_area ,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
bb=1
typ=vsiz(vsiz(0)+1)
;**Get region
;**--- ------
   ife=f_fg(1)
   jfe=f_fg(2)
   sl_region,	cx,cy, a,b,d,e, ife,jfe, vsiz
   bb =sl_psizm(vare,vare_z,2,d+1,e+1,typ,-1,-1)
   if vsiz(0) lt 3  then vare(0,0)=erey(a:a+d,b:b+e) $
		    else vare(0,0)=erey(a:a+d,b:b+e,cf)
;**Loop twice to find center and dx/dy
;**---- ----- -- ---- ------ --- -----
   for l=1,2 do begin
;
;1)**Find center
;  **---- ------
     bb=sl_psizm(arex,arex_z,1,vare_z(1),8,-1,-1,-1)
     bb=sl_psizm(arey,arey_z,1,vare_z(1),8,-1,-1,-1)
     bb=sl_fsum(vare,1,vare_z,arex)
     for k=1,2 do begin
;**	Tangent on projection then flat background.
;**	------------------------------------------
	cx=cy
;	tg=0.
;	for i=1,arex_z(1)-1 do tg=tg + arex(i) - arex(i-1)
;	tg=tg/ (arex_z(1)-1)
	tg=(arex(arex_z(1)-1)-arex(0))/(arex_z(1)-1)
	for i=1,arex_z(1)-1 do arex(i)=arex(i) - i*tg
	arex(0)=arex(*) - sl_minf(arex,arex_z,il)
;
;**	Cross distribution then find center.
;**	-----------------------------------
	arey (arey_z(1)-1) = arex(arex_z(1)-1)
	for i=arey_z(1)-2,0,-1 do arey(i)=arex(i) + arey(i+1)
	for i=1,arex_z(1)-1    do arex(i)=arex(i) + arex(i-1)
	cy=0
	il=arex_z(1)-1
	while cy lt il do if arex(cy) lt arey(il) then cy=cy+1 else il=il-1
	v1=arex(cy)-arey(cy) & v2=v1 & v3=v1
	if cy gt 0 	     then v2=arex(cy-1) - arey(cy-1)
	if cy lt arex_z(1)-1 then v3=arex(cy+1) - arey(cy+1)
	if v1 lt 0  then v1=-v1 & if v2 lt 0 then v2=-v2 & if v3 lt 0 then v3=-v3
	if v1 gt v2 then cy=cy-1
	if v1 gt v3 then cy=cy+1
;
;**	Change projection.
;**	-----------------
	bb=sl_psizm(arex,arex_z,1,vare_z(2),8,-1,-1,-1)
	bb=sl_psizm(arey,arey_z,1,vare_z(2),8,-1,-1,-1)
	bb=sl_fsum(vare,0,vare_z,arex)
     endfor
;
;**Replace region
;**------- ------
     cx=cx+a  & cy=cy+b
     sl_region,	cx,cy, a,b,d,e, ife,jfe, vsiz
     bb =sl_psizm(vare,vare_z,2,d+1,e+1,typ,-1,-1)
     if vsiz(0) lt 3  then vare(0,0)=erey(a:a+d,b:b+e) $
		      else vare(0,0)=erey(a:a+d,b:b+e,cf)
;
;2)**Approximate dx/dy
;  **----------- -----
;**	Flat background again
;**	---- ---------- -----
	v1=0. & v2=0.
	for j=0,vare_z(2)-1 do begin	v1=v1+vare(0,j)
					v2=v2+vare(vare_z(1)-1,j) & endfor
	tg= (v2-v1) / ((vare_z(1)-1)*vare_z(2))
	for j=0,vare_z(2)-1 do $
	 for i=1,vare_z(1)-1 do vare(i,j)=vare(i,j) - i*tg
;
	v1=0. & v2=0.
	for i=0,vare_z(1)-1 do begin	v1=v1+vare(i,0)
					v2=v2+vare(i,vare_z(2)-1) & endfor
	tg= (v2-v1) / ((vare_z(2)-1)*vare_z(1))
	for i=0,vare_z(1)-1 do $
	 for j=1,vare_z(2)-1 do vare(i,j)=vare(i,j) - j*tg
;**	Contour line
;**	------- ----
	if f_fg(16) ne 12 then begin
	   il=1
	   bb=sl_surf(0,vare,vare_z(1),vare_z(2),1,vare_z(3),vare_z(1),vare_z(2),$
				    0,0,-1,90, il , 11 ,0,0)
	   vare_z(3)=4
	endif
;**	Projections of the lines
;**	----------- -- --- -----
	v2=0 & v1=0
	for j=0,vare_z(2)-1  do begin
	 v3=0
	 for i=0,vare_z(1)-1 do if vare(i,j) gt 1 then v3=1
	 v2=v2+v3
	endfor
	for i=0,vare_z(1)-1  do begin
	 v3=0
	 for j=0,vare_z(2)-1 do if vare(i,j) gt 1 then v3=1
	 v1=v1+v3
	endfor
;**	Adjust dx/dy
;**	------ -----
	il=il+1
	j =d+1
	i =j-v1
	if (i gt j/il/2) then if (i ge j/il) then ife=ife - (i-j/il) $
					     else ife=ife - (i-j/il/2) else $
	if (i lt j/il/2) then if (i lt 4   ) then ife=ife + (j/il/2-i)
	j =e+1
	i =j-v2
	if (i gt j/il/2) then if (i ge j/il) then jfe=jfe - (i-j/il) $
					     else jfe=jfe - (i-j/il/2) else $
	if (i lt j/il/2) then if (i lt 4   ) then jfe=jfe + (j/il/2-i)
;
	if ife lt 4 then ife=4
	if jfe lt 4 then jfe=4
;
     sl_region,	cx,cy, a,b,d,e, ife,jfe, vsiz
     bb =sl_psizm(vare,vare_z,2,d+1,e+1,typ,-1,-1)
     if vsiz(0) lt 3  then vare(0,0)=erey(a:a+d,b:b+e) $
		      else vare(0,0)=erey(a:a+d,b:b+e,cf)
   endfor
;
;3)**Find angle
;  **---- -----
   if rad eq 0  then begin
;**	Flat background
;**	---- ----------
	v1=0. & v2=0.
	for j=0,vare_z(2)-1 do begin	v1=v1+vare(0,j)
					v2=v2+vare(vare_z(1)-1,j) & endfor
	tg= (v2-v1) / ((vare_z(1)-1)*vare_z(2))
	for j=0,vare_z(2)-1 do $
	 for i=1,vare_z(1)-1 do vare(i,j)=vare(i,j) - i*tg
;
	v1=0. & v2=0.
	for i=0,vare_z(1)-1 do begin	v1=v1+vare(i,0)
					v2=v2+vare(i,vare_z(2)-1) & endfor
	tg= (v2-v1) / ((vare_z(2)-1)*vare_z(1))
	for i=0,vare_z(1)-1 do $
	 for j=1,vare_z(2)-1 do vare(i,j)=vare(i,j) - j*tg
;**	Contour filled & low pass
;**	------- ------   --- ----
	sl_ellip,1  ,vare,vare_z,0 ,0 ,cx-a,cy-b,d,e, v1,v2,v3
	bb=sl_surf(0,vare,vare_z(1),vare_z(2),1,vare_z(3),vare_z(1),vare_z(2),$
				  0,0,-1,90, 2 , 10 ,0,0)
	vare_z(3)=4
;**	Slices
;**	------
	v2=sl_totf (vare,vare_z(1),vare_z(2),typ) / (vare_z(1)*vare_z(2))
	bb=sl_psizm(arex,arex_z,1, d+1 + 2*(e-cy+b)-1 ,8,-1,-1,-1)
	bb=sl_psizm(arey,arey_z,1, d+1 + 2*(e-cy+b)-1 ,8,-1,-1,-1)
	vare_z(7)=0 & vare_z(13)=vare_z(1)-1
	vare_z(8)=0 & vare_z(14)=vare_z(2)-1
	k=0 & i=d & l=cy-b
	for j=cy-b,e-1    do begin
	   il=sl_slice(vare,vare_z, i,l, 0,0, 0,j, 1,v1)
	   if l gt 0 then l=l-1 else i=i-1
	   il=0 & for n= 0,ares_z(1)-1 do if ares(n,0) lt v2 then il=il+1
	   if il eq 0 then il=1
	   arex(k)=sl_totf(ares,ares_z(1),1,ares_z(ares_z(0)+1)) /il
	   arey(k)=v1
	   k =k+1 & endfor
	for j= 0  ,d	  do begin
	   il=sl_slice(vare,vare_z, i,l, 0,0, j,e, 1,v1)
	   if l gt 0 then l=l-1  $
		     else if i gt 0 then i=i-1 $
				    else l=l+1
	   il=0 & for n= 0,ares_z(1)-1 do if ares(n,0) lt v2 then il=il+1
	   if il eq 0 then il=1
	   arex(k)=sl_totf(ares,ares_z(1),1,ares_z(ares_z(0)+1)) /il
	   arey(k)=v1
	   k =k+1 & endfor
	for j=e-1,cy-b+1,-1 do begin
	   il=sl_slice(vare,vare_z, i,l, 0,0, d,j, 1,v1)
	   if i gt 0 then i=i-1 else l=l+1
	   il=0 & for n= 0,ares_z(1)-1 do if ares(n,0) lt v2 then il=il+1
	   if il eq 0 then il=1
	   arex(k)=sl_totf(ares,ares_z(1),1,ares_z(ares_z(0)+1)) /il
	   arey(k)=v1
	   k =k+1 & endfor
;**	Choice angle
;**	------ -----
	for i=0,arex_z(1)-2 do begin
		k=i
		for j=i+1,arex_z(1)-1  do if arex(j) gt arex(k) then k=j
		if  i ne k then begin
		    v1=arex(k) & arex(k)=arex(i) & arex(i)=v1
		    v1=arey(k) & arey(k)=arey(i) & arey(i)=v1 & endif
	endfor
	v1=0.  &  v2=0. & v3=0. & v4=0.
	j =arey_z(1)/2
	if j*2 lt arey_z(1) then k=1 else k=0
	for i=0,j+k-1	    do begin	v1=v1+sl_cos(arey(i))
					v3=v3+arex(i) & endfor
	for i=j,arey_z(1)-1 do begin	v2=v2+sl_cos(arey(i))
					v4=v4+arex(i) & endfor
	v1=v1/(j+k)
	v2=v2/(arey_z(1)-j)
	v4=v3  /v4
	v1=(v1-v2)/2.
	v2=sl_acos(v1)*180./3.1416
	v5=   arey(0) *180./3.1416
	if v1 gt 0 then if (v5 lt 45.) or (v5 gt 135.) then v2=45. -(v2  -45.)
	if v1 lt 0 then if (v5 lt 45.) or (v5 gt 135.) then v2=135.+(135.-v2 )
   endif else begin
	v1=cx-ccx-1
	v4=ccy-1-cy
	if (v4 eq 0) then v2=0.  else $
	if (v1 eq 0) then v2=90. else begin
		v3=v1 *v1+ v4*v4
		bb=sl_sqrt(v3,1)
		v2=sl_acos(v1/v3)*180./3.1416
		if v4 lt 0 then v2=360.-v2
   endelse  &   endelse
   bb=sl_dd(2,arex,arex_z)
   bb=sl_dd(2,arey,arey_z)
   bb=sl_dd(2,vare,vare_z)
   if v2  gt 180. then v2=v2-180.
   if v2  gt  90. then v2=v2-90.
   if v2  gt  45. then v2=v2-90.
   f_el=v2
;**
;4)**Find dx/dy  v1=npt v3=bgrd v4=sigma v5=sum
;  **---- -----
;  DX
   ki =ife/2 & if ki lt 3 then ki=3
   kfe=ife
	bb=sl_psizm(arex,arex_z,1, kfe+1 ,8,-1,-1,-1)
	bb=sl_psizm(arey,arey_z,1, kfe+1 ,8,-1,-1,-1)
	for i=0,kfe do begin
		sl_ellip,4,erey,vsiz,cf,f_el,cx,cy,i+ki-1,jfe-1, v1,v3,-1,v4,v5
		arex(i)=v5 - v1*v3
		arey(i)=v5
	endfor
;	Lisse & normalize
		arex(0)=  (arex(0)+arex(1)) /2 & v1=arex(0)  & v3=v1
		arey(0)=  (arey(0)+arey(1)) /2 & v4=arey(0)  & v5=v4
		for i=1,kfe-1 do begin
		  arex(i)=(arex(i)+arex(i-1)+arex(i+1)) /3
		  if arex(i) lt v1 then v1=arex(i)
		  if arex(i) gt v3 then v3=arex(i)
		  arey(i)=(arey(i)+arey(i-1)+arey(i+1)) /3
		  if arey(i) lt v4 then v4=arey(i)
		  if arey(i) gt v5 then v5=arey(i)
		endfor
		arex(kfe)=(arex(kfe)+arex(kfe-1)) /2
		if arex(kfe) lt v1 then v1=arex(kfe)
		if arex(kfe) gt v3 then v3=arex(kfe)
		arey(kfe)=(arey(kfe)+arey(kfe-1)) /2
		if arey(kfe) lt v4 then v4=arey(kfe)
		if arey(kfe) gt v5 then v5=arey(kfe)
;
		arex(0)= (arex -v1)/(v3-v1)
		arey(0)= (arey -v4)/(v5-v4)
;**
;**	Function is max(Cum.dX - Cum.dY)
;**
		v1= arex(0)
		v3= arey(0)
		tg= 0. & il=0
		for i=1,kfe do begin
		    v2=arex(i)
		    v4=arey(i)
                    arex(i)=arex(i-1) + arex(i) * (arex(i)-v1)
                    arey(i)=arey(i-1) + arey(i) * (arey(i)-v3)
		    v1=v2  & v3=v4
		    v5=arex(i)-arey(i)
		    if v5 gt tg then begin tg=v5 & il=i & endif
		endfor

;		v1= arex(0) & arex(0)=0
;		v3= arey(0) & arey(0)=0
;		tg= 0. & il=0
;		for i=1,kfe do begin
;		    v2=arex(i)
;		    v4=arey(i)
;                   arex(i)=arex(i)-v1 +arex(i-1)
;		    arey(i)=arey(i)-v3 +arey(i-1)
;		    v1=v2  & v3=v4
;		    v5=arex(i)-arey(i)
;		    if v5 gt tg then begin tg=v5 & il=i & endif
;		endfor
	ife=ki+il

	kj =jfe/2 & if kj lt 3 then kj=3


;	tt=!window
;	tvwindow,5
;	set_xy
;	plot ,arex(*)
;	oplot,arey(*)
;	oplot, [0,il],[0.,tg]
;	tvselect,tt
;**
   f_fg(1)=ife
   f_fg(2)=jfe
;
return,	bb
end
;
;
;
function sl_bgbox, erey,vsiz,f,f_ab
;******* ********
common tmp_bgbox,  bg,ez,bg_kp,ki,kj
bg   =0.
bg_kp=0
if f_ab(0,0) ge 0 then begin
	if  vsiz(0) eq 3 then ez=vsiz(3) else ez=1
	ki =f_ab(1,0)-f_ab(0,0)
	kj =f_ab(1,1)-f_ab(0,1)
	j  =f_ab(0,1)-1
	if (j ge 0) and (kj gt 0) then begin
		   if ez eq 1 then for i=f_ab(0,0),f_ab(1,0) do bg=bg+erey(i,j) $
			      else for i=f_ab(0,0),f_ab(1,0) do bg=bg+erey(i,j,f)
				   bg_kp=f_ab(1,0)-f_ab(0,0) +1 &  endif
	i  =f_ab(0,0)-1
	if (i ge 0) and (ki gt 0) then begin
		   if ez eq 1 then for j=f_ab(0,1),f_ab(1,1) do bg=bg+erey(i,j) $
			      else for j=f_ab(0,1),f_ab(1,1) do bg=bg+erey(i,j,f)
				   bg_kp=f_ab(1,1)-f_ab(0,1) +1+bg_kp &  endif
	i  =f_ab(1,0)+1
	if (i lt vsiz(1)) and (ki gt 0) then begin
		   if ez eq 1 then for j=f_ab(0,1),f_ab(1,1) do bg=bg+erey(i,j) $
			      else for j=f_ab(0,1),f_ab(1,1) do bg=bg+erey(i,j,f)
				   bg_kp=f_ab(1,1)-f_ab(0,1) +1+bg_kp &  endif
	j  =f_ab(1,1)+1
	if (j lt vsiz(2)) and (kj gt 0) then begin
		   if ez eq 1 then for i=f_ab(0,0),f_ab(1,0) do bg=bg+erey(i,j) $
			      else for i=f_ab(0,0),f_ab(1,0) do bg=bg+erey(i,j,f)
				   bg_kp=f_ab(1,0)-f_ab(0,0) +1+bg_kp  &  endif
;**
	if bg_kp gt 0 then bg=bg/bg_kp
;**
	endif
return,bg
end
;
;
pro provfu,erey,vsiz,c,l,f,bx
;** ******
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common  my_fun,	a,b,d,e,bfx,bfy,c1,c2,c3,c4,cj,ez,fmf,ifu,jfu,l1,l2,vp,vh,$
		rbx,rby,mn,mx,mni,mxi,h,p,rvmm,rvmi,sp,int7,fmi4,fmf9,fsmo,$
		st1,st2,st3,st4,st5,st6,st7,st8,st9,st10,st11,st12,st13,$
		st14,st15,st16,st17,st18,f24,tap,tip,mnj,mxj,c5,c6,c7,c8,c9,c10
;**
	 areu_z(0)=2 & areu_z(2)=1
	 if cj le 3  then begin areu_z(1)=d+1 & areu_z(2)=e+1   &  endif else $
	 if cj eq 4  then begin areu_z(1)=1   & areu_z(2)=e+1   &  endif else $
	 if cj eq 5  then begin areu_z(0)=1   & areu_z(1)=d+1	&  endif
	 areu_z(areu_z(0)+1)=tip
;**
         if (f_fg(8) eq 1) then  if ez  le 1  then f_fg(8)=0
         if (f_fg(8) eq 2) then  if ez  le 1  then f_fg(8)=0
	 if (f_fg(8) eq 0) then  if ez  le 1  then f_fg(9)=0
;
	 if (f_fg(9) eq 1) or  ((cj le 3)  and (f_fg(8) ne 0)) or $
	   ((f_fg(8) eq 1) and  (cj eq 4)) or   $
	   ((f_fg(8) eq 2) and  (cj eq 5)) then areu_z(areu_z(0)+1)=8
;
         if (f_fg(8) eq 1) then   begin
;		b = f - bx/2 & if b   lt vsiz(9)  then b=vsiz(9)
;    		e = bx - 1   & if b+e gt vsiz(15) then e=vsiz(15)-b
		b = f & if b eq vsiz(15) then b=b-1
		e = vsiz(15)-b
		if cj le 4 then  areu_z(2)=e+1
		bb=sl_dd(1,areu, areu_z)
		if f_fg(9)  then begin l1=vsiz(14) & c2=(l1-l+2)/2
   		 if cj le 3 then bb=sl_tsum(erey(a:a+d,l:l1,b:b+e),1,2,areu) else $
		 if cj eq 4 then bb=sl_tsum(erey(c    ,l:l1,b:b+e),1,2,areu) else $
		 if cj eq 5 then if l ne l1  then $
				 bb=sl_tsum(erey(a:a+d,l:l1,f)    ,1,1,areu) else $
				 areu(0)  = erey(a:a+d,l   ,f)
		endif else  begin
		 if cj le 3 then bb=sl_tsum(erey(a:a+d,l,b:b+e)   ,1,2,areu) else $
		 if cj eq 4 then bb=sl_tsum(erey(c    ,l,b:b+e)   ,0,2,areu) else $
		 if cj eq 5 then areu(0)  =       erey(a:a+d,l,f)     &  endelse
	 endif else  $
         if (f_fg(8) eq 2) then  begin   a=b  & d=e
;		b = f - bx/2 & if b   lt vsiz(9)  then b=vsiz(9)
;    		e = bx - 1   & if b+e gt vsiz(15) then e=vsiz(15)-b
		b = f & if b eq vsiz(15) then b=b-1
		e = vsiz(15)-b
		if cj le 3 then areu_z(1)=d+1
		if cj le 3 then areu_z(2)=e+1  else $
		if cj eq 5 then areu_z(1)=e+1  else $
		if cj eq 4 then if f_fg(9) then begin
		                areu_z(0)=1
		                areu_z(1)=d+1
				areu_z(2)=areu_z(3) & endif $
			   else areu_z(2)=d+1
;care cj=5 dimension
		bb=sl_dd(1,areu,areu_z)
		if f_fg(9)    then begin l1=vsiz(13) & c2=(l1-c+2)/2
		 if cj le 3 then bb=sl_tsum(erey(c:l1,a:a+d,b:b+e),0,2,areu) else $
		 if cj eq 4 then bb=sl_tsum(erey(c:l1,a:a+d,f)    ,0,1,areu) else $
		 if cj eq 5 then bb=sl_tsum(erey(c:l1,l,b:b+e)    ,0,1,areu)
		endif  else  begin
		 if cj le 3 then bb=sl_tsum(erey(c   ,a:a+d,b:b+e),0,2,areu) else $
		 if cj eq 4 then areu(0,0)=       erey(c   ,a:a+d,f)	    else $
		 if cj eq 5 then bb=sl_tsum(erey(c,l,b:b+e)	  ,0,1,areu)
 		endelse
	 endif else  $
         if (f_fg(8) eq 0)   then begin
		bb=sl_dd(1,areu,areu_z)
		if f_fg(9)   then begin l1=vsiz(15) & c2=(l1-f+2)/2
		 if  f ne l1 then begin
		  if cj le 3 then bb=sl_tsum(erey(a:a+d,b:b+e,f:l1),2,2,areu) else $
		  if cj eq 4 then bb=sl_tsum(erey(c    ,b:b+e,f:l1),2,2,areu) else $
		  if cj eq 5 then bb=sl_tsum(erey(a:a+d,l    ,f:l1),2,1,areu)
		 endif	else begin
		  if cj le 3 then areu(0,0)= erey(a:a+d,b:b+e,f)   else $
		  if cj eq 4 then areu(0,0)= erey(c    ,b:b+e,f)   else $
		  if cj eq 5 then areu(0)  = erey(a:a+d,l    ,f)
		 endelse
		endif  else   begin
		 if ez eq 1   then begin
		  if cj le 3 then areu(0,0)=    erey(a:a+d,b:b+e)	 else $
		  if cj eq 4 then areu(0,0)=    erey(c	  ,b:b+e)	 else $
		  if cj eq 5 then areu(0)  =    erey(a:a+d,l)
		 endif else  $
		  if cj le 3 then areu(0,0)=    erey(a:a+d,b:b+e,f)	 else $
		  if cj eq 4 then areu(0,0)=    erey(c	  ,b:b+e,f)	 else $
		  if cj eq 5 then areu(0)  =    erey(a:a+d,l    ,f)
		 f_ab(0,0)=a & f_ab(1,0)=a+d
		 f_ab(0,1)=b & f_ab(1,1)=b+e
		 f_ab(0,2)=f & f_ab(1,2)=f
		 if cj eq 4 then begin f_ab(0,0)=c & f_ab(1,0)=c & endif else $
		 if cj eq 5 then begin f_ab(0,1)=l & f_ab(1,1)=l & endif
		endelse
	 endif
;
	 if  areu_z(0) eq 2 then begin
				 rbx=areu_z(1)   &  rby=areu_z(2) & endif $
	 else  if  cj  eq 4 then rby=areu_z(1) else rbx=areu_z(1)
;**
	 tip=areu_z(areu_z(0)+1)
return
end
;
;
;
function sl_funn ,jf,erey,vsiz,c,l,f,k_bx,k_by,explv
;******* *******  ** **** **** ***** **** **** *****
;**
;** quick around representation.
;** ----- ------ --------------
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common	my_tvg,	w_co,w_cw,w_fl,w_fy,w_hi,w_lo,w_lt,w_nc,w_no,w_od,$
		w_ps,w_ty,w_ig,w_wk
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
	common	my_kb,	kb_tb,kb_cs,kb_es,kb_ls,kb_gh,kb_bx,kb_by,kb_kk,kb_car
;**
common my_keep,	rvl,rvm,vlt,vmt
;**
common my_viewr,bxy
;**
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common  my_fun,	a,b,d,e,bfx,bfy,c1,c2,c3,c4,cj,ez,fmf,ifu,jfu,l1,l2,vp,vh,$
		rbx,rby,mn,mx,mni,mxi,h,p,rvmm,rvmi,sp,int7,fmi4,fmf9,fsmo,$
		st1,st2,st3,st4,st5,st6,st7,st8,st9,st10,st11,st12,st13,$
		st14,st15,st16,st17,st18,f24,tap,tip,mnj,mxj,c5,c6,c7,c8,c9,c10
;**
;carez + erey
;care mn,mx,mni,mxi,mnj,mxj,c1,c2,c4,rvmm,rvmi
   bb =sl_tvget(3,w_cw)
   if f_fg(45) ne 0 then bb=sl_tvsel(f_w1) else bb =sl_tvsels(f_w1)
   if bb ne 1 then begin bb=sl_glory(0)
   			 if bb gt 0 then bb=sl_tvsel(f_w1)
			 endif
   if bb eq 1 then begin
    if (f_fg(45) ne 0) and (tv_flg(1) ne 1) then begin
       bb =sl_tvget(28,l1) & bb=sl_tvget(29,l2)
       if (l1 gt 0)   then  $
       if (l1 ne f_bx) or (l2 ne f_wy) then begin
				f_wy=l2/2 & f_wy=f_wy*2
				f_wx=l1
				if f_wy lt 132 then begin
					f_fg(3)=1 & f_wy=132
				endif else if f_fg(3) eq 0 then f_fg(3)=1
				if f_fg(13) eq 0 then begin
					if f_wx*5/3 gt tv_x then f_wx=tv_x/5*3
				endif else f_wx=f_wx/5*3
				if f_wx lt 300 then f_wx=300
				f_wp=(f_wx+1)/3 & f_wp=f_wp*2
				f_wx= f_wp*3/2
				f_bx= f_wx+f_wx*f_fg(13)*2/3
				f_fg(2 )=f_fg(1)*f_wy/f_wp
				if f_fg(2) lt 2 then f_fg(2)=2
				bb  = sl_tvclear(dummy)
				bb=sl_glory (0)
				bb=sl_tvsel (f_w1)
;				bb=sl_tvwake(f_w1)
				f24=-1 & endif
    endif
    f_ab(0,0)=-1
    if (jf le 0) and (f_sh eq 0) then begin bb=sl_tvpop(f_w1,0) & f_sh=1
    endif  else  if  (jf gt 0)   then begin
    bb  =sl_tvget(4 ,w_no)
    bb  =sl_tvget(7 ,w_od)
    bb  =sl_tvget(6 ,w_fy)
    bb  =sl_tvget(8 ,w_ty)
    bb  =sl_tvget(21,w_ig)
;
    bb  =sl_tvset(4 , 1)
    bb  =sl_tvset(6 , 0)
    bb  =sl_tvset(7 ,tv_od)
;**
	cj  =jf
;	if   f_fg(45) eq  1 then bb=sl_tvpop(f_w1,1)
	if   vsiz(0)  eq  3 then ez=vsiz(3) else ez =1
	tip= vsiz(vsiz(0)+1)
	if  (tip gt 16) or (tip eq 8) then tap=1  else tap=0
	if f_sh then begin f_sh =0  & endif
;			   bb=sl_tvwake(f_w1)
;	if (jf ne f_ic) then begin
;			     int7(0)=0 & int7(4)=0
;			     for k=0,f_wy-1,2 do begin
;				 int7(2)=f_wp & int7(3)=f_wy
;				 int7(1)=k    & int7(5)=k+2
;				 int7(6)=f_w1
;				 bb=sl_tvmov(int7) & endfor
;			     endif
;**
	if f_fg(47) ne 0  then if (cj  eq 4) or (cj eq 5) then cj=3
	if (jf eq  7)	  and (ez  eq  1)   then cj=3
	if vsiz(2)  le 1  then if  cj  le 4 then cj=5
	if vsiz(1)  le 1  then if  cj  le 5 then cj=4
	if (jf eq  6)     and (jf  eq f_ic) then cj=100 +jf
	if ((f_fg(8) ne 0) or (jf eq 7)) and (cj le 3  ) then f_fg(31)=0
;**
	bb=sl_tvscreen(0,f_wp-1 ,0,f_wy-1)
;**
	mn =rvl  & mx =rvm  & km=0
	rbx= 1   & rby=1
	if f_fg(49) ge 2 then bx=f_fg(49) else bx=k_bx
	if f_fg(50) ge 2 then by=f_fg(50) else by=k_by
;**     Slice.
	if f_fg(24) eq 1 then begin
	   if f_fg(8) eq 0 then begin ifu=f & jfu=f
	   endif	   else begin ifu=f & jfu=vsiz(15)  & endelse
	   c5 =sl_slice(erey,vsiz,c,l,ifu,jfu,bxy(6),bxy(7),f_fg(25),f_sa)
	   mxj=rvm &  mnj= rvl
	   if f_fg(5) then mxj = sl_maxim(ares,ares_z,c2,mnj)
	   a  =bxy(6)-c
	   if (a ne 0) then begin
		c4= float(bxy(7)-l)/a
		c3= float(l+1) - (c4*(c+1))
		bb= sl_gf (c4,1,0,fmf)
		bb= sl_sti(st10,sl_str(c4,fmf), 2)
		bb= sl_gf (c3,1,0,fmf)
		bb= sl_sti(st10,sl_str(c3,fmf),14)
		if  c3 ge 0 then bb= sl_sti(st10,'+',14)
		if bxy(8) ge 0 then begin
			a = bxy(8)-bxy(10)
			if (a ne 0) then begin
			    c8= float(bxy(9)-bxy(11))/a
			    c7= float(bxy(11)+1) - (c8*(bxy(10)+1))
			    c4= c4-c8
			    if  c4 ne 0 then begin
				c4=(c7-c3)/c4
				c3=(c8*c4 +c7)
				bb= sl_gf (c4,1,0,fmf)
				bb= sl_sti(st11,sl_str(c4,fmf), 6)
				bb= sl_gf (c3,1,0,fmf)
	   			bb= sl_sti(st11,sl_str(c3,fmf),18)
		endif & endif & endif
	   endif
	   c4 =(180.*f_sa/3.1416)
	endif
;**	Color
	if cj eq 6 then bb=sl_psizm(areu,areu_z,2,f_wp	     ,f_wy,4   ,-1,-1)
;**	Stdev
	if cj eq 7 then bb=sl_psizm(areu,areu_z,1,(vsiz(15)+1)*2+3, 8,-1,-1,-1)
;**     Other
	if cj le 5 then begin
	 ifu=bx
	 jfu=by
 	 if (f_fg(31) eq 1) and (cj le 3) and (f_fg(32) eq 0) then   begin
             if bx  lt by  then ifu=by+bx/2 else ifu=bx+by/2
	     if ifu lt 24  then ifu=24
	     if ifu lt (f_wp-80) then begin
		ifu =  sl_pfix((f_wp-80)/ifu)  & ifu=(f_wp-80)/ifu & endif
	     jfu = ifu * f_wy /(f_wp-80)
	 endif
;
	 a = c - ifu/2 & d = a+ifu & if   a lt vsiz(7)  then a=vsiz(7)
	 b = l - jfu/2 & e = b+jfu & if   b lt vsiz(8)  then b=vsiz(8)
	 d = d - a -1		   & if a+d gt vsiz(13) then d=vsiz(13)-a
	 e = e - b -1		   & if b+e gt vsiz(14) then e=vsiz(14)-b
	 c2= 1
	 c7= 0.
	 c8= 0.
;**
	 provfu,erey,vsiz,c,l,f,bx
	 c6 = areu_z(1) & if areu_z(0) gt 1 then c6 = c6 * areu_z(2)
;**
	 if  (cj le 3) then begin
	  if (f_fg(31) eq 1) then begin
;	  low pass areu
	     rbx=bx  &  rby=by
	     mx =sl_maxim(areu,areu_z,c1,mn)
	     if  f_fg(13)      then begin
	      if f_fg(24) ne 1 then begin
			  bb=sl_pp(0 ,areu,areu_z,arei,arei_z)
			  bb=sl_d_p(7,arei,arei_z,3 ,[0,0],0,0)
			  endif
	      sl_ellip,4,areu,areu_z,0,f_el ,c-a,l-b,rbx-1,rby-1,c6,c7,mn,c8,c9
	     endif else  $
	      sl_ellip,1,areu,areu_z,0,f_el ,c-a,l-b,rbx-1,rby-1,c6,c7,mn
	  endif
	 endif
;**
	 if f_fg(5) or  f_fg(9)  or (f_fg(32) eq 1) then begin
		    if (cj gt 3) or (f_fg(31) ne 1) then begin  km=1
				 mx =sl_maxim(areu,areu_z,c1,mn)  &  endif
		    if (f_fg(32) eq 1) then if (f_ab(0,0) ge 0) then begin
				 f_ln=c1/areu_z(1)
				 f_cn=c1-areu_z(1)*f_ln
				 f_cn=f_cn+f_ab(0,0)
				 f_ln=f_ln+f_ab(0,1)
				 f_zn=     f_ab(0,2)
				 f_fg(32)=2
				 endif else f_fg(32)=0
	 endif
	 if f_fg(5) or  f_fg(9)  then begin
		    if (tip gt 16) or (tip eq 8) then tap=1 else tap=0
		    if  tip eq 2 then begin  mx=fix(mx)
					     mn=fix(mn) & endif
	 endif else begin
		    c1 =(rvm-rvl)/10  & if c1  gt tv_nc then c1=tv_nc
		    mn = rvl+ c1
		    mx = rvm-(c1*f_fg(7)) & if mx lt mn then mx=mn
		    mx = mx * c2 & mn = rvl
	 endelse
	 arev(f,2)=0. & arev(f,3)=0.
	 arev(f,5)=mn & arev(f,6)=mx
	 arev(f,8)=0.
;**
	 if f_fg(13) or (f_fg(24)  eq 2) then begin
		      if areu_z(0) eq 2  then begin
			if f_fg(31) eq 1 then c2=c9 else begin
			 c2=sl_totf(areu,areu_z(1),areu_z(2),areu_z(areu_z(0)+1))
			 c7=sl_bgbox(erey,vsiz,f,f_ab)
			endelse
			c3=c2 / c6
			arev(f,8)= c6
		      endif  else  begin
			c2=sl_totf(areu,areu_z(1),0,areu_z(areu_z(0)+1))
			c3=c2/areu_z(1)
			c7=sl_bgbox(erey,vsiz,f,f_ab)
			arev(f,8)=areu_z(1)
		      endelse
		      arev(f,2)=c2
		      arev(f,3)=c3
		      if (ez gt 1) and (f_fg(24) eq 0) $
				   and (f_fg(31) eq 0) and (cj le 5) then begin
;			bb=sl_psizm(arei,arei_z,1,vsiz(15)+3  ,8,-1,-1,-1)
;wilkinson
			if f_fg(8) eq 0 then begin
			  bb=sl_psizm(arei,arei_z,1,vsiz(15)-f+1,8,-1,-1,-1)
			  for i=vsiz(15),f,-1 do begin
			   provfu,erey,vsiz,c,l,i,bx
			   c2=sl_totf(areu,areu_z(1),areu_z(2),areu_z(areu_z(0)+1))
			   arei(i-f)=c2 - sl_bgbox(erey,vsiz,i,f_ab)*c6
			  endfor
			endif else begin
			  bb=sl_psizm(arei,arei_z,1,areu_z(2),8,-1,-1,-1)
			  bb=sl_fsum(areu,0,areu_z,arei)
			  if f_fg(8) eq 2 then begin
				areu=sl_transp(areu,areu_z(1),areu_z(2),tip)
				bb=sl_psiz(areu_z,2,areu_z(2),areu_z(1),tip,-1,-1)
			  endif
			endelse
			mxi=sl_maxim(arei,arei_z,rvmm,mni)

;			arei(0) =[0., arev  (0:vsiz(15),2) , 0.]
;			mni  = 0.& mxi=sl_maxf(arei,arei_z,rvmm)
	 endif   &    endif
;**
	 arev(f,7)=c7
;**
	 c1 =float(mx-mn) / f_fg(15)/2
	 if (jf eq 7)  and (f_fg(3)  ne 0) then  $
			if (f_fg(24) ne 1) then  $
				bb=sl_d_p(41,areu,areu_z,0,0,c10,rvmm) $
			else	bb=sl_d_p(41,ares,ares_z,0,0,c10,rvmm)
;**
	endif else c1=1
;**
	fsmo=0
	if (f_fg(12) ne 0) then fsmo=1
	if (f_fg(0)  gt 0) and (not f_fg(5)) then if fsmo eq 1  then fsmo=2 $
								else fsmo=3
;**
	if cj eq 1 then begin
			bfx =  d+1 & bfy=e+1
 			if f_fg(14) eq 2 then begin
;			   if tv_od eq 1 then bb=sl_tvscreen(f_wp-1,0 ,0,f_wy-1) ;!!??
			   if not f_fg(5) then begin  rvmm=rvm  & rvm=mx
			     		bb=sl_d_p(0,areu,areu_z,0,0,rvl,rvm)
					areu(areu_z(1)-1,0)=mx  & rvm =rvmm & endif
			   if ((d ge 31) or (e ge 31)) then begin
			     bfx =(bfx)/16 & if bfx lt 1 then bfx=1
			     bfx =(d+1)/bfx
			     bfy =(bfy)/16 & if bfy lt 1 then bfy=1
			     bfy =(e+1)/bfy
			     areu=sl_redim(areu,areu_z(1),areu_z(2),tip,bfx,bfy,0)
			     areu_z(1)=bfx & areu_z(2)=bfy
			     areu_z(6)=long(bfx)*bfy
			endif & endif
			endif   else $
	if cj eq 2 then begin
			ifu=0 & jfu=0
                        if f_fg(16) eq 12  then begin
;			   if tv_od eq 1   then bb=sl_tvscreen(0,f_wp-1,f_wy-1,0) ;!!??
			   bfx= d+1   & bfy= e+1
			   bb = sl_tvxyz(0,d,0,e)
			endif else begin
			   bfx=f_wp/2 & bfy=f_wy/2
			   areu=sl_redim(areu,areu_z(1),areu_z(2),tip,bfx,bfy,0)
			   areu_z(1)=bfx & areu_z(2)=bfy
			   areu_z(6)=long(bfx)*bfy
			endelse
			endif else $
	if cj eq 3 then begin bfy=f_wy  & bfx=f_wp
			ifu = bfx/(d+1) & jfu=bfy/(e+1)
			rvmm= mx & rvmi=mn
			if (f_fg(0) gt 0) and (not f_fg(5)) then $
		 		  bb=sl_dislog(areu,areu_z,rvmi,rvmm)
			bb=sl_scalf(areu,areu_z,rvmi,rvmm,km,2,dummy,tv_flg(2))
;			areu=sl_scale(areu,areu_z(1),areu_z(2),tip,rvmi,rvmm)
;			areu_z(areu_z(0)+1)=2
			tip =2
			if (f_fg(12)  ne 0) then o=ifu else o=-1
			if  jfu lt o then o=jfu
			if (tv_flg(0) eq 0) and (f_fg(47) eq 0) then ifu=0
;			if (f_fg(3)   lt 2) then ifu=0
			if (ifu lt 1) or (jfu lt 1) then begin
			   areu=sl_redim(areu,areu_z(1),areu_z(2),tip,bfx,bfy,0)
			   areu_z(1)=bfx & areu_z(2)=bfy
			   areu_z(6)=long(bfx)*bfy
			   if (o gt 9) then o=9 else $
			   if (o ge 0) and (o lt 3) then o=3
			   if (o gt 2) then bb=sl_lis(areu,bfx,bfy,tip,o,1)
			endif & endif
	if (cj eq 2) or (cj eq 3) then begin
			if f_fg(8) eq 0 then begin
				   p(0)=(c-a)+1
				   if tv_od eq 1 then h(0)=(b+e-l)+1   $
						 else h(0)=(l-b)  +1   & endif $
			else begin p(0)=areu_z(1)/2 & h(0)=areu_z(2)/2 & endelse
			if (cj eq 3) and (ifu*jfu ge 1) then  begin
						      h(0)=h(0)*jfu- jfu /2
						      p(0)=p(0)*ifu- ifu /2
			endif else begin h(0)=h(0)*f_wy/(e+1)  -bfy/(e+1)/2
					 p(0)=p(0)*f_wp/(d+1)  -bfx/(d+1)/2
			endelse
			p(0)= p(0)-4 &   p(1)=p(0)+8
			h(1)=h(0)
			endif
	if cj eq 4 then begin if f_fg(8) eq 2 then sp =d    else sp =e
			      if f_fg(8) eq 2 then ifu=l-a  else $
			      if f_fg(8) eq 1 then ifu=f-b  else ifu=l-b
			      if f_fg(8) eq 2 then b  =a
			      h(0)=mn  & h(1)=areu(ifu)
			      p(0)=ifu & p(1)=ifu  &  endif else $
	if cj eq 5 then begin if f_fg(8) eq 2 then sp =e    else sp =d
			      if f_fg(8) eq 2 then ifu=f-b  else ifu=c-a
			      if f_fg(8) eq 2 then a  =b
			      h(0)=mn  & h(1)=areu(ifu)
			      p(0)=ifu & p(1)=ifu  &  endif else $
	if cj eq 6 then begin areu=sl_redim ( f_pl,tv_flg(2),1,4,f_wp ,f_wy,0)
			      endif else $
	if cj eq 7 then begin jfu = vsiz(15)  & ifu=(jfu+1)*2+2 & mni=arev(0,0)
			      areu=[mni ,arev (0:jfu,0),mni,arev(0:jfu,1),mni]
			      mxi  =sl_maxim(areu,areu_z,rvmm,mni)
			      areu(0)=mni  & areu(jfu+2)=mni & areu(ifu)=mni
			      bb=sl_tvxyz(0,ifu,mni,mxi)
			      h(0)=mni & h(1)=areu(f+1)
			      p(0)=f+1 & p(1)=f+1         & endif
;**
	  st1=     sl_stx(explv, 0,15)
	  st2='(' +sl_stx(explv,19,11)+','+sl_stx(explv,37,4)+')'
	  st3='<'
	  if (f_fg(3) ne 0)  then  begin
;			     High G_H so get bx by,min max,sli surf ellip angles
;			     ---------------------------------------------------
			     bb=sl_sti(st4 ,sl_str(rbx ,fmi4),6 )
			     bb=sl_sti(st4  ,sl_str(rby ,fmi4),12)
			     bb=sl_sti(st12,sl_str(sl_pfix(f_el),fmi4),11)
			     bb=sl_sti(st17,sl_str(        c6 ,'(i9)'),7)
			     if (f_fg(24) ne 1) then begin
				  bb=sl_gf (mx,tap,0,fmf)
				  bb=sl_sti(st5,sl_str(mn, fmf   ),7 )
				  bb=sl_sti(st6,sl_str(mx, fmf   ),7 ) & endif $
			     else bb=sl_sti(st8,sl_str(c4, fmf9  ),7 )
			     if (jf eq 7) and (cj le 5) then begin
				  bb=sl_gf (c10,1,0,fmf)
				  bb=sl_sti(st9,sl_str(c10, fmf   ),7 ) & endif
			     if cj eq 1  then begin
				  bb=sl_sti(st7 ,sl_str(f_az,fmi4) ,6 )
				  bb=sl_sti(st7 ,sl_str(f_ax,fmi4) ,12)
			     endif & endif
	     if    cj lt  100  then $
	      if f_fg(47) gt 0 then st3=st3     +kb_gh(f_fg(47)) $
			       else st3=st3     +kb_gh(cj)
	     if  f_fg(8)  gt 0 then st3=st3+',' +kb_gh(8+f_fg(8))
	     if  f_fg(5)       then st3=st3+',' +kb_gh(11)
	     if  f_fg(7)  gt 0 then st3=st3+',' +kb_gh(12)
	     if  f_fg(9)       then st3=st3+',' +kb_gh(13)
	     if  f_fg(27) eq 0 then st3=st3+', '+kb_gh(14)
	     if  cj le 3  then begin
	      if f_fg(0)  gt 0 then if (not f_fg(5)) $
			       then st3=st3+', '+kb_gh(15)
	      if f_fg(12) gt 0 then st3=st3+', '+kb_gh(16)
	     endif
	     if  cj eq 3  then $
	      if f_fg(47) gt 0 then st3=st3     +kb_gh(17)
	     		    	    st3=st3+'>'
	  if f_fg(24) eq 1 then l1=0 else l1=(80*f_fg(13))
	  bb=sl_tvras(f_wp,0 , f_wx-f_wp +l1 ,f_wy,0,f_bx-1,f_wy-1)
	  l1=f_wy/2
	  l2=f_wx+2
;
	if  f_fg(13) and (f_fg(3) ne 0) and (f_fg(24) ne 1) then  begin
		vp(0)=f_wx+1 & vp(1)=f_wx+1
		vh(1)=f_wy-1
		bb=sl_tvline  (vp,vh ,2,0,-1)
	endif
;VALUE,POS
	  bb=sl_tvs(f_wp,  l1  +10,st1,1.,0,-1)
	  bb=sl_tvs(f_wp,  l1  -5 ,st2,1.,0,-1)
	  bb=sl_tvs(f_wp,2        ,st3,1.,0,-1)
	  if f_fg(3) ne 0   then begin
;DX,DY
		bb=sl_tvs(f_wp,f_wy-12,st4,1.,0,-1)
		if (f_fg(24) ne 1) then begin
;MIN,MAX,S_ANG
			bb=sl_tvs(f_wp,32       ,st5,1.,0,-1)
			bb=sl_tvs(f_wp,17       ,st6,1.,0,-1) & endif $
		else	bb=sl_tvs(f_wp,  l1  +25,st8,1.,0,-1)
;DEVIAT
		if (jf eq 7) and (cj le 5) then $
				  bb=sl_tvs(f_wp,  l1  -20,st9 ,1.,0,-1)
;SURF ANG
		if (cj eq 1) then bb=sl_tvs(f_wp,f_wy-27  ,st7 ,1.,0,-1)
;
		if  f_fg(13) and (f_fg(24) ne 1) then  begin
		 if cj le 5  then begin
;SUM
		   bb=sl_tvs(l2  ,   f_wy-12 ,st13	    ,1.,0,-1)
		   bb=sl_gf(c2,1,0,fmf)
		   bb=sl_tvs(l2  ,   f_wy-27 ,sl_str(c2,fmf),1.,0,-1)
;AVG
		   bb=sl_gf(c3,1,0,fmf)
		   bb=sl_tvs(l2  ,   f_wy-42 ,sl_str(c3,fmf),1.,0,-1)
;		   if (cj le 3) then begin
;ELLI ANG
			if (f_fg(31) eq 1)  then $
			bb=sl_tvs(f_wp,  l1  +40,st12,1.,0,-1)
;NB NPTS
			bb=sl_tvs(f_wp,       47,st17,1.,0,-1)
;SIGNAL
			bb=sl_tvs(l2  ,   l1  +10 ,st14	    ,1.,0,-1)
			c9=c2 - c7*c6
		        bb=sl_gf(c9,1,0,fmf)
		        bb=sl_tvs(l2  , l1 -5,sl_str(c9,fmf),1.,0,-1)
;SG/NOISE
			bb=sl_tvs(l2  ,   l1  -20 ,st15	    ,1.,0,-1)
			if c7 ne 0 then c9=c9 / (c7*c6)
		        bb=sl_gf(c9,1,0,fmf)
		        bb=sl_tvs(l2  , l1-35,sl_str(c9,fmf),1.,0,-1)
;STD DEV
		       if (f_fg(31) eq 1)  then begin
			bb=sl_tvs(l2  ,        17 ,st16	    ,1.,0,-1)
		        bb=sl_gf(c8,1,0,fmf)
		        bb=sl_tvs(l2  ,    2 ,sl_str(c8,fmf),1.,0,-1)
		       endif else begin
;BGR
			bb=sl_tvs(l2  ,        17 ,st18	    ,1.,0,-1)
		        bb=sl_gf(c7,1,0,fmf)
		        bb=sl_tvs(l2  ,    2 ,sl_str(c7,fmf),1.,0,-1)
		       endelse
;		   endif
		 endif
		endif
	  endif
	  if  ((cj ne  1) or (f_fg(14) eq 2)) and ((cj ne 3) or (f_fg(47) gt 0))$
	  and  (cj lt 100) and ((cj ne  2) or (f_fg(16) eq 12)) then $
		bb=sl_tvras(0,0,f_wp,f_wy,0,f_bx-1,f_wy-1)      else $	;cici
	  if   (cj eq  3) and (ifu*jfu ge 1)   then begin
		l1=f_wy-1-areu_z(2)*jfu
		if l1 gt 0 then  $
		   bb=sl_tvras(0,areu_z(2)*jfu,f_wp,l1		,0,f_bx-1,f_wy-1)
		l1=f_wp  -areu_z(1)*ifu
		if l1 gt 0 then  $
		   bb=sl_tvras(areu_z(1)*ifu,0,l1,areu_z(2)*jfu ,0,f_bx-1,f_wy-1)
		endif
	  if   cj eq 7 then begin
		bb=sl_tvs(       1,f_wy-10,'Dev' ,1.,0,-1)
		bb=sl_tvs(f_wp/2+1,f_wy-10,'Avg ',1.,0,-1) & endif
;**
	prov_fun2, jf,vsiz, c,l,f ,bx,by
;**
    bb  =sl_tvset(4 ,w_no)
    bb  =sl_tvset(7 ,w_od)
    bb  =sl_tvset(6 ,w_fy)
    bb  =sl_tvset(8 ,1,1,0,0,0,0,0)
   endif
   f_ic=jf
   f24 =f_fg(24)
   if w_cw gt 0 then bb=sl_tvsels(w_cw)
   endif
return,1
end
;
pro prov_fun2,	jf,vsiz, c,l,f ,bx,by
;** *********
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common	my_tvg,	w_co,w_cw,w_fl,w_fy,w_hi,w_lo,w_lt,w_nc,w_no,w_od,$
		w_ps,w_ty,w_ig,w_wk
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_viewr,bxy
;**
common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
		ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
		sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
		sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
		arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
common  my_fun,	a,b,d,e,bfx,bfy,c1,c2,c3,c4,cj,ez,fmf,ifu,jfu,l1,l2,vp,vh,$
		rbx,rby,mn,mx,mni,mxi,h,p,rvmm,rvmi,sp,int7,fmi4,fmf9,fsmo,$
		st1,st2,st3,st4,st5,st6,st7,st8,st9,st10,st11,st12,st13,$
		st14,st15,st16,st17,st18,f24,tap,tip,mnj,mxj,c5,c6,c7,c8,c9,c10
;**
	case cj of
;**
    1:	if c1 ne 0  then $
	if f_fg(14) ne 2 then begin
		   fz=f_az & if fz lt 0 then fz=0
		   bb=sl_surf(0,areu,bfx,bfy,1,tip,f_wp,f_wy,$
				mn,mx,fz,f_ax,1,f_fg(14),f_fg(20),fsmo)
		   bb=sl_tvset(7,1)
		   bb=sl_psiz(areu_z, 2,f_wp,f_wy,4,-1,-1)
		   bb=sl_tvimag(areu,areu_z,0,0)
	endif else bb=sl_surf(0,areu,bfx,bfy,1,tip,f_wp,f_wy,$
				mn,mx,f_az,f_ax,1,2,0,fsmo)
    2:	if c1 ne 0 then begin
		   bb=sl_tvget(17,w_nc)
		   bb=sl_tvset(17,1)
		   bb=sl_surf(0,areu,bfx,bfy,1,tip,f_wp,f_wy,mn,mx,-1,90,$
						   f_fg(15),f_fg(16),0,fsmo)
		   bb=sl_tvset(17,w_nc)
		   if f_fg(16) ne 12 then begin
		      bb=sl_tvset(7,1)
		      bb=sl_psiz(areu_z, 2,f_wp,f_wy,4,-1,-1)
		      bb=sl_tvimag(areu,areu_z,0,0)
		   endif
	bb=sl_tvmod(1,10)
	bb=sl_tvline (p,h ,2,0,-1)
	p(0)=p(0)+4 & p(1)=p(1)-4
	h(0)=h(0)-4 & h(1)=h(1)+4
	bb=sl_tvline (p,h ,2,0,-1)
	bb=sl_tvmod(1,3)
	endif
    3:	begin
	if (f_fg(47) eq 5) then begin
	    c9= f_wy/areu_z(2)
	    if  c9  gt 0 then begin
		bb= sl_tvset(8 ,0,0,0,0,0,1,1)
		bb= sl_tvset(21,0)
		bb= sl_psizm(tare,tare_z,1,areu_z(1),areu_z(areu_z(0)+1),-1,-1,-1)
		for i=long(0),areu_z(2)-1  do begin
		    if tv_od eq 0 then $
			bb=sl_tvscreen(0,f_wp-1,i*c9,(i+1)*c9-1) $
		    else $
			bb=sl_tvscreen(0,f_wp-1,f_wy-c9*(i+1),f_wy-c9*i-1)
		    bb=sl_tvxyz(0,areu_z(1)-1 ,rvmi,rvmm)
		    tare(0)=areu(*,i)
		    bb=sl_tvplt (-1,tare_z(1),tare,0)
		endfor
	    endif
	endif else if (f_fg(47) eq 4) then begin
	    c9= f_wy/areu_z(1)
	    if  c9  gt 0 then begin
		bb= sl_tvset(8 ,0,0,0,0,0,1,1)
		bb= sl_tvset(21,0)
;	        bb=sl_psizm(jare,jare_z,1,areu_z(2),4,-1,-1,-1)
;	        jare(0)=sl_index(jare_z(1),4)
		bb= sl_psizm(tare,tare_z,2,1,areu_z(2),areu_z(areu_z(0)+1),-1,-1)
		for i=long(0),areu_z(1)-1  do begin
		    if tv_od eq 0 then $
			bb=sl_tvscreen(f_wp-1,0,f_wy-c9*(i+1),f_wy-c9*i-1) $
		    else $
			bb=sl_tvscreen(0,f_wp-1,f_wy-c9*(i+1),f_wy-c9*i-1)
		    bb=sl_tvxyz(0,areu_z(2)-1 ,rvmi,rvmm)
		    tare(0,0)=areu(i,*)
		    bb=sl_tvplt (-1,tare_z(2),tare(0,*),0)
		endfor
	    endif
	endif else begin
		if (ifu*jfu gt 1) then bb=sl_tvpix(ifu,jfu)
        	bb=sl_tvimag(areu,areu_z,(f_wp-bfx)/2,0)
		bb=sl_tvpix(1,1)
	endelse
	bb=sl_tvmod(1,10)
	bb=sl_tvline (p,h ,2,0,-1)
	p(0)=p(0)+4 & p(1)=p(1)-4
	h(0)=h(0)-4 & h(1)=h(1)+4
	bb=sl_tvline (p,h ,2,0,-1)
	bb=sl_tvmod(1,3)
	end
    4:	begin
	if f_fg(3) ne 0 then begin
		bb=sl_tvscreen(25,f_wp-15,25,f_wy-15)
		bb=sl_tvset(8 ,0,0,1,1,0,0,0)
		bb=sl_tvset(13, 1)
		if (f_fg(3) ge 2) or (f_wy gt 160) then $
				 bb=sl_tvset(14,3) else bb=sl_tvset(14,1)
		bb=sl_tvaxis  (mn ,mx    ,0,'Z',1.)
		bb=sl_tvaxis  (b+1,b+sp+1,4,'Y',1.,'')
	endif else bb=sl_tvset(8 ,0,0,0,0,0,1,1)
	bb=sl_tvxyz(0,sp,mn,mx)
	bb=sl_tvset(21,0)
	bb=sl_tvplt(-1,areu_z(1),areu,0)
	bb=sl_tvget(18,w_lt)
	bb=sl_tvset(18,1)
	bb=sl_tvplt(-1,2,p,2,h)
	bb=sl_tvset(13,0)
	bb=sl_tvset(14,0)
	bb=sl_tvset(18,w_lt)
	bb=sl_tvset(21,w_ig)		& end
    5:	begin
	if f_fg(3) ne 0 then begin
		bb=sl_tvscreen(25,f_wp-15,25,f_wy-15)
		bb=sl_tvset(8 ,0,0,1,1,0,0,0)
		bb=sl_tvset(13, 1)
		if (f_fg(3) ge 2) or (f_wy gt 160) then $
				 bb=sl_tvset(14,3) else bb=sl_tvset(14,1)
		bb=sl_tvaxis  (mn ,mx    ,0,'Z',1.)
		bb=sl_tvaxis  (a+1,a+sp+1,4,'X',1.,'')
	endif else bb=sl_tvset(8 ,0,0,0,0,0,1,1)
	bb=sl_tvxyz(0,sp,mn,mx)
	bb=sl_tvget(18,w_lt)
	bb=sl_tvset(21,0)
	bb=sl_tvplt(-1,areu_z(1),areu,0)
	bb=sl_tvset(18,1)
	bb=sl_tvplt(-1,2,p,2,h)
;	bb=sl_tvfill(0,sl_index(sp+1,4),sp   ,mx,areu,mx,tv_nc/1.2,3,90)
	bb=sl_tvset(13,0)
	bb=sl_tvset(14,0)
	bb=sl_tvset(21,w_ig)
	bb=sl_tvset(18,w_lt)		& end
    6:	begin
	bb=sl_tvimag(areu,areu_z,0,0)   & end
    7:	begin
	bb=sl_tvset(21,0)
	bb=sl_tvget(18,w_lt)
	bb=sl_tvget(9 ,w_ps)
	bb=sl_tvset(18,1)
	bb=sl_tvset(8 ,0,0,0,0,0,1,1)
	bb=sl_tvplt(-1,2,p,2,h)
	jfu =jfu+3+f & h(1)=areu(jfu) & p(0)=jfu & p(1)=jfu
	bb=sl_tvplt(-1,2,p,2,h)
	bb=sl_tvset(18,0)
	bb=sl_tvset(9 ,10)
	bb=sl_tvplt(-1,areu_z(1),areu,0)
	bb=sl_tvset(9 ,w_ps)
	bb=sl_tvset(21,w_ig)
	bb=sl_tvset(18,w_lt)		& end
;**
	else:
	endcase
;**
	if f_fg(13) and ((f_fg(24) eq 0) or (f_fg(24) eq 2)) then begin
 	 if  (cj le 3) and (f_fg(31) eq 1) and (f_ab(0,0) ge 0) then begin
;	 See outside box.
		rbx=bx-1 & rby=by-1
		ifu=c -f_ab(0,0) & jfu=l -f_ab(0,1)
		tip=arei_z(arei_z(0)+1)
;
		sl_ellip,2,arei,arei_z,0,f_el,ifu,jfu,rbx,rby,  0,0,c7
		rbx=f_wp-80
		mx =sl_maxim(arei,arei_z,rvmm,mn)
;
		if f_fg(14) eq 2 then begin
;cici		   bb =sl_tvras(f_wx+80,0,rbx,f_wy,0,f_bx-1,f_wy-1)
		   bb =sl_tvscreen(f_wx+80,f_wx+80+rbx-1 ,0,f_wy-1)
;		   if tv_od eq 1 then bb =sl_tvscreen(f_wx+80+rbx-1,f_wx+80 ,0,f_wy-1) ;!!??
		   bb =sl_surf(0,arei,arei_z(1),arei_z(2),1,tip,rbx,f_wy,$
				 mn,mx,0,f_ax,1,2,0,fsmo)
		endif else begin
		   rvmm=(mx-mn)/2
		   if c7-rvmm gt mn then mn = c7-rvmm
		   if c7+rvmm lt mx then mx = c7+rvmm
		   fz=f_az & if fz lt 0 then fz=0
		   bb =sl_surf(0,arei,arei_z(1),arei_z(2),1,tip,rbx,f_wy,$
				 mn,mx,fz,f_ax,1,f_fg(14),f_fg(20),fsmo)
		   bb =sl_tvset(7,1)
		   bb =sl_psiz(arei_z, 2,rbx,f_wy,4,-1,-1)
		   bb =sl_tvimag(arei,arei_z,f_wx+80,0)
		endelse
	 endif else if (ez gt 1) and (cj le 5) and (f_fg(24) eq 0) $
			and    (f_fg(31) eq 0) then begin
;	 Plot integration vector
;	 wilkinson
	   bb=sl_tvget(18,w_lt)
;	   bb=sl_tvget(9 ,w_ps)
;	   jfu=0   & h(0)=mni &  h(1)=arei(jfu)  & p(0)=jfu & p(1)=jfu
	   bb=sl_tvset(21,0)
	   bb=sl_tvset(18,1)
	   bb=sl_tvras(f_wx+80,0,f_wp-80,f_wy,0,f_bx-1,f_wy-1)

	   if f_fg(3) ne 0 then begin
	   	bb=sl_tvscreen(f_wx+105,f_wx+f_wp-10 ,25,f_wy-7)
		bb=sl_tvset(8 ,0,0,1,1,0,0,0)
		bb=sl_tvset(13, 1)
		if (f_fg(3) ge 2) or (f_wy gt 160) then $
				 bb=sl_tvset(14,3) else bb=sl_tvset(14,1)
		c7 =sl_totf(arei,arei_z(1),0,arei_z(arei_z(0)+1))
		bb =sl_gf (c7,1,0,fmf)
		st2='S total= '+sl_str(c7, fmf)
		bb=sl_tvs(f_wx+110,f_wy-15,st2,1.,0,tv_flg(2)-1)
		bb=sl_tvaxis  (mni ,mxi  ,0,'',1.)
		bb=sl_tvaxis  (f+1,vsiz(15)+1,4,'Frames',1.,'')
		bb=sl_tvset(13,0)
		bb=sl_tvset(14,0)
	   endif else begin
	   	bb=sl_tvset(8 ,0,0,0,0,0,1,1)
	   	bb=sl_tvscreen(f_wx+80,f_wx+f_wp-1 ,0,f_wy-1)
		endelse
	   bb=sl_tvxyz(0,vsiz(15)-f,mni,mxi)
;	   bb=sl_tvplt(-1,2,p,2,h)
	   bb=sl_tvset(18,0)
;	   bb=sl_tvset(9 ,10)
	   if f_fg(8) eq 0 then bb=sl_tvplt(-1,arei_z(1),arei,0)
	   bb=sl_tvset(21,w_ig)
	   bb=sl_tvset(18,w_lt)
;	   bb=sl_tvset(9 ,w_ps)
;
	 endif else if  (jf ne f_ic) or (f_fg(24) ne f24) then begin
;	 Just color table
		bb  =sl_psizm(areu,areu_z,2,f_wp-80,f_wy,4,-1,-1)
		areu=sl_redim(f_pl,tv_flg(2),1,4,areu_z(1),areu_z(2),0)
		bb  =sl_tvimag(areu,areu_z ,f_wx+80,0)
	 endif
	endif
;**
	if f_fg(13) and (f_fg(24) gt 2) then begin
	   if f_fg(24) eq 3 then begin
;	   Output extracted points
	      if ares_z(0) eq 2 then  begin
		tip =ares_z   (ares_z(0)  +1)
		bb  =sl_psizm(areu,areu_z,2,f_wp-80,f_wy,tip,-1,-1)
		areu=sl_redim(ares,ares_z(1),ares_z(2) ,tip,areu_z(1),areu_z(2),0)
		bb  =sl_dd(2,  ares,ares_z)
		bb  =sl_scalf (areu,areu_z,0,0,0,2,dummy,tv_flg(2))
;		areu=sl_scale (areu,    f_wp-80,f_wy,tip,0,0)
;		areu_z(areu_z(0)+1)=2
		tip =2
		bb  =sl_tvimag(areu,areu_z ,f_wx+80,0)
	      endif
	      f_fg(24)=2
	   endif else if  (jf ne f_ic) or (f_fg(24) ne f24) then begin
;	   or just color table
		bb  =sl_psizm(areu,areu_z,2,f_wp-80,f_wy,4,-1,-1)
		areu=sl_redim(f_pl,tv_flg(2),1,4,areu_z(1),areu_z(2),0)
		bb  =sl_tvimag(areu,areu_z ,f_wx+80,0)
	endif & endif
;**
	if f_fg(13) and (f_fg(24) eq 1) then $
	 if ares_z(2) eq 1 then begin
;	   Output vector slice
	   bb=sl_tvras(f_wx,0,f_wp-1,f_wy,0,f_bx-1,f_wy-1)
	   if f_fg(3) ne 0 then begin
		bb=sl_tvscreen(f_wx+2,f_wx+f_wp-40 ,33,f_wy-18)
		bb=sl_tvset(8 ,0,0,1,1,0,0,0)
		bb=sl_tvset(13, 1)
		if f_fg(3) ge 2 then bb=sl_tvset(14,3) else bb=sl_tvset(14,1)
		if bxy(8)  ge 0 then bb=sl_tvs(f_wx+2,f_wy-15,st11,1.,0,tv_flg(2)*2/3)
		bb=sl_tvaxis  (mnj,mxj ,2,'Z',1.)
		bb=sl_tvaxis  ( 0 , c5 ,4,st10,1.,'')
	   endif else begin
		bb=sl_tvset(8 ,0,0,0,0,0,1,1)
		bb=sl_tvscreen(f_wx+2,f_wx+f_wp-1 ,0,f_wy-1) & endelse
	   bb=sl_tvset(21,0)
	   bb=sl_tvxyz( 0,ares_z(1)-1,mnj,mxj)
	   bb=sl_tvplt(-1,ares_z(1) ,ares,0)
	   bb=sl_tvset(21,w_ig)
	   bb=sl_tvset(13,0)
	   bb=sl_tvset(14,0)
	 endif else begin
;	   Output image slice
		tip =ares_z  (ares_z(0)  +1)
		bb  =sl_psizm(areu,areu_z,2,f_wp,f_wy,tip,-1,-1)
		areu=sl_redim(ares,ares_z(1),ares_z(2) ,tip,areu_z(1),areu_z(2),0)
		bb  =sl_scalf(areu,areu_z,mn,mx,0,2,dummy,tv_flg(2))
;		areu=sl_scale(areu,areu_z(1),areu_z(2) ,tip,mn,mx)
;		areu_z(areu_z(0)+1)=tip
		tip =2
		bb  =sl_tvimag(areu,areu_z ,f_wx,0)
	 endelse
return
end
;
;
;
function sl_raster,	erey,windn
;*******
;**
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
		kx=tv_x & ky=tv_y & kw=tv_w
		tv_x=kx & tv_y=ky & tv_w=kw
		f_fg(19)=0
		k=f_fg(12)
		f_fg(12)=1
;prov		bb=sl_views(erey,windn,ttlv,cc,lc,spc,fcl,fcg,vsiz)
		f_fg(12)=k
		f_fg(19)=1
		tv_x=kx & tv_y=ky & tv_w=kw
return,1
end
;
;
;
function sl_hardc,	sx,sy ,num ,spt,spm ,erey,vsiz,windn,fhd,nocol,lga
;******* ********
;** copy current view. (!!!! tvpix !!!!)
;** **** ******* ****
;carez
;
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_keep,	rvl,rvm,vlt,vmt
;**
common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
		io_cur,io_ext,io_seq,io_str
;**
	common my_vcol,	r,g,b ,cr,cg,cb
;**
	common	my_area,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;
on_ioerror,bobo
;
;**	fhd =0 	read screen PS--> flg=0
;**	fhd =1  encapsulat  PS--> flg=1 if possible else flg=0
;**	fhd =2  from data   PS--> flg=3
;**	fhd =3  byte img      --> flg=1 if possible else flg=0
;**	fhd =4  CGM           --> flg=0
    if (fhd le 4) then  begin
	if  (spt ge -1) and (spt ne  2) $
			and (spm ne  6) and (ared_z(0) eq 2) $
			then flg=1 else flg=0
	if  (fhd eq  0) then flg=0
	if  (fhd eq  4) then flg=0
	if  (fhd eq  2) and (vsiz(0) eq 2) then flg=3
;
	if   flg eq  1  then begin
		bb=sl_raster(erey,windn)
	endif
;**
	if   flg eq  1  then  begin
		if (sx gt sy) and (fhd ne 2) and (fhd ne 3) then  begin
	   	   bb=sl_psizm(vare,vare_z,2,sy,sx,ared_z(ared_z(0)+1),-1,-1)
		   vare(0,0)=sl_transp(ared ,sx,sy,ared_z(ared_z(0)+1))
		   bb=sl_dd(2,  ared,ared_z)
		endif else begin
		   bb=sl_pp(0,ared,ared_z,vare,vare_z)
		   if tv_od eq 1 then bb=sl_d_p(10,ared,ared_z,0,0)
		endelse
		bb= sl_d_p(38,vare,vare_z,0,[2,0],0,255 )
		endif
	if   flg eq  3  then  begin
	   	   bb=sl_psizm(vare,vare_z,2,vsiz(1),vsiz(2),vsiz(vsiz(0)+1),-1,-1)
		   if   lga eq 1  then begin
			bb=sl_pp(0,erey,vsiz,vare,vare_z)
			trvl=rvl  &  trvm=rvm
			bb=sl_dislog(vare,vare_z,trvl,trvm)
			bb=sl_scalf( vare,vare_z,trvl,trvm,0,1,dummy,tv_flg(2))
		   endif else $
			bb=sl_scalf(erey,vsiz  ,rvl,rvm,0,0,vare,tv_flg(2))
	endif
	if   flg eq  0  then  begin
		bb=sl_tvget(7,i)
		bb=sl_tvset(7,tv_od)
		if (sx gt sy) and (fhd ne 2) and (fhd ne 3) then  begin
		   bb=sl_psizm(ared,ared_z,2,sx,sy,2,-1,-1)
		   ared(0,0)=sl_tvread(0,0,  sx,sy)
		   bb=sl_d_p(10,ared,ared_z,0,0)
	   	   bb=sl_psizm(vare,vare_z,2,sy,sx,2,-1,-1)
		   vare(0,0)=sl_transp(ared ,sx,sy,2)
		   bb=sl_dd(2,ared,ared_z)
		endif else begin
		   bb=sl_psizm(vare,vare_z,2,sx,sy,2,-1,-1)
		   vare(0,0)=sl_tvread(0,0,  sx,sy)
;		   bb=sl_d_p(10,vare,vare_z,0,0)
		   endelse
		bb=sl_tvset(7,i)
	endif
	if (fhd ne 3) then begin
		id=sx+sy+num+spt+spm+windn+tv_col+11*io_seq
		io_seq=io_seq+1
		io_spe=io_cur+'scan_'+sl_stbr(sl_str(id,'(i6)'),2)
		if nocol eq 0 then bb=sl_tvgtcol(tr,tg,tb)
		if fhd lt 3 then begin
		   bb=sl_tvdev(3)
		   io_str=io_ext(16) & endif
		if fhd eq 4 then begin
		   bb=sl_tvdev(4)
		   io_str=io_ext(17) & endif
		bb=sl_tvhdfil(1,io_spe,io_str,fhd,nocol)
		if nocol eq 0 then bb=sl_tvhdlct(tr,tg,tb)
;		if nocol eq 1 then vare =vare/17
		bb=sl_tvhdimg(vare,vare_z, io_spe,io_str)
		bb=sl_tvhdfil( 0)
;		bb=sl_tvhdout(  io_spe,io_str,fhd)
		bb=sl_tvdev  (-1)
		io_txt(0) ='.Created file: '+ io_spe +'.'+io_str
		io_txt(1) ='.'
		io_txt(2) ='---->  Click here to continue  <---- '
		bb=sl_tvmenuc(0,0,io_txt,'Copy Output',-2.,-2.)
	endif
	if  fhd eq 3  then begin
		bb=sl_savarea(6, vare,[vare_z(1),vare_z(2),-1],vare_z, windn)
	endif
	bb=sl_dd(2,vare,vare_z)
    endif
bobo:	bb=sl_tvdev  (-1)
return,1
end
;
;
;
function sl_curset, exspc,exsi,exsj ,fxy,nnz
;******* *********
;**
common my_vecfun, vf_w,vf_cw,vf_wy,vf_bx,vf_py,vf_l1,vf_l2,vf_ch,vf_st,vf_ft,$
		  vf_x41,vf_x46,vf_y41,vf_y46,vf_y51,vf_y52,vf_xb4,vf_yb4,$
		  vf_g,vf_tt4,vf_mm4,vf_tt5,vf_mm5,vf_tmp,iare,jare,xare,yare,$
		  iare_z,jare_z,xare_z,yare_z,mxy,mny,mxx,mnx,a,b,d,e,p,h,vf_z
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_keep,	rvl,rvm,vlt,vmt
;**
	common  my_insert,  i_txt,i_idx,i_fil,i_rout,i_ps,i_rs,$
			    i_trout,i_tfil,i_tlang,i_tdx,i_enter,i_rcall
;**
;**
;** Info
;** ----
;**	f_fg(0)	 = 0 linear scaling	1 logarithmic scaling               N
;**	f_fg(10) = 0 Normal shape	1 Square shape                      q
;**	f_fg(12) = 1 Smoothed image(if fxy>=3)                              O
;**	f_fg(22) = 0 			1 Frames separately scaled(nz>1)    =
;**	f_fg(27) = 1 Arrows for move    0 Arrows for Resize                 #
;**	f_fg(38) > 0 Panning mode                                          ^P
;**	tv_flg(2)=   nb colors are available
;**	rvl,rvm  =   rescale boundaries
;**	i_tdx    > 2 inserted functions
;
	bb=0
	if vf_g(0) ne f_fg(0)   then begin vf_g(0)=f_fg(0)    & bb=1  & endif
	if vf_g(1) ne f_fg(10)  then begin vf_g(1)=f_fg(10)   & bb=1  & endif
	if f_fg(12) eq 1 then j=fxy  else j=0
	if vf_g(2) ne j		then begin
		 	if ((vf_g(2) ge 3) and (j lt 3))  or    $
			   ((vf_g(2) lt 3) and (j ge 3)) then     bb=1
			    vf_g(2) =j &  endif
	j=f_fg(22)*nnz
	if vf_g(3) ne j		then begin
		 	if (vf_g(3) gt 1) and (j le 1)  or    $
			   (vf_g(3) le 1) and (j gt 1) then     bb=1
			    vf_g(3) =j &  endif
	if vf_g(4) ne f_fg(27)  then begin vf_g(4)=f_fg(27)   & bb=1  & endif
	if (rvl-vlt + vmt-rvm) ne 0 then j=1 else j=0
	if vf_g(5) ne j		then begin vf_g(5)=j	      & bb=1  & endif
;	if vf_g(6) ne tv_flg(2) then begin vf_g(6)=tv_flg(2)  & bb=1  & endif
	if vf_g(7) ne f_fg(38)  then begin vf_g(7)=f_fg(38)   & bb=1  & endif

	if vf_g(8) ne i_tdx     then begin vf_g(8)=i_tdx      & bb=1  & endif

	if bb ne 0 then begin
		exsi= 9 & j=11
		if vf_g(0)  eq 1 then begin exspc(j)=exspc(0) & j=j+1 & endif
		if vf_g(1)  eq 1 then begin exspc(j)=exspc(1) & j=j+1 & endif
		if vf_g(2)  ge 3 then begin exspc(j)=exspc(2) & j=j+1 & endif
		if vf_g(3)  gt 1 then begin exspc(j)=exspc(3) & j=j+1 & endif
		if vf_g(4)  eq 1 then begin exspc(j)=exspc(4) & j=j+1 & endif
		if vf_g(4)  eq 0 then begin exspc(j)=exspc(5) & j=j+1 & endif
		if vf_g(5)  eq 1 then begin exspc(j)=exspc(6) & j=j+1 & endif
		if vf_g(7)  gt 0 then begin exspc(j)=exspc(7) & j=j+1 & endif
		exspc(j)=exspc(19)
		exsj= j
	endif
return,	bb
end
;
;
function sl_working, inc,ttl,flg,exp
;******* **********
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
;**
	common	my_tvg,	w_co,w_cw,w_fl,w_fy,w_hi,w_lo,w_lt,w_nc,w_no,w_od,$
			w_ps,w_ty,w_ig,w_wk
;**
		if (flg eq 1) then explm(0)=exp else $
		if (flg eq 2) then explm(1)=exp
;
		if (inc gt 0) and (inc ne 50) and (inc ne 100) then begin
		    i=1
;		    if f_fg(3)   eq 0 then i=0
		    if tv_flg(5) gt 0 then $
		    i=sl_vecfun(5,0,explm,'Current set', $
					tv_xp+(f_wx  +tv_dx)*(1-i)/tv_dx,$
					tv_yp-(f_wy/4+tv_dy)   *i /tv_dy)
		endif else if tv_flg(5) gt 0 then $
		    i=sl_vecfun(5,0,explm,ttl,tv_xp,tv_yp-0.1)
;
		if (flg eq 1) then begin
				if sl_stx(exp,0,1) eq '%' then  begin  w_wk=1
					bb=sl_tvshap(50)
				endif else if w_wk eq 1   then  begin  w_wk=0
					bb=sl_tvshap(-1)  &	endif
		endif else		   if w_wk eq 1   then  begin  w_wk=0
					bb=sl_tvshap(-1)  & 	endif
return,1
end
;
;
;
;
function sl_resize, dum
;******* *********
;**	 Resize image
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
				  k =fxy+xpa
				  if ccl gt lcl then i=lcl  else i=ccl
				  if lcl eq 1   then i=ccl/2
				  i  =-i/15  &  if   i gt k then i=k
				  if k eq 0 then k=1
				  i  =sl_click (i,k,fxy,6,0) & ot=2
				  if (i ne 0) and (i ne fxy) then  begin
				   if i gt 0 then  k=k-i     else  k=i-k
				   if k lt 0 then  k= -k
				  endif else k=-1
return,k
end
;
;
;
;
function sl_magang,	b6,b7,c0,c1,rsm,rsl,vta,vtm
;******* *********
;**
common	tmp_magang,	v1,v2,v3
;**
	v3 = 0.01
	v1 = float((c0 -b6)*(c0 -b6) + (c1 -b7)*(c1 -b7))
	bb = sl_sqrt(v1,1)
	v2 = float((rsm-b6)*(rsm-b6) + (rsl-b7)*(rsl-b7))
	bb = sl_sqrt(v2,1)
	bb = 0
	vta= 0.
	vtm= 1.
	if (v1 gt v3) and (v2 gt v3) then begin
	    vtm=v2/v1
	    v1 =-sl_atang( c1 -b7 , c0 -b6 )
	    v2 =-sl_atang( rsl-b7 , rsm-b6 )
	    vta=(180.*(v2-v1)/3.1416)
	    bb = 1
	    endif
	if vtm gt 1. then if vtm-1. lt v3 then vtm=1. else $
	if vtm lt 1. then if 1.-vtm lt v3 then vtm=1.
	if vta gt 0. then if  vta   lt v3 then vta=0. else $
	if vta lt 0. then if -vta   lt v3 then vta=0.
return ,bb
end
;
;
;
pro sl_pan, px,py,pz ,cc,lc,fcl, fcg,vsiz ,flg
;** ******
;**
common pan_tmp,	x1,xd
flg=0
;**	Pan X
	x1=px -  fcg(0)/2 & if x1 lt 0 then x1=0
	xd=x1 +  fcg(0)-1
	if xd ge vsiz(1)   then begin
				xd= vsiz(1)-1
				x1= xd - fcg(0)+1 & endif
	if x1 ne cc  then begin	cc= x1 & flg=1	  & endif
;**
;**	Pan Y
	x1=py -  fcg(1)/2 & if x1 lt 0 then x1=0
	xd=x1 +  fcg(1)-1
	if xd ge vsiz(2)   then begin
				xd= vsiz(2)-1
				x1= xd - fcg(1)+1 & endif
	if x1 ne lc  then begin	lc= x1 & flg=1	  & endif
;**
;**	Pan z
	if vsiz(0) gt 2 then begin
	x1=pz -  fcg(2)/2 & if x1 lt 0 then x1=0
	xd=x1 +  fcg(2)-1
	if xd ge vsiz(3)   then begin
				xd =vsiz(3)-1
				x1 =xd - fcg(2)+1 & endif
	if x1 ne fcl then begin	fcl=x1 & flg=1	  & endif
	endif
return
end
;
;
;
pro sl_pflex,	arefl,k,ttp
;** ********
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
		  if ttp eq 2 then begin
				 if (arefl(0,k) le vsiz(1)) and $
				    (arefl(0,k) gt   0    ) and $
				    (arefl(1,k) le vsiz(2)) and $
				    (arefl(1,k) gt   0    ) then begin
				     f_cn=long(arefl(0,k))-1
				     f_ln=long(arefl(1,k))-1
				     f_zn=nfp
				     sl_pan, f_cn,f_ln,f_zn,$
					     cc,lc,fcl, fcg,vsiz,  bb
				     if bb then begin	f_fg(35)=5
							ired(0)=f_cn
							ired(1)=f_ln
							ired(2)=f_zn
				     endif else f_fg(32)=2
				     endif
		  endif else if ttp eq 1 then begin
				 if (arefl(1,k) le vsiz(1)) and $
				    (arefl(1,k) gt   0    ) and $
				    (arefl(2,k) le vsiz(2)) and $
				    (arefl(2,k) gt   0    ) and $
				    (arefl(3,k) le   nz   ) and $
				    (arefl(7,k) le vsiz(1)) and $
				    (arefl(8,k) le vsiz(2)) then begin
				     if arefl(3,k) le 0 then arefl(3,k)=1
				     f_cn=long(arefl(1,k))-1
				     f_ln=long(arefl(2,k))-1
				     f_zn=long(arefl(3,k))-1
				     sl_pan, f_cn,f_ln,f_zn,$
					     cc,lc,fcl, fcg,vsiz,  bb
				     if bb then begin	f_fg(35)=5
							ired(0)=f_cn
							ired(1)=f_ln
							ired(2)=f_zn
				     endif else f_fg(32)=2
				     f_el=arefl(6,k)
				     if (f_el ge  360.) or $
					(f_el le -360.) then f_el=0.
				     f_fg(1)=arefl(7,k)
				     f_fg(2)=arefl(8,k)
				     if f_fg(1) lt 2 then f_fg(1)=15
				     if f_fg(2) lt 2 then f_fg(2)=15
		  endif    &     endif
return
end
;
;
;
pro provi,  windn,erey
;** *****   ***** ****
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common  my_insert,  i_txt,i_idx,i_fil,i_rout,i_ps,i_rs,$
			    i_trout,i_tfil,i_tlang,i_tdx,i_enter,i_rcall
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;carez + erey
	if   vsiz(0) ge 3 then nz=vsiz(3) else nz = 1
	four=vsiz(0)
;**	Get window info.
;**	--- ------ ----
		w     = tv_win(0 ,windn)
		ccl   = tv_win(1 ,windn)
		lcl   = tv_win(2 ,windn)
		fxy   = tv_win(3 ,windn)
		ny    = tv_win(4 ,windn)
		nnz   = tv_win(5 ,windn)
		cc    = tv_win(6 ,windn)
		lc    = tv_win(7 ,windn)
		fcl   = tv_win(8 ,windn)
		fac   = tv_win(9 ,windn)
		fvw   = tv_win(10,windn)
		num   = tv_win(11,windn)
		ins   = tv_win(12,windn)
		spc   = tv_win(13,windn)
		cf    = tv_win(14,windn)
		cm    = tv_win(15,windn)
		typ   = tv_win(16,windn)
		if w gt 0 then tv_od = tv_win(17,windn)
;		if tv_win(18,windn) ne tv_col then sl_manycol,tv_win(18,windn)
		xpa   = tv_win(19,windn)
		sdt   = tv_win(20,windn)
		dxy(0)= tv_win(24,windn)
		dxy(1)= tv_win(25,windn)
		rot   = tv_win(26,windn)
		fqc   = tv_win(27,windn)
		bxy(12)=tv_win(28,windn)
		bxy(13)=tv_win(29,windn)
		spt   = spc/10
		spm   = spc-10*spt  & if spm  lt 0 then spm =-spm
		if (spt eq  1) or (spt eq -4) or (spt eq -6) then begin
		   f_az    =tv_win(30,windn)
		   f_ax    =tv_win(31,windn)
		   f_fg(14)=tv_win(32,windn)
		   f_fg(20)=tv_win(35,windn) & endif else $
		if (spt eq -1) then begin
		   f_fg(15)=tv_win(33,windn)
		   f_fg(16)=tv_win(34,windn) & endif
		f_fg(0)    =tv_win(38,windn)
		f_fg(10)   =tv_win(39,windn)
		f_fg(12)   =tv_win(40,windn)
		f_fg(22)   =tv_win(41,windn)
		if  fac gt 1 then fcx= float(fac) else fcx=1.
		if -fac gt 1 then fcy=-float(fac) else fcy=1.
		if fxy  ge 0 then begin fcx= fcx * fxy
					fcy= fcy * fxy
		endif 	     else begin fcx= fcx /(-fxy)
					fcy= fcy /(-fxy)
			     endelse
		if  lcl gt 1 then begin
		 if ccl gt 2 then if f_fg(1) gt ccl then  f_fg(1)=ccl
		 if lcl gt 2 then if f_fg(2) gt lcl then  f_fg(2)=lcl
		 if frst eq 1 then begin
		    f_fg(2 )=f_fg(1)*f_wy/f_wp  & if f_fg(2) lt 2 then f_fg(2)=2
		 endif
		endif
		fcg(0)  =ccl   & fcg(1)  =lcl & fcg(2)  =nnz
		bxy(0)  =0     & bxy(1)  =0   & bxy(2)  =ccl   & bxy(3) =lcl
		bxy(4)  =0     & bxy(5)  =0
		w_num(0)=windn & w_num(1)=num & w_num(2)=-1
		f_fg(41)=0
		fdp=   0
		if sdt eq 2 then fdd = 1 else fdd = 0
		ttlv= 'x'
		if (vsiz(1) eq ccl) and (vsiz(2) eq lcl) and $
					(nz eq nnz)      then  dif=0 else dif=1
		typ   = vsiz(vsiz(0)+1)
		if (typ ge 8 )	    and (typ ne 16)      then    f=1 else   f=0
                if (typ eq 64)      then cpx=1 		 else  cpx=0
		ccl = ccl-1
		lcl = lcl-1
		fcm = nnz-1
;**
		if   dif then begin fp=fcl  &  cp=cc & lp=lc & endif else begin
				    fp=0    &  cp=0  & lp=0  & endelse
		vsiz( 7)=cp     & vsiz( 8)=lp     & vsiz( 9)=fp
		vsiz(10)=ccl    & vsiz(11)=lcl    & vsiz(12)=fcm
		vsiz(13)=cp+ccl & vsiz(14)=lp+lcl & vsiz(15)=fp+fcm
;**
		if (vsiz(1) le vsiz(13)) or $
		   (vsiz(2) le vsiz(14)) or $
		   (nz      le vsiz(15)) then w=0
;**		Seems to be ok.
;**		----- -- -- --
;**
		if arev_z(1) lt nz then begin	arev_z(1) =nz
						bb=sl_dd(1,arev,arev_z) & endif
;**		Special view.
;**		------- ----
		if (cpx) then begin
		    bb =sl_pp(0,  erey,vsiz,arel,arel_z)
		    bb =sl_d_p(16,erey,vsiz,0)

;		    vlt=sl_minf(erey,vsiz,cf)
;		    vmt=sl_maxf(erey,vsiz,cm)
;		    bb =sl_d_p(30,erey,vsiz,0,0,vlt,vmt)
		    f_fg(0)=1

		    f_fg(30)=4 & vik=18 & cpx=0 & ot=3
		endif
		if frst eq  1 then begin
		   frst  =  0
		   if (spt eq -2)  then begin
		      if w lt  0 then bb=sl_savarea(3,arec,w_num,arec_z) $
		      else if sdt eq 0 then begin
			bb=sl_resarea(3,arec,arec_z,dxy,w_num)
			res(0)=sl_spacial(erey,1,0,vsiz,rot)
		      endif  else bb=sl_dd(2,arec,arec_z)
		      if arec_z(6)  eq 0   then w=-1   & endif
		   if (spt eq -3) or (spm eq 6) or (spt eq -4) then  begin
			      if spt eq -3 then clfc(0)=spm else clfc(0)=0
			      bb=sl_d_p(40,erey,vsiz,dif,clfc)
			      f_fg(44)= 1
		   endif else f_fg(44)=-1
		endif
;**
;**
		if (tv_lst gt 0) and (tv_lst ne w) then begin
					bb=sl_tvsel (tv_lst)
					if bb then bb=sl_tvtidy(tv_lst,1)
					endif
		if ( w     gt 0) then begin
					bb=sl_tvsel (w)
					if bb then begin
						bb=sl_tvwake(w)
						bb=sl_tvcur_w(w,-1,-1, 0,0,0)
						bb=sl_tvmcur(1,0,0)
						tv_lst  = w
					endif else w=0
		endif
		if ( w     gt 0) then begin
;**		Point to the maximum in view.
;**		----- -- --- ------- -- ----
		   if cf eq cm   then begin
		      vl  = sl_minf(erey,vsiz,cf)
		      vl  = sl_maxf(erey,vsiz,cm)
		   endif

		   tv_win(14,windn)=cf
		   tv_win(15,windn)=cm
;**
		   nf  = cf /   (vsiz(1)*vsiz(2))
		   cf  = cf - nf*vsiz(1)*vsiz(2)
		   l   = cf /    vsiz(1) & c =cf - vsiz(1)*l
		   if nz eq 1 then vl =  erey(c ,l)  else vl=erey(c ,l ,nf)
		   bb=sl_cv(vl,vf,f,0,cpx)
		   vlt=vf
;**
		   nf  = cm /   (vsiz(1)*vsiz(2))
		   cm  = cm - nf*vsiz(1)*vsiz(2)
		   l   = cm /    vsiz(1) & c =cm - vsiz(1)*l
		   if nz eq 1 then vl =  erey(c ,l)  else vl=erey(c ,l ,nf)
		   bb=sl_cv(vl,vm,f,0,cpx)
		   vmt=vm
		   if rvl eq rvm then begin rvl = vf & rvm = vm & endif
;**
		g      = 0
		bb=sl_gf(vm, f,g ,fmt)
		bb=sl_sti(explz,sl_str( vm  ,fmt) ,6 )
		bb=sl_sti(explz,sl_str((c+1) ,i5) ,19)
		bb=sl_sti(explz,sl_str((l+1) ,i5) ,25)
		bb=sl_sti(explz,sl_str((nf+1),i4) ,37)
;**
		c=c-cp & l=l-lp
		if nf  ge fp then   nf=nf-fp
		if (c  lt 0) or (c  gt ccl) then c =ccl/2
		if (l  lt 0) or (l  gt lcl) then l =lcl/2
		if (nf lt 0) or (nf gt fcm) then nf=fcm/2
;**
		if vik eq 0 then explm(1)=explz
		if num ge 0 then bb=sl_sti(ex_l, sl_str(num,i6),35)
;**		Set first cursor position.
;**		--- ----- ------ --------
		s_out=  0
		if (spt le -2)  then begin
;**			Special view.
;**			------- ----
			if (spt eq -2)  then begin
			   bb=sl_psiz(csiz,2,arec_z(1),arec_z(2),arec_z(4),-1,-1)
			   i =sl_maxf(arec(*,*,0),csiz,j) & endif else $
			if (spt eq -3) or (spt eq -4) then  begin
			   bb=sl_psiz(csiz,2,dxy(0),dxy(1), typ,-1,-1)
			   j =l*dxy(0)+c & s_out=3	  & endif else $
			if (spt eq -6)  then begin
			   bb=sl_psiz(csiz,2,ccl+1 ,ccl+1 , typ,-1,-1)
			   j =ccl*ccl/2  &   endif
			y   =  j   / csiz(1)
			x   =  fix(fcx*(j  - csiz(1)*y))
			plx =  fix(fcx* csiz(1))
			ply =  fix(fcy* csiz(2))
			plny=  ply*ny
			nx  =  1
			y   =  ply - fix(fcy * y) - 1
			if tv_od eq 0 then y=ply-y- 1
		endif   else   begin
;**			Normal view.
;**			------ ----
			plx =  fix(fcx*(ccl+1))
			ply =  fix(fcy*(lcl+1))
			plny=  ply* ny
			nx  =  nnz/ ny & if nx*ny lt nnz then nx=nx+1
			if ired(0) ge 0 then begin
			   c=  ired(0)-cp
			   l=  ired(1)-lp
			   nf= ired(2)-fp
			   ot=2 & endif
			sl_dc, c,l ,x,y
			endelse
		ired(0)=-1
		plnx  =plx*nx
		if (x ge plnx) or (y ge plny) then begin
					x=0 & y=0 & c=0 & l=0 & nf=0 & endif
		mfi(0)=ccl+1 & mfi(1)=lcl+3
		sl_box,0
		xu=plnx/2
		yu=plny-15
		if not ros then begin
		  if  (spt  eq 1) or  (spt eq -4) or  (spt eq -1) then begin
;		   if (f_vu gt 0) and (f_vu le 5) then f_vu=1
		   if (spt eq -1) then i=f_fg(16) else i=f_fg(14)
		   bb=sl_surf(-1,0,ccl+1,lcl+1,fcm+1,$
				 0,plx,ply,0,0,f_az,f_ax,f_fg(15),i,f_fg(20),0)
		   endif
		  arev(*,0)  =-1
		  arev(*,1:3)= 0
		endif
		bb=sl_sti(explb,sl_str(cc +1    ,i5),7 )
		bb=sl_sti(explb,sl_str(cc +1+ccl,i5),14)
		bb=sl_sti(explb,sl_str(lc +1    ,i5),20)
		bb=sl_sti(explb,sl_str(lc +1+lcl,i5),27)
		bb=sl_sti(explb,sl_str(fcl+1    ,i4),33)
		bb=sl_sti(explb,sl_str(fcl+1+fcm,i4),39)
		if vik eq 0 then explm(0)= explb
		inc=0
		if vik gt 0 then begin inc=vik & vik=0 & endif
		if   f_fg(38)  ne 2 then begin
		 if (inc eq 0) or (inc eq 50) then begin
			ot= 3
			ros	=0
			waits	=wayt
			entitl  =ex_p2
			bb=sl_tvnobut(0)
			if tv_flg(6) eq 0 then if tv_flg(1)  eq 0 then $
			i =sl_tvmenu(1,0,expex,ex_ex,tv_x/tv_dx,0)
			bb=sl_tvpop (w,1)
			i =sl_vecfun(4,0,expl,ex_l,tv_xp,tv_yp)
			bb=sl_tvwmaj(w,vf,vm,rvl,rvm,f_fg,f_vu,spt,f_ax,f_az)
		 endif 	else if not ros then begin
			if cpx then i=sl_vecfun(4,0,exff,ex_ff,tv_xp,tv_yp)
			i =sl_vecfun(4,2)
			bb=sl_tvshap(-1)
			bb=sl_working(inc, entitl,0) & endif
		 bb=sl_curset(exspc,exsi,exsj, fxy,nnz)
		 if bb eq 1 then if (tv_flg(7) eq 0) then begin
;			bb=sl_tvfont(1)
			if i_tdx gt 2 then $
			 i=sl_tvmenuh(-3,-24,[exspc(exsi:exsj),i_trout(0:i_tdx),$
				      exsph],ex_sp,(tv_x-200)/tv_dx,tv_y/tv_dy) $
			else $
			 i=sl_tvmenuh(-3,-24,[exspc(exsi:exsj),$
				      exsph],ex_sp,(tv_x-200)/tv_dx,tv_y/tv_dy)
			bb=sl_tvpop (w,1)
;			bb=sl_tvfont(0)
		endif
		endif
		fct(0)=1 & fct(1)=1 & fct(2)=1
		if fcx lt  1 then  fct(0)=fix(1/fcx)
		if fcy lt  1 then  fct(1)=fix(1/fcy)
		if fct(0)*fct(1) gt 1 then fqc=1 else fqc=0
		clop =  0
		xs   =  c  & xq=c
		ys   =  l  & yq=l
		zs   =  nf & zq=nf
		endif
return
end
;
;
pro provc,  windn,erey
;** *****   ***** ****
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;carez + erey
			    entitl  = ex_p2
			    bb=sl_tvmcur(2,xu,yu)
			    if vin lt 0 then begin
					     i=sl_tvmenul(0,0,expc,ex_c,-2.,-2.)
					     if i ge 0 then  i=exz(i,0)
			    endif	else i=vin
			    vin=-1
			    ot=1
;**		Change table.
;**		------ -----
			    if i eq 0 then begin sl_manycol,-1
						 keep_col=tv_col & endif else $
			    if i eq 1 then begin
;**		Lower & upper color limit.
;**		----- - ----- ----- -----
				j= sl_tvmerr(0)
				if j eq 4  then sl_colexp,-1 $
				else if j  eq 2 then begin
				     entitl = ex_p1
				     vd	    =-1
				     f_vu   = 6
				     inc    = j
				endif
				endif  else $
			    if i eq 2  then begin
;**		Rescale.
;**		-------
			      if s_out eq 0 then begin
				rvl=vf & rvm=vm
				fmx=fmt
;				if spt eq -3 then begin
;				rvm=sl_maxim(ov_sum1,ovs1_z,j,rvl)
;				vd =sl_maxim(ov_sum2,ovs2_z,j, vl)
;					 if rvl gt vl then rvl=vl
;					 if rvm lt vd then rvm=vd
;				vd =sl_maxim(ov_sum3,ovs3_z,j, vl)
;					 if rvl gt vl then rvl=vl
;					 if rvm lt vd then rvm=vd
;				bb =sl_gf(rvm,1,g,fmx)
;				endif
			        bb=sl_sti(explr, sl_str(rvl,fmx),18)
			        bb=sl_sti(explr, sl_str(rvm,fmx),33)
				explm(1)= explr
				j	= sl_vecfun(4,0,exps,ex_s1,tv_xp,tv_yp)
				fldat   = 0
				f_fg(48)= 0
		                inc=3
			      endif
			      endif  else $
			    if i eq 4  then begin
;**		Slice.
;**		-----
			      if ccl*lcl gt 0 then begin
				f_fg(24)= 1  & f_fg(25)=0  & f_fg(26)=0
				rql=f_fg(25) & rqm=f_fg(26)
				bxy(6)  = (ccl+1)/2-1+cp
				bxy(7)  = (lcl+1)/2-1+lp
				bxy(8)  =  -1
			        bb=sl_sti(explc, sl_str(bxy(6)+1,i4),12)
			        bb=sl_sti(explc, sl_str(bxy(7)+1,i4),18)
			        bb=sl_sti(explc, sl_str(rql*2 +1,i3),26)
			        bb=sl_sti(explc,	       'OFF',40)
				explm(1)= explc
				j	= sl_vecfun(4,0,exci,ex_i,tv_xp,tv_yp)
				f_vu	= 3
				if (spt ne 0) and (spt ne -1)  then begin
						 spt=0 & vik=14 & zerr=37
				endif else  inc=14
			      endif
		 	      endif  else $
			    if i eq 5  then begin
;**		Overviews.
;**		---------
				bb=sl_trsig(0,0, 0,0,0,1)
				tv_flg(7)=0
				endif  else $
			    if i eq 6  then begin
;**		Un_expand.
;**		---------
				if dif then begin
				 zerr=35
				 ired(0)=ccp & ired(1)=llp & ired(2)=nfp
				 endif
				endif  else $
			    if i eq 7  then begin
;**		Hard copy.
;**		---- ----
				bb=sl_working(inc,entitl,1,expli) & ot=1
				bb=sl_hardc(bxy(12),bxy(13),num,spt,spm,erey,$
					   vsiz,windn,f_fg(33),f_fg(51),f_fg(0))
;**		Settings.
;**		--------
				endif  else  $
			    if i eq 8  then bb=sl_settings(mfi,xu,yu,zerr,0,-1) $
					    else $
			    if i eq 10 then begin
;**		Other graphics.
;**		----- --------
;**		Image processing.
;**		----  ----------
;**		Data  processing.
;**		----  ----------
;**		Math  functions .
;**		----  ----------
					    zerr=5 & endif else $
			    if i eq 11 then zerr=6 else $
			    if i eq 12 then zerr=7 else $
			    if i eq 13 then zerr=8 else $
			    if i eq 14 then zerr=9 else $
			    if i eq 16 then begin
;**		Save this work.
;**		---- ---- ----
			     bb=sl_working(inc,ex_p2,1,explp)
		   	     bb=sl_psizm(vare,vare_z,2,bxy(12),bxy(13),2,-1,-1)
	      		     bb=sl_psizm(sare,sare_z,2,192    ,192    ,2,-1,-1)
		   	     vare(0,0)=sl_tvread(0,0,  bxy(12),bxy(13))

			     if bxy(12) ge bxy(13) then vd=bxy(12) else vd=bxy(13)
			     vd=192./vd
			     x1=fix(bxy(12)*vd) & y1=fix(bxy(13)*vd)
			     if (x1 le 50)  or (y1 le 50)  then begin x1=192 & y1=192 & endif
			     xd=(192-x1)/2  &  yd=(192-y1)/2

	      		     sare(xd,yd)=sl_redim(vare,  bxy(12),bxy(13),2,x1,y1,0)
	      		     bb=sl_dd(2,vare,vare_z)
			     if  dif then  begin
			         tv_win(14)=  0
			         tv_win(15)=  0
			         if nz  eq 1 then begin
				  bb=sl_psiz(csiz,2,ccl+1,lcl+1,typ,-1 ,-1)
			          bb=sl_savarea(0,erey(cp:vsiz(13),lp:vsiz(14))$
				    	             ,fcg,csiz,windn,sare,sare_z)
				 endif else begin
				  bb=sl_psiz(csiz,3,ccl+1,lcl+1,nnz,typ,-1)
			          bb=sl_savarea(0,erey(cp:vsiz(13),lp:vsiz(14),$
				     	 fp:vsiz(15)),fcg,csiz,windn,sare,sare_z)
				 endelse
			     endif  else bb=sl_savarea(0,erey,fcg,vsiz,windn,sare,sare_z)
	      		     bb=sl_dd(2,sare,sare_z)
			     bb=sl_working(inc,ex_p2,1,explb)
			     endif  else $
			    if i eq 17 then begin
;**		Duplicate the view.
;**		--------- --- ----
				bb=sl_d_p(15,erey,vsiz,dif)
				zerr=38
				endif  else $
			    if i eq 19 then begin
;**		Remove the view.
;**		------ --- ----
				c=-2 & zerr=4
;**		Insert.
;**		------
				endif  else $
			    if i eq 20 then begin
				clfc(0)=ccp & clfc(1)=llp & clfc(2)=nfp
				zerr  =0
				if f_fg(42) eq 1 then begin
				   bb=sl_funn  (0)
				   bb=sl_vecfun(-1,0)
				   bb=sl_tvpop(w,0)
				   bidare=1
				   j=sl_break(erey,vsiz,clfc,bidare,csiz)
				   if j eq 4 then begin zerr=35  &  inc=0
				   endif     else begin
					vd = sl_maxim(erey,vsiz,j,vl)
					if (vl ne vf) or (vd ne vm) then zerr=39
				   endelse
				   f_fg(42)=0
				endif else $
				if f_fg(42) eq 0 then begin
				   j=sl_insert(w,erey,vsiz,clfc)
				   f_fg(42)=3
				endif else f_fg(42)=0
				if j eq 1 then zerr=39
				if zerr eq 0 then begin
					bb=sl_tvsel(w)
				   	if bb eq 1 then bb=sl_tvpop(w,1)
					endif
			    	endif  else $
			    if i eq 21 then begin
;**		Molecule.
;**		--------
;				bb=sl_molimg(erey,vsiz,rvl,rvm)
;				spt=-6 & zerr=38
			    	endif  else $
			    if i eq 22 then begin
;**		Quit.
;**		----
				zerr=4 & c=-1
				endif  else $
			    if i eq 89 then begin
;**		Area for FFT test
;**		---- --- --- ----
				clfc(0)=129 &clfc(1)=clfc(0)
				bb=sl_d_p(89,erey,vsiz,8,clfc,255.)
				zerr=38 & spt=0
			    endif

			    bb=sl_tvmcur(2,x,y)
return
end
;
;
;
pro provj, dum
;** *****  ***
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
			    entitl  = ex_p2
;			    if (nnz gt 1) then begin
				if lcl le 1 then i=0 else $
				i	 = sl_tvmenul(0,0,expr,ex_r,-2.,-2.)
			        zerr=1
				if (i eq 2) and (nnz gt 1) then begin
;no masun			   rsl	 = nf &  rsm = nf
;masun
				   rsl	 = 0  &  rsm = nnz-1
;end masun
				   bb=sl_sti(explr,sl_str( rsl+fcl+1,i5),20)
				   bb=sl_sti(explr,sl_str( rsm+fcl+1,i5),35)
				   explm(1) = explr & ot=1
				   j= sl_vecfun(4,0,expx,ex_x1,tv_xp,tv_yp)
		                   inc=4
				   endif   else $
				if i eq 1  then begin
				   rsl	 = 0
				   rsm	 = lcl
				   bb=sl_sti(explr,sl_str( rsl+lc+1,i5),20)
				   bb=sl_sti(explr,sl_str( rsm+lc+1,i5),35)
				   explm(1) = explr & ot=1
				   j= sl_vecfun(4,0,expx,ex_x2,tv_xp,tv_yp)
		                   inc=5
				   endif   else $
				if i eq 0  then begin
				   rsl	 = 0
				   rsm	 = ccl
				   bb=sl_sti(explr,sl_str( rsl+cc+1,i5),20)
				   bb=sl_sti(explr,sl_str( rsm+cc+1,i5),35)
				   explm(1) = explr & ot=1
				   j= sl_vecfun(4,0,expx,ex_x3,tv_xp,tv_yp)
		                   inc=6
				   endif   else $
				if i eq 3  then begin
				   rsl	 = ccl+1
				   rsm	 = lcl+1
				   bb=sl_sti(explk,sl_str( nnz,i5),36)
				   j =sl_vecfun(4,0,expk,ex_x4,tv_xp,tv_yp)
				   f_vu  = 3
				   if f_fg(31) eq 1 then $
					 bb=sl_trsig(zerr,inc,vin,mfi,ot,98)
				   f_fg(27)=0
				   f_fg(49)=f_fg(1)
				   f_fg(50)=f_fg(2)
		                   inc=9
				endif
;			    endif
return
end
;
;
;
;
pro provk, windn
;** *****  *****
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
				k=0
				while k ge 0 do  begin k=-1
			         bb=sl_tvmcur(2,xu,yu)
				 if vin lt 0 then begin
					     j=sl_tvmenul(0,0,expg,ex_g,-2.,-2.)
					     if j ge 0 then j=exz(j,3)
				 endif	     else j=vin
			         vin = -1
;**			Defaults menu.
;**			-------- ----
				 if (j eq 9) then  begin
				  k  =0
				  m4 =3
				  while k ge 0 do  begin
				   if m4 ne 4 then bb=sl_tvmcur(2,xu,yu)
				   ex_t1=expgs (7)
				   bb=sl_sti(ex_t1,sl_str(f_az,i4),19)
				   bb=sl_sti(ex_t1,sl_str(f_ax,i4),32)
				   expgs (7)=ex_t1
				   ij=sl_tvmenul(0,m4,expgs,ex_gs,-2.,-2.)
				   if ij ge 0 then i =exz(ij,4) $
				   	else begin i =-1 & j=-1 & k=-1 & endelse
				   m4=4
				   case  i of
				   0:	begin sl_stron,expgs,k,k,37,37,' ',' '
					k=-1 & ij=-1 & end
				   1:	begin
					f_fg(14) = 1
					j=1 & end
				   3:	begin
					f_fg(14) = 3
					j=1 & end
				   4:	begin
					f_fg(14) = 4
					j=1 & end
				   5:	begin
					f_fg(14) = 5
					j=1 & end
				   6:	begin
					f_az=sl_click(-5,360,f_az,1,0)
					f_ax=sl_click( 0,360,f_ax,2,0)
					ij=-1 & ot=2 & end
				   7:	if nnz gt 1 then begin
					f_fg(14) =6
					j=1
					endif
				   8:	begin  ij=tv_nc/4
					f_fg(15) =sl_click(2,ij,f_fg(15),3,0)
					ij=-1 & ot=2 & end
				   10:	begin
					f_fg(16) =10
					j=0 & end
				   11:	begin
					f_fg(16) =11
					j=0 & end
				   12:	begin
					f_fg(14) = 7
					j=1 & end
				   13:	begin
			 		if f_fg(12) eq 1 then f_fg(12)=0 $
							 else f_fg(12)=1
					if f_fg(12) ne 1 then begin
					   sl_stron,expgs,ij,ij,37,37,' ',' '
					   ij=-1     & endif
					j= 6         & end
				   14:	begin
					j =sl_tvmerr(0)
					if j eq 4 then  begin
					   sl_stron,expgs,ij,ij,20,20,' ','*'
					   sl_stron,expgs,ij,ij,28,36,' ',' '
					   f_fg(20)=3 & endif
					if j eq 2 then  begin
					   sl_stron,expgs,ij,ij,28,28,' ','*'
					   sl_stron,expgs,ij,ij,20,36,' ',' '
					   f_fg(20)=1 & endif
					if j eq 1 then  begin
					   sl_stron,expgs,ij,ij,36,36,' ','*'
					   sl_stron,expgs,ij,ij,20,28,' ',' '
					   f_fg(20)=2 & endif
					j = 1 & ij=-1 & end
				   15:	if nnz eq 2 then begin
					f_fg(14) =8
					j=1
					endif
				   16:	begin
					f_fg(16) =13
					j=0 & end
				   else:ij=-1
				   endcase
				   if ij ge 0 then begin
					sl_stron,expgs,k,ij,37,37,' ','*'
					k =ij & endif
				  endwhile
				  bb=sl_tvdmenu(0)
				  if (j eq 9) or (f_fg(37) eq 0) then begin
						  f_fg(35)=0 & j=9  & endif
				  if (j eq 0) or (j eq 1)	 then begin
					   ot=2 & f_fg(35)=0 & j=9  & endif
				  i= (tv_flg(17) and 1) & if i eq 0 then begin f_fg(14)= 2
				  					       f_fg(16)=12 & endif
				 endif
;**
				 if (j le 8) and (j ge 0) then  begin
				    ij  =spt
				    zerr=37
;**			Mapped contour.
;**			------ -------
                                    if (j eq 0) then spt=-1
;**			Surface.
;**			-------
                                    if (j eq 1) then begin
					if f_fg(14) eq 6  then spt=-4 else spt=1
					if f_fg(14) eq 8  then spt=-4
					if f_fg(14) eq 3  then i=104  else $
					if f_fg(14) eq 4  then i=111  else $
					if f_fg(14) eq 5  then i=104  else $
					if f_fg(14) eq 6  then i=108  else $
					if f_fg(14) eq 7  then i=101  else $
					if f_fg(14) eq 10 then i=108  else i=105
;					sl_manycol,i
					endif
;**			Projections.
;**			-----------
                                    if (j eq 2) or (j eq 5) then $
				     if vsiz(2) gt 1 then begin
					if f_fg(46) ne 0 then f_fg(46)=0 else $
					if spm eq  6 then spm=0 else $
					if spt eq -3 then spt=0 else $
					if nnz eq  1 then spm=6 else spt=-3
				        if spt eq -3 then begin
					 if ((j eq 5) and (spm ne 1)) or $
					    ((j eq 2) and (spm eq 1)) then fdp=1
					   if j eq 5 then spm=1 else spm=0
					   sl_manycol,105
				     endif & endif
;**			Deep, volume.
;**			----  ------
				     if (j eq 3) or (j eq 4)  then $
					if fcm  gt 0 then begin  spt= 0
						f_fg(46)= 0
						if j   eq 3 then spt=-2
						if j   eq 4 then f_fg(46)=1
						if spm eq 6 then spm=0
						sl_manycol,103
					endif else zerr=1
;**			Vectors.
;**			-------
                                    if (j eq 7)   then spt=2
;**			Video.
;**			-----
				    if (j eq 6)   then spt=0
;**                     Choice frame.
;**			------ -----
				    if nnz gt 1 then $
				     if (f_fg(46) eq 0)  then $
				     if (f_fg(10)) or ( (plx/ply lt 2)  and  $
							(ply/plx lt 2)) then $
				     if  (spt eq -1) or (spt eq 0)  or $
					 (spt eq  1) then $
				      if (ij  eq -1) or (ij  eq 0) or (ij eq 1)$
				      then begin
					   i=sl_click(-5,fcm+1,-1,12,0)
                                           if i gt 0 then f_fg(11)=fcl+i-1 else $
					   if i eq 0 then begin
						     spt= ij & zerr=1 & endif
					   ot=2 & endif
				 endif
				case  j of
;**			Resize.
;**			------
				14:begin k =sl_resize(0)
					 if k ge 0 then begin
						tv_win(19,windn)=k
						k=-1  & zerr=37
					 endif
				  end
;;**			Image rotations or flick.
;;**			----- --------- -- -----
				15:begin
				   if  (nnz eq 1) or (spt eq -4) then begin
					if spm eq  6 then spm=0
					ij=1
				   endif else begin
					if (((spt eq -1) or (spt eq 0)) and $
						(f_fg(10)))		or  $
					     (spt eq  1) then $
					      ij=sl_click(-5,fcl+fcm+1,-1,12,0)$
					else  ij=-1
				   endelse
				   if ij ne 0 then begin
				    if ij gt 0 then begin
					i=sl_vecfun(4,0,expe,ex_e,tv_xp,tv_yp)
					if nnz eq  1 then rql=  0 else rql=ij
					if spt ne -4 then spt=  1
					f_fg(21)=2
					inc=7
				    endif else begin
					i=sl_vecfun(4,0,expfl,ex_fl,tv_xp,tv_yp)
					rql=fcl  & rqm=fcl+fcm & rsl=fcl-1
					if (spt ne -1) and (spt ne 0) and $
					   (spt ne  1) then spt =  0
					inc=17   & f_fg(25)=1  & f_fg(28)=1
				    endelse
				    ros=1
				   endif
				   end
;**			Annotations.
;**			-----------
				16:begin bb=sl_funn  (0)
					 bb=sl_vecfun(-1,0)
					 bb=sl_tvpop(w,0)
					 i =sl_annot(windn)
					 bb=sl_tvpop(w,1)
                                         if i eq 0 then f_fg(35)=0 else zerr=37
				   end
;**			Turn 180°.
;**			--------
				20:begin tv_od = sl_tog(tv_od)
					 zerr=37   &  end
				else:
				endcase
				endwhile
			        bb=sl_tvmcur(2,x,y)
return
end
;
;
pro provl,	windn,erey,xsiz
;** *****	***** **** ****
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;carez + erey
			   bb=sl_tvmcur(2,xu,yu)
			   if vin  ge 0 then j=vin  else begin
			    if zerr eq 6 then j=sl_tvmenul(0,0,expy,ex_y,-2.,-2.)
			    if zerr eq 7 then j=sl_tvmenul(0,0,expd,ex_d,-2.,-2.)
			    if zerr eq 8 then j=sl_tvmenul(0,0,expp,ex_d,-2.,-2.)
			    if zerr eq 9 then j=sl_tvmenul(0,0,expo,ex_o,-2.,-2.)
			   endelse
			    xd=zerr-6
			    zerr=1 &  vin= -1
			   if j ge 0 then begin
				clfc (0)=sl_tvmerr(0) * 2
				if clfc(0) eq 4 then clfc(0)=16
				clfc (1)= f_fg(22)
				bb=sl_working(inc,ex_p2,1,explp)
;**				---- ------
;**				Save before
;**				---- ------
				if (f_fg(17)) then $
				 if exy(j,0,xd) or ((exy(j,1,xd)) and (dif)) $
				   then bb=sl_savarea(1,erey,w_num,vsiz,windn)
;**				---  ------
;**				Cut before (care if zerr=38)
;**				--- ------
				if (exy(j,1,xd)) and (dif) then   begin
				  bb=sl_d_p(1,erey,vsiz,dif)
				  fdd= 1
				  dif= 0
				  cc = 0  & lc=0 & fcl=0
				  if     vsiz(0) ge 3 then nz=vsiz(3) else nz=1
				  fcg(0)=vsiz(1) & fcg(1)=vsiz(2) & fcg(2)=nz
				  xsiz(0)=vsiz(*)
				  zerr=35 & endif
;**				---  -----
;**				D_P  entry
;**				---  -----
				if (exy(j,2,xd) gt 0) and (exy(j,3,xd) eq 0) $
				   then  begin  bb=sl_d_p(exy(j,2,xd),$
				   		   erey,vsiz,dif,clfc,rvl,rvm)
					 if bb eq 0 then f_fg(6)=0
				   	 zerr=39  & endif
;**
			case exy(j,3,xd) of
;**
;**			Previous data.
;**			-------- ----
			1:	begin
				   bb=sl_resarea(1,erey,vsiz,1,w_num)
				   if bb eq 1 then begin
				     rvm=rvl &   zerr=37
				     fdp=1
				     if  (vsiz(0) ge 3) then nz=vsiz(3) else nz=1
				     if  (vsiz(1) ne tv_win(21,windn)) or    $
					 (vsiz(2) ne tv_win(22,windn)) or    $
					 (nz      ne tv_win(23,windn)) then  $
					  zerr=35 $
				     else begin
					  i=sl_tvmenu(0,0,expm,ex_m,-2.,-2.)
					  if i lt 0 then i=0
					  f_fg(6)=i
				     endelse
				   endif
				end
;**			Reduce.
;**			------
			2:	begin f_fg(6)=0 & zerr=35 & end
;**			Mean	filter.
;**			Median  filter.
;**			Unsharp masking.
;**			------- -------
			3:	begin
				i  =  vsiz(1)/ 3
				if (vsiz(2) lt 5) and $
				  ((spt ne 2) or (nnz gt 1)) then i=0
				if i  gt 2 then i=sl_click  (0,i ,3,10,0)
				if i  gt 2 then begin
				   if f_fg(17)  then  $
				   bb=sl_savarea(1,erey,w_num,vsiz,windn)
				   if spt eq 2 then clfc(0)=1 else   clfc(0)=0
				   bb=sl_d_p(exy(j,2,xd),erey,vsiz,i,clfc,rvl,rvm)
				   zerr=39 & endif & end
;**			Rescale.
;**			-------
			4:	if s_out eq 0  then begin  fmx =fmt
			        bb=sl_sti(explr, sl_str(vf,fmx),18)
			        bb=sl_sti(explr, sl_str(vm,fmx),33)
				explm(1)= explr
				i	= sl_vecfun(4,0,exps,ex_s2,tv_xp,tv_yp)
				rvl	= vf & rvm = vm
				fldat	= 1
				f_fg(48)= 0
		                inc=3 	&  endif
;**			Correlate.
;**			---------
			5:	if nnz gt 1 then   begin
				   bb=sl_d_p(exy(j,2,xd),erey,vsiz,dif)
				   spm=5
				   sl_manycol,108
				   zerr=38 & end
;**			Frequencies or densities
;**			-----------    ---------
			6: if (ccl*lcl gt 0) and (f_fg(16) ne 12) then begin
			    if (vsiz(1) le plx) or (vsiz(2) le ply) $
				  then begin clfc(0)=plx     & clfc(1)=ply
			    endif else begin clfc(0)=vsiz(1) & clfc(1)=vsiz(2)
			    endelse
			    if nnz gt 1 then begin
				   bb=sl_psizm(ares,ares_z,3,clfc(0),clfc(1),$
							   nnz,4,-1)
				   bb=sl_psiz(csiz,2,vsiz(1),vsiz(2),typ,-1,-1)
				   for  k=0,nnz-1 do begin
					bb=sl_pp(0,erey(*,*,k),csiz,arei,arei_z)
				        bb=sl_surf(0,arei,vsiz(1),vsiz(2),1,$
					    typ,clfc(0),clfc(1),rvl,rvm,-1,90,$
					    f_fg(15),f_fg(16),f_fg(20),f_fg(12))
					ares(0,0,k)=arei
					endfor
				   bb=sl_dd(2,arei,arei_z)
			    endif else begin
					bb=sl_pp(0,erey,vsiz,ares,ares_z)
				        bb=sl_surf(0,ares,vsiz(1),vsiz(2),1,$
					    typ,clfc(0),clfc(1),rvl,rvm,-1,90,$
					    f_fg(15),f_fg(16),f_fg(20),f_fg(12))
					bb=sl_psiz(ares_z,2,clfc(0),$
							    clfc(1),4,-1,-1)
			    endelse
			    spt=0 & zerr=38
			   endif
;			6:	begin
;				     bb=sl_d_p(exy(j,2,xd),erey,vsiz)
;				     if clfc(0) eq 8  then begin
;				        bb=sl_d_p(16 ,ares,ares_z,1)
;;				        bb=sl_d_p(30 ,ares,ares_z,0,0,0,0)
;					f_fg(0)=1
;				     endif else $
;				     if clfc(0) eq 16 then begin
;				        bb=sl_d_p(17 ,ares,ares_z,1)
;				     endif else begin
;				        bb=sl_d_p(18 ,ares,ares_z,1) & endelse
;				     sl_manycol,104
;				     zerr=38  &  end
;**			Menu math.
;**			---- ----
			7:	zerr=8
;**			Make  one frame
;**			X Y mixed salad.
;**			---------------
			8:	if nnz gt 1 then begin
				     bb=sl_d_p(exy(j,2,xd),erey,vsiz)
				     spm = 9
				     zerr=38  & endif
;**			Transpose X Y.
;**			-------------
			9:	begin
				     bb=sl_d_p(exy(j,2,xd),erey,vsiz)
				     spm = 9
				     zerr=38  & end
;**			sum over frames.
;**			X   projections
;**			Y   projections
;**			--- ---- ------
			10:	if vsiz(10)*vsiz(11) gt 0 then begin
				   spm =exy(j,2,xd)
				   if vsiz(12) eq 0 then spt=2
				   bb=sl_d_p(spm,erey,vsiz,dif)
				   zerr=38
				   if spm eq 43 then spm=2 else $
				   if spm eq 44 then spm=3 else $
				   if spm eq 26 then spm=8
				endif
;**			Menu miscela.
;**			---- -------
			11:	zerr=7
;**			Stand Dev.
;**			----- ---
			12:	if nnz gt 1 then begin
				      zerr=38  & spm=7
				      bb=sl_d_p(exy(j,2,xd),erey,vsiz)
				      sl_manycol,107 & endif
;**			Rebinage.
;**			--------
			13:	begin clfc(0)=vsiz(1)*2 & clfc(1)=vsiz(2)*2
				if vsiz(1) gt 2 then begin
				  if clfc(0) lt f_wx then clfc(0)=f_wx
				  if clfc(0) lt  plx then clfc(0)= plx
				  clfc(0)=sl_click(2,clfc(0),vsiz(1),4,2)
				  endif
				if vsiz(2) gt 2 then begin
				  if clfc(1) lt f_wx then clfc(1)=f_wx
				  if clfc(1) lt  ply then clfc(1)= ply
				  clfc(1)=sl_click(2,clfc(1),vsiz(2),5,2)
				  endif else clfc(1)=1
				if (clfc(0) ne vsiz(1)) or (clfc(1) ne vsiz(2)) $
				then begin
				     bb=sl_d_p(exy(j,2,xd),erey,vsiz,dif,clfc)
				     zerr=35 & fdd=1 & endif
				end
;**			sum each frames.
;**			--- ---- ------
			14:	if vsiz(12) gt 0 then begin
				bb=sl_d_p(exy(j,2,xd),erey,vsiz,dif)
				zerr=38 & spm=1 & spt=2 & endif
;**			Shift.
;**			-----
			15:	begin	rql=-1  & rqm=-1 & rsl=-1 & rsm=-1
					explm(1)= explo3
					i=sl_vecfun(4,0,expo3,ex_o3,tv_xp,tv_yp)
					inc=8  & end
;**			Subtract.
;**			--------
			16:	if nnz gt 1 then begin
					rql=-1  & rqm=-1
					explm(1)= explo1
					i=sl_vecfun(4,0,expo1,ex_o1,tv_xp,tv_yp)
					inc=11  & end
;**			Add.
;**			---
			17:	if nnz gt 1 then begin
					rql=-1  & rqm=-1
					explm(1)= explo1
					i=sl_vecfun(4,0,expo2,ex_o2,tv_xp,tv_yp)
					inc=12  & end
;**			line up X direct.
;**			---- -- - ------
			18:	if nnz gt 1 then begin
				bb=sl_d_p(43,erey,vsiz,dif)
				i  =0
				bb =sl_psiz(csiz,1,ares_z(1),ares_z(3),-1,-1,-1)
				for k=0,nnz-1 do begin
						 vd=sl_maxf(ares(*,k),csiz,bb)
						 i =bb+i
						 arev(k,4)=bb  & endfor
				i=i/nnz
				for k=0,nnz-1 do  if i-arev(k,4) ne 0 then begin
				 clfc(1)=0 & clfc(0)=i-arev(k,4)  & clfc(2)=k+fp
				 bb=sl_d_p(42,erey,vsiz,dif,clfc) & endif
				bb=sl_dd(2,ares ,ares_z)
				zerr=39 & endif
;**			line up Y direct.
;**			---- -- - ------
			19:	if nnz gt 1 then begin
				bb=sl_d_p(44,erey,vsiz,dif)
				i  =0
				bb =sl_psiz(csiz,1,ares_z(1),ares_z(3),-1,-1,-1)
				for k=0,nnz-1 do begin
						 vd=sl_maxf(ares(*,k),csiz,bb)
						 i =bb+i
						 arev(k,4)=bb  & endfor
				i=i/nnz
				for k=0,nnz-1 do if  i-arev(k,4) ne 0 then begin
				 clfc(0)=0 & clfc(1)=i-arev(k,4)  & clfc(2)=k+fp
				 bb=sl_d_p(42,erey,vsiz,dif,clfc) & endif
				bb=sl_dd(2,ares ,ares_z)
				zerr=39 & endif
;**			Joint a frame
;**			----- - -----
			20:	begin clfc(0)=0  &  clfc(1)=0
				      clfc(2)=1  &  fdd=1
				     bb=sl_d_p(exy(j,2,xd),erey,vsiz,dif,clfc)
				     erey(0,0,nz)=erey(*,*,nfp)
				     nz=nz+1  & nnz=nz & zerr=35 & end
;**			Taking values.
;**			------ ------
			21:	begin
				f_fg(34)=0 & f_fg(23)=10
				f_fg(24)=2 & f_fg(25)=0 & f_vu=3
				rql=long(1)  & rqm=long(100)
				bb =sl_psizm(arel,arel_z,2,6,rqm,8,-1,-1)
				arel(0,0)=0
				i  =cp+ccl/2 +1
			        bb =sl_sti(ex_p4, sl_str(i,i4),19)
				arel(1,0)=i
				i  =lp+lcl/2 +1
			        bb =sl_sti(ex_p4, sl_str(i,i4),25)
				arel(2,0)=i
				if ccl le lcl then i=ccl/2+1 $
					      else i=lcl/2+1
			        bb =sl_sti(ex_p4, sl_str(i,i4),38)
				arel(3,0)=i
				arel(4,0)=0
				arel(5,0)=0
				vtm=1
				bb =sl_sti(ex_p3 ,'    0'     ,0 )
				bb =sl_sti(ex_p3 ,'         ' ,6 )
				explm(0)=  ex_p4 & ot=1
				explm(1)=  ex_p3
				i  =sl_vecfun(4,0,expw,ex_w,tv_xp,tv_yp)
				inc=13  &  end
;**			Scrolling filter.
;**			--------- ------
			22:  if ccl*lcl gt 0 then begin
				f_fg(23)=10
				f_fg(27)=0
				vtm=0.
				bb=sl_gf(float(vm),1,0,fma)
				i	= sl_vecfun(4,0,exsf,ex_sf,tv_xp,tv_yp)
				f_vu	= 3
				if (spt ne 0) and (spt ne 2) then begin
					    explm(0) = explb
					    spt=0 & vik=15 & zerr=37
				endif else  inc=15
			     endif
;**			Fit functions.
;**			--- ---------
			23:  begin
				f_fg(23)=10     & f_fg(25)=0
				rsl=3
				bb=sl_sti(expfi,'X',10)
				bb=sl_sti(expfi,sl_str(rsl,i3),26)
				explm(1)= expfi
				i	= sl_vecfun(4,0,exfi,ex_fi,tv_xp,tv_yp)
				inc=16
			     end
;**			invers a frame.
;**			------ - -----
			24:  begin
				if nz gt 1 then $
				     clfc(0)=sl_click(-5,nz,nfp+1,12,0) $
				else clfc(0)=1
				if   clfc(0) ne 0 then begin
				     bb=sl_d_p(exy(j,2,xd),erey,vsiz,dif,clfc)
				     zerr=39 & endif
			     end
;**			Frequency filter.
;**			--------- ------
			25:  begin
				f_fg(30)=3
				rql	= typ
				bb=sl_pp(0, erey,vsiz,areo,areo_z)
				if f_fg(34) ne 1 then $
				   bb=sl_d_p(exy(j,2,xd),erey,vsiz)
				bb=sl_d_p(16	     ,erey,vsiz,0)
;				bb=sl_d_p(30	     ,erey,vsiz,0,0,rvl,rvm)
				f_fg(0)=1
				if (spt ne 0) and (spt ne 2) then spt=0
				explm(0)  = explb
				vik=18+50 & zerr=39
			     end
;**			Magnify and rot.
;**			------- --- ---
			26:  if ccl*lcl gt 0 then begin
				f_fg(24)= 1  & f_fg(23)=10  & f_fg(27)=1
				bxy(6)  = (ccl+1)/2-1+cp
				bxy(7)  = (lcl+1)/2-1+lp
				bxy(8)  =  -1
				rqm=bxy(6) & rql=bxy(7) & vta=0. & vtm=1.
				clfc(0)=-1 & clfc(1)=0  & clfc(2)=0
				rsm=-1	   & rsl=0
			        bb=sl_sti(explo2, sl_str(rqm+1,i4), 5)
			        bb=sl_sti(explo2, sl_str(rql+1,i4),11)
			        bb=sl_sti(explo2, ' 1.00    ',20)
			        bb=sl_sti(explo2, ' 0.00    ',34)
				explm(1)= explo2
				i=sl_vecfun(4,0,exprs,ex_rs,tv_xp,tv_yp)
				if f_fg(13) eq 0  then begin
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
                                        ot=2 &   endif
				inc=19
			     endif
;**			Elliptical adjustement.
;**			---------- -----------
			27:  if ccl*lcl gt 0 then begin
				f_fg(24)=2
				rql=long(1)  & rqm=long(100)
				bb =sl_psizm(arel,arel_z,2,9,rqm,8,-1,-1)
				i  =vsiz(1)/2
			        bb =sl_sti(ex_p6, sl_str(i,i4),30)
				arel(1,0)=i
				i  =vsiz(2)/2
			        bb =sl_sti(ex_p6, sl_str(i,i4),36)
				arel(2,0)=i
				arel(4,0)=1
				rsl=long(1)
				vtm=long(1)
				bb =sl_sti(ex_p3 ,'    0'     ,0 )
				bb =sl_sti(ex_p3 ,'         ' ,6 )
				explm(0)=  ex_p6 & ot=1
				explm(1)=  ex_p3
				i=sl_vecfun(4,0,exadj,ex_ad,tv_xp,tv_yp)
				f_vu  = 3
;				if f_fg(31) ne 1 then $
;					bb=sl_trsig(zerr,inc,vin,mfi,ot,98)
				if f_fg(13) eq 0 then begin
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
                                        endif
				inc=21
			     endif
;**			Distribution curve
;**			------------ -----
			28:  if vsiz(10)*vsiz(11) gt 0 then begin
				clfc(0)=nf
				bb=sl_d_p(exy(j,2,xd),erey,vsiz,dif,clfc,rvl,rvm)
				zerr=38 & spt=2 & spm=4
			     endif
;**			Radial integration
;**			------ -----------
			29:  if ccl*lcl gt 0 then begin
				if f_fg(13) eq 0  then $
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
				ot=1
				f_fg(23)=0
				f_fg(24)=1
				if ccl le lcl then i=lcl+1 else i=ccl+1
				bb =sl_psizm(arel,arel_z,2,5,i*3/2,8,-1,-1)
				arel(0,0)=0
				i  =cp+ccl/2 +1
			        bb =sl_sti(ex_p7, sl_str(i,i4),14)
				arel(1,0)=i-1
				bxy(6)   =i-1
				i  =lp+lcl/2 +1
			        bb =sl_sti(ex_p7, sl_str(i,i4),19)
				arel(2,0)=i-1
				bxy(7)   =i-1
				bxy(8)   = -1
				if ccl le lcl then i=ccl/2 else i=lcl/2
				rsl=1
				rsm=i
			        bb =sl_sti(ex_p7, sl_str(rsl  ,i4),31)
			        bb =sl_sti(ex_p7, sl_str(rsm+1,i4),39)
				vtm=0.
				arel(3,0)=0.
				arel(4,0)=360.
				bb =sl_sti(ex_p8 ,'    0'     ,0 )
				bb =sl_sti(ex_p8 ,'         ' ,6 )
				bb =sl_sti(ex_p8 ,'    '      ,19)
				bb =sl_sti(ex_p8 ,'  0'       ,32)
				bb =sl_sti(ex_p8 ,'360'       ,40)
				explm(0)=  ex_p7
				explm(1)=  ex_p8
				i  =sl_vecfun(4,0,exrad,ex_ra,tv_xp,tv_yp)
				inc=22
			     endif
			else:
			endcase
;**
			if ot ne 1 then bb=sl_working(inc,ex_p2,1,explb)
		   endif
		   bb=sl_tvmcur(2,x,y)
return
end
;
;
pro provs, erey
;** *****
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
;**			Special view.
;**                     ------- ----
;**			Flick....
			if (spt eq 2) or (inc eq 17) then s_out=0
;**			Surface indices..........
			if ((spt eq 1) or (spt eq -4)) and (not ros) then begin
			    if s_out   eq 0   then begin
			      if tv_od eq 0   then y1=ply-y1-1
			      if (spt  eq -4) then $
				  if (f_fg(14) eq 6) then s_out=3 else nf=fcm
			      bb=sl_surf(-2,0,ccl,lcl,1,0,x1,y1,c,l)
			      endif else  $
			    if s_out   eq 2   then $
			      if (f_az gt 90) and (f_az lt 270) then c=ccl-c
			    if s_out   eq 1   then begin
			      if (f_ax gt 90) and (f_ax lt 270) then i=1 else i=0
			      if (f_az gt 90) and (f_az lt 270) then j=1 else j=0
			      if (i-j   ne 0) then l=lcl-l   &  endif
;
;**			Proj.3D indices..........
			endif else if spt eq -3 then begin
			   if (l le lcl) and  (s_out eq 1) then begin
				   s_out=5 &   c=ccl & nf=fcm & endif     else  $
			   if (c le ccl) and ((s_out eq 2) or (l le lcl)) then  $
				if s_out  eq  2 then begin l=lcl & nf=fcm
				   s_out=6 &   endif else  s_out=3 else $
			   if (c gt ccl) and ((s_out eq 2) or (l lt lcl+c-ccl)) $
				then     begin nf=c-ccl-1   &  l=l-nf & c=ccl
				if s_out  eq  2 then begin
				   s_out=4 & l=lcl & endif   else $
				if s_out  eq  0 then s_out=1 else c=-1
			   endif   else begin	nf=l-lcl-1 & c=c-nf & l=lcl
				   s_out=2  &   endelse
			   if (c lt 0  ) or (l lt 0  ) or (nf lt 0  ) or $
			      (c gt ccl) or (l gt lcl) or (nf gt fcm) then begin
				c=xs  & l=ys & nf=zs  & endif
			endif
;**			Spacial indices..........
			if  spt eq -2  then begin
			    clfc(0)=c & clfc(1)=l & clfc(2)=nf
			    res(0) = sl_spacial(erey,0,clfc,vsiz,-1)
			    c	   = res(0)
			    l	   = res(1)
			    nf	   = res(2)
;**			Reduced indices..........
;			endif else if (fqc)  then begin
;max			    clfc(0)=c & clfc(1)=l & clfc(2)=nf
;			    res(0) = sl_xred(erey , clfc,vsiz,fct,cpx)
;			    c	   = res(0)
;			    l	   = res(1)
			endif
return
end
;
;
pro provz,	windn,erey,rvl,rvm
;** *****	***** **** *** ***
common my_refl, arefl,arefl_z,rf_cur,rf_t
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
	common  my_insert,  i_txt,i_idx,i_fil,i_rout,i_ps,i_rs,$
			    i_trout,i_tfil,i_tlang,i_tdx,i_enter,i_rcall
;**
;carez + erey
;**	Common interactive
;**	------ -----------
			if f_fg(42) eq 2 then begin
;**			External call
;**			-------- ----
			   f_fg(42)=0
			   bb=0
			   if i_rcall ge 10 then begin
			      i=i_rcall-10
			      i=i+2
			      if i lt i_tdx then begin
				 i_rout= sl_stx (i_trout(i),0,25)
				 i_rout= sl_stbr(i_rout,2)
				 i_ps(0)=ccp+1 & i_ps(1)=llp+1 & i_ps(2)=nfp+1
				 j=typ
				 if typ eq 2  then j=1 else $
				 if typ eq 4  then j=2 else $
				 if typ eq 16 then j=3 else $
				 if typ eq 8  then j=4 else $
				 if typ eq 32 then j=5 else $
				 if typ eq 64 then j=6
				 ij=0
				 if (i_tlang(i) eq 1) or $
				    (i_tlang(i) eq 2) then begin
				    bb=sl_execute(ij,i_rout,erey, $
					  vsiz(1),vsiz(2),nz,i_ps,j)
				 endif else $
				 if (i_tlang(i) eq 3) then begin
				    bb=sl_callt('LIB$SET_LOGICAL','LIBRTL',$
						'SCAN_FUNC',i_tfil(i))
				    ij=sl_calll(i_rout,'SCAN_FUNC',erey, $
					  vsiz(1),vsiz(2),nz,i_ps,j)
				 endif
			      endif
			   endif
;**			   Update
			   if bb eq 0 then i_rcall=0 $
			   else if ij eq 1 then f_fg(35)=3
			endif
;**
			if f_fg(40) gt 0 then begin
;**			  Rescale
;**			  -------
			  if f_fg(40) eq 1 then if vo lt rvm then rvl=vo
			  if f_fg(40) eq 2 then if vo gt rvl then rvm=vo
			  if f_fg(40) eq 3 then begin
				if  f_fg(41) ne 1 then begin
				    i =(vm-vf)/tv_nc/4 + 2
				    rvm=vo+(vm-vo)/i/4
				    rvl=vo-(vo-vf)/i
				    endif
				f_fg(35)=3
			  endif
			  f_fg(40)=0
			  f_fg(41)=1
			endif
			if   f_fg(39) ne 0 then begin
;**			  1:position 2:get file 3:next pos
;**			    --------   --------   --------
			  if f_fg(39) eq 2 then begin
				bb=sl_getrflx(arefl,arefl_z,-1,ttl)
				bb=sl_tvmcur(2,x,y)
			 	inc=inc+50 & zerr=1
				rf_cur=0
			  endif else $
			  if f_fg(39) eq 1 then begin
;			    Choice position.
				if arefl_z(2) gt 0 then begin
				 i =sl_click(1,arefl_z(2)-1,rf_cur,13,-1)
				 if i ne rf_cur then f_fg(39)=4
				 rf_cur=i
				 bb=sl_tvmcur(2,x,y)
				endif
			  endif else $
			  if f_fg(39) eq 3 then begin
;			    Next position.
				if arefl_z(2) gt 0 then begin
				 rf_cur=rf_cur+1
				 if rf_cur lt arefl_z(2) then f_fg(39)=4 $
							 else rf_cur  =0
			  endif  &  endif
			  if f_fg(39) eq 4 then begin
;			    Position.
				if arefl_z(1) eq 9 then rf_t=1 else rf_t=2
				sl_pflex, arefl,rf_cur ,rf_t
			  endif
			  f_fg(39)=0
			endif
;
			if   f_fg(35) ne 0  then begin
;**			  1:expand 2,6:zoom 3:update 4:change graphic 5:pan
;**			  -------- -------- -------- -------- ------- -----
;**			  7:Size box
;**			  ----------
			  if f_fg(35) eq 2  then begin
				 if fcx/fcy lt 2  then begin
;**				 Zoom X
				   x1=c - (ccl+1)/4 & if x1 lt 0 then x1=0
				   xd=x1+ (ccl+1)/2 - 1
				   if xd gt ccl   then begin
						  xd=ccl
						  x1=ccl-(ccl+1)/2 +1 & endif
				   if xd-x1 ge 15 then begin
						  cc=cc+x1
						  fcg(0)=xd-x1+1      & endif
				 endif
				 if fcy/fcx lt 2 then begin
;**				 Zoom Y
				   y1=l - (lcl+1)/4 & if y1 lt 0 then y1=0
				   yd=y1+ (lcl+1)/2 - 1
				   if yd gt lcl  then begin
						 yd=lcl
						 y1=lcl-(lcl+1)/2 +1  & endif
				   if yd-y1 ge 15 then begin
						  lc=lc+y1
						  fcg(1)=yd-y1+1      & endif
				 endif
			  endif else if (f_fg(35) eq 6) or $
					(f_fg(35) eq 7) then begin
				 if (f_fg(37) ne 0) then begin
				  if (nf eq zq) then begin
				   if (c ne xq) and (l ne zq) then begin
				    x1=xq &  xd=c
				    y1=yq &  yd=l
				    if xq gt c then begin x1=c & xd=xq & endif
				    if yq gt l then begin y1=l & yd=yq & endif
				    if f_fg(35) eq 6 then begin
				       cc=cc+x1
				       lc=lc+y1
				       fcg(0)  =xd-x1+1
				       fcg(1)  =yd-y1+1
				       f_fg(35)=2
				    endif else begin
				       xd=xd-x1+1
				       yd=yd-y1+1
				       if xd ge yd then f_fg(1)=xd else f_fg(1)=yd
				       f_fg(2 )=f_fg(1)*f_wy/f_wp
				       if f_fg(2) lt 2 then f_fg(2)=2
				       f_fg(35)=0
				    endelse
				   endif else f_fg(35)=0
				  endif else if f_fg(35) eq 6 then begin
				    x1=zq &  xd=nf
				    if zq gt nf then begin x1=nf & xd=zq & endif
				    fcl= fcl +x1
				    fcg(2)=xd-x1+1
				    f_fg(35)=2
				  endif else f_fg(35)=0
				 endif  else f_fg(35)=0
			  endif else if (f_fg(35) eq 1) then begin
				 if dif then begin
				   if ((ccl+1)*4 le vsiz(1)) or $
				      ((lcl+1)*4 le vsiz(2)) then begin
;**					Un-zoom X
					x1=ccp- (ccl+1)  & if x1 lt 0 then x1=0
					xd=x1 + (ccl+1) *2 -1
					if xd gt vsiz(1)-1  then begin
						  xd=vsiz(1)-1
						  x1=xd-(ccl+1)*2 +1
						  if x1 lt 0 then x1=0  & endif
;**					Un-zoom y
					y1=llp- (lcl+1)  & if y1 lt 0 then y1=0
					yd=y1 + (lcl+1) *2 -1
					if yd gt vsiz(2)-1  then begin
						  yd=vsiz(2)-1
						  y1=yd-(lcl+1)*2 +1
						  if y1 lt 0 then y1=0  & endif
;
					cc=x1		& lc=y1
					fcg(0)=xd-x1+1	& fcg(1)=yd-y1+1
					tv_win(19,windn)=0
				   endif else begin
					cc= 0		& lc= 0
					fcg(0)=vsiz(1)	& fcg(1)=vsiz(2)
				   endelse
				 endif else f_fg(35)=0
			  endif
			  if f_fg(35) ne 0 then begin
;				if (f_fg(35) le 2) or (f_fg(35) eq 5) then begin
				if (f_fg(35) le 2)                    then begin
				   if inc ne 3 then $
				    if (rvl eq vf) and (rvm eq vm) then rvm=rvl
				   fdp=1    & endif
				if inc eq 19     then f_fg(24)=0
				if inc eq 7      then f_fg(21)=2
				if f_fg(24) eq 1 then f_fg(29)=1
				vik=inc & zerr=37
				if f_fg(35) ne 5 then begin   ired(0)=c+cp
					ired(1)=l+lp & ired(2)=nf+fp & endif
				f_fg(35)=0
			  endif
			endif
;**
			if   f_fg(29) ne 0 then begin
			  if f_fg(29) eq 1 then if f_fg(24) eq 1 then begin
;				Set pivot.
;				--- -----
				if f_fg(13) eq 0  then begin
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
                                        ot=2 &   endif
				bxy(6)=ccp & bxy(7)=llp
				f_fg(26)=0  & endif
			  if f_fg(29) eq 2 then if f_fg(24) eq 1 then begin
;				Fix slice.
;				--- -----
				   bxy(8) =bxy(6) &  bxy(9) =bxy(7)
				   bxy(10)=ccp    &  bxy(11)=llp
				   sl_box,3    &  endif
			  f_fg(29)=0 & endif
;**
			if f_fg(36) ne 0 then begin
			 if (f_vu le 3) and (lcl gt 0) then begin
;			 Position ellips
;			 -------- ------
;common			  bb=sl_working(inc,ex_p2,1,explp) & ot=1
			  f_cn=ccp
			  f_ln=llp
			  f_zn=nfp
			  bb=sl_tvshap(50)
			  if inc eq 21 then bb=sl_ellipos(erey,vsiz,f_cn,f_ln,$
				  nfp,f_fg,f_el,arel(4,0),arel(1,0),arel(2,0))$
			  else bb=sl_ellipos(erey,vsiz,f_cn,f_ln,nfp,f_fg,f_el,0)
			  bb=sl_tvshap(-1)
			  f_fg(32)=2 & endif
			 f_fg(36)=0  & endif
;**	Zoommm
;**	------
			if zerr eq 2  then   begin
				if (nnz eq 1) then begin
;**			Zoom frame.
;**			---- -----
				   xd = ccl
				   yd = lcl
				   if xd gt yd then j=yd+1 else j=xd+1
				   j  =((xd+1)*(yd+1))/(j*j)
;**				   Square off.
;**				   ------ ---
				   x1 =(xd+1)*2/((yd+1)*j) & if x1 lt 1 then x1=1
				   y1 =(yd+1)*2/((xd+1)*j) & if y1 lt 1 then y1=1
;**				   Limits.
;**				   ------
				   i  = c - xd/3/x1 & if  i lt  0 then i = 0
				   x1 = c + xd/3/x1 & if x1 gt xd then x1=xd
				   j  = l - yd/3/y1 & if  j lt  0 then j = 0
				   y1 = l + yd/3/y1 & if y1 gt yd then y1=yd
;**				   Keep 16 points.
;**				   ---- -- ------
				   if  xd lt 16  then  begin
						 i  =  0 & x1 = xd
						 xd =  1 & endif
				   if  yd lt 16  then  begin
						 j  =  0 & y1 = yd
						 yd =  1 & endif
;**				   Zoom.
;**				   ----
				   if  xd * yd gt 1  then  begin
				    cc  = cc +  i
				    lc  = lc +  j
;as 				    erey(i +cp:x1+cp,j +lp:y1+lp,nf+fp)  &  nz=1
				    fcg(0)=x1-i+1  &  fcg(1)=y1-j+1  &  fcg(2)=1
				    if (rvl eq vf) and (rvm eq vm) then rvm=rvl
				   endif else zerr=1
				endif else begin
;**			Scan reduction.
;**			---- ---------
;as				erey(cp:ccl+cp, lp:lcl+lp, rsl+fp:rsm+fp)
				fcg(0) =ccl+1 & fcg(1)=lcl+1 & fcg(2)=rsm-rsl+1
				fcl    = fcl  + rsl
				if (rvl eq vf) and (rvm eq vm) then rvm=rvl
				if  ( rsl eq rsm) then begin windn=-windn-1
;				   if sdt  then   begin
					   vsiz(9)=fcl & vsiz(12)=0
					   bb=sl_d_p(15,erey,vsiz,1)
					   fcl=0 & zerr=38
;					   endif
				   endif
				endelse
;**	 Zoommm  continue.
;**	 ------  --------
			endif  else if  zerr ge 35 then $
			if (zerr eq 35) then begin
;**			Reset.
;**			-----
				fcg(0)=vsiz(1) & fcg(1)=vsiz(2)
				if (nz gt 1) and (nnz  eq 1) and (w gt 0) then $
				    fcg(2)=1 else fcg(2)=nz
;as				    erey(*,*,fcl)
				endif else $
			if (zerr eq 52) then begin
;**			Reduce frames V.
;**			------ ------ -
				zerr=2
;as				erey(cp:ccl+cp, rsl+lp:rsm+lp, fp:fcm+fp)
				fcg(0)= ccl+1 & fcg(1)=rsm-rsl+1 & fcg(2)=fcm+1
				lc    = lc +rsl
				endif   else  $
			if (zerr eq 62) then begin
;**			Reduce frames H.
;**			------ ------ -
				zerr=2
;as				erey(rsl+cp:rsm+cp, lp:lcl+lp, fp:fcm+fp)
				fcg(0)= rsm-rsl+1 & fcg(1)=lcl+1 & fcg(2)=fcm+1
				cc    = cc +rsl
				endif   else  $
			if (zerr eq 92) then begin
;**			Expand a region.
;**			------ - ------
				zerr=2
;as				erey(rql:rql+rsl, rqm:rqm+rsm, fp:fp+fcm)
				fcg(0) = rsl+1  & fcg(1) =rsm+1 & fcg(2)=fcm+1
				cc     = rql    & lc     =rqm
				endif
return
end
;
;
pro provit,	erey,windn,xsiz
;** ******
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
			if zerr ge 10 then begin
			    if zerr eq 10  then   begin
;**			stretching (down).
;**			----------  ----
				if vd ne vo then begin
					i  =vo*tv_nc/vm
;					j  =tv_nc/5*4 & if i gt j then i=j
					sl_colexp,i,tv_nc-1
					vd = vo & endif
			        endif else  $
			    if zerr eq 20  then   begin
;**			stretching (up).
;**			----------  --
;				if vd ne vo then begin
;					i  =vo*tv_nc/vm
;					j  =tv_nc/5   & if i lt j then i=j
;					sl_colexp,0,i
;					vd = vo & endif
				if f_fg(52) eq 1 then begin
					i  =fix( float(x)/bxy(12) * tv_nc )
					j  =fix( float(y)/bxy(13) * tv_nc )
					sl_colexp,j,i
				endif
			        endif else  $
			    if zerr eq 34  then   begin
;**			Rescaling.
;**			---------
				if (f_fg(48) eq 0) and (vm-vf ge 2) and  $
					(vm-vf le long(2)^30)       and  $
					(vm    le long(2)^30) then begin $
				    if rvl ne vf then vo=rvl
				    vo=vo-vo+sl_click(long(vf),long(vm), $
						      long(vo),14,0)
				    if vo lt rvm then rvl=vo
;				    bb=sl_tvmcur(2,x,y)
				endif else $
				if (s_out eq 0) and (vo lt rvm) then rvl=vo
;
			        bb=sl_sti(explr, sl_str(rvl,fmt),18)
				explm(1) = explr &  ot=1 &  f_fg(48)=0
			        endif else  $
			    if zerr eq 31  then   begin
				if (f_fg(48) eq 0) and (vm-vf ge 2) and  $
					(vm-vf le long(2)^30)       and  $
					(vm    le long(2)^30) then begin $
				    if rvm ne vm then vo=rvm
				    vo=vo-vo+sl_click(long(vf),long(vm), $
						      long(vo),15,0)
				    if vo gt rvl then rvm=vo
;				    bb=sl_tvmcur(2,x,y)
				endif else $
				if (s_out eq 0) and (vo gt rvl) then rvm=vo
;
			        bb=sl_sti(explr, sl_str(rvm,fmt),33)
				explm(1) = explr &  ot=1 &  f_fg(48)=0
			        endif else  $
			    if zerr eq 30  then   begin & endif else $
			    if zerr eq 32  then   begin
				bb=sl_sti(explr,'         ',18)
			        bb=sl_sti(explr,'         ',33)
				if (rvl ne vf)   or  (rvm ne vm) then begin
				    fdp=1
				    if fldat    then  begin
				        j=sl_tvmenu(0,0,expn,ex_n,-2.,-2.)
					if j lt 0 then j=3
				        if j ne 3 then if f_fg(17) then $
				        bb=sl_savarea(1,erey,w_num,vsiz,windn)
					if  rvl eq vf then i=2  else $
					if  rvm eq vm then i=1  else i=0
					bb=sl_d_p(0,erey,vsiz,(i+3*j),0,rvl,rvm)
					rvm=rvl
					fdd=1
				     endif
				endif
;**			Resetting.
;**			---------
				endif else $
			    if zerr eq 35  then   begin
				if   vsiz(0) ge 3 then nz=vsiz(3) else nz=1
				xsiz(0)=vsiz(*)
				cc =0  &  lc=0
				tv_win(19,windn)=0
				rvm=rvl
				fdp=1
				f_fg(34)=0
				if (not dif) or (nnz gt 1) or (w eq 0) then fcl=0
				dif=0
				endif else $
			    if zerr eq 37  then   begin & endif else $
			    if zerr eq 44  then   begin
;**			Reducing scan.
;**			-------- ----
				if nf le rsm then rsl  = nf
;no masun			if nf eq rsm then rsm  = fcm
;no masun			bb=sl_sti(explr, sl_str(rsm+fcl+1,i5),35)
				bb=sl_sti(explr, sl_str(rsl+fcl+1,i5),20)
				bb=sl_working(inc,entitl,2,explr)
			        endif else  $
			    if zerr eq 41  then   begin
				if nf ge rsl then rsm  = nf
;no masun			if nf eq rsl then rsl  = 0
;no masun			bb=sl_sti(explr, sl_str(rsl+fcl+1,i5),20)
				bb=sl_sti(explr, sl_str(rsm+fcl+1,i5),35)
				bb=sl_working(inc,entitl,2,explr)
			        endif else  $
			    if zerr eq 40  then   begin
;no masun			if (rsl eq rsm)   and   (nf ne rsl) then begin
;no masun			bb=sl_sti(explr, sl_str(nf+fcl+1,i5),20)
;no masun			bb=sl_sti(explr, sl_str(nf+fcl+1,i5),35)
;no masun			bb=sl_working(inc,entitl,2,explr)
;no masun			rsl = nf & rsm = nf   &  endif
				endif else $
			    if zerr eq 42  then   begin
			        zerr=2
				if (rsl eq 0) and (rsm eq fcm) then zerr=3
				endif else $
			    if zerr eq 54  then   begin
;**			Reducing frame V.
;**			-------- ----- -
				if l  lt rsm then rsl  = l
				bb=sl_sti(explr, sl_str(rsl+lc +1,i5),20)
				bb=sl_working(inc,entitl,2,explr)
			        endif else  $
			    if zerr eq 51  then   begin
				if l  gt rsl then rsm  = l
				bb=sl_sti(explr, sl_str(rsm+lc +1,i5),35)
				bb=sl_working(inc,entitl,2,explr)
			        endif else  $
			    if zerr eq 64  then   begin
;**			Reducing frame H.
;**			-------- ----- -
				if c  lt rsm then rsl  = c
				bb=sl_sti(explr, sl_str(rsl+cc +1,i5),20)
				bb=sl_working(inc,entitl,2,explr)
			        endif else  $
			    if zerr eq 61  then   begin
				if c  gt rsl then rsm  = c
				bb=sl_sti(explr, sl_str(rsm+cc +1,i5),35)
				bb=sl_working(inc,entitl,2,explr)
			        endif else  $
			    if (zerr eq 50) or (zerr eq 60)    then   begin
			        endif else  $
			    if (zerr eq 52) or (zerr eq 62)    then   begin
				if (rsl  eq 0 ) and $
				 (((zerr eq 52) and (rsm eq lcl)) or $
				  ((zerr eq 62) and (rsm eq ccl)))  then zerr=3 $
				else if (rvl eq vf) and (rvm eq vm) then rvm=rvl
			        endif else  $
			    if zerr eq 74  then   begin
;**			Image rotating.
;**			----- --------
				bb=sl_rotfun(2,w,0,0,0,0,0,0,0,f_az,f_ax)
				f_az=sl_click( 0,360,f_az,1,0)
				f_ax=sl_click( 0,360,f_ax,2,0)
				bb=sl_rotfun(3,w,0,0,0,0,0,0,0,f_az,f_ax)
			        endif else  $
			    if zerr eq 71 then begin
				bb=sl_settings(mfi,0,0,zerr,inc,66-40)
			        endif else  $
			    if zerr eq 70 then begin
				if f_fg(21) eq 2 then begin
					f_fg(11)=rql-1 & f_fg(21)=1
					zerr=37 & vik=inc
				endif else bb=sl_rotfun(1,w,f_fg(18))
			        endif else  $
			    if zerr eq 72  then   begin
				bb=sl_rotfun(2,w,0,0,0,0,0,0,0,f_az,f_ax)
				if rql gt 0 then f_fg(11)=rql-1
				zerr=37  & ros=0
			        endif else $
			    if zerr eq 84  then   begin
;**			Shifting.
;**			--------
			        bb=sl_sti(explo3, sl_str(nf+fcl+1,i4), 9)
			        bb=sl_sti(explo3, sl_str(c +cc +1,i5),18)
			        bb=sl_sti(explo3, sl_str(l +lc +1,i5),24)
				rql=llp	& rqm=ccp & rop=nfp
				bb=sl_working(inc,entitl,2,explo3)
				f_fg(24)=1 & f_fg(29)=1
			        endif else  $
			    if zerr eq 81  then   begin
			        bb=sl_sti(explo3, sl_str(c +cc +1,i5),32)
			        bb=sl_sti(explo3, sl_str(l +lc +1,i5),38)
				rsl=llp	& rsm=ccp & f_fg(29)=2
				bb=sl_working(inc,entitl,2,explo3)
			        endif else  $
			    if zerr eq 80  then   begin & endif else $
			    if zerr eq 82  then   begin
				f_fg(24)=0
				rql=rsl-rql & rqm=rsm-rqm
				if (rql ne 0) or (rqm ne 0) then begin
				    bb=sl_sti(explo3,'    '	   ,9 )
			            bb=sl_sti(explo3,'     ,     ' ,18)
			            bb=sl_sti(explo3,'     ,     ' ,32)
				    f_fg(11) = rop
				    clfc(0)=rqm & clfc(1)=rql & clfc(2)=rop
				    bb=sl_d_p(42,erey,vsiz,dif, clfc)
				    zerr=39
				endif else zerr=3
			        endif else $
			    if (zerr eq 94) or (zerr eq 154) then   begin
;**			Expanding a region.
;**			--------- - ------
				f_fg(1)=f_fg(1)/2
				f_fg(2)=f_fg(2)/2
				if f_fg(1) lt   2    then f_fg(1)=2
				if f_fg(2) lt   2    then f_fg(2)=2
				ot=2
			        endif else  $
			    if (zerr eq 91) or (zerr eq 151) then   begin
				f_fg(1)=f_fg(1)*2
				f_fg(2)=f_fg(2)*2
				if f_fg(1) gt bxy(2) then f_fg(1)=bxy(2)
				if f_fg(2) gt bxy(3) then f_fg(2)=bxy(3)
				ot=2
			        endif else  $
			    if zerr eq 90  then   begin
				if f_fg(13) eq 0  then begin
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
                                        ot=2 &   endif
				if (f_fg(1) ne rsl) or $
				   (f_fg(2) ne rsm) then begin
				   rsl=f_fg(1) & rsm=f_fg(2)
				   bb=sl_sti(explk,sl_str( rsl,i5),18)
				   bb=sl_sti(explk,sl_str( rsm,i5),27)
				   bb=sl_working(inc,entitl,2,explk)
				   endif
			        endif else  $
			    if zerr eq 92  then   begin
				bb= sl_settings(mfi,0,0,zerr,inc,50-40) & ot=2
				if f_fg(49) ge 2 then f_fg(1)=f_fg(49)
				if f_fg(50) ge 2 then f_fg(2)=f_fg(50)
				f_fg(49)=0  &  f_fg(50)=0
;				if f_fg(1) gt 31 then f_fg(1)=31
;				if f_fg(2) gt 31 then f_fg(2)=31
				if (rsl gt ccl) and (rsm gt lcl) then zerr=3 $
				else begin
					rql = c+cc - rsl/2 & rsl = rql+rsl
					rqm = l+lc - rsm/2 & rsm = rqm+rsm
					if rql lt cc then rql = cc
					if rqm lt lc then rqm = lc
					rsl = rsl - rql -1
	 				rsm = rsm - rqm -1
					if rql+rsl gt cc+ccl then rsl=cc+ccl-rql
					if rqm+rsm gt lc+lcl then rsm=lc+lcl-rqm
					rvm=rvl   &  endelse
				f_fg(27)=1
			        endif else  $
			    if (zerr eq 114) or (zerr eq 124) then  begin
;**			Adding.
;**			------
;**			Subtracting.
;**			-----------
			        bb=sl_sti(explo1, sl_str(nf+fcl+1,i5),15)
				rql=nfp
				bb=sl_working(inc,entitl,2,explo1)
			        endif else  $
			    if (zerr eq 111) or (zerr eq 121) then  begin
			        bb=sl_sti(explo1, sl_str(nf+fcl+1,i5),25)
			        bb=sl_sti(explo1, sl_str(nf+fcl+1,i5),37)
				rqm=nfp
				bb=sl_working(inc,entitl,2,explo1)
			        endif else  $
			    if (zerr eq 110) or (zerr eq 120) then  begin
							      endif else $
			    if (zerr eq 112) or (zerr eq 122) then  begin
				if (rql ge 0) and (rqm ge 0)  then  begin
				    bb=sl_sti(explo1,'     ',15)
			            bb=sl_sti(explo1,'     ',25)
			            bb=sl_sti(explo1,'     ',37)
				    f_fg(11)=rqm
				    clfc(0) =rql & clfc(1)=rqm & clfc(2)=rqm
				    if zerr  eq 112  then  $
					 bb=sl_d_p(45,erey,vsiz,dif,clfc) $
				    else bb=sl_d_p(46,erey,vsiz,dif,clfc)
				    zerr=39
				endif else zerr=3
			        endif else $
			    if ((zerr ge 130) and (zerr le 139)) or $
			       ((zerr ge 210) and (zerr le 219)) then begin
;**			Taking values.
;**			Taking signal.
;**			------ ------
				provit1,erey,windn
			        endif else $
			    if ((zerr ge 140) and (zerr le 149)) or $
			       ((zerr ge 220) and (zerr le 229)) then begin
;**			Slicing .
;**			Radial integration.
;**			------ -----------
				provit2,erey,windn
			        endif else $
			    if zerr eq 150 then   begin
;**			scrolling Filter.
;**			--------- ------
				if f_fg(13) eq 0  then begin
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
                                        ot=2 &   endif
			        endif else  $
			    if (zerr eq 152) or (zerr eq 164) then begin
;				Stop
				bb  =sl_settings(mfi,0,0,zerr,inc,50-40)
				zerr=39
			        endif else $
			    if (zerr eq 153) or (zerr eq 155) or $
			       (zerr eq 156) then begin
;				Change point
				if zerr eq 153  then vd=arev(nfp,3) else $
				if zerr eq 156  then vd=arev(nfp,5) else vd=0
				if not f then vd =sl_pfix(vd)
				if nz eq 1  then erey(ccp,llp)     =vd $
					    else erey(ccp,llp,nfp) =vd
				ot=2
			        endif else $
			    if  zerr eq 157  then begin
;				Change region (clear,avg,minim)
				if (f_ab(0,0) ge 0) then begin
				 if f_fg(23) le 3 then begin
				   if f_fg(23) eq 1 then vd=arev(nfp,3)	else $
				   if f_fg(23) eq 7 then vd=arev(nfp,5)	else $
				   if f_fg(23) eq 3 then vd=vtm $
						    else vd=0
				   if not f then vd =sl_pfix(vd)
				   if (f_fg(31) eq 1) and (f_vu le 3) then begin
				    rql=0
				    sl_ellip,2,erey,vsiz ,nfp,f_el,ccp,llp,$
					f_fg(1)-1,f_fg(2)-1, rql,rql,vd
				   endif else begin
				    for k=f_ab(0,2),f_ab(1,2) do $
				     for jj=f_ab(0,1),f_ab(1,1) do $
				      for ii=f_ab(0,0),f_ab(1,0) do begin
					   if nz eq 1 then erey(ii,jj)  =vd $
						      else erey(ii,jj,k)=vd
				    endfor
				   endelse
				 endif
;				 Subtract avg
				 if (f_fg(23) eq 5) and (vtm ne 0) then begin
				   for k=f_ab(0,2),f_ab(1,2) do $
				    for jj=f_ab(0,1),f_ab(1,1) do $
				     for ii=f_ab(0,0),f_ab(1,0) do begin
					if nz eq 1 then begin
					   vl=erey(ii,jj)
					   if vl lt vtm then erey(ii,jj)=0 $
							else erey(ii,jj)=vl-vtm
					endif else begin
					   vl=erey(ii,jj,k)
					   if vl lt vtm then erey(ii,jj,k)=0 $
							else erey(ii,jj,k)=vl-vtm
					endelse
				   endfor
				 endif
				 f_fg(23)=10 & ot=2
				endif
			        endif else $
			    if (zerr eq 158) or (zerr eq 188) then begin
;				Update image
				rvm=rvl
				zerr=37 &  vik=inc
			        endif else $
			    if  zerr eq 159  then begin
;				Store average
				vtm=arev(nfp,3)
				if not f then vtm =float(sl_pfix(vtm))
			        bb =sl_sti(ex_p5, sl_str(vtm,fma),13)
				bb = sl_working(inc,ex_p2,1,ex_p5)
			        endif else $
			    if  zerr eq 162  then begin
;**			Fitting
;**			-------
;				Update image
				if (rvl eq vf) and (rvm eq vm) then rvm=rvl
				zerr=37 & vik=inc
			        endif else $
			    if  zerr eq 160  then begin
				if f_fg(13) eq 0  then begin
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
                                        ot=2 &   endif
			        endif else $
			    if  zerr eq 161  then begin
;				Along X/Y
;				f_fg(25)=sl_tog(f_fg(25))
;				if f_fg(25) eq 0 then bb=sl_sti(expfi,'X',10) $
;						 else bb=sl_sti(expfi,'Y',10)
;				bb = sl_working(inc,ex_p2,2,expfi)
			        endif else $
			    if  zerr eq 165  then begin
				if (f_ab(0,0) ge 0) then begin
				  bb=sl_working(inc,ex_p2,1,explp)
				  bb=sl_prefit(erey,vsiz,nfp,f_ab, $
						    f_fg(23),f_fg(25),rsl)
				  explm(0)=explb & ot=1
				  endif
				f_fg(23)=10
			        endif else $
			    if  zerr eq 166  then begin
;				Degree +1
				if rsl lt 20 then rsl=rsl+1
				bb =sl_sti(expfi,sl_str(rsl,i3),26)
				bb =sl_working(inc,ex_p2,2,expfi)
			        endif else $
			    if  zerr eq 167  then begin
;				Degree -1
				if rsl gt 1  then rsl=rsl-1
				bb=sl_sti(expfi,sl_str(rsl,i3),26)
				bb =sl_working(inc,ex_p2,2,expfi)
			        endif else $
			    if  zerr eq 174  then begin
;**			Flicking.
;**			--------
;				Resize
				k =sl_resize(0)
				if k ge 0 then tv_win(19,windn)=k
			        endif else $
			    if  zerr eq 172  then begin
;				Stop
				fcg(0) =ccl+1 & fcg(1)=lcl+1 & fcg(2)=rqm-rql+1
				fcl    =rql      & ros=0
				zerr=37 & f_fg(25)=0 & f_fg(28)=0
			        endif else $
			    if  zerr eq 171  then begin
;				Pause
				f_fg(25)=sl_tog(f_fg(25))
				if f_fg(25) then ros=1 else ros=0
			        endif else $
			    if  zerr eq 170  then begin
				if f_fg(25)  then begin
				   fcg(0) =ccl+1 & fcg(1)=lcl+1 & fcg(2)=1
				   if f_fg(18) eq 0 then rsl=rsl+1  $
						    else rsl=rsl-1
				   if rsl lt rql then begin f_fg(18)=0
							 rsl=rsl+2 & endif else $
				   if rsl gt rqm then begin f_fg(18)=1
							 rsl=rsl-2 & endif
				   f_fg(28)=f_fg(18)+1
				   fcl    =rsl   & zerr=37      & vik =inc
				endif
			        endif else $
			    if (zerr ge 180) and (zerr le 199) then begin
				provit3,erey,windn,xsiz
			        endif else $
			    if zerr lt 500 then begin	inc=0 & waits=wayt
;**			Tampo then 0
;**			----- ---- -
			        endif else $
			    if zerr ge 500 then	begin
;**			Tampo then inc
;**			----- ---- ---
				k=zerr/10
				if  k *10  eq zerr then	begin inc=inc-50
							      zerr=0 & endif
			    endif
			    endif
;********
			    if zerr eq 38  then   begin
;**			Context change.
;**			------- ------
				if ares_z(0) ge 3 then nz=ares_z(3)  else  nz=1
				fcg(0)=ares_z(1) & fcg(1)=ares_z(2) & fcg(2)=nz
				cc =0   & lc=0 & fcl=0 & fdp=1
				if vik gt 0 then begin vf=rvl & vm=rvm & endif
				rvm=rvl & windn=-windn-1
				endif else $
			    if zerr eq 39  then   begin
;**			Data change.
;**			---- ------
				f_fg(34)= f_fg(34)-1
				rvm=rvl & fdd=1 & endif
return
end
;
;
pro provit1,	erey,windn
;** *******
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;**			Ellipsoide adjustement.
;**			Taking values.
;**			------ ------
			    if (zerr eq 134) or (zerr eq 214) $
					     or (zerr eq 218) then begin
;**			    Close.
				if rql gt 1 then begin rql=rql-1
				  arel(0,0)=rql
				  clfc(0)=rql & clfc(1)=-1
				  bb=sl_working(inc,ex_p2,1,explp)
				  if zerr eq 134 then i=2 else i=1
				  bb=sl_savarea(5,arel,clfc,arel_z,i)
				endif
				rql=arel_z(1)
				bb=sl_dd(2,arel,arel_z)
				if zerr eq 218 then begin
;**				  Get a reflex file
				  bb=sl_getrflx(arel,arel_z,rql,ex_t1)
				  if bb then begin
				     rsl=long(1)
				     rql=long(arel(0,0))
				     i  =long(arel(1,rql))
				     j  =long(arel(2,rql))
				     bb =sl_gf(arel(0,rql),1,0,fma)
				     bb =sl_sti(ex_p3,sl_str(i, i4),19)
				     bb =sl_sti(ex_p3,sl_str(j, i4),25)
				     bb =sl_sti(ex_p3,sl_str(arel(0,rql),fma),6)
			             bb =sl_sti(ex_p3,sl_str(       rql ,i5) ,0)
				     rql=rql+1 & rqm=rql & vtm=rql
				     bb =sl_working(inc,ex_p2,2 ,ex_p3)
				  endif else zerr=214
				  bb =sl_tvmcur(2,x,y)
				endif
				if zerr ne 218 then begin
;**				  Stop
				  zerr=3 & endif
			        endif else $
			    if  zerr eq 130 then begin
				if f_fg(13) eq 0  then begin
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
					ot=2 &   endif
;
				if f_fg(23) eq 0  then begin
				 if (f_ab(0,0) ge 0) then begin
				   if (f_fg(31) eq 1) and (f_vu le 3) then begin
				    rsm=0
				    sl_ellip,2,erey,vsiz ,nfp,f_el,ccp,llp,$
					f_fg(1)-1,f_fg(2)-1, rsm,rsm,0
				   endif else begin
				    for k=f_ab(0,2),f_ab(1,2) do $
				     for jj=f_ab(0,1),f_ab(1,1) do $
				      for ii=f_ab(0,0),f_ab(1,0) do begin
					   if nz eq 1 then erey(ii,jj)  =vd $
						      else erey(ii,jj,k)=vd
				      endfor
				   endelse
				 endif
				 f_fg(23)=10
				endif
			        endif else $
			    if  zerr eq 210 then begin
				if f_fg(24) eq 0 then f_fg(24)=2
			        endif else $
			    if  zerr eq 131 then begin
;**			    Create image of values
				bb = sl_working(inc,ex_p2,1,explp)
				f_fg(24) =0
				arel(0,0)=rql-1
				bb=sl_lstframe(arel,arel_z,1, 0)
				if bb eq 1 then zerr=38 else zerr=3
				bb=sl_dd(2,arel,arel_z)
			        endif else $
			    if (zerr eq 132) or (zerr eq 212) then begin
;**			    Show selection
				bb=sl_working(inc,ex_p2,1,expli)
				arel(0,0)=rql-1
				bb=sl_lstframe(arel,arel_z,0, 2)
				if bb eq 1 then  f_fg(24)=3
				if zerr eq 132 then explm(0)=ex_p4 $
					       else explm(0)=ex_p6
				bb=sl_working(inc,ex_p2,1,explm(0))
				if (zerr eq 212) and (rql gt 2) then begin
;**				show vector
				   bb =sl_psizm(ares,ares_z,2,rql-1,1,8,-1,-1)
				   for k=1,rql-1 do ares(k-1,0)=arel(0,k)
				   vik=inc
				   spt=2 & zerr=38
				endif
			        endif else $
			    if (zerr eq 133) or (zerr eq 213) then begin
;**			    Take value or mean or sum or signal
				if rql ge rqm-2 then  begin
				   clfc(0)=0  & clfc(1)=50  & clfc(2)=0
				   bb=sl_d_p(13,arel,arel_z,0,clfc)
				   rqm=rqm+clfc(1)
				   endif
				if zerr eq 213 then begin
;				   Signal
				   arel(4,rql) =arev(nfp,7)*arev(nfp,8)
				   arel(0,rql) =arev(nfp,2)-arel(4,rql)
				   arel(6,rql) =f_el
				   arel(7,rql) =f_fg(1)
				   arel(8,rql) =f_fg(2)
				endif else begin
				 if f_fg(25) eq 1 then begin
;				   Mean
				   arel(4,rql) =arev(nfp,7)
				   arel(0,rql) =arev(nfp,3) - $
						arev(nfp,7)   & endif else $
				 if f_fg(25) eq 3 then begin
;				   Sum
				   arel(4,rql) =arev(nfp,7)*arev(nfp,8)
				   arel(0,rql) =arev(nfp,2)-arel(4,rql)
				   endif else $
				 if f_fg(25) eq 2 then begin
;				   Value
				   arel(4,rql) =arev(nfp,7)
				   arel(0,rql)=vo-arev(nfp,7) & endif
				 f_fg(25)=0
				endelse
				arel(1,rql)=ccp +1
				arel(2,rql)=llp +1
				arel(3,rql)=nfp +1
				arel(5,rql)=vtm
				vtm= vtm+1
				bb = sl_gf(arel(0,rql),1,0,fma)
				bb = sl_sti(ex_p3,sl_stx(explv,6,35),6)
				bb = sl_sti(ex_p3,sl_str(arel(0,rql),fma),6)
			        bb = sl_sti(ex_p3,sl_str(       rql ,i5) ,0)
				rql= rql+1
				if zerr eq 133 then explm(0)=ex_p4 $
					       else explm(0)=ex_p6
				bb = sl_working(inc,ex_p2,2 ,	ex_p3)
			        endif else $
			    if  zerr eq 135 then begin
;**			    Radius
				vd=(arel(1,0)-ccp+1) * (arel(1,0)-ccp+1) + $
				   (arel(2,0)-llp+1) * (arel(2,0)-llp+1)
				bb =sl_sqrt(vd,1)    &  i=fix(vd)
				arel(3,0)=i
			        bb =sl_sti(ex_p4, sl_str(i,i4),38)
				bb = sl_working(inc,ex_p2,1,ex_p4)
			        endif else $
			    if  zerr eq 136 then begin
;**			    Center
				arel(1,0)=ccp+1
				arel(2,0)=llp+1
				if c le l then i=c else i=l
				ot =ccl-c & if i gt ot then i=ot
				ot =lcl-l & if i gt ot then i=ot
				arel(3,0)=i & ot=0
			        bb =sl_sti(ex_p4, sl_str( i   ,i4),38)
			        bb =sl_sti(ex_p4, sl_str(ccp+1,i4),19)
			        bb =sl_sti(ex_p4, sl_str(llp+1,i4),25)
				bb = sl_working(inc,ex_p2,1,ex_p4)
				f_fg(29)=1
			        endif else $
			    if  zerr eq 215 then begin
;**			    Clear radial mode
				arel(4,0)=0
			        bb =sl_sti(ex_p6, sl_str(  0  ,i4),30)
			        bb =sl_sti(ex_p6, sl_str(  0  ,i4),36)
				bb = sl_working(inc,ex_p2,1,ex_p6)
			        endif else $
			    if  zerr eq 216 then begin
;**			    Center
				arel(1,0)=ccp+1
				arel(2,0)=llp+1
				arel(4,0)=1
			        bb =sl_sti(ex_p6, sl_str(ccp+1,i4),30)
			        bb =sl_sti(ex_p6, sl_str(llp+1,i4),36)
				bb = sl_working(inc,ex_p2,1,ex_p6)
				f_fg(29)=1
			        endif else $
			    if  zerr eq 137 then begin
;**			    take region
			     if f_ab(0,0) ge 0 then begin
				bb=sl_working(inc,ex_p2,1,explp)
				ot=  f_ab(1,0)-f_ab(0,0)+1
				ot= (f_ab(1,1)-f_ab(0,1)+1)*ot
				if rql+ot ge rqm-2 then  begin
				   clfc(0)=0  & clfc(1)=ot*2  & clfc(2)=0
				   bb=sl_d_p(13,arel,arel_z,0,clfc)
				   rqm=rqm+clfc(1)
				   endif
				if (f_fg(31) eq 1) and (f_vu le 3) then begin
				    sl_box,1
				    sl_ellip,3,erey,vsiz,nfp,f_el,ccp,llp,$
					f_fg(1)-1,f_fg(2)-1, 0,0,0
				    ot =aregx_z(1)-1
				    for k=0, ot do begin
				      i =aregx(k)+1 & ii=aregx(ot-k)-1
				      j =aregy(k)
				      if i le ii then for jj=i,ii do begin
					if nz eq 1 then arel(0,rql)=erey(jj,j) $
						   else arel(0,rql)=erey(jj,j,nfp)
					arel(0,rql)=arel(0,rql)-arev(nfp,7)
					arel(1,rql)=jj +1
					arel(2,rql)=j  +1
					arel(3,rql)=nfp+1
					arel(4,rql)=arev(nfp,7)
					arel(5,rql)=vtm
					rql=rql+1
				    endfor & endfor
				endif else begin
				 for k=f_ab(0,2),f_ab(1,2) do $
				 for jj=f_ab(0,1),f_ab(1,1) do $
				  for ii=f_ab(0,0),f_ab(1,0) do begin
					if nz eq 1 then arel(0,rql)=erey(ii,jj) $
						   else arel(0,rql)=erey(ii,jj,k)
					arel(0,rql)=arel(0,rql)-arev(nfp,7)
					arel(1,rql)=ii+1
					arel(2,rql)=jj+1
					arel(3,rql)=k +1
					arel(4,rql)=arev(nfp,7)
					arel(5,rql)=vtm
					rql=rql+1
				  endfor
				endelse
				vtm=vtm+1
				bb = sl_sti(ex_p3,sl_stx(explv,6,35),6)
			        bb = sl_sti(ex_p3,sl_str(  rql  ,i5),0)
				explm(0) =  ex_p4 & ot=0
				bb = sl_working(inc,ex_p2,2,ex_p3)
				endif
			        endif else $
			    if (zerr eq 138) then begin
;				Update image
				f_fg(11)=nfp
				zerr=37 & vik=inc
			        endif else $
			    if (zerr eq 139) or (zerr eq 219) then begin
;**			    Back.
				rql=rql-1
				if vtm gt 1 then vtm=vtm-1
				i  =arel(5,rql)
				while (rql gt 0) and (i eq arel(5,rql)) do rql=rql-1
				bb =sl_gf(arel(0,rql),1,0,fma)
				bb=sl_sti(ex_p3,sl_str(    arel(0,rql),fma),6)
			        bb=sl_sti(ex_p3,sl_str(fix(arel(1,rql)),i4),19)
			        bb=sl_sti(ex_p3,sl_str(fix(arel(2,rql)),i4),25)
			        bb=sl_sti(ex_p3,sl_str(fix(arel(3,rql)),i4),36)
			        bb=sl_sti(ex_p3,sl_str(           rql  ,i5), 0)
				rql=rql+1
				bb=sl_working(inc,entitl,2,ex_p3)
				endif else $
			    if zerr eq 211 then   begin
;			    Show a reflex
				if  rql gt 1 then begin
				 if rsl ge rql then  rsl=rql-1
				 k =sl_click(1,rql-1,rsl,13,-1)
				 sl_pflex, arel,k ,1
				endif
				bb=sl_tvmcur(2,x,y)
			    endif
return
end
;
;
function sl_rad_lu, area,vsiz,nf ,cti,ctj,ry1,ry2,a1,a2 ,arel
;******* *********
;**
common rad_tmp,  i1,i2,it,j1,j2,jt,nz,r,ta,tt,ra1,ra2,ii1,ii2,ii3,ii4,$
		 jj1,jj2,jj3,jj4,ki,kj
;**
;float jt,r,ra1,ra2
;
	if vsiz(0) ge 3 then nz=vsiz(0) else nz=1
	ci   =float(cti)
	cj   =float(ctj)
	r1   =ry1 & if r1 le 0  then r1= 1
	r2   =ry2 & if r2 lt r1 then r2=r1

	if ((a1 eq 0.) and (a2 eq 360.)) or $
	   ((a1 eq 1.) and (a2 eq 360.)) or $
	   ((a1 eq 0.) and (a2 eq 359.)) or $
	    (a1 eq a2) then ta=0 else ta=1

	ra1=a1*3.1416/180.
	ra2=a2*3.1416/180.
	csa1=cos(ra1)
	csa2=cos(ra2)
	sna1=sin(ra1)
	sna2=sin(ra2)
;
	ii1=ci+csa1*r1
	ii2=ci+csa1*r2
	ii3=ci+csa2*r1
	ii4=ci+csa2*r2
	same=1-ta & if a1 ge a2 then if (a1/90 eq a2/90) then same=1

	if ((sna1 gt 0) and (sna2 lt 0)) or (same eq 1) then i1 =round(ci-r2) $
					else i1 =round(min([ii1,ii2,ii3,ii4]))
	if ((sna1 lt 0) and (sna2 gt 0)) or (same eq 1) then i2 =round(ci+r2) $
					else i2 =round(max([ii1,ii2,ii3,ii4]))
	if i1 lt 0       then i1=0
	if i2 ge vsiz(1) then i2=vsiz(1)-1
;
	jj1=cj-sna1*r1
	jj2=cj-sna1*r2
	jj3=cj-sna2*r1
	jj4=cj-sna2*r2
	if ((csa1 gt 0) and (csa2 lt 0)) or (same eq 1) then j1 =round(cj-r2) $
					else j1 =round(min([jj1,jj2,jj3,jj4]))
	if ((csa1 lt 0) and (csa2 gt 0)) or (same eq 1) then j2 =round(cj+r2) $
					else j2 =round(max([jj1,jj2,jj3,jj4]))

	if j1 lt 0       then j1=0
	if j2 ge vsiz(2) then j2=vsiz(2)-1
;
	ci2=round(ci*2)
	ra1=-0.01+a1
	ra2= 0.01+a2
	arel(*,r1:r2)=0
;
	for j =j1,j2 do begin
	    kj=cj-j
	    jt=kj*(kj)
	    for i=i1,i2 do begin
		ki=i-ci   &  r=jt+ki*ki   &  r=sqrt(r)
		it=long(r+0.49)
		if  (it ge r1) then begin
		 if (it le r2) then begin
			if  ta eq 1 then tt=sl_radi_a( kj, ki, ra1,ra2) $
			    	    else tt=1
			if  tt eq 1 then begin
			 if nz eq 1 then arel(1,it)=arel(1,it)+area(i,j) $
			 else  		 arel(1,it)=arel(1,it)+area(i,j,nf)
			 arel(3,it)=arel(3,it)+1
			endif
		 endif else if i ge ci then i=i2+ 1
		endif  else if i lt ci then i=ci2-i-1
	    endfor
	endfor
;
	if arel(0,0) lt r2 then arel(0,0)=r2
	for i=r2,r1,-1  do begin
			if arel(3,i) lt 1 then begin
			   arel(1,i)=arel(1,i+1)
			   arel(3,i)=1
			   endif
			arel(0,i)=i
			r=arel(1,i)
			arel(1,i)=r/arel(3,i)
			bb=sl_sqrt(r,1)
			arel(2,i)=r/arel(3,i)
			arel(4,i)=nf+1
	endfor
return, 1
end
;
;
pro provit2,	erey,windn
;** *******
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
			    if  zerr eq 143 then begin
;**			Slicing.
;**			-------
;				Stop.
				bb  =sl_settings(mfi,0,0,zerr,inc,50-40)
				bb  =sl_dd(2,arei,arei_z)
				zerr=3
;				if bxy(8) ge 0 then zerr=37
				f_fg(24)=0
			        endif else $
			    if  zerr eq 144 then begin
;				Set pivot.
				bxy(6)=ccp & bxy(7)=llp
 			        bb=sl_sti(explc, sl_str(bxy(6)+1,i4),12)
 			        bb=sl_sti(explc, sl_str(bxy(7)+1,i4),18)
 				explm(1)= explc & ot=1
				f_fg(26)= 0
			        endif else $
			    if  zerr eq 141 then begin
;				Show slice
				if ares_z(0) eq 2 then begin
;					bb  =sl_settings(mfi,0,0,zerr,inc,50-40)
;					bb  =sl_dd(2,arei,arei_z)
;					f_fg(24)=0
					if ares_z(2) lt 10 then spt=2
				 	vik =inc
					zerr=38 & endif
			        endif else $
			    if  zerr eq 140 then begin
				if f_fg(13) eq 0  then begin
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
                                        ot=2 &   endif
				if f_fg(26) then $
				   if  (xq ne xs) or (yq ne ys) then begin
					bxy(6)=bxy(6)+xs-xq
					bxy(7)=bxy(7)+ys-yq
					if bxy(6) lt cp     then begin
					   bxy(7)=bxy(7)-sl_pfix((cp-bxy(6)) $
								 *sl_tang(f_sa))
					   bxy(6)=cp	  & endif else $
					if bxy(6) gt cp+ccl then begin
					   bxy(7)=bxy(7)-sl_pfix((cp+ccl-bxy(6))$
								 *sl_tang(f_sa))
					   bxy(6)=cp+ccl  & endif
					if bxy(7) lt lp     then begin
					   bxy(6)=bxy(6)-sl_pfix((lp-bxy(7)) $
								 /sl_tang(f_sa))
					   bxy(7)=lp	  & endif else $
					if bxy(7) gt lp+lcl then begin
					   bxy(6)=bxy(6)-sl_pfix((lp+lcl-bxy(7))$
								 /sl_tang(f_sa))
					   bxy(7)=lp+lcl  & endif
					if bxy(6) lt cp     then bxy(6)=cp else $
					if bxy(6) gt cp+ccl then bxy(6)=cp+ccl
				   endif
				xq = xs  & yq = ys
				if f_fg(26) ne rqm then  begin rqm=f_fg(26)
					if rqm  then bb=sl_sti(explc,' ON',40) $
						else bb=sl_sti(explc,'OFF',40)
 				        explm(1)= explc & ot=1 & endif
				if f_fg(25) ne rql then  begin rql=f_fg(25)
 			                bb=sl_sti(explc,sl_str(rql*2+1,i3),26)
 				        explm(1)= explc & ot=1 & endif
			        endif else $
			    if  zerr eq 142 then begin
;				Write slice
				if  (ares_z(0) eq 2) then begin
				 if (arei_z(0) eq 2) and (arei_z(2) eq 2) $
				     and (arei_z(1) eq ares_z(1)) then begin
				     clfc(0)=ares_z(1) & clfc(1)=4
				     clfc(2)=-1
				     bb=sl_psizm(tare,tare_z,2,4,ares_z(1),$
						 8,-1,-1)
				     for i=0,ares_z(1)-1 do begin
					 tare(0,i)=ares(i,0)
					 tare(1,i)=arei(i,0)+1
					 tare(2,i)=arei(i,1)+1
					 tare(3,i)=nfp+1
				     endfor
				     bb=sl_savarea(5,tare,clfc,tare_z,0)
				     bb=sl_dd(2,tare,tare_z)
				 endif else begin
				     clfc(0)=ares_z(2) &  clfc(1)=ares_z(1)
				     clfc(2)=-1
				     bb=sl_savarea(5,ares,clfc,ares_z,0)
				 endelse
				 bb=sl_dd(2,ares,ares_z)
				 bb=sl_tvmcur(2,x,y)
				endif
			        endif else $
			    if (zerr eq 148) then begin
;				Update image
				zerr=37 & vik=inc
			        endif else $
			    if (zerr eq 220) then begin
;**		Radial integration
;**		------ -----------
			        endif else $
			    if (zerr eq 221) then begin
;			    Show vect integ
				j=arel(0,0)
				if j gt 1 then begin
				 bb=sl_psiz(csiz,1,j,arel_z(arel_z(0)+1),-1,-1,-1)
				 k  =sl_maxf( arel(0,1:j),csiz,i)
				 vtm=vsiz(1)*vsiz(1)/4. + vsiz(2)*vsiz(2)/4.
				 bb =sl_sqrt(vtm ,1)
				 if  vtm gt k then k=long(vtm)
				 bb =sl_psizm(ares,ares_z,2,k+1,1,8,-1,-1)
				 for k=1,j do ares(arel(0,k),0)=arel(1,k)
;
;				 bb =sl_dd(2, arel,arel_z)
;				 bb =sl_settings(mfi,0,0,zerr,inc,50-40)
;				 f_fg(24)=0
				 vik=inc
				 spt=2 & zerr=38
				endif
			        endif else $
			    if (zerr eq 222) then begin
;			    Write vect
				if arel(0,0) gt 0 then begin
				  bb=sl_pp(0,arel,arel_z,ares,ares_z)
				  j=0
				  for k=0, ares(0,0) do $
					if ares(0,k) gt 0  then begin
					   ares(0,j) = ares(*,k)
					   j=j+1	&	endif
				  ares(0,0)= j-1
				  ares(1,0)=ares(1,0)+1 & ares(2,0)=ares(2,0)+1
				  clfc(0)  =ares(0,0)   & clfc(1)  =-1
				  bb=sl_working(inc,ex_p2,1,explp)
				  bb=sl_savarea(5,ares,clfc,ares_z,3) & endif
				  bb=sl_working(inc,ex_p2,1,ex_p7)
				  bb=sl_dd(2,ares,ares_z)
				  bb=sl_tvmcur(2,x,y)
			        endif else $
			    if (zerr eq 224) then begin
;			    Stop
				bb=sl_dd(2,arel,arel_z)
				f_fg(24)=0
				bb=sl_settings(mfi,0,0,zerr,inc,50-40)
				zerr=3
			        endif else $
			    if (zerr eq 223) then begin
;			    Center
				arel(1,0)=ccp
				arel(2,0)=llp
				if arel(0,0) gt 0 then arel(*,1:*)=0
				arel(0,0)=0
			        bb =sl_sti(ex_p7, sl_str(ccp+1,i4),14)
			        bb =sl_sti(ex_p7, sl_str(llp+1,i4),19)
				bb =sl_working(inc,ex_p2,1,ex_p7)
				f_fg(29)=1
			        endif else $
			    if (zerr eq 225) then begin
;			    Lower angle
;			    Upper angle
				i  =ccp-arel(1,0)
				j  =arel(2,0)-llp
				vtm=0.
	   			if (i  ne 0) or (j ne 0) then vtm=sl_atang(j,i)
				if vtm lt 0 then vtm= 2*3.1416 + vtm
				vtm=vtm*180./3.1416
				if f_fg(23) eq 1 then arel(3,0)=sl_pfix(vtm)
				if f_fg(23) eq 2 then arel(4,0)=sl_pfix(vtm)
				if (arel(3,0) eq arel(4,0)) then begin
				    arel(3,0)=0.
				    arel(4,0)=360. & endif
				i  = fix(arel(3,0))
				j  = fix(arel(4,0))
			        bb = sl_sti(ex_p8,sl_str(     i   ,i3) ,32)
			        bb = sl_sti(ex_p8,sl_str(     j   ,i3) ,40)
				bb =sl_working(inc,ex_p2,2,ex_p8)
			        endif else $
			    if (zerr eq 226) then begin
;			    Lower radius
;			    Upper radius
				i  =ccp-arel(1,0) & vtm=float(i*i)
				i  =llp-arel(2,0) & vtm=vtm + i*i
				bb =sl_sqrt(vtm,1)
				if  vtm lt 1 then vtm=1.
				if f_fg(23) eq 1 then begin
					rsl=sl_pfix(vtm)
					bb =sl_sti(ex_p7, sl_str( rsl ,i4),31)
					endif
				if f_fg(23) eq 2 then begin
					rsm=sl_pfix(vtm)
					bb =sl_sti(ex_p7, sl_str( rsm ,i4),39)
					endif
				bb =sl_working(inc,ex_p2,1,ex_p7)
			        endif else $
			    if (zerr eq 227) then begin
;			    Record current radius
				i =ccp-arel(1,0) & vtm=float(i*i)
				i =llp-arel(2,0) & vtm=vtm + i*i
				bb=sl_sqrt(vtm,1)
				i =sl_pfix(vtm)*2
				sl_box,1
				bb=sl_radies(erey,vsiz,nfp,0.,arel(1,0),$
				      arel(2,0),i,i,vtm,rql,arel(3,0),arel(4,0))
				if (rql gt 0) and (i gt 1)  then begin
				 j=i/2
				 if arel(0,0) lt j then arel(0,0)=j
				 arel(0,j)=i/2
				 arel(1,j)=vtm/rql
				 bb=sl_sqrt(vtm,1)
				 arel(2,j)=vtm/rql
				 arel(3,j)=rql
				 arel(4,j)=nfp+1
				 bb = sl_gf(arel(1,j),1,0,fma)
			         bb = sl_sti(ex_p8,sl_str( rql     ,i5) ,0 )
				 bb = sl_sti(ex_p8,sl_str(arel(1,j),fma),6 )
			         bb = sl_sti(ex_p8,sl_str(arel(0,j),i4) ,19)
				 bb = sl_working(inc,ex_p2,2,ex_p8)
				endif
			        endif else $
			    if (zerr eq 228) then begin
;			    Record lower to upper radius
				     bb =sl_working(inc,ex_p2,1,explp)

				     if rsl gt rsm  then begin
				      i=rsl &  rsl=rsm & rsm=i & endif

				     bb=sl_rad_lu(erey,vsiz,nfp,arel(1,0),$
						  arel(2,0),rsl,rsm,$
						  arel(3,0),arel(4,0),arel)
				     if bb eq 0 then begin
				     sl_box,1
				     for j =rsl,rsm do begin
					 bb=sl_radies(erey,vsiz,nfp,0.,$
					    arel(1,0),arel(2,0),j*2,j*2,vtm,rql,$
					    arel(3,0),arel(4,0))
					 if rql gt 0  then begin
				 	  arel(0,j)=j
				 	  arel(1,j)=vtm/rql
				 	  bb=sl_sqrt(vtm,1)
				 	  arel(2,j)=vtm/rql
				 	  arel(3,j)=rql
				 	  arel(4,j)=nfp+1
					 endif
				     endfor
				     if arel(0,0) lt rsm then arel(0,0)=rsm
				     endif
				     bb=sl_gf(arel(1,rsm),1,0,fma)
			             bb=sl_sti(ex_p8,sl_str(arel(3,rsm),i5) ,0 )
				     bb=sl_sti(ex_p8,sl_str(arel(1,rsm),fma),6 )
			             bb=sl_sti(ex_p8,sl_str(arel(0,rsm),i4) ,19)
				     explm(0)=ex_p7
				     bb = sl_working(inc,ex_p2,2,ex_p8)
			    endif
return
end
;
;
;
pro provit3,	erey,windn,xsiz
;** *******
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
			    if zerr eq 180 then   begin
;**			Filtering in frequencies.
;**			--------- -- -----------
				if f_fg(13) eq 0  then begin
					bb=sl_settings(mfi,0,0,zerr,inc,50-40)
                                        ot=2 &   endif
;				Just enter in process.
				if f_fg(30) gt 2  then begin
					f_fg(23)=10
					f_fg(27)= 0
					f_vu	= 3
					i =sl_vecfun(4,0,exff,ex_ff,tv_xp,tv_yp)
					sl_manycol,104
					bb=sl_gf(10.,1,0,fma)
;					Come with complex.
					if f_fg(30) eq 4 then begin
						   explm(0)= explb
						   explm(1)= explz & ot=1
						   tv_win(16,windn)=64
						   rql=-1  &  endif
					f_fg(34)=2
					f_fg(30)=1
				endif
			        endif else  $
			    if (zerr eq 181) then begin
;				Save
				bb = sl_working(inc,ex_p2,1,explp)
				tv_win(16,windn)=64
				bb=sl_savarea(0,arel,fcg,arel_z,windn,0,[0,0,0,0])
				bb=sl_pp(0,arel,arel_z,erey,vsiz)
				bb=sl_dd(2,arel,arel_z)
				bb=sl_dd(2,areo,areo_z)
				xsiz(0)=vsiz(*)
				zerr=4 & c= -1
			        endif else  $
			    if (zerr eq 182) or (zerr eq 186) then begin
;				Spacial or Cancel
				bb = sl_working(inc,ex_p2,1,explp)
				if ((zerr eq 182) and (f_fg(34) ne 2)) $
				or  (rql  eq -1 ) then begin
				  if rql  eq -1   then rql=8
				  bb  =sl_d_p(12,erey,vsiz,rql)
				endif else begin
				  bb  =sl_pp(0,areo,areo_z,erey,vsiz)
				endelse
				bb  =sl_dd(2,areo,areo_z)
				bb  =sl_settings(mfi,0,0,zerr,inc,50-40)
				if zerr eq 182 then f_fg(34)=2
				f_fg(0)=0
				zerr=39
			        endif else $
			    if (zerr eq 184) then begin
				bb = sl_working(inc,ex_p2,1,explp)
				if f_fg(30) eq 1 then begin
;				Phase
				      bb=sl_d_p(17,erey,vsiz,0)
				      f_fg(30)=2
				      f_fg(0) =0
				endif else if f_fg(30) eq 2 then begin
;				Power
				      bb=sl_d_p(16,erey,vsiz,0)
;				      bb=sl_d_p(30,erey,vsiz,0,0,rvl,rvm)
				      f_fg(0) =1
				      f_fg(30)=1 & endif
				zerr=39 & vik=inc
			        endif else  $
			    if  zerr eq 185  then begin
;				Clear point
				if nz eq 1  then begin  erey(ccp,llp)     =0
							arel(ccp,llp)	  =0.01
				endif	    else begin  erey(ccp,llp,nfp) =0
							arel(ccp,llp,nfp) =0.01
				endelse
				f_fg(34)=0 & ot=2
			        endif else $
			    if  zerr eq 187  then begin
				if f_fg(23)  eq 0 then begin
;				Clear region
				 if (f_ab(0,0) ge 0) then begin
				   bb=sl_working(inc,ex_p2,1,explp)
				   if (f_fg(31) eq 1) and (f_vu le 3) then begin
				    vtm=0.
				    sl_ellip,2,erey,vsiz  ,nfp,f_el,ccp,llp,$
					f_fg(1)-1,f_fg(2)-1, vtm,vtm,0.
				    sl_ellip,2,arel,arel_z,nfp,f_el,ccp,llp,$
					f_fg(1)-1,f_fg(2)-1, vtm,vtm,0.01
				   endif else begin
				    for k=f_ab(0,2),f_ab(1,2) do begin
				     if nz eq 1 then begin
						erey(f_ab(0,0):f_ab(1,0),   $
						     f_ab(0,1):f_ab(1,1))  =0
						arel(f_ab(0,0):f_ab(1,0),   $
						     f_ab(0,1):f_ab(1,1))  =0.01
				     endif	else begin
						erey(f_ab(0,0):f_ab(1,0),   $
						     f_ab(0,1):f_ab(1,1),k)=0
						arel(f_ab(0,0):f_ab(1,0),   $
						     f_ab(0,1):f_ab(1,1),k)=0.01
				     endelse
				    endfor
				   endelse
				   explm(0)=explb & ot=1
				 endif
				endif
				if (f_fg(23) eq 1) or (f_fg(23) eq 3) then begin
;				Bandpass
				 bb =sl_working(inc,ex_p2,1,explp)
				 rsm=(ccl+1)/2 & rsl=(lcl+1)/2
				 vtm=(float(ccl)/2.-c)*(float(ccl)/2.-c) + $
				     (float(lcl)/2.-l)*(float(lcl)/2.-l)
				 bb =sl_sqrt(vtm,1)
				 rqm=sl_pfix(vtm*2)
				 if f_fg(23) eq 3 then  begin rqm=rqm-2
						  	f_fg(23)=2 & endif
		sl_ellip,f_fg(23),erey,vsiz  ,nfp,f_el,rsm,rsl,rqm,rqm, vtm,vtm,0.
		sl_ellip,f_fg(23),arel,arel_z,nfp,f_el,rsm,rsl,rqm,rqm, vtm,vtm,0.01
				 explm(0)=explb
                                endif
				if f_fg(23)  eq 5 then begin
;				Clear Line
				 if nz eq 1 then begin erey(cp:ccl+cp,llp)    =0
						       arel(cp:ccl+cp,llp)    =0.01
				 endif	    else begin erey(cp:ccl+cp,llp,nfp)=0
						       arel(cp:ccl+cp,llp,nfp)=0.01
				 endelse
				endif
				if f_fg(23)  eq 7 then begin
;				Clear Column
				 if nz eq 1 then begin erey(ccp,lp:lcl+lp)    =0
						       arel(ccp,lp:lcl+lp)    =0.01
				 endif	    else begin erey(ccp,lp:lcl+lp,nfp)=0
						       arel(ccp,lp:lcl+lp,nfp)=0.01
				 endelse
				endif
				f_fg(23)=10 & f_fg(34)=0 & ot=1
			        endif else  $
			    if  zerr eq 189  then begin
;				Pass to user.
				bb=sl_funn  (0)
				bb=sl_vecfun(-1,0)
				bb=sl_tvpop(w,0)
				clfc(0)=ccp & clfc(1)=llp & clfc(2)=nfp
				jj=sl_break(arel,arel_z,clfc,ares,ares_z)
				jj=sl_vecfun(4,0,exff,ex_ff,tv_xp,tv_yp) & ot=1
				bb=sl_tvsel (w)
				if (bb ne 1)  or $
				   (arel_z(0) ne vsiz(0)) or $
				   (arel_z(1) ne vsiz(1)) or $
				   (arel_z(2) ne vsiz(2)) then  zerr=3 $
				else     bb=sl_tvpop(w,1)
				f_fg(34)=0
			    endif else $
			    if zerr eq 194 then	begin
;**			Magnifying and rot.
;**			---------- --- ---
;				Old position
				clfc(0)=ccp & clfc(1)=llp & clfc(2)=nfp
			        bb=sl_sti(explo3, sl_str(clfc(2)+1,i4), 9)
			        bb=sl_sti(explo3, sl_str(clfc(0)+1,i5),18)
			        bb=sl_sti(explo3, sl_str(clfc(1)+1,i5),24)
				if rsm ge 0 then begin
				   bb=sl_magang(rqm,rql,clfc(0),clfc(1),$
						rsm,rsl,vta,vtm)
				   bb=sl_gf(vtm,1,0,fma)
			           bb=sl_sti(explo2, sl_str(vtm,fma),20)
				   bb=sl_gf(vta,1,0,fma)
			           bb=sl_sti(explo2, sl_str(vta,fma),34)
				   explm(1)=explo2
				   endif
				bb=sl_working(inc,entitl,1,explo3)
				f_fg(29)=2
			        endif else $
			    if zerr eq 191 then	begin
;				New position
				rsm=ccp & rsl=llp
			        bb=sl_sti(explo3, sl_str(rsm +1,i5),32)
			        bb=sl_sti(explo3, sl_str(rsl +1,i5),38)
				if clfc(0) ge 0 then begin
				   bb=sl_magang(rqm,rql,clfc(0),clfc(1),$
						rsm,rsl,vta,vtm)
				   bb=sl_gf(vtm,1,0,fma)
			           bb=sl_sti(explo2, sl_str(vtm,fma),20)
				   bb=sl_gf(vta,1,0,fma)
			           bb=sl_sti(explo2, sl_str(vta,fma),34)
				   explm(1)=explo2
				   endif
				bb=sl_working(inc,entitl,1,explo3)
				f_fg(29)=2
			        endif else $
			    if zerr eq 190 then	begin
			        endif else $
			    if zerr eq 192 then	begin
;				Mag and rot
				f_fg(24)=0
				if (vta ne 0.) or (vtm ne 1.) then begin
				    bb=sl_sti(explo3,'    '	   ,9 )
			            bb=sl_sti(explo3,'     ,     ' ,18)
			            bb=sl_sti(explo3,'     ,     ' ,32)
				    bb=sl_working(inc,ex_p2,1,explp)
				    bb=sl_rotscal(erey,vsiz,clfc(2),$
						  vta,vtm,rqm,rql)
				    zerr=39
				endif else zerr=3
			        endif else $
			    if zerr eq 193 then	begin
;				Center
				f_fg(24)= 1
				bxy(6)=ccp & bxy(7)=llp
				rqm=bxy(6) & rql=bxy(7)
				if (rsm ge 0) and (clfc(0) ge 0) then begin
				   bb=sl_magang(rqm,rql,clfc(0),clfc(1),$
						rsm,rsl,vta,vtm)
				   bb=sl_gf(vtm,1,0,fma)
			           bb=sl_sti(explo2, sl_str(vtm,fma),20)
				   bb=sl_gf(vta,1,0,fma)
			           bb=sl_sti(explo2, sl_str(vta,fma),34)
				   endif
			        bb=sl_sti(explo2, sl_str(rqm+1,i4), 5)
			        bb=sl_sti(explo2, sl_str(rql+1,i4),11)
				bb=sl_working(inc,entitl,2,explo2)
			        endif else $
			    if zerr eq 195 then	begin
;				Clear mag or angle
				if f_fg(23) eq 0 then begin
				  vta=0. & bb=sl_sti(explo2, ' 0.00    ',34)
				  endif
				if f_fg(23) eq 1 then begin
				  vtm=1. & bb=sl_sti(explo2, ' 1.00    ',20)
				  endif
				f_fg(23)=10
				bb=sl_working(inc,entitl,2,explo2)
			        endif
return
end
;
;
;
pro provm, erey
;** *****
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
common my_ovs,	ov_sum1,ov_sum2,ov_sum3,ov_sum4,ov_sum5,ov_sum6,ov_sum7,$
		ovs1_z ,ovs2_z ,ovs3_z ,ovs4_z ,ovs5_z ,ovs6_z ,$
		ov_pmx,ov_f,ov_z,ov_l,ov_m
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
			  c_o = sl_str( c+cc  +1,i5)
			  l_o = sl_str( l+lc  +1,i5)
			  f_o = sl_str(nf+fcl +1,i4)
			  if s_out  eq 0 then begin
			     if nz  eq 1 then vl = erey(ccp,llp) else $
					      vl = erey(ccp,llp,nfp)
			     bb=sl_cv(vl,vo,f,g,cpx)  & fmx=fmt
			     if (spt gt -2) and (spt ne 1) and (f_fg(13)) $
					   then  sl_box,2
			  endif  else  begin
			     if s_out eq 1 then c_o=s_o else $
			     if s_out eq 2 then l_o=s_o else $
			     if s_out eq 3 then	f_o=s_o else $
			     if s_out eq 4 then	begin
						c_o=s_o & l_o=s_o & endif else $
			     if s_out eq 5 then	begin
						c_o=s_o & f_o=s_o & endif else $
			     if s_out eq 6 then	begin
						l_o=s_o & f_o=s_o & endif
			     if nnz gt 1     then $
			      if s_out eq 1  then vl=ov_sum1(l,nf) else $
			      if s_out eq 2  then vl=ov_sum2(c,nf) else $
			      if s_out eq 3  then vl=ov_sum3(c,l)  else $
			      if s_out eq 4  then vl=ov_sum4(nf )  else $
			      if s_out eq 5  then vl=ov_sum5(l  )  else $
			      if s_out eq 6  then vl=ov_sum6(c  )
			     if nnz eq 1     then $
			      if s_out eq 1  then vl=ov_sum1(l)    else $
			      if s_out eq 2  then vl=ov_sum2(c)
			     bb=sl_cv(vl,vo,1,g,cpx)
			     bb=sl_gf(   vo,1,g,fmx)		& endelse
			  bb=sl_sti(explv,sl_str(vo,fmx),6)
			  bb=sl_sti(explv,c_o,19)
			  bb=sl_sti(explv,l_o,25)
			  bb=sl_sti(explv,f_o,37)
return
end
;
;
;
function sl_zoomi, x,y,xd,yd
;******* ********
;**
	common	my_geto,go_v7,go_v2,go_v3,go_rql,go_rqm,go_x5,go_y5
;**
;**	Return -1 for none, 1 for left, -2 for middle, -3 for right button.
	ok=0  & ok2=0 & wiwi=(sys_dep('MACHINE') eq 'win')
	go_x5(0)=x & go_x5(3)=x & go_x5(4)=x
	go_y5(0)=y & go_y5(1)=y & go_y5(4)=y
	xd=x & yd=y
	xp=x & yp=y
	bb=sl_tvmod(2,6)
	bb=sl_tvmod(1,6)
	but=1 & bat=1
	while (xd ge 0) and (but ne 0) do begin
	   bat=but
	   if  (xd ne xp) or (yd ne yp) then begin
		if ok eq 1 then begin
		    if bat eq 2 then bb=sl_tvline(go_x5,go_y5,5,0,255) $
		    else	     bb=sl_tvpol (5,go_x5,go_y5,255,0)
		    ok=0 &  endif
		xp=xd & yp=yd
		if bat eq 2 then sz=15 else sz=15
		if (xd-x gt sz) or (xd-x lt -sz) and  $
		   (yd-y gt sz) or (yd-y lt -sz) then begin
		    go_x5(1)=xd  &  go_x5(2)=xd
		    go_y5(2)=yd  &  go_y5(3)=yd
		    if bat eq 2 then bb=sl_tvline(go_x5,go_y5,5,0,255) $
		    else	     bb=sl_tvpol (5,go_x5,go_y5,255,0)
		    ok=1 & ok2=1
		endif
	   endif
	   bb =sl_tvgcur(xd,yd,but,0)
	   if wiwi then if but eq 1 then but=2
	endwhile

	if ok eq 1 then if bat eq 2 then bb=sl_tvline(go_x5,go_y5,5,0,255) else $
			if bat eq 4 then bb=sl_tvpol (5,go_x5,go_y5,255,0) else $
			if xd  lt 0 then bb=sl_tvpol (5,go_x5,go_y5,255,0)
	if xd lt 0 then ok=-1
	if ok eq 0 then if ok2 eq 1 then ok=-1
	if ok eq 1 then if bat eq 1 then ok=-3 else $
			if bat eq 2 then ok=-2
	bb=sl_tvmod(2,3)
	bb=sl_tvmod(1,3)

return, ok
end
;
;
;
function sl_viewr,  erey,xsiz,windn,recurs
;******* ********   **** **** ***** ******
;**
;** Get more from the view.
;** --- ---- ---- --- ----
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_keep,	  rvl,rvm,vlt,vmt,$
		  explm,explv,explz,expld,expo ,ex_o ,expn,ex_n,expgs,ex_gs  ,$
		  ex_o1,ex_o2,ex_o3,expo1,expo2,expo3,explo1,explo2,explo3   ,$
		  expfi,expl ,expc ,expr ,exps ,expx ,expd ,expg ,expe ,expb ,$
		  explp,ex_l ,ex_c ,ex_r ,ex_s1,ex_x1,ex_d ,ex_g ,ex_e ,expp
common my_keep2,  expli,ex_s2,ex_x2,ex_x3,ex_p1,ex_p2,ex_p3,ex_t1,explk,ex_t3,$
		  exy  ,expf ,ex_f ,expy ,ex_y ,expm ,ex_m ,explb,exph ,explr,$
      		  explc,exci ,ex_i ,exsf ,ex_sf,ex_gh,ex_p4,ex_p5,exfi ,ex_fi
common my_keep3,  expw ,ex_w ,ex_x4,expk ,exz  ,ex_ff,exff ,ex_fl,expfl,exadj,$
		  ex_ad,exphc,exsph,ex_sp,exprs,ex_rs,wayt,i3,i4,i5,i6,f6,s_o,$
		  ex_p6,ex_p7,ex_p8,expex,ex_ex,exspc,exsi ,exsj ,ex_ra,exrad
;**
;common my_ovs,	ov_sum1,ov_sum2,ov_sum3,ov_sum4,ov_sum5,ov_sum6,ov_sum7,$
;		ovs1_z ,ovs2_z ,ovs3_z ,ovs4_z ,ovs5_z ,ovs6_z ,$
;		ov_pmx,ov_f,ov_z,ov_l,ov_m
;**
common my_viewr,  bxy,c,cc,ccl,ccp,cf,clfc,clop,cm,cp,cpx,csiz,c_o,  $
		  dxy,dif,bb,entitl,f,fcg,fcl,fcm,fct,fcx,fcy,ired,  $
		  fdd,fdp,fldat,fma,fmt,fmx,four,fp,fqc,frst,fvw,fxy,f_o,g
common my_viewr2, ins,inc,keep_col,l,lc,lcl,llp,lp,l_o,mfi,nf,nfp,nnz,nz,$
		  num,nx,ny,ot,plnx,plny,plx,ply,recars,res,rop,ros,rot,rql,$
		  rqm,rsl,rsm,rti,sdt,spc,spm,spt,s_out,ttl,ttlv,typ,vin,vik
common my_viewr3, vf,vm,vo,vsiz,w,waits,w_num,x,xpa,xs,xu,y,ys,yu,zerr,zs, $
		  i,j,ij,xq,yq,zq,vd,vl,x1,y1,xd,yd,vtm,vta
;**
	common  my_insert,  i_txt,i_idx,i_fil,i_rout,i_ps,i_rs,$
			    i_trout,i_tfil,i_tlang,i_tdx,i_enter,i_rcall
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
;	common	my_refl,arefl,arefl_z,rf_cur,rf_t
;**
;carez + erey
;
;float   fcx,fcy,vd,vl,vtm,waits
;care    rvl,rvm,vd,vf,vl,vm,vo,vtm (only vl=erey)
;
;**
	vsiz(0) =  xsiz(*)
	frst	=  1
;**
	f_fg(24)=  0 & f_fg(28) =  0 & f_fg(30)=0 & f_fg(34)=0 & f_fg(38)=0
	f_fg(49)=  0 & f_fg(50) =  0
	w	=  0 & vik	=  0
	rvl	=  0 & rvm	=  0
	recurs	=  0 & ros	=  0
	keep_col=  tv_col
;**
	repeat begin
	recars	=  0
;**
;prov
	if vsiz(0) le 1 then bb=sl_d_p(13,erey,vsiz,0,[0,1,0])
	xsiz(0)=sl_size(erey) & xsiz(6)=vsiz(6)
	if (xsiz(0) ne vsiz(0)) or (xsiz(1) ne vsiz(1)) or $
	   (xsiz(2) ne vsiz(2)) or $
	   (xsiz(xsiz(0)+1) ne vsiz(vsiz(0)+1)) then bb=sl_iotype('Viewr Size',0,0)
;**
	provi,windn,erey
;prov	if   vsiz(0) ge 3 then nz=vsiz(3) else nz = 1
;	four=vsiz(0)
;;**	Get window info.
;;**	--- ------ ----
;		w     = tv_win(0 ,windn)
;		ccl   = tv_win(1 ,windn)
;		lcl   = tv_win(2 ,windn)
;		fxy   = tv_win(3 ,windn)
;		ny    = tv_win(4 ,windn)
;		nnz   = tv_win(5 ,windn)
;		cc    = tv_win(6 ,windn)
;		lc    = tv_win(7 ,windn)
;		fcl   = tv_win(8 ,windn)
;		fac   = tv_win(9 ,windn)
;		fvw   = tv_win(10,windn)
;		num   = tv_win(11,windn)
;		ins   = tv_win(12,windn)
;		spc   = tv_win(13,windn)
;		cf    = tv_win(14,windn)
;		cm    = tv_win(15,windn)
;		typ   = tv_win(16,windn)
;		if w gt 0 then tv_od = tv_win(17,windn)
;		if tv_win(18,windn) ne tv_col then sl_manycol,tv_win(18,windn)
;		xpa   = tv_win(19,windn)
;		sdt   = tv_win(20,windn)
;		dxy(0)= tv_win(24,windn)
;		dxy(1)= tv_win(25,windn)
;		rot   = tv_win(26,windn)
;		fqc   = tv_win(27,windn)
;		bxy(12)=tv_win(28,windn)
;		bxy(13)=tv_win(29,windn)
;		spt   = spc/10
;		spm   = spc-10*spt  & if spm  lt 0 then spm =-spm
;		if (spt eq  1) or (spt eq -4) or (spt eq -6) then begin
;		   f_az    =tv_win(30,windn)
;		   f_ax    =tv_win(31,windn)
;		   f_fg(14)=tv_win(32,windn)
;		   f_fg(20)=tv_win(35,windn) & endif else $
;		if (spt eq -1) then begin
;		   f_fg(15)=tv_win(33,windn)
;		   f_fg(16)=tv_win(34,windn) & endif
;		f_fg(0)    =tv_win(38,windn)
;		f_fg(10)   =tv_win(39,windn)
;		f_fg(12)   =tv_win(40,windn)
;		f_fg(22)   =tv_win(41,windn)
;		if  fac gt 1 then fcx= float(fac) else fcx=1.
;		if -fac gt 1 then fcy=-float(fac) else fcy=1.
;		if  fxy ge 0 then begin fcx= fcx * fxy
;					fcy= fcy * fxy
;		endif 	     else begin fcx= fcx /(-fxy)
;					fcy= fcy /(-fxy)
;			     endelse
;		if  lcl gt 1 then begin
;		 if ccl gt 2 then if f_fg(1) gt ccl then  f_fg(1)=ccl
;		 if lcl gt 2 then if f_fg(2) gt lcl then  f_fg(2)=lcl
;		 if frst eq 1 then begin
;		    f_fg(2 )=f_fg(1)*f_wy/f_wp  & if f_fg(2) lt 2 then f_fg(2)=2
;		 endif
;		endif
;		fcg(0)  =ccl   & fcg(1)  =lcl & fcg(2)  =nnz
;		bxy(0)  =0     & bxy(1)  =0   & bxy(2)  =ccl   & bxy(3) =lcl
;		bxy(4)  =0     & bxy(5)  =0
;		w_num(0)=windn & w_num(1)=num & w_num(2)=-1
;		f_fg(41)=0
;		fdp   =  0
;		if sdt eq 2 then fdd = 1 else fdd = 0
;		ttlv  = 'x'
;		if (vsiz(1) eq ccl) and (vsiz(2) eq lcl) and $
;					(nz eq nnz)      then  dif=0 else dif=1
;		typ   = vsiz(vsiz(0)+1)
;		if (typ ge 8 )	    and (typ ne 16)      then    f=1 else   f=0
;		if (typ eq 64)      then cpx=1 		 else  cpx=0
;		ccl = ccl-1
;		lcl = lcl-1
;		fcm = nnz-1
;;**
;		if   dif then begin fp=fcl  &  cp=cc & lp=lc & endif else begin
;				    fp=0    &  cp=0  & lp=0  & endelse
;		vsiz( 7)=cp     & vsiz( 8)=lp     & vsiz( 9)=fp
;		vsiz(10)=ccl    & vsiz(11)=lcl    & vsiz(12)=fcm
;		vsiz(13)=cp+ccl & vsiz(14)=lp+lcl & vsiz(15)=fp+fcm
;;**
;		if (vsiz(1) le vsiz(13)) or $
;		   (vsiz(2) le vsiz(14)) or $
;		   (nz      le vsiz(15)) then w=0
;;**		Seems to be ok.
;;**		----- -- -- --
;;**
;		if arev_z(1) lt nz then begin	arev_z(1) =nz
;						bb=sl_dd(1,arev,arev_z) & endif
;;**		Special view.
;;**		------- ----
;		if (cpx) then begin
;		    bb =sl_pp(0,  erey,vsiz,arel,arel_z)
;		    bb =sl_d_p(16,erey,vsiz,0)
;;		    vlt=sl_minf(erey,vsiz,cf)
;;		    vmt=sl_maxf(erey,vsiz,cm)
;;		    bb =sl_d_p(30,erey,vsiz,0,0,vlt,vmt)
;		    f_fg(0)=1
;		    f_fg(30)=4 & vik=18 & cpx=0 & ot=3
;		endif
;		if frst eq  1 then begin
;		   frst  =  0
;		   if (spt eq -2)  then begin
;		      if w lt  0 then bb=sl_savarea(3,arec,w_num,arec_z) $
;		      else if sdt eq 0 then begin
;			bb=sl_resarea(3,arec,arec_z,dxy,w_num)
;			res(0)=sl_spacial(erey,1,0,vsiz,rot)
;		      endif  else bb=sl_dd(2,arec,arec_z)
;		      if arec_z(6)  eq 0    then w=-1 & endif
;		   if (spt eq -3) or (spm eq 6) or (spt eq -4) then  begin
;			      if spt eq -3 then clfc(0)=spm else clfc(0)=0
;			      bb=sl_d_p(40,erey,vsiz,dif,clfc)
;			      f_fg(44)=1
;		   endif else f_fg(44)=-1
;		endif
;;**
;;**
;		if (tv_lst gt 0) and (tv_lst ne w) then begin
;					bb=sl_tvsel (tv_lst)
;					if bb then bb=sl_tvtidy(tv_lst,1)
;					endif
;		if ( w     gt 0) and (not ros) then begin
;					bb=sl_tvsel (w)
;					if bb then begin
;						bb=sl_tvwake(w)
;						bb=sl_tvcur_w(w,-1,-1, 0,0,0)
;						bb=sl_tvmcur(1,0,0)
;						tv_lst  = w
;					endif else w=0
;		endif
;		if ( w     gt 0) then begin
;;**		Point to the maximum in view.
;;**		----- -- --- ------- -- ----
;		if cf eq cm   then begin
;		   vl  = sl_minf(erey,vsiz,cf) & tv_win(14,windn)=cf
;		   vl  = sl_maxf(erey,vsiz,cm) & tv_win(15,windn)=cm
;		endif
;		   nf  = cf /   (vsiz(1)*vsiz (2))
;		   cf  = cf - nf*vsiz(1)*vsiz(2)
;		   l   = cf /    vsiz(1) & c =cf - vsiz(1)*l
;		   if nz eq 1 then vl =  erey(c ,l)  else vl=erey(c ,l ,nf)
;		   bb=sl_cv(vl,vf,f,0,cpx)
;;**
;		   nf  = cm /   (vsiz(1)*vsiz(2))
;		   cm  = cm - nf*vsiz(1)*vsiz(2)
;		   l   = cm /    vsiz(1) & c =cm - vsiz(1)*l
;		   if nz eq 1 then vl =  erey(c ,l)  else vl=erey(c ,l ,nf)
;		   bb=sl_cv(vl,vm,f,0,cpx)
;		   if rvl eq rvm then begin rvl = vf & rvm = vm & endif
;;**
;		g      = 0
;		bb=sl_gf(vm, f,g ,fmt)
;		bb=sl_sti(explz,sl_str( vm  ,fmt) ,6 )
;		bb=sl_sti(explz,sl_str((c+1) ,i5) ,19)
;		bb=sl_sti(explz,sl_str((l+1) ,i5) ,25)
;		bb=sl_sti(explz,sl_str((nf+1),i4) ,37)
;;**
;		c=c-cp & l=l-lp
;		if nf  ge fp then   nf=nf-fp
;		if (c  lt 0) or (c  gt ccl) then c =ccl/2
;		if (l  lt 0) or (l  gt lcl) then l =lcl/2
;		if (nf lt 0) or (nf gt fcm) then nf=fcm/2
;;**
;		if vik eq 0 then explm(1)=explz
;		if num ge 0 then bb=sl_sti(ex_l, sl_str(num,i6),35)
;;**		Set first cursor position.
;;**		--- ----- ------ --------
;		s_out=  0
;		if (spt le -2) then begin
;;**			Special view.
;;**			------- ----
;			if (spt eq -2)  then begin
;			   bb=sl_psiz(csiz,2,arec_z(1),arec_z(2),arec_z(4),-1,-1)
;			   i =sl_maxf(arec(*,*,0),csiz,j) & endif else $
;			if (spt eq -3) or (spt eq -4) then  begin
;			   bb=sl_psiz(csiz,2,dxy(0),dxy(1), typ,-1,-1)
;			   j =l*dxy(0)+c & s_out=3	  & endif else $
;			if (spt eq -6)  then begin
;			   bb=sl_psiz(csiz,2,ccl+1 ,ccl+1 , typ,-1,-1)
;			   j =ccl*ccl/2  &   endif
;			y   =  j   / csiz(1)
;			x   =  fix(fcx*(j  - csiz(1)*y))
;			plx =  fix(fcx* csiz(1))
;			ply =  fix(fcy* csiz(2))
;			plny=  ply*ny
;			nx  =  1
;			y   =  ply - fix(fcy * y) - 1
;			if tv_od eq 0 then y=ply-y- 1
;		endif   else   begin
;;**			Normal view.
;;**			------ ----
;			plx =  fix(fcx*(ccl+1))
;			ply =  fix(fcy*(lcl+1))
;			plny=  ply* ny
;			nx  =  nnz/ ny & if nx*ny lt nnz then nx=nx+1
;			if ired(0) ge 0 then begin
;			   c=  ired(0)-cp
;			   l=  ired(1)-lp
;			   nf= ired(2)-fp
;			   ot=2 & endif
;			sl_dc, c,l ,x,y
;			endelse
;		ired(0)=-1
;		plnx  =plx*nx
;		if (x ge plnx) or (y ge plny) then begin
;					x=0 & y=0 & c=0 & l=0 & nf=0 & endif
;		mfi(0)=ccl+1 & mfi(1)=lcl+3
;		sl_box,0
;		xu=plnx/2
;		yu=plny-15
;		if not ros then begin
;		  if  (spt  eq 1) or  (spt eq -4) or  (spt eq -1) then begin
;;		   if (f_vu gt 0) and (f_vu le 5) then f_vu=1
;		   if (spt eq -1) then i=f_fg(16) else i=f_fg(14)
;		   bb=sl_surf(-1,0,ccl+1,lcl+1,fcm+1,$
;				 0,plx,ply,0,0,f_az,f_ax,f_fg(15),i,f_fg(20),0)
;		   endif
;		  arev(*,0)  =-1.
;		  arev(*,1:3)= 0.
;		endif
;		bb=sl_sti(explb,sl_str(cc +1    ,i5),7 )
;		bb=sl_sti(explb,sl_str(cc +1+ccl,i5),14)
;		bb=sl_sti(explb,sl_str(lc +1    ,i5),20)
;		bb=sl_sti(explb,sl_str(lc +1+lcl,i5),27)
;		bb=sl_sti(explb,sl_str(fcl+1    ,i4),33)
;		bb=sl_sti(explb,sl_str(fcl+1+fcm,i4),39)
;		if vik eq 0 then explm(0)= explb
;		inc=0
;		if vik gt 0 then begin inc=vik & vik=0 & endif
;		if f_fg(38)  ne 2 then begin
;		 if (inc eq 0) or (inc eq 50) then begin
;			ot= 3
;			ros	=0
;			waits	=wayt
;			entitl  =ex_p2
;			bb=sl_tvnobut(0)
;			if tv_flg(6) eq 0 then if tv_flg(1)  eq 0 then $
;			i = sl_tvmenu(1,0,expex,ex_ex,tv_x/tv_dx,0)
;			bb= sl_tvpop(w,1)
;			i = sl_vecfun(4,0,expl,ex_l,tv_xp,tv_yp)
;			bb= sl_tvwmaj(w,vf,vm,rvl,rvm,f_fg,f_vu,spt,f_ax,f_az)
;		 endif 	else if not ros then begin
;			if cpx then i=sl_vecfun(4,0,exff,ex_ff,tv_xp,tv_yp)
;			i = sl_vecfun(4,2)
;			bb= sl_tvshap(-1)
;			bb= sl_working(inc, entitl,0) & endif
;		 bb=sl_curset(exspc,exsi,exsj ,fxy,nnz)
;		 if bb eq 1 then if tv_flg(7) eq 0 then begin
;;			bb=sl_tvfont(1)
;			if i_tdx gt 2 then $
;			 i=sl_tvmenuh(-3,-24,[exspc(exsi:exsj),i_trout(0:i_tdx),$
;				      exsph],ex_sp,(tv_x-200)/tv_dx,tv_y/tv_dy) $
;			else $
;			 i=sl_tvmenuh(-3,-24,[exspc(exsi:exsj),$
;				      exsph],ex_sp,(tv_x-200)/tv_dx,tv_y/tv_dy)
;			bb=sl_tvpop (w,1)
;;			bb=sl_tvfont(0)
;		endif
;		endif
;		fct(0)=1 & fct(1)=1 & fct(2)=1
;		if fcx lt  1 then  fct(0)=fix(1./fcx)
;		if fcy lt  1 then  fct(1)=fix(1./fcy)
;		if fct(0)*fct(1) gt 1 then fqc=1 else fqc=0
;		clop =  0
;		xs   =  c  & xq=c
;		ys   =  l  & yq=l
;		zs   =  nf & zq=nf
;	     endif
;**
	     repeat  begin
		if w le 0 then if w eq -2 then zerr=37 else zerr=35 else begin
;**		Display values sometimes.
;**		------- ------ ---------
			ccp=c+cp & llp=l+lp & nfp=nf+fp
;**
			if (ot ne 0) then begin
			if (f_fg(45) ne 0) or (f_fg(38) eq 2) then $
							bb=sl_tvmcur(2,x,y)
			if (inc eq 0) or (inc eq 50) then explm(0) = explb
			if  (ot  ne 2) and (f_vu ne 0) then begin
;**			 Deviation,Mean.
;**			 --------------
			 if f_vu eq 7 then begin
			  if arev(nfp,0) eq -1  then  begin
			   bb=sl_d_p(41,erey,vsiz,dif,[nf,0] ,vd,vl)
			   bb=sl_cv(vd,vd,1,g,cpx)
			   bb=sl_cv(vl,vl,1,g,cpx)
			   arev(nfp,0)=vd & arev(nfp,1)=vl
			   bb =sl_psiz(csiz,2,arev_z(1),2,8,-1,-1)
			   vd =sl_maxim(arev(*,0:1),csiz,j,vl)
			   if -vl gt vd then  vd=-vl
			   bb=sl_gf( vd,1,g,fma)
			  endif
			  bb=sl_sti(expld,sl_str(arev(nfp,0),fma),6 )
			  bb=sl_sti(expld,sl_str(arev(nfp,1),fma),20)
			  bb=sl_sti(expld,sl_str(nf+fcl+1   ,i4) ,36)
			  explm(0)= expld
			 endif
;**
			bb=sl_working(inc, entitl,0)
			endif
;**			Get pointed value.
;**			--- ------- -----
			  provm,erey
;prov			  c_o = sl_str( c+cc  +1,i5)
;			  l_o = sl_str( l+lc  +1,i5)
;			  f_o = sl_str(nf+fcl +1,i4)
;			  if s_out  eq 0 then begin
;			     if nz  eq 1 then vl = erey(ccp,llp) else $
;					      vl = erey(ccp,llp,nfp)
;			     bb=sl_cv(vl,vo,f,g,cpx)  & fmx=fmt
;			     if (spt gt -2) and (spt ne 1) and (f_fg(13)) $
;					   then  sl_box,2
;			  endif  else  begin
;			     if s_out eq 1 then c_o=s_o else $
;			     if s_out eq 2 then l_o=s_o else $
;			     if s_out eq 3 then	f_o=s_o else $
;			     if s_out eq 4 then	begin
;						c_o=s_o & l_o=s_o & endif else $
;			     if s_out eq 5 then	begin
;						c_o=s_o & f_o=s_o & endif else $
;			     if s_out eq 6 then	begin
;						l_o=s_o & f_o=s_o & endif
;			     if nnz gt 1     then $
;			      if s_out eq 1  then vl=ov_sum1(l,nf) else $
;			      if s_out eq 2  then vl=ov_sum2(c,nf) else $
;			      if s_out eq 3  then vl=ov_sum3(c,l)  else $
;			      if s_out eq 4  then vl=ov_sum4(nf )  else $
;			      if s_out eq 5  then vl=ov_sum5(l  )  else $
;			      if s_out eq 6  then vl=ov_sum6(c  )
;			     if nnz eq 1     then $
;			      if s_out eq 1  then vl=ov_sum1(l)    else $
;			      if s_out eq 2  then vl=ov_sum2(c)
;			     bb=sl_cv(vl,vo,1,g,cpx)
;			     bb=sl_gf(   vo,1,g,fmx)		& endelse
;			  bb=sl_sti(explv,sl_str(vo,fmx),6)
;			  bb=sl_sti(explv,c_o,19)
;			  bb=sl_sti(explv,l_o,25)
;			  bb=sl_sti(explv,f_o,37)
;;**
;;			  if  f_vu eq 0 then bb=sl_working(inc,entitl,1,explv) $
;;					else waits=0.

			  bb=sl_vecfun(-4,0,0,0,0,0,erey,vsiz,ccp,llp,nfp,$
							 f_fg(1),f_fg(2))

			  if  f_vu ne 0 then $
			  if  f_vu eq 7 then bb=sl_funn(f_vu,erey,vsiz ,  $
				      ccp,llp,nfp,f_fg(1),f_fg(2) ,explv) else $
			  if s_out eq 0 then bb=sl_funn(f_vu,erey,vsiz ,  $
				      ccp,llp,nfp,f_fg(1),f_fg(2) ,explv) else $
			  if s_out eq 1 then bb=sl_funn(5   ,erey,vsiz ,  $
				      cp+ccl/2,llp,nfp,ccl+2,ccl+2,explv) else $
			  if s_out eq 2 then bb=sl_funn(4   ,erey,vsiz ,  $
				      ccp,lp+lcl/2,nfp,lcl+2,lcl+2,explv) else $
			  if s_out eq 3 then begin i=f_fg(8)  &  f_fg(8)=1
					     bb=sl_funn(4   ,erey,vsiz ,  $
				      ccp,llp,fp,fcm+2,fcm+2,explv)
					f_fg(8)=i  & endif $
			  else		     bb=sl_funn(6   ,erey,vsiz ,  $
				      ccp,llp,nfp,f_fg(1),f_fg(2), explv)

			if ot ne 3 then  f_fg(45)=0
			ot= 0
			endif else begin f_fg(45)=0
			      if ((spm eq 6) or  (spt eq -3)) $
			      and (clop gt 500) then bb=sl_rotfun(1,w,f_fg(18))
			endelse
;**
			x1=x & y1=y & xd=-1 & yd=-1  & zerr=0
;**     Interactive flag
			if  (inc eq  7) then f_fg(37)=2   else $
			if ((inc ge  0) and (inc  le  3))   or $
			                    (inc  eq  8)    or $
			   ((inc ge 11) and (inc  le 17))   or $
			    (inc eq 19) or  (inc  eq 21)    or $
			    (inc eq 22)			  then f_fg(37)=1 $
							  else f_fg(37)=0
;**	Pan mode
			if (f_fg(38) ge 1) then if (f_fg(37) gt 0) then  begin
			 if dif then begin
				bb=sl_tvgcur(x,y,zerr,0)
				if (zerr eq 0) and $
				   ( (x ne x1) or (y ne y1) ) then f_fg(38)=2 $
				else if f_fg(38) eq 2 then   begin f_fg(38)=1
					bb =sl_working(inc, entitl,0) & endif
			 endif else f_fg(38)=0
			endif
;**     Maxi or wanted position
			if f_fg(32) eq 2  then  begin
				    f_fg(32)=0    & ot=2
 				    f_cn =f_cn-cp & f_ln=f_ln-lp & nf=f_zn-fp
				    sl_dc,f_cn    , f_ln ,x,y
				    bb=sl_tvmcur(2,x,y)
				    if fqc then begin
					i= fix(f_cn*fcx)  & j= fix(f_ln*fcy)
					bxy(4)=f_cn-i/fcx & bxy(5)=f_ln-j/fcy
			endif   &   endif
;**	Signal
			stat=0  &   catch,stat
			if stat ne 0 then begin catch,/cancel & print,!err_string & endif

			if  (not f_fg(27)) then j=1 else j=0
			sl_signal,x,y,fcx,fcy   ,rti ,j  ,ot
			while (x ne xd) or (y ne yd) do begin
		  			 xd=x    & yd=y
					 bb=sl_tvgcur(x,y,i,0)
					 if i  ne 0  then begin
					    bb=sl_tvshap(58)
					    bb=sl_zoomi(x,y,xd,yd)
;**					    Zoom...
					    if bb eq 1  then begin i=0
						if f_fg(37) ne 0 then f_fg(35)=6
						endif
;**					    Size box...
					    if bb eq -2 then begin f_fg(35)=7
								  i=0 & endif
					    if (bb eq -1) or (bb eq -3) then i=0
					    bb=sl_tvnobut(0)
					    bb=sl_tvshap(-1)
					    if (inc ne 0) or (i ne 4) then zerr=i
					endif else begin
					    xd=x & yd=y
					endelse
			endwhile
;
			if vik lt 0 then $
			   if inc eq 0 then begin zerr=-vik & vik = 0 & endif
;
			bb=sl_tvget(3,j)
;**     Cursor out or window just closed.
			if (j ne w) or (x lt 0)  then begin
				f_fg(45)=1
				bb=sl_x('focus_reset')
				if ros  then begin x=x1 & y =y1
				endif   else begin
					bb =sl_vecfun(-3,1,0,ex_t3)
					if tv_flg(1) eq 0 then $
					   j=sl_chk_win(-2,xd,yd,i,tv_flg(5))
					ij= 0
					x =-1
;					Loop until cursor in or keyboard pressed
;					---  ----- ------ -- -- -------- -------
					while (x lt 0) and (rti le 0) do begin

					  if x  ne -2 then $
					  	bb=sl_tvwait(.5, 1,1,-w-1 ,xd,yd)

;**					  Test scroll handies if not motif
;**					  ---- ------ ------- -- --- -----
					  if tv_flg(1) eq 0 then begin
					  bb=sl_tvsel(3)
					  if bb eq 1 then begin
					   bb=sl_tvgcur(x,y,j,0)
					   if x ge 0 then begin
;					    bb=sl_tvfont(1)
					    if i_tdx gt 2 then $
					     j=sl_tvmenuh(3,6,$
					      [exspc(exsi:exsj),$
					       i_trout(0:i_tdx),exsph],$
					       ex_sp,(tv_x-200)/tv_dx,tv_y/tv_dy) $
					    else $
					     j=sl_tvmenuh(3,6,$
					      [exspc(exsi:exsj),exsph],$
					       ex_sp,(tv_x-200)/tv_dx,tv_y/tv_dy)
					    if j eq 1 then rti=33
					   endif else ij=1
					  endif
					  endif
;**					  Test option menu or widget_button
;**					  ---- ------ ---- -- -------------
					  i=0
					  if tv_flg(1) eq 0 then begin
					   bb=sl_tvsel(1)
					   if bb eq 1 then begin
					      bb=sl_tvgcur(x,y,j,0)
					      if x ge 0 then begin
						ij =0
						i  =sl_tvmenu(1,7,expex,$
						    ex_ex,tv_x/tv_dx,0)
					      endif  else ij=1
					   endif
					  endif else if yd eq w then i=xd
					  if i ne 0 then begin
						rti=33
						case i of
;**						 Menus..
						  32: if inc eq 0 then zerr=1 $
							 else begin    vik=-1
						         rti=27 & endelse
						 100: if inc eq 0 then zerr=1 $
							 else begin    vik=-1
						         rti=27 & endelse
;**						 Remove..
						  -1: if inc eq 0 then begin
							 zerr=1 & vin=19 & rti=24
							 endif    else begin
						         rti=24 & vik=-1
							 vin=19 & endelse
						  24: if inc eq 0 then begin
							 zerr=1 & vin=19
							 endif    else begin
						         rti=27 & vik=-1
							 vin=19 & endelse
						 102: if inc eq 0 then begin
							 zerr=1 & vin=19
							 endif    else begin
						         rti=27 & vik=-1
							 vin=19 & endelse
;**						 Close..
						 -88: if inc eq 0 then zerr=4 $
							 else begin    vik=-4
						         rti=27 & endelse
						  22: if inc eq 0 then zerr=4 $
							 else begin    vik=-4
						         rti=27 & endelse
						 104: if inc eq 0 then zerr=4 $
							 else begin    vik=-4
						         rti=27 & endelse
						 106: if inc eq 0 then zerr=4 $
							 else begin    vik=-4
						         rti=27 & endelse
;**						 Zoom..
						  30: if inc eq 0 then zerr=2 $
						         else  bb =sl_handerr(4)
						 108: if inc eq 0 then zerr=2 $
						         else  bb =sl_handerr(4)
;**						 Un_zoom..
						  31: if (zerr     eq 0) and  $
							 (f_fg(37) ne 0) then begin
							  zerr=1 & vin=6
						         endif else bb=sl_handerr(4)
;**						 Convol  F12
						  34:	 sl_kbcar,string(12b)
;**						 Special ^E
						  35:	 sl_kbcar,string(5b)
;**						 Math    ^F
						  36:	 sl_kbcar,string(6b)
;**						 Fram Op J
						  37:	 sl_kbcar,'J'
;**						 Colors
						  38:	 sl_kbcar,'C'
;**						 Save..
						  23: if inc eq 0 then begin
							 zerr=1 & vin=16
							 endif    else begin
						         rti=27 & vik=-1
							 vin=16 & endelse
;**						 Print..
						  25: begin sl_box,1 & rti=2 & end
;**						 Duplicate..
						  26: if inc eq 0 then begin
							 zerr=1 & vin=17
							 endif    else begin
						         rti=27 & vik=-1
							 vin=17 & endelse
;**						 Log..
						  40:	 sl_kbcar,'N'
;**						 Smooth..
						  41:	 sl_kbcar,'O'
;**						 Slice
						  42:	 sl_kbcar,'/'
;**						 Stop slice
						  420:	 sl_kbcar,'/'
;**						 Square
						  43:	 sl_kbcar,'q'
;**						 Surface
						  44:	 sl_kbcar,'S'
;**						 Levels
						  45:	 sl_kbcar,'L'
;**						 Image
						  46:	 sl_kbcar,'I'
;**						 Other F11
						  48:	 sl_kbcar,string(11b)
;**						 Params
						  49:	 sl_kbcar,'M'
;**						 Update
						  50:	 sl_kbcar,'u'
;**						 Rescale..
						  61:    if f_fg(37) eq 1 $
							 then begin
							 if inc eq 3 then zerr=2 $
							 else f_fg(35)=3
							 endif
;**						 G_H image
						  71:	 sl_kbcar,'i'
;**						 G_H levels
						  72:	 sl_kbcar,'l'
;**						 G_H surface
						  73:	 sl_kbcar,'s'
;**						 G_H x
						  74:	 sl_kbcar,'x'
;**						 G_H y
						  75:	 sl_kbcar,'y'
;**						 G_H r
						  80:	 sl_kbcar,'r'
;**						 G_H n
						  81:	 sl_kbcar,'n'
;**						 G_H o
						  82:	 sl_kbcar,'o'
;**						 G_H p
						  83:	 sl_kbcar,'p'
;**						 G_H e
						  84:	 sl_kbcar,'e'
;**						 G_H #
						  85:	 sl_kbcar,'#'
;**						 G_H |
						  86:	 sl_kbcar,'|'
;**						 G_H _
						  87:	 sl_kbcar,'_'
						else:begin  rti=0
						     if (i ge 1000) then begin

						     if (i ge 1000) and  $
							(i le 1100) then begin
;**						     Low  scale..
						      rvl=vf+(vm-vf)*(i-1000)/100
						      if rvl gt rvm then rvm=vm
						      endif else  $
						     if (i ge 2000) and $
							(i le 2100) then begin
;**						     High scale..
						      rvm=vf+(vm-vf)*(i-2000)/100
						      if rvm lt rvl then rvl=vf
						     endif

						     bb=sl_tvwmaj(w,vf,vm,rvl,rvm,$
						           f_fg,f_vu,spt,f_ax,f_az)

						     endif
						     end
						endcase
					  endif
;**
;**					  Test w always present
;**					  ---- - ------ -------
					  bb=sl_tvsel(w)
					  bb=sl_tvget(3,j)
					  if (j eq w)   then begin
					   if rti le 0  then begin
						bb=sl_tvgcur(x,y,i,0)
						if x ge 0 then ij=0 else ij=1
						if i ne 0 then x=-1 $
						else begin
;**     					  Test resize
;**						  ---- ------
 			 			  bb=sl_tvget(28,i)
			 			  bb=sl_tvget(29,j)
			 			  if  i gt 0 then $
			    			  if (bxy(12) ne i) $
			    			  or (bxy(13) ne j) $
						  then x=0
						endelse
					  	sl_signal,0,0,0,0,rti,2,ot
					   endif else  x=-1
					  endif else rti=24
;**					  Test Window selection
;**					  ---- ------ ---------
					  i  = 0
					  j  =-1
					  if rti le 0  then $
					  if tv_flg(1) eq 0 then begin
					   if ij gt 0 then begin
					      ij = 0
					      if rti  le 0 then $
					        j=sl_chk_win(w,xd,yd,i,tv_flg(5))
					   endif
					  endif else begin
					     if (xd eq 21) or (xd eq  1) or   $
							      (xd eq -1) then $
						if yd ne w then begin
						 for k=0,tv_wsz(1)-1 do $
						 if (tv_win(0,k) eq yd) then j=k
						 if  xd ne -1 then i=1  $
						 else   bb=sl_purge(j)
						endif
					  endelse
					  if i eq 1 then begin
						  rti=33
						  if inc eq 0 then zerr= 4 $
						  else begin
						  vik=-4 & rti=27 & endelse
						  if j ge 0 then recurs=-j-1
					  endif
					  if x ge 0 then begin
					     bb=sl_tvwait(0.01, 0,1,-w-1 ,xd,yd)
					     if xd gt 0 then x=-2
					  endif
					endwhile
;
					bb =sl_tvwait(0.01, 0,1,-w-1 ,i,0)
					bb =sl_ioclear(0)
		 			bb=sl_tvsel (w)
					bb =sl_tvget(3,j)
					if j   ne w  then begin rti=24 & vin=19
					    if inc eq 0 then zerr=1 else vik=-1
					endif
					if rti eq 24 then inc=0 $
					else begin
					 bb =sl_vecfun(4,2)
					 bb =sl_vecfun(5,2)
 					 if f_vu gt 0 then begin bb=sl_tvsel(f_w1)
					   if bb then bb=sl_tvpop(f_w1,1) & endif
					 bb=sl_tvsel (w)
					endelse
				endelse
				if (rti gt 0) and (rti ne 24) then    begin
						x=x1 & y =y1
						bb=sl_tvmcur(2,x,y) & endif
			endif
;**
;	Window size changed !
			if (j eq w) and (f_fg(45) ne 0) then begin
 			 bb=sl_tvget(28,xd) & bb=sl_tvget(29,yd)
			 if xd gt 0 then begin
			    vd=float(bxy(12))
			    vl=float(bxy(13))
			    if (xd ne vd) or (yd ne vl) then begin
 				f_fg(6)=2
				sl_box,0
;**				update or abort.
;**				------ -- -----
;				if f_fg(37) ne 0 then begin
				 f_fg(35)=3
;**				 Resize.
;**				 ------
				 if xd gt tv_x then xd=tv_x
				 if yd gt tv_y then yd=tv_y
				 i =sl_pfix((xd-vd + yd-vl) /(ccl + lcl +2))
				 if i eq 0 then if (xd lt vd) and ( yd lt vl) $
						then i=-1  $
					   else if (xd gt vd) and ( yd gt vl) $
						then i= 1
				 tv_win(19,windn)=tv_win(19,windn) - i
				 if tv_win(19,windn) lt 0 then begin
				    if f_fg(43) eq 0 then f_fg(43)=1
				    tv_win(19,windn) =0
				    i=xd
				    j=yd
				    if (ccl+1 le xd) and (lcl+1 le yd) and $
				       (fxy   ge -1) then begin
					 i=i/2 & j=j/2  & endif
;
				    if  ccl+1   gt   i then begin
					cc=cc+(ccl - i)/2 & fcg(0)=i  & endif
				    if  lcl+1   gt   j then begin
					lc=lc+(lcl - j)/2 & fcg(1)=j  & endif
				    f_fg(35)=5
				 endif
;**				 Square off.
;**				 ------ ---
				 if vd ge vl then begin
						  vl=sl_pfix(float(xd)/yd)
						  vd=sl_pfix(vd/vl)
				 endif else begin vl=sl_pfix(float(yd)/xd)
						  vd=sl_pfix(vl/vd) & endelse
				 if (f_fg(10) eq 1) and (vl ge 2) then f_fg(10)=0
;				 if (f_fg(10) eq 0) then $
;				       if (vd ge 2) and (vl lt 2) then f_fg(10)=1
;				endif else  rti=27
			    endif
			 endif else sl_box,0
			endif
;** ---
;** ---
			if (zerr ne 0) or (rti gt 0) then f_fg(45)=1
;** ---
;** ---
;**     Treat signal
			if (rti gt 0) or (f_fg(42) eq 3) then begin
				bb=sl_trsig(zerr,inc,vin,mfi,ot,rti)
				if f_fg(42) eq 3 then f_fg(42)=0
				bb=sl_curset(exspc,exsi,exsj, fxy,nnz)
				if bb eq 1 then if (tv_flg(7) eq 0) $
						or (i_tdx gt 2) then begin
				   tv_flg(7)=0
;				   bb=sl_tvfont(1)
				   if i_tdx gt 2 then $
			 	    i=sl_tvmenuh(-3,-24,[exspc(exsi:exsj),$
					 i_trout(0:i_tdx),$
				         exsph],ex_sp,(tv_x-200)/tv_dx,tv_y/tv_dy) $
				   else $
			 	    i=sl_tvmenuh(-3,-24,[exspc(exsi:exsj),$
				         exsph],ex_sp,(tv_x-200)/tv_dx,tv_y/tv_dy)
				   bb=sl_tvpop (w,1)
				   if f_vu gt 0 then bb=sl_tvpop(f_w1,1)
				   bb=sl_vecfun(-1,1)
;				   bb=sl_tvfont(0)
				endif
			endif
;**	Index
			if  (x ne x1)   or  (y ne y1)    then f_fg(52)=1 $
							 else f_fg(52)=0
			if ((x ge plnx) and (y ge plny)) then begin
				x=x1 & y =y1
;				if not ros then bb= sl_tvmcur(2,x,y)
				endif
			if  (x ne x1) or (y ne y1) or (fqc) then begin
			 s_out=0 & i=x & j=y
			 if   x ge plnx then  begin s_out =1 & x=plnx-1 & endif
			 if   y ge plny then  begin s_out =2 & y=plny-1 & endif
			 if  s_out gt 0 then  if (spt ne -3) and (spm ne 6)   $
					then  s_out=0
			 y1	 = plny -y -1
			 xd	 = x  / plx
			 yd	 = y1 / ply
			 x1	 = x  - xd * plx
			 y1	 = y1 - yd * ply
			 if tv_od  eq 0 then y1 = ply - y1 - 1
			 l	 = fix(y1 / fcy)+ bxy(5)
			 c	 = fix(x1 / fcx)+ bxy(4)
			 nf	 = ny * xd + yd
			 x=i  & y= j
			 if nf gt  fcm then nf = fcm
;**
			 provs,  erey
;;**			 Special view.
;;**                     ------- ----
;;**			 Flick....
;;**			 Surface indices..........
;;**			 Proj.3D indices..........
;;**			 Spacial indices..........
;;**			 Reduced indices..........
			endif
;**
			if  not ros    then  begin
;**			other signal input........
			if (rti eq  7) or (rti eq  8)  then begin
				f_fg(45)=0
				if  spt gt -2 then begin
				    if rti eq 7    then nf=nf-1 else nf=nf+1
				    if  nf lt 0    then nf=fcm
				    if  nf gt fcm  then nf= 0
				    sl_dc,  c,l ,   x,y
				    bb=sl_tvmcur (2,x,y) & endif
				rti=0 & endif
;** ---
;** ---
			if (rti gt 0) or (zerr ne 0) or $
				     (f_fg(39) eq 2) or (f_fg(42) eq 2) then $
				      bb=sl_x('focus_out') $
                                 else bb=sl_x('focus_in')
;** ---
;** ---
			if rti  gt 0 then begin
			    if (rti eq  2) or (rti eq 3) then begin
				bb=sl_working(inc,entitl,1,expli)
				if (rti eq 3) and (f_vu gt 0) then begin
				    if f_fg(33) eq 2 then i=0 else i=f_fg(33)
				    bb= sl_tvsel(f_w1)
				    if  bb eq 1 then $
					bb=sl_hardc(f_bx,f_wy,0,0,6,0,vsiz,0,$
							 i,f_fg(51),f_fg(0))
				    bb= sl_tvsel(w)
				endif
				if (rti eq 2) then $
					bb=sl_hardc(bxy(12),bxy(13),num,spt ,$
					        spm,erey,vsiz,windn,f_fg(33),$
							 f_fg(51),f_fg(0))
				ot=1 & endif
			    if (rti eq 15) then begin
				bb =sl_vecfun(-1,0)
				bb =sl_iotype(ex_t3,0,0)
				if f_vu gt 0 then bb =sl_tvpop(f_w1,1)
				rti=0
				while rti ne 15 do begin
					bb=sl_wait(.5)
					sl_signal,0,0,0,0,rti,2,ot & endwhile
				i  =sl_vecfun(4,2)
				bb =sl_working(inc, entitl,0)
				bb =sl_ioclear(0)
				f_fg(45)=1
				inc=inc+50  & zerr=1
 				if f_vu gt 0 then begin  bb=sl_tvsel(f_w1)
				   if bb then bb =sl_tvpop (f_w1,1) & endif
 				bb =sl_tvsel (w)
				if bb eq 1 then begin
; 				   bb=sl_tvwake(w)
				   bb=sl_tvpop (w,1)
                                   bb=sl_tvmcur(2,x,y)
				endif	   else begin
;				Sabotage.
				   sl_box,0
				   inc=0 & vin=19 & zerr=1 & endelse
			    endif
			    if (rti eq 33) then if inc eq 2 then inc=0
			    rti=0
			endif
;;**
			if  (c ne xs) or (l ne ys) or (nf ne zs)    then  begin
			     if (f_vu ne 7) then   ot=2   else ot=1
			     if (nf eq zs)  then   ot=2
			     xq =xs & yq  =ys & zq = zs
			     xs = c & ys  = l & zs = nf & clop=0  & endif $
			else if  clop gt 2000 then begin
			      if (f_fg(45) ne 0) or (ot ne 0) then clop=0 else $
			      if (spm  ne 6) and (spt ne -3)  then $
				   bb=sl_tvwait(.5, 1,2,w ,i,0) else clop=0
			endif else begin
				   bb=sl_tvwait(0., 0,1,w ,i,0)  &   clop=clop+1
			endelse
;**end ros
			endif
;**pan accepted
			if f_fg(38) eq 2 then begin
				ired(0)=c +cp
				ired(1)=l +lp
				ired(2)=nf+fp
				sl_pan, ired(0),ired(1),ired(2),$
					cc,lc,fcl, fcg,vsiz,  bb
				if bb then begin f_fg(35)=5
						 inc=inc+100
				endif else ired(0)=-1
			endif
;**
;**			Widget MAJ
;**			------ ---
			if f_fg(45) ne 0 then $
			bb=sl_tvwmaj(w,vf,vm,rvl,rvm,f_fg,f_vu,spt,f_ax,f_az)
;**
			zerr=zerr+ 10 * inc
			endelse
;**     Tidy.
;**	----
			if zerr  eq 4 then c = -1 else $
			if zerr  eq 1 then begin
;**	Image control.
;**	----- -------
			    provc,windn,erey
;;**		Change table.
;;**		------ -----
;;**		Lower & upper color limit.
;;**		----- - ----- ----- -----
;;**		Rescale.
;;**		-------
;;**		Slice.
;;**		-----
;;**		Reset view.
;;**		----- ----
;;**		Overviews.
;;**		---------
;;**		Settings.
;;**		--------
;;**		Other graphics.
;;**		----- --------
;;**		Image processing.
;;**		----  ----------
;;**		Data  processing.
;;**		----  ----------
;;**		Math  functions .
;;**		----  ----------
;;**		Save this work.
;;**		---- ---- ----
;;**		Duplicate the view.
;;**		--------- --- ----
;;**		Remove the view.
;;**		------ --- ----
;;**		Insert.
;;**		------
;;**		Molecule.
;;**		--------
			    endif else $
			if zerr  eq 2 then begin
;**	expand control.
;**	------ -------
			    provj,0
			endif
;**	Graphics control.
;**	-------- -------
                        if (zerr eq 5) or (f_fg(35) eq 4) then begin
				provk,windn
;;**			Defaults menu.
;;**			-------- ----
;;**			Special views.
;;**			------- -----
;;**			Mapped contour.
;;**			------ -------
;;**			Surface.
;;**			-------
;;**			Projections.
;;**			-----------
;;**			Deep.
;;**			----
;;**			Vectors.
;;**			-------
;;**
;;**			Turn 180°.
;;**			--------
;;**			Resize.
;;**			------
;;**			Image rotations.
;;**			----- ---------
;;**			Video.
;;**			-----
			endif
;**	Data control.
;**	---- -------
			while (zerr ge 6) and  (zerr le 9)  do  begin
			    provl,windn ,erey ,xsiz
;;**				Save before
;;**				---- ------
;;**				Cut  before
;;**				---  ------
;;**				D_P  entry
;;**				---  -----
;;**			Previous data.
;;**			-------- ----
;;**			Reduce.
;;**			------
;;**			Mean	filter.
;;**			Median  filter.
;;**			Unsharp masking.
;;**			------- -------
;;**			Rescale.
;;**			-------
;;**			Correlate.
;;**			---------
;;**			Frequency.
;;**			---------
;;**			Menu math.
;;**			---- ----
;;**			X Y mixed salad.
;;**			---------------
;;**			Transpose X Y.
;;**			-------------
;;**			sum over frames.
;;**			X   projections
;;**			Y   projections
;;**			--- ---- ------
;;**			Menu miscela.
;;**			---- -------
;;**			Stand Dev.
;;**			----- ---
;;**			Compaction.
;;**			----------
;;**			sum each frames.
;;**			--- ---- ------
;;**			Shift.
;;**			-----
;;**			Subtract.
;;**			--------
;;**			Add.
;;**			---
;;**			line up X direct.
;;**			---- -- - ------
;;**			line up Y direct.
;;**			---- -- - ------
;;**			line up on maxi.
;;**			---- -- -- ----
;;**			Take  values.
;;**			----- ------
;;**			Scrolling filter.
;;**			--------- ------
			endwhile
;**
;**	Interactive control.
;**	----------- -------
			provit,erey, windn,xsiz
;;**	10		stretching (down).
;;**			----------  ----
;;**	20		stretching (up).
;;**			----------  --
;;**	30		Rescaling.
;;**			---------
;;**	40		Reducing scan.
;;**			-------- ----
;;**	50		Reducing frame V.
;;**			-------- ----- -
;;**	60		Reducing frame H.
;;**			-------- ----- -
;;**	70		Image rotating.
;;**			----- --------
;;**	80		Shifting.
;;**			--------
;;**	90		Expanding a region.
;;**			--------- - ------
;;**	110		Adding.
;;**			------
;;**	120		Subtracting.
;;**			-----------
;;**	130		Taking values.
;;**			------ ------
;;**			    Close.
;;**			    Back.
;;**			    Show selection
;;**			    Take value or mean
;;**			    Radius
;;**			    Center
;;**			    take region
;;**			    Create image of values
;;**	140		Slicing.
;;**			-------
;;**	150		scrolling Filter.
;;**			--------- ------
;;				Stop
;;				Change point
;;				Change region (clear,avg,minim)
;;				Subtract avg
;;				Update image
;;				Store average
;;**	160		Fitting.
;;**			-------
;;**	170		Flicking.
;;**			--------
;;**	180		Filtering in frequencies.
;;**			--------- -- -----------
;;**	190		Magnifying.
;;**			----------
;;**	200		Tampo
;;**			-----
;;**	210		Taking reflexions.
;;**			------ ----------
;;**	220		Radial integration.
;;**			------ -----------
;;**	35		Resetting.
;;**			---------
;;**	37		Display.
;;**			-------
;;**	38		Context change.
;;**			------- ------
;;**	39		Data change.
;;**			---- ------
;**	Reset interactive
;**	----- -----------
			if zerr eq 3  then   begin
				      bb=sl_sti(explr,'         '   ,18)
			              bb=sl_sti(explr,'         '   ,33)
				      bb=sl_sti(explo1,'     '	    ,15)
			              bb=sl_sti(explo1,'     '	    ,25)
			              bb=sl_sti(explo1,'     '	    ,37)
				      bb=sl_sti(explo3,'    '	    ,9 )
			              bb=sl_sti(explo3,'     ,     ',18)
			              bb=sl_sti(explo3,'     ,     ',32)
				      i =sl_vecfun(4,0,expl,ex_l,tv_xp,tv_yp)
				      explm(1)   =   explz &  ot=1
				      f_fg(24)=0 &   f_fg(28)=0 & ros=0
				      f_fg(49)=0 &   f_fg(50)=0
				      if f_fg(6) ne 2 then begin zerr=1
								 inc=50
				      endif else begin zerr=37 & inc=0 & endelse
			endif
;
			provz,windn,erey,rvl,rvm
;**	Common interactive
;**	------ -----------
;**	Zoommm
;**	------
;**			Zoom frame.
;**			---- -----
;**				   Square off.
;**				   ------ ---
;**				   Limits.
;**				   ------
;**				   Keep 16 points.
;**				   ---- -- ------
;**				   Zoom.
;**				   ----
;**			Scan reduction.
;**			---- ---------
;**	 Zoommm  continue.
;**	 ------  --------
;**			Reset.
;**			-----
;**			Reduce frames V.
;**			------ ------ -
;**			Reduce frames H.
;**			------ ------ -
;**			Expand a region.
;**			------ - ------
		       if (zerr eq 2 )  or  (zerr eq 32)  or $
			 ((zerr ge 35)  and (zerr le 39)) then begin
			  bb=sl_x('focus_out')
			  c 	  =-1
			  if w gt 0 then sl_box,1
;**
;**		Special transform.
;**		------- ---------
		          if zerr eq  2  then  fdp=1
			  if  fdp or fdd then  bb=sl_dd(2,arec,arec_z)
			  if  fdd or (fdp and  zerr ne 32)  then f_fg(44)=-1
			  if zerr eq 35   and (fcg(1) eq 1) then spt=2
			  if (spm eq  6)  and (fcg(1) eq 1) then spm=0
			  if		      (fcg(0) eq 1) then spt=0
			  if fcg(2) eq 1 then  begin
			   if (spt le -2) and (spt gt -6)   then spt=0
			   f_fg(46)=0
			  endif
			  if (spt eq -3)  and (zerr ne 32)  then rvm=rvl
;
			  if w   gt 0 then $
			  if vik eq 0 then bb=sl_working(0,ex_p2,1,expli) $
				      else bb=sl_tvshap(50)
;
			  if windn   lt 0 then begin w_num(0)=-windn-1
			    if vik   eq 0 then $
			     if (sdt gt 0) and (fdd eq 1)  then $
				bb=sl_savarea(4,erey,w_num,vsiz,w_num(0)) else $
			     if spc eq -2 then  $
				bb=sl_savarea(3,arec,w_num,arec_z)
			  endif
;
			  if (spt le  0) then spc=10*spt-spm else spc=10*spt+spm
;**		Do it and loop.
;**		-- -- --- ----
			  if  zerr ne 38 then  begin
			    bb=sl_views(erey,windn,ttlv,cc,lc,spc,fcl,fcg,vsiz)
			    if bb eq 1   then  begin
				if (sdt gt 0) and (fdd eq 1) then $
					 tv_win(20,windn)=2
				recars=1   &   endif
			  endif   else  begin
			    f_fg(0)=-1
			    if vik eq 14 then ttlv=ex_gh(2) else $
			    if vik eq 21 then ttlv=ex_gh(1) else $
			    if vik eq 22 then ttlv=ex_gh(0)
			    bb=sl_views(ares,windn,ttlv,cc,lc,spc,fcl,fcg,ares_z)
			    if bb      ne 1 then begin  recars=1
							windn=w_num(0)
			    endif else $
			    if(vik eq 0) and ((spm eq 0) or (spm eq 9)) $
					    then begin  recurs=3
							tv_win(20,windn) =2
			    endif else begin
				recars=1
				i=w_num(0)
				w_num(0)= windn
				tv_win(20,windn) =1
				bb=sl_savarea(4,ares,w_num,ares_z,windn)
				bb=sl_dd(2,ares,ares_z)
				windn=i
				rvl=vf & rvm=vm
			    endelse
			  endelse
			  zerr=0
		       endif
;**
		       k=zerr/10 & k=zerr-k*10
		       if (k ne 0) and (inc lt 50) then begin inc=inc+50
							tv_win(18,windn)=tv_col
                                                        endif
		endrep until c lt 0
	endrep  until (not recars)
;	sl_colexp,-1
	bb=sl_vecfun(0)
	bb=sl_tvdmenu(1)
	bb=sl_tvdmenu(3)
	if (recurs le 0)  then begin
		if w gt 0 then begin
		 if tv_flg(1) ne 1 then begin
		  if f_fg(13) eq 1 then bb=sl_settings(mfi,0,0,zerr,inc,50-40)
		  if recurs   eq 0 then bb=sl_funn(0)
		 endif
;**		 Special exit
		 if spt eq -2   then   bb=sl_savarea(3,arec,w_num,arec_z)
		 if c eq -2 then begin tv_win(20 , windn)=0
				       bb=sl_purge(windn) & endif
		endif
	endif

	if w gt 0  then begin
			bb=sl_tvsel(w) & bb=sl_tvget(3,j)
			if (j eq w) then begin
	  		    sl_box ,1  & bb=sl_tvtidy(w,1) & endif
	endif
;	if keep_col ne  tv_col then   sl_manycol,keep_col
	f_fg(45)=1
	bb=sl_tvxyz(0,0,0,0)
return,1
end
;
;
;
function sl_viewer,  erey, windn , vsiz , flg
;******* *********   ****  *****   ****   ***
;**
;** Entry allowing context change.
;** ----- -------- ------- ------
;**
	common my_viewer, v_xsiz,v_loop,v_wndn,v_wntv,v_rec,w_cw,v_x1,v_x5
;**
	common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common my_area,	ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
	common my_keep,	rvl,rvm,vlt,vmt
;**
bb=0
;**	Any param. must be ok
;**	--- -----  ---- -- --
;sl_tp
	v_xsiz(0)=vsiz
	if v_xsiz(0)   eq 0 then  v_xsiz(0) =sl_size(erey)
	v_wndn	 =windn
	if  (v_wndn    ge 0) and (v_wndn     lt tv_wsz(1)) then $
	if ((v_xsiz(0) gt 1) and (v_xsiz(0)  le 4))  $
			     or (tv_win(20,v_wndn) gt 0)   then begin
;**
	v_wntv(0)=v_wndn
	v_wntv(1)=tv_win(11,v_wndn)
	bb	 =sl_tvget(3,w_cw)
	v_rec	 =0
	v_loop	 =flg
	rvl	 =0
	rvm	 =0
;**
	while   v_loop gt 0 do begin
	   if (tv_win(20,v_wndn) gt 0) then begin bb=1
			if v_loop eq 1 then begin
			   	   i =sl_tvmenunw(5,0,v_x1,' ',3.,15.)
				   bb=sl_resarea(4,areb,areb_z,0,v_wntv)
				   i =sl_tvdmenunw(5)
			endif
			if bb then bb=sl_viewr(areb,areb_z,v_wndn,v_rec)

	   endif  else begin       bb=sl_viewr(erey,v_xsiz,v_wndn,v_rec)
				   if vsiz(0)  gt 0 then vsiz(0) =v_xsiz
				   endelse
;**
	   v_wntv(0)=v_wndn
	   v_wntv(1)=tv_win(11,v_wndn)
	   v_loop=0
;**
	   if v_rec  eq 3       then begin
			 bb=sl_pp(0,ares,ares_z,areb,areb_z)
			 bb=sl_dd(2,ares,ares_z)
			 v_loop=2 &  endif
	   bb=sl_glory(-2)
	   bb=sl_vecfun(-2,0)
	endwhile
;**
	bb=sl_tvmenunw(5,0,v_x5,' ',3.,15.)
	if (flg eq 0) and (tv_win(0 ,v_wndn) gt 0) $
		      and (tv_win(20,v_wndn) eq 0) then begin
				  bb=sl_savarea(1,erey,v_wntv,v_xsiz,v_wndn)
				  tv_win(20,v_wndn)=1
	endif else if flg gt 0 then begin
	   if v_rec lt 0 then flg=v_rec
	   if (tv_win(20,v_wndn) eq 2) then  begin
				  bb=sl_savarea(4,areb,v_wntv,areb_z,v_wndn)
				  tv_win(20,v_wndn)=1
				  endif

	   bb=sl_dd(2,areb,areb_z)
	   bb=sl_dd(2,arec,arec_z)
	   bb=sl_dd(2,ared,ared_z)
	   bb=sl_dd(2,arel,arel_z)
	   bb=sl_dd(2,ares,ares_z)
	endif
	bb=sl_tvdmenunw(5)
	if  w_cw  gt 0 then bb=sl_tvsel(w_cw)
	endif
return, bb
end
;
;
;
function sl_opview,	erey,vsiz,w_1,w_2,oprt,flg
;******* *********
;**
;**	Views operations. 2:+ 3:- 4:* 5:| 6:#
;**	----- ----------    -   -   -   -   -
	common my_opview,xdm1,ydm1,zdm1,typ1,xdm2,ydm2,zdm2,typ2,tip,xi,yi,zi
;**
	common my_viewer, v_xsiz,v_loop,v_wndn,v_wntv,v_rec,w_cw,v_x1,v_x5
;**
	common my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
			tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
	common my_area ,ares  ,areb  ,arec  ,ared  ,arev  ,aref  ,arer  ,$
			ares_z,areb_z,arec_z,ared_z,arev_z,aref_z,arer_z,$
			sare  ,tare  ,vare  ,areu  ,arei  ,arex  ,arey  ,$
			sare_z,tare_z,vare_z,areu_z,arei_z,arex_z,arey_z,$
			arel  ,arel_z,aregx,aregx_z,aregy,aregy_z,areo,areo_z
;**
bb=0
	if flg eq 0 then begin
	xdm1=tv_win(21,w_1)
	ydm1=tv_win(22,w_1)
	zdm1=tv_win(23,w_1)
	typ1=tv_win(16,w_1)
	xdm2=tv_win(21,w_2)
	ydm2=tv_win(22,w_2)
	zdm2=tv_win(23,w_2)
	typ2=tv_win(16,w_2)
;**
	b   =sl_opcheck(oprt)
;**
	if (zi ge 0)  and (xi ge 0) and (yi ge 0) then  bb=1
;**
	endif else begin
;**	Restore the data
;**	------- --- ----
		v_wntv(0)=w_1
		v_wntv(1)=tv_win(11,w_1)
		bb=sl_resarea(4,tare,tare_z,0,v_wntv)
		v_wntv(0)=w_2
		v_wntv(1)=tv_win(11,w_2)
		if bb then $
		bb=sl_resarea(4,vare,vare_z,0,v_wntv)
		if bb then begin
;**		Operate
;**		-------
		   bb=sl_d_pm(tare,tare_z ,oprt, vare,vare_z  ,erey,vsiz)
		endif
		b =sl_dd(2,tare,tare_z)
		b =sl_dd(2,vare,vare_z)
	endelse
return, bb
end
;
;
;
function sl_which ,dummy
;******* ********
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common	my_which, wh_d,wh_m,wh_n,wh_s,wh_t,wh_spc,wh_tb,wh_ti
;**
common  my_sr,	sr_bb,sr_dirc,sr_dwn,sr_num,sr_spdl,sr_spt,sr_u,$
	sr_winc,sr_v2,sr_v3,sr_typ
;**
bb=0
	i=0  &  wh_ti(i)=-1
	for j=0,tv_wsz(1)-1 do $
		if (tv_win(0,j) gt 0) or (tv_win(0,j) eq -2) then begin
		k	= tv_win(13,j)/ 10 +6
		l	= tv_win(16,j)/ 2
		if  l eq 4  then l=3 else if l eq 8  then l=4 else $
		if  l eq 16 then l=5 else if l ge 32 then l=6
		wh_tb(i)='Display ' +sl_str(j,'(i2)')+wh_s  $
				    +sl_str(tv_win(11,j) ,'(i6)')+wh_d $
				    +sl_str(tv_win(21,j) ,'(i5)')+wh_m $
				    +sl_str(tv_win(22,j) ,'(i4)')+wh_m $
				    +sl_str(tv_win(23,j) ,'(i3)')+') ' $
				    +wh_spc(k)+' '+sr_typ(l)
		wh_ti(i)= j
		i	= i+1   &  endif
	if i gt 0 then begin
		wh_tb(i)= wh_n  &  wh_ti(i)= -1
		bb	= i	&  endif
return, bb
end
;
;
;
function sl_dtmp, dummy
;******* *******
;**
common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
		io_cur,io_ext,io_seq,io_str
;**
		k	= sl_iofind('*_*',io_ext(7),0,dirc)
		if k gt 0 then bb=sl_run('d','*_*',sl_stbr(io_ext(7),0),0,1)
return,k
end
;
;
function sl_menu_bg, w,fl, text,ttl, px,py, rfl
;******* **********
;**
;** Work correctly if w>0 and fl=0
;**
    common tmp_menbg, ab,bb,x,y,j
;**
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
rfl=-1 & i=0 & j=0
ab =sl_tvmenu(w,fl, text,ttl, px,py)

while  (ab lt w*100) and (rfl lt 0) do begin
	bb=sl_tvsel(w)
	if bb eq 1 then begin
	   bb=sl_tvgcur(x,y,j,0)
	   if x  ge 0 then begin ab=sl_tvmenu(w,7, text,ttl, px,py) & endif
	   if ab lt w*100  then begin
		 bb=sl_tvwait(1., 1,1,-w-1, j,i)

		 if tv_flg(1) eq 0 then bb=sl_chk_win(-1,x,y,j, -1) $
		 else if (j eq 1) or (j eq 21) then begin
			bb=-1 & j=1
			for k=0,tv_wsz(1)-1 do if (tv_win(0,k) eq i) then bb=k
		 endif

		 if j eq 1 then if bb ge 0 then  rfl=bb
	   endif
	endif else ab=w*1000
endwhile

if  ab ge w*100 then ab=ab - w*100
bb =sl_tvdmenu(w)
return, ab
end
;
;
;
pro sl_c_scan,	dummy
;** *********
;**
	common c_scan,	c_ini,c_x0,c_t0,c_t2,c_t3,c_t4,c_t5,c_x1,c_x2,c_x3 ,$
			c_x4,c_x5,c_x6,c_matdm,c_matrl,c_matdy,c_siz,c_area,$
			c_w,c_win,c_k,c_dirc,c_frm,c_pos,c_bo,c_sr
;**
	common	my_which, wh_d,wh_m,wh_n,wh_s,wh_t,wh_spc,wh_tb,wh_ti
;**
	common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
			io_cur,io_ext,io_seq,io_str
;**
    if n_elements(c_ini) eq 0 then c_ini=0
    while   c_ini ge 0   do begin
	if  c_ini eq 0 then begin
				 sl_main
				 c_ini=-1
				 bb   =sl_dtmp (0)
				 bb   =sl_tty  (1)
				 bb   =sl_glory(-1)
				 if bb eq 1 then begin
				    bb=sl_ioclear(0)
				    ii=sl_tvmenu(0,5,c_x6,'SCAN',35.,35.)
				    if (ii eq 0) or (ii eq 1) then c_ini=2
				    bb=sl_savarea(5,0,[-1],0,7)
				 endif
	endif else begin
;**	Be sure of correct state.
;**	-- ---- -- ------- -----
	bb  = sl_tvdev(1)
	bb  = sl_tvmod(0,3)
	bb  = sl_tvshap(-1)
	endelse
	bb=sl_ioclear(0)
;
	while   c_ini gt 0   do begin
		jj=sl_which  (0)
		kk=-1
                if c_ini ne 2 then begin
		   if jj le 0 then ii=sl_tvmenu(0,0, c_x0,c_t0,16.,35.) $
		   else ii=sl_menu_bg(1,0,[c_x0,wh_tb(0:jj)],c_t0,16.,35., kk)
		endif else c_ini=1
		if ii eq -1 then ii=8
		if ii ge 10 then ii=ii+200-10 else $
		if ii ge 0  then ii=ii+100
		if kk ge 0  then ii=-kk-1
		case ii of
;	Restore
;	*******
		100: begin
			if  c_siz(0) gt 0 then begin
			   bb=sl_viewer  (c_area,c_win,c_siz,0)
			   bb=sl_dd(2 ,   c_area,c_siz) & endif
			c_win=-1
			bb=sl_getentry (c_w,c_win)
			bb=sl_purge    (c_win)
			bb=sl_tvmenunw(5,0,c_x1,' ',3.,15.)
			c_dirc=''
			bb=sl_resarea(0,c_area,c_siz,c_win,0, c_dirc)
			bb=sl_tvdmenunw(5)
			ii=1
			bb=sl_viewer (c_area,c_win,c_siz,ii)
			end
;	Matx
;	****
		101: begin
			if c_siz(0) gt 0 then begin
			   bb=sl_viewer  (c_area,c_win,c_siz,0)
			   bb=sl_tvdmenu(5)
			   bb=sl_dd(2 ,   c_area,c_siz) & endif
			c_dirc=' '
			c_frm =-1
			bb=sl_matx(0,c_dirc,c_area,c_matdm,c_k,c_matdy,c_matrl,$
				   c_frm,c_pos,c_bo,c_sr, '')
			if bb eq 1   then begin
					  c_siz(0)=2
					  c_siz(1)=c_matdm(0)
					  c_siz(2)=c_matdm(1)
					  c_siz(3)=c_k
					  if c_k gt 1 then c_siz(0)=3
					  c_siz(c_siz (0)+1)=c_matrl(1,0)
					  c_siz(6)=1
					  for j=1,c_siz(0) do c_siz(6)= $
							      c_siz(6)*c_siz(j)
					  c_win=-1
			   		  bb=sl_getentry(c_w,c_win)
					  bb=sl_purge   (c_win)
					  bb=sl_put_strfile(1,c_win,c_dirc,$
						c_matdm(0),c_matdm(1),c_k ,$
						c_matrl(0),c_matrl(1),c_frm,$
						c_pos,c_bo,c_sr)
					  ii=1
					  bb=sl_viewer  (c_area,c_win,c_siz,ii)
					  endif
			bb=sl_ioclear(0)
			end
;	Supervise
;	*****
		102: super
;	Operations
;	**********
		103: if jj gt 0 then begin
			j =sl_tvmenul(0,0,c_x2,c_t2,10.,15.)
			if (j ge 2) and (j le 7) then begin
			  c_t3=c_t2
			  bb  =sl_sti   (c_t3,sl_stx(c_x2(j),8,10),2)
			  bb  =sl_tvmenun(5,0,c_x3,c_t3,3.,15.)
			  ii  =sl_tvmenul(0,0,wh_tb(0:jj),wh_t,10.,15.)
			  if ii ge 0 then c_i1=wh_ti(ii) else c_i1=-1
			  ii  =0
			  if c_i1 ge 0 then begin
			       bb=sl_sti   (c_t3,sl_str(c_i1,'(i2)'),0)
			       bb=sl_tvmenun(5,0,c_x4,c_t3,3.,15.)
			       ii=sl_tvmenul(0,0,wh_tb(0:jj),wh_t,10.,15.)
			       if ii ge 0 then c_i2=wh_ti(ii) else c_i2=-1
			       c_i2=wh_ti(ii)
			       ii=0
			       if c_i2 ge 0 then  begin
				   bb=sl_sti(c_t3,sl_str(c_i2,'(i2)'),12)
				   bb=sl_tvmenun(5,0,c_x1,c_t3,3.,15.)
				   bb=sl_opview(c_area,c_siz,c_i1,c_i2,j,0)
				   if bb then begin
				      if c_siz(0) gt 0 then begin
				         bb=sl_viewer  (c_area,c_win,c_siz,0)
				         bb=sl_dd(2 ,   c_area,c_siz) & endif
				      bb=sl_opview  (c_area,c_siz,c_i1,c_i2,j,1)
				      if bb then begin
					c_win=-1
					bb=sl_getentry (c_w,c_win)
					bb=sl_purge    (c_win)
					ii=1
					bb=sl_viewer   (c_area,c_win,c_siz,ii)
				      endif
				   endif else bb=sl_handerr(5)
			  endif & endif
			  bb=sl_tvdmenu(5)
		     endif & endif
;	Remove
;	******
		104: if jj gt 0 then begin
			j =sl_tvmenul(0,0,wh_tb(0:jj),wh_t,16.,35.)
			if j ge 0 then j=wh_ti(j)
			if j  ge 0   then  begin
				 bb=sl_purge  (j)
				 if j  eq c_win then bb=sl_dd(2,c_area,c_siz)
			endif &  endif
;	Change directory
;	****** *********
		105: begin
			if jj le 0 then j=0 else j=1
			bb=sl_wgaccept(j,c_t4 ,1,1,io_cur)
			bb=sl_stbr(io_cur,2)
			bb=sl_stdim(io_cur,j)
			io_spe=sl_stx(io_cur,j-1,1)
			if (io_spe ne ':') and (io_spe ne ']') and  $
			   (io_spe ne sys_dep('DIVIDER')) and (io_spe ne '' ) then $
							       io_cur=io_cur+sys_dep('DIVIDER')
			c_x0(5)=c_t5+io_cur
			c_matdy(0,0)=io_cur+'*'
			bb=sl_ioclear(0)
		     end
;	Overview
;	********
		107: begin
			bb=sl_trsig(8,9, 0,0,0,1)
		     end
;	Exit
;	****
		108: begin
			bb=sl_dd(2,c_area,c_siz)
			bb=sl_purge(-500)
;			bb=sl_tvend(0)
			c_ini=0
		     end
;	View
;	****
		else:if (ii ge 200) and (ii lt 300) then begin
			 j=wh_ti(ii-200)
			 if j ge 0 then  begin  ii=1
				 bb=sl_viewer(c_area,j,c_siz,ii)
			 endif & endif
		endcase
;
;	Recursive View
;	********* ****
		while ii lt 0 do begin
				 j =-ii-1
				 ii=1
				 bb=sl_viewer(c_area,j,c_siz,ii)
		endwhile
	endwhile
    endwhile
    bb=sl_tty (0)
    exit
return
end
;
pro  xtrip
;**  *****
sl_c_scan
return
end
;
pro  scan,area ,wentry ,wother
;**  ****
	common c_scan,	c_ini,c_x0,c_t0,c_t2,c_t3,c_t4,c_t5,c_x1,c_x2,c_x3 ,$
			c_x4,c_x5,c_x6,c_matdm,c_matrl,c_matdy,c_siz,c_area,$
			c_w,c_win,c_k,c_dirc,c_frm,c_pos,c_bo,c_sr
;**
n_e=n_elements(area)
if  n_e le 0 then sl_c_scan $
else begin
    		if n_elements(c_ini) eq 0 then c_ini=0
		if (c_ini le 0) then begin
				sl_main
				bb   =sl_dtmp (0)
				bb   =sl_glory(-1)
				if bb eq 1 then c_ini=1
				endif
		if (c_ini gt 0) and (n_e gt 1) then begin
				if n_elements(wentry) gt 0 then c_win=wentry $
							   else c_win=-1
				if c_win lt 0 then begin
				   c_win=-1
				   bb=sl_getentry(c_w,c_win)
				   bb=sl_purge   (c_win)
				endif
				c_siz(0)=sl_size (area)
				if c_siz(0) eq 1 then begin
		   		 bb=sl_pp(0,area,c_siz,c_area,c_siz)
		   		 bb=sl_psizm(area,c_siz,2,c_siz(1),1,c_siz(2),-1,-1)
		   		 area(0,0)=c_area(*)
		   		endif

				ii=1
				bb=sl_viewer  (area,c_win,c_siz,ii)
				if ii lt 0 then wother=-ii-1

				wentry=c_win
				endif
endelse
return
end
;
pro  super,dummy
return
end
;
;
pro sl_lampscan, flg, p1,p2, p3,p4 ,fname
;** ***********
;**
;**     tv_flg(8)  = true tv_x
;**     tv_flg(9)  = true tv_y
;**     tv_flg(10) =      tv_w
;**
;**     tv_flg(11) = new  tv_x
;**     tv_flg(12) = new  tv_y
;**     tv_flg(13) = new  tv_w
;**
;**     tv_flg(14) = base for views or 0 if root
;**     tv_flg(15) = base of  Lamp
;**
	common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		      	tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common my_glor,	f_w1,f_wx,f_wy,f_bx,f_py,f_az,f_ax,f_pl,f_ic,f_vu,f_fg,f_sh,$
		f_h1,f_h2,f_h3,f_wp,f_ib,f_tt,f_sa,f_ab,f_cn,f_ln,f_zn,f_el
;**
	common c_scan,	c_ini,c_x0,c_t0,c_t2,c_t3,c_t4,c_t5,c_x1,c_x2,c_x3 ,$
			c_x4,c_x5,c_x6,c_matdm,c_matrl,c_matdy,c_siz,c_area,$
			c_w,c_win,c_k,c_dirc,c_frm,c_pos,c_bo,c_sr
;**
	common my_keep,	  rvl,rvm,vlt,vmt

;**
;**	Lamp set new tv_x=p1 ,tv_y=p2 ,tv_w=0 size and a base p3. Lamp base = p4, Wi = fname
;**	---- --- --- -------  -------  ------ ---- --- - ---- --  ---- ---- - --  -- - -----
	if flg eq 'set_size' then begin
	 if p1 gt  0 then begin
	   tv_flg(11) 	= p1
	   tv_flg(12)	= p2
	   tv_flg(13)	= 0
	   tv_flg(14)	= p3
	   tv_flg(18)	= tv_od
	   bb= sl_tvget(7,tv_od)
	 endif
	   tv_flg(16)	= fname
	   bb=sl_tvlamp_base(1,p4)
;
;**	Scan check these new settings
;**	---- ----- ----- --- --------
	endif else $
	if flg eq 'get_size' then begin
	 if tv_flg(11) gt 0 then begin
	   tv_x		= tv_flg(11)
	   tv_y		= tv_flg(12)
	   tv_w		= tv_flg(13)
	 endif
	   p3		= tv_flg(14)
;1
;**	Scan clear these new settings
;**	---- ----- ----- --- --------
	endif else $
	if flg eq 'clear_size' then begin
	   tv_x		= tv_flg(8)
	   tv_y		= tv_flg(9)
	   tv_w		= tv_flg(10)
	   tv_flg(11) 	= 0
	   tv_flg(14) 	= 0
	   tv_od	= tv_flg(18)
;	   tv_flg(16) 	= 0
;
;**	Lamp send event
;**	---- ---- -----
	endif else $
	if flg eq 'send_event' then begin
	   bb=sl_x('focus_reset')
	   bb=sl_tvlamp_base(2,p1)
;
;**	Lamp set params
;**	---- --- ------
	endif else $
	if flg eq 'set_params' then begin
	   f_ax    =p1
	   f_az    =p2
	   f_fg(15)=p3
	   if p4(0) gt 0 then f_fg(14)=p4(0)
	   if p4(1) gt 0 then f_fg(16)=p4(1)
	   f_fg(0) =-1
;
;**	Lamp call scan for a full screen display
;**	---- ---- ---- --- - ---- ------ -------
	endif else $
	if flg eq 'scan' then begin
	   if p2 ge 0 then if p3 eq 0 then tv_win(0,p2)=0
	   if p2 lt 0 then begin c_win=-1 & bb=sl_getentry (c_w,c_win)
	   			 bb=sl_purge(c_win) & p2=c_win & endif
			   bb=sl_str_to_long( 1,fname(0),tv_win,p2,70 ,128)
			   bb=sl_str_to_long( 1,fname(1),tv_win,p2,102,64)
			   bb=sl_str_to_long( 1,fname(2),tv_win,p2,118,64)
			   bb=sl_str_to_long( 1,fname(3),tv_win,p2,134,40)
			   bb=sl_str_to_long( 1,fname(4),tv_win,p2,144,80)
	   scan, p1 ,p2 ,p4
	   if tv_win(0,p2) le 0 then p2=-1 else begin
			   bb=sl_str_to_long(-1,an_ttl1 ,tv_win,p2,70 ,128)
			   bb=sl_str_to_long(-1,an_xlab ,tv_win,p2,102,64)
			   bb=sl_str_to_long(-1,an_ylab ,tv_win,p2,118,64)
			   bb=sl_str_to_long(-1,an_zlab ,tv_win,p2,134,40)
			   bb=sl_str_to_long(-1,an_com1 ,tv_win,p2,144,80)
			   fname=[an_ttl1,an_xlab,an_ylab,an_zlab,an_com1]
			   endelse
;
;**	Lamp call views for ui display
;**	---- ---- - --- --- -- -------
	endif else $
	if flg eq 'views' then begin
	   tv_win(0,0)=0
	   rvl=rvm
	   bb=sl_views(p1,0,' ', 0,0,p2,0,[-1,0,0] )

;**	Lamp purge a window
;**	---- ----- - ------
	endif else $
	if flg eq 'purge' then begin
	   bb=sl_purge    (p1)
;
;**	Lamp call Restore
;**	---- ---- -------
	endif else $
	if flg eq 'restore' then begin
		c_win=-1
		bb=sl_getentry (c_w,c_win)
		bb=sl_purge    (c_win)
		if p2 ne -2 then begin
			bb=sl_tvmenunw(5,0,c_x1,' ',3.,15.)
			bb=sl_resarea(0 ,p1 ,c_siz,c_win,0, fname)
                        if bb eq 1 then  begin  p2=c_win & tv_win(0,c_win)=c_w
                        endif	   else		p2=-1
			bb=sl_tvdmenunw(5)
		endif else begin
			bb=sl_resarea(-1,p1 ,c_siz,c_win,0, fname)
			if bb ne 0 then begin
			   bb=sl_str_to_long(-1,an_ttl1,tv_win,c_win,70 ,128)
			   bb=sl_str_to_long(-1,an_xlab,tv_win,c_win,102,64)
			   bb=sl_str_to_long(-1,an_ylab,tv_win,c_win,118,64)
			   bb=sl_str_to_long(-1,an_zlab,tv_win,c_win,134,40)
			   bb=sl_str_to_long(-1,an_com1,tv_win,c_win,144,80)
			   p3=[an_ttl1,an_xlab,an_ylab,an_zlab,an_com1,'']
			   p3=[p3,'Xsize='+strtrim(string(tv_win(1,c_win)),2)]
			   p3=[p3,'Ysize='+strtrim(string(tv_win(2,c_win)),2)]
			   p3=[p3,'Zsize='+strtrim(string(tv_win(5,c_win)),2)]
			   if tv_win(16,c_win) eq 2  then p3=[p3,'Type is byte']
			   if tv_win(16,c_win) eq 4  then p3=[p3,'Type is integer 2']
			   if tv_win(16,c_win) eq 8  then p3=[p3,'Type is float']
			   if tv_win(16,c_win) eq 16 then p3=[p3,'Type is integer 4']
			   if tv_win(16,c_win) eq 32 then p3=[p3,'Type is double float']
			   if tv_win(16,c_win) eq 64 then p3=[p3,'Type is complex']
			   p3=[p3,'','Stretch factor='+strtrim(string(tv_win(3,c_win)),2)]
			   if tv_win(38,c_win) eq 1  then p3=[p3,'Display aspect is logarithmic']
			   i =tv_win(13,c_win)/10
			   if i eq  0 then p3=[p3,'Display representation is image']
			   if i eq  1 then p3=[p3,'Display representation is surface']
			   if i eq  2 then p3=[p3,'Display representation is vector']
			   if i eq -1 then p3=[p3,'Display representation is contour']
			   if i eq -2 then p3=[p3,'Display representation is deep']
			   if i eq -3 then p3=[p3,'Display representation is projections']
			   if i eq -4 then p3=[p3,'Display representation is 4d surface']
			endif
		endelse
;
;**	Lamp call Matx
;**	---- ---- ----
	endif else $
	if (flg eq 'matx') or (flg eq '.gel') or (flg eq '.image') $
					      or (flg eq 'pass'  ) then begin
			c_win=-1
			c_frm =-1
			c_dirc=' '
			if fname ne '' then c_matdy(0)=fname
			if flg eq 'pass'   then begin
			   c_matdm(0)=p4(0) & c_matdm(1)=p4(1) &  c_k  =p4(2)
			   c_matrl(0)=p4(6) & c_matrl(1)=p4(3)
			   c_frm     =p4(4)
			   c_pos     =p4(8)
			   c_bo      =p4(5)
			   c_sr      =p4(7)
			   endif
			if flg eq '.image' then begin
			   c_matdm(0)=1200  & c_matdm(1)=1200  &  c_k  =1
			   c_matrl(0)=0     & c_matrl(1)=4     &  c_pos=1 & c_bo=1 & c_sr=2401
			   c_frm=0
			   endif
			if flg eq '.gel'   then begin		  c_k  =1
			   c_matrl(0)=512   & c_matrl(1)=8     &  c_pos=0 & c_bo=0 & c_sr=0
			   c_frm=2
;			   ***Unix
			   c_matrl(0)=0 & c_bo=1
			   endif
			if (p2 eq -2) then c_sr=-1
			bb=sl_matx(0,c_dirc, p1 ,c_matdm,c_k,c_matdy,c_matrl,$
				   c_frm,c_pos,c_bo,c_sr, p3)
			if (p2 ne -2) and (bb eq 1) then begin
					  if fname eq '' then fname=c_dirc
					  c_siz(0)=2
					  c_siz(1)=c_matdm(0)
					  c_siz(2)=c_matdm(1)
					  c_siz(3)=c_k
					  if c_k gt 1 then c_siz(0)=3
					  c_siz(c_siz (0)+1)=c_matrl(1,0)
					  c_siz(6)=1
					  for j=1,c_siz(0) do c_siz(6)= $
							      c_siz(6)*c_siz(j)
			   		  bb=sl_getentry(c_w,c_win)
					  bb=sl_purge   (c_win)
					  tv_win(0,c_win)=c_w
					  bb=sl_put_strfile(1,c_win,c_dirc,$
						c_matdm(0),c_matdm(1),c_k ,$
						c_matrl(0),c_matrl(1),c_frm,$
						c_pos,c_bo,c_sr)
			endif
			p2=c_win
;
;**	Lamp check window selected
;**	---- ----- ------ --------
	endif else $
	if flg eq 'w_to_wind' then begin
		for k=0,tv_wsz(1)-1 do $
		if (tv_win(0,k) eq p1) then p2=k
;
;**	Lamp call Matx
;**	---- ---- ----
	endif else $
	if flg eq 'test' then begin p1=1
	if n_elements(tv_flg) lt 17 then scan,1
	if n_elements(tv_flg) ge 17 then p2=tv_flg(17)
	endif
return
end
;
;;
pro sl_main
;**********
;**
;**Device   dependencies
;********   ************
;**
common	my_tv,	tv_x,tv_y,tv_w,tv_b,tv_od,tv_nc,tv_col,tv_lst,tv_flg, $
		tv_dx,tv_dy,tv_xp,tv_yp,tv_wsz,tv_win,tv_ini,tv_mps,tv_rd
;**
common	my_io,	io_rec,io_spe,io_nam,io_dir,io_txt,io_dim,io_dima,$
		io_cur,io_ext,io_seq,io_str
;**
	tv_x	=1024
	tv_y	= 864
	tv_dx	=tv_x/33
	tv_dy	=tv_y/28
	tv_nc 	= 256
	tv_od	=   0	;!!??
	tv_mps	=   7
	io_rec	= long(128)*127*2
;**
	io_spe  = sl_sarr(2,60,1)
	io_nam  = sl_sarr(2,60,60)
	io_dir  = sl_sarr(2,60,1)
	io_txt	= sl_sarr(2,60,3)
	io_dim  = sl_larr(1,17)
	io_dima = sl_larr(1,17)
;**
;**	free 43-->49
	tv_wsz	=sl_iarr(1,2)
	tv_wsz(0)=170
	tv_wsz(1)=31
	tv_win	= sl_larr(2,tv_wsz(0),tv_wsz(1))
	tv_flg	= sl_larr(1,19)
	tv_b	=  32
	tv_col 	= 101
	tv_lst 	=   0
;**     sl_grafin,device[-1=def 0=uis 1=x 2=sun]
	sl_grafin,-1
	tv_xp	=   0.
	tv_yp	=float(tv_y-160)/tv_dy
	if tv_x ge tv_y then tv_ini=tv_x else tv_ini=tv_y
	tv_w	= tv_y/4  -26
	sl_comi  ,0
	sl_inview,0
	sl_super ,0
	tv_flg(8)	= tv_x
	tv_flg(9)	= tv_y
	tv_flg(10)	= tv_w
	tv_ini		= 3
return
end
