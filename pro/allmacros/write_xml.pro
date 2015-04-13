function write_tag, lev , tag , attr , str ,opn
;******* *********
ctrT=' '
for i=1,lev do ctrT=ctrT+' '
if tag  gt '' then if opn gt 0 then ctrT=ctrT+'<'+tag else ctrT=ctrT+'</'+tag
if attr gt '' then ctrT=ctrT+' '+attr+'> '  else if tag  gt '' then ctrT=ctrT+'> '
if str  gt '' then ctrT=ctrT+str
if opn  eq 2  then ctrT=ctrT+'</'+tag+'>'
return, ctrT
end

pro write_xml, FileName , Data , XC=x, YC=y , ZC=z , E=e  , N=n   $
;** *********
                               , PR=p, PV=pv, PAR_TXT=p_txt       $
                               , W_tit=wt  , X_tit=xt , Y_TIT=yt  $
                               , Z_tit=zt  , OTHER_TIT=ot, PAR_TXT_ALL=pa_txt $
                               , SRC=src, HIST=hist, LIM=lim, MACH=mach $
                               , DOE=doe, SNAP=snap, DATE=date, FIFI=fifi
;**
;**	Keywords:
;**		  XC       = vector of x coordinates.
;**		  YC       = vector of y coordinates.
;**		  ZC       = vector of z coordinates.
;**		  W_TIT    =   main title
;**		  X_TIT    = x axis title
;**		  Y_TIT    = y axis title
;**		  Z_TIT    = z axis title
;**		  OTHER_TIT=    sub title
;**		  N        = monitors
;**		  PR       = vector of parameter values
;**		  PAR_TXT  = string array of text associated to DATP.P (same size)
;**		  PV       = an array of any dimensions containing other parameter values
;**		  E        = the errors associated to DATA (same size)
fid=0
CATCH,stat & IF stat ne 0 then begin print,!err_string & return & endif
ON_IOERROR,mis

;******************
;**For Tests only**
;******************
if n_elements(p) eq 0 then begin s=size(Data)
   e=findgen(s(1)) & p=[1.,2.] & p_txt=['a','b'] & pv=findgen(2,3,4) & fifi=FileName
   n=[1,2,3]       & x=indgen(s(1))  & if s(0) gt 1 then y=indgen(s(2)) else y=4.5
   pa_txt=p_txt+ ' = '+string(p)     & if s(0) gt 2 then z=indgen(s(3)) else z=6.4
   wt='W' & xt='X' & yt='Y' & zt='Z' & ot='O' & src='Test'   & hist='Histoire'
   lim='This is limtx' & mach='win'  & snap=bytscl(dist(15)) & date=systime()
   endif
;*****************
;**Preliminaries**
;*****************
if wt eq '' then wt='W' & if xt eq '' then xt='X' & if yt  eq '' then yt='Y'
if zt eq '' then zt='Z' & if ot eq '' then ot='s' & if src eq '' then src='?'
myRG =[max(Data,min=mini),mini]

fileN=FileName & i=rstrpos(fileN,'.') & if i gt 0 then fileN=strmid(fileN,0,i)
fileG=fileN                           & i=strpos(FileName,fifi)>0   & lan=0
fileZ=strmid(fileG,i,60)              & fileF=fileZ+'.xml' & fileG=fileZ+'-1.png'
codx='txt' & codz='.gz'
if !version.release lt '5.3' then begin codx='xdr' & codz="" & endif

sx=n_elements(X) & sy=n_elements(Y) & sz=n_elements(Z)
S =size(Data)
xd=S(1) & dim=strtrim(string(xd),2)
	if S(0) gt 1 then begin yd=s(2) & dim=dim+','+strtrim(string(yd),2) & endif else yd=0
      if S(0) gt 2 then begin zd=s(3) & dim=dim+','+strtrim(string(zd),2)
		if sz lt zd then   Z=findgen(zd)+1 & sz=n_elements(Z)         & endif else zd=0

tiip=['ud','byte','int','long','float','double','double','string','ud','ud','ud','ud','uint','ulong','ud','ud']
tip =tiip(s(s(0)+1))
ymj =bin_date(systime()) & day=string(ymj(0),format='(I4,"-")')+string(ymj(1),format='(I2,"-")')+ $
                               string(ymj(2),format='(I2," ")')+string(ymj(3),format='(I2,":")')+ $
                               string(ymj(4),format='(I2,":")')+string(ymj(5),format='(I2)')
mon =float(N(*,0))

;*******************
;**Create XML file**
;*******************
OPENW,fid, FileN+'.xml', /get_lun
;*    ***
 str ='Written_by_LAMP: Lamp + Idl'+!version.release+' on '+!version.os   ;+' '+!version.os_family
 attr=      'file_name="'+FileF+'"'
 attr=attr+' file_time="'+day+'"'
 attr=attr+     ' user="'+src+' '+date+'"'
 PRINTF, fid, write_tag(0 , 'NXmlfile' , attr , str ,1) & PRINTF,fid,''

  if n_elements(hist) eq 1 then str=hist else str=''
  PRINTF, fid, write_tag(1 , 'NXentry'  , 'name="entry1"' , str  ,1) & PRINTF,fid,''

   attr='title="'+wt+'" sub_title="'+ot+'"'
   if n_elements(src)   eq 1 then attr=attr+' source="'+src +'"'
   if n_elements(p)     eq 1 then attr=attr+ ' param="'+strtrim(string(p(0)),2)+'"'
   PRINTF, fid, write_tag(2 , 'NXdata'  , attr  , lim  ,1) & PRINTF,fid,''

     np=n_elements(p)
     if np gt 1 then begin
      PRINTF, fid, write_tag(3 , 'Parameters' , 'lines="'+strtrim(string(np),2)+'"' ,'',1)
      for i=0,np-1 do PRINTF, fid, write_tag(4 ,'','', pa_txt(i) ,0)
      PRINTF, fid, write_tag(3 , 'Parameters'  , '' , '' , 0) & PRINTF,fid,''
     endif

     if n_elements(snap) gt 1 then begin WRITE_KIF,FileG,snap,transparent=[0] ;ii=sys_dep('GIFTRANS',FileG)
      if strpos(FileG,'.png') gt 0 then giff='type="png"'
      if strpos(FileG,'.jpg') gt 0 then giff='type="jpeg"'
      if strpos(FileG,'.gif') gt 0 then giff='type="gif"'
      attr=giff+' size="[192,192]" file_name="'+FileG+'"'
      PRINTF, fid, write_tag(3 , 'Snapshot'  , attr  , ''  ,2) & PRINTF,fid,''
     endif

     if sx gt 1 then begin sxc=size(x)  & SC=strtrim(string(sxc(1)),2)
	if sxc(0) gt 1 then SC=SC+','+strtrim(string(sxc(2)),2)
	attr='axis="1" type="float" dims="['+SC+']" units="" label="'+xt+'"'
	PRINTF, fid, write_tag(3 , 'X_coord'  , attr  , ''  ,1)
	PRINTF, fid, float(x)
	PRINTF, fid, write_tag(3 , 'X_coord'  ,  ''   , ''  ,0) & PRINTF,fid,''
     endif
     if sy gt 1 then begin sxc=size(y)  & SC=strtrim(string(sxc(1)),2)
	if sxc(0) gt 1 then SC=SC+','+strtrim(string(sxc(2)),2)
	attr='axis="2" type="float" dims="['+SC+']" units="" label="'+yt+'"
	PRINTF, fid, write_tag(3 , 'Y_coord'  , attr  , ''  ,1)
	PRINTF, fid, float(y)
	PRINTF, fid, write_tag(3 , 'Y_coord'  ,  ''   , ''  ,0) & PRINTF,fid,''
     endif
     if sz gt 1 then begin sxc=size(z)  & SC=strtrim(string(sxc(1)),2)
	if sxc(0) gt 1 then SC=SC+','+strtrim(string(sxc(2)),2)
	attr='axis="3" type="float" dims="['+SC+']" units="" label="'+zt+'"'
	PRINTF, fid, write_tag(3 , 'Z_coord'  , attr  , ''  ,1)
	PRINTF, fid, float(z)
	PRINTF, fid, write_tag(3 , 'Z_coord'  ,  ''   , ''  ,0) & PRINTF,fid,''
     endif

     if n_elements(PV) gt 1 then begin sxc=size(PV) & SC=strtrim(string(sxc(1)),2)
	if sxc(0) gt 1 then SC=SC+','+strtrim(string(sxc(2)),2)
	if sxc(0) gt 2 then SC=SC+','+strtrim(string(sxc(3)),2)
	if sxc(0) gt 3 then SC=SC+','+strtrim(string(sxc(4)),2)
	attr='type="float" dims="['+SC+']"'
	PRINTF, fid, write_tag(3 , 'Var_params'  , attr  , ''  ,1)
	PRINTF, fid, PV
	PRINTF, fid, write_tag(3 , 'Var_params'  ,  ''   , ''  ,0) & PRINTF,fid,''
     endif

     if n_elements(Data) gt 128*512. then FO=1 else FO=0
     attr='signal="1" type="'+tip+'" dims="['+dim+']"'
     if FO then attr=attr+' file_name="'+fileZ+'.'+codx+codz+'" code="'+codx+'"'
     attr=attr+' min="'+strtrim(string(myRG(1)),2)+'" max="'+strtrim(string(myRG(0)),2)+'"'
     attr=attr+' units="count"'
     if n_elements(mon) eq 1 then attr=attr+' monitors="'+strtrim(string(mon(0)),2)+'"'
     if sy              eq 1 then attr=attr+ ' y_value="'+strtrim(string(  y(0)),2)+'"'
     if sz              eq 1 then attr=attr+ ' z_value="'+strtrim(string(  z(0)),2)+'"'
     PRINTF, fid, write_tag(3 , 'Data' ,attr , '' ,1)
     if not FO then PRINTF, fid, Data $
	 else begin  ON_IOERROR,mio
	             if codx eq 'xdr' then begin OPENW ,lan,fileN+'.'+codx      ,/get_lun,/XDR
	                                         WRITEU,lan,Data
	             endif            else begin OPENW ,lan,fileN+'.'+codx+codz,/get_lun,/compress
	                                         PRINTF,lan,Data        & endelse
	             mio:if lan gt 0 then FREE_LUN,lan & ON_IOERROR,mis & endelse
     PRINTF, fid, write_tag(3 , 'Data'  ,  ''   , ''  ,0) & PRINTF,fid,''

     if n_elements(E)  gt 1 then begin sxc=size(E)  & SC=strtrim(string(sxc(1)),2)
	 if sxc(0) gt 1 then SC=SC+','+strtrim(string(sxc(2)),2)
     if n_elements(E) gt 128*512. then FO=1 else FO=0
	 attr='type="float" dims="['+SC+']"'
     if FO then attr=attr+' file_name="'+fileZ+'.err'+codz+'" code="'+codx+'"'
	 PRINTF, fid, write_tag(3 , 'Errors'  , attr  , ''  ,1)
	 if not FO then PRINTF, fid, E $
	 else begin  ON_IOERROR,mie
	             if codx eq 'xdr' then begin OPENW ,lan,fileN+'.err'     ,/get_lun,/XDR
	                                         WRITEU,lan,E
	             endif            else begin OPENW ,lan,fileN+'.err'+codz,/get_lun,/compress
	                                         PRINTF,lan,Data        & endelse
	             mie:if lan gt 0 then FREE_LUN,lan & ON_IOERROR,mis & endelse
	 PRINTF, fid, write_tag(3 , 'Errors'  ,  ''   , ''  ,0) & PRINTF,fid,''
     endif

   PRINTF, fid, write_tag(2 , 'NXdata'  , '' , ''  ,0) & PRINTF,fid,''

   if (size(N))(1) eq  S(1) then mon=float(N(*,0)) else mon=float(N)
   if n_elements(mon)  gt 1 then begin sxc=size(mon) & SC=strtrim(string(sxc(1)),2)
   PRINTF, fid, write_tag(2 , 'NXmonitor'  , ''  , ''  ,1)
	if sxc(0) gt 1 then SC=SC+','+strtrim(string(sxc(2)),2)
	if sxc(0) gt 2 then SC=SC+','+strtrim(string(sxc(3)),2)
	attr='type="float" dims="['+SC+']"'
	PRINTF, fid, write_tag(3 , 'Monitors'  , attr  , ''  ,1)
	PRINTF, fid, mon
	PRINTF, fid, write_tag(3 , 'Monitors'  ,  ''   , ''  ,0)
   PRINTF, fid, write_tag(2 , 'NXmonitor'  , ''  , ''  ,0) & PRINTF,fid,''
   endif

  PRINTF, fid, write_tag(1 , 'NXentry'  , '' , ''  ,0)
 PRINTF, fid, write_tag(0 , 'NXmlfile' , '' , '' ,0)
 FREE_LUN,fid
return
mis:	print,!err_string & if fid gt 0 then free_lun, fid
end
