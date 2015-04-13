;************************
function HDx_OPEN       , FileName , read=read
;************************
common   HDx_com,hdf5,vers,file_id,root,att_loc,att_names,grp_loc,grp_names,grp_id,sd_loc,sd_id,sd_names,vg_lone,Natts

if n_elements(vers) eq 0 then begin  hdf5=0 & vers=4
	 if !version.release ge '5.5' then if !version.release lt '5.6' then hdf5=execute('dlm_load,"hdf5"') else hdf5=1
endif &  if hdf5 then begin b=0 & ii=execute('b=H5F_IS_HDF5(FileName)') & if b then vers=5 else vers=4 & endif
if vers eq 5 then begin
	 att_loc=-1L & file_id=H5F_OPEN( FileName)
	 grp_loc=-1L & grp_id =0L & grp_names=''
	 sd_loc =-1L & sd_id  =0L & sd_names =''
	 root=H5G_OPEN( file_id  , '/')     & Natts=H5A_GET_NUM_ATTRS(root)
	 N   =H5G_GET_NMEMBERS(file_id,'/') & ok=0 & sk=0
	 if N gt 0 then begin
	    for i=0,N-1 do begin
		g=H5G_GET_MEMBER_NAME(file_id,'/',i)
		s=H5G_GET_OBJINFO    (root,g)
		if s.type eq 'GROUP'   then begin   grpi=H5G_OPEN(root,g)
			grp_id =[grp_id,grpi] & grp_loc=[grp_loc,root]  & grp_names  =[grp_names,g] & ok=ok+1 & endif
		if s.type eq 'DATASET' then begin
			sd_id  =[sd_id ,0]    & sd_loc =[sd_loc ,root]  & sd_names   =[sd_names ,g] & sk=sk+1 & endif
	    endfor
	    if sk gt 0 then begin sd_id  =sd_id  (1:sk) & sd_loc =sd_loc (1:sk) & sd_names =sd_names (1:sk) & endif
	    if ok gt 0 then begin grp_id =grp_id (1:ok) & grp_loc=grp_loc(1:ok) & grp_names=grp_names(1:ok) & endif
	 endif
	 if grp_loc(0) eq -1 then vg_lone=-1 $
	               else begin vg_lone=indgen(n_elements(grp_loc))
                              for j=0,n_elements(grp_loc)-1 do hd0_init_vgsd, grp_loc(j),grp_names(j),grp_id(j) & endelse
 	 return,file_id
endif else $
return,  HDF_OPEN       ( FileName , read=read)
end

;************************
function HDx_SD_START   , FileName , read=read
;************************
common   HDx_com
if vers eq 5 then begin
         return,root
endif else $
return,  HDF_SD_START   ( FileName , read=read)
end

;************************
function HDx_SD_ATTRFIND, sdid, name
;************************
common   HDx_com
if vers eq 5 then begin
	 if att_loc ne sdid then begin N=H5A_GET_NUM_ATTRS(sdid) & att_names='!'
	    if N gt 0 then begin
	       att_names=strarr(N)
	       att_loc=sdid
	       for i=0,N-1 do begin a=H5A_OPEN_IDX(sdid,i)
	                            atn  =H5A_GET_NAME(a)
	                            h4   =strpos(atn,'_GLO') & if h4 gt 0 then atn=strmid(atn,0,h4) ;(h4toh5)
	                            att_names(i)= atn
				    H5A_CLOSE,a & endfor
	 endif & endif
	 idx=where(strlowcase(att_names) eq strlowcase(name)) & return,idx(0)
endif else $
return,  HDF_SD_ATTRFIND( sdid, name)
end

;************************
function HDx_VG_LONE    , fid
;************************
common   HDx_com
if vers eq 5 then begin
         return,vg_lone
endif else $
return,  HDF_VG_LONE    ( fid)
end

;************************
function HDx_VG_ATTACH  , fid,j
;************************
common   HDx_com
if vers eq 5 then begin
         return,grp_id(j) ;Group already opened
endif else $
return,  HDF_VG_ATTACH  ( fid,j)
end

;************************
function HDx_SD_SELECT  , sdid,j
;************************
common   HDx_com
if vers eq 5 then begin
         selid=H5D_OPEN(sd_loc(j),sd_names(j))
         sd_id(j)=selid
         return  ,selid
endif else $
return,  HDF_SD_SELECT  ( sdid,j)
end

;************************
function HDx_SD_NAMETOINDEX, sdid,name
;************************
common   HDx_com
if vers eq 5 then begin
         j=where(sd_names eq name)
         return,j(0)
endif else $
return,  HDF_SD_NAMETOINDEX( sdid,name)
end

;************************
function HDx_SD_IDTOREF , sdsid
;************************
common   HDx_com
if vers eq 5 then begin
         j=where(sd_id eq sdsid)
         return,j(0)
endif else $
return,  HDF_SD_IDTOREF ( sdsid)
end

;************************
function HDx_SD_DIMGETID, kp_id,n
;************************
common   HDx_com
if vers eq 5 then begin
         return,-1 ;Obsolete
endif else $
return,  HDF_SD_DIMGETID( kp_id,n)
end

;************************
pro      HDx_SD_ATTRINFO, sdsid,j,NAME=Aname,TYPE=tip,COUNT=count,DATA=tmp
;************************
common   HDx_com
if vers eq 5 then begin
         a=H5A_OPEN_IDX  (sdsid,j)
         Aname=H5A_GET_NAME(a)
         tmp  =H5A_READ(a)
         count=n_elements(tmp) & if strpos(Aname,'HDF4_') eq 0 then count=0 ;(h4toh5)
         tip  ='STRING' & ii=execute('tip=size(tmp,/tname)')
         H5A_CLOSE,a
endif else $
         HDF_SD_ATTRINFO, sdsid,j,NAME=Aname,TYPE=tip,COUNT=count,DATA=tmp
end

;************************
pro      HDx_VG_GETINFO , tid, name=nm,class=cn
;************************
common   HDx_com
if vers eq 5 then begin
	 j  =where(grp_id eq tid) & j=j(0)
	 nm =grp_names(j)
	 cn =' '
	 idx=HDx_SD_ATTRFIND(tid, 'NX_class')
	 if idx lt 0 then idx=HDx_SD_ATTRFIND(tid, 'class')
	 if idx lt 0 then idx=HDx_SD_ATTRFIND(tid, 'NXclass')
	 if idx lt 0 then idx=HDx_SD_ATTRFIND(tid, 'HDF4_VGROUP_CLASS') ;(h4toh5)
	 if idx ge 0 then begin a=H5A_OPEN_IDX  (tid,idx) & cn=H5A_READ(a) & H5A_CLOSE,a & endif
   ;Bug in API
	 if strlowcase(cn) eq 'nxinstrumen' then cn='NXinstrument'
	 if strlowcase(cn) eq 'nxmonito' then cn='NXmonitor'
	 if strlowcase(cn) eq 'nxsampl' then cn='NXsample'
	 if strlowcase(cn) eq 'nxentr' then cn='NXentry'
	 if strlowcase(cn) eq 'nxdat' then cn='NXdata'
	 if strlowcase(cn) eq 'nxbea' then cn='NXbeam'
	 if strlowcase(cn) eq 'nxuse' then cn='NXuser'
	 if strlowcase(cn) eq 'nxlo' then cn='NXlog'
   ;end of Bug
endif else $
         HDF_VG_GETINFO , tid, name=nm,class=cn
end

;************************
pro      HDx_VG_GETTRS  , gid,tags,refs
;************************
common   HDx_com
if vers eq 5 then begin
         tags=-1 & refs=-1
         gp_r  =where(grp_loc eq gid) & if gp_r(0) ge 0 then begin refs= gp_r & tags=gp_r*0+1965 & endif     ;magic
         sd_r  =where(sd_loc  eq gid) & if sd_r(0) ge 0 then $
	                                if refs(0) ge 0 then begin refs=[refs,sd_r] & tags=[tags,sd_r*0+720] ;magic
	                                endif           else begin refs= sd_r & tags=sd_r*0+720  & endelse
endif else $
         HDF_VG_GETTRS  , gid,tags,refs
end

;************************
pro      HDx_SD_FILEINFO, sdid  ,  Nsets, Nattrib
;************************
common   HDx_com
if vers eq 5 then begin
         Nattrib=Natts
         Nsets  =n_elements(sd_loc)
endif else $
         HDF_SD_FILEINFO, sdid  ,  Nsets, Nattrib
end

;************************
pro      HDx_SD_GETINFO ,sdsid, NAME=Sname,TYPE=tip,DIMS=dims,LABEL=label,UNIT=unit,NATTS=nattrib
common   HDx_com
if vers eq 5 then begin
         j=where(sd_id eq sdsid) & j=j(0)
         Sname  =sd_names(j)
         label  =' ' & unit=' ' ;Obsolete
         nattrib=H5A_GET_NUM_ATTRS(sdsid)
         ref_t  =H5D_GET_TYPE     (sdsid)
            tip =H5T_GET_CLASS(ref_t) & tip=strupcase(strmid(tip,4,20))
            H5T_CLOSE,ref_t
         sp     =H5D_GET_SPACE(sdsid)
            dims=H5S_GET_SIMPLE_EXTENT_DIMS(sp)
            H5S_CLOSE,sp
endif else $
         HDF_SD_GETINFO ,sdsid, NAME=Sname,TYPE=tip,DIMS=dims,LABEL=label,UNIT=unit,NATTS=nattrib
end

;************************
pro      HDx_SD_GETDATA ,sdsid, Data
;************************
common   HDx_com
if vers eq 5 then begin
         Data=H5D_READ(sdsid)
endif else $
         HDF_SD_GETDATA ,sdsid, Data
end

;************************
pro      HDx_SD_DIMGET  , dmid, LABEL=xt, UNIT=unit, SCALE=x, COUNT=cn
;************************
common   HDx_com
if vers eq 5 then begin
         xt=' ' & unit=' ' & x=0 & count=1 ;Obsolete
endif else $
         HDF_SD_DIMGET  , dmid, LABEL=xt, UNIT=unit, SCALE=x, COUNT=cn
end

;************************
pro      HDx_SD_ENDACCESS,sdsid
;************************
common   HDx_com
if vers eq 5 then begin
         H5D_CLOSE      , sdsid
endif else $
         HDF_SD_ENDACCESS,sdsid
end

;************************
pro      HDx_VG_DETACH  , gid
;************************
common   HDx_com
if vers eq 5 then begin
	;keep Group opened
endif else $
         HDF_VG_DETACH  , gid
end

;************************
pro      HDx_SD_END     , sdid
;************************
common   HDx_com
if vers eq 5 then begin
         H5G_CLOSE      , sdid
endif else $
         HDF_SD_END     , sdid
end

;************************
pro      HDx_CLOSE      , fid
;************************
common   HDx_com
if vers eq 5 then begin
         H5F_CLOSE      , fid
         H5_CLOSE
endif else $
         HDF_CLOSE      , fid
end

;** ***********
pro hd0_init_vgsd, grp,name,recur
;** ***********
common   HDx_com
	 N  =H5G_GET_NMEMBERS(grp,name)
	 for i=0,N-1 do begin
		g=H5G_GET_MEMBER_NAME(grp,name,i)
		s=H5G_GET_OBJINFO    (recur,g)
		    if s.type eq 'GROUP'   then begin   grpi=H5G_OPEN(recur,g)
			   grp_id =[grp_id,grpi]  & grp_loc=[grp_loc,recur]  & grp_names  =[grp_names,g]
		           hd0_init_vgsd, recur,g,grpi & endif
		    if s.type eq 'DATASET' then begin
		           sd_id  =[sd_id ,0]     & sd_loc =[sd_loc ,recur]  & sd_names   =[sd_names ,g]
		           if sd_loc(0) eq -1 then begin sk= n_elements(sd_loc)-1
		                                    sd_id=sd_id(1:sk) & sd_loc=sd_loc(1:sk) & sd_names=sd_names(1:sk) & endif
            endif
     endfor
end

;** ********
pro read_hdf, FileName , Data , XC=x, YC=y , ZC=z , E=e  , N=n   $
;** ********
                         , PR=p, PV=pv, PAR_TXT=p_txt            $
                         , W_tit=wt   , X_tit=xt , Y_TIT=yt      $
                         , Z_tit=zt   , OTHER_TIT=ot , SNAP=snap $
                         , SOURCE=src , Img=immg , HIST=his

;** Read HDF Lamp format & NeXus.

;** Input  parameters: FileName & optionnaly immg="1.1" for "first entry . first data".
;**		       Data must be set to 0 (read all) or -88 (read attributs only)

;** Output parameters: All others+Data

magic = 1965 ;& magic=720
fid=0 & sdid=0
CATCH,stat & IF stat ne 0 then begin print,!err_string & if sdid gt 0 then HDx_SD_END,sdid & catch, /cancel
                                                         if fid  gt 0 then HDx_CLOSE,fid   & return & endif
ON_IOERROR,mis

;*****************
;**Preliminaries**
;*****************
imm= 1 & if n_elements(immg) ne 1 then img=1 else begin  ON_IOERROR,mismg
		simg=str_sep(string(immg),'.')   &  img=long(simg(0))>1
		if n_elements(simg) eq 2 then begin img=long(simg(1))>1 & imm=long(simg(0))>1 & endif
		mismg:ON_IOERROR,mis
         endelse
src='' & his ='' & lim ='' & p=0  & xd='' & yd='' & zd=''  & wt='' & xt=''  & yt='' & aff='' & mel=''
zt ='' & ot  ='' & mach='' & snap=bindgen(2,2)    & x =0   & y = 0 & z = 0  & lampF=0
pv = 0 & e   = 0 & n   = 0 & p_txt=' ='   & pa_txt=[' =0'] & tiip='UNKNOWN' & axes=['?']
idy=-1 & time='' & D88 = Data(0)          & space=' '      & transp=0
Quiet=!quiet     & !quiet=1

;*****************
;**Open HDF file**
;*****************
fid  = HDx_OPEN     (FileName , /READ)
;*     ********
if fid gt 0 then begin Data=[1,2,3,4]

 sdid = HDx_SD_START (FileName, /READ)
 ;*     ************
 idx =HDx_SD_ATTRFIND(sdid, 'file_name')   & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=his
 idx =HDx_SD_ATTRFIND(sdid, 'title')       & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=his
 idx =HDx_SD_ATTRFIND(sdid, 'titl')        & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=his
 idx =HDx_SD_ATTRFIND(sdid, 'file_time')   & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=time
 idx =HDx_SD_ATTRFIND(sdid, 'user_name')   & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=src
 idx =HDx_SD_ATTRFIND(sdid, 'owner')       & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=src
 idx =HDx_SD_ATTRFIND(sdid, 'user')        & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=src
 idx =HDx_SD_ATTRFIND(sdid, 'affiliation') & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=aff
 idx =HDx_SD_ATTRFIND(sdid, 'email')       & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=mel
 time=strtrim(string(byte(time)>32b),2)    & his =strtrim(string(byte(his)>32b),2) & src=strtrim(string(byte(src)>32b),2)
 aff =strtrim(string(byte(aff) >32b),2)    & mel =strtrim(string(byte(mel)>32b),2)
 if aff gt ' ' then src=src+' ('+aff+')'   & if mel gt ' ' then src=src+' ->'+mel
 his =his+' '+time
 ot  =src
 wt  =his
 idx =HDx_SD_ATTRFIND(sdid, 'instrument')  & if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=ins
                                             if idx ge 0 then begin src =strtrim(string(byte(ins)>32b),2)
                                                                    ot  =src+' '+ot & endif
;************************
;**Select right NXentry**
;************************
 lest=HDx_VG_LONE(fid)   & NXentry=0 & list=["","",""] & nlest='0'
 ;*   ***********
 if lest(0) ge 0 then FOR j=0,n_elements(lest)-1 do begin
                          tid= HDx_VG_ATTACH (fid,lest(j)) & HDx_VG_GETINFO,tid, class=cn
                               HDx_VG_DETACH, tid
                          if   strlowcase(cn) ne "nxentry" then lest(j)=-1
                      ENDFOR & idx=where(lest ge 0)
 if idx(0)  ge 0 then begin        lest =lest(idx)
                                   imm  =imm< n_elements(lest)
                                   nlest=strtrim(string(n_elements(lest)),2)
                                   NXentry=HDx_VG_ATTACH(fid ,lest((imm-1)>0))
                                           HDx_VG_GETTRS,NXentry,tags,refs
                                   FOR  i= 0,n_elements(tags)-1 do IF tags(i) eq magic then begin
                                      tid= HDx_VG_ATTACH(fid ,refs(i))
                                           HDx_VG_GETINFO,tid, name=nm,class=cn
                                           HDx_VG_DETACH, tid
                                           list=[list,string(refs(i)),nm+'1',cn]
                                   endif & list=strlowcase(list)  &   endif
                                   
;***********************
;**Select right NXdata**
;***********************
 idx =where(list eq "nxdata")  &  nidx  ='0'
           if  idx(0) gt 0  then  nidx  =strtrim(string(n_elements(idx)),2)
 img=img<n_elements(idx) &  idxd= img-1  &  NXdata=idx(idxd)

 if  NXdata  gt 0   then  NXdata=HDx_VG_ATTACH(fid , long(list(NXdata-2)))
 if  NXdata  gt 0   then  begin  HDx_VG_GETTRS,NXdata,tags, refs
                                 NXF="NeXus  entry:"+strtrim(string(imm),2)+"/"+nlest
                                 NXF= NXF + " data:"+strtrim(string(img),2)+"/"+nidx
                    endif  else  NXF="HDF"

     HDx_SD_FILEINFO,sdid  ,  Nsets, Natts

;**  ***************
;**Check for Writer*
;*******************
 idx=HDx_SD_ATTRFIND(sdid, 'Written_by_LAMP') & if idx ge 0 then lampF=1
 if  idx lt 0 then $
 idx=HDx_SD_ATTRFIND(sdid, 'LAMP_FORMAT')     & if idx ge 0 then lampF=1
 ;*  ***************
 if lampF then begin

  HDx_SD_ATTRINFO, sdid, idx, DATA=time

  idx=HDx_SD_ATTRFIND(sdid, 'SOURCE')
      if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=src

  idx=HDx_SD_ATTRFIND(sdid, 'HISTORY')
      if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=his

  idx=HDx_SD_ATTRFIND(sdid, 'TITLES')
      if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=wt

  idx=HDx_SD_ATTRFIND(sdid, 'OTHER')
      if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=ot

  idx=HDx_SD_ATTRFIND(sdid, 'MIN_MAX_VALUES')
      if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=lim

  idx=HDx_SD_ATTRFIND(sdid, 'PARAMETERS')
	if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=P

 ;idx=HDx_SD_ATTRFIND(sdid, 'MACHINE')
 ;    if idx ge 0 then HDx_SD_ATTRINFO,sdid,idx,DATA=mach

  kp_id=HDx_SD_SELECT(sdid,HDx_SD_NAMETOINDEX(sdid,'DATA'))

  FOR i=0, Nsets-1 do begin OK=1
    sdsid=HDx_SD_SELECT(sdid,i)
    if NXdata gt 0 then begin idx=where(refs eq HDx_SD_IDTOREF(sdsid))
       if idx(0) lt 0 then OK=0 & endif
    if OK then begin
       HDx_SD_GETINFO,sdsid, NAME=Sname, TYPE=tip, DIMS=dims & Sname=strupcase(Sname)

      case Sname of
     'PARAMETERS'   : begin HDx_SD_GETDATA,sdsid, pa_txt
                            pa_txt=STR_SEP(pa_txt,' '+string(10b)+' ')
                            if n_elements (pa_txt) eq 1 then $
	                      pa_txt=STR_SEP(pa_txt(0),' '+string(14b)+' ')
                            cn=n_elements (pa_txt)-1 & P=fltarr(cn) & ON_IOERROR,mes
                            p_txt=strarr(cn)
                            for j=0,cn-1 do begin  tmp=pa_txt(j) & r=RSTRPOS(tmp,'=')>0
                                if r ge 0 then begin P(j)=FLOAT(STRMID(tmp,r+1,30))
                                                     p_txt(j)=  STRMID(tmp,0,r+1) & endif
                            endfor
                            mes:ON_IOERROR,mis
                      end
     'DATA'         : begin if D88 ne -88 then HDx_SD_GETDATA ,sdsid, Data & tiip=tip & dims=string(dims)
                            xd=dims(0) & if n_elements(dims) ge 2 then yd=dims(1) & if n_elements(dims) ge 3 then zd=dims(2)
                            idy=HDx_SD_ATTRFIND(sdsid, 'Y_VALUE') & if idy ge 0 then HDx_SD_ATTRINFO,sdsid,idy,DATA=Y
                            idz=HDx_SD_ATTRFIND(sdsid, 'Z_VALUE') & if idz ge 0 then HDx_SD_ATTRINFO,sdsid,idz,DATA=Z
                            idn=HDx_SD_ATTRFIND(sdsid,'MONITORS') & if idn ge 0 then HDx_SD_ATTRINFO,sdsid,idn,DATA=N
                            END
     'X'            : begin if D88 ne -88 then HDx_SD_GETDATA ,sdsid,x & idx=HDx_SD_ATTRFIND(sdsid,'units')
                            if idx ge  0  then HDx_SD_ATTRINFO,sdsid,idx,DATA=xt & END
     'X_COORDINATES': begin dmid=HDx_SD_DIMGETID (kp_id,0)
                            HDx_SD_DIMGET, dmid, LABEL=xt, UNIT=unit, SCALE=x, COUNT=cn
                            if strpos(unit,'-->') ge 0 then begin off=0L & ON_IOERROR,mex
                               reads,unit,off & mex:x=lindgen(cn)+off    & ON_IOERROR,mis
                            endif  & end
     'Y'            : begin if D88 ne -88 then HDx_SD_GETDATA ,sdsid,y & idx=HDx_SD_ATTRFIND(sdsid,'units')
                            if idx ge  0  then HDx_SD_ATTRINFO,sdsid,idx,DATA=yt & END
     'Y_COORDINATES': begin dmid=HDx_SD_DIMGETID (kp_id,1)
                            HDx_SD_DIMGET, dmid, LABEL=yt, UNIT=unit, SCALE=y, COUNT=cn
                            if strpos(unit,'-->') ge 0 then begin off=0L & ON_IOERROR,mey
                               reads,unit,off & mey:y=lindgen(cn)+off    & ON_IOERROR,mis
                            endif  & end
     'Z'            : begin if D88 ne -88 then HDx_SD_GETDATA ,sdsid,z & idx=HDx_SD_ATTRFIND(sdsid,'units')
                            if idx ge  0  then HDx_SD_ATTRINFO,sdsid,idx,DATA=zt & END
     'Z_COORDINATES': begin dmid=HDx_SD_DIMGETID (kp_id,2)
                            HDx_SD_DIMGET, dmid, LABEL=zt, UNIT=unit, SCALE=z
                      end
     'VAR_PARAMS'   : if D88 ne -88 then HDx_SD_GETDATA,sdsid, pv
     'ERRORS'       : if D88 ne -88 then HDx_SD_GETDATA,sdsid, e
     'SNAPSHOT'     : if D88 eq -88 then HDx_SD_GETDATA,sdsid, snap
      ELSE          :
      endcase
    endif
    HDx_SD_ENDACCESS, sdsid
  ENDFOR

  if (n_elements(y) eq 1) and (idy lt 0) then begin
     idx=HDx_SD_ATTRFIND(kp_id, 'X_SPACE') & if idx ge 0 then HDx_SD_ATTRINFO,kp_id,idx,DATA=X
     idx=HDx_SD_ATTRFIND(kp_id, 'Y_SPACE') & if idx ge 0 then HDx_SD_ATTRINFO,kp_id,idx,DATA=Y
     idx=HDx_SD_ATTRFIND(kp_id, 'Z_SPACE') & if idx ge 0 then HDx_SD_ATTRINFO,kp_id,idx,DATA=Z
     if  idx ge 0 then space=' SPACE MODEL:'
  endif

 ;*****************
 ;Writer is'nt LAMP
 ;*****************
 endif else begin
  FOR i=0, Nsets-1 do begin  ok=1
    sdsid=HDx_SD_SELECT(sdid,i)
    if NXdata gt 0 then begin idx=where(refs eq HDx_SD_IDTOREF(sdsid))
       if idx(0) lt 0 then OK=0 & endif

    if OK then begin
      HDx_SD_GETINFO,sdsid, NAME=Sname,TYPE=tip,DIMS=dims,LABEL=label,UNIT=unit,NATTS=natts & Sname=strupcase(Sname)

      if NXdata le 0 then begin
         if Sname eq 'CNT1' then if (D88 ne -88) and (n_elements(data) eq 4) then HDx_SD_GETDATA,sdsid, data
         if Sname eq 'Y'    then if (D88 ne -88)                             then HDx_SD_GETDATA,sdsid, data
         if Sname eq 'X'    then if (D88 ne -88)                             then HDx_SD_GETDATA,sdsid, X
         if Sname eq 'TTHE' then if (D88 ne -88)                             then HDx_SD_GETDATA,sdsid, Y
         if Sname eq 'TTHE' then yt='Two theta'
         if Sname eq 'SPEC' then if (D88 ne -88)                             then HDx_SD_GETDATA,sdsid, Isis_SPEC
         if Sname eq 'MDET' then if (D88 ne -88)                             then HDx_SD_GETDATA,sdsid, Isis_MDET
         if Sname eq 'LEN2' then if (D88 ne -88)                             then HDx_SD_GETDATA,sdsid, Isis_LEN2
         if Sname eq 'NSP1'                                                  then HDx_SD_GETDATA,sdsid, Isis_NSP1
         if Sname eq 'XUNITS'                                                then HDx_SD_GETDATA,sdsid, xt
         if Sname eq 'YUNITS'                                                then HDx_SD_GETDATA,sdsid, zt
         if Sname eq 'TITL'                                                  then HDx_SD_GETDATA,sdsid, wt & wt=strtrim(wt,2)
         if Sname eq 'NAME'                                                  then HDx_SD_GETDATA,sdsid, src
      endif else if Sname eq 'ERRORS' then begin if D88 ne -88               then HDx_SD_GETDATA,sdsid, e
      endif else begin
	 idx=HDx_SD_ATTRFIND(sdsid, 'long_name')
	    if idx ge 0 then       HDx_SD_ATTRINFO,sdsid,idx,DATA=Lname else Lname=' ' & Lname=strtrim(string(byte(Lname)>32b),2)
	 idx=HDx_SD_ATTRFIND(sdsid, 'units')
	    if idx ge 0 then       HDx_SD_ATTRINFO,sdsid,idx,DATA=Lunit else Lunit=' ' & Lunit=strtrim(string(byte(Lunit)>32b),2)
	 idx=HDx_SD_ATTRFIND(sdsid, 'title')
	    if idx ge 0 then       HDx_SD_ATTRINFO,sdsid,idx,DATA=Ltitl else Ltitl=' ' & Ltitl=strtrim(string(byte(Ltitl)>32b),2)
	 idx=HDx_SD_ATTRFIND(sdsid, 'monitor')
	    if idx ge 0 then       HDx_SD_ATTRINFO,sdsid,idx,DATA=N

	 idx=HDx_SD_ATTRFIND(sdsid, 'signal')
	    if idx ge 0 then begin HDx_SD_ATTRINFO,sdsid,idx,DATA=vaa & val=1
	                           ON_IOERROR,misvaa1 & val=long(vaa(0)) & misvaa1:ON_IOERROR,mis
	                           if val eq 1 then begin
	                           ON_IOERROR,mis1
	                           if D88 ne -88 then  HDx_SD_GETDATA,sdsid, Data & mis1:tiip=tip
	                           ON_IOERROR,mis
	                           if label gt ' ' then  wt=label & lim=''  &  zt=Sname+' '+Lunit
				   if ltitl gt ' ' then  wt=ltitl+' '+Lunit $
	                                           else  wt=Sname+' '+Lunit+' '+wt+' '+Lname
	                           if yt    eq ''  then  yt=zt
	                           FOR j=0, n_elements(dims)-1  DO lim=lim+string(dims(j))+' '
	                           endif & endif
	 ON_IOERROR,mis
	 idx=HDx_SD_ATTRFIND(sdsid, 'axis')
	    if idx ge 0 then begin HDx_SD_ATTRINFO,sdsid,idx,DATA=vaa & val=1
	       ON_IOERROR,misvaa2 & val=long(vaa(0)) & misvaa2:ON_IOERROR,mis
	       idx=HDx_SD_ATTRFIND(sdsid, 'primary') & vaa=1
	       if idx ge 0 then    HDx_SD_ATTRINFO,sdsid,idx,DATA=vaa & prim=1
	       ON_IOERROR,misvaa3 & prim=long(vaa(0)) & misvaa3:ON_IOERROR,mis
	                        if prim   eq 1 then begin tt=label
				   if ltitl gt ' ' then   tt=ltitl+' '+Lunit $
	                                           else   if tt eq '' then tt=Sname+' '+Lunit+' '+Lname
	                           if val eq 1 then begin ON_IOERROR,mis2 & HDx_SD_GETDATA,sdsid, x & mis2:xt=tt & endif
	                           if val eq 2 then begin ON_IOERROR,mis3 & HDx_SD_GETDATA,sdsid, y & mis3:yt=tt & endif
	                           if val eq 3 then begin ON_IOERROR,mis4 & HDx_SD_GETDATA,sdsid, z & mis4:zt=tt & endif
	                           ON_IOERROR,mis
	    endif       &       endif
	 idx=HDx_SD_ATTRFIND(sdsid, 'axes')
	    if idx ge 0 then begin HDx_SD_ATTRINFO,sdsid,idx,DATA=axes & axes=strtrim(string(byte(axes)>32b),2)
	       b=strpos(axes,'[') & if b ge 0 then  axes=strmid (axes,b+1,100)
	       b=strpos(axes,']') & if b gt 0 then  axes=strmid (axes, 0 , b )
	       axes=strcompress(axes,/remove_all) & axes=str_sep(axes,',')
	    endif
      endelse
    endif
    HDx_SD_ENDACCESS, sdsid
  ENDFOR
 endelse
 ;*****************
 if NXdata gt 0 then HDx_VG_DETACH, NXdata

 ;***************
 ;NXother  groups
 ;***************
 idx=where(list eq "nxinstrument")  &  NXinstrument=idx(0) &  tags=[-1] &  refs=[-1] & grnm=''
 idx=where(list eq "nxmonitor")     &  NXmonitor   =idx(0) & stags=[-1] & srefs=[-1]
 idx=where(list eq "nxsample")      &  NXsample    =idx(0) & ttags=[-1] & trefs=[-1]
 idx=where(list eq "nxdata")        &  NXdata      =idx(idxd)
 idx=where(list eq "nxbeam")        &  NXbeam      =idx(0)
 idx=where(list eq "nxlog")         &  NXlog       =idx(0)
;idx=where(list eq "nxuser")        &  NXuser      =idx(0)

 if NXinstrument gt 0 then   begin
    NXinstrument=HDx_VG_ATTACH(fid, long(list(nxinstrument-2))) & HDx_VG_GETTRS,NXinstrument,ttags,trefs
    FOR  i= 0,n_elements(ttags)-1 do $
    IF ttags(i) eq magic then begin tid=HDx_VG_ATTACH(fid ,trefs(i))
       HDx_VG_GETTRS,tid,stags,srefs & HDx_VG_GETINFO,tid, name=nm,class=cn & cn=strmid(cn,2,20)
    tags=[tags,stags] & refs=[refs,srefs] & t=string(stags) & t(*)='(I '+cn+') '   & grnm=[grnm,t] & HDx_VG_DETACH, tid  & endif
    tags=[tags,ttags] & refs=[refs,trefs] & t=string(ttags) & t(*)='(Instrument) ' & grnm=[grnm,t] & HDx_VG_DETACH, NXinstrument
    ttags=[-1] & trefs=[-1] & endif

 if NXmonitor    gt 0 then   begin
    NXmonitor   =HDx_VG_ATTACH(fid,long(list(nxmonitor-2)))     & HDx_VG_GETTRS,NXmonitor,ttags,trefs
    tags=[tags,ttags] & refs=[refs,trefs] & t=string(ttags) & t(*)='(Monitor) '    & grnm=[grnm,t] & HDx_VG_DETACH, NXmonitor
    ttags=[-1] & trefs=[-1] & endif

 if NXsample     gt 0 then   begin
    NXsample    =HDx_VG_ATTACH(fid,long(list(nxsample-2)))      & HDx_VG_GETTRS,NXsample,ttags,trefs
    tags=[tags,ttags] & refs=[refs,trefs] & t=string(ttags) & t(*)='(Sample) '     & grnm=[grnm,t] & HDx_VG_DETACH, NXsample
    ttags=[-1] & trefs=[-1] & endif

 if NXentry      gt 0 then   begin                                HDx_VG_GETTRS,NXentry,ttags,trefs
    tags=[tags,ttags] & refs=[refs,trefs] & t=string(ttags) & t(*)='(Entry) '      & grnm=[grnm,t]
    ttags=[-1] & trefs=[-1] & endif

 if NXdata       gt 0 then   if (not lampF) then begin
    NXdata      =HDx_VG_ATTACH(fid, long(list(nxdata-2)))       & HDx_VG_GETTRS,NXdata,ttags,trefs
    tags=[tags,ttags] & refs=[refs,trefs] & t=string(ttags) & t(*)='(Data) '       & grnm=[grnm,t] & HDx_VG_DETACH, NXdata
    ttags=[-1] & trefs=[-1] & endif

 if NXbeam       gt 0 then   begin
    NXbeam      =HDx_VG_ATTACH(fid, long(list(nxbeam-2)))       & HDx_VG_GETTRS,NXbeam,ttags,trefs
    tags=[tags,ttags] & refs=[refs,trefs] & t=string(ttags) & t(*)='(Beam) '       & grnm=[grnm,t] & HDx_VG_DETACH, NXbeam
    ttags=[-1] & trefs=[-1] & endif

 if NXlog        gt 0 then   begin
    NXlog       =HDx_VG_ATTACH(fid, long(list(nxlog-2)))        & HDx_VG_GETTRS,NXlog,ttags,trefs
    tags=[tags,ttags] & refs=[refs,trefs] & t=string(ttags) & t(*)='(Vlog) '       & grnm=[grnm,t] & HDx_VG_DETACH, NXlog
    ttags=[-1] & trefs=[-1] & endif

 if n_elements(tags) gt 1 then begin
    FOR i=0, Nsets-1 do begin
	sdsid = HDx_SD_SELECT(sdid,i) & idx=where(refs eq HDx_SD_IDTOREF(sdsid)) & idx=idx(0)
	if idx ge 0 then begin
	  HDx_SD_GETINFO,sdsid, NAME=Sname,TYPE=tip,DIMS=dims,NATTS=natts
	  itx= HDx_SD_ATTRFIND(sdsid, 'units')
	   if (itx ge 0) then  HDx_SD_ATTRINFO,sdsid,itx,DATA=Lu else Lu=' ' & Lu=strtrim(string(byte(Lu)>32b),2)
	  itx= HDx_SD_ATTRFIND(sdsid, 'signal')
	   if (itx ge 0) then signal=1 else signal=0
;	************************
;	Check for SDS parameters
;	************************
	  ON_IOERROR,mis5
	  HDx_SD_GETDATA,sdsid,tmp & mis5:ON_IOERROR,mis

	  if (grnm(idx) eq '(Monitor) ') and (((n_elements(N) eq 1) and (N(0)  eq 0)) or signal)   then N=tmp else $
	  if (strpos(strlowcase(Sname),'monitor') eq 0) and (N(0) eq 0) then N=tmp

	  if (tip eq 'BYTE') then begin on_ioerror,misOb & t=strtrim(string(byte(tmp)>32b),2) & tmp=t & dims=1 & tip='STRING' & misOb:ON_IOERROR,mis & endif
	  totdim=n_elements(tmp)
	  ida=where(axes eq Sname) & ida=ida(0)

	  if (grnm(idx) eq '(Data) ') and (ida ge 0) then begin
		j=HDx_SD_ATTRFIND(sdsid, 'long_name')
	     if j   ge 0 then HDx_SD_ATTRINFO,sdsid,j,DATA=Lname else Lname=Sname & Lname=strtrim(string(byte(Lname)>32b),2)
		j=HDx_SD_ATTRFIND(sdsid, 'units')
	     if j   ge 0 then HDx_SD_ATTRINFO,sdsid,j,DATA=Lunit else Lunit=' '   & Lunit=strtrim(string(byte(Lunit)>32b),2)
	     if ida eq 0 then begin x=tmp & xt=Lname+' '+Lunit & endif
	     if ida eq 1 then begin y=tmp & yt=Lname+' '+Lunit & endif
	     if ida eq 2 then begin z=tmp & zt=Lname+' '+Lunit & endif

	  endif else if totdim le 12 then begin if totdim gt 1 then tmp=reform(tmp,totdim)
	    if (strpos(strlowcase(Sname),'title') eq 0) then wt=string(tmp(0)) else begin
		if (tip eq 'STRING') then begin Sname=Sname+': '+strmid(tmp(0),0,40) & tmp=0 & endif
		on_ioerror,misOd & t=0 & t=float(tmp)   & misOd:ON_IOERROR,mis & p=[p,t] & t=string(t)
		if n_elements(t) gt 1 then tix='_'+strtrim(sindgen(n_elements(t)),2) else tix=''
		pa_txt=[pa_txt,grnm(idx)+Sname+tix+' '+Lu+' = '+t] & t(*)=' ' & p_txt=[p_txt ,grnm(idx)+Sname+tix+' '+Lu+' ='+t]
	    endelse
	  endif
	  if grnm(idx) eq '(Vlog) ' then if n_elements(tmp) gt 1 then PV=tmp
;	************************
;	Check for ATT parameters
;	************************
	  if (natts gt 0) then FOR j=0,natts-1 do begin
	   HDx_SD_ATTRINFO,sdsid,j,NAME=Aname,TYPE=tip,COUNT=count,DATA=tmp
	   if count gt 0 then begin
	    if (strpos(strlowcase(Aname),'monitor') eq 0) and (n_elements(N) eq 1) and (N(0) eq 0) then N=tmp
	    if (tip eq 'BYTE') then begin on_ioerror,misOy & t=strtrim(string(byte(tmp)>32b),2) & tmp=t & count=1 & tip='STRING' & misOy:ON_IOERROR,mis & endif
	    if (count le 9) and (Aname ne 'axis') and (Aname ne 'signal') and (Aname ne 'units') and (Aname ne 'axes') then begin
		if (tip eq 'STRING') then begin Aname=Aname+': '+strmid(tmp(0),0,40) & tmp=0 & endif
		on_ioerror,misOa & t=0 & t=float(tmp)   & misOa:ON_IOERROR,mis & p=[p,t] & t=string(t)
		if n_elements(t) gt 1 then tix='_'+strtrim(sindgen(n_elements(t)),2) else tix=''
		pa_txt=[pa_txt,grnm(idx)+Aname+tix+' = '+t] & t(*)=' ' & p_txt=[p_txt ,grnm(idx)+Aname+tix+' ='+t] & endif
	   endif
	  ENDFOR
;	********************
	endif & HDx_SD_ENDACCESS, sdsid
    ENDFOR
    ;***************
    ;Sort parameters
    ;***************
    if (n_elements(p) gt 1) and (p_txt(0) eq ' =') and (p(0) eq 0) then begin p_txt=p_txt(1:*)
        idx=sort(strmid(p_txt,0,40)) & p_txt=p_txt(idx) & p=p(idx+1) & pa_txt=pa_txt(idx+1) & endif
 endif

 if NXentry gt 0 then HDx_VG_DETACH, NXentry

 ;**********************************
 ;Show what we have seen for browser
 ;**********************************
 if D88 eq -88 then begin Data=[time,' ']
      Data=[Data,' SOURCE:         '+src,' '  ,' HISTORY:        '+his ,' ']
  if (lampF  eq 1)  then $
	Data=[Data,' X_SIZE:         '+xd       ,' Y_SIZE:         '+yd      ,' Z_SIZE:         '+zd,space]
	Data=[Data,' FORMAT:         '+NXF      ,' TYPE:           '+tiip,' ',' RANGES:         '+lim ,' ']
	Data=[Data,' TITLES: '+wt,'      X: '+xt,'      Y: '+yt              ,'      Z: '+zt,'  OTHER: '+ot,' ']

	Data=[Data,' PARAMETERS:',' ----------']
	for i=0,n_elements(pa_txt)-1 do $
	Data=[Data,' * '+pa_txt(i)]

 ;if (lampF  eq 1)  then Data=[Data,' ',' MACHINE: '+mach,' ']

 ;****************************************
 ;Else give back the data in correct order
 ;****************************************
 endif else begin siz=SIZE(Data)
 	;*****
 	;Isis?
 	;*****
	if (siz(0) eq 2) and (n_elements(Isis_SPEC) gt 1) then begin
	   if siz(2)  eq Isis_NSP1(0)+1 then begin DATA=DATA(*,1:*) & siz=SIZE(Data) & endif
	   if n_elements(Isis_MDET) gt 0 then begin
	      Isis_MDET=Isis_MDET-1
	      Isis_SPEC=Isis_SPEC-1
	      N  =Data (*,Isis_SPEC(Isis_MDET))
	      Mdist=Isis_LEN2(Isis_MDET)
	      P    =[P,MDIST]
	      P_TXT=[P_TXT,'Distance Monitor '+strtrim(string(indgen(n_elements(N))+1),2)+' ']
	      Isis_SPEC(Isis_MDET)=-1 & Isis_SPEC=Isis_SPEC(where(Isis_SPEC ge 0))
	      Y(Isis_MDET)=-1         & idn =where(Y ge 0)
	      Y  =Y(idn)
	      Isis_LEN2=Isis_LEN2(idn)
	   endif
	   idn =sort (Y) & Y=float(Y(idn))
	   PV  =Isis_LEN2(idn)
	   X   =float(X) & if xt eq '' then xt='Channels'
	   DATA=DATA (*,Isis_SPEC(idn)) & siz=SIZE(Data)
	endif
 	;**********
 	;Histogram?
 	;**********
 	if   siz(0) ge 1 then begin sxs=SIZE(x) & sys=SIZE(y)
	 if  sxs(0) eq 1 then if (sxs(1) ne siz(1))  and (sxs(1) ne siz(2))   then $
	                      if (sxs(1) eq siz(1)+1) or (sxs(1) eq siz(2)+1) then begin
				x =x+(shift(x,-1)-x)/2. & x=x(0:n_elements(x)-2)
				ot=ot+' (histogram)
			      endif
	 if  sys(0) eq 1 then if (sys(1) ne siz(1))  and (sys(1) ne siz(2))   then $
	                      if (sys(1) eq siz(1)+1) or (sys(1) eq siz(2)+1) then begin
				y =y+(shift(y,-1)-y)/2. & y=y(0:n_elements(y)-2)
			      endif
	endif
 	;**********
 	;Transpose?
 	;**********
	if (siz(1) eq n_elements(y))    and (siz(2) eq n_elements(x)) then transxy=1 else transxy=0
  	if  siz(0) ge 2 then if (transp) or (siz(1) ne siz(2)) then $
	if (transxy) or (transp) then begin
		if (siz(1) lt siz(2)*2/3) then transp=1
		trans2=transxy*transp
		if (SIZE(x))(0)  ge 2  then if (not trans2) then x =transpose(x)
		if (SIZE(y))(0)  ge 2  then if (not trans2) then y =transpose(y)
		if (transp) then begin Data=transpose(Data)
		                       if (SIZE(e))(0) ge 2 then e=transpose(e)
		endif
		if (not trans2) then  begin tmp=yt & yt=xt & xt=tmp  & tmp=y & y=x & x =tmp & endif
		if (SIZE(n)) (0) ge 2  then if (SIZE(n)) (2) eq (SIZE(data))(1) then n =transpose(n)
		if (SIZE(pv))(0) ge 2  then if (SIZE(pv))(2) eq (SIZE(data))(1) then pv=transpose(pv)
	endif
	if (siz(siz(0)+1) eq 12) then Data=long (Data)
 	if (siz(siz(0)+1) eq 13) then Data=float(Data)
 endelse

 HDx_SD_END, sdid
;**********
 HDx_CLOSE , fid
;*********
endif
!quiet=Quiet
return
mis:	print,!err_string
end
