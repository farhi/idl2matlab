pro write_hdf, FileName , Data , XC=x, YC=y , ZC=z , E=e  , N=n   $
;** *********
                               , PR=p, PV=pv, P_TXT=p_txt       $
                               , W_tit=wt  , X_tit=xt , Y_TIT=yt  $
                               , Z_tit=zt  , OTHER_TIT=ot, SRC=src, HIST=hist $
			       , PAR_TXT_ALL=pa_txt, LIM=lim, MACH=mach $
                               , DOE=doe, SNAP=snap, DATE=date, FIFI=fifi
;**
;**	Call for data-write HDF NeXus (called by LAMP).

;**	FileName: name of the output file (file.hdf)
;**	Data    : the data of any dimension.
;**	Keywords:		  
;**		  XC       = vector of x coordinates.
;**		  YC       = vector of y coordinates.
;**		  ZC       = vector of z coordinates (or scalar).
;**		  W_TIT    =   main title
;**		  X_TIT    = x axis title
;**		  Y_TIT    = y axis title
;**		  Z_TIT    = z axis title
;**		  OTHER_TIT=    sub title
;**		  N        = monitors (or scalar).
;**		  PR       = vector of float parameter values (or scalar).
;**		  P_TXT    = string array of text associated to PR (same size).
;**		  PV       = an array of any dimensions containing other parameter values (or 0).
;**		  E        = the errors associated to DATA (same size or 0).
;**		  SRC      = Instrument name.
;**		  HIST     = An history of the Data (string).
;**	Other Keywords:
;**		  PAR_TXT_ALL(unnecessary) p_txt + '=' + string(p) (if "" write_hdf will do for you)
;**		  LIM      = (unnecessary)
;**		  MACH     = (unused)
;**		  DOE      = (unused)
;**		  SNAP     = (optional) a 192*192 bytarea image snapshot of the Data.
;**		  DATE     = (optional) date of experiment (string).
;**		  FIFI     = (unused)

fid=0 & sdid=0
if !version.release lt '4.0' then begin print,string(7b)+"IDL version is too low !!!"   & return & endif
CATCH,stat &   IF stat ne 0  then begin print,!err_string & if sdid gt 0 then HDF_SD_END,sdid & if fid gt 0 then HDF_CLOSE,fid & return & endif
ON_IOERROR,mis

;******************
;**For Tests only**
;******************
if n_elements(p) eq 0 then begin
   s=size(Data) & x=indgen(s(1)) & if s(0) gt 1 then y=indgen(s(2)) else y=0
   z=0 & e=findgen(s(1)) & n=[1,2,3] & p=[1.,2.] & p_txt=['a','b'] & pv=findgen(2,3,4)
   wt='W' & xt='X' & yt='Y' & zt='Z' & ot='O' & src='Test'   & hist='Histoire'
   lim='This is limtx' & mach='win'  & snap=bytscl(dist(15)) & date=systime()
   endif
;*****************
;**Preliminaries**
;*****************
if wt eq '' then wt='W' & if xt eq '' then xt='X' & if yt  eq '' then yt='Y'
if zt eq '' then zt='Z' & if ot eq '' then ot='s' & if src eq '' then src='?'
if n_elements(date)   ne 1 then date=''
if n_elements(pa_txt) lt n_elements(p) then pa_txt=p_txt + '=' + string(p)
sdtag=720
myRG =[max(Data,min=mini),mini]

sx=n_elements(X) & sy=n_elements(Y) & sz=n_elements(Z)
S =size(Data)
xd=S(1) & if S(0) gt 1 then yd=s(2) else yd=0
          if S(0) gt 2 then begin zd=s(3)
                            if sz lt zd then Z=findgen(zd)+1 & sz=n_elements(Z)
          endif else zd=0

SV=[S(1:S(0))]

tiip=['ud','byte','short','long','float','double','double','string','ud','ud','ud','ud','uint','ulong','ud','ud']
ntip=[ 0L ,  21L ,  22L  ,  24L ,  5L   ,   6L   ,   6L   ,   3L   , 0L , 0L , 0L , 0L ,  23L ,  25L  , 0L , 0L ]
tip =tiip(s(s(0)+1))
nip =ntip(s(s(0)+1))
ymj =bin_date(systime()) & day=string(ymj(0),format='(I4,"-")')+string(ymj(1),format='(I2,"-")')+ $
                               string(ymj(2),format='(I2," ")')+string(ymj(3),format='(I2,":")')+ $
                               string(ymj(4),format='(I2,":")')+string(ymj(5),format='(I2)')
mon =float(N(*,0))
moni=1D/(total(mon)>1)
CAL={ Cal:moni, Cal_Err:0.0D, Offset:0.0D, Offset_Err:0.0D, Num_Type:nip }

;*******************
;**Create HDF file**
;*******************
fid  = HDF_OPEN     (FileName, /CREATE)
;*     ********
if fid gt 0 then begin

 sdid = HDF_SD_START (FileName, /RDWR)
;*      ************

 HDF_SD_ATTRSET, sdid, 'Written_by_LAMP','Lamp & Idl'+!version.release+' on '+!version.os+' '+!version.os_family, /STRING
 HDF_SD_ATTRSET, sdid, 'file_name'      , FileName    , /STRING
 HDF_SD_ATTRSET, sdid, 'file_time'      , day         , /STRING
 HDF_SD_ATTRSET, sdid, 'user'           , src+' '+date, /STRING

 vdid = HDF_VG_ATTACH(fid,-1,/WRITE) & HDF_VG_SETINFO,vdid, name ='entry1', class ='NXentry'
 vdat = HDF_VG_ATTACH(fid,-1,/WRITE) & HDF_VG_SETINFO,vdat, name ='data1' , class ='NXdata'
        HDF_VG_INSERT,vdid  , vdat

 if n_elements(src)   eq 1 then $
 HDF_SD_ATTRSET, sdid, 'SOURCE'        , src , /STRING
;**************
 if n_elements(hist)  eq 1 then $
 HDF_SD_ATTRSET, sdid, 'HISTORY'       , hist, /STRING
;**************
 HDF_SD_ATTRSET, sdid, 'TITLES'        , wt  , /STRING
;**************
 HDF_SD_ATTRSET, sdid, 'OTHER'         , ot  , /STRING
;**************
 if n_elements(lim)   eq 1 then $
 HDF_SD_ATTRSET, sdid, 'MIN_MAX_VALUES', lim , /STRING
;**************
;if n_elements(p)     eq 1 then $
 HDF_SD_ATTRSET, sdid, 'PARAMETERS'    , P   , n_elements(P)    , /FLOAT
;**************
;if n_elements(mach)  eq 1 then $
;HDF_SD_ATTRSET, sdid, 'MACHINE'       ,mach , /STRING
;**************

 np=n_elements(p)
 if np gt 1 then if n_elements(pa_txt) ge np then begin
    lf=string(10b) & para=''
    for i=0,np-1 do para=para+pa_txt(i)+' '+lf+' '
    sd0id= HDF_SD_CREATE   (sdid,'PARAMETERS',[strlen(para)] ,/STRING)
           HDF_SD_ADDDATA  ,sd0id,para & ref= HDF_SD_IDTOREF  (sd0id)
           HDF_SD_ENDACCESS,sd0id      &      HDF_VG_ADDTR,vdat,sdtag,ref
;*         ****************
 endif

 sdsid= 0 & ii=execute('sdsid= HDF_SD_CREATE(sdid,"DATA",SV,/'+tip+')' )
;*                             *************
 HDF_SD_SETINFO, sdsid, FORMAT=tip, LABEL=WT, CALDATA=cal, RANGE=myRG
;**************
 HDF_SD_ATTRSET, sdsid, 'signal'  , 1      ,1, /LONG
 HDF_SD_ATTRSET, sdsid, 'units'   , wt       , /STRING
 if n_elements(mon) eq 1 then HDF_SD_ATTRSET, sdsid, 'MONITORS',mon     ,1, /FLOAT
 if sy              eq 1 then HDF_SD_ATTRSET, sdsid, 'Y_VALUE' ,float(y),1, /FLOAT
 if sz              eq 1 then HDF_SD_ATTRSET, sdsid, 'Z_VALUE' ,float(z),1, /FLOAT

 if (s(0) gt 1) or (sy eq 1) then begin sdxid=HDF_SD_DIMGETID(sdsid,0)
   HDF_SD_DIMSET,sdxid,NAME='X_COORDINATES',LABEL=XT,SCALE=float(X),/BW_INCOMP & endif
;* *************

 if (s(0) gt 1) then begin              sdyid=HDF_SD_DIMGETID(sdsid,1)
   HDF_SD_DIMSET,sdyid,NAME='Y_COORDINATES',LABEL=YT,SCALE=float(Y),/BW_INCOMP & endif
;* *************

 if (s(0) gt 2) then begin              sdzid=HDF_SD_DIMGETID(sdsid,2)
   HDF_SD_DIMSET,sdzid,NAME='Z_COORDINATES',LABEL=ZT,SCALE=float(Z),/BW_INCOMP & endif
;* *************

 if (s(0) eq 1) and (sy gt 1) then $
   HDF_SD_ATTRSET, sdsid, 'X_SPACE', float(x), n_elements(x)  , /FLOAT
 if (s(0) eq 1) and (sy gt 1) then $
   HDF_SD_ATTRSET, sdsid, 'Y_SPACE', float(y), n_elements(y)  , /FLOAT
 if (s(0) eq 1) and (sz gt 1) then $
   HDF_SD_ATTRSET, sdsid, 'Z_SPACE', float(z), n_elements(z)  , /FLOAT

 if !version.release ge '5.2.1' then if strpos(strlowcase(FileName),'_c' ) ge 0 then begin cps=0
                                     if strpos(strlowcase(FileName),'_c1') ge 0 then       cps=1
                                     if strpos(strlowcase(FileName),'_c2') ge 0 then       cps=4
                                     if strpos(strlowcase(FileName),'_c3') ge 0 then       cps=3
                                     if strpos(strlowcase(FileName),'_c4') ge 0 then       cps=4
                                     if cps gt 0 then HDF_SD_SETCOMPRESS,sdsid,cps
                                     endif
 HDF_SD_ADDDATA, sdsid, Data  & ref= HDF_SD_IDTOREF   (sdsid)
;**************
 HDF_SD_ENDACCESS, sdsid      &      HDF_VG_ADDTR,vdat ,sdtag,ref
;****************

;*****For NEXUS compatibility*****
 if sx gt 1 then begin sxc=size(x)  & SC=[sxc(1:sxc(0))] 
    sdxid= HDF_SD_CREATE   (sdid ,'X',SC ,/FLOAT)
           HDF_SD_ATTRSET  ,sdxid,'axis',1,1,/LONG & HDF_SD_ATTRSET,sdxid,'units',XT,/STRING 
           HDF_SD_ADDDATA  ,sdxid,float(x)  &  ref = HDF_SD_IDTOREF(sdxid)
           HDF_SD_ENDACCESS,sdxid           &        HDF_VG_ADDTR  ,vdat,sdtag,ref & endif

 if sy gt 1 then begin syc=size(y)  & SC=[syc(1:syc(0))]
    sdyid= HDF_SD_CREATE   (sdid ,'Y',SC ,/FLOAT)
           HDF_SD_ATTRSET  ,sdyid,'axis',2,1,/LONG & HDF_SD_ATTRSET,sdyid,'units',YT,/STRING 
           HDF_SD_ADDDATA  ,sdyid,float(y)  &  ref = HDF_SD_IDTOREF(sdyid)
           HDF_SD_ENDACCESS,sdyid           &        HDF_VG_ADDTR  ,vdat,sdtag,ref & endif

 if sz gt 1 then begin szc=size(z)  & SC=[szc(1:szc(0))] 
    sdzid= HDF_SD_CREATE   (sdid ,'Z',SC ,/FLOAT)
           HDF_SD_ATTRSET  ,sdzid,'axis',3,1,/LONG & HDF_SD_ATTRSET,sdzid,'units',ZT,/STRING 
           HDF_SD_ADDDATA  ,sdzid,float(z)  &  ref = HDF_SD_IDTOREF(sdzid)
           HDF_SD_ENDACCESS,sdzid           &        HDF_VG_ADDTR  ,vdat,sdtag,ref & endif
;*****End NEXUS compatibility*****

 if n_elements(PV)   gt 1 then begin npv=size(PV)   & SP=[npv(1:npv(0))] 
    sd2id= HDF_SD_CREATE   (sdid,'VAR_PARAMS',SP ,/FLOAT)
           HDF_SD_ADDDATA  ,sd2id, PV & ref= HDF_SD_IDTOREF  (sd2id)
           HDF_SD_ENDACCESS,sd2id     &      HDF_VG_ADDTR,vdat,sdtag,ref & endif

 if n_elements(E)    gt 1 then begin nee=size(E)    & SE=[nee(1:nee(0))] 
    sd3id= HDF_SD_CREATE   (sdid,'errors'    ,SE ,/FLOAT)
           HDF_SD_ADDDATA  ,sd3id,  E & ref= HDF_SD_IDTOREF  (sd3id)
           HDF_SD_ENDACCESS,sd3id     &      HDF_VG_ADDTR,vdat,sdtag,ref & endif

 if n_elements(SNAP) gt 1 then begin snp=size(SNAP) & SN=[snp(1:snp(0))] 
    sd4id= HDF_SD_CREATE   (sdid,'SNAPSHOT'  ,SN ,/BYTE)
           HDF_SD_ADDDATA  ,sd4id,SNAP &  & ref= HDF_SD_IDTOREF  (sd4id)
           HDF_SD_ENDACCESS,sd4id         &      HDF_VG_ADDTR,vdat,sdtag,ref
;*         ****************
 endif

 HDF_VG_DETACH, vdat
;*************
 if (size(N))(1) eq  S(1) then mon=float(N(*,0)) else mon=float(N)
 if n_elements(mon)  gt 1 then begin smo=size(mon)  & SM=[smo(1:smo(0))]
 vmon = HDF_VG_ATTACH(fid,-1,/WRITE) & HDF_VG_SETINFO, vmon, name ='monitors' , class ='NXmonitor'
        HDF_VG_INSERT,vdid  , vmon
 sdmon= HDF_SD_CREATE(sdid,'MONITOR1',SM     , /float)
        HDF_SD_ATTRSET  ,sdmon,'signal', 0,1 , /BYTE
        HDF_SD_ADDDATA  ,sdmon, mon & ref= HDF_SD_IDTOREF  (sdmon)
        HDF_SD_ENDACCESS,sdmon      &      HDF_VG_ADDTR,vmon,sdtag,ref
        HDF_VG_DETACH   ,vmon	      & endif
;*      ****************

 HDF_VG_DETACH, vdid	

 HDF_SD_END   , sdid
 HDF_CLOSE    , fid
endif	
return
mis:	print,!err_string & if fid gt 0 then HDF_CLOSE, fid
end
