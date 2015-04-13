;-------------------------------------------------------------------------------
; The NeXus - API in IDL
;
; This is a reimplementation of the NeXus API routines in the IDL language.
; For details about the functions defined herein and about NeXus see the
; NeXus WWW-pages.
; 
; copyright: you may do everything withs this code, but one thing:
; 
; you may not sue me or my employer if it does not work or gives invalid
; results. You are at you own. This is why you have the source. 
; In other terms: Absolutely no Warranties are given.
;
; version 1.0
; 
; Initial coding:  Mark Koennecke, August 1998
;                  Laboratory for Neutron Scattering
;                  Paul Scherrer Institute
;                  CH-5232 Villigen-PSI
;                  Switzerland
;                  Mark.Koennecke@psi.ch
;-------------------------------------------------------------------------------- 
; WARNING: IDL-possible-bug: Do not change the calls to hdf_sd_attrinfo. I call
; it often with too many keywords, requesting information not needed. But I
; noticed, that hdf_sd_attrinfo would kill IDL when called with different sets
; of keywords. My recommendation: leave it alone! M.K., August 1998  
;
;-------------------------- NXprintError --------------------------------------
; hack Nxprinterror if you want errors printed somewhere else and not onto the
; IDL prompt. You may choose to pop message boxes instead.
PRO NXprinterror, txt
     Print, txt
END
;----------------------- NXopen ------------------------------------------------
FUNCTION NXopen, filename, access, handle
;
   if N_PARAMS() LT 3 THEN BEGIN
       NXprinterror, 'Insufficient number of arguments to NXopen'
   END      
;
;  the NeXus data structure
   handle = {NXstruct, NXID: 110898, Acess: ' ', iVID:LONG(0), $
            iSID:LONG(0) , iCurrentVG: LONG(0), $
              iCurrentSDS:LONG(0), iVREF:LONARR(10), iStackPtr:0}
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
; open file, depending on access code
   if access EQ 'read' then begin
      handle.iVID = hdf_open(filename,/read)
      handle.Acess = 'read'
      handle.iSID = hdf_sd_start(filename,/read)
    end
 ;
   IF access EQ 'write' THEN BEGIN
      handle.iVID = hdf_open(filename,/write)
      handle.iSID = hdf_sd_start(filename,/RDWR)
      handle.Acess = 'write'
   END
;  
   IF access EQ 'create' THEN BEGIN
       handle.iSID = hdf_sd_start(filename,/create)
       handle.iVID = hdf_open(filename,/write)
       handle.Acess = 'write'
       hdf_sd_attrset,handle.iSID, 'file_name',filename
;----- fix me, time is not in NeXus standard format
       tim = systime()
       hdf_sd_attrset, handle.iSID, 'file_time',tim
   END
   IF (handle.iVID EQ 0) OR (handle.iSID EQ 0) THEN BEGIN
       NXprinterror, 'ERROR: failed to open HDF file'
       return, 0
   END
    return, 1
END
;------------------------- NXcheck -------------------------------------------
FUNCTION NXcheck, handle
     IF N_PARAMS() LT 1 THEN BEGIN
        NXprinterror, 'ERROR: Internal, insufficient number of arguments to NXcheck'
        return, 0
      END
      IF handle.NXID  NE  110898 THEN BEGIN
         NXprinterror, 'ERROR: invalid NeXus file handle'
         return, 0
      END
      return, 1     
END
;------------------------ NXclose----------------------------------------------
FUNCTION NXclose, handle 
;
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;-------- close open vGroups
   IF handle.iCurrentVG NE 0 THEN BEGIN
         hdf_vg_detach, handle.iCurrentVG
   END
;-------- close open SDS
   IF handle.iCurrentSDS NE 0 THEN BEGIN
         hdf_sd_endaccess, handle.iCurrentSDS
   END
;-------- close SDS-API
   hdf_sd_end, handle.iSID
;--------- close file
   hdf_close, handle.iVID
   handle = 0
   return, 1
END
;------------------------------ NXIfindvgroup ---------------------------------
FUNCTION NXIfindvgroup, handle, name, class
;------root level
  IF handle.iCurrentVG EQ 0 THEN BEGIN
     list = hdf_vg_lone(handle.iVID)
     IF N_ELEMENTS(list) EQ 0 THEN BEGIN
        return, 0
     END
;------------ look them all up
     FOR I = 0, (N_ELEMENTS(list) - 1) DO BEGIN
       vgid = hdf_vg_attach(handle.iVID,list(I))
       hdf_vg_getinfo, vgid, name=nm, class =cn
       hdf_vg_detach, vgid
       IF (name EQ nm) AND (class EQ cn) THEN BEGIN
         return, list(I)
       END 
     END
  END ELSE BEGIN
    hdf_vg_gettrs, handle.iCurrentVG, tags, refs
    FOR I = 0, (N_ELEMENTS(tags) - 1) DO BEGIN
      IF tags(I) EQ 1965 THEN BEGIN
        vgid = hdf_vg_attach(handle.iVID,refs(I))
        hdf_vg_getinfo, vgid, name=nm, class =cn
        hdf_vg_detach, vgid
        IF (name EQ nm) AND (class EQ cn) THEN BEGIN
           return, refs(I)
        END 
      END
    END
  END
  return, 0
END
;------------------------------- NXmakegroup -----------------------------------
FUNCTION NXmakegroup, handle, nname, cclass
;----- check arguments
   IF N_PARAMS() LT 3 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXmakegroup'
       return, 0
    END 
 ;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;--------- make we do it?
    IF handle.Acess NE 'write' THEN BEGIN
       NXprinterror, 'ERROR: no permission to create vGroup'
       return,0
    END
;------------ make sure, that a group with this name does not yet exist
    iRet = NXIfindvgroup(handle,nname,cclass)
    IF iRet NE 0 THEN BEGIN
         NXprinterror, 'ERROR: vGroup already exists'
         return, 0
    END
;-------- do it
    iNew = hdf_vg_attach(handle.iVID,-1,/write)
    IF iNew LT 0 THEN BEGIN
       NXprinterror, 'ERROR: failed to create vGroup'
       return, 0
    END
    hdf_vg_setinfo, iNew, name = nname, class = cclass
;-------- insert when apropriate
    IF handle.iCurrentVG NE 0 THEN BEGIN
        hdf_vg_insert, handle.iCurrentVG, iNew
    END
    hdf_vg_detach, iNew
    return, 1
END
;---------------------- NXopengroup --------------------------------------
FUNCTION NXopengroup, handle, nname, cclass
;----- check arguments
   IF N_PARAMS() LT 3 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXopengroup'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;--------- search the vGroup
   iRef = NXIfindvgroup(handle,nname,cclass)
   IF iRef EQ 0 THEN BEGIN
       NXprinterror, 'ERROR: Requested vGroup NOT found'
       return,0
   END
;--------- at root:
   IF handle.iCurrentVG EQ 0 THEN BEGIN
      IF handle.Acess EQ 'read' THEN $ 
         handle.iCurrentVG = hdf_vg_attach(handle.iVID,iRef) $
      ELSE  $ 
         handle.iCurrentVG = hdf_vg_attach(handle.iVID,iRef,/write) 
      handle.iStackPtr = handle.iStackPtr + 1
      handle.iVREF(handle.iStackPtr) = iRef 
   END ELSE BEGIN
      hdf_vg_detach, handle.iCurrentVG
      handle.iStackPtr = handle.iStackPtr + 1
      handle.iVREF(handle.iStackPtr) = iRef 
      IF handle.Acess EQ 'read' THEN $ 
         handle.iCurrentVG = hdf_vg_attach(handle.iVID,iRef) $
      ELSE  $ 
         handle.iCurrentVG = hdf_vg_attach(handle.iVID,iRef,/write) 
   END
   IF handle.iCurrentVG LT 0 THEN BEGIN
     NXprinterror, 'ERROR: HDF failed to open vGroup'
     handle.iCurrentVG = 0
     return, 0
   END ELSE $ 
         return, 1
END
;---------------------- NXclosegroup -----------------------------------------
FUNCTION NXclosegroup, handle
;----- check arguments
   IF N_PARAMS() LT 1 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXclosegroup'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;------ the trivial case: we are at root
  IF handle.iCurrentVG EQ 0 THEN BEGIN
               return, 1 
  END ELSE BEGIN 
    hdf_vg_detach,handle.iCurrentVG
    handle.iStackPtr = handle.iStackPtr - 1
    IF handle.iStackPtr LE 0 THEN BEGIN ; we are at root now
        handle.iStackPtr = 0
        handle.iCurrentVG = 0
    END ELSE BEGIN ; open lower vGroup
      iRef = handle.iVREF(handle.iStackPtr) 
      IF handle.Acess EQ 'read' THEN $ 
         handle.iCurrentVG = hdf_vg_attach(handle.iVID,iRef) $
      ELSE  $ 
         handle.iCurrentVG = hdf_vg_attach(handle.iVID,iRef,/write) 
    END
  END
  return, 1
END
;------------------------ NXIfindsds ------------------------------------------
FUNCTION NXIfindsds, handle, name
;----------- no arg checking, as this is supposed to be called only internally
;------------ root level
  IF handle.iCurrentVG EQ 0 THEN BEGIN
     hdf_sd_fileinfo, handle.iSID, iSDS, iATT
     for i = 0, (iSDS - 1) DO BEGIN
         iNew = hdf_sd_select(handle.iSID,i)
         hdf_sd_getinfo, iNew,name = me
         iRef = hdf_sd_idtoref(iNew)
         hdf_sd_endaccess,iNew
         IF me EQ name THEN return, iRef 
     END 
  END ELSE BEGIN ; vGroup level
          hdf_vg_getinfo,handle.iCurrentVG, NENTRIES = iN
          FOR i = 0, (iN - 1) DO BEGIN
             hdf_vg_gettr, handle.iCurrentVG,i,iTag,iRef
             IF (iTag EQ 700) OR (iTag EQ 720) OR (iTag EQ 703) THEN BEGIN
               ; DFTAG_SDG, DFTAF_NDG, DFTAG_SDS
               iNew = hdf_sd_reftoindex(handle.iSID,iRef)
               i2 = hdf_sd_select(handle.iSID,iNew)
               hdf_sd_getinfo, i2, name = me
               hdf_sd_endaccess, i2
               IF me EQ name THEN return, iRef          
             END
          END
  END
  return, 0
END
;--------------------------- NXmakedata --------------------------------------
FUNCTION NXmakedata, handle, name, datatype, rank, dimensions
;----- check arguments
   IF N_PARAMS() LT 5 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXmakedata'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;---------- do we have write privilege?
   IF handle.Acess NE 'write' THEN BEGIN
      NXprinterror, 'ERROR: no privilege to create SDS'
      return, 0
   END
;---------- is there already a SDS with the same name at the current level?
   iRet = NXIfindsds(handle,name)
   IF iRet NE 0 THEN BEGIN
      NXprinterror, 'ERROR: Cannot create duplicate SDS name'
      return, 0
   END
;------ check rank
   IF rank LE 0 THEN BEGIN
      NXprinterror, 'ERROR: invalid rank parameter specified'
      return, 0
   END
;------ check dims
   IF N_ELEMENTS(dimensions) LT rank THEN BEGIN
      NXprinterror, 'ERROR: not enough dimension values in dimensions array'
      return, 0
   END
;------- be nice, if there is already an SDS open
   IF handle.iCurrentSDS NE 0 THEN BEGIN
      hdf_sd_endaccess, handle.iCurrentSDS
      handle.iCurrentSDS = 0
   END
;--- disallow SDS creation at root level
   IF handle.iCurrentVG EQ 0 THEN BEGIN
      NXprinterror, 'ERROR: SDS creation at root level not permitted in NeXus'
      return, 0
   END
;------ actually create the data set, depending on the value of the type info
   CASE datatype OF
        'NX_FLOAT32': $
           iRes = hdf_sd_create(handle.iSID,name,dimensions,/DFNT_FLOAT32)
        'NX_FLOAT64': $
           iRes = hdf_sd_create(handle.iSID,name,dimensions,/DFNT_FLOAT64)
        'NX_INT8': $
           iRes = hdf_sd_create(handle.iSID,name,dimensions,/Byte)
        'NX_UINT8': $
           iRes = hdf_sd_create(handle.iSID,name,dimensions,/DFNT_UINT8)
        'NX_CHAR': $
           iRes = hdf_sd_create(handle.iSID,name,dimensions,/DFNT_CHAR)
        'NX_INT16': $
           iRes = hdf_sd_create(handle.iSID,name,dimensions,/DFNT_INT16)
        'NX_UINT16': $
           iRes = hdf_sd_create(handle.iSID,name,dimensions,/DFNT_UINT16)
        'NX_INT32': $
           iRes = hdf_sd_create(handle.iSID,name,dimensions,/DFNT_INT32)
        'NX_UINT32': $
           iRes = hdf_sd_create(handle.iSID,name,dimensions,/DFNT_UINT32)
         ELSE: BEGIN
            NXprinterror, 'ERROR: datatype not recognised'
            return, 0
         END        
    ENDCASE
    IF iRes LT 0 THEN BEGIN
        NXprinterror, 'ERROR: failed to create SDS'
        return, 0
    END
;------ link into vGroup
    IF handle.iCurrentVG NE 0 THEN BEGIN
       iRef = hdf_sd_idtoref(iRes)
       hdf_vg_addtr,handle.iCurrentVG, 700,iRef
    END
    hdf_sd_endaccess, iRes
    return, 1
END
;---------------------- NXopendata ---------------------------------------------
FUNCTION NXopendata, handle, name
;----- check arguments
   IF N_PARAMS() LT 2 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXopendata'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;-------- find the SDS
   iData = NXIfindSDS(handle,name)
   IF iData EQ 0 THEN BEGIN
      NXprinterror, 'ERROR: SDS NOT found!'
      return, 0
   END   
;------ be nice: close SDS's which may still be open
   IF handle.iCurrentSDS NE 0 THEN BEGIN
       hdf_sd_endaccess, handle.iCurrentSDS
       handle.iCurrentSDS = 0
   END
;------- now do the job
   iNew = hdf_sd_reftoindex(handle.iSID,iData)
   handle.iCurrentSDS = hdf_sd_select(handle.iSID,iNew)
   IF handle.iCurrentSDS LE 0 THEN BEGIN
      NXprinterror, 'ERROR: failed to open SDS'
      handle.iCurrentSDS = 0
      return, 0
   END
   return, 1
END
;-------------------- NXclosedata ---------------------------------------------
FUNCTION NXclosedata, handle
;----- check arguments
   IF N_PARAMS() LT 1 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXclosedata'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;--------
   IF handle.iCurrentSDS NE 0 THEN BEGIN
      hdf_sd_endaccess, handle.iCurrentSDS
      handle.iCurrentSDS = 0
   END ELSE NXprinterror, 'WARNING: no SDS open, nothing to do'
   return, 1
END
;------------------------- NXgetdata ------------------------------------------
FUNCTION NXgetdata, handle, data
;----- check arguments
   IF N_PARAMS() LT 2 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXgetdata'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;------- check if there is an open SDS
   IF handle.iCurrentSDS EQ 0 THEN BEGIN
      NXprinterror, 'ERROR: no SDS open'
      return, 0
   END
;------- do read data
   hdf_sd_getdata, handle.iCurrentSDS, data
   return, 1
END
;--------------- NXgetslab -------------------------------------------------
FUNCTION NXgetslab, handle, data, start, ente
;----- check arguments
   IF N_PARAMS() LT 4 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXgetslab'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;------- check if there is an open SDS
   IF handle.iCurrentSDS EQ 0 THEN BEGIN
      NXprinterror, 'ERROR: no SDS open'
      return, 0
   END
;------- do read data
   hdf_sd_getdata, handle.iCurrentSDS, data, start=start, count=ente
   return, 1
END
;------------------------------ NXgetattr ------------------------------------
FUNCTION NXgetattr, handle, name, data, type
;----- check arguments
   IF N_PARAMS() LT 4 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXgetattr'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;--------- find the attribute
    IF handle.iCurrentSDS EQ 0 THEN BEGIN ; global attribute
       iID = hdf_sd_attrfind(handle.iSID,name)
    END ELSE BEGIN
       iID = hdf_sd_attrfind(handle.iCurrentSDS,name)
    END
    IF iID LT 0 THEN BEGIN
       NXprinterror, 'ERROR: attribute NOT found!'
       return, 0
    END
;--- read the attribute
    IF handle.iCurrentSDS EQ 0 THEN BEGIN
       hdf_sd_attrinfo,handle.iSID, iID,name=nm,data = data, HDF_TYPE = type
    END ELSE BEGIN
       hdf_sd_attrinfo,handle.iCurrentSDS, iID,name=nmm, data = data, HDF_TYPE = type
    END
    return, 1   
END
;------------------ NXputdata --------------------------------------------------
FUNCTION NXputdata, handle, data
;----- check arguments
   IF N_PARAMS() LT 2 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXputdata'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;------- check if there is an open SDS
   IF handle.iCurrentSDS EQ 0 THEN BEGIN
      NXprinterror, 'ERROR: no SDS open'
      return, 0
   END
;------- write  data
   hdf_sd_adddata, handle.iCurrentSDS, data
   return, 1
END
;--------------- NXputslab -------------------------------------------------
FUNCTION NXputslab, handle, data, start, ente
;----- check arguments
   IF N_PARAMS() LT 4 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXputslab'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;------- check if there is an open SDS
   IF handle.iCurrentSDS EQ 0 THEN BEGIN
      NXprinterror, 'ERROR: no SDS open'
      return, 0
   END
;------- write data
   hdf_sd_adddata, handle.iCurrentSDS, data, start=start, count=ente
   return, 1
END
;------------------------------ NXputattr ------------------------------------
FUNCTION NXputattr, handle, name, data
;----- check arguments
   IF N_PARAMS() LT 3 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXputattr'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;--- set the attribute
    IF handle.iCurrentSDS EQ 0 THEN BEGIN
       hdf_sd_attrset,handle.iSID,name,data
    END ELSE BEGIN
       hdf_sd_attrset,handle.iCurrentSDS, name,data
    END
    return, 1   
END
;----------------------- NXgetinfo ---------------------------------------------
FUNCTION NXgetinfo, handle, rank, dim, type
;----- check arguments
   IF N_PARAMS() LT 2 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXgetdata'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;------- check if there is an open SDS
   IF handle.iCurrentSDS EQ 0 THEN BEGIN
      NXprinterror, 'ERROR: no SDS open'
      return, 0
   END
;------- get data
   hdf_sd_getinfo, handle.iCurrentSDS, dims = dim, HDF_TYPE = type, NDIMS = rank
   return, 1
END
;-------------------------- NXgroupdir -------------------------------------
FUNCTION NXgroupdir, handle,names, class
;----- check arguments
   IF N_PARAMS() LT 3 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXgroupdir'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;------ handling root level
   IF handle.iCurrentVG EQ 0 THEN BEGIN
      list = hdf_vg_lone(handle.iVID)
      iLen = N_ELEMENTS(list)
      names = STRARR(iLen)
      class = STRARR(iLen)
      FOR I = 0, (iLen - 1) DO BEGIN
         vgid = hdf_vg_attach(handle.iVID,list(I))
         hdf_vg_getinfo, vgid, name = nm, class = cn
         hdf_vg_detach, vgid
         names(I) = nm
         class(I) = cn
      END
      return, 1
   END ELSE BEGIN ; vGroup level
       hdf_vg_gettrs, handle.iCurrentVG, tags, refs
       iLen = N_ELEMENTS(tags)
       inames = STRARR(iLen+1)
       iclass = STRARR(iLen+1)
       iFound = 0
       FOR i = 0, (iLen - 1) DO BEGIN
            iTag = tags(I)
            IF iTag EQ 1965 THEN BEGIN ; vGroup
               vgid = hdf_vg_attach(handle.iVID,refs(I))
               hdf_vg_getinfo, vgid, name =nm, class = cn
               hdf_vg_detach, vgid
               inames(iFound) = nm
               iclass(iFound) = cn
               iFound = iFound + 1
            END 
            IF (iTag EQ 700) OR (iTag EQ 720) OR (iTag EQ 703) THEN BEGIN
              ; scientific data
                iNew = hdf_sd_reftoindex(handle.iSID,refs(I))
                i2 = hdf_sd_select(handle.iSID, iNew)
                hdf_sd_getinfo, i2, name = nm
                hdf_sd_endaccess, i2
                inames(iFound) = nm
                iclass(iFound) = 'SDS'
                iFound = iFound + 1 
            END
       END
       names = inames(0:iFound)
       class = iclass(0:iFound)
       return, 1 
   END
   return, 0
END
;-------------------------- NXgetattdir --------------------------------
FUNCTION NXattdir, handle, names
;----- check arguments
   IF N_PARAMS() LT 2 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXattdir'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;---------- root level
   IF handle.iCurrentSDS EQ 0 THEN BEGIN
      hdf_sd_fileinfo, handle.iSID, ds, atts
      names = STRARR(atts)
      FOR i = 0, (atts - 1) DO BEGIN
        hdf_sd_attrinfo, handle.iSID, i, name = nm, data =dat, HDF_TYPE = typ
        names(i) = nm
      END
      return, 1
   END ELSE BEGIN ; SDS attributes
      hdf_sd_getinfo, handle.iCurrentSDS, NATTS = atts
      names = STRARR(atts)
      FOR i = 0, (atts - 1) DO BEGIN
        hdf_sd_attrinfo, handle.iCurrentSDS, i, name = nm, data =dat, HDF_TYPE =typ
        names(i) = nm
      END
      return, 1
   END
END
;---------------------------- NXgetgroupid -----------------------------------
FUNCTION NXgetgroupid, handle, id
;----- check arguments
   IF N_PARAMS() LT 2 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXgetgroupid'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;------ invalid when at root
   IF handle.iCurrentVG EQ 0 THEN BEGIN
      NXprinterror, 'ERROR: No ID for root level'
      return, 0
   END
;------ alright do it
   id = lonarr(2)
   id(0) = 1965
   id(1) = handle.iCurrentVG
   return, 1   
END
;---------------------------- NXgetdataid -----------------------------------
FUNCTION NXgetdataid, handle, id
;----- check arguments
   IF N_PARAMS() LT 2 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXgetdataid'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;------ invalid when at root
   IF handle.iCurrentSDS EQ 0 THEN BEGIN
      NXprinterror, 'ERROR: No  SDS open, no ID s then'
      return, 0
   END
;------ alright do it
   id = lonarr(2)
   id(0) = 703
   id(1) = hdf_sd_idtoref(handle.iCurrentSDS)
   return, 1   
END
;-------------------------- NXmakelink ------------------------------------------
FUNCTION NXmakelink, handle, id
;----- check arguments
   IF N_PARAMS() LT 2 THEN BEGIN
       NXprinterror, 'ERROR: Insufficient number of arguments to NXmakelink'
       return, 0
    END 
;--------- check our handle
   iRET = NXcheck(handle)
     IF iRET NE 1 THEN return, 0
;----------- install error handler
   catch, errvar
   IF errvar NE 0 THEN BEGIN
      NXprinterror, !ERR_String
      return, 0
   END 
;--------- invalid if no vGroup open
    IF handle.iCurrentVG EQ 0 THEN BEGIN
            NXprinterror, 'ERROR: will not link into root level'
            return, 0 
    END
;----------- do it
    hdf_vg_addtr, handle.iCurrentVG, id(0), id(1)
    return, 1
END
