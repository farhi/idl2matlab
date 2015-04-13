;Dids!!! Dialog_Message, spec2hdf have been commented

function sdep, DS=ds, PS=ps, VF=vf, VS=vs, W=w

;+
; NAME:
;	SDEP
;
; PURPOSE:
;	Returns several System DEPendent parameters, like OS family (default), 
;	directory separator, path separator, etc. 
;
; CATEGORY:
;	General utilities
;
; CALLING SEQUENCE:
; 
;	Result = SDEP()
;
; INPUTS:
;	None
;
; KEYWORD PARAMETERS:
;	DS:	(Directory Separator) When set, SDEP returns the directory 
;		separator (i.e. 
;		/ (slash) under Unix or \ (backslash) under Windows).
;	PS:	(Path Separator) When set, SDEP returns the path separator 
;		(i.e. :  under Unix or ; (backslash) under Windows).
;	VS:	(Version Short) returns the idl short-version (i.e. 
;		'5' for idl 5.0.2)
;	VF:	(Version Full) returns the idl full versiom (i.e. 
;		'5.0.2' for idl 5.0.2)
;	W:	(Widget allowed?) Returns 1 when widget are allowed (i.e. 
;		!d.nane is 'WIN','X' or 'MAC') otherwise returns 0 (i.e. 'PS').
;
; OUTPUTS:
;	This function returns (with no keywords) the !version.os_family
;	value in uppercase. If any keyword is set, the returned value
;	is changed to the one described below.
;
; RESTRICTIONS:
;	Never used under Mac
;
; PROCEDURE:
;	Straightforward
;
; EXAMPLE:
;	print,sdep()
;
; MODIFICATION HISTORY:
; 	Written by:	srio@esrf.fr and dejus@aps.anl.gov
;	Sept, 1997	
;	97/10/16 srio@esrf.fr adds /w keyword.
;	98/12/23 srio@esrf.fr tentative update for Mac
;-
; returns system dependent values
; returns osversion if no keywords are set, otherwise returns the
; requested separator (ds = directory separator, ps = path separator)
; srio@esrf.fr 97-09-15 and dejus@aps.anl.gov 09/15/97.

osversion = StrUpCase(!version.os_family)

if keyword_set(ds) then begin		; directory separator
  CASE osversion OF
    'UNIX': 	return,'/'
    'WINDOWS': 	return,'\'
    'MACOS': 	return,':'
    else:		return,''
  ENDCASE
endif

if keyword_set(ps) then begin		; path separator
  CASE osversion OF
    'UNIX': 	return,':'
    'WINDOWS':	return,';'
    'MACOS': 	return,','
     else:		return,''
  ENDCASE
endif

if keyword_set(vf) then return,!version.release

if keyword_set(vs) then return,strmid(!version.release,0,1)

if keyword_set(w) then begin		; path separator
  CASE !d.name OF
    'WIN': 	return,1
    'X':	return,1
    'MAC': 	return,1
     else:	return,0
  ENDCASE
endif

return, osversion
end ; sdep

; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This function checks if the parameter is a valid handle to a Spec data file.
function __isaspechandle, handle
   a=size(handle)
   if a(2) ne 8 then return, 0
   a = tag_names(handle)
   if a(0) ne 'SPECFILE' then return, 0 else return, 1
end
; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This function returns the first token found in the string `line' and
; modifies the content of `line' to hold only the remainder of the string.
function __linecut, line
   line = strtrim(line, 1) + ' '
   len = strlen(line)
   p = strpos(line, ' ')
   aux = strmid(line, 0, p)
   p = p + 1
   line = strtrim(strmid(line, p, len - p), 2)
   return, aux
end
; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This function returns an array loaded with the tokens found in the input
; string `oldline'. Tokens are separated by a 1 or 2 white spaces according
; to the parameter `nspaces'. The function returns an error if the numbers
; of tokens found is less than `imin' or greater than `imax'. This feature
; can be cancelled by setting `imin' and/or `imax' to zero.
function __linesplit, oldline, nspaces, imin, imax
   case nspaces of
      1: begin
         line = strtrim(strcompress(' ' + oldline))
         pos = where(byte(line) eq 32b)
      end

      2: begin
         line = '  ' + strtrim(oldline, 2)
         aux0 = byte(line)
         aux = aux0 eq 32b
         pos = where(aux and [aux(1:*), 0b] and     $
                     not [0b, aux(0:n_elements(aux)-2)])
         aux0(pos) = 1B
         line = strcompress(string(aux0))
         pos = where(byte(line) eq 1B)
      end

      else: return, ''
   endcase

   n = n_elements(pos)
   if n eq 1 then begin
      list = strmid(line, nspaces, strlen(line) - nspaces)
   endif else begin
      length = [pos(1:*), strlen(line)] - pos - nspaces
      pos = pos + nspaces
      list = strarr(n)
      for i = 0, n-1 do list(i) = strmid(line, pos(i), length(i))
   endelse

   if imin gt 0 and n_elements(list) lt imin then      $
      message, 'Too few fields in <'+oldline+'>' 
   if imax gt 0 and n_elements(list) gt imax then      $
      message, 'Too many fields in <'+oldline+'>' 

   return, list
end
; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This function makes up an error message.
function __cleanmsg, msg
   p = strpos(msg, ':')
   return, strtrim(strmid(msg, p+1, strlen(msg) - p), 2)
end
; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This procedure reads a new full logic line from the data file.
pro __readline, Unit, line
   readf, Unit, line
   p = strlen(line) - 1
   aux=''
   while strmid(line, p, 1) eq '\' do begin
      strput, line, ' ', p
      readf, Unit, aux
      line = line + aux
      p = strlen(line) - 1
   endwhile
end
; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This procedure loads `array' with the data in the string `line'. The data
; type depends on the initial type of array. If in `line' there are less data
; than the number of elements in `array', the remainder elements are filled
; with 0 or '' and the variable `error' is set to 1.
pro __reads, line, array, error
   if (size(array(0)))(1) ne 7 then begin
      array(*)=0
      on_ioerror, toofew
      !ERROR = 0
      reads, line, array
toofew: 
      on_ioerror, null
      if !ERROR eq 0 then error = 0 else error =1
   endif else begin
      array(*) = ''
      narr = n_elements(array)
      _line = strtrim(strcompress(' ' + line))
      pos = where(byte(_line) eq 32b)
      n = n_elements(pos)
      if n eq 1 then begin
         array(0) = strmid(_line, 1, strlen(_line) - 1)
      endif else begin
         length = [pos(1:*), strlen(_line)] - pos - 1
         nread = (narr < n)
         if n ge narr then error = 0 else error =1
         list = strarr(n)
         for i = 0, nread - 1 do begin
            array(i) = strmid(_line, pos(i) + 1, length(i))
         endfor
      endelse
   endelse
end
; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This procedure appends a scan `s' to the array `scanlist' and is only
; used by spec_access().
pro __addscan, scanlist, s, linect, range, scancounter
   s.headerlines =   linect(0)
   s.predatalines =  linect(1)
   s.datalines =     linect(2)
   s.postdatalines = linect(3)

   if scancounter eq 0 then scanlist = s else scanlist=[scanlist, s]

   scancounter = scancounter + 1 
end
; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This procedure produces and error if the parameter is not a valid handle.
pro __speccheck, handle
;Dids
   if not __isaspechandle(handle) then message, 'Variable is not a valid SPEC handle.'
end
; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This procedure loads in `handle' the information contained in the main header
; block that correspond to the current scan.
pro __headerload, handle 
   if handle.headerpt ne handle.scan(handle.currscan).headerpt then begin
      n = handle.scan(handle.currscan).headerlines
      motorlbl = handle.motornames
      motorlbl(*, *) = ''
      motor_no = handle.motor_no
      motor_no(*) = 0
      epoch = -1L
      nmotlines = -1
      if n gt 0 then begin
         openr, Unit, handle.specfile, /get_lun
         point_lun, Unit, handle.scan(handle.currscan).headerpt
         line = ''
         motline = 0
	 ; srio 98/08/25 places "nmotlines = -1" outside the "if" block
         ;nmotlines = -1
         for i = 1, n do begin
            __readline, Unit, line
            case strmid(line, 1, 1) of
               'E': begin
                  epoch=long(strmid(line, 2, strlen(line)-2))
               end

               'O': begin
                  aux = __linecut(line)
                  reads, aux, format='(2X,I)', motline
                  aux = __linesplit(line, 2, 0, 0)
                  if aux(0) eq '' then begin
                     motor_no(motline) = 0
                  endif else begin
                     motor_no(motline) = n_elements(aux)
                     motorlbl(motline, 0:n_elements(aux) - 1) = aux
                  endelse
                  if motline + 1 gt nmotlines then nmotlines = motline + 1
               end

               else:

            endcase
         endfor
         free_lun, Unit
      endif
      handle.headerpt = handle.scan(handle.currscan).headerpt
      handle.epoch = epoch
      handle.nmotorlines = nmotlines
      handle.motor_no = motor_no
      handle.motornames = motorlbl
   endif
end
; INTERNAL FUNCTIONS AND PROCEDURES
;___________________________________
;
; This procedure makes the scan specified by `scan_id' the current one.
pro __specload, handle, scan_id, errmsg, INDEX=idx

   aux = size(scan_id)
   if aux(0) ne 0 then begin
      message, 'Scan number must be a scalar value'
   endif

   n = handle.currscan
   if n ge 0 then begin
      curr_n = handle.scan(n).scan_n
      curr_ap = handle.scan(n).scan_ap
   endif else begin
      curr_n = -1
      curr_ap = -1
   endelse

   if aux(1) eq 0 then begin
   endif else if aux(1) eq 7 then begin
      Up_scan_id = strupcase(scan_id)
      case Up_scan_id of
         'CURRENT': begin
            if n lt 0 then begin
               errmsg = 'No current scan'
               !ERR = 1
               return 
            endif else begin
               goto, load
            endelse
         end

         'NEXT': begin
            if n ge 0 and n lt handle.total-1  then begin
               n = n + 1
               goto, load
            endif else begin
               errmsg = 'No next scan'
               !ERR = 1
               return 
            endelse
         end

         'FIRST': begin
            if handle.total gt 0  then begin
               n = 0 
               goto, load
            endif else begin
               errmsg = 'No first scan'
               !ERR = 1
               return 
            endelse
         end

         'LAST': begin
            if handle.total gt 0  then begin
               n = handle.total - 1 
               goto, load
            endif else begin
               errmsg = 'No last scan' 
               !ERR = 1
               return 
            endelse
         end

         else: begin
            s_no = 0
            on_ioerror, isastring
            s_no = fix(scan_id)
isastring:  on_ioerror, null
            p = strpos(scan_id, '.') + 1
            if p gt 0 then begin 
               s_ap = fix(strmid(scan_id, p, strlen(scan_id)-p))
            endif else begin
               s_ap = 0
            endelse
         end
      endcase
   endif else begin
      if aux(1) lt 1 or aux(1) gt 5 then begin
         errmsg = 'Not a valid scan number.'
         !ERR = 1
         return 
      endif
      s_no = fix(scan_id)
      s_ap = round(100*(scan_id - s_no)) 
   endelse

   if s_no eq 0 or (s_no lt 0 and not keyword_set(idx)) then begin
      errmsg = 'Not a valid scan number.'
      !ERR = 1
      return 
   end

   if keyword_set(idx) then begin
      if s_no lt 0 then s_no = handle.total + s_no + 1
      if s_no gt 0 and s_no le handle.total then begin
         n = s_no - 1
         goto, load
      endif else begin
         errmsg = 'Not enough scans in "' + handle.specfile + '".'
         !ERR = 1
         return 
      endelse
   endif else if s_no eq curr_n and (s_ap eq 0 or s_ap eq curr_ap) then begin
      goto, load
   endif 
   if s_ap eq 0 then begin
      if n ge 0 then begin
         for n = n, handle.total-1 do begin
            if handle.scan(n).scan_n eq s_no then goto, load 
         endfor
      endif
      s_ap=1
   endif
   for n = 0, handle.total-1 do begin
      if handle.scan(n).scan_n  eq s_no and           $
         handle.scan(n).scan_ap eq s_ap then begin
            goto, load
      endif
   endfor
   
   errmsg = 'Scan '+strtrim(string(s_no),2)+'['+strtrim(string(s_ap),2)+ $
            '] not found in "'+ handle.specfile + '".'
   !ERR = 1
   return 

LOAD:
   if n ne handle.currscan then begin
      handle.label(*)=''

      if handle.scan(n).n_col gt 0 and handle.scan(n).labelpt gt 0 then begin
         openr,Unit,handle.specfile,/get_lun
         point_lun, Unit, handle.scan(n).labelpt+2
         line=''
         __readline, Unit, line
         free_lun, Unit

         aux = __linesplit(line, 2, handle.scan(n).n_col, handle.scan(n).n_col)
         handle.label=aux 
      endif

      handle.currscan = n
   endif
   !ERR = 0
   end
;______________________________________________________________________________
;+
; NAME:
;       SPEC_COLUMNS()
;
; PURPOSE:
;       This function returns the number of data columns of a particular scan
;       in a SPEC file.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_COLUMNS(Handle, Scan_Id)
;
; INPUTS:
;       Handle   - Handle to the SPEC data file initialised by a previous call
;                  to SPEC_ACCESS().
;
;       Scan_Id  - Scan identifier. This parameter can be a numeric or string
;                  scalar value that accepts several syntax and keywords as
;                  it is explained in the `SCAN IDENTIFIERS' section at the
;                  beginning of this file). The simplest case is to use
;                  the SPEC scan number as Scan_Id.
;
; KEYWORDS:
;       INDEX:  Set this keyword to interpret a numeric value in Scan_Id
;               as the index of the scan instead of the scan number.
;
;       MCDATA: Set this keyword to return the number of channels in the
;               multichannel data lines instead of the number of standard
;               data columns.
;
; OUTPUT: 
;       The number of columns in the selected scan. If the MCDATA keyword is
;       not set the information is obtained from the `#N' line in the SPEC
;       data file and corresponds to the number of standard data columns.
;       When MCDATA is set, the returned value is the number of channels in
;       the multichannel data lines obtained from the `#@CHANN' line.
;
; RESTRICTIONS:
;       This function requires a valid handle to a SPEC data file obtained by
;       a previous call to SPEC_ACCESS().
;       If Scan_Id does not represent a valid scan the function produces an
;       error.
;
; EXAMPLE:
;       To print the number of data columns in the second occurrence of the
;       scan 150 in the file 'baddata', enter:
;
;       Dummy = SPEC_ACCESS(File1_handle, 'baddata')
;       PRINT, SPEC_COLUMNS(File1_handle, 150.02)
;______________________________________________________________________________
;-

function spec_columns, handle, scan_id, INDEX=idx, MCDATA=mcdata
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      message, __cleanmsg(!err_string) ;, /traceback
   endif

   ; Check arguments
   if N_PARAMS() ne 2 then message, 'Incorrect number of arguments.'
   __speccheck, handle
   __specload, handle, scan_id, errmsg, INDEX=idx
   if !ERR then message, errmsg

   if keyword_set(mcdata) then begin
      return, handle.scan(handle.currscan).n_chan
   endif else begin
      return, handle.scan(handle.currscan).n_col 
   endelse
end
;______________________________________________________________________________
;+
; NAME:
;       SPEC_DATA()
;
; PURPOSE:
;       This function returns an array with data from a scan in a SPEC file.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_DATA(Handle, Scan_Id [, Columns])
;
; INPUTS:
;       Handle   - Handle to the SPEC data file initialised by a previous call
;                  to SPEC_ACCESS().
;
;       Scan_Id  - Scan identifier. This parameter can be a numeric or string
;                  scalar value that accepts several syntax and keywords as
;                  it is explained in the `SCAN IDENTIFIERS' section at the
;                  beginning of this file). The simplest case is to use
;                  the SPEC scan number as Scan_Id.
;
; OPTIONAL INPUTS:
;       Columns  - Vector that describes the columns to be loaded in the output
;                  array. This vector can be of either numeric or string type.
;                  When numbers are used columns are selected by their relative
;                  position in the data file. When non-numeric strings are used
;                  they must match the labels of the columns to select. By
;                  using a string array it is possible to combine both
;                  selection methods.
;                  If negative numbers are used it is assumed that the column
;                  position is obtained by counting backwards from the last
;                  one: i.e. -1 means the last column, -2 the one before the
;                  last, and so on.
;
; KEYWORDS:
;       INDEX:  Set this keyword to interpret a numeric value in Scan_Id
;               as the index of the scan instead of the scan number.
;
;       MCDATA: Set this keyword to return data from the multichannel data
;               lines instead of the standard data columns.
;
;       LABELS: String vector to be filled with the labels of the returned
;               columns. This option can only be used if the MCDATA keyword
;               is not set.
;
;       OFFSET: Number of points that will be skipped before extracting the
;               required data. This value is defaulted to 0.
;
;       STEP:   Interval used to extract data points. Only the first one of
;               each group of STEP consecutive data points will be extracted.
;               The default value is 1.
;
;       NPOINTS: Maximum number of points that will be returned. If it is
;               zero or greater than the actual number of available data
;               points, this keyword has not any effect.
;
;       CHANNELS: Float vector to be filled with the channel numbers of the
;               multichannel data. It is not used if the MCDATA keyword is
;               not set.
;
;       CALIBRATED: Float vector to be filled with the calibrated values
;               (usually energy) of the channels in the multichannel data set.
;               It is not used if the MCDATA keyword is not set.
;
;       STRING: Set this keyword to force the output to be a string array. 
;
;       DOUBLE: Set this keyword to force the output to be a double-precision
;               array.
;
;       FLOAT:  Set this keyword to force the output to be a single-precision
;               floating-point array.
;
;       LONG:   Set this keyword to force the output to be a longword integer
;               array.
;
;       INT:    Set this keyword to force the output to be an integer array.
;
; OUTPUT: 
;       This function returns an array containing the data columns selected by
;       the Columns parameter. If no Columns parameter is given, all the
;       columns in the selected scan are returned. By default the output 
;       is a double-precision array for standard data and a long array for
;       multichannel data, but the output array can be forced to any type by
;       setting the proper keyword.
;       If a particular column does not exist in the SPEC file, the
;       corresponding column in the output array is filled with zeros.
;
; RESTRICTIONS:
;       This function requires a valid handle to a SPEC data file obtained by
;       a previous call to SPEC_ACCESS().
;       If Scan_Id does not represent a valid scan the function produces an
;       error.
;
; EXAMPLE:
;       To load the array GoodData with the position of the first column of the
;       scan 5, the monitor counts and the detector counts (that are assumed to
;       be in the last column of the file), enter:
;
;       Dummy = SPEC_ACCESS(File_handle, 'weakdata')
;       GoodData = SPEC_DATA(File_handle, 27, ['1', 'Monitor', '-1'])
;
;______________________________________________________________________________
;-

function spec_data, handle, scan_id, columns, INDEX=idx, MCDATA=mcdata,        $
                           LABELS=labels, OFFSET=offset, STEP=step,NPOINTS=np,$
                           CHANNELS=chann, CALIBRATED=calib,                  $
                           DOUBLE=dtype, FLOAT=ftype, LONG=ltype, INT=itype,  $
                           STRING=stype
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      message, __cleanmsg(!err_string);, /traceback
   endif

   ; Check arguments
   if N_PARAMS() lt 2 or N_PARAMS() gt 3 then  $
                                   message, 'Incorrect number of arguments.'
   __speccheck, handle
   __specload, handle, scan_id, errmsg, INDEX=idx
   if !ERR then message, errmsg

   if keyword_set(mcdata) and keyword_set(labels) then  $
                                   message, 'No labels in multichannel data.'
   if not keyword_set(offset) then offset = 0
   if not keyword_set(step) then step = 1
   if not keyword_set(np) then np = 0
   offset = long(offset)
   step = long(step)
   np = long(np)
   if offset lt 0 then message, 'OFFSET must be a positive value.'
   if step lt 1 then message, 'STEP must be >= 1.'
   if step lt 0 then message, 'NPOINTS must be >= 0.'

   if keyword_set(mcdata) then begin
      n = handle.scan(handle.currscan).n_chan
      npoints = handle.scan(handle.currscan).a_datapoints
   endif else begin
      n = handle.scan(handle.currscan).n_col
      npoints = handle.scan(handle.currscan).n_datapoints
   endelse
   if npoints le 0 then message, 'No data points.'
   if npoints le offset then message, 'Offset too big.'
   npoints = long((npoints - offset - 1)/step) + 1
   if np gt 0 and npoints gt np then npoints = np

   if N_PARAMS() eq 2 then begin
      ncols = n
      index = indgen(ncols)
   endif else begin
      aux = size(columns)
      if aux(0) gt 1 then message, 'Too many dimensions.'
      ncols= aux(aux(0) + 2)
      index=intarr(ncols)
      aux = aux(aux(0) + 1)
      if aux gt 0 and aux lt 6 then begin
         index = fix(columns)  
      endif else if aux eq 7 then begin
         for i=0, ncols-1 do begin
            c=0
            on_ioerror, isastring
            c=fix(columns(i)) 
isastring:  on_ioerror, null
            if c eq 0 then begin
               if keyword_set(mcdata) then begin
                  message, "Not labels allowed with multichannel data."
               endif
               for j=1, n do begin
                  if columns(i) eq handle.label(j-1) then begin
                     c=j 
                     goto, set
                  endif
               endfor
            endif 
set:        index(i)=c
         endfor
      endif else begin
         message, "Bad data type."
      endelse
      index = index * (index ge -n and index le n) - 1
      index = index + (n+1)*(index lt -1) 
   endelse

   aux = where(index lt 0)
   if aux(0) ne -1 then begin
      message, "Wrong column specification [" +                  $
                                    strtrim(string(columns(aux(0))),2) + "]."
   endif

   if keyword_set(stype) then begin
      data = strarr(ncols, npoints)
      datal = strarr(n)
      datatype = 2
   endif else if keyword_set(mcdata) and not keyword_set(ftype) and  $
                                      not keyword_set(dtype) then begin
      data = lonarr(ncols, npoints)
      datal = lonarr(n)
      datatype = 1
   endif else begin
      data = dblarr(ncols, npoints)
      datal = dblarr(n)
      datatype = 0
   endelse

   openr,Unit,handle.specfile,/get_lun
   point_lun, Unit, handle.scan(handle.currscan).datapt
   line=''
   point = -offset
   pt = 0L
   for i = 0L, handle.scan(handle.currscan).datalines -1 do begin
      if pt eq npoints then goto, done
      __readline, Unit, line
      ok = point ge 0 and (point mod step) eq 0
      found = 0
      case strmid(line, 0, 1) of
         '':  ; Empty line. Do nothing

         '#': ; Header or comment. Do nothing

         '@': begin
            if keyword_set(mcdata) and strmid(line, 1, 1) eq 'A' then begin
               found = 1
               if ok then begin
                  aux0 = __linecut(line)
                  __reads, line, datal
               endif
            endif
         end

         else: begin
            if not keyword_set(mcdata) then begin
               found = 1
               if ok then begin
                  __reads, line, datal
               endif
            endif
         end
      endcase
      if found then begin
         if ok then begin
            data(*, pt) = datal(index)
            pt = pt + 1
         endif
         point = point + 1
      endif
   endfor
done:
   if keyword_set(mcdata) then begin
      nchan = 0      ; integer
      chmin = 0.     ; float
      chmax = 0.     ; float
      Chreduc = 0    ; integer
      calA = 0.      ; float
      calB = 1.      ; float
      calC = 0.      ; float
      point_lun, Unit, handle.scan(handle.currscan).scanpt
      for i = 1, handle.scan(handle.currscan).predatalines do begin
         __readline, Unit, line
         if strmid(line, 0, 7) eq '#@CHANN' then begin
            line = strmid(line, 7, strlen(line)-7) + ' 0 0 0 0'
            reads, line, nchan, chmin, chmax, chreduc
            if chreduc eq 0 then chreduc = 1
            if chmin eq chmax and nchan gt 1 then begin
               chmax = chmin + (nchan - 1) * chreduc
            endif
         endif else if strmid(line, 0, 7) eq '#@CALIB' then begin
            line = strmid(line, 7, strlen(line)-7) + ' 0 0 0'
            reads, line, calA, calB, calC 
         endif
      endfor
      if nchan gt 1 then begin
         chann = chmin + findgen(nchan)*(chmax-chmin)/(nchan - 1)
      endif else begin
         chann = chmin
      endelse
      calib = calA + chann * (calB + chann * calC)
   endif
   free_lun, Unit

   if not keyword_set(mcdata) then begin
      aux = [handle.label, '']
      labels = aux(index)
   endif

   if keyword_set(stype) then return, data
   if keyword_set(ftype) then return, float(data)
   if keyword_set(itype) then return, fix(data)
   if keyword_set(ltype) then return, long(data)
   return, reform(data)
end
;______________________________________________________________________________
;+
; NAME:
;       SPEC_HEADERS()
;
; PURPOSE:
;       This function returns header lines from a particular scan in a SPEC
;       data file. Header lines begin by the '#' character and can be situated
;       before or after the block of data lines.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_HEADERS(Handle, Scan_Id [, Selec_Array])
;
; INPUTS:
;       Handle   - Handle to the SPEC data file initialised by a previous call
;                  to SPEC_ACCESS().
;
;       Scan_Id  - Scan identifier. This parameter can be a numeric or string
;                  scalar value that accepts several syntax and keywords as
;                  it is explained in the `SCAN IDENTIFIERS' section at the
;                  beginning of this file). The simplest case is to use
;                  the SPEC scan number as Scan_Id.
;
; OPTIONAL INPUTS:
;       Selec_Array - Array that contains selectors to the header lines.
;                  A selector is a string with the first characters of the
;                  header lines that are wanted to be returned. A selector can
;                  include or not the first '#' character of the header lines.
;                  For instance, the string 'P' selects all the headers in the
;                  scan that contain motor positions (those that begin with 
;                  the '#P' characters) and the string '#G0' selects the 
;                  first line of geometry parameters.
;
; KEYWORDS:
;       INDEX:  Set this keyword to interpret a numeric value in Scan_Id
;               as the index of the scan instead of the scan number.
;
;       ALL:    Set this keyword to extend the scope of the search and include
;               the main block of header lines as well as comments embedded in
;               the data block.
;               In normal SPEC data files the main header block is placed at
;               the beginning of the file and includes the file name line ('#F')
;               the epoch line ('#E') and the motor names ('#O' lines). If 
;               there are more than one main header block in the file, the
;               function uses the last one before the given scan.
;               Comment lines appear embedded in the data block when a scan
;               is resumed after been aborted from SPEC.
;
; OUTPUT: 
;       This function returns a string array that contains all the header lines
;       that match the selection criteria. If Selec_Array is not specified
;       all the header lines are returned.
;
; RESTRICTIONS:
;       This function requires a valid handle to a SPEC data file obtained by
;       a previous call to SPEC_ACCESS().
;       If Scan_Id does not represent a valid scan the function produces an
;       error.
;
; EXAMPLE:
;       To load the string array NewHeaders with the header lines that contain
;       the positions of the first eight motors ('#P0' line) and the user
;       defined values ('#U' lines), enter:
;
;       Dummy = SPEC_ACCESS(FileHandle, 'baddata')
;       Nscans = SPEC_HEADERS(FileHandle, BestScan, ['#P0','U'])
;
;       (the file in this example is 'baddata' and the scan number is in the
;       variable called BestScan)
;______________________________________________________________________________
;-

function spec_headers, handle, scan_id, headers, ALL=all, INDEX=idx
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      message, __cleanmsg(!err_string);, /traceback
   endif

   ; Check arguments
   if N_PARAMS() lt 2 then message, 'Incorrect number of arguments.'
   if N_PARAMS() eq 2 then begin
      headers = ''
   endif
   __speccheck, handle
   __specload, handle, scan_id, errmsg, INDEX=idx
   if !ERR then message, errmsg
   if keyword_set(all) then begin
      __headerload, handle
      nlh = handle.scan(handle.currscan).headerlines 
   endif else begin
      nlh = 0L
   endelse

   aux = size(headers)
   if aux(aux(0) + 1) ne 7 or aux(0) gt 1 then begin
       message, 'Argument is not a valid header specification.'
   endif
   nheaders = aux(aux(0) + 2)
   _headers = strarr(nheaders)

   for i = 0L, nheaders - 1 do begin
      if strmid(headers(i), 0, 1) ne '#' then begin
         _headers(i) = '#' + headers(i)
      endif else begin
         _headers(i) = headers(i)
      endelse
   endfor
   headlen = strlen(_headers)

   nl0 = nlh + handle.scan(handle.currscan).predatalines 
   nl1 = nl0 + handle.scan(handle.currscan).datalines
   nlt = nl1 + handle.scan(handle.currscan).postdatalines

   openr,Unit,handle.specfile,/get_lun
   if nlh gt 0 then begin
      point_lun, Unit, handle.scan(handle.currscan).headerpt
   endif 
   line = ''
   ct = 0L
   for i = 1L, nlt do begin
      if i eq nlh + 1 then begin
         point_lun, Unit, handle.scan(handle.currscan).scanpt
      endif
      __readline, Unit, line
      if i le nl0 or i gt nl1 or keyword_set(all) then begin
         if line ne '' and strmid(line, 0, 1) eq '#' then begin
            for j = 0L, nheaders - 1 do begin
               if strmid(line, 0, headlen(j)) eq _headers(j) then begin
                  if ct eq 0 then begin
                     result = line
                  endif else begin
                     result = [result, line]
                  endelse
                  ct = ct + 1
                  goto, outfor
               endif
            endfor
outfor:
         endif
      endif
   endfor
   free_lun, Unit

   if ct eq 0 then begin
      return, ''
   endif else begin
      return, result
   endelse
end
;______________________________________________________________________________
;+
; NAME:
;       SPEC_LABELS()
;
; PURPOSE:
;       This function returns the labels of data columns of a particular scan
;       in a SPEC file.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_LABELS(Handle, Scan_Id)
;
; INPUTS:
;       Handle   - Handle to the SPEC data file initialised by a previous call
;                  to SPEC_ACCESS().
;
;       Scan_Id  - Scan identifier. This parameter can be a numeric or string
;                  scalar value that accepts several syntax and keywords as
;                  it is explained in the `SCAN IDENTIFIERS' section at the
;                  beginning of this file). The simplest case is to use
;                  the SPEC scan number as Scan_Id.
;
; KEYWORDS:
;       INDEX:  Set this keyword to interpret a numeric value in Scan_Id
;               as the index of the scan instead of the scan number.
;
; OUTPUT: 
;       This function returns an string array that contains the labels of the
;       the data columns in the SPEC file. The information is extracted from
;       the '#L' line. The dimension of the array depends on the number of
;       columns of this particular scan.
;
; RESTRICTIONS:
;       This function requires a valid handle to a SPEC data file obtained by
;       a previous call to SPEC_ACCESS().
;       If Scan_Id does not represent a valid scan the function produces an
;       error.
;
; EXAMPLE:
;       To load the array NewLabels with the labels of the data columns of the
;       scan no. 27 from a file called 'karaoke', enter:
;
;       Dummy = SPEC_ACCESS(File1_handle, 'karaoke')
;       NewLabels = SPEC_LABELS(File1_handle, 27)
;______________________________________________________________________________
;-

function spec_labels, handle, scan_id, INDEX=idx
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      message, __cleanmsg(!err_string);, /traceback
   endif

   ; Check arguments
   if N_PARAMS() ne 2 then message, 'Incorrect number of arguments.'

   __speccheck, handle
   __specload, handle, scan_id, errmsg, INDEX=idx
   if !ERR then message, errmsg
   n=handle.scan(handle.currscan).n_col
   if n gt 0 then return, handle.label(0:n-1) else return, ''
end
;______________________________________________________________________________
;+
; NAME:
;       SPEC_MOTORS()
;
; PURPOSE:
;       This function returns the names or position of motors in a particular
;       scan in a SPEC file.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_MOTORS(Handle, Scan_Id [, MotorList [, BadValue]])
;
; INPUTS:
;       Handle   - Handle to the SPEC data file initialised by a previous call
;                  to SPEC_ACCESS().
;
;       Scan_Id  - Scan identifier. This parameter can be a numeric or string
;                  scalar value that accepts several syntax and keywords as
;                  it is explained in the `SCAN IDENTIFIERS' section at the
;                  beginning of this file). The simplest case is to use
;                  the SPEC scan number as Scan_Id.
;
; OPTIONAL INPUTS:
;       MotorList - String array that contains the names of the motors which
;                  position is requested.
;                  In addition to motor names, it is also possible to specify
;                  a motor by using a string with the format 'line,motor' where
;                  `line' in the number of the `#P' header line that includes
;                  the motor information, and `motor' is the relative position
;                  of the motor in that line. For instance, '2,1' refers to
;                  the first motor in the `#P2' line.
;
;       BadValue - Dummy value to be returned as a motor position if the
;                  corresponding motor is not found or when the headers are
;                  corrupted and the NOERROR ketword is set. This value is
;                  defaulted to 1E20.
;
; KEYWORDS:
;       INDEX:  Set this keyword to interpret a numeric value in Scan_Id
;               as the index of the scan instead of the scan number.
;
;       NOERROR: Set this keyword to not return an error when the file headers
;               are corrupted. 
;
; OUTPUT: 
;       If MotorList is specified this function returns the position of the
;       motors included in the list. Otherwise it returns a string array
;       that contains the names of all the motors associated to the given scan.
;
; RESTRICTIONS:
;       This function requires a valid handle to a SPEC data file obtained by
;       a previous call to SPEC_ACCESS().
;       If Scan_Id does not represent a valid scan the function produces an
;       error.
;       If the file was created with an old version of SPEC and a motor name
;       has more than 8 characters this function will not work. It will not
;       work if a motor name includes two or more consecutive white spaces.
;
; EXAMPLE:
;       To print the position of the motors 'Theta', 'Piezo 1' and the second
;       motor of the first `#P' line in the fifth scan of a file called
;       'baddata', enter:
;
;       Dummy = SPEC_ACCESS(HHH, 'baddata')
;       PRINT, SPEC_MOTORS(HHH, -5, ['Theta', 'Piezo 1', '0,2'])
;______________________________________________________________________________
;-
function spec_motors, handle, scan_id, motorlist, badvalue, INDEX=idx,   $
                                                           NOERROR=noerror
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      message, __cleanmsg(!err_string);, /traceback
   endif

   ; Check arguments
   if N_PARAMS() lt 2 or N_PARAMS() gt 4 then       $
                                    message, 'Incorrect number of arguments.'

   __speccheck, handle
   __specload, handle, scan_id, errmsg, INDEX=idx
   if !ERR then message, errmsg
   __headerload, handle
 
   if N_PARAMS() ge 3 then begin
      aux = size(motorlist)
      if aux(aux(0) + 1) eq 7 then begin
         list = strtrim(motorlist, 2)
      endif else begin
         message, 'Argument is not a valid motor name list.'
      endelse
   endif

   if N_PARAMS() lt 4 then begin
      returnbadvalue = 0
      badvalue = 1E20
   endif else begin
      returnbadvalue = 1
   endelse

   headersP = spec_headers(handle, 'current', '#P')
   motorlines = n_elements(headersP)

   if motorlines eq 0 then begin
      rows = 0
   endif else begin
      for i = 0, motorlines - 1 do begin
         auxline = headersP(i)
         aux = __linecut(auxline)
         headersP(i) = auxline
         reads, aux, format='(2X,I)', motline
         mot = __linesplit(auxline, 1, 0, 0)
         if i eq 0 then begin
            linelist = motline
            motlist = n_elements(mot)
         endif else begin
            linelist = [linelist, motline]
            motlist = [motlist, n_elements(mot)]
         endelse
      endfor
      rows = max(linelist) + 1
      cols = intarr(rows)
      allmotpos = fltarr(rows, max(motlist))

      for i = 0, motorlines - 1 do begin
         motline = linelist(i)
         cols(motline) = motlist(i)
         mot = __linesplit(headersP(i), 1, 0, 0)
         allmotpos(motline, 0:cols(motline)-1) = float(mot(0:cols(motline)-1))
      endfor
   endelse

   if handle.nmotorlines ne rows then begin
      corrupted = 1
   endif else begin
      corrupted = 0
      for i = 0, rows - 1 do begin
         if cols(i) ne handle.motor_no(i) then begin
            corrupted = 1
         endif
      endfor
   endelse

   if corrupted and not keyword_set(noerror) then begin
      message, 'File headers corrupted'
   endif

   if N_PARAMS() eq 2 then begin
      motornames = ''
      if not corrupted then begin
         for i = 0, handle.nmotorlines - 1 do begin
            for j = 0, handle.motor_no(i) - 1 do begin
               if motornames(0) eq '' then begin
                  motornames = handle.motornames(i, j)
               endif else begin
                  motornames = [motornames, handle.motornames(i, j)]
               endelse
            endfor
         endfor
      endif
      return, motornames
   endif else begin
      for k = 0, n_elements(list) - 1 do begin
         pos = badvalue
         motfound = 0
         for i = 0, rows - 1 do begin
            for j = 0, cols(i) - 1 do begin
               if not corrupted then begin
                  if handle.motornames(i, j) eq list(k) then begin
                     pos = allmotpos(i, j)
                     motfound = 1
                     goto, found
                  endif
               endif
            endfor
         endfor
         if not motfound then begin
            p = strpos(list(k), ',')
            if p gt 0 then begin
               on_ioerror, arestrings
               x = fix(strmid(list(k), 0, p))
               y = fix(strmid(list(k), p+1, strlen(list(k))-p-1)) - 1
               if x ge 0 and x lt rows then begin
                  if y ge 0 and y le cols(x) then begin
                     pos = allmotpos(x, y)
                     motfound = 1
                  endif
               endif
arestrings:    on_ioerror, null
            endif
         endif
found:
         if motfound or returnbadvalue or keyword_set(noerror) then begin
            if k eq 0 then begin
               motorpos = pos
            endif else begin
               motorpos = [motorpos, pos]
            endelse
         endif else begin
            message, 'Motor "'+ list(k) +'" not found.'
         endelse
      endfor
      return, motorpos
   endelse

end
;______________________________________________________________________________
;+
; NAME:
;       SPEC_NAME()
;
; PURPOSE:
;       This function returns the name of a particular scan in a SPEC file.
;       In most of the cases the name of a scan is the full macro that was
;       used to produce obtain the data.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_NAME(Handle, Scan_Id)
;
; INPUTS:
;       Handle   - Handle to the SPEC data file initialised by a previous call
;                  to SPEC_ACCESS().
;
;       Scan_Id  - Scan identifier. This parameter can be a numeric or string
;                  scalar value that accepts several syntax and keywords as
;                  it is explained in the `SCAN IDENTIFIERS' section at the
;                  beginning of this file). The simplest case is to use
;                  the SPEC scan number as Scan_Id.
;
; KEYWORDS:
;       INDEX:  Set this keyword to interpret a numeric value in Scan_Id
;               as the index of the scan instead of the scan number.
;
; OUTPUT: 
;       The name of the selected scan. The information is obtained from the
;       '#S' line.
;
; RESTRICTIONS:
;       This function requires a valid handle to a SPEC data file obtained by
;       a previous call to SPEC_ACCESS().
;       If Scan_Id does not represent a valid scan the function produces an
;       error.
;
; EXAMPLE:
;       To print the name of the scan 150 from a file called 'baddata', enter:
;
;       Dummy = SPEC_ACCESS(File1_handle, 'baddata')
;       PRINT, SPEC_NAME(File1_handle, 150)
;______________________________________________________________________________
;-

function spec_name, handle, scan_id, INDEX=idx
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      message, __cleanmsg(!err_string);, /traceback
   endif

   ; Check arguments
   if N_PARAMS() ne 2 then message, 'Incorrect number of arguments.'

   __speccheck, handle
   __specload, handle, scan_id, errmsg, INDEX=idx
   if !ERR then message, errmsg
   return, handle.scan(handle.currscan).name 
end
;______________________________________________________________________________
;+
; NAME:
;       SPEC_POINTS()
;
; PURPOSE:
;       This function returns the number of data points of a particular scan
;       in a SPEC file.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_POINTS(Handle, Scan_Id)
;
; INPUTS:
;       Handle   - Handle to the SPEC data file initialised by a previous call
;                  to SPEC_ACCESS().
;
;       Scan_Id  - Scan identifier. This parameter can be a numeric or string
;                  scalar value that accepts various syntax and keywords as
;                  it is explained in the `SCAN IDENTIFIERS' section at the
;                  beginning of this file). The simplest case is to use
;                  the SPEC scan number as Scan_Id.
;
; KEYWORDS:
;       INDEX:  Set this keyword to interpret a numeric value in Scan_Id
;               as the index of the scan instead of the scan number.
;
;       MCDATA: Set this keyword to make the function return the number of
;               points with multichannel data (MCA, ...) instead of the 
;               conventional data. 
;
; OUTPUT: 
;       The number of data points in the scan specified by Scan_Id.
;
; RESTRICTIONS:
;       This function requires a valid handle to a SPEC data file obtained by
;       a previous call to SPEC_ACCESS().
;       If Scan_Id does not represent a valid scan the function produces an
;       error.
;
; EXAMPLE:
;       To print the number of data points in the scan 999 from a file called
;       'sosada', enter:
;
;       Dummy = SPEC_ACCESS(File1_handle, 'sosada')
;       PRINT, SPEC_POINTS(File1_handle, 999)
;______________________________________________________________________________
;-

function spec_points, handle, scan_id, INDEX=idx, MCDATA=mcdata
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      message, __cleanmsg(!err_string);, /traceback
   endif

   ; Check arguments
   if N_PARAMS() ne 2 then message, 'Incorrect number of arguments.'

   __speccheck, handle
   __specload, handle, scan_id, errmsg, INDEX=idx
   if !ERR then message, errmsg

   if keyword_set(mcdata) then begin
      npoints = handle.scan(handle.currscan).a_datapoints
   endif else begin
      npoints = handle.scan(handle.currscan).n_datapoints
   endelse
   return, npoints 
end
;______________________________________________________________________________
;+
; NAME:
;       SPEC_SCAN()
;
; PURPOSE:
;       This function returns a vector loaded with the numbers of a group of
;       scans in a SPEC file.
;       The values returned correspond to either the numbers ('#S' lines) of
;       the scans or their indexes (relative position in the file).
;       This function can be used to check if a scan or group of scans exist
;       in a SPEC data file before calling any other function of the library.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_SCAN(Handle, ScanList [, ErrMsg])
;
; INPUTS:
;       Handle   - Handle to the SPEC data file initialised by a previous call
;                  to SPEC_ACCESS().
;
;       ScanList - List of scans in the SPEC data file. ScanList can be a 
;                  scalar or an array. In the case of arrays all the elements 
;                  are included in the list. ScanList can also be of numeric
;                  or string type.
;                  When using character strings one can specify a group of
;                  consecutive scans in compressed form using the syntax
;                  `first-last'. If `first' or `last' are not specified they
;                  are defaulted to the first and last scan in the file. It is
;                  also possible to include several scans or groups of scans
;                  in a single string by using commas or white spaces as
;                  separators.
;                  The values in ScanList follow the same convention used for
;                  scan identifiers (see the `SCAN IDENTIFIERS' section at the
;                  beginning of this file). By default, numeric values are
;                  interpreted as SPEC scan numbers, but this behaviour is
;                  changed by the INDEX keyword.
;
; KEYWORDS:
;       INDEX:  Set this keyword to interpret the numeric values in ScanList
;               as the indexes of the scans instead of as scan numbers.
;               Care must be taken when using indexes in strings that specify
;               groups of scans. In order to allow the use of negative indexes
;               the sign of the index must be explicitly included when the
;               expression can be ambiguous. As an example compare the
;               following expressions when INDEX is set: 
;                  '-2'    represents the scan before the last one (index=-2).
;                  '--2'   represents the scans from the first to the one
;                           before the last (index=1 to index=-2).
;                  '-+2'   represents the first two scans in the file
;                           (index=1 to index=2).
;
;       RETURN_INDEX: Set this keyword to return the index of the scans instead
;               of the scan numbers.
;
;       OCCURRENCE: Set this keyword to return the scan number and the scan
;               occurrence in the format SSSS.OO as explained in the `SCAN
;               IDENTIFIERS' section at the beginning of this file). This 
;               keyword has not any effect if RETURN_INDEX is also set.
;
;       STRING: Set this keyword to force the output as a string vector instead
;               of float.
;
;       NO_EMPTY: Set this keyword to make the output value of the function
;               refer only to those scans that actually contain standard data
;               columns, excluding the empty ones.
;
;       MCDATA: Set this keyword to make the output value of the function
;               refer only to those scans that contain multichannel data.
;
;       NO_RANGE: Set this keyword to force the function to accept only 
;               single scans as input instead of lists.
;
; OUTPUT: 
;       If there is no error this function returns a vector that contains the
;       list of scans specified in ScanList. If any of the scans is not found
;       or any other error occurs, the function return -1.
;
; OPTIONAL OUTPUTS:
;
;       ErrMsg  - Error message if an error occurs.
;    
; SIDE EFFECTS:
;       The `current' scan is set to the last scan in the list.
;
; RESTRICTIONS:
;       This function requires a valid handle to a SPEC data file obtained by
;       a previous call to SPEC_ACCESS().
;
; EXAMPLE:
;       To print the number of most recently accessed scan, enter:
;
;       PRINT, SPEC_SCAN(OldHandle, 'current')
;______________________________________________________________________________
;-
function spec_scan, handle, scan_list, errmsg, INDEX=idx, RETURN_INDEX=rindex,$
                                               OCCURRENCE=occurr, STRING=str, $
                                               NO_EMPTY=noemp, MCDATA=mca,    $
                                               NO_RANGE=no_range
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      message, __cleanmsg(!err_string);, /traceback
   endif

   ; Check arguments
   if N_PARAMS() ne 2 and N_PARAMS() ne 3 then begin
      message, 'Incorrect number of arguments.'
   endif
   errmsg = ''

   __speccheck, handle

   aux = size(scan_list)
   nelem = n_elements(scan_list)
   isstring = (aux(aux(0)+1) eq 7)
   if keyword_set(no_range) then begin
      if nelem gt 1 then return, -1
      if isstring then begin
         if strpos(scan_list(0),',') ne -1 or strpos(scan_list(0),'-') ne -1 $
                                                   then return, -1
      endif
   endif

   if not isstring then begin
      for i=0, nelem - 1 do begin
         __specload, handle, scan_list(i), errmsg, INDEX=idx
         if !ERR then return, -1
         if i eq 0 then begin
            indexlist = handle.currscan
         endif else begin
            indexlist = [indexlist, handle.currscan]
         endelse
      endfor
   endif else begin
      _scan_list = scan_list(0)
      for i=1, nelem - 1 do _scan_list = _scan_list + ',' + scan_list(i)
      _scan_list = strcompress(_scan_list, /remove_all)
      p = strpos(_scan_list, ',')
      while p ge 0 do begin
         strput, _scan_list, ' ', p
         p = strpos(_scan_list, ',')
      endwhile
      _scan_list = __linesplit(strcompress(_scan_list), 1, 0, 0)
      ngroups = n_elements(_scan_list)
      if ngroups eq 1 and _scan_list(0) eq '' then return, -1
      for i=0, ngroups-1 do begin
         p = strpos(_scan_list(i), '-')
         if p eq 0 and keyword_set(idx) then begin
            nextchar = strmid(_scan_list(i), 1, 1)
            if nextchar ne '-' and nextchar ne '+' and nextchar ne '' then begin
               p = strpos(_scan_list(i), '-', 1) 
            endif
         endif
         if p ge 0 then begin
            scan0 = strmid(_scan_list(i), 0, p)
            if scan0 eq '' then scan0 = 'FIRST'
            scan1 = strmid(_scan_list(i), p + 1, strlen(_scan_list(i)) - p - 1)
            if scan1 eq '' then scan1 = 'LAST'
         endif else begin
            scan0 = _scan_list(i)
            scan1 = ''
         endelse
         __specload, handle, scan0, errmsg, INDEX=idx
         if !ERR then return, -1 
         firstscan = handle.currscan
         if i eq 0 then begin
            indexlist = firstscan
         endif else begin
            indexlist = [indexlist, firstscan]
         endelse
         if scan1 ne '' then begin
            __specload, handle, scan1, errmsg, INDEX=idx
            if !ERR then return, -1 
            if handle.currscan ge firstscan then delta = 1 else delta = -1
            for j = firstscan + delta, handle.currscan, delta do begin
               indexlist = [indexlist, j]
            endfor
         end
      endfor
   endelse

   if keyword_set(mca) then begin
      dpoints = handle.scan(indexlist).a_datapoints
   endif else if keyword_set(noemp) then begin
      dpoints = handle.scan(indexlist).n_datapoints
   endif else goto, NoFilter
   aux = where(dpoints gt 0)
   if aux(0) eq -1 then return, -1
   indexlist = indexlist(aux)

NoFilter:
   if keyword_set(rindex) then begin
      result = indexlist + 1
   endif else if keyword_set(occurr) then begin
      result = handle.scan(indexlist).scan_n +     $
               handle.scan(indexlist).scan_ap/100.
   endif else begin
      result = handle.scan(indexlist).scan_n 
   endelse

   return, result
end
;______________________________________________________________________________
;+
; NAME:
;       SPEC_SAVE()
;
; PURPOSE:
;       This function saves the data contained in a 2D numeric array into a file
;       following the SPEC data file format. If the file does not exist it is created.
;       I the file exits, it must be a valid SPEC data file and the data is appended
;       as a new scan. 
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_SAVE(Handle, Data [, File])
;
; INPUTS:
;       Handle - Handle to the SPEC data file. It is initialized, if necessary, and
;                updated after the data is saved.
;
;       Data   - Numeric array containing the data to be saved. The first dimension
;                correspond to the data columns in the SPEC file.
;
; OPTIONAL INPUT/OUTPUT
;       File   - If Handle is not a valid SPEC file handle, this is parameter is the
;                name of the file where the data will be saved. Otherwise File
;                specifies a named variable where the actual name of the file will be
;                returned.
;
; KEYWORDS:
;       COLUMNS: Vector that specifies the columns in the array Data that will be
;               saved. If this keyword is not set all the columns are saved.
;
;       LABELS: String vector that contains the labels for the data columns.
;
;       COMMENTS: String vector that contains comment lines that will be included
;               in the header section of the scan.
;
;       NAME:   String that will appear as scan name in the `#S' line.
;
;       OVERWRITE: Set this keyword to overwrite the file if it exists. By default
;               data is appended to existing files.
;
;       NO_CONFIRM: Set this keyword to not ask for confirmation when writting data
;               on existing files.
;
;       NO_ERROR: Set this keyword to not produce an IDL error if an error is found
;               when writting the data. 
;
;	DIALOG_PARENT: Set this keyword equal to a string that specifies the name 
;		of the parent widget (to be passed to Dialog_Message)
;
; OUTPUT: 
;       This function returns the scan number under which the data is saved in the
;       file. If the operation is cancelled, the function returns zero. If an error
;       happens and NO_ERROR is set the function returns -1 and the error message
;       is stored in the system variable !ERR_STRING.
;
; EXAMPLE:
;       To save the data in the array NewData in the file 'Newfile', enter:
;
;       PRINT, SPEC_SAVE(DummyHandle, NewData, 'Newfile')
;______________________________________________________________________________
;-
function spec_save, handle, data, file, COLUMNS=columns, LABELS=labels,      $
                     COMMENTS=comm, NAME=name, NO_ERROR=noerror,             $
                     OVERWRITE=overwrite, NO_CONFIRM=no_conf,                $
		     DIALOG_PARENT=dialog_parent
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      if keyword_set(noerror) then return, -1
      message, __cleanmsg(!err_string);, /traceback
   endif

   aux = size(data)
   if aux(0) ne 2 then begin
      message, 'Data is not a 2D array.'
   end
   totcol = aux(1)

   if n_elements(columns) eq 0 then columns = indgen(totcol)
   ncol = n_elements(columns)
   if min(columns) lt 0 or max(columns) ge totcol then begin
      message, 'Column values are out of range.'
   endif

   nlab = n_elements(labels)
   if nlab lt ncol then begin
      extralbl= ('Column'+strtrim(indgen(ncol),2))(nlab:ncol-1)
      if nlab eq 0 then begin
         labels = extralbl
      endif else begin
         labels = [labels, extralbl]
      endelse
   endif
   labels = labels(0:ncol-1)

   if not keyword_set(name) then name = ''
;Dids
   if not keyword_set(file) then begin
      if __isaspechandle(handle) then begin
         file = handle.specfile
      endif else return, 0
   endif

   ; Check if writable and if new or SPEC file 

   openw, Unit, file, /get_lun, /append, error=err
   if err then begin
      message, 'Cannot open "'+file+'" for writting.'
   endif
   status = fstat(Unit)
   if status.size gt 0 then begin
      if spec_access(handle, file) eq 0 then begin
         free_lun, Unit
         message, 'Exists and is not a SPEC file'
      endif else begin
         scan = spec_scan(handle, 'last') + 1
      endelse
   endif else begin
      scan = 1
   endelse

   if keyword_set(overwrite) then begin
      if not keyword_set(no_conf) and scan gt 1 then begin
        ;tmp = Dialog_Message('SPEC file exists, overwrite data ?', /default_no, $
        ;                     title='Writting data', /question, $
	;		      DIALOG_PARENT=dialog_parent)
	 tmp = 'Yes'
         if tmp eq 'No' then begin
            free_lun, Unit
            return, 0
         endif
      endif
      free_lun, Unit
      openw, Unit, file, /get_lun
      scan = 1
   endif else begin
      if not keyword_set(no_conf) and scan gt 1 then begin
        ;tmp = Dialog_Message('SPEC file exists, append data ?', /question, $
        ;                      title='Writting data',		       $
	;		       DIALOG_PARENT=dialog_parent)
	 tmp = 'Yes'
         if tmp eq 'No' then begin
            free_lun, Unit
            return, 0
         endif
      endif
   endelse

   ; Write the data

   if scan eq 1 then begin
      printf, Unit, '#F ',file
      printf, Unit, '#E',strcompress(long(systime(1)))
      printf, Unit, '#D ',systime(0)
   endif 
   printf, Unit, ''
   printf, Unit, '#S',strcompress(scan),' ',name
   printf, Unit, '#D ',systime(0)
   if keyword_set(comm) then printf, Unit, transpose('#C ' + comm)
   printf, Unit, format='("#N ", I2)' ,ncol
   printf, Unit, format= '("#L",'+string(ncol)+'("  ",A))', labels
   printf, Unit, format='('+string(ncol)+'(G0.6," "))', data(columns, *)
   free_lun, Unit

   return, scan
end
;Dids PRO spec2hdf,filein,fileout,GROUP=group
;Dids ;+
;Dids ; NAME:
;Dids ;       SPEC2HDF
;Dids ;
;Dids ; PURPOSE:
;Dids ;       This procedure reads a SPEC file and creates an HDF file
;Dids ;	with its contents.
;Dids ;
;Dids ; CATEGORY:
;Dids ;       Filters.
;Dids ;
;Dids ; CALLING SEQUENCE:
;Dids ;       SPEC2HDF [, infile, outfile] 
;Dids ;
;Dids ; OPTIONAL INPUTS:
;Dids ;       infile - Name of the input file.
;Dids ;       outfile - Name of the output file.
;Dids ;	(if infile and/or outfile are not input, a dialog window 
;Dids ;	will appear to define them).
;Dids ;
;Dids ; KEYWORDS:
;Dids ;       GROUP: Set this keyword to the widget ID of an existing widget
;Dids ;               that serves as "group leader".
;Dids ;
;Dids ; OUTPUT:
;Dids ;
;Dids ; PROCEDURE:
;Dids ;	Uses SPECACCESS and HDF routines.
;Dids ;
;Dids ; EXAMPLE:
;Dids ;
;Dids ; MODIFICATION HISTORY:
;Dids ;       00-10-26  Written by M. Sanchez del Rio (srio@esrf.fr)
;Dids ;-
;Dids ;
;Dids ;=====================================================================
;Dids ;
;Dids COMMON spec2hdf_paths,pathIn,pathOut
;Dids 
;Dids On_Error,2
;Dids 
;Dids IF N_Elements(pathIn) EQ 0 THEN pathIn = ''
;Dids IF N_Elements(pathOut) EQ 0 THEN pathOut = ''
;Dids 
;Dids IF SDep(/w) NE 1 AND N_Params() LT 2 THEN Message,$
;Dids   'Usage: spec2hdf,filein,fileout'
;Dids 
;Dids IF N_Params() EQ 0 THEN BEGIN
;Dids   filein = Dialog_Pickfile(/READ,DIALOG_PARENT=group, $
;Dids      GET_PATH=pathin,PATH=pathIn)
;Dids   IF filein EQ '' THEN RETURN
;Dids   fileout=filein+'.hdf'
;Dids   fileout = Dialog_Pickfile(/WRITE,DIALOG_PARENT=group,FILE=fileout, $
;Dids     GET_PATH=pathOut,PATH=pathOut)
;Dids   IF fileout EQ '' THEN RETURN
;Dids ENDIF
;Dids IF N_Params() EQ 1 THEN BEGIN
;Dids   fileout = Dialog_Pickfile(/WRITE,DIALOG_PARENT=group,FILE=filein+'.hdf',$
;Dids     GET_PATH=pathOut,PATH=pathOut)
;Dids   IF fileout EQ '' THEN RETURN
;Dids ENDIF
;Dids 
;Dids Message,/Info,'Scanning file: '+filein
;Dids n=spec_access(h,filein)
;Dids hdffile=fileout
;Dids 
;Dids hdfhandle = HDF_SD_START(hdffile, /CREATE)
;Dids IF hdfhandle EQ 0 THEN message,'Error opening HDF file: '+hdffile
;Dids 
;Dids inormal = spec_scan(h,'-',/return_index)
;Dids imcdata = spec_scan(h,'-',/mcdata,/return_index)
;Dids 
;Dids ;
;Dids ; file header
;Dids ;
;Dids IF inormal[0] NE -1 THEN txt = spec_headers(h,1,/index,/all)  ELSE $
;Dids                          txt = spec_headers(h,1,/index,/all,/mcdata) 
;Dids 
;Dids igood = where(strmid(StrCompress(txt,/Remove_All),0,2) EQ '#S')
;Dids IF igood[0] NE -1 THEN BEGIN
;Dids   headerfile = txt[0:igood[0]-1]
;Dids   ihandle = HDF_SD_CREATE(hdfhandle,'File header',[0])
;Dids   FOR j=0L,N_ELEMENTS(headerfile)-1 DO BEGIN
;Dids         tmp = headerfile[j]
;Dids         HDF_SD_ATTRSET,ihandle,headerfile[j],headerfile[j]
;Dids   ENDFOR
;Dids ENDIF
;Dids 
;Dids ;
;Dids ; normal scans
;Dids ;
;Dids n = n_elements(inormal)
;Dids IF inormal[0] NE -1 THEN BEGIN
;Dids   Message,/Info,'Writing '+StrCompress(n,/Remove_All)+' normal scans...  '
;Dids   FOR i=0,n-1 DO BEGIN
;Dids     iscan = inormal[i]
;Dids     txt = spec_headers(h,iscan,/index,all=iall) 
;Dids     npoints = spec_points(h,iscan,/index)
;Dids     IF npoints GT 0 THEN BEGIN
;Dids       data = spec_data(h,iscan,/index)
;Dids       name = spec_headers(h,iscan,'S',/index)
;Dids       nx = (size(data))[1]
;Dids       ny = (size(data))[2]
;Dids       nn=[nx,ny]
;Dids       ihandle = HDF_SD_CREATE(hdfhandle,name,[nx,ny],/float)
;Dids       HDF_SD_ADDDATA,ihandle,data
;Dids       FOR j=0,n_elements(txt)-1 DO BEGIN
;Dids         HDF_SD_ATTRSET,ihandle,txt[j],txt[j]
;Dids       ENDFOR
;Dids     ENDIF
;Dids   ENDFOR
;Dids ENDIF
;Dids 
;Dids ;
;Dids ; mcdata scans
;Dids ;
;Dids n = n_elements(imcdata)
;Dids IF imcdata[0] NE -1 THEN BEGIN
;Dids   Message,/Info,'Writing '+StrCompress(n,/Remove_All)+' MCA scans...  '
;Dids   FOR i=0,n-1 DO BEGIN
;Dids     iscan = imcdata[i]
;Dids     txt = spec_headers(h,iscan,/index,all=iall) 
;Dids     npoints = spec_points(h,iscan,/index)
;Dids     IF npoints GT 0 THEN BEGIN
;Dids       data = spec_data(h,iscan,/index,/mcdata)
;Dids       name = spec_headers(h,iscan,'S',/index)
;Dids       nx = (size(data))[1]
;Dids       ny = (size(data))[2]
;Dids       nn=[nx,ny]
;Dids       ihandle = HDF_SD_CREATE(hdfhandle,name+' [MCDATA]',[ny,nx],/float)
;Dids       HDF_SD_ADDDATA,ihandle,data,/noreverse
;Dids       FOR j=0,n_elements(txt)-1 DO BEGIN
;Dids         HDF_SD_ATTRSET,ihandle,txt[j],txt[j]
;Dids       ENDFOR
;Dids     ENDIF
;Dids   ENDFOR
;Dids ENDIF
;Dids 
;Dids 
;Dids ; HDF_CLOSE,hdfhandle
;Dids HDF_SD_END,hdfhandle
;Dids Message,/Info,'File written to disk: '+fileout
;Dids 
;Dids END
; Version  3.00beta  10/10/1995
; European Synchrotron Radiation Facility (ESRF)
;______________________________________________________________________________
;+
; FILE: `spec_access.pro'
;
; OVERVIEW:
;       Set of IDL functions to read and write SPEC data files. These functions
;       provide information about the SPEC file (number of scans, scan list,
;       etc...) or about a particular scan (scan data, motor positions, header
;       information ...).
;       In order to speed up the access to the data, a file must be open by the
;       function SPEC_ACCESS() before any other operation is performed. This
;       function returns a handle to the file that has to be used in all
;       subsequent file access. The handle is a variable used to transfer
;       information internally between different functions and must not be
;       modified by the main program. A handle is associated to each individual
;       file and therefore several SPEC files can be accessed by managing
;       different handles.
;       All the functions require at least a file handle as parameter. If the
;       file handle has not been previously initialized, the functions call
;       internally SPEC_ACCESS() and prompt the user for a file name.
;       Most of the functions require also a scan identifier (see below). If 
;       the scan identifier is missing the functions call to XSPECSCAN that
;       is an utility procedure that displays the available scans in the data
;       file and lets the user select one.
;
; SCAN IDENTIFIERS
;       A scan identifier is an expression that designates a particular scan
;       in the data file. It must be a numeric or string scalar. Numeric values
;       can be expressed as strings by using their alphanumeric representation.
;       A scan identifier must be of one of the following three types:
;
;       Scan Number - A non-zero positive number that corresponds to the SPEC
;                     scan number included in the `#S' line in the data file.
;                     A mechanism has been provided to deal with files that
;                     include scans with repeated numbers: if a scan identifier
;                     is expressed as a decimal number SSSS.OO, it refers to
;                     the OOth occurrence of the scan SSSS in the data file.
;                     Special care must be taken when using this feature with
;                     number-valued expressions because the first two decimal
;                     places are always considered. For instance, the
;                     identifier 12.02 refers to the second occurrence of the
;                     scan 12, while 12.2 refers to the 20th occurrence of the
;                     same scan. This problem does not arise when string
;                     expressions are involved: the identifiers '12.02' and
;                     '12.2' refer both to the same scan in the data file.
;
;       Scan Index  - A non-zero integer number that indicates the position of
;                     the scan in the data file. Positive indexes correspond
;                     to the position of scan from the beginning of the file,
;                     the first scan in the file has an index 1. Negative
;                     indexes indicate the position of the scan by counting
;                     backwards from the end: -1 is the last scan in the file,
;                     -2 is the one before the last, and so on.
;                     When an index is used as a scan identifier, the INDEX
;                     keyword must be set in the function call in order to
;                     avoid ambiguities with scan numbers.
;
;       Descriptor  - One of the following words:
;                     'FIRST'  : refers to the first scan in the file.
;                     'LAST'   : refers to the last scan in the file.
;                     'CURRENT': refers to the scan that has been most
;                                recently accessed by any of the functions
;                                of this library.
;                     'NEXT'   : refers to the scan immediately after the
;                                `current' scan.
;       
;       
; FUNCTION LIST:
;       SPEC_ACCESS()  - Returns the number of scans in the file or a scan list.
;       SPEC_COLUMNS() - Returns the number of data columns in a given scan.
;       SPEC_DATA()    - Returns the data from a given scan.
;       SPEC_HEADERS() - Returns headers from a given scan.
;       SPEC_LABELS()  - Returns the column labels in a given scan.
;       SPEC_MOTORS()  - Returns the positions or names of the motors in a scan.
;       SPEC_NAME()    - Returns the name of a given scan.
;       SPEC_POINTS()  - Returns the number of data points in a given scan.
;       SPEC_SAVE()    - Saves data in a SPEC data file.
;       SPEC_SCAN()    - Returns the numbers or the indexes of a list of scans.
;
; MODIFICATION HISTORY:
;       1.00 March/95,  Written by P. Fajardo.
;                       based on 'spec_analyzer.pro' from M. Sanchez del Rio
;       2.00  June/95,  Added support for Multichannel Data (P. Fajardo).
;       2.01  5/07/95,  SPEC_DATA() modified to speed up data access. Default
;                       return data type is no longer string. New __reads
;                       procedure and __linesplit() function.
;                       Now the error handling routines return to the caller
;                       when an error is found (P. Fajardo).
;       2.02 21/08/95,  Fixed a bug in the handling of split lines. New CHANNELS
;                       and CALIBRATED keywords in SPEC_DATA() (P. Fajardo).
;       2.03 29/09/95,  New NO_EMPTY, MCDATA and FILENAME keywords in
;                       SPEC_ACCESS(). New NO_EMPTY, MCDATA and NO_RANGE
;                       keywords in SPEC_SCAN() (P. Fajardo).
;       3.00  --------
;       3.01 16/12/96,  Some elements of file handles changed from integer to
;                       long to cope with very long files.
;       3.02 01/10/97,  srio@esrf.fr places the function spec_access() at
;			the end of the file to allow idl to resolve all
;			names (for use in xop).
;______________________________________________________________________________
;-


;______________________________________________________________________________
;+
; NAME:
;       SPEC_ACCESS()
;
; PURPOSE:
;       This function returns the number of scans in a SPEC data file or
;       optionally the list of scans. The first time that it is called or if
;       the datafile is modified it reads the file and creates an internal
;       index to speed up the subsequent access to the data.
;       This function must be called before any other function of the library.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       Result = SPEC_ACCESS(Handle [, SPECfile])
;
; INPUTS:
;       Handle   - Variable used as a handle to the file. This variable is
;                  modified internally by the function and must be used in
;                  all subsequent function calls to access the data file.
;                  If the variable used is not a valid handle to the file in
;                  SPECfile or if the length of the file has changed, the
;                  internal index is rebuilt. 
;                  If the variable used is a valid handle that has been 
;                  initialised in a previous call to SPEC_ACCESS(), the
;                  function checks if the current size of the file is the
;                  same than in the previous call. If so, the internal index
;                  is not rebuilt. 
;
; OPTIONAL INPUTS:
;       SPECfile - String that contains the name of the data file. This
;                  parameter is only optional in the case that Handle is a
;                  valid SPEC file handle initialised in a previous call to
;                  SPEC_ACCESS().
;
;
; KEYWORDS:
;       LIST:   Set this keyword to produce a list of the scans in the file.
;               By default the list is a vector that contains all the numbers
;               of the scans in the file. This behaviour can be changed by
;               the COMPRESS and STRING keywords.
;
;       COMPRESS: Set this keyword to return the scan list in a compressed
;               format. In a compressed list the scans with consecutive numbers
;               are grouped in blocks. Each block is described by the numbers
;               of its first and last scan. If the STRING keyword is not set
;               the result is an array with two dimensions where the element
;               (0, i) is the number of the first scan in the group i, and the
;               element (1, i) is the number of the last scan in the group.
;               This keyword has no effect if the LIST keyword is not set.
;
;       STRING: Set this keyword to format the result as a character string
;               rather than as a number, vector or array. If the result is a
;               compressed list, the groups in the output string are separated
;               by commas (',') and the first and last scan number of each
;               group are separated by a minus sign ('-').
;
;       READALWAYS: Set this keyword to force the function to read the file
;               and build the internal index even in the case that Handle is a
;               valid handle to the same file and the size of the file has not
;               changed. If READALWAYS is set, the parameters SPECfile must
;               be specified.
;
;       NOREAD: Set this keyword to force the function not to read again the
;               file even in the case that its size has changed.
;
;       NO_EMPTY: Set this keyword to make the output value of the function
;               refer only to those scans that actually contain standard data
;               columns, excluding the empty ones.
;
;       MCDATA: Set this keyword to make the output value of the function
;               refer only to those scans that contain multichannel data.
;
;       FILENAME: Variable to be loaded with the name of the Spec data file.
;               This keyword is only useful when the function is called with
;               a previously initialized SPEC file handle and the file name
;               is not known.
;
; OUTPUT: 
;       If the LIST keyword is not set this function returns the total number
;       of scans in the SPEC data file. If the LIST keyword is set, it returns
;       a list of the scans.
;
; SIDE EFFECTS:
;       When the internal index is built the `current' scan is set to the
;       first scan in the file.
;
; RESTRICTIONS:
;       This function only works with ASCII files that follow the conventions
;       used by the program SPEC to generate data files.
;
; EXAMPLE:
;       To create an internal index of the SPEC data file 'data.run23' and
;       load the variable Nscans with the total number of scans in the file,
;       enter:
;
;       Nscans = SPEC_ACCESS(File1_handle, "data.run23")
;
;       The variable File1_handle should not be modified by the main program.
;       It may be used to access information from the same file by other
;       function calls.
;______________________________________________________________________________
;-
function spec_access, handle, file, LIST=list, STRING=str, COMPRESS =compress,$
                                    READALWAYS=rall,  NOREAD=noread, $
                                    FILENAME=file0, MCDATA=mca, NO_EMPTY=noemp
   catch, error_status
   if error_status ne 0 then begin
      catch, /cancel
      on_error, 2
      message, __cleanmsg(!err_string) ;, /traceback
   endif

; Check arguments
   if N_PARAMS() eq 0 then begin
      message, 'Incorrect number of arguments.'
   endif

;Dids
   if N_PARAMS() eq 1 then begin
      if not __isaspechandle(handle) then message, 'No file selected.' $
      else begin
         file = handle.specfile
      endelse
   endif 

   file0 = file

   if keyword_set(noread) then begin
      goto, result
   endif

; Open SPEC data file
;
; srio changed these lines *porting to Windows* 97/01/13
;   file=(findfile(file))(0)
;   if file eq '' then message, 'File not found'
   if (findfile(file))(0) eq '' then message, 'File not found'
   openr, Unit, file, /get_lun
   status = fstat(Unit)

   if not keyword_set(rall) and __isaspechandle(handle) then begin
      if handle.specfile eq file and handle.filesize eq status.size then begin
         goto, close
      endif 
   endif

   scancounter = 0
   scanlist = 'no scans'

   line = ''
   name = ''
   range = 0B
   linect=lonarr(4)
   n_colmax = 1                              
   n_motlinesmax = 0
   n_motperlinemax = 0
   maxnumber = 0

   s = {scan,                $
          scan_n: 0L,        $
          scan_ap: 0L,       $
          name: '',          $
          predatalines: 0L,  $
          datalines: 0L,     $
          postdatalines: 0L, $
          scanpt: 0L,        $
          labelpt: -1L,      $
          datapt: 0L,        $
          n_col: -1L,        $
          n_chan: -1L,       $
          n_datapoints: 0L,  $
          a_datapoints: 0L,  $
          headerpt: 0L,      $
          headerlines: 0L    $
       }
;
; Start a loop to identify all the scans in the file 
;
   linect(0) = -1
   while (not EOF(Unit)) do begin
      point_lun,-Unit,memory_pointer
      __readline,Unit,line
      linect(range)=linect(range)+1

      if strmid(line,0,1) eq '#' then begin
         if range eq 2 then range = 3

         case strmid(line,1,1) of
            'S': begin
               scan_n=-1
               aux = strmid(line, 2, strlen(line)-2)+' '
               reads, aux, scan_n, name       ; read scan no.
               if strmid(line,2,1) eq ' ' and scan_n gt 0 then begin
                  if range gt 0 then begin
                     __addscan, scanlist, s, linect, range, scancounter
                  endif

                  range = 1
                  linect([1, 2, 3]) = 0
                  if scan_n gt maxnumber then maxnumber = scan_n
                  s.scan_n = scan_n
                  s.name = strtrim(name, 2)
                  s.scanpt = memory_pointer
                  s.n_col = -1
                  s.n_datapoints = 0
                  s.a_datapoints = 0
                  s.datapt = 0L
                  s.labelpt=-1L
               end
            end 

            'N': begin
               ; get no. of data columns
               s.n_col=fix(strmid(line, 2, strlen(line)-2))      
               if s.n_col gt n_colmax then n_colmax = s.n_col
            end

            'L': begin
               s.labelpt=memory_pointer
            end

            'F': begin
               if range gt 0 then begin
                  __addscan, scanlist, s, linect, range, scancounter
                  range = 0
                  s.headerpt = memory_pointer
                  linect(0) = 0
               endif
            end

            'E': begin
               if range gt 0 then begin
                  __addscan, scanlist, s, linect, range, scancounter
                  range = 0
                  s.headerpt = memory_pointer
                  linect(0) = 0
               endif
            end

            'O': begin
               if range gt 0 then begin
                  __addscan, scanlist, s, linect, range, scancounter
                  range = 0
                  s.headerpt = memory_pointer
                  linect(0) = 0
               endif

               aux = __linecut(line)
               reads, aux, format='(2X,I)', aux        ;get no. of motor lines
               if aux + 1 gt n_motlinesmax then n_motlinesmax = aux + 1

               aux = n_elements(__linesplit(line, 2, 0, 0))
               if aux gt n_motperlinemax then n_motperlinemax = aux
            end

            '@': begin
               if strmid(line, 2, 5) eq 'CHANN' then begin
                  reads, line + ' -1', format='(7X,I)', aux
                  s.n_chan = aux
               end
            end

            else: 

         endcase
      endif else if line ne '' then begin
         if range eq 1 then begin
            s.datapt = memory_pointer
            range = 2
         endif else if range eq 3 then begin
            linect(2) = linect(2) + linect(3)
            linect(3) = 0
            range = 2
         endif

         if range eq 2 then begin
            if strmid(line, 0, 2) eq '@A' then begin
               s.a_datapoints = s.a_datapoints + 1
            endif else begin
               s.n_datapoints = s.n_datapoints + 1
            endelse
         endif
      endif
   endwhile

   if range gt 0 then begin
      linect(range)=linect(range)+1
      __addscan, scanlist, s, linect, range, scancounter
   endif

   index = intarr(maxnumber + 1)
   for i = 0, scancounter - 1 do begin
      aux = scanlist(i).scan_n
      index(aux) = index(aux) + 1
      scanlist(i).scan_ap = index(aux)
   endfor

   if n_motlinesmax eq 0 or n_motperlinemax eq 0 then begin
      n_motlinesmax = 1
      n_motperlinemax = 1
   endif 

   handle = {                   $
      SPECfile: file,           $  ;SPEC filename
      filesize: status.size,    $  ;filesize in bytes
      total: scancounter,       $  ;total no. of scans
      scan: scanlist,           $  ;individual scan information
                                $
      currscan: -1,             $  ;current scan 
      label: strarr(n_colmax),  $  ;column labels for currscan
                                $
      headerpt: -1L,            $  ;pointer to last loaded header 
      epoch: -1L,               $  ;epoch value in main header block
      nmotorlines: 0,           $  ;no. motor lines in the main header block
      motor_no: intarr(n_motlinesmax),                     $
                                $  ;motors per line in the main header block
      motornames: strarr(n_motlinesmax, n_motperlinemax)   $
                                $  ;motor names in the main header block
   }

   __specload, handle, 1, /INDEX

close:
   free_lun, Unit

result:
   if handle.total gt 0 then begin
      if keyword_set(mca) and keyword_set(noemp) then begin
         aux = where(handle.scan(*).a_datapoints gt 0 or    $
                     handle.scan(*).n_datapoints gt 0)
         if aux(0) ne -1 then fulllist = handle.scan(aux).scan_n
      endif else if keyword_set(mca) then begin
         aux = where(handle.scan(*).a_datapoints gt 0)
         if aux(0) ne -1 then fulllist = handle.scan(aux).scan_n
      endif else if keyword_set(noemp) then begin
         aux = where(handle.scan(*).n_datapoints gt 0)
         if aux(0) ne -1 then fulllist = handle.scan(aux).scan_n
      endif else begin
         fulllist = handle.scan(*).scan_n
      endelse
   endif
   totsize = n_elements(fulllist)

   if totsize eq 0 then  begin
      if keyword_set(str) then begin
         return, ''
      endif else begin
         return, 0
      endelse
   endif
   ncols = 1
   ngroups = 1
   if keyword_set(list) then begin
      if not keyword_set(compress) then begin
         list = fulllist
         ngroups = totsize
      endif else begin
         ncols = 2
         list = [fulllist(0), fulllist(0)]
         for i = 1L, totsize - 1 do begin
            n = fulllist(i)
            if n eq list(1, ngroups - 1) + 1 then begin
               list(1, ngroups - 1) = n
            endif else begin
               list = [[list], [n, n]]
               ngroups = ngroups + 1
            endelse
         endfor
      endelse
   endif else begin
      list = totsize
   endelse

   
   if not keyword_set(str) then begin
      return, list
   endif else begin
      str=''
      for i = 0, ngroups - 1 do begin 
         if ncols eq 1 then begin
            str0 = strtrim(string(list(i)), 2)
         endif else if list(0, i) eq list(1, i) then begin
            str0 = strtrim(string(list(0, i)), 2)
         endif else begin
            str0 = strtrim(string(list(0, i)), 2) + '-'  $
                 + strtrim(string(list(1, i)), 2)
         endelse

         if i eq 0 then str = str0 else str = str + ', ' + str0
      endfor
      return, str 
   endelse
end

;*******************************************************************************************
;*******************************************************************************************
;*******************************************************************************************

pro spec_wplot,event, runtxt
;** **********
;**
common c_rdspec, c_hand, c_fil, c_siz, c_inor, c_imcd, c_ot, c_list, c_lz, c_fifi, c_bas

runtxt=''
index =event.index
txt   = c_list(index)
i     = strpos(txt,'.')
if (i gt 0) and (i lt 8) then begin    ;runtxt=c_fifi+'{'+strtrim(strmid(txt,0,i),2)+'}'
	r1 = strtrim(string(c_hand.scan(index).scan_n ),2)
	r2 = strtrim(string(c_hand.scan(index).scan_ap),2)
	runtxt=c_fifi+'{'+r1+'.'+r2+'}'
endif
if n_elements(c_list) ne c_lz then SPEC_WLIST
end

pro spec_wlist,lbase
;** **********
;**
common c_rdspec, c_hand, c_fil, c_siz, c_inor, c_imcd, c_ot, c_list, c_lz, c_fifi, c_bas

c_lz =n_elements(c_list)
if n_elements(lbase) eq 1 then $
     c_bas=widget_list(lbase,xsize=20,ysize=c_lz,value=c_list,uvalue=[-88,569,0,-1,0]) $
else if c_bas gt 0 then widget_control,bad_id=ii,c_bas,set_value=c_list
end

function read_spec, INST , PATH , FILENAME , STATUS , DATP
;******* *********
;**
;**	Standard call for a data-read function interfacing LAMP.

common c_rdspec, c_hand, c_fil, c_siz, c_inor, c_imcd, c_ot, c_list, c_lz, c_fifi, c_bas

if n_elements(INST) eq 0 then return,1

STATUS=11
DATA  =0

CATCH,stat & if stat ne 0 then begin CATCH,/cancel & print,!err_string & return,DATA & endif

;Check for scan number
;---------------------
FileN=FILENAME(0)
if n_elements(FILENAME) gt 1 then immg=FILENAME(1) else immg=1
ac=0
i =strpos(FileN,'{')                     & if i lt 0 then i=strpos(FileN,'[')
if i gt 1 then begin j=strpos(FileN,'}') & if j lt 0 then j=strpos(FileN,']')
                     ac=1
                     if j gt i+1  then immg=strmid(FileN,i+1,j-i-1)
		     FileN=strmid(FileN,0,i) & endif
c_fifi=FileN
FileN =PATH+FileN
img= 1 & imm=1   & simg = str_sep(string(immg),'.')
ON_IOERROR,mismg &  img = long(simg(0))>1
if n_elements(simg) eq 2 then imm=long(simg(1))>1
mismg:
imgs=strtrim(string(img),2)+'.'+strtrim(string(imm),2)
if not ac then FILENAME(0)=FILENAME(0)+'{'+imgs+'}'

;Check for file
;--------------
if n_elements(c_fil) eq 0 then begin c_fil='' & c_bas=0L & endif
if c_fil eq FileN         then new=0 else begin new=1 & c_fil=FileN & endelse

ON_IOERROR ,misfil
OPENR, Unit, FileN, /get_lun & S=fstat(Unit) & free_lun,unit
STATUS=13

if not new then if c_siz ne S.size then new=1

;Initialize Handel
;-----------------
if new  then begin print,'Initialising input file ....'
   n=spec_access(c_hand,FileN)
   if n le 0 then  return,DATA
   c_inor = spec_scan(c_hand,'-',/no_empty)
   c_imcd = spec_scan(c_hand,'-',/mcdata)
   c_fil  = FileN
   c_siz  = S.size
   HEAD   = spec_headers(c_hand,1,/ALL,/INDEX)
   F='' & D='' & C=''
   for i=n_elements(HEAD)-1,0,-1 do begin HD=strcompress(HEAD(i))
	a = strmid(HEAD(i),0,2)
	if a eq '#F' then F=' Origin:...'+strmid(HD,(strlen(HD)-20)>3,20)
	if a eq '#D' then D=' Start:'    +strmid(HD,3,25)
	if a eq '#C' then C=' Sample:'   +strmid(HD,3,50)
   endfor
   c_ot   = C+D+F

   inor   = spec_scan(c_hand,'-',/return_index)-1
   imcd   = spec_scan(c_hand,'-',/return_index,/mcdata)-1
   c_list = '<none>'
   if  inor(0) ge 0 then c_list= strtrim(string(c_hand.scan(inor).scan_n),2)+'. '   + $
                                 c_hand.scan(inor).name
   if  imcd(0) ge 0 then begin
    if inor(0) ge 0 then c_list=[c_list,'  ----','MCdata-> '+strtrim(string(c_hand.scan(imcd).scan_n),2)+' '+ $
                                 c_hand.scan(imcd).name]
    if inor(0) lt 0 then c_list= strtrim(string(c_hand.scan(imcd)),2)+'. MC '+ $
                                 c_hand.scan(imcd).name
   endif
   c_lz =0
   P_MAC_EVENT,0,[-88,570,0,-1]
   if n_elements(c_list) ne c_lz then SPEC_WLIST
endif
STATUS=9

;Check if scan number is there
;-----------------------------
mcd=0 & nor=0
idx=where(c_inor eq img) & if idx(0) ge 0 then nor=1
idx=where(c_imcd eq img) & if idx(0) ge 0 then mcd=1
if mcd+nor eq 0 then return,DATA
STATUS=14

;Prepare all variables
;---------------------
P_TXT=spec_headers(c_hand,imgs)+'  '
P_TXT=[P_TXT,FileN+'{'+imgs+'} points--> in PVi variable of Lamp  ']
P    =intarr(n_elements(P_TXT))
PV=0
X =0 & Y =img & Z =img
E =0
N =0
WT='' & XT='' & YT='' & ZT='' & OT=c_ot

T=0. & M=0. & Q='' & Tp=''
For i=n_elements(P_TXT)-1,0,-1 do begin
	a = strmid(P_TXT(i),0,2)
	if a eq '#S' then WT=strmid(strcompress(P_TXT(i)),3,100)
	if a eq '#Q' then Q =' (hkl:' +strmid(strcompress(P_TXT(i)),3,50)+')'
	if a eq '#X' then Tp=' Temp:'+strmid(strcompress(P_TXT(i)),3,50)
	if a eq '#T' then reads,strmid(P_TXT(i),2,20), T
	if a eq '#M' then reads,strmid(P_TXT(i),2,20), M
endfor
WT=WT+Tp     ;+Q
N =[[M],[T]]

;Read the data
;-------------
STATUS=0
if nor then begin
		PV   =spec_data(c_hand,imgs,LABELS=LABL)
		
		if not mcd then begin
		 YT   =LABL(n_elements(LABL)-1)
		 XT   =LABL(0)
		 sz   =SIZE(PV)
		 if sz(0) eq 2 then begin
		   DATA=reform(PV(sz(1)-1,*))
		   MONI=reform(PV(sz(1)-2,*))
		   X  =0 & ok=-1
		   for i=0,sz(1)-3 do begin
			if ok lt 0 then if PV(i,0) ne PV(i,sz(2)-1) then begin
				X=reform(PV(i,*)) & ok=i & endif
		   endfor
		   if ok ge 0 then XT=LABL(ok)
		   E=sqrt(DATA)
		   if N(0,1) gt 0 then begin N=fltarr(sz(2),2) & N(*,0)=MONI & N(*,1)=T
		   endif else $
		   if N(0,0) gt 0 then begin N=fltarr(sz(2),2) & N(*,0)=M & N(*,1)=MONI & endif
		 endif else begin
		   DATA=PV
		 endelse
		endif
endif
if mcd then begin
		DATA =spec_data(c_hand,imgs,/MCDATA,CHANNELS=X,CALIBRATED=calib)
		sz=SIZE(DATA)
		XT='Channels'
		Nt=N
		N =fltarr(sz(1),3) & N(*,0)=M & N(*,1)=T & N(*,2)=calib
		Y =indgen(sz(2))
		if nor then begin
		 sz   =SIZE(PV)
		 YT   =LABL(0)
		 if sz(0) eq 2 then begin
		   ok=-1
		   for i=0,sz(1)-2 do begin
			if ok lt 0 then if PV(i,0) ne PV(i,sz(2)-1) then begin
				Y=reform(PV(i,*)) & ok=i & endif
		   endfor
		   if ok ge 0 then YT=LABL(ok)
		 endif
		endif
endif

;Return every thing
;------------------
DATP={X:      x   , Y:y , Z:z , E:e , N:n   ,$
      W_TIT:  wt  , X_TIT:xt  , Y_TIT:yt    ,$
      Z_TIT:  zt  , OTHER_TIT:ot            ,$
      P:      p   ,    $
      PAR_TXT:p_txt,   $
      PV:     pv       }


misfil: RETURN, DATA
;       ************
 END
