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