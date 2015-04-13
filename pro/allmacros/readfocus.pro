;------------ initialise NeXus routines first
@nidl.pro
function readfocus, INST , PATH , FILENAME , STATUS , DATP
;******* *************  
;**
;**	Reads a NeXus FOCUS data file into  LAMP.
		  
;**	Return of the function
;**	 DATA     is an array of any dimensions and type containing the data values (spectra).

;**	Input  parameters:
;**	 INST(0)  is the file_type  (or instrument_name ) (string defined in customize tables).
;**	 INST(1)  is the file_group (or instrument_group) (string defined in customize tables).
;**	 PATH     is the full path where to find the data (string defined in customize tables).
;**	 FILENAME is the name of the data file.
;**		  if FILENAME(1) exists,  this is the requested image number in the file.

;**	Output parameters:
;**	 STATUS   is the returned error code you can choose from the following list:
;**		  0 =' Successfull read'	
;**		  1 =' Client/server on local node not established'
;**		  2 =' Client/server on router node not established'
;**		  3 =' The local  node cannot access the server node'
;**		  4 =' The router node cannot access the server node'
;**		  5 =' VME memory read error'
;**		  7 =' Sequence error in data transfer'
;**		  9 =' Parameter error'
;**		  10=' Router is busy with other transfer'
;**		  11=' Cant open the file or file not found'
;**		  13=' Data file incomplete'
;**		  14=' Bad instrument data definition'
;**		  24=' Cant read the file'.
;**		  
;**	 DATP     is a structure defined as follow: (all tags are OPTIONAL)
;**		  DATP.X        = vector of x coordinates.
;**		  DATP.Y        = vector of y coordinates.
;**		  DATP.Z        = vector of z coordinates.
;**		  DATP.W_TIT    =   main title
;**		  DATP.X_TIT    = x axis title
;**		  DATP.Y_TIT    = y axis title
;**		  DATP.Z_TIT    = z axis title
;**		  DATP.OTHER_TIT=    sub title
;**		  DATP.N        = monitors
;**		  DATP.P        = vector of parameter values up to 31
;**		  DATP.PAR_TXT  = string array of text associated to DATP.P (same size)
;**		  DATP.PV       = an array of any dimensions containing other parameter values
;**		  DATP.E        = the errors associated to DATA (same size)
;**		  DATP.TIME     = string date of the experiment.
;**
;** Mark Koennecke, August 1998

 DATA  =0
 STATUS=11
 CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN, DATA & endif

   if strpos(filename,'.') lt 0 then begin
 ;-------- build filename
 ;----- find year, as last element of path string
   res = str_sep(path,'/')
   i = N_ELEMENTS(res)
   year = res(i-2)
   num = String(filename,Format= '(I5.5)') 
   fname = inst(0)+num+STRING(year)+'.hdf' 
   fpath=path + fname

   endif else begin fname=filename & fpath=path + fname & endelse

   print, 'Lamp opening : ' + fpath

;------ open the file
   test = nxopen(fpath,'read',handle)
   if test NE 1 THEN BEGIN
      status = 11
      return, 0
   END
;-------- get some global attributes
   test1 = nxgetattr(handle,'file_time',ftime,typ)
   test2 = nxgetattr(handle,'file_name',foriname, typ)
   test3 = nxgetattr(handle,'owner',user,typ)
   IF (test1 NE 1) OR (test2 NE 1) OR (test3 NE 1) THEN BEGIN
       status = 24
       return,0
   END 
;------ open data vGroup and get counts, two_theta
   test = nxopengroup(handle,'entry1','NXentry')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxopengroup(handle,'bank1','NXdata')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxopendata(handle,'counts')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,cnts)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
   test = nxopendata(handle,'theta')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,detnum)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
   test = nxopendata(handle,'time_binning')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,tof)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
;------ leave this vGroup
   test=nxclosegroup(handle)
;------ get title Information
   test = nxopendata(handle,'title')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,title)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
  ; -------- get sample information
   test = nxopengroup(handle,'sample','NXsample')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxopendata(handle,'name')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,sample)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
   test = nxopendata(handle,'distance')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,sadist)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
   test = nxopendata(handle,'temperature_mean')
   IF test NE 1 THEN BEGIN
      ; ignore
       temp = -999.
   END ELSE BEGIN
     test = nxgetdata(handle,temp)
     IF test NE 1 THEN BEGIN
        status = 24
        return,0
      END
     test = nxclosedata(handle)
   END
;----- close sample groups
   test=nxclosegroup(handle)
; ------- enter instrument group
   test = nxopengroup(handle,'FOCUS','NXinstrument')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
; ------- enter monochromator group
   test = nxopengroup(handle,'monochromator','NXmonochromator')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
;------ get lambda
   test = nxopendata(handle,'lambda')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,lambda)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
   test = nxclosegroup(handle)
;------- get monitor
   test = nxopengroup(handle,'counter','NXmonitor')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
;------ get monitor
   test = nxopendata(handle,'monitor')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,monitor)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
   test=nxclosegroup(handle) 
;------- fermi-chopper
   test = nxopengroup(handle,'fermi_chopper','NXchopper')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
;------ get speed
   test = nxopendata(handle,'rotation_speed')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,fermispeed)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
;-------- get phase
   test = nxopendata(handle,'phase')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,fermiphase)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
;------ fermi-distance
   test = nxopendata(handle,'distance')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,fermidist)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
   test=nxclosegroup(handle) 
;-------- disk chopper
   test = nxopengroup(handle,'disk_chopper','NXchopper')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
;------ get speed
   test = nxopendata(handle,'rotation_speed')
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxgetdata(handle,diskspeed)
   IF test NE 1 THEN BEGIN
      status = 24
      return,0
   END
   test = nxclosedata(handle)
   test = nxclosegroup(handle)
;------ close the file
  test = nxclosegroup(handle)
  test = nxclose(handle) 
;--------- find the elastic line
  len = n_elements(tof)
  maxi = -999999
  pos =  0
  FOR I = 0, (len-1) DO BEGIN
     IF (cnts(I,75) GT maxi) THEN BEGIN
         maxi = cnts(I,75)
         pos = I
     END
  END
;-------- build parameter info
   pp = fltarr(31)
   pp(0) = temp
   pp(1) = lambda
   pp(2) = monitor
   pp(3) = fermispeed
   pp(4) = diskspeed
   pp(5) = fermiphase
   pp(6) = fermidist
   pp(7) = sadist
   pp(8) = 2500.0
   pp(21) = pp(1)
   pp(27) = 2.9997
   pp(18) = tof(2) - tof(1)
   pp(9) = pos
   ppi =  strarr(31)
   ppi(0) ='mean_temperature'
   ppi(1) = 'Lambda'
   ppi(2) = 'Monitor_Counts'
   ppi(3) = 'fermi_chopper_speed'
   ppi(4) = 'disk_chopper speed'
   ppi(5) = 'chopper_phase'
   ppi(6) = 'fermi_chopper_distance'
   ppi(7) = 'sample_distance'
   ppi(8) = 'detector_distance'
   ppi(9) = 'elastic_peak'
   ppi(18) = 'channel_width'
   ppi(21) = 'Lamda'
   ppi(27) = 'flight_path'

   ot = sample + ',' + string(foriname) 
   ott = ot + ',' + string(user)

;------ fill in structure				xv       =  FINDGEN(40)*2 +15			;Normaly in data file
      STATUS=0							;Status is ok
;     ********
      DATP={X:      detnum,    $
            Y:      tof,       $					;Pass those variables which were 
            W_TIT:  title,    $ 					;read-in into the DATP structure
            OTHER_TIT: ott, $
	    X_TIT:  'time-of-flight',    $
            Y_TIT:  'two theta',$
            Z_TIT:  'Counts', $                   
            TIME: ftime , $
	    P:      pp,   $
	    PAR_TXT: ppi }
;     **********************  					
	    
 
 RETURN, cnts							;Return the data values
;***********

 END

