; MONO.PRO: Monochomatic data analysis for D17
;---------------------------------------------
; (Stephen Brown, 16/09/2001)

; Notes: This programme calculates the reflectivity against q for monochromatic
; data. This is done by dividing the reflected and direct beam intensities for
; various combinations of polariser/analyser (up to 4 in total). Because 
; monochromatic data usually consists of a large number of numors, once the
; data is loaded it is stored in heap structures to allow the analysis to 
; be repeated using different parameters. Most of the analysis is accomplished 
; using "fpeak" (which finds intensity of a peak) and "analysis" (which does
; everything else). 

; Most common blocks in the programme are used to store the widget id's
; for the user interface. The important common blocks (with regards to calcs.)
; are "COMMON detector", which contains the "useful area of detector" (xmin,xmax)
; and an array of 4 elements pointing to the approximate position of the reflected
; beam for each of the 4 possible polariser/analyser combinations; and 
; "COMMON loaded_data" which contains pointers to the loaded data. 

;===============================================================================
; Function to get an integer. numstring is the string containing the integer.
; errstr should containt the a string with the name of the field that is being
; entered (e.g 'Direct Beam, 1st Numor') this is used to display an error
; message if something goes wrong (in which case -1 is returned).
; The key word MAX_VALUE allows a maximum value to be specified. If not then
; the max value is the largest positive integer that can be stored in a signed
; long integer. The keyword ALLOW_EMPTY allows the field to be empty. If it
; is empty then 0 is returned rather than the ususal -1 (error). The keyword
; QUIET stops the printing out of error messages. -1 is still returned to 
; signal error, though. This keyword is used to disable the error messages
; that are used when getting integers from fields in the main window so that
; this routine may be used to get integers from other windows.
;===============================================================================
FUNCTION getint,numstring,errstr,MAX_VALUE=maxval,ALLOW_EMPTY=allow,QUIET=qu

; If the max value is not set then set to largest +ve integer that can be
; stored in a signed long integer i.e 2^31-1 
	IF NOT KEYWORD_SET(maxval) THEN maxval=LONG(2)^LONG(31)-LONG(1)
; If the ALLOW_EMPTY keyword is set then the minimum value is zero
; (default for empty field) otherwise the default is 1
	IF KEYWORD_SET(allow) THEN minval=0 ELSE minval=1

	temp=STRCOMPRESS(numstring,/REMOVE_ALL)
; check for empty field
	IF temp EQ '' THEN BEGIN
; Only print out error if field is not allowed to be empty
		IF NOT KEYWORD_SET(allow) THEN BEGIN
; Only print out error message if QUIET keyword isn't set
			IF NOT KEYWORD_SET(qu) THEN $
				msgwin,errstr+': Field Empty! Field must be an integer [1,'+$
				STRTRIM(STRING(maxval),1)+'].',/ERROR
			RETURN,-1
; otherwise return 0 (String empty)
		ENDIF ELSE RETURN,0
	END
; convert to byte array
	temp=BYTE(temp)

; check for illegal caharcters and return error if necessary
	FOR i=0,N_ELEMENTS(temp)-1 DO BEGIN
		IF (temp[i] LT 48B) OR (temp[i] GT 57B) THEN BEGIN
; Display window and signal error on console.
; Only print out error message if QUIET keyword isn't set
			IF NOT KEYWORD_SET(qu) THEN $
				msgwin,errstr+': Illegal character "'+$
				STRING(temp[i])+'" '+' Field must be an integer [1,'+$
				STRTRIM(STRING(maxval),1)+'].',/ERROR
			RETURN,-1
		END
	END

; If there are no illegal characters then convert to an integer. Check size:
	num=LONG(STRCOMPRESS(numstring,/REMOVE_ALL))
	IF (num GT maxval) OR (num LT minval) THEN BEGIN
; Only print out error message if QUIET keyword isn't set
		IF NOT KEYWORD_SET(qu) THEN $
			msgwin,errstr+': Value '+numstring+' out of range. '+$
			' Must be an integer [1,'+STRTRIM(STRING(maxval),1)+'].'$
			,/ERROR
		RETURN,-1
	END
; Everything's okay so just convert the string to an integer
	RETURN,num
END

;===============================================================================
; getfloat. Simple function to return a  float from a string. Minimum 
; amount of error detection is done, i.e it only signals an error if the 
; function FLOAT(...) fails. This is done to stop a silly IDL error message
; coming up on the console making the programme look unprofessional as if it had 
; been written by ILL stagiare. Returns NaN (IEEE 'Not a number') on conversion
; error. Keywords MIN_VALUE and MAX_VALUE specify the minimum and maximum 
; respectively.
;===============================================================================
FUNCTION getfloat,str,MIN_VALUE=minval,MAX_VALUE=maxval

	ON_IOERROR,bad_float

; convert
	number=FLOAT(str)

; If we get here then everything has converted, but is the range correct?
; check this only if the relevant keyword is specified
	IF KEYWORD_SET(maxval) THEN $ 
		IF (number GT maxval) THEN RETURN,!VALUES.F_NAN
	IF KEYWORD_SET(minval) THEN $
		IF (number LT minval) THEN RETURN,!VALUES.F_NAN

	RETURN,number

; If number <=0.0 or IO_ERROR then return NaN
bad_float:
	RETURN,!VALUES.F_NAN
END


;===============================================================================
; Procedure to display a message (msgstr) window with a title (msgtit) supplied
; as an optional keyword.
; Keyword ERROR causes msgwin to print error message (title set to 'Error!'
; and error message printed on console.)
; The keyword NOSENSTIZE stops changes in sensitisation of the main window
; useful if msgwin is called from a child of the main window (such as the
; (modal) direct beam attenuator-correction window. Otherwise msgwin performs
; necessary changes in sensitisation of main window.
;===============================================================================
PRO msgwin,msgstr,TITLE=msgtit,ERROR=err, NOSENSITIZE=noneed

; Common block for base id
COMMON baseid,mono_base,file_base,anal_base

; common block for message okay button
COMMON msgok,butt_msgok

; Common block containing widget id of status label
COMMON status,loadedflag,label_status

; If no title is supplied just set it to default
	IF NOT KEYWORD_SET(msgtit) THEN msgtit='Message'
; If error keyword is supplied then print error message to console and set
; appropriate window title
	IF KEYWORD_SET(err) THEN BEGIN
		PRINT,'****************************'
		PRINT,'Error! Calculations aborted!'
		PRINT,'****************************'
		msgtit = 'Error!'
; set status according to whether data is loaded or not
		IF loadedflag EQ 0 THEN WIDGET_CONTROL,label_status,$
			SET_VALUE='Last operation caused an error. No data is loaded.'$
		ELSE WIDGET_CONTROL,label_status,$
			SET_VALUE='Last operation caused an error. Data is loaded.'
	END

; Window with msgtit TITLE and msgstr message. OK button below
	msg_base = WIDGET_BASE(GROUP_LEADER=mono_base,TITLE=msgtit,/MODAL,/COLUMN)

; work out dimensions of box
	xdim=35
	ydim=FIX(STRLEN(msgstr)/xdim)+1
; make sure its not greater than 30 lines (if it is user can scroll)
	IF ydim GT 30 THEN ydim=30
; display text
	dummy = WIDGET_TEXT(msg_base,VALUE=msgstr,/WRAP,XSIZE=xdim,YSIZE=ydim,/SCROLL)
	butt_msgok = WIDGET_BUTTON(msg_base,VALUE='  OK  ')

; resensitise bases if the NOSENSITIZE keyword isn't set
	IF NOT KEYWORD_SET(noneed) THEN BEGIN
		WIDGET_CONTROL,anal_base,SENSITIVE=1
; only resensitise file base if no data is loaded
		IF NOT loadedflag THEN 	WIDGET_CONTROL,file_base,SENSITIVE=1
	END

	WIDGET_CONTROL,msg_base,/REALIZE
	XMANAGER,'msgwin',msg_base
END

;===============================================================================
; Event handler for message window. Destroys window if ok is pressed.
;===============================================================================
PRO msgwin_event,event

; common block for message base id
COMMON msgok,butt_msgok
;If OK button is pressed destroy widget
	IF event.id EQ butt_msgok THEN WIDGET_CONTROL,event.top,/DESTROY
	
END

;===============================================================================
; Reads a data numor 'num' in directory 'path'. The data is stored in a heap
; stucture which is allocated at the end. The pointer to the struct. is 
; returned in dataptr. The structure contains not only the (1d) data, but 
; also the instrument parameters. 2D data is summed in the y-direction.
;===============================================================================
FUNCTION data_readm,path,num,dataptr

;converts num(int) to 6 digit string with trailing zeroes
;make six digit string of 0's
	name='000000'
;convert num to string and trim all blank spaces
	tempnum=STRTRIM(STRING(num),2)
;put these digits in occupying the last strlen(tempnum) places of name
	STRPUT,name,tempnum,6-STRLEN(tempnum)
; include path in name
	name=path+name

	CLOSE,3
; open file (handle=3) checking for open errors
	OPENR,3,name,ERROR=err
	CATCH,Error_status

	IF (err NE 0) OR (Error_status NE 0) THEN BEGIN
			IF err NE 0 THEN msgwin,'Error opening '+name+' (Check numor and data directory)',/ERROR $
			ELSE msgwin,'"FUNCTION data_readm" : Caught general error in reading file '$
				+name+' (Corrupt numor?)' ,/ERROR
		RETURN,0
	END

	PRINT,'		opened: ',name

; floating point arrays of 128 and 256 elements
	par1 = FLTARR(128)
	par2 = FLTARR(256)

; string array of 34 elements
	txt=SINDGEN(34)
; set all 34 elements to XXX....
	txt[*]='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
; this is used to read individual spacer lines
	txt1=txt[1]

; read in 34 lines of text
	READF,3,txt

; read 128 floats (2nd block of numbers in file, 1st block is ignored by code above)
	READF,3,par1
; read the two line spacer
	READF,3,txt1
	READF,3,txt1
;read 256 numbers 
	READF,3,par2

;output useful information
;print,'x1= ',par1(97),' x2= ',par1(98),' y1= ',par1(99),' y2= ',par1(100)
	nx=par1(101)

;print,'nx= ',par1(101),' ny= ',par1(102)

;print,'run time= ',runtime,' s',' det= ',par2(15),'dan = ',par2(16)

;dan is the detector angle and dett is the sample-detector distance

; Now we can read the data -first read 3 spacer lines
	READF,3,txt1
	READF,3,txt1
	READF,3,txt1

;read total number pixels in detector
	READF,3,tot

; Array sizes. In monochromatic mode tsize must equal 1. 
	tsize=LONG(par1(94))
; All references to tsize are 1 following this check:
	IF tsize NE 1 THEN BEGIN
		msgwin,'Numor '+STRCOMPRESS(STRING(num),/REMOVE_ALL)+$
		' has multiple time channels! Time of flight numors cannot be analysed by this programme',/ERROR
		RETURN,0
	END

; x and y sizes, and dimensions
	xsize=LONG(par1(98)-par1(97)+1)
	ysize=LONG(par1(100)-par1(99)+1)
	dsize=LONG(xsize*ysize)

;print,'tsize= ',tsize,' detector size= ',dsize
;print,tot
;print,'xsize= ',xsize,' ysize= ',ysize,' tot1= ',dsize*tsize,' tot2= ',tot

;signal error if total number of pixels isn't equal to xsize*ysize
; note that the monitor is an extra tsize (=1 for monochromatic) pixels at 
; the end of the data
	IF (tot NE (dsize+LONG(1))) THEN BEGIN
		msgwin,'Error in data array dimensions in numor '+STRCOMPRESS(STRING(num),/REMOVE_ALL)+$
			'. (xsize*ysize not equal to number of pixels).',/ERROR
		RETURN,0
	END


;set up an array of long integers with specified size
	det=LONARR(ysize,xsize)
; set up monitor arrray. tsize=1 for monochromatic mode
	mon=LONARR(tsize)

;read detector data and monitor data from file
	READF,3,det,mon

; convert data to floating point in w1
	w1=FLTARR(xsize)
	IF ysize GT 1 THEN w1[*]=FLOAT(TOTAL(det[*,*],1)) ELSE w1[*]=FLOAT(det[*])

	CLOSE,3
; print total monitor counts and monitor counts per unit time
;print,'total counts in detector = ',total(w1),' (',total(w1)/runtime,')'

; Monitor is just a single pixel in monochromatic mode. Convert to float

	moni=FLOAT(mon[0])

; ATTENUATOR: There are three attenuators of different thickness. These are either
; in (1) or out of (0) the beam. The "attenuator number" is formed as the binary
; of these three 'bits' (e.g. 000 = no attenuators, 010 = 'middle' attenuator in
; et.c.)

	attnum = FIX(par2[101])*4 + FIX(par2[100])*2 + FIX(par2[99])

; stucture to be passed back containing the numor data.
	thedata = {datstruct,detdata:w1,monitor:moni,nx:par1[101],$
		runtime:par1[2]/10.,dan:par2[16],dett:par2[15],tsize:par1[94],$
		xsize:par1[98]-par1[97]+1,ysize:par1[100]-par1[99]+1,$
		os:par2[103],att:attnum,s2:par2[93],s3:par2[95],san:par2[2]}

	dataptr = PTR_NEW(thedata)

	CATCH,/CANCEL
	RETURN,1

END




;===============================================================================
; FUNCTION fpeak: Finds the peak of a 1d array 'arr' using a centre of mass 
; calculation over a width 'fgwidth' around the peak. Errors aree passed
; through the array errarr and propagated through the calculation. Background
; may be subtracted by specifying the vectors 'bgleft' and 'bgright' (which
; have 2 elements [0]=width of LHS/RHS background, [1]= shift). If 'fitbg'
; is 0 (false) then the background is averaged, otherwise it is fitted to a 
; straight line.
; Keyword PEAK_NEAR tells where to do the c.o.m calculation. If not specified
; then the calculation is done about the maximum pixel.
;===============================================================================
FUNCTION fpeak,arr,errarr,fgwidth,bgleft,bgright,fitbg,PEAK_NEAR=near

; Common block containing useful area of detector and approx whereabouts of 
; specular peak (for each polarisation, i.e. up to 4)
COMMON detector,xmin,xmax,refpmean

; If a pixel near the peak is specified then search around this. Otherwise
; search for the maximum.
	IF KEYWORD_SET(near) THEN centre=near ELSE maxval = MAX(arr,centre)

; set sum ranges
	range_low = centre-ROUND(FLOAT(fgwidth)/2.0)
	range_high = centre+ROUND(FLOAT(fgwidth)/2.0)

; Get detector ranges:
; If xmax is zero(=default) then set max_x to the size of the array, otherwise
; set it to the value of xmax
	IF xmax EQ 0 THEN max_x=N_ELEMENTS(arr)-1 ELSE max_x=xmax
; The default of xmin is zero so this does not need to be set
	min_x=xmin

; return with error if out of range when looking for COM
	IF (range_low LT min_x) OR (range_high GT max_x) THEN $
											RETURN,[-1.0,-1.0,-1.0]

; numerator and denom. of Centre of Mass sum
	num = 0.0
	denom = 0.0

; COM calculation
	FOR i=0,range_high-range_low DO BEGIN
; sum up elements in numerator and denominator
		num=num+FLOAT(i)*arr[range_low+i]
		denom=denom+arr[range_low+i]
	ENDFOR

; calc. com
	com = num/denom + range_low

;foreground range: f1:f2
	f1 = ROUND(com - FLOAT(fgwidth-1)/2.0)
	f2 = ROUND(com + FLOAT(fgwidth-1)/2.0)

; return with error if left or right edges are out of detector (i.e. left 
; edge of left backgrond and right edge for right bg-see bg calc. below)
	IF (f1-1-bgleft[0]-bgleft[1] LT min_x) OR $
		(f2+1+bgright[0]+bgright[1] GT max_x) THEN $
											RETURN,[-1.0,-1.0,com]

; create a new array to contain the background corrected data and its error
	newarr = FLTARR(f2-f1+1)
	newerrarr = FLTARR(f2-f1+1)

; An array to store the background to be subtracted for the data over the foreground 
; range [f1:f2]
	background = FLTARR(f2-f1+1)

;*************************************************
; Background subtraction:
;-------------------------------------------------
; If no background to subtract then set all elements of 'background' to 0
	IF (bgleft[0] EQ 0) AND (bgright[0] EQ 0) THEN background[*]=0 $
	ELSE BEGIN
;-------------------------------------------------
; Average if 'fitbg' is false (0)
		IF fitbg EQ 0 THEN BEGIN
; Use RHS only...
			IF (bgleft[0] EQ 0) AND (bgright[0] GT 0) THEN $
				background[*] = MEAN(arr[f2+1+bgright[1]:$
					f2+1+bgright[1]+bgright[0]])
; LHS only...
			IF (bgleft[0] GT 0) AND (bgright[0] EQ 0) THEN $
				background[*] = MEAN(arr[f1-1-bgleft[1]-bgleft[0]:$
					f1-1-bgleft[1]])
; Average over both left and right sides...
			IF (bgleft[0] GT 0) AND (bgright[0] GT 0) THEN $
				background[*] = 0.5*(MEAN(arr[f2+1+bgright[1]:$
					f2+1+bgright[1]+bgright[0]])+MEAN(arr[f1-1-bgleft[1]$
					-bgleft[0]:f1-1-bgleft[1]]))
		ENDIF $
;-------------------------------------------------
; If fitbg is non-zero then fit:
			ELSE BEGIN
; First create an array containing the pixel numbers of all the pixels used in
; the background fit: (bgpixind = "bg pixel index")
; Use RHS only...
			IF (bgleft[0] EQ 0) AND (bgright[0] GT 0) THEN $
				bgpixind =  f2 + 1 + bgright[1]+INDGEN(bgright[0])
; LHS only...
			IF (bgleft[0] GT 0) AND (bgright[0] EQ 0) THEN $
				bgpixind =  f1 - 1 - bgleft[1] - INDGEN(bgleft[0])
; Create pixel indices for both left and right sides by concatenating
; the two series above:
			IF (bgleft[0] GT 0) AND (bgright[0] GT 0) THEN $
				bgpixind = [f1 - 1 - bgleft[1] - INDGEN(bgleft[0]),$
				f2 + 1 + bgright[1]+INDGEN(bgright[0])]

; Remove pixels addressed by bgpixind that index a error pixel equal to 0.0
; (these will cause LINFIT to fuck up)
		bgpixind = bgpixind[WHERE(errarr[bgpixind] GT 0.0)]
; fit the background using the selected pixels in bgpixind and their errors
; Use /DOUBLE to prevent things like underflow.
		shes_well_fit = LINFIT(bgpixind,arr[bgpixind],SDEV=errarr[bgpixind],$
								/DOUBLE)
; generate the background by evaluating the fit in the foreground range [f1:f2]
		background = shes_well_fit[0] + shes_well_fit[1] * (INDGEN(f2-f1+1)+f1)
		END
	END

; Take off background
	newarr[*] = arr[f1:f2] - background[*]
; Error due to background is SQRT(background). Total error is quadrature sum
; of this and the error array.
	newerrarr[*] = SQRT(errarr[f1:f2]^2+background[*])

; Calculate peak intensity and its error by adding up all pixels (total intensity)
	peak_int = TOTAL(newarr)
; Add in quad. errors on individual pixels
	int_err = SQRT(TOTAL(newerrarr[*]^2))

; Return results
	RETURN,[peak_int,int_err,com]
END


;===============================================================================
; THE MAIN ANALYSIS ROUTINE! This routine is called whem the analyse button
; is pressed. All user inputted fields are passed through the structure
; 'user' (defined in mono_event, below). If no data is loadec then this procedure
; first loads all data and a window is displayed to allow the user to check/
; modify the direct beam attenuation. Then the "approximate" position of the
; reflected beam peak is found. This is done because can't simply find max
; of reflected beam data in places where reflectivity is low so need an area
; to look for it. After data is loaded (or if data has already been loaded)
; the analysis is done. First the reflectivity is calculated by using fpeak
; to find the intensity of the direct and reflected beam peaks-these are then 
; divided. Then q (and its resolution) is calculated. Finally the data is stored
; to a file and plotted. The plotted data is first bundled/grouped using
; group_monodata.
;===============================================================================
PRO analysis,user

; Common block for base id's
COMMON baseid,mono_base,file_base,anal_base

; common block for pointers to data. and mean of reflected peaks
COMMON loaded_data,db_data,ref_data,water

; Common block containing widget id of status label
COMMON status,loadedflag,label_status

; Common block for widget id's of DB attenuation correction window
; Also contains the number of the first numor for plotting purposes
COMMON attcorr,plotbase,butt_done_attcorr,butt_redo_attcorr,$
			text_factor_attcorr,butt_correctby,slider_startatt,firstnumor

; common block for message base id (So the plot window can use the same event
; handler as a message window since it also only has an OKAY button to close it)
COMMON msgok,butt_msgok

; Common block containing useful area of detector
COMMON detector,xmin,xmax,refpmean

; number of direct beam numors
	n_dbnumors = user.db_last-user.db_first+1

;----------------------------------------------------
; If there is no data loaded then proceed to load:
	IF NOT loadedflag THEN BEGIN

		PRINT,STRING([10B,10B])+'LOADING DATA FILES'

; Free memory allocated to old data set
		PTR_FREE,db_data,ref_data
		PTR_FREE,water

; Read water file.
		IF user.waterfile GT 0 THEN BEGIN
			PRINT,'Reading water file...'
			WIDGET_CONTROL,label_status,SET_VALUE='Loading data...(Water file)'
			IF NOT data_readm(user.datadir,user.waterfile,numor) THEN RETURN
; store data of water file into an array
			water = numor
		ENDIF ELSE BEGIN
			PRINT,'No water file!'
; create a null pointer - no water correction
			water = PTR_NEW()
		END

; Resize pointer array. n_dbnumors for direct beam. n_dbnumors* polariation
; option for refl. beam
 		db_data = PTRARR(n_dbnumors)
		ref_data = PTRARR(user.pol+1,n_dbnumors)

; array to store maxima of each reflected beam numor
		refmax = FLTARR(user.pol+1,n_dbnumors)

;***********************************************************************************
; This loop reads in the numors for driect and relfected beams.  The position of the
; maximum is found for each of the reflected beam numors

		FOR i=0, n_dbnumors-1 DO BEGIN

			runoftot=STRTRIM(STRING(i+1),1)+'/'+STRTRIM(STRING(n_dbnumors),1)
			PRINT,'Reading data set '+runoftot
			WIDGET_CONTROL,label_status,SET_VALUE='Loading data...('+runoftot+')'

			PRINT,'	Direct Beam file'

; Read in the ith direct beam numor, if error then return
			IF NOT data_readm(user.datadir,i+user.db_first,numor) THEN RETURN

; store pointer to data
			db_data[i]=numor			

			PRINT,'	Reflected beam file(s)'

; Read in the ith reflected beam numors (one for each polarisation=user.pol+1)
; user.pol=0 (unpolarised), 1 (polarised), 2/3 (polarised with analyser)
; user.pol+1=number of refl. numors per direct beam numor
			FOR j=0,user.pol DO BEGIN
				IF NOT data_readm(user.datadir,$
							i*(user.pol+1)+j+user.ref_first,numor) THEN RETURN
; store pointer to data in pointer array
				ref_data[j,i]=numor

; find max of data and store it in refmax ONLY if dan>2.3. Otherwise put -1.0 (not
; a valid pixel) as the max. See later comments concerning finding mean peak value.
; (Do temporary water correction first if necessary). Divide by zero is ignored
; by /NAN keyword
				IF PTR_VALID(water) THEN maxval=MAX((*numor).detdata/(*water).detdata,/NAN,mpix) $
; Invalid water pointer-> no water file loaded-> no water correction
					ELSE maxval=MAX((*numor).detdata,mpix)

				IF (*numor).dan GT 2.3 THEN refmax[j,i]=mpix ELSE refmax[j,i]=-1.0
			ENDFOR

		END

;******************************************************************************* 
; Now correct for any attenuators placed in the direct beam

		PRINT,'Correcting for direct beam attenuation.'
		WIDGET_CONTROL,label_status,SET_VALUE='Correcting for direct beam attenuation.'

; >>>-***- HARDWIRED DEFAULT FACTOR FOR ATTENUATOR EXPONENTIAL -***-<<<
; "magic" factor in exp(...) of attenuator
		magic_factor = 1.0

; store first db numor into a common variable for plottting purposes in direct
; beam attenuation-correction window
		firstnumor = user.db_first

; Window to contain a graph which the user can use to check that attenuators
; are properly corrected for
		!P.MULTI=[0,1,1]
; create a modal window
		plotbase=WIDGET_BASE(GROUP_LEADER=mono_base,/MODAL,$
						TITLE='Direct beam attenuation correction',/COLUMN)
		graph1=WIDGET_DRAW(plotbase,XSIZE=700,YSIZE=300,RETAIN=2)
		subplotbase = WIDGET_BASE(plotbase,/ROW)
		butt_correctby = CW_BGROUP(subplotbase,$
			['Reading attenuator number from numor','Looking for intensity drops'],$
			/EXCLUSIVE,/FRAME,LABEL_LEFT='Deduce attenuator number by:',SET_VALUE=0)
		slider_startatt = WIDGET_SLIDER(subplotbase,MINIMUM=0,MAXIMUM=7,$
			TITLE='Select attenuator to start with.',/FRAME,VALUE=0)
		dummy = WIDGET_LABEL(plotbase,VALUE=$
			'NB: Use of attenuator number in numor file is recommended unless this has not been written')
		dummybase = WIDGET_BASE(plotbase,/ROW,/BASE_ALIGN_CENTER)
		dummy = WIDGET_LABEL(dummybase,VALUE=$
				'Factor in exponential attenuation: EXP(-factor*att_number)')
		text_factor_attcorr = WIDGET_TEXT(dummybase,/EDITABLE,$
				VALUE=STRING(magic_factor),XSIZE=10)
		butt_done_attcorr = WIDGET_BUTTON(dummybase,VALUE='Done')
		butt_redo_attcorr = WIDGET_BUTTON(dummybase,VALUE='Do again')

; realise.
		WIDGET_CONTROL,plotbase,/REALIZE
; desensitize slider since by default it is not needed unless the user
; selects 'Looking for intensity drops' as the method to correct for attenuators
		WIDGET_CONTROL,slider_startatt,SENSITIVE=0

; "simulate" the redo button being pressed. This is done so that we can use the
; event handler for the window to plot the graph as soon as the window is realised
; so the user doesn't have to press a button before seeing a graph. This could
; have been done by setting the /NOTIFY_REALIZE keyword on plotbase to specify
; a procedure to call when the window is created. However the idiots who
; made IDL didn't think to call this procedure in the same way as calling an
; ordinary event handler thus rendering this option impossible. So here is a 
; little fix...
		attcorrwin_event,{id:butt_redo_attcorr}

;  Register: Control will be passed back once the window is closed
; (coz its modal)
		XMANAGER,'attcorrwin',plotbase

;*******************************************************************************
; Find distribution of reflected beam peaks disregarding any numors which may
; contain direct beam spillage (i.e. where dan<2.3 - these are set to -1.0)
; Only do this if the user has not specified an 'override' for this , in which
; case refpmean[j] will be non-zero

; error flag
		badflag=0
		FOR j=0,user.pol DO BEGIN 
; Only bother if refpmean[j] is zero, i.e. if user has not specified peak
; position
			IF refpmean[j] EQ 0.0 THEN BEGIN
				Error_status=0
; error handler
				CATCH,Error_status
; set the error flag to 1 so that a warning can be signalled if this calc
; fails
				IF Error_status NE 0 THEN BEGIN 
					badflag = 1 
					refpmean[j]=0.0
				ENDIF ELSE BEGIN
; Create a temporary array containing all reflected beam numor maximums where
; dan>2.3 (i.e. where max > 0.0 - see check above)
					temparr=refmax[j,WHERE(refmax[j,*] GT 0.0)]
; Calc the distribution of the first HALF of these numors. It is assumed that
; for these the reflectivity is reasonably large and won't be drowned out
; by noise. The mean of this dist. should give an idea as to where to look for
; the specular peak.
					peakdist=MOMENT(temparr[0:FIX(N_ELEMENTS(refmax[j,*])/2.0)])
; store the mean in the COMMON varaible refpmean (4-vector floating array)
					refpmean[j] = peakdist[0]
				END
				CATCH,/CANCEL
			ENDIF
		ENDFOR

; ouput warning message if there was a problem in finding refpmean
		IF badflag EQ 1 THEN BEGIN
; set the loaded data flag to 1 (true)....
			loadedflag=1
			msgwin,'Warning! Unable to find the approximate position of the reflected beam peak for some/all polarisations. You will have to set these manually by pressing the PEAKS/RANGES button before analysing. The approximate reflected beam position is set to zero for all those which failed.',TITLE='Warning!'
; Put 'data loaded' on status label
			WIDGET_CONTROL,label_status,SET_VALUE='Data Loaded. !PLEASE MANUALLY SET REFLECTED BEAM PEAKS!'
; ...but exit the analysis procedure so the user can correct the problem
			RETURN
		END
; Finished loading data, so set loadedflag to true (1).
		loadedflag = 1
	ENDIF
; *****************END OF DATA LOADING*********************

; Put 'data loaded' on status label
	WIDGET_CONTROL,label_status,SET_VALUE='Data Loaded.'

;***********************************************************************************
; Do the data analysis:

	PRINT,'Analysing data...'

; RESULT ARRAYS:
; Arrays for reflectivity versus q (and errors). One for each polarisation.
	reflectivity = FLTARR(user.pol+1,n_dbnumors)
	reflectivity_err = FLTARR(user.pol+1,n_dbnumors)
	q = FLTARR(user.pol+1,n_dbnumors)
	q_res = FLTARR(user.pol+1,n_dbnumors)

;--------------------------
; This loop analysis each set of data (i.e. each direct beam and its reflected
; beam numor(s) (no. of ref. beams depends on polarisation
	FOR i=0,n_dbnumors-1 DO BEGIN

;-------------
; Copy db data into working structure and find Poissonian error
; Normalise and correct for water. Do background subtraction and find peak
		db = *db_data[i]
		dberr = FLTARR(db.xsize)
		dberr = SQRT(db.detdata)

; Normalise to water if a water file was specified (i.e. pointer 'water'
; is valid)
		IF PTR_VALID(water) THEN BEGIN 
			db.detdata = db.detdata/(*water).detdata
			dberr = dberr/(*water).detdata
		END

; Find peak intensity of direct beam
		dbpeak = fpeak(db.detdata,dberr,user.fgwidth,user.bgleft,user.bgright,user.bgsub)
; Signal error if fpeak ran off the detector
		IF dbpeak[0] EQ -1.0 THEN BEGIN
; If dbpeak[2]=-1.0 then failure occured before COM calculation
			IF dbpeak[2] EQ -1.0 THEN msgwin,/ERROR,$
				'"Range outside detector" error in finding centre of mass for direct beam numor '+$
				STRCOMPRESS(STRING(i+user.db_first),/REMOVE_ALL)+$
				'. Try reducing the foreground width (which is used to'+$
				' as the width for the centre of mass calculation) or adjusting the useful detector size in PEAKS/RANGES'$
; If dbpeak[2] NE -1.0 then the COM was found, but when subtracting background
; fpeak ran off the detector. dbpeak[2] contains COM that caused error
			ELSE msgwin,/ERROR,$
				'"Range outside detector" error when finding peak intensity and/or subtracting background for direct beam numor '+$
				STRCOMPRESS(STRING(i+user.db_first),/REMOVE_ALL)+$
				'. Try reducing the foreground  and background widths (shifts) or adjusting the useful detector size in PEAKS/RANGES.'+$
				'(NB: The centre of mass was found at '+STRCOMPRESS($
				STRING(dbpeak[2]),/REMOVE_ALL)+')'
			RETURN
		END


;*******************************************************************
; Loop over all polarisations for reflected beam and do calculations
		FOR j=0,user.pol DO BEGIN
			ref = *ref_data[j,i]
			referr = SQRT(ref.detdata)

; Normalise
			IF user.norm EQ 0 THEN BEGIN
					ref.detdata = ref.detdata/(ref.monitor/db.monitor)
					referr[*] = referr[*]/(ref.monitor/db.monitor)
			ENDIF ELSE BEGIN
					ref.detdata = ref.detdata/(ref.runtime/db.runtime)
					referr[*] = referr[*]/(ref.runtime/db.runtime)
			END

; Normalise to water if a water file was specified
			IF PTR_VALID(water) THEN BEGIN
				ref.detdata = ref.detdata/(*water).detdata
				referr[*] = referr[*]/(*water).detdata[*]
			END
 
; Find reflected peak intensity
			refpeak = fpeak(ref.detdata,referr,user.fgwidth,user.bgleft,$
								user.bgright,user.bgsub,PEAK_NEAR=refpmean[j])

; Signal error if fpeak ran off the detector
			IF refpeak[0] EQ -1.0 THEN BEGIN
; If refpeak[2]=-1.0 then failure occured before COM calculation
				IF refpeak[2] EQ -1.0 THEN msgwin,/ERROR,$
					'"Range outside detector" error in finding centre of mass for reflected beam numor '+$
					STRCOMPRESS(STRING((i*user.pol)+j+user.ref_first),/REMOVE_ALL)+$
					'. Try reducing the foreground width (which is used to'+$
					' as the width for the centre of mass calculation) or adjusting the useful detector size in PEAKS/RANGES'$
; If refpeak[2] NE -1.0 then the COM was found, but when subtracting background
; fpeak ran off the detector. refpeak[2] contains COM that caused error
				ELSE msgwin,/ERROR,$
					'"Range outside detector" error when finding peak intensity and/or subtracting background for reflected beam numor '+$
					STRCOMPRESS(STRING((i*user.pol)+j+user.db_first),/REMOVE_ALL)+$
					'. Try reducing the foreground  and background widths (shifts) or adjusting the useful detector size in PEAKS/RANGES.'+$
					'(NB: The centre of mass was found at '+STRCOMPRESS($
					STRING(refpeak[2]),/REMOVE_ALL)+')'
				RETURN
			END

;-----------------------------
; Calc. Reflectivity and error
;-----------------------------
; Ratio of reflected to direct beam
			reflectivity[j,i] = refpeak[0]/dbpeak[0]
; Combination of fractional errors in quadrature
			reflectivity_err[j,i] = SQRT((refpeak[1]/refpeak[0])^2+$
								(dbpeak[1]/dbpeak[0])^2)*ABS(reflectivity[j,i])

;--------------------------
; Calculate q and its error
;--------------------------
; degrees per radian (convert rad->deg.)
			dpr=180./!PI
			IF (user.qcalc EQ 1) THEN theta = ref.san $
			ELSE BEGIN
; pixel which is at centre of detector (i.e. intersection of detector plane
; and line which is perpendicular to detector plane that passes thru sample
; (NB: nx is a binning factor, usually 1, i.e. 1 pixel per bin)
; !!!!!!!HARDWIRED NUMBER 135.79!!!!!!
				pcen=135.79/db.nx
; millimeters per pixel (note that detector distance, dett is in millimeters)
; !!!!!!!HARDWIRED NUMBER 1.04!!!!!!
				mmpp=1.04*db.nx

; calculation of theta from simple geometry using centre of mass in pixels
; (COM's are in refpeak[2] and dbpeak[2])
				theta=(ref.dan+dpr*ATAN((pcen-refpeak[2])*mmpp/ref.dett))/2-$
					  (db.dan+dpr*ATAN((pcen-dbpeak[2])*mmpp/db.dett))/2
			END

; resolution of theta. This is calculated from the slit openings. It contributes
; to the q-resolution in two ways: 1) As the resolution of theta, 2) As the
; resolution of angles leaving the monochromator. This contributes to the
; wavelength resolution.
; !!!!!!!HARDWIRED NUMBER interslit distance = 3.500!!!!!!
			d_theta=2.*ATAN((db.s2+db.s3)/(3500.0*2.))*dpr

; (Fractional error of wavelength)^2. Note monochr_dspread is fractional
; spread in d-spacings of monochromator crystal. This is in percent (%).
; d_theta/user.monochr_ang is the fractional error in the outgoing angles
			fracerr_lambda = (user.monochr_dspread/100.0)^2 + $
							(d_theta/user.monochr_angle)^2

; Standard formula for q, use the user specified wavelength
			q[j,i] = 4*!PI*SIN(theta/dpr)/user.wavelength

; Resolution for q (combination of fractional errors of wavelength and theta)
; Note that fracerr_lambda is already sqaured (see above)
			q_res[j,i] = SQRT(fracerr_lambda+(d_theta/theta)^2)*ABS(q[j,i])
		
		ENDFOR
; (end of polarisation loop)
;***********************************************************

; Loop to next direct beam numor
	ENDFOR

;----------------------
; Output data
;----------------------

;******PLOT************

; Set number of graphs and size of window according to number of plots 
; (polarisations). Also set titles of windows through graph_titles.
; ('basetitle' is a string describing what polariser/analyser is used.
; the string array, 'graphtitles' is made up of basetitle + a number for
; each polarisation)
	CASE user.pol OF
		0: BEGIN
			!P.MULTI=[0,1,1]
			graph_xsize = 600
			graph_ysize = 400
			basetitle='Unpolarised'
			subtitles = ''
			END
		1: BEGIN
			!P.MULTI=[0,1,2]
			graph_xsize = 600 
			graph_ysize = 300
			basetitle = 'Polarised, no analyser:'
			subtitles = ['+','-']
			END
		2: BEGIN
			!P.MULTI=[0,1,3]
			graph_xsize=600
			graph_ysize=200
			basetitle = 'Polarised, analyser with no +-/-+ distinction:'
			subtitles = ['++','+-','--']
			END
		3: BEGIN
			!P.MULTI=[0,2,2]
			graph_xsize=600
			graph_ysize=400
			basetitle = 'Polarised, analyser with +-/-+ distinction:'
			subtitles = ['++','+-','-+','--']
			END
	ENDCASE

; concatenate base- and sub- titles to create an array of titles- one for
; each polarisation
	graph_titles=basetitle+subtitles

; Set up the plotting base widget
	plotbase = WIDGET_BASE(GROUP_LEADER=mono_base,/COLUMN,$
					TITLE='Plot(s) of Log(reflectivity) versus q')
	graphs = WIDGET_DRAW(plotbase, XSIZE=graph_xsize, YSIZE=graph_ysize,$
				RETAIN=2)
; Use butt_msgok for button (same as for message window)
	butt_msgok = WIDGET_BUTTON(plotbase,VALUE='DONE')
	
;realize
	WIDGET_CONTROL,plotbase,/REALIZE
; Set event handler to message window handler (which looks to see if butt_msgok
; is pressed.
	XMANAGER,'msgwin',plotbase

; Plot the graphs
; x/y axis labels
	!X.TITLE = 'q (Angstroms^-1)'
	!Y.TITLE = 'Log10(Ref)'
	FOR j=0,user.pol DO BEGIN
; Group the data. Combines points that are 'close' within the resolution of q
; Grouped data is x=q, y=log(reflectivity), err_y=error in y
		group_monodata,q[j,*],reflectivity[j,*],reflectivity_err[j,*],$
				q_res[j,*],y,x,err_y

; PLOT:
; get maximum pixel value and index
		maxy=MAX(y,mpix)
; max is max value plus its error bar
		maxy=maxy+err_y[mpix]
; similar for min
		miny=MIN(y,mpix)
		miny=miny-err_y[mpix]
; set plot range accordingly
		!Y.RANGE=[miny,maxy]
; select appropriate title
		!P.TITLE=graph_titles[j]
		PLOTERR,x,y,err_y
	ENDFOR

;*****WRITE TO FILE******

; Create an array of filenames. The user enters a filename, e.g. mono_output.dat
; A file is written for each of the polariser/analyser combinations (up to 4)
; an array of filenames is created for a file '[filename].[extention]' 
; (where [filename] and [extention] are strings containing filename and 
; extention) with the polariser/analyser set up appended to the [filename]. 
; E.g. for a polarisor with no analyser (2 types: +:'spin up'; 
; -:'spin down) the following files are created: [filename]_+.[extention]
; and [filename]_-.[extention]. So for 'mono_output.dat' the files
; 'mono_output_+.dat' and 'mono_output_-.dat' would be created.

; Find the position of the full stop ('.', i.e. [filename]/[extention] 
; delimeter
	stoppos = STRPOS(user.outputfile,'.')
; If there is no full stop-> no extention so set accordingly
	IF stoppos EQ -1 THEN BEGIN
		filename=user.outputfile
		fileext=''
; If there is a full stop then get the filename and extention:
	ENDIF ELSE BEGIN
; extract filename (all characters up to full stop)
		filename=STRMID(user.outputfile,0,stoppos)
; extract extention (all characters from (including) full stop
		fileext=STRMID(user.outputfile,stoppos,STRLEN(user.outputfile)-stoppos)
	END

; Now stick the pol/anal settings to the end of each filename
	CASE user.pol OF
; no polarisation -> simple,
		0: filenames = filename
; Polariser:
		1: filenames = filename + '_'+['+','-']
; Polariser+analyser, but no +-, -+ distinction
		2: filenames = filename + '_'+['++','+-','--']
; Polariser and analyser
		3: filenames = filenames + '_' + ['++','+-','-+','--']
	ENDCASE

; no re-attatch the ext. - if there was one:
	IF fileext NE '' THEN filenames=filenames+fileext

; Write files
	FOR j=0, user.pol DO BEGIN

		PRINT,'Writing data file '+filenames[j]+'...'

; attempt to open file
		CLOSE,10
		OPENW,10,filenames[j],ERROR=err

; Begin an error handler for write errors
		error_status=0
		CATCH,error_status

; Control is passed here on error so first check that no error occured. If so
; output error message and return
		IF (err NE 0) OR (error_status NE 0) THEN BEGIN
; Different messages depending on whether it was an open or write error
			IF err NE 0 THEN BEGIN
				msgwin,/ERROR,'Error opening file: "'+filenames[j]+'".'+$
					'(Check filename?)'+$
					STRING(10B)+'Error message: "'+!ERR_STRING+'".'
				RETURN
			END
			IF error_status NE 0 THEN BEGIN
				msgwin,/ERROR,'Error writing to file: "'+filenames[j]+'".'+$
					STRING(10B)+'Error message: "'+!ERR_STRING+'".'
				RETURN
			END
		ENDIF ELSE BEGIN
; No error so proceed with writing

; Print a little header saying what it is when it was created and how
; many other files (i.e. polarisations) there are in the set
			PRINTF,10,'***Monochromatic data analysis output file**'
			PRINTF,10,'Created:'+SYSTIME(0)
			PRINTF,10,'(File '+STRCOMPRESS(STRING(j+1),/REMOVE_ALL)+$
				' of '+STRCOMPRESS(STRING(user.pol+1),/REMOVE_ALL)+')'

; Print out the parameters used:
; Input files:
			PRINTF,10,'Direct beam numors:'+STRCOMPRESS(STRING(user.db_first),$
				/REMOVE_ALL)+'-'+STRCOMPRESS(STRING(user.db_last),$
				/REMOVE_ALL)
			PRINTF,10,'Reflected beam numors:'+STRCOMPRESS(STRING(user.ref_first),$
				/REMOVE_ALL)+'-'+STRCOMPRESS(STRING(user.ref_last),$
				/REMOVE_ALL)
			PRINTF,10,'Number of polarisations:'+STRCOMPRESS(STRING(user.pol+1),$
				/REMOVE_ALL)
			PRINTF,10,'Water file:'+STRCOMPRESS(STRING(user.waterfile),$
				/REMOVE_ALL)
			PRINTF,10,STRING(10B)
; Analysis options:	
			PRINTF,10,'Foreground width:'+STRCOMPRESS(STRING(user.fgwidth),$
				/REMOVE_ALL)
			PRINTF,10,'Left background [shift,width]: '+$
				STRCOMPRESS(STRING(user.bgleft[0]),/REMOVE_ALL)+','+$
				STRCOMPRESS(STRING(user.bgleft[1]),/REMOVE_ALL)
			PRINTF,10,'Right background [shift,width]: '+$
				STRCOMPRESS(STRING(user.bgright[0]),/REMOVE_ALL)+','+$
				STRCOMPRESS(STRING(user.bgright[1]),/REMOVE_ALL)
			PRINTF,10,'Monochromator angle:'+$
				STRCOMPRESS(STRING(user.monochr_angle),/REMOVE_ALL)
			PRINTF,10,'Monochromator fractional d-spacing:'+$
				STRCOMPRESS(STRING(user.monochr_dspread),/REMOVE_ALL)
			IF user.norm EQ 0 THEN PRINTF,10,'Normalised to monitor' $
				ELSE PRINTF,10,'Normalised to runtime.'
			IF user.bgsub EQ 0 THEN PRINTF,10,'Background was averaged.' $
				ELSE PRINTF,10,'Background was fitted.'
			IF user.qcalc EQ 0 THEN PRINTF,10,'Theta was calculated.' $
				ELSE PRINTF,10,'SAN was used as theta.'

; *!*!*!*! NB: It is important to have the wavelenght on the penultimate line.
; 'bundle.pro' searches for the string 'lam' in the 2nd to 4th characters of a 
; line and discard all lines up to and including that and the next too. 
; Therefore it is important to have this in the file so the data can be 
; read by 'bundle.pro' properly.

; Note that the first part of the string must be ' lam' 
			PRINTF,10,' lambda (Wavelength /Angstroms):'+$
				STRCOMPRESS(STRING(user.wavelength),/REMOVE_ALL)

; Now we're allowed one more line before 'bundle.pro' expects the data
; Indicate what the columns are on this line
			PRINTF,10,'	q	q resolution	reflectivity	ref error'

; Now write data for each point (numor: n_dbnumors in total)
			FOR i=0,n_dbnumors-1 DO PRINTF,10,q[j,i],q_res[j,i],$
					reflectivity[j,i],reflectivity_err[j,i]
		ENDELSE
		CATCH,/CANCEL
	ENDFOR	
; close the file
	CLOSE,10
END

;===============================================================================
; Procedure to group data. Combines data from points which are, within the 
; resolution, too close to each other. Also takes logarithm of the data and
; propagates the errors accordingly
; x = q values. y = Reflectivity values. e = error on reflectivity data
; dq = error on q values
; a = groupd reflectivity. b = groupd q values. c = groupd refl. errors
;===============================================================================
PRO group_monodata,x,y,e,dq,a,b,c

; arrays to store groupd data
	ny=FLTARR(N_ELEMENTS(y))
	nx=FLTARR(N_ELEMENTS(x))
	nee=FLTARR(N_ELEMENTS(e))
	ndq=FLTARR(N_ELEMENTS(dq))


	PRINT,'Bundling nearby data points for plotting:'

; resf = 'Magic' resolution factor. if the difference in q values is less 
; than dq/resf then group the points. With resf=2. data points 
; separated by half the error on q are groupd
	resf=2.

; number of elements in data
	tot=N_ELEMENTS(x)-1

; i counts the number of iterations purely for information on a print statement
	i=1
	
; This loop repeats until the number of pairs found is zero. (NB: It is 
; possible that the number of pairs will not decrease after an iteration.
; In this case data has been groupd, but the resulting point appears close
; to another point. This is why the number of pairs may remain equal to, e.g.
; 1 for the last few iterations
	REPEAT BEGIN

; reset c. c is used as a counter to address the data
		c=0
; set num to the number of elements in the data
		num=tot
; new is used to address the elements of the arrays where the groupd data is stored
		new=0
; counts the number of pairs encountered in each iteration
		pairs=0

; loop through data array
; while c is less than the last element (i.e. up to and including penultimate)
		WHILE c LT num DO BEGIN

; If q values are sufficiently close....
			IF (x(c+1)-x(c) LT dq(c)/resf) AND (x(c+1) ne 0) THEN BEGIN

; ... then group the data. This is done by averaging the q values and reflectivity of
; each point and the errors are obtained by adding in the two errors in quadrature
				ny(new)=(y(c+1)+y(c))/2.
				nee(new)=(sqrt(e(c)^2.+e(c+1)^2.))/2
				nx(new)=(x(c)+x(c+1))/2.
				ndq(new)=(dq(c)+dq(c+1))/2.

; increase number of pairs
				pairs=pairs+1
; increment counter by two because two numbers have been processed
				c=c+2
; decrease the number of data elements by 1: two points have been groupd
				tot=tot-1
			ENDIF ELSE BEGIN
; If no bundling is necessary then just copy the data into the new arrays.
				ny(new)=y(c)
				nx(new)=x(c)
				nee(new)=e(c)
				ndq(new)=dq(c)
; inc. counter by one
				c=c+1
			ENDELSE

;inc. groupd data array counter
			new=new+1
		END

; set old data to new groupd data ready for next iteration
		y=ny
		x=nx
		e=nee
		dq=ndq

		PRINT,pairs,' pairs found on iteration ',i
; increment number of iterations
		i=i+1
	
; stop if no pairs were found
	ENDREP UNTIL pairs eq 0

; Take only the (grouped) reflectivity data that is >0.0 (to prevent error
; when taking log)
	data_ind = WHERE(ny[0:tot-1] GT 0.0)

; return results: Log reflectivity data and calc. error on reflectivity.
; (a=log(reflect.) b=q, c=error on log(refl.)
	a=ALOG10(ny[data_ind]) 
	b=nx[data_ind] 
	c=(nee[data_ind]/ny[data_ind])/ALOG(10)

	RETURN

END

;===============================================================================
; Event handler for Direct beam attenuation correction window. The attenuation
; correction is done using the attenuator number written to the numor 
; (recommeded) or by specifying a start attenuator and looking for drops.
; The user can change the attenuation factor. By pressing the "do again"
; button the user can inspect the graph of intensity versus numor to see
; if it is correct (the intensity should increase continuously as the slits
; are opened). When the user is satisfied the "done" button is pressed and the
; direct beam data are corrected using the selected parameters.
;===============================================================================
PRO attcorrwin_event,event

; Common block for widget id's of DB attenuation correction window
; Also contains the number of the first numor for plotting purposes
COMMON attcorr,plotbase,butt_done_attcorr,butt_redo_attcorr,$
			text_factor_attcorr,butt_correctby,slider_startatt,firstnumor

; common block for pointers to data. and mean of reflected peaks
; Only really need direct beam in this procedure
COMMON loaded_data,db_data,ref_data,water


; If the 'correct by' button was changed then sensitize the 'starting attenuator'
; slider if 'Look for intensity drops' method was chosen. This slider specifies
; which attenuator was in on the first numor. Subsequent attenuators are
; found by looking for attenuator drops and by incrementing the numor number
	WIDGET_CONTROL,butt_correctby,GET_VALUE=sel
	IF event.id EQ butt_correctby THEN BEGIN
		IF sel EQ 1 THEN WIDGET_CONTROL,slider_startatt,SENSITIVE=1 $
		ELSE WIDGET_CONTROL,slider_startatt,SENSITIVE=0
	END

; Return if event doesn't come from one of the buttons or from the realisation
; of the window
	IF (event.id NE butt_done_attcorr) AND (event.id NE butt_redo_attcorr) $
			AND (event.id NE plotbase) THEN RETURN

; array to contain total counts for each db numor, corrected for attenuation
; this is for plotting purposes only
	corr_counts = FLTARR(N_ELEMENTS(db_data))

; total counts of last numor (initialise to 1e-10 to avoid divide by zero on 1st 
; iteration)
	last_count = 1e-10

; starting number of attenuators in beam
	WIDGET_CONTROL,slider_startatt,GET_VALUE=temp
	att = temp[0]

; get magic factor from user and convert to floating point
	WIDGET_CONTROL,text_factor_attcorr,GET_VALUE=temp
	magic_factor = FLOAT(temp[0])

; Loop over all direct beam numors
	FOR i=0,N_ELEMENTS(db_data)-1 DO BEGIN

; sum counts
		counts=TOTAL((*db_data[i]).detdata)

; If user selects "correct by looking for intensity drops"  (see above, sel)
; then do that...
		IF sel EQ 1 THEN BEGIN		
; If there is a large enough drop (factor of 0.8) then an attenuator has been 
; added. Increment attenuator count
			IF counts/last_count LT 0.8 THEN BEGIN
				IF att EQ 7 THEN BEGIN 
; If more than 7 attenuators are found display an error message (NOT using
; the /ERROR keyword and using /NOSENSITIZE so that the main window doesn't
; change its sensitisation).
					msgwin,'Error: an attenuator number greater'+$
					' than 7 was encountered, i.e. too many intensity drops occured.'+$
					' (Try lowering starting attenuator number?)',TITLE='Error',$
					/NOSENSITIZE
					RETURN
				END
				att=att+1
			END
; Otherwise set attenuator from file
		ENDIF ELSE att=(*db_data[i]).att

		fac=(EXP(-1.0*magic_factor*FLOAT(att)))
; attenutator-corrected counts are simply counts divided by exponential
		corr_counts[i]=counts/fac

; correct ACTUAL data only if user is satisfied and pressed 'done' button
		IF event.id EQ butt_done_attcorr THEN $
					(*db_data[i]).detdata[*]=(*db_data[i]).detdata[*]/fac
	
; last counts = current counts for next loop
		last_count = counts

	ENDFOR

; Get rid of window if done
	IF event.id EQ butt_done_attcorr THEN BEGIN 
		WIDGET_CONTROL,event.top,/DESTROY
	END

; Replot if Do again button is pressed or if window is realised
	IF (event.id EQ butt_redo_attcorr) THEN BEGIN
; various labels
		!P.TITLE = 'Plot of total attenuator-corrected counts versus  numor'
		!X.TITLE = 'Numor'
		!Y.TITLE = 'Total (corrected) counts'
		!Y.RANGE = [0.0,MAX(corr_counts)]
		PLOT,INDGEN(N_ELEMENTS(db_data))+firstnumor,corr_counts
	ENDIF
END

;===============================================================================
; Event handler for peaks/ranges window. This gets the values for the useful
; area of the detector and the approx. positions of the reflected beam peaks
; performing the necessary error checks.
;===============================================================================
PRO pixels_event,event

; Common block containg OK button id for peaks/ranges window
COMMON pixelsbutt,butt_pixelsok,text_xmax,text_xmin,text_refp1,text_refp2,$
		text_refp3,text_refp4

; Common block containing useful area of detector and approx whereabouts of 
; specular peak (for each polarisation, i.e. up to 4)
COMMON detector,xmin,xmax,refpmean

; Get values if okay button is pressed
	IF event.id EQ butt_pixelsok THEN BEGIN

; get the contents of the text widgets
		WIDGET_CONTROL,text_xmax,GET_VALUE=xmax_str
		WIDGET_CONTROL,text_xmin,GET_VALUE=xmin_str		
		WIDGET_CONTROL,text_refp1,GET_VALUE=refp1_str
		WIDGET_CONTROL,text_refp2,GET_VALUE=refp2_str
		WIDGET_CONTROL,text_refp3,GET_VALUE=refp3_str
		WIDGET_CONTROL,text_refp4,GET_VALUE=refp4_str

; store strings into array
		refp_str=STRARR(4)
		refp_str=[refp1_str[0],refp2_str[0],refp3_str[0],refp4_str[0]]

; get xmax (temp is used as temporary varaible during conversion)
; (Use QUIET keyword to disable error messages used for the main window)
		temp1=getint(xmax_str[0],'Useful area of detector: Maximum x',$
					MAX_VALUE=1000,/ALLOW_EMPTY,/QUIET)
; return on error
		IF temp1 EQ -1 THEN BEGIN 
			msgwin,'Error: xmax is badly formatted or out of range.'+$
				' Must be integer in range [0,1000].',TITLE='Error!'
			RETURN
		END

; get xmin
		temp2=getint(xmin_str[0],'Useful area of detector: Minimum x',$
					MAX_VALUE=1000,/ALLOW_EMPTY,/QUIET)
; return on error
		IF temp2 EQ -1 THEN BEGIN 
			msgwin,'Error: xmin is badly formatted or out of range.'+$
				' Must be integer in range [0,1000].',TITLE='Error!'
			RETURN
		END


; Check that xmin<xmax (only if neither are zero-the default)
		IF (temp2 GE temp1) AND (temp2 NE 0) AND (temp1 NE 0) THEN BEGIN
			msgwin,'Error minimum x value must be less than maximum!',$
					TITLE='Error!'
			RETURN
		END

; Get approximate specular-peak positions
		temparr=FLTARR(N_ELEMENTS(refp_str))
; range is constrained by xmin, xmax specified by user:
; If xmax=temp1 ne 0 (default) then set this as the maxval, otherwise set
; large value
		IF temp1 NE 0 THEN maxval=temp1 ELSE maxval=1000.0
		FOR i=0,N_ELEMENTS(refp_str)-1 DO BEGIN
; convert to float using maxval=xmax as upper bound. Can't set minvalue to 
; xmin since user can enter 0.0 as default. Check this on line after.
			temparr[i]=getfloat(refp_str[i],MIN_VALUE=0.0,MAX_VALUE=maxval)
; If temparr[i] isn't finite then error occured. Also check that it's greater
; than xmin=temp2 (if temp2 or temparr[i] not = 0 (default))
			IF  (NOT FINITE(temparr[i])) OR $
					((temparr[i] LT FLOAT(temp2)) AND (temp2 NE 0) AND (temparr[i] NE 0.0)) THEN BEGIN
				msgwin,'Error: Specular peak position ('+$
					STRCOMPRESS(STRING(i+1),/REMOVE_ALL)+$
					') is bad or out of range (xmin,xmax).',TITLE='Error!',$
					/NOSENSITIZE
				RETURN
			END
		ENDFOR

; If we get here then all is well! store variables and destroy widget
		xmax=temp1 & xmin=temp2
; Set only those which aren't zero (if there are any)
		ones_to_set=WHERE(temparr GT 0.0)
		IF ones_to_set[0] NE -1 THEN refpmean[ones_to_set]=temparr[ones_to_set]
		WIDGET_CONTROL,event.top,/DESTROY
	ENDIF
END

;===============================================================================
; Event handler for main window. This gets all the text from the text widgets
; and converts them to the appropriate types (integers/floats etc). Numerous
; error checks are made to ensure that the fields are correctly formatted and
; within range. Other checks are performed, eg to check that the number of db
; and ref. numors correspond, given the number of polarisations. If the 
; fields are all okay then the defaults file is written.
;===============================================================================
PRO mono_event,event

; Common block for base id's
COMMON baseid,mono_base,file_base,anal_base

; common block for button id's
COMMON buttons,	butt_quit, butt_anal, butt_reset,butt_pixels

; common blocks for all of the user entered parameters
COMMON textid,text_db_first,text_db_last,text_water,text_lambda,text_output,$
	text_ref_first,text_ref_last,text_foreground,text_bg_left,text_bg_right,$
	text_dir,text_mcr_ang,text_mcr_dspread

COMMON buttid,butt_polarise,butt_normalise,butt_bg,butt_qcalc

; common block for pointers to data. 
COMMON loaded_data,db_data,ref_data,water

; Common block containing widget id of status label
COMMON status,loadedflag,label_status

; Common block containing useful area of detector and approx whereabouts of 
; specular peak (for each polarisation, i.e. up to 4)
COMMON detector,xmin,xmax,refpmean

; Common block containg OK button id for peaks/ranges window and the widget ids
; of the text boxes
COMMON pixelsbutt,butt_pixelsok,text_xmax,text_xmin,text_refp1,text_refp2,$
		text_refp3,text_refp4

; Quit:
	IF event.id EQ butt_quit THEN BEGIN 
; Free memory and destroy main widget
		PTR_FREE,db_data,ref_data
		WIDGET_CONTROL,event.top,/DESTROY
	END

	IF event.id EQ butt_reset THEN BEGIN
; delete data
		PTR_FREE,db_data,ref_data
; pointers are now 'dangling' set them to null pointers for relative safety
		db_data=PTR_NEW()
		ref_data=PTR_NEW()

; set detector ranges xmin,xmax both to 0 (taken to mean defaults, i.e. 
; xmin = 0, xmax=size of array-see PRO fpeak)
		xmin = 0 & xmax = 0

; reset refpmean (mean of specular peak used in centre of mass calc.) to 0.0
; (=default=let PRO analysis look for it)
		refpmean[*]=0.0

; change status window accordingly
		WIDGET_CONTROL,label_status,SET_VALUE='Data reset. No data loaded.'
		loadedflag=0
; resensitise file entry fields
		WIDGET_CONTROL,file_base,SENSITIVE=1
	END

; Peaks/Ranges button
	IF event.id EQ butt_pixels THEN BEGIN

; base widget (modal)
		pixels_base = WIDGET_BASE(GROUP_LEADER=mono_base,/MODAL,$
							TITLE='Pixel ranges/Peaks',/COLUMN)

; Title stuff
		dummy = WIDGET_LABEL(pixels_base,$
				VALUE='NB: To select default (or old value) leave box empty or')
		dummy = WIDGET_LABEL(pixels_base,VALUE=' set to zero. (Defaults in square brackets [])')
		dummy = WIDGET_LABEL(PIXELS_base,VALUE='',YSIZE=10)

; Useful area of detector
		dummy = WIDGET_LABEL(pixels_base,$
							VALUE='Useful area of detector in pixels')
		dumbarse_base = WIDGET_BASE(pixels_base,/FRAME,/ROW)
		dummy = WIDGET_LABEL(dumbarse_base,VALUE='X min [0]')
		text_xmin = WIDGET_TEXT(dumbarse_base,/EDITABLE,XSIZE=4,$
					VALUE=STRCOMPRESS(STRING(xmin),/REMOVE_ALL))
		dummy = WIDGET_LABEL(dumbarse_base,VALUE='X max [size of array]')
		text_xmax = WIDGET_TEXT(dumbarse_base,/EDITABLE,XSIZE=4,$
					VALUE=STRCOMPRESS(STRING(xmax),/REMOVE_ALL))
		dummy = WIDGET_LABEL(PIXELS_base,VALUE='',YSIZE=10)

; "Reflected peak near pixel no." stuff
		dummy = WIDGET_LABEL(pixels_base,$
							VALUE='Specular peak near:')
		dummy = WIDGET_LABEL(pixels_base,VALUE='(NB: 1 for each polarisation')
		dummy = WIDGET_LABEL(pixels_base,VALUE=' [default=automatic search])')

		thickas_base = WIDGET_BASE(pixels_base,COLUMN=4,/FRAME,/BASE_ALIGN_CENTER)

		dummy = WIDGET_LABEL(thickas_base,VALUE='1st')
		text_refp1 = WIDGET_TEXT(thickas_base,/EDITABLE,XSIZE=8,$
					VALUE=STRCOMPRESS(STRING(refpmean[0]),/REMOVE_ALL))
		dummy = WIDGET_LABEL(thickas_base,VALUE='2nd')
		text_refp2 = WIDGET_TEXT(thickas_base,/EDITABLE,XSIZE=8,$
					VALUE=STRCOMPRESS(STRING(refpmean[1]),/REMOVE_ALL))
		dummy = WIDGET_LABEL(thickas_base,VALUE='3rd')
		text_refp3 = WIDGET_TEXT(thickas_base,/EDITABLE,XSIZE=8,$
					VALUE=STRCOMPRESS(STRING(refpmean[2]),/REMOVE_ALL))
		dummy = WIDGET_LABEL(thickas_base,VALUE='4th')
		text_refp4 = WIDGET_TEXT(thickas_base,/EDITABLE,XSIZE=8,$
					VALUE=STRCOMPRESS(STRING(refpmean[3]),/REMOVE_ALL))

; ok button
		butt_pixelsok = WIDGET_BUTTON(pixels_base,VALUE='OK')

;realisation
		WIDGET_CONTROL,pixels_base,/REALIZE
		XMANAGER,'pixels',pixels_base
	END

;Analyse:
	IF event.id EQ butt_anal THEN BEGIN

; desentitise file and analysis bases
		WIDGET_CONTROL,file_base,SENSITIVE=0
		WIDGET_CONTROL,anal_base,SENSITIVE=0

;-----------------------------------------------
; Read in the text widgets and check
;-----------------------------------------------

; Read in text widgets
		WIDGET_CONTROL,text_db_first,GET_VALUE=dbfirst
		WIDGET_CONTROL,text_db_last,GET_VALUE=dblast
		WIDGET_CONTROL,text_water,GET_VALUE=fwater
		WIDGET_CONTROL,text_lambda,GET_VALUE=lambdastr
		WIDGET_CONTROL,text_output,GET_VALUE=output
		WIDGET_CONTROL,text_ref_first,GET_VALUE=reffirst
		WIDGET_CONTROL,text_ref_last,GET_VALUE=reflast
		WIDGET_CONTROL,text_foreground,GET_VALUE=foreground
		WIDGET_CONTROL,text_bg_left,GET_VALUE=bgleft
		WIDGET_CONTROL,text_bg_right,GET_VALUE=bgright
		WIDGET_CONTROL,text_dir,GET_VALUE=dir
		WIDGET_CONTROL,text_mcr_ang,GET_VALUE=mcrang
		WIDGET_CONTROL,text_mcr_dspread,GET_VALUE=mcrdspread
	
; Read in radio button options (integers)
		WIDGET_CONTROL,butt_polarise,GET_VALUE=pol_opt
		WIDGET_CONTROL,butt_normalise,GET_VALUE=norm_opt
		WIDGET_CONTROL,butt_bg,GET_VALUE=bg_opt
		WIDGET_CONTROL,butt_qcalc,GET_VALUE=qcalc_opt

; Convert numor text widgets to numors returning on errors (-1)
; (nb: 999999 is max numor value)
		dbfirst=getint(dbfirst[0],'Direct beam, 1st Numor',MAX_VALUE=999999)	
		IF dbfirst EQ -1 THEN RETURN
		dblast=getint(dblast[0],'Direct beam, last Numor',MAX_VALUE=999999)
		IF dblast EQ -1 THEN RETURN
		reffirst=getint(reffirst[0],'Reflected beam, 1st Numor',MAX_VALUE=999999)
		IF reffirst EQ -1 THEN RETURN
		reflast=getint(reflast[0],'Reflected beam, last Numor',MAX_VALUE=999999)
		IF reflast EQ -1 THEN RETURN
		fwater=getint(fwater[0],'Water (efficiency) file',MAX_VALUE=999999,/ALLOW_EMPTY)
		IF fwater EQ -1 THEN RETURN
	
; If 1st numor greater than last signal error for direct beam
		IF dbfirst GT dblast THEN BEGIN
			msgwin,'Direct beam: 1st Numor ('+$
			STRTRIM(STRING(dbfirst),1)+')  greater than last ('+$
			STRTRIM(STRING(dblast),1)+')!',/ERROR
			RETURN
		END
;...Do similar for reflected beam
		IF reffirst GT reflast THEN BEGIN
			msgwin,'Reflected beam: 1st Numor ('+$
			STRTRIM(STRING(reffirst),1)+')  greater than last ('+$
			STRTRIM(STRING(reflast),1)+')!',/ERROR
			RETURN
		END

; Now check that there are the correct number of reflected numors with
; respect to direct beam numors given the polarisation option
	
; number of ref and db numors
		ndb=dblast-dbfirst+1
		nref=reflast-reffirst+1
	
; pol_opt=0,1,2,3. 0=unpolarised therfore ther are as many reflected as
; direct beam numors (pol_opt+1=1). 1=polarised, so 2 reflected for
; each direct beam (pol_opt+1=2). 2=analyser that only distiguished ++,-- or
; +-/-+ (i.e. 3*) and 3=analyser which distiguishes ++,--,+- and -+.
		IF nref NE ndb*(pol_opt+1) THEN BEGIN
; make a string describing the polarisation options.
			CASE pol_opt OF
				0:polstr='unpolarised'
				1:polstr='polariser - no analyser'
				2:polstr='analyser with no +-,-+ distinction'
				3:polstr='analyser with +-,-+ distinction'
			ENDCASE

; make a string to say whether there are too many/few ref. beam numors
			IF nref GT ndb*(pol_opt+1) THEN too='many' ELSE too='few'
; print out a very long error message (NB STRING([10B,10B]) prints out
; 2 carriage returns.
			msgwin,'Mismatch in number of reflected and direct beam numors!'+$
			STRING([10B,10B])+'For chosen polarsiation options ('+polstr+$
			') there must be '+STRTRIM(STRING(pol_opt+1),1)+' reflected '+$
			'numors per direct beam. You have '+STRTRIM(STRING(nref),1)+$
			' reflected beam numors and '+STRTRIM(STRING(ndb),1)+$
			' direct beam numors. Therfore you have '+$
			STRTRIM(STRING(ABS(nref-ndb*(pol_opt+1))),1)+' too '+$
			too+' reflected beam numors.',/ERROR
			RETURN
		END

; NOW GET THE WAVELENGTH
	
;convert (lambda>0.0 so specify v. small min value, 1e-10
		lambda=getfloat(lambdastr[0],MIN_VALUE=1e-10)

; If lambda is not finite (i.e NaN in this case) then conversion error occured
; or lambda<=0.0
		IF NOT FINITE(lambda) THEN BEGIN
			msgwin,'Bad wavelength."'+lambdastr[0]+'".'+$
			STRING([10B,10B])+'Wavelength should be entered as a floating'+$
			' point in Angstroms. Obviously, wavelength>0.0.',/ERROR
			RETURN
		END

; Get the monochromator angle (exactly same procedure as for wavelength above)
		mcr_ang = getfloat(mcrang[0],MIN_VALUE=1e-10)

		IF NOT FINITE(mcr_ang) THEN BEGIN
			msgwin,'Bad monochromator angle."'+mcrang[0]+'".'+$
			STRING([10B,10B])+'Monochromator angle should be entered as a floating'+$
			' point in degrees (>0.0 deg.).',/ERROR
			RETURN
		END

; Get the percentage d-spacing of the monochromator
		mcr_dspread=getfloat(mcrdspread[0],MIN_VALUE=1e-10)
	
		IF NOT FINITE(mcr_dspread) THEN BEGIN
			msgwin,'Bad d-spacing spread of monochromator."'+mcrdspread[0]+'".'+$
			STRING([10B,10B])+'Fractional d-spacing spread should be entered as a floating'+$
			' point in percent (>0.0%).',/ERROR
			RETURN
		END

; Get foreground width return if error
		foreground=getint(foreground[0],'Foreground width',MAX_VALUE=100)
		IF foreground EQ -1 THEN RETURN

; get background left width and shift formatted as "[width[,shift]]"
; (where [] brackets indicate that the field can be omitted. So shift
; can be omitted and so can width, but there can be no shift if width is omitted
; look for a comma
		commapos = STRPOS(bgleft[0],',')
		CASE commapos OF
; No commas -> check only for width (may also be empty and therefore 0)
; set shift to 0
			-1:BEGIN
				bgleft_width = getint(bgleft[0],'Background left (width)'$
					,MAX_VALUE=100,/ALLOW_EMPTY)
				IF bgleft_width EQ -1 THEN RETURN
				bgleft_shift = 0
				END
; comma in 1st position -> ERROR!
			0:BEGIN
				msgwin,'Badly formatted background left.'+$
					STRING([10B,10B])+'Comma in illegal position. Must be formatted as [width,[shift]] where brackets indicate optional parameters.',/ERROR
				RETURN
				END
; otherwise if comma is in okay position then get both width and shift
			ELSE: BEGIN
				bgleft_width = getint(STRMID(bgleft[0],0,commapos),'Background left (width)',MAX_VALUE=100,/ALLOW_EMPTY)
				IF bgleft_width EQ -1 THEN RETURN
				bgleft_shift = getint(STRMID(bgleft[0],commapos+1,STRLEN(bgleft[0])-commapos-1),'Background left (shift)',MAX_VALUE=100,/ALLOW_EMPTY)
				IF bgleft_shift EQ -1 THEN RETURN
				IF (bgleft_width EQ 0) AND (bgleft_shift GT 0) THEN BEGIN
					msgwin,'Background left: Zero width, but non zero shift! A width must be defined if there is to be a shift.',/ERROR
					RETURN
				END
			END
		ENDCASE

;Now do exactly the same for the right background.
; get background right width and shift formatted as "[width[,shift]]"
; (where [] brackets indicate that the field can be omitted. 
; look for a comma
		commapos = STRPOS(bgright[0],',')
		CASE commapos OF
; No commas -> check only for width (may also be empty and therefore 0)
; set shift to 0
			-1:BEGIN
				bgright_width = getint(bgright[0],'Background right (width)'$
					,MAX_VALUE=100,/ALLOW_EMPTY)
				IF bgright_width EQ -1 THEN RETURN
				bgright_shift = 0
				END
; comma in 1st position -> ERROR!
			0:BEGIN
				msgwin,'Badly formatted background right.'+$
					STRING([10B,10B])+'Comma in illegal position. Must be formatted as [width,[shift]] where brackets indicate optional parameters.',/ERROR
				RETURN
				END
; otherwise if comma is in okay position then get both width and shift
			ELSE: BEGIN
				bgright_width = getint(STRMID(bgright[0],0,commapos),'Background right (width)',MAX_VALUE=100,/ALLOW_EMPTY)
				IF bgright_width EQ -1 THEN RETURN
				bgright_shift = getint(STRMID(bgright[0],commapos+1,STRLEN(bgright[0])-commapos-1),'Background right (shift)',MAX_VALUE=100,/ALLOW_EMPTY)
				IF bgright_shift EQ -1 THEN RETURN
				IF (bgright_width EQ 0) AND (bgright_shift GT 0) THEN BEGIN
					msgwin,'Background right: Zero width, but non zero shift! A width must be defined if there is to be a shift.',/ERROR
					RETURN
				END

			END
		ENDCASE
	
;-------------------------------------
; Now that options have been checked	
; write them to mono_defaults.dat
;-------------------------------------

; attempt to open file
		CLOSE,10
		OPENW,10,'mono_defaults.dat',ERROR=err

; catch write errors
		Error_status=0
		CATCH,Error_status

; This begins the error handler for Error_status (write errors) and err (open
; errors). It sets err to non-zero at the end to signal that the no attempt
; should be made to write to the file
		IF (err NE 0) OR (Error_Status NE 0) THEN BEGIN
; print out warning message
			PRINT,'Warning: Unable to write programme settings to mono_defaults.dat'
			PRINT,'Error:',!ERR_STRING
			PRINT,'This is not a critical error, calculations will proceed.'
; set error flag to non-zero value (1) (so it won't try to write to file)
; (see if statement below)
			err=1
		ENDIF

; only attempt to write if err is zero (no error)
		IF (err EQ 0) THEN BEGIN
			PRINTF,10,'D17 Monochromatic analysis defaults file'
			PRINTF,10,'Direct beam: 1st Numor'
			PRINTF,10,STRCOMPRESS(STRING(dbfirst),/REMOVE_ALL)
			PRINTF,10,'Direct beam: Last Numor'
			PRINTF,10,STRCOMPRESS(STRING(dblast),/REMOVE_ALL)
			PRINTF,10,'Reflected beam: 1st Numor'
			PRINTF,10,STRCOMPRESS(STRING(reffirst),/REMOVE_ALL)	
			PRINTF,10,'Reflected beam: Last Numor'
			PRINTF,10,STRCOMPRESS(STRING(reflast),/REMOVE_ALL)
			PRINTF,10,'Water (efficiency) file'
			PRINTF,10,STRCOMPRESS(STRING(fwater),/REMOVE_ALL)
			PRINTF,10,'Wavelength (Angstroms)'
			PRINTF,10,STRCOMPRESS(STRING(lambda),/REMOVE_ALL)
			PRINTF,10,'Monochromator angle (degrees)'
			PRINTF,10,STRCOMPRESS(STRING(mcr_ang),/REMOVE_ALL)
			PRINTF,10,'Output file name'
			PRINTF,10,STRCOMPRESS(output[0],/REMOVE_ALL)
			PRINTF,10,'Foreground width'
			PRINTF,10,STRCOMPRESS(STRING(foreground),/REMOVE_ALL)
			PRINTF,10,'Background left (width,shift)'
			PRINTF,10,STRCOMPRESS(STRING(bgleft_width)+','+STRING(bgleft_shift),/REMOVE_ALL)
			PRINTF,10,'Background right (width,shift)'
			PRINTF,10,STRCOMPRESS(STRING(bgright_width)+','+STRING(bgright_shift),/REMOVE_ALL)
			PRINTF,10,'Data directory'
			PRINTF,10,STRCOMPRESS(dir[0],/REMOVE_ALL)
			PRINTF,10,'Percentage spread in d-spacing of monochromator'
			PRINTF,10,STRCOMPRESS(STRING(mcr_dspread),/REMOVE_ALL)
			PRINTF,10,'Polarisation option'
			PRINTF,10,STRCOMPRESS(STRING(pol_opt),/REMOVE_ALL)
			PRINTF,10,'Normalisation option'
			PRINTF,10,STRCOMPRESS(STRING(norm_opt),/REMOVE_ALL)
			PRINTF,10,'Background subtraction method'
			PRINTF,10,STRCOMPRESS(STRING(bg_opt),/REMOVE_ALL)
			PRINTF,10,'Q calculation method'
			PRINTF,10,STRCOMPRESS(STRING(qcalc_opt),/REMOVE_ALL)
		ENDIF
		CLOSE,10
		CATCH,/CANCEL

; make a structure containing user input parameters to pass to analysis
		user = {userinput,db_first:dbfirst,db_last:dblast,ref_first:reffirst,$
		ref_last:reflast,waterfile:fwater,wavelength:lambda,$
		outputfile:output[0],datadir:dir,fgwidth:foreground,bgleft:[bgleft_width,$
		bgleft_shift],bgright:[bgright_width,bgright_shift],monochr_angle:mcr_ang,$
		monochr_dspread:mcr_dspread,pol:pol_opt,norm:norm_opt,bgsub:bg_opt,$
		qcalc:qcalc_opt}

; call main analysis routine
		analysis,user

; resensitize base
		WIDGET_CONTROL,anal_base,SENSITIVE=1
		END

END

;===============================================================================
; Main procedure "PRO mono". Loads defaults file. Displays main window.
; 'nuff said.
;===============================================================================
PRO d17mono

; Common block for base ids
COMMON baseid,mono_base,file_base,anal_base

; common block for button id's
COMMON buttons,	butt_quit, butt_anal, butt_reset,butt_pixels

; common blocks for all of the user entered parameters
COMMON textid,text_db_first,text_db_last,text_water,text_lambda,text_output,$
	text_ref_first,text_ref_last,text_foreground,text_bg_left,text_bg_right,$
	text_dir,text_mcr_ang,text_mcr_dspread
COMMON buttid,butt_polarise,butt_normalise,butt_bg,butt_qcalc

; common block for pointers to data.
COMMON loaded_data,db_data,ref_data,water

COMMON status,loadedflag,label_status

; Common block containing useful area of detector
COMMON detector,xmin,xmax,refpmean

;----------------------------------
; Initialisation

; Initialise useful area of detector as 0 (default)
	xmin = 0 & xmax = 0

; refpmean is an array containing the pixel indices of where the specular 
; peak is near. The centre of mass calculation is performed about this
; pixel. NB one pixel for each of the 4 (possible) polarisations
	refpmean=FLTARR(4)
	refpmean[*]=0.0

; Initialise data pointers (stored in common block) as NULL pointers
	db_data = PTR_NEW()
	ref_data = PTR_NEW()
	water = PTR_NEW()

; Set loadedflag to zero (false, no data loaded)
	loadedflag=0

; Gay welcome message
	PRINT,''
	PRINT,'Starting mono.pro. The monochromatic reflectivity analysis programme.'
	PRINT,' * Loading defaults...'

;------------------------------------
; Open defaults file:

	CLOSE,10
	OPENR,10,'mono_defaults.dat',ERROR=err
	
; strings for text widget defaults
	defaults=STRARR(13)
	defaults[*]='         '
; defaults for buttons
	butt_defaults=INTARR(4)
; string for spacers to disregard
	spacer=''
; temp. string to read lines from file
	temp=''

; Error handler:
	Error_status=0
	CATCH,Error_status
; If there was an error used following 'hardwired' defaults
	IF (Error_status NE 0) OR (err NE 0) THEN BEGIN
		PRINT,'Error loading defaults file:',!ERR_STRING
		PRINT,'Setting hardwired defaults.'
; ***********CHANGE HARDWIRED DEFAULTS HERE**************
; (These are only used when there is no defaults file)

; direct beam 1st and last numors
		defaults[0]=''
		defaults[1]=''
; reflected beam 1st and last numors
		defaults[2]=''
		defaults[3]=''
; water file
		defaults[4]=''
; wavelength 
		defaults[5]='5.0'
; monochromator angle (degrees)
		defaults[6]='2.0'
; output file
		defaults[7]='mono_output.dat'
; Foreground
		defaults[8]=''
; Background left and right
		defaults[9]=''
		defaults[10]=''
; data directory
		defaults[11]=''
; percentage spread in d-spacing of monochromator
		defaults[12]='4.0'
; Default buttons, all first choice (0):
		butt_defaults[0]=0 & butt_defaults[1]=0 &butt_defaults[2]=0 & butt_defaults[3]=0
; If an error occured set err to non-zero so it won't try to read the file again
; (see next if statement)
		err=1
	ENDIF

; Try to read the default file if it was successfully opened
	IF err EQ 0 THEN BEGIN
; catch any input errors (e.g. premature end of file)	
;read title line-discard
		READF,10,spacer
		FOR i=0,N_ELEMENTS(defaults)-1 DO BEGIN
; read the spacer
			READF,10,spacer,FORMAT='(A)'
; Can't simply read into defaults[i] because when passed to READF this is
; only a copy. Must use a temp variable (see p177 Building IDL....)
			READF,10,temp,FORMAT='(A)'
			defaults[i]=temp
		ENDFOR
; read default for each button widget
; (now define temp as integer)
		temp=0
		FOR i=0,N_ELEMENTS(butt_defaults)-1 DO BEGIN
			READF,10,spacer
; ditto here, as above
			READF,10,temp
			butt_defaults[i]=FIX(temp)
; Just in case the file is edited and a negative option is put in
; (more checks on butt_defaults are made below)
			IF butt_defaults[i] LT 0 THEN butt_defaults[i]=0
		ENDFOR
; If button default is outside range then set to hardwired default (0)
		IF butt_defaults[0] GT 3 THEN butt_defaults[0]=0
		IF butt_defaults[1] GT 1 THEN butt_defaults[1]=0
		IF butt_defaults[2] GT 1 THEN butt_defaults[2]=0
		IF butt_defaults[3] GT 1 THEN butt_defaults[3]=0
	ENDIF
	
	CLOSE,10	

;end error handling
	CATCH,/CANCEL

;-----------------------------------------------------------
; Below is thc code displaying the main window. The base widget is mono_base
; all other base widgets are simply to put pretty frames around text fields.
; The variable dummy is used as a widget ID for non-interactive widgets
; such as labels and spaces and 'null' widgets (which just pad out columns)
; The only important things are the text widgets "text_[name]" which are
; the IDs for the text entry fields. butt_[name] are buttons. The window
; is arranged into 3 columns and has a total y pixel size of 500
	mono_base = WIDGET_BASE(TITLE = 'D17 : Monochromatic Analysis',/COLUMN)

	dummy = WIDGET_LABEL(mono_base,VALUE='DATA FIELDS')

;Base widget for files
	file_base = WIDGET_BASE(mono_base,COLUMN=3,/FRAME)

; -------------1st Column for file_base
; Direct beam entry stuff
	dummy = WIDGET_LABEL(file_base,VALUE='Direct Beam')
	db_base = WIDGET_BASE(file_base,/FRAME,COLUMN=2,/BASE_ALIGN_CENTER)
	dummy = WIDGET_LABEL(db_base,VALUE='1st Numor',YSIZE=35)
	dummy = WIDGET_LABEL(db_base,VALUE='Last Numor',YSIZE=35)
	text_db_first = WIDGET_TEXT(db_base,/EDITABLE,XSIZE=6,VALUE=defaults[0],SCR_YSIZE=35)
	text_db_last = WIDGET_TEXT(db_base,/EDITABLE,XSIZE=6,VALUE=defaults[1],SCR_YSIZE=35)

	dummy = WIDGET_LABEL(file_base,VALUE='Data directory',SCR_YSIZE=35,/ALIGN_RIGHT)
	dummy = WIDGET_LABEL(file_base,VALUE='Water file',SCR_YSIZE=35,/ALIGN_RIGHT)

; -------------2nd Column for file_base
; REflected beam entry stuff
	dummy = WIDGET_LABEL(file_base,VALUE='Reflected Beam')
	ref_base = WIDGET_BASE(file_base,/FRAME,COLUMN=2,/BASE_ALIGN_CENTER)
	dummy = WIDGET_LABEL(ref_base,VALUE='1st Numor',YSIZE=35)
	dummy = WIDGET_LABEL(ref_base,VALUE='Last Numor',YSIZE=35)
	text_ref_first = WIDGET_TEXT(ref_base,/EDITABLE,XSIZE=6,VALUE=defaults[2],SCR_YSIZE=35)
	text_ref_last = WIDGET_TEXT(ref_base,/EDITABLE,XSIZE=6,VALUE=defaults[3],SCR_YSIZE=35)

;water file
	text_dir = WIDGET_TEXT(file_base,/EDITABLE,XSIZE=15,SCR_YSIZE=35,VALUE=defaults[11])
; Data dir. entry field
	text_water = WIDGET_TEXT(file_base,/EDITABLE,XSIZE=6,/ALIGN_LEFT,SCR_YSIZE=35,VALUE=defaults[4])	
; -------------3rd Column for file_base
; polarisation options
	butt_polarise = CW_BGROUP(file_base,['Unpolarised (1)','Polarised (2)',$
	'Analyser (3)','Analyser +- (4)'],/EXCLUSIVE,LABEL_TOP=$
	'Polarisation options',/FRAME, SET_VALUE=butt_defaults[0])

	dummy = WIDGET_LABEL(file_base,VALUE='',SCR_YSIZE=0)
	dummy = WIDGET_LABEL(file_base,VALUE='',SCR_YSIZE=0)
	dummy = WIDGET_LABEL(file_base,VALUE='',SCR_YSIZE=0)


	dummy = WIDGET_LABEL(mono_base,VALUE='',SCR_YSIZE=10)

	dummy = WIDGET_LABEL(mono_base,VALUE='STATUS')
	stat_base = WIDGET_BASE(mono_base,/FRAME)
	label_status = WIDGET_LABEL(stat_base,VALUE='No data loaded',/ALIGN_CENTER,/DYNAMIC_RESIZE)

	dummy = WIDGET_LABEL(mono_base,VALUE='',SCR_YSIZE=10)

; Base widget for analysis stuff
	dummy = WIDGET_LABEL(mono_base,VALUE='ANALYSIS FIELDS')
	anal_base=WIDGET_BASE(mono_base,COLUMN=3,/FRAME)

; -------------1st Column for anal_base

; normalisation options
	butt_normalise = CW_BGROUP(anal_base,['Monitor','Run time'],/EXCLUSIVE,$
	LABEL_TOP='Normalisation options',/FRAME, SET_VALUE=butt_defaults[1])

; q calc. options
	butt_qcalc = CW_BGROUP(anal_base,['Theta (calculated)','SAN (Instrument)'],/EXCLUSIVE,LABEL_TOP=$
	'Calculate q from:',/FRAME, SET_VALUE=butt_defaults[3])

;wavelength
	dummy = WIDGET_LABEL(anal_base,VALUE='Wavelength (Angst.)')
	text_lambda = WIDGET_TEXT(anal_base,/EDITABLE,XSIZE=5,VALUE=defaults[5])

;output file name
	dummy = WIDGET_LABEL(anal_base,VALUE='Output file')
	text_output = WIDGET_TEXT(anal_base,/EDITABLE,XSIZE=12,VALUE=defaults[7])

; -------------2nd Column for anal_base	


;Foreground
	fg_base = WIDGET_BASE(anal_base,/ROW)
	dummy = WIDGET_LABEL(fg_base,VALUE='Foreground')
	text_foreground = WIDGET_TEXT(fg_base,/EDITABLE,XSIZE=4,VALUE=defaults[8])

	dummy = WIDGET_LABEL(anal_base,VALUE='',YSIZE=0)
	dummy = WIDGET_LABEL(anal_base,VALUE='',YSIZE=0)

; Background
	bg_base = WIDGET_BASE(anal_base,/FRAME,/COLUMN)	
	dummy = WIDGET_LABEL(bg_base,VALUE='Background')
	dummy = WIDGET_LABEL(bg_base,VALUE='Format:"[width[,shift]]"')
	dummy = WIDGET_LABEL(bg_base,VALUE='',YSIZE=5)
	leftbg_base = WIDGET_BASE(bg_base,/ROW)
	dummy = WIDGET_LABEL(leftbg_base,VALUE='Left: ',/ALIGN_RIGHT)
	text_bg_left = WIDGET_TEXT(leftbg_base,/EDITABLE,XSIZE=7,VALUE=defaults[9])
	rightbg_base = WIDGET_BASE(bg_base,/ROW)
	dummy = WIDGET_LABEL(rightbg_base,VALUE='Right: ',/ALIGN_RIGHT)
	text_bg_right = WIDGET_TEXT(rightbg_base,/EDITABLE,XSIZE=7,VALUE=defaults[10])
	dummy = WIDGET_LABEL(anal_base,VALUE='',YSIZE=0)
; Background subtraction method selection
	butt_bg = CW_BGROUP(anal_base,['Average','Fit'],/EXCLUSIVE,$
	LABEL_TOP='Backgroung subtract',/FRAME,SET_VALUE=butt_defaults[2])

; -------------3rd Column for anal_base	

;monochromator base
	dummy = WIDGET_LABEL(anal_base,VALUE='Monochromator')
	mcr_base = WIDGET_BASE(anal_base,/COLUMN,/FRAME)
	dummy = WIDGET_LABEL(mcr_base,VALUE='Angle (deg.)',/ALIGN_CENTER)
	text_mcr_ang = WIDGET_TEXT(mcr_base,/EDITABLE,XSIZE=5,VALUE=defaults[6],/ALIGN_CENTER)
	dummy = WIDGET_LABEL(mcr_base,VALUE='Fractional d-spread (%)',/ALIGN_CENTER)	
	text_mcr_dspread = WIDGET_TEXT(mcr_base,/EDITABLE,XSIZE=5,VALUE=defaults[12],/ALIGN_CENTER)

	dummy = WIDGET_LABEL(anal_base,VALUE='',YSIZE=0)

;Analyse button
	dummy = WIDGET_LABEL(anal_base,VALUE='',YSIZE=10)
; base widget for buttons
	butt_base = WIDGET_BASE(anal_base,/COLUMN)
	butt_anal = WIDGET_BUTTON(butt_base,VALUE='ANALYSE')

; pixel ranges button
	butt_pixels = WIDGET_BUTTON(butt_base,VALUE='  PEAKS/RANGES  ')
; Reset Button
	butt_reset = WIDGET_BUTTON(butt_base,VALUE='RESET')

; "QUIT" button
	butt_quit = WIDGET_BUTTON(butt_base,VALUE='QUIT')


; Realise main window and register event handler PRO mono_event
	WIDGET_CONTROL,mono_base,/REALIZE
	XMANAGER,'mono',mono_base
END
