;information file on how to enter data is tofinfo.inf
; pro tof    ;;;look for the main program at the bottom!!

;====================================================================================================
; This function takes a string as input an extracts the numors an returns an array of numors. Errors
; are signalled by the flag 'eflag': If this is 0 no error has occured, If it is 1 an error occured.
; The string should be formatted as follows: Individual numors should be punctuated by a plus sign (+)
; ranges of numors can be specified by the min/max numors separated by a comma.
; e.g. 1+5+10,13+20 gives the array of numors: [1,5,10,11,12,13,20]
;====================================================================================================
function getnumors,intlist

; Common block for error flag
common EFLAGS,eflag
	
; array to store numors
	numors=lonarr(1000)
; integer of numor counts
	numor_index=0
	
;convert string to bytes, removing all whitespace add 0 'terminator'
	numlist=[byte(strcompress(intlist,/REMOVE_ALL)),0B]
;index of numlist
	string_index=0
; temporary storage space for an integer of up to 6 digits
	thisnum=bytarr(6)
; index for thisnum-counts number of digits in number	
	count=0
; If true (1) this varaible indicates that the last punctuation was a comma
	comma=0
	
;----------------------------------------------------
; Main loop - loops over string (numlist). Terminates
; when 0 (null) terminator is encountered	
	repeat begin

; read in character
		temp=numlist[string_index]
; increment string index
		string_index=string_index+1
		
; Case block: determines character. If a digit is encountered then it is stored
; in a temporary array until punctuation is encountered. IF a plus is encountered
; then this numor is stored in the list of numors array. If a comma is encountered
; then the number is stored temporarily until the next number (and the following plus
; or terminator) is read, when a range of numors is generated
		case 1 of

;>>>>>>>>> Case: Any ASCII digit: '0' through '9'
; insert digit into thisnum: temporary storage array
		(temp ge 48B) and (temp le 57B): begin	
; If number of digits exceeds 6 then signal numor too big error
			if count eq 6 then begin
				eflag=2
				return,0
			end
; if not then insert digit
			thisnum[count]=temp
; increment counter
			count=count+1
		end
				
;>>>>>>>> Case: '+' or null terminator
; In this case add the previous numor to the list of numors
; or, if there was a comma, add a numor range
		(temp eq 43B) or (temp eq 0B): begin
; If count(=number of digits) is zero then there is no number between punctuation!
			if count eq 0 then begin
; Allow for empty strings -ie only terminator. In which case return special error.
				if (temp eq 0B) and (numor_index eq 0) then begin 
					eflag=4
					return,0
				endif else begin
; otherwise, bog standard formatting cock-up (error 1)
					eflag=1
					return,0
				end
			end
			
; Convert digits stored in thisnum to intger
			rend=long(string(thisnum))

; if value of numor is zero ('0') then signal error
			if rend eq 0 then begin
				eflag=2
				return,0
			end

; If the last punctuation was a comma, then insert a range into the numors array
		if comma then begin
; if end of range is less than beginning then signal error
			if rend lt rbegin then begin
				eflag=5
				return,0
			end
; Put series of integers from rbegin (first numor) to rend (last numor)
			numors[numor_index:(numor_index+rend-rbegin)]=indgen(rend-rbegin+1)+rbegin
; update the numor counter appropriately
			numor_index=numor_index+rend-rbegin+1
; reset comma to false
			comma=0
; reset digit counter
			count=0
; flush string
			thisnum[*]=0
; If no comma then simply insert single numor into array
		endif else begin
			numors[numor_index]=rend
; increment numor index
			numor_index=numor_index+1
; reset counter for next numor
			count=0	
; fluch string
			thisnum[*]=0
			end
		end
		
;>>>>>>>>>> Case: comma ','
		temp eq 44B: begin
; If comma flag is already true (1) then signal formatting error
; (two commas in a row)
		if comma then begin
			eflag=1
			return,0
		end
; otherwise set comma flag to true (1)
		comma=1
; If count(=number of digits) is zero then there is no number
			if count eq 0 then begin
				eflag=1
				return,0
			end
		
; store thisnum (byte array of digits) into 'start of numor range' variable for later use
			rbegin=long(string(thisnum))
; reset digit count
			count=0
; flush string
			thisnum[*]=0
; if value of numor is zero ('0') then signal error
			if rbegin eq 0 then begin
				eflag=2
				return,0
			end
		end
		
; >>>>>>>>>> Case: Illegal character, send error message
		else: begin
			eflag=3
			return,0
		end
		endcase
; Loop back to top unless the terminator is encountered
	endrep until temp eq 0
	
; If we've got this far without returning then there's no error
	eflag=0
; return an array with the numors in
	return,numors[0	:numor_index-1]
end

;********************************************************************************
;converts text widgit strings to appropriate type and checks for errors
;defaults=0,1 according to whether 'def' was entered
;intcheck=0,1 according to whether an integer is expected
;commacheck=0,1,2 according to whether a comma is expected (2=either)
;strcheck=0,1 according to whether a string is expectec
;errcheck1=0 (false),1 (true) according to whether two numbers (ie a range of
; numors) is expected as opposed to just a single number
;errcheck2=0 (false),1 (true) according to whether the field must be completed 
; in order for the calculations to be done.

function convert,txtwid,defaults,intcheck,commacheck,strcheck,errcheck1,errcheck2
common par,par2
common var0,def
common varflag,flag0,flag1,flag2,flag3,flag4,flag5

; parameter to return is an array of 3 strings (possibly to be an array of 3 integers/floats)
valnum=strarr(3)

widget_control,txtwid,get_value=val

valstr=val(0)

valstr=strcompress(valstr,/remove_all)

;signal error flag if a 'def' is given but defaults aren't allowed
if (defaults eq 0) and (valstr eq def) then flag5=1
; if default is set then set valnum (return array) to 1	
if valstr eq def then valnum(0)=1 else $
; else if string_check is true (1) then set the return varaible to the string (valstr)
; NB: Isn't the expression (valstr ne def) redundant since it's already checked as a condition
; to get here, i.e. "if valstr eq def" above
if (valstr ne def) and (strcheck eq 1) then valnum(1)=valstr else begin
; If we're not looking for a string then do the other checks

; look for the position of THE comma
	commapos=strpos(valstr,',')

	if commapos eq -1 then begin
; If there are no commas, but a comma is required then flag an error
		if commacheck eq 1 then flag4=1

; convert the string (which should be a single number, no commas) into a float and
; an integer
		valflt=float(valstr)
		valint=fix(valstr)

; compare the string representation of the converted number. If this is not equal
; to the original string an error has occured 	
		if strpos(string(valflt),valstr) eq -1 then flag2=1
		
; if intcheck=1 (true) and the floating version isn't the same as the integer
; then its not an integer-> signal error flag
		if (intcheck eq 1) and (valint ne valflt) then flag3=1


; If an integer is required return the integer 
		if (intcheck eq 1) and (errcheck1 eq 1) then valnum=[0,valint,0]
; If an integer isn't required then return the floating point
		if (intcheck eq 0) and (errcheck1 eq 1) then valnum=[0,valflt,valflt]
; no range needed
		if (intcheck eq 0) and (errcheck1 eq 0) then valnum(1)=valflt
		if (intcheck eq 1) and (errcheck1 eq 0) then valnum(1)=valint

	endif else begin
; Commas have been found:

; If the comma occupies the first character then user has been painfully stupid.	
		if commacheck eq 0 then flag2=1

; sets valstr (a 3 string array) to: '0', the string between the first character and the comma, the
; string between after the comma and the end.
		valstr=['0',strmid(valstr,0,commapos),strmid(valstr,commapos+1)]

; make arrays of float and integers containing the respective versions of valstr.
		valflt=float(valstr)
		valint=fix(valstr)
	
; check the string versions of the float to the original string. If they aren't equal the user has not 
; entered a number. signal 'Not a number error'
		if (strpos(string(valflt(1)),valstr(1)) eq -1) or (strpos(string(valflt(2)),valstr(2)) eq -1) then flag2=1
; if intcheck=1 (true) and the integers aren't equal to the floats then they aren't integers. Signal 'not int' error
		if (intcheck eq 1) and ((valint(1) ne valflt(1)) or (valint(2) ne valflt(2))) then flag3=1
; If intcheck=1 (true) then return the integers, otherwise return the floats
		if intcheck eq 1 then valnum=valint else valnum=valflt

	endelse

; If errcheck=1 (true) then a range (two numbers) rather than a single number is required.
; This checks to ensure that the first number is smaller than the second, otherwise the
; range is invalid.
; Note the fudge: The range check is not required if integers are being read. This is for the 
; background shift/width which isn't a range. Numor (which are integer) ranges are dealt with by
; another procedure "getnumors"
	if (errcheck1 eq 1) and (intcheck eq 0) then begin

		if valnum(1) gt valnum(2) then flag0=1

	endif

; If errorcheck2=1 (true) then the field must contain a non-zero number. This is the case
; for Direct beam (1), Reflect (1), water, lambda, theta (1) and fac (1). Things like 
; Direct beam (2) are optional and only need to be specified if there is more than one 
; angle of reflection.
	if errcheck2 eq 1 then begin

		if valnum(1) eq 0 then flag1=1

	endif

endelse	

; returns result.
return,valnum

end


;*******************************************************************************************
;calculates factor by which to multiply refb so as to match refa

function factor,refa,refb,qa,qb,errora,errorb

common varbwid,base

xa=qa(sort(qa))
xb=qb(sort(qb))
ya=refa(sort(qa))
yb=refb(sort(qb))
errora=errora(sort(qa))
errorb=errorb(sort(qb))

xaovsub=where(xa ge min(xb))

range=max(xaovsub)-min(xaovsub)

xaov=xa(min(xaovsub):max(xaovsub))

dataa=ya(min(xaovsub):max(xaovsub))
erra=errora(min(xaovsub):max(xaovsub))

datab=fltarr(range)
err2=fltarr(range)
errb=fltarr(range)

numersum=0
denomsum=0

catch,errstat
if errstat ne 0 then begin
	eflag=8
	errormessage,'Incompatible data: check run set order'
	catch,/cancel
endif

for i=1,range-1 do begin
		
	pntabsub=min(where(xb ge xaov(i)))
	pntab=xb(pntabsub)
	databab=yb(pntabsub)
	errbab=errorb(pntabsub)
	pntbesub=max(where(xb lt xaov(i)))
	pntbe=xb(pntbesub)
	databbe=yb(pntbesub)
	errbbe=errorb(pntbesub)
	
	databdiff=databbe-databab

datab(i)=(((pntab-xaov(i))/databdiff)*databbe)+(((xaov(i)-pntbe)/databdiff)*databab)
		errb(i)=sqrt((((pntab-xaov(i))/databdiff)*errbbe)^2+(((xaov(i)-pntbe)/databdiff)*errbab)^2)
	
	if pntab-xaov(i) le xaov(i)-pntbe then begin
		datab(i)=yb(pntabsub)
		errb(i)=errorb(pntabsub)
	endif else begin
		datab(i)=yb(pntbesub)
		errb(i)=errorb(pntbesub)
	endelse
	
	if (dataa(i) gt 1.e-11) and (datab(i) gt 1.e-11) and (erra(i) gt 0.) and (errb(i) gt 0.) then err2(i)=(erra(i)+errb(i))*(erra(i)+errb(i)) else err2(i)=1.e20

	numer=(1/err2(i))*dataa(i)*datab(i)

	denom=(1/err2(i))*datab(i)*datab(i)

	numersum=numersum+numer
	denomsum=denomsum+denom

endfor

catch,/cancel

fac=numersum/denomsum
return,fac

end


;***************************************************************************************************************
;deals with events from TOF window ('pro tof')

pro tof_event,event
common par,par2
common var0,def
common varwid,lab,txt,but,gap
common varbwid,base
common varflag,flag0,flag1,flag2,flag3,flag4,flag5
common varfac,fac1,fac2,fac3
common varth,th1,th2,th3
common varset,norm,bg,r,fil
common varbg,bgrd
common varpath,path

; common block containing the time->lambda conversion array so it can be output
; to a file for each run
common varpar,tofd,opena,period,lamarr,nx,dett,s2,s3

; common block for useful xrange of detector and area to find peak in
common detranges,useful,peak_searchx,peak_searchy

; Error flag.
common EFLAGS,eflag

;takes the user value of the widget that cause the event and puts it equal to ev
widget_control,event.id,get_uvalue=ev

;closes all windows if 'quit' is pressed
if ev eq 'quit' then widget_control,base,/destroy

;begins anal2ysis of data if 'done' is pressed
if ev eq 'done' then begin

;flag*=error flags (0=no error, 1=error)
	flag0=0	
	flag1=0	
	flag2=0
	flag3=0
	flag4=0
	flag5=0
	flag6=0

;check*=flag for which run sets were used
	check0=0
	check1=0

;----*----*----*----*----*----*----*----*----*----*----*----*----*----*----
; Lots of repetative code to read in numors and output appropriate errors.
; (Should really be made a little more succinct by making a procedure
;----*----*----*----*----*----*----*----*----*----*----*----*----*----*----

; Read run 3 direct beam----------------------------
	widget_control,txt(2),get_value=val
	dtb3=getnumors(val(0))
; If eflag (error flag) is not zero or 4 (=empty array) then signal error
; empty arrays are allowed for runs 2 and 3 so long as both db and ref are empty
	if (eflag ne 0) and (eflag ne 4) then begin
		if eflag eq 1 then errormessage,'Illegal formatting:Run 3 Direct beam'
		if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Run 3 Direct beam'
		if eflag eq 3 then errormessage,'Illegal character: Run 3 Direct beam'
		if eflag eq 5 then errormessage,'Invalid range (end<beginning): Run 3 Direct beam'
		return
	end
		
; Read run 3 reflected beam-----------------------
	widget_control,txt(7),get_value=val
	refl3=getnumors(val(0))
;errors
	if (eflag ne 0) and (eflag ne 4) then begin
		if eflag eq 1 then errormessage,'Illegal formatting:Run 3 Reflected beam'
		if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Run 3 Reflected beam'
		if eflag eq 3 then errormessage,'Illegal character: Run 3 Reflected beam'
		if eflag eq 5 then errormessage,'Invalid range (end<beginning): Run 3 Reflected beam'
		return
	end
		
; If only 1 field is entered then signal error
	if ((dtb3[0] eq 0) and (refl3[0] ne 0)) or ((dtb3[0] ne 0) and (refl3[0] eq 0)) then begin 
	errormessage,'Incomplete fields:Run 3 (Enter both DB and REF or neither).'
	return
	end
	
; Read run 2 direct beam-------------------------
	widget_control,txt(1),get_value=val
	dtb2=getnumors(val(0))
; errors
	if (eflag ne 0) and (eflag ne 4) then begin
		if eflag eq 1 then errormessage,'Illegal formatting:Run 2 Direct beam'
		if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Run 2 Direct beam'
		if eflag eq 3 then errormessage,'Illegal character: Run 2 Direct beam'
		if eflag eq 5 then errormessage,'Invalid range (end<beginning): Run 2 Direct beam'
		return
	end
		
; Read run 2 reflected beam---------------------
	widget_control,txt(6),get_value=val
	refl2=getnumors(val(0))
;errors
	if (eflag ne 0) and (eflag ne 4) then begin
		if eflag eq 1 then errormessage,'Illegal formatting:Run 2 Reflected beam'
		if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Run 2 Reflected beam'
		if eflag eq 3 then errormessage,'Illegal character: Run 2 Reflected beam'
		if eflag eq 5 then errormessage,'Invalid range (end<beginning): Run 2 Reflected beam'
		return
	end
		
; If only 1 field is entered then signal error
	if ((dtb2[0] eq 0) and (refl2[0] ne 0)) or ((dtb2[0] ne 0) and (refl2[0] eq 0)) then begin 
	errormessage,'Incomplete fields:Run 2 (Enter both DB and REF or neither).'
	return
	end
	
; Read run 1 direct beam-----------------------
	widget_control,txt(0),get_value=val
	dtb1=getnumors(val(0))
; errors: note that eflag=4 (=no numors) is an error for run 1
	if eflag ne 0 then begin
		if eflag eq 1 then errormessage,'Illegal formatting:Run 1 Direct beam'
		if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Run 1 Direct beam'
		if eflag eq 3 then errormessage,'Illegal character: Run 1 Direct beam'
		if eflag eq 4 then errormessage,'Missing direct beam numors: Run 1'
		if eflag eq 5 then errormessage,'Invalid range (end<beginning): Run 1 Direct beam'
		return
	end
		
; Read run 1 reflected beam
	widget_control,txt(5),get_value=val
	refl1=getnumors(val(0))
; errors: note that eflag=4 (=no numors) is an error for run 1
	if eflag ne 0 then begin
		if eflag eq 1 then errormessage,'Illegal formatting:Run 1 Reflected beam'
		if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Run 1 Reflected beam'
		if eflag eq 3 then errormessage,'Illegal character: Run 1 Reflected beam'
		if eflag eq 4 then errormessage,'Missing reflected beam numors: Run 1'
		if eflag eq 5 then errormessage,'Invalid range (end<beginning): Run 1 Reflected beam'
		return
	end

; Read run 1 Instrument background
	widget_control,txt(10),get_value=val
	instbg1=getnumors(val(0))
;errors
	if (eflag ne 0) and (eflag ne 4) then begin
		if eflag eq 1 then errormessage,'Illegal formatting:Run 1 Instrument background'
		if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Run 1 Instrument background'
		if eflag eq 3 then errormessage,'Illegal character: Run 1 Instrument background'
		if eflag eq 5 then errormessage,'Invalid range (end<beginning): Run 1 Instrument background'
		return
	end
	
; Read run 2 Instrument background
	widget_control,txt(11),get_value=val
	instbg2=getnumors(val(0))
;errors
	if (eflag ne 0) and (eflag ne 4) then begin
		if eflag eq 1 then errormessage,'Illegal formatting:Run 2 Instrument background'
		if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Run 2 Instrument background'
		if eflag eq 3 then errormessage,'Illegal character: Run 2 Instrument background'
		if eflag eq 5 then errormessage,'Invalid range (end<beginning): Run 2 Instrument background'
		return
	end
; return error if Inst. BG set, but no direct beam/reflect runs set
	if (instbg2[0] ne 0) and (refl2[0] eq 0) then begin
		errormessage,'Inst. BG set but Direct beam/Reflected beam numors missing: Run 2.'
		return
	end
	
; Read run 3 Instrument background
	widget_control,txt(12),get_value=val
	instbg3=getnumors(val(0))
;errors
	if (eflag ne 0) and (eflag ne 4) then begin
		if eflag eq 1 then errormessage,'Illegal formatting:Run 3 Instrument background'
		if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Run 3 Instrument background'
		if eflag eq 3 then errormessage,'Illegal character: Run 3 Instrument background'
		if eflag eq 5 then errormessage,'Invalid range (end<beginning): Run 3 Instrument background'
		return
	end
; return error if Inst. BG set, but no direct beam/reflect runs set
	if (instbg3[0] ne 0) and (refl3[0] eq 0) then begin
		errormessage,'Inst. BG set but Direct beam/Reflected beam numors missing: Run 3.'
		return
	end	
; read water files
	widget_control,txt(3),get_value=val
; If 'def' is entered then set water=0 so that water file reading code knows to look
; for the default file
	if val[0] eq 'def' then water=0 else begin
		water=getnumors(val[0])
; errors: note that eflag=4 (=no numors) just means use default (signalled by 1st element of array 'water'
; being zero)
		if (eflag ne 0) and (eflag ne 4) then begin
			if eflag eq 1 then errormessage,'Illegal formatting:Water files'
			if eflag eq 2 then errormessage,'Invalid numor (0<numor<1000000):Water files'
			if eflag eq 3 then errormessage,'Illegal character:Water files'
			if eflag eq 5 then errormessage,'Invalid range (end<beginning): Water files'
		return
		end
	endelse
		
	lamda=convert(txt(4),1,0,2,0,1,1)

	path=convert(txt(8),0,0,0,1,0,0)
	
	fil=convert(txt(9),0,0,0,1,0,0)
	
	th1=convert(txt(19),1,0,0,0,0,0)
	th2=convert(txt(20),1,0,0,0,0,0)
	th3=convert(txt(21),1,0,0,0,0,0)
	
	r=convert(txt(13),0,1,0,0,0,0)
; check that foreground width isn't negative
	if r[1] lt 0 then begin
		errormessage,'Negative foreground width!'
		return
	end
	
;*******************************
; Quick sloppy fix coming up...
;*******************************

; convert returns either a single integer, or if a comma is supplied a 3 (for some reason...) array
; with the second and third elements containing the first (width) and second (shift) integers.
; Convert should really return a structure!!
	leftbg=convert(txt(14),0,1,2,0,1,0)
; Code below strips the leading 0 for a 3-array
	leftbg=[leftbg[1],leftbg[2]]
; Unfortunately have to do same sloppy manipulation for right hand side
	rightbg=convert(txt(18),0,1,2,0,1,0)
	rightbg=[rightbg[1],rightbg[2]]
; Now have two background arrays :leftbg[width,shift] rightbg[width,shift]. Concatenate
; these as single bg array to pass to anal2ysis
	bg=[leftbg,rightbg]
; bg[leftwidth,leftshift,rightwidth,rightshift]
; Yeuch.... that was messy

; check that all background shifts/widths to left and right are positive
	if (bg[0] lt 0) or (bg[1] lt 0) or (bg[2] lt 0) or (bg[3] lt 0) then begin
		errormessage,'Negative background width/shift!'
		return
	end
; Now do same for useful area of detector and peak search area:
	useful=convert(txt(22),0,1,2,0,1,0)
	useful=[useful[1],useful[2]]
	if (useful[0] lt 0) or (useful[1] gt 286) then begin
		errormessage,'Useful area of detector outside of physical size (0,286) of detector!'
		return
	end
	peak_searchx=convert(txt(23),0,1,2,0,1,0)
	peak_searchx=[peak_searchx[1],peak_searchx[2]]

	if (peak_searchx[0] lt useful[0]) or (peak_searchx[1] gt useful[1]) then begin
		errormessage,'Peak search x range is outside useful area of detector'
		return
	end

; if y range is set to "def" then set the range to -1,-1 so that the correct
; default range can be put in when the numors are read and their size is known
	widget_control,txt(24),get_value=temp
	if temp[0] EQ 'def' then peak_searchy=[-1,-1] else begin
		peak_searchy=convert(txt(24),0,1,2,0,1,0)
		peak_searchy=[peak_searchy[1],peak_searchy[2]]
		if (peak_searchy[0] lt 0) or (peak_searchy[1] gt 500) then begin
			errormessage,'Peak search y is outside y range (0,500)'
			return
		end
	end
	


	fac1=convert(txt(15),1,0,0,0,0,1)
	fac2=convert(txt(16),1,0,0,0,0,0)
	fac3=convert(txt(17),1,0,0,0,0,0)

;sets variable to the value of button widget in TOF window 
	widget_control,but(0),get_value=norm
	widget_control,but(1),get_value=bgrd


;if any errors in datat then appropriate message is displayed by calling 'pro errormessage'
	if flag0 eq 1 then errormessage,'Invalid ranges'
	if flag1 eq 1 then errormessage,'Incomplete fields'	
	if flag2 eq 1 then errormessage,'Field must be a number'	
	if flag3 eq 1 then errormessage,'Field must be integer'	
	if flag4 eq 1 then errormessage,'Comma expected'	
	if flag5 eq 1 then errormessage,'Default not available'	
	if flag6 eq 1 then errormessage,'Must complete all fields for given run set'	

;if there are no errors then proceed with anal2ysis
	if (flag0 eq 0) and (flag1 eq 0) and (flag2 eq 0) and (flag3 eq 0) and (flag4 eq 0) and (flag5 eq 0) and (flag6 eq 0) then begin

;renders TOF window insensitive
		widget_control,base,sensitive=0
		close,10

; write contents of text widgets (which have been checked above) into the default file tof_defaults.dat
		openw,10,'tof_defaults.dat'
		printf,10,'Direct beam runs (1)'
		widget_control,txt(0),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Direct beam runs (2)'
		widget_control,txt(1),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Direct beam runs (3)'
		widget_control,txt(2),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Reflection runs (1)'
		widget_control,txt(5),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Reflection runs (2)'
		widget_control,txt(6),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Reflection runs (3)'
		widget_control,txt(7),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Instrument background runs (1)'
		widget_control,txt(10),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Instrument background runs (2)'
		widget_control,txt(11),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Instrument background runs (3)'
		widget_control,txt(12),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Water (Efficiency) file'
		widget_control,txt(3),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Wavelength range'
		widget_control,txt(4),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Theta (1)'
		widget_control,txt(19),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Theta (2)'
		widget_control,txt(20),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Theta (3)'
		widget_control,txt(21),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Factor (1)'
		widget_control,txt(15),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Factor (2)'
		widget_control,txt(16),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Factor (3)'
		widget_control,txt(17),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Foreground range'
		widget_control,txt(13),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Left Background range
		widget_control,txt(14),get_value=tempstring
		printf,10,tempstring		
		printf,10,'Right Background range'
		widget_control,txt(18),get_value=tempstring
		printf,10,tempstring
		printf,10,'Normalisation method'
		printf,10,norm
		printf,10,'Method to determine background'
		printf,10,bgrd
		printf,10,'Output filename'
		widget_control,txt(9),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Data Path'
		widget_control,txt(8),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Useful x range on detector'
		widget_control,txt(22),get_value=tempstring		
		printf,10,tempstring
		printf,10,'X range to find peak in'
		widget_control,txt(23),get_value=tempstring		
		printf,10,tempstring
		printf,10,'Y range to find peak in'
		widget_control,txt(24),get_value=tempstring		
		printf,10,tempstring
		close,10
;if default was selected then use default water file, 'water_LAMPascii'
		if water[0] eq 0 then begin
		
			print,' '
			print,' '
			print,'Reading default Water file, ''water_LAMPascii''...'
			w3=make_array(3,286,type=4)

;check default file exists
			filcheck=findfile('water_LAMPascii',count=checkfil)
			if checkfil eq 0 then begin 
; Default water file does not exist
				print,'Default water file water_LAMPascii does not exist. No water correction!'
				w2=make_array(1,286,type=4)
				w2[*]=1.0
			endif else begin

			close,4
			openr,4,"water_LAMPascii"
     			readf,4,w3
;makes it 1-D array
			w2=w3(1,*)
			close,4
;			print,'min water=',min(w2),'max water= ',max(w2)
			end
		endif else begin
;otherwise call 'pro data_read2' to read water files entered
			print,' '
			print,' '
			print,'Reading Water files...'
			watersum=0
;loop data_read2 for each file and sum detector counts (water)

			for i=0,N_ELEMENTS(water)-1 do begin
				data_read2,water[i],wat
			; return if an error occured during reading
				if eflag ne 0 then return
				watersum=wat+watersum
			endfor
;call 'pro awater' to set the size of the array corresponding to the useful size of the detector
			awater,watersum,w2
		endelse
				
;run set (1) 
		print,' '
		print,' '
		print,'Reading data files for set (1)...'

;form summed direct beam array
		print,' '
		print,'for direct beam...'
		db1sum=0
		db1monsum=0
		db1timesum=0
;loop data_read2 for each file and sum detector counts (db*), monitor counts (db*mon) and runtime (db*time) also take detector angle (dbdan*)
		for i=0,N_ELEMENTS(dtb1)-1 do begin
			data_read2,dtb1[i],db1,db1mon,db1time,dbdan1
		; return if an error occured during reading
			if eflag ne 0 then return
			db1sum=db1+db1sum
			db1monsum=db1mon+db1monsum
			db1timesum=db1time+db1timesum
		endfor

;create error array for summed detector counts
		erdb1=sqrt(db1sum)

; output the time-> lambda conversion array
	CLOSE,10
	OPENW,10,'a1_lambda_convert.dat'
	PRINTF,10,'Pixel,   Wavelength (Angstroms)'
	FOR I=0,N_ELEMENTS(lamarr)-1 DO PRINTF,10,i,',',lamarr[i]
	CLOSE,10
		
;form summed reflected beam array
		print,' '
		print,'for reflected beam...'
		ref1sum=0
		ref1monsum=0
		ref1timesum=0
; store SAN angles into an array for comparison with calculated value
		san=fltarr(N_ELEMENTS(refl1))
;loop data_read2 for each file and sum detector counts (ref*), monitor counts (ref*mon) and runtime (ref*time) also take detector angle (refdan*)
		for i=0,N_ELEMENTS(refl1)-1 do begin
			data_read2,refl1[i],ref1,ref1mon,ref1time,refdan1
; store SAN (par2[2]) into sans array
			san[i]=par2[2]
		; return if an error occured during reading
			if eflag ne 0 then return
			ref1sum=ref1+ref1sum
			ref1monsum=ref1mon+ref1monsum
			ref1timesum=ref1time+ref1timesum
		endfor
;create error array for summed detector counts
		erref1=sqrt(ref1sum)

; If ther are instrument backgrounds to be subtracted then read and sum these
		if instbg1[0] ne 0 then begin
;form summed reflected beam array
		print,' '
		print,'for Instrument background...'
		instbg1sum=0
		instbg1monsum=0
		instbg1timesum=0
;loop data_read2 for each file and sum detector counts (ref*), monitor counts (ref*mon) and runtime (ref*time) also take detector angle (refdan*)
		for i=0,N_ELEMENTS(instbg1)-1 do begin
			print,instbg1
			data_read2,instbg1[i],instbgd1,instbg1mon,instbg1time,instbgdan1
	; return if an error occured during reading
			if eflag ne 0 then return
			instbg1sum=instbgd1+instbg1sum
			instbg1monsum=instbg1mon+instbg1monsum
			instbg1timesum=instbg1time+instbg1timesum
		endfor
;create error array for summed detector counts
		erinstbg1=sqrt(instbg1sum)
		end

; NORMALISE REFLECTED AND BACKGROUND RUNS TO THE DIRECT BEAM
		print,'Normalising....'		
;(norm=0) normalise summed reflected beam (normref*sum) to runtime 
		if norm eq 0 then begin
			normref1sum=ref1sum*(db1timesum/ref1timesum)
			normerref1sum=(db1timesum/ref1timesum)*erref1
; do same for Instrument backgrounds if they are present
			if instbg1[0] ne 0 then begin
				norminstbg1sum=instbg1sum*(db1timesum/instbg1timesum)
				normerinstbg1sum=(db1timesum/instbg1timesum)*erinstbg1
			end
		endif
;(norm=1) normalise summed reflected beam (normref*sum) to monitor 
		if norm eq 1 then begin
			normref1sum=ref1sum*(db1monsum/ref1monsum)
			normerref1sum=(db1monsum/ref1monsum)*erref1
; do same for Instrument backgrounds if they are present
			if instbg1[0] ne 0 then begin
				norminstbg1sum=instbg1sum*(db1monsum/instbg1monsum)
				normerinstbg1sum=(db1monsum/instbg1monsum)*erinstbg1
			end
		endif

; If there are instrument backgrounds to subtract then subtract the normalised sum and
; calculate the error.
		if instbg1[0] ne 0 then begin
			print,'Subtracting Normalised Instrument Backgrounds.....'
			normref1sum=normref1sum-norminstbg1sum
			normerref1sum=sqrt(normerref1sum^2+normerinstbg1sum^2)
			db1sum=db1sum-norminstbg1sum
			erdb1=sqrt(erdb1^2+normerinstbg1sum^2)
		end
		
		print,' '
		print,' '
		print,'anal2ysing data files for set (1)...'

;call 'pro anal2' to create Reflectivity data
;returns w*=Reflectivity, x*=q, e*=Reflectivity error, qe*=q resolution
		anal2,db1sum,erdb1,dbdan1,normref1sum,normerref1sum,refdan1,w2,r(1),bg,w5,x5,e5,qe5,lamda,th1	
; if an error was signalled then restart
		if eflag ne 0 then return
;if default factor was selected then use fac1=1
		if fac1(0) eq 1 then fac1(1)=1.

;check that calculated value of theta agrees with SAN 
		for i=0,N_ELEMENTS(san)-1 do begin
			if (san[i]-th1[1] gt 0.1) or (san[i]-th1[1] lt -0.1) then begin
				eflag=0
		errormessage,'Warning: Run 1:Difference between calculated theta and SAN of numor '+strtrim(string(refl1[i]),2)+' is greater than 0.1'
			end
		endfor
		
		print,' '
		print,'...done set (1)'

;multiply Ref and Referr by fac
		w5=fac1(1)*w5
		e5=fac1(1)*e5

;find number of points
		z5=size(e5)

;sort data in order of increasing q and call arrays *s
		xs=x5(sort(x5))
		ws=w5(sort(x5))
		es=e5(sort(x5))
		qes=qe5(sort(x5))

;if 2 run sets were entered then 1st element of dtb2 is not zero
		if dtb2[0] ne 0 then begin

;run set (2) 
			print,' '
			print,' '
			print,'Reading data files for set (2)...'
				
			print,' '
			print,'for direct beam...'
			db2sum=0
			db2monsum=0
			db2timesum=0
			for i=0,N_ELEMENTS(dtb2)-1 do begin
				data_read2,dtb2[i],db2,db2mon,db2time,dbdan2
			; return if an error occured during reading
				if eflag ne 0 then return
				db2sum=db2+db2sum
				db2monsum=db2mon+db2monsum
				db2timesum=db2time+db2timesum
			endfor
			erdb2=sqrt(db2sum)

; output the time-> lambda conversion array
		CLOSE,10
		OPENW,10,'a2_lambda_convert.dat'
		PRINTF,10,'Pixel,   Wavelength (Angstroms)'
		FOR I=0,N_ELEMENTS(lamarr)-1 DO PRINTF,10,i,',',lamarr[i]
		CLOSE,10

			print,' '
			print,'for reflected beam...'
			ref2sum=0
			ref2monsum=0
			ref2timesum=0
			
			san=fltarr(N_ELEMENTS(refl2))
			for i=0,N_ELEMENTS(refl2)-1 do begin
				data_read2,refl2[i],ref2,ref2mon,ref2time,refdan2
				san[i]=par2[2]
			; return if an error occured during reading
				if eflag ne 0 then return
				ref2sum=ref2+ref2sum
				ref2monsum=ref2mon+ref2monsum
				ref2timesum=ref2time+ref2timesum
			endfor
			erref2=sqrt(ref2sum)
			
; If ther are instrument backgrounds to be subtracted then read and sum these
			if instbg2[0] ne 0 then begin
;form summed reflected beam array
			print,' '
			print,'for Instrument background...'
			instbg2sum=0
			instbg2monsum=0
			instbg2timesum=0
;loop data_read2 for each file and sum detector counts (ref*), monitor counts (ref*mon) and runtime (ref*time) also take detector angle (refdan*)
			for i=0,N_ELEMENTS(instbg2)-1 do begin
				data_read2,instbg2[i],instbgd2,instbg2mon,instbg2time,instbgdan2
			; return if an error occured during reading
				if eflag ne 0 then return
				instbg2sum=instbgd2+instbg2sum
				instbg2monsum=instbg2mon+instbg2monsum
				instbg2timesum=instbg2time+instbg2timesum
			endfor
;create error array for summed detector counts
			erinstbg2=sqrt(instbg2sum)
			end

			print,'Normalising....'
			if norm eq 0 then begin
				normref2sum=ref2sum*(db2timesum/ref2timesum)
				normerref2sum=(db2timesum/ref2timesum)*erref2
; do same for Instrument backgrounds if they are present
				if instbg2[0] ne 0 then begin
					norminstbg2sum=instbg2sum*(db2timesum/instbg2timesum)
					normerinstbg2sum=(db2timesum/instbg2timesum)*erinstbg2
				end
			endif
			if norm eq 1 then begin
				normref2sum=ref2sum*(db2monsum/ref2monsum)
				normerref2sum=(db2monsum/ref2monsum)*erref2
; do same for Instrument backgrounds if they are present
				if instbg2[0] ne 0 then begin
					norminstbg2sum=instbg2sum*(db2monsum/instbg2monsum)
					normerinstbg2sum=(db2monsum/instbg2monsum)*erinstbg2
				end
			endif	
			
; If there are instrument backgrounds to subtract then subtract the normalised sum and
; calculate the error.
		if instbg2[0] ne 0 then begin
			print,'Subtracting Normalised Instrument Backgrounds.....'
			normref2sum=normref2sum-norminstbg2sum
			normerref2sum=sqrt(normerref2sum^2+normerinstbg2sum^2)
			db2sum=db2sum-norminstbg2sum
			erdb2=sqrt(erdb2^2+normerinstbg2sum^2)
		end

			print,' '
			print,' '
			print,'anal2ysing data files for set (2)...'

			anal2,db2sum,erdb2,dbdan2,normref2sum,normerref2sum,refdan2,w2,r(1),bg,w6,x6,e6,qe6,lamda,th2
; if an error was signalled then restart
			if eflag ne 0 then return

;check that calculated value of theta agrees with SAN 
		for i=0,N_ELEMENTS(san)-1 do begin
			if (san[i]-th2[1] gt 0.1) or (san[i]-th2[1] lt -0.1) then begin
				eflag=0
		errormessage,'Warning: Run 2:Difference between calculated theta and SAN of numor '+strtrim(string(refl2[i]),2)+' is greater than 0.1'
			end
		endfor
			
;if default factor was selected then call 'function factor' to match the Reflectivity curve from set (2) to that of set (1)
			if fac2(0) eq 1 then fac2(1)=factor(w5,w6,x5,x6,e5,e6)
;fac*=0 implies no overlap between the Ref curves
			if fac2(1) eq 0. then begin
				print,' '
				print,'No Overlap between (1) & (2)'
			endif
				
			print,' '
			print,'...done set (2)'

			w6=fac2(1)*w6
			e6=fac2(1)*e6
			z6=size(e6)

;create single arrays combining set (1) and (2) for Ref,q,Referr,qres
			x=fltarr(z5(1)+z6(1))
			w=fltarr(z5(1)+z6(1))
			e=fltarr(z5(1)+z6(1))
			qe=fltarr(z5(1)+z6(1))
			for i=0,z5(1)-1 do begin
				x(i)=x5(i)
				w(i)=w5(i)
				e(i)=e5(i)
				qe(i)=qe5(i)
			endfor
			for i=z5(1),z5(1)+z6(1)-1 do begin
				x(i)=x6(i-z5(1))
				w(i)=w6(i-z5(1))
				e(i)=e6(i-z5(1))
				qe(i)=qe6(i-z5(1))
			endfor
			
			xs=x(sort(x))
			ws=w(sort(x))
			es=e(sort(x))
			qes=qe(sort(x))
			
		endif
			
;if 3 run sets were entered then 1st element of dtb3 is not 0
		if dtb3[0] ne 0 then begin

;run set (3) 
			print,' '
			print,' '
			print,'Reading data files for set (3)...'

			print,' '
			print,'for direct beam...'
			db3sum=0
			db3monsum=0
			db3timesum=0
			for i=0,N_ELEMENTS(dtb3)-1 do begin
				data_read2,dtb3[i],dbd3,db3mon,db3time,dbdan3
			; return if an error occured during reading
				if eflag ne 0 then return
				db3sum=dbd3+db3sum
				db3monsum=db3mon+db3monsum
				db3timesum=db3time+db3timesum
			endfor
			erdb3=sqrt(db3sum)

; output the time-> lambda conversion array
			CLOSE,10
			OPENW,10,'a3_lambda_convert.dat'
			PRINTF,10,'Pixel,   Wavelength (Angstroms)'
			FOR I=0,N_ELEMENTS(lamarr)-1 DO PRINTF,10,i,',',lamarr[i]
			CLOSE,10

			print,' '
			print,'for reflected beam...'
			ref3sum=0
			ref3monsum=0
			ref3timesum=0
			
			san=fltarr(N_ELEMENTS(refl3))
			for i=0,N_ELEMENTS(refl3)-1 do begin
				data_read2,refl3[i],ref3,ref3mon,ref3time,refdan3
				san[i]=par2[2]
			; return if an error occured during reading
				if eflag ne 0 then return
				ref3sum=ref3+ref3sum
				ref3monsum=ref3mon+ref3monsum
				ref3timesum=ref3time+ref3timesum
			endfor
			erref3=sqrt(ref3sum)
			
; If ther are instrument backgrounds to be subtracted then read and sum these
			if instbg3[0] ne 0 then begin
;form summed reflected beam array
			print,' '
			print,'for Instrument background...'
			instbg3sum=0
			instbg3monsum=0
			instbg3timesum=0
;loop data_read2 for each file and sum detector counts (ref*), monitor counts (ref*mon) and runtime (ref*time) also take detector angle (refdan*)
			for i=0,N_ELEMENTS(instbg3)-1 do begin
				data_read2,instbg3[i],instbgd3,instbg3mon,instbg3time,instbgdan3
			; return if an error occured during reading
				if eflag ne 0 then return
				instbg3sum=instbgd3+instbg3sum
				instbg3monsum=instbg3mon+instbg3monsum
				instbg3timesum=instbg3time+instbg3timesum
			endfor
;create error array for summed detector counts
			erinstbg3=sqrt(instbg3sum)
			end

			print,'Normalising....'

			if norm eq 0 then begin
				normref3sum=ref3sum*(db3timesum/ref3timesum)
				normerref3sum=(db3timesum/ref3timesum)*erref3
; do same for Instrument backgrounds if they are present
				if instbg3[0] ne 0 then begin
					norminstbg3sum=instbg3sum*(db3timesum/instbg3timesum)
					normerinstbg3sum=(db3timesum/instbg3timesum)*erinstbg3
				end

			endif
			if norm eq 1 then begin
				normref3sum=ref3sum*(db3monsum/ref3monsum)
				normerref3sum=(db3monsum/ref3monsum)*erref3
; do same for Instrument backgrounds if they are present
				if instbg3[0] ne 0 then begin
					norminstbg3sum=instbg3sum*(db3monsum/instbg3monsum)
					normerinstbg3sum=(db3monsum/instbg3monsum)*erinstbg3
				end

			endif
			
; If there are instrument backgrounds to subtract then subtract the normalised sum and
; calculate the error.
		if instbg3[0] ne 0 then begin
			print,'Subtracting Normalised Instrument Backgrounds.....'
			normref3sum=normref3sum-norminstbg3sum
			normerref3sum=sqrt(normerref3sum^2+normerinstbg3sum^2)
			db3sum=db3sum-norminstbg3sum
			erdb3=sqrt(erdb3^2+normerinstbg3sum^2)
		end

			print,' '
			print,' '
			print,'anal2ysing data files for set (3)...'

			anal2,db3sum,erdb3,dbdan3,normref3sum,normerref3sum,refdan3,w2,r(1),bg,w7,x7,e7,qe7,lamda,th3
; if an error was signalled then restart
			if eflag ne 0 then return
			
;check that calculated value of theta agrees with SAN 
		for i=0,N_ELEMENTS(san)-1 do begin
			if (san[i]-th3[1] gt 0.1) or (san[i]-th3[1] lt -0.1) then begin
				eflag=0
		errormessage,'Warning: Run 3:Difference between calculated theta and SAN of numor '+strtrim(string(refl3[i]),2)+' is greater than 0.1'
			end
		endfor

;if default factor was selected then call 'function factor' to match the Reflectivity curve from set (3) to that of set (2)
			if fac3(0) eq 1 then fac3(1)=factor(w6,w7,x6,x7,e6,e7)
			if fac3(1) eq 0. then begin
				print,' '
				print,'No Overlap between (2) & (3)'
			endif

			print,' '
			print,'...done set (3)'

			w7=fac3(1)*w7
			e7=fac3(1)*e7
			z7=size(e7)

			x=fltarr(z5(1)+z6(1)+z7(1))
			w=fltarr(z5(1)+z6(1)+z7(1))
			e=fltarr(z5(1)+z6(1)+z7(1))
			qe=fltarr(z5(1)+z6(1)+z7(1))
			for i=0,z5(1)-1 do begin
				x(i)=x5(i)
				w(i)=w5(i)
				e(i)=e5(i)
				qe(i)=qe5(i)
			endfor
			for i=z5(1),z5(1)+z6(1)-1 do begin
				x(i)=x6(i-z5(1))
				w(i)=w6(i-z5(1))
				e(i)=e6(i-z5(1))
				qe(i)=qe6(i-z5(1))
			endfor
			for i=(z5(1)+z6(1)),(z5(1)+z6(1)+z7(1)-1) do begin
				x(i)=x7(i-z5(1)-z6(1))
				w(i)=w7(i-z5(1)-z6(1))
				e(i)=e7(i-z5(1)-z6(1))
				qe(i)=qe7(i-z5(1)-z6(1))
			endfor

			xs=x(sort(x))
			ws=w(sort(x))
			es=e(sort(x))
			qes=qe(sort(x))

		endif

;remove all bad points from the arrays and call arrays *c (corrected)
		ec=es(where(es))
		qec=qes(where(es))
		xc=xs(where(es))
		wc=ws(where(es))
		
;for the purposes of the output file: If default water file is specified then
; redefine water as a string containing the name of the default water file
		if water[0] eq 0 then water='water_LAMPascii'

;check that output file has a valid name
		catch,outfilcheck
		if outfilcheck ne 0 then begin
;set eflag to non-zero to signal error
			eflag=10
			errormessage,'Invalid output file name: '''+strcompress(string(fil(1)),/remove_all)+''''
			catch,/cancel
		endif

;print data info at top of output file
		close,10
		openw,10,fil(1),width=128
		printf,10,' db1:',dtb1
		printf,10,' ref1:',refl1,' th1:',th1(1),' fac1:',fac1(1)
; only print run 2 stuff if there actually is a run 2
		if dtb2[0] ne 0 then begin
			printf,10,' db2:',dtb2
			printf,10,' ref2:',refl2,' th2:',th2(1),' fac2:',fac2(1)
		end
; only print run3 stuff if there is a run 3
		if dtb3[0] ne 0 then begin
			printf,10,' db3:',dtb3
			printf,10,' ref3:',refl3,' th3:',th3(1),' fac3:',fac3(1)
		end
		printf,10,' water:',water,' lamda:',lamda(1),lamda(2),' fgrd:',r(1),' bgrd:',bg
		
 		close,10
		catch,/cancel
		
;run 'pro endmessage' giving the details of the calculated parameters
		endmessage

;run 'pro output' listing the arrays to the output file and plotting the full Reflectivity curve
		output,xc,wc,ec,qec,fil(1)

	endif		

endif

return

end



;************************************************************************************
;reads data for each file (num,int), gives det counts (w1), monitor counts (w2)
; runtime, detector angle (dan)

pro data_read2,num,w1,w2,runtime,dan


; takes a data numor and opens the file, reads paramter blocks and comes back with
; the lambda array for TOF (lamarr) the detector array (w1) the monitor array (w2)

common EFLAGS,eflag
common vars0,def
common varpar,tofd,opena,period,lamarr,nx,dett,s2,s3
common varpath,path
common par,par2

;reset error flag
eflag=0

close,3

; floating point arrays of 128 and 256 elements
par1 = fltarr(128)
par2 = fltarr(256)

; string array of 34 elements
txt=sindgen(34)
; set all 34 elements to XXX....
txt(*)='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
txt1=txt(1)

;converts num(int) to 6 digit string with trailing zeroes
name='000000'				;make six digit string of 0's
tempnum=strtrim(string(num),2)		;convert num to string and trim all blank spaces
strput,name,tempnum,6-strlen(tempnum)	;put these digits in occupying the last strlen(tempnum) places of name
name=path(1)+name			; include path in name

;check to see if numor exists
filcheck=findfile(name,count=checkfil)		
; If not signal error and return
if checkfil eq 0 then begin 
	eflag=7
	errormessage,'File not found: '+name
	return
end

; inform user of status and open file (handle=3)
openr,3,name
print,'opened: ',name

; read in 34 lines of text
readf,3,txt
; output 5th and 26th lines (text)
;print,txt(4)
;print,txt(25)

; read 128 floats (2nd block of numbers in file, 1st block is ignored by code above)
readf,3,par1
; read the two line spacer
readf,3,txt1
readf,3,txt1
;read 256 numbers 
readf,3,par2

; degrees per radian
dpr=180./!pi

;output useful information
;print,'no of chans= ',par1(94),' chan width= ',par1(95),' tof delay= ',par1(96)
;print,'x1= ',par1(97),' x2= ',par1(98),' y1= ',par1(99),' y2= ',par1(100)
nx=par1(101)

; print out chopper speeds and openings  nx and ny are the detector grouping factors
; ie base detector is 286*276 if nx=2 and ny=1 then detector is 144*276

;print,'nx= ',par1(101),' ny= ',par1(102)
;print,'chop 1 speed req= ',par2(40),' chop 1 phase req= ',par2(41)
;print,'chop 2 speed req= ',par2(42),' chop 2 phase req= ',par2(43)
;print,'chop 1 speed act= ',par2(44),' chop 1 phase act= ',par2(45)
;print,'chop 2 speed act= ',par2(46),' chop 2 phase act= ',par2(47)

; useful chopper variables openr is the requested chopper opening and opena is the read back value
openr=45.-(par2(43)-par2(41))
opena=45.-(par2(47)-par2(45))

;!chopper opening offset=1.02!
opena=opena-1.02

; chopper period in seconds
period=60./par2(44)

; chopper delay angle between pickup and centre of projected window
dela=(285.+0.9-opena)/2.

; delt is the chopper delay time in seconds
delt=(dela/360.)*period

; cht is hardwired chopper separation in m
cht=85.e-3

; chopsam is the sample to mid-chopper distance in meters
chopsam=4.0115-(cht)/2.

; chopmon is the monitor to detector distance
chopmon=.455

; tofd is the time- of flight distance from mid-chopper to detector par2(15) is
; the sample to detector distance in mm
tofd=chopsam+(par2(15)/1000.)

;chanpa is the number of time channels per angstrom for the detector (neutron wavelength) 
chanpa=(tofd/3956.)/(par1(95)*1.e-6)

;chanpam is the number of time channels per angstrom for the monitor (neutron wavelength
chanpam=(chopmon/3956.)/(par1(95)*1.e-6)

; r is the time grouping factor between the monitor and the detector (not used at present
;as we measure the direct beam on the detector and use the monitor just for normalisation

r=chopsam/tofd

; chopper delay in time channels
delchan=delt/(par1(95)*1e-6)

; electronic delay (set by par tof in MAD) in channels
delechan=par1(96)/par1(95)

;printing out useful stuff
;print,'TOF distance    = ',tofd, ' period chans    =',period/(par1(95)*1e-6)
;print,'channels/Ang    = ',chanpa,' chop del chans  = ',delchan
;print,'chopper period  = ',period*1000.,' elec del chans  = ',delechan
;print,'opening      req= ',openr,' opening      act= ',opena
;print,'chop delay angle= ',dela,' chop delay  time= ',delt*1000.,' ms'
;print,'     san= ',par2(2),' deg.'
;print,' 27A TOF        = ',27.*chanpa,' 4.5A TOF        = ',4.5*chanpa

runtime=par1(2)/10.

;print,'run time= ',runtime,' s',' det= ',par2(15),'dan = ',par2(16)

;dan is the detector angle and dett is the sample-detector distance
dan=par2(16)
dett=par2(15)

; Now we can read the data -first read 3 spacer lines
readf,3,txt1
readf,3,txt1
readf,3,txt1

;read total number pixels in detector
readf,3,tot

;set size of arrays (usually in TOF ysize is 1 i.e we have xpxels*time channels only)
tsize=long(par1(94))
xsize=long(par1(98)-par1(97)+1)
ysize=long(par1(100)-par1(99)+1)
dsize=xsize*ysize

;signal error if total number of pixels isn't equal to xsize*ysize
if (tot ne (dsize*tsize+tsize)) then print,' Error in data array dimensions'

;print,'tsize= ',tsize,' detector size= ',dsize
;print,'xsize= ',xsize,' ysize= ',ysize,' tot1= ',dsize*tsize,' tot2= ',tot

;set up an array of long integers with specified size
det=lonarr(ysize,xsize,tsize)
; usually pixels are summed in physical y direction of detector-> only 2d
if (ysize eq 1) then det=lonarr(xsize,tsize)

; set up monitor arrray. Monitor is basically an extra line of pixels at the end
; of the data
mon=lonarr(tsize)

;2x2 arrays of data
xy=lonarr(xsize,ysize)
xt=lonarr(xsize,tsize)
yt=lonarr(ysize,tsize)

c=1

;read detector data and monitor data from file
readf,3,det,mon

s2=par2(54)
s3=par2(50)

nx=par1(101)

;millimeters per pixel
mmpp=1.016*nx

;useful area of det in pixels
xminp=16
xmaxp=255
; convert to useful area in mm
xminmm=xminp*mmpp
xmaxmm=xmaxp*mmpp


;make lamda array- conversion between time and wavelength
yy=findgen(tsize)		;generates  floating point array with sequence 0.0,1.0,2.0, e.t.c
lamarr=(yy+delechan-delchan+0.5)/chanpa

; convert data to floating point in w1
w1=float(det)
; add up total monitor counts
w2=float(total(mon))

close,3
; print total monitor counts and monitor counts per unit time
;print,'total counts in detector = ',total(w1),' (',total(w1)/runtime,')'

return

end


;*********************************************************************************************************
;sets water file (w1) to 1 outside useful area of det and returns w2

pro awater,w1,w2

xsize=286
w2=fltarr(xsize)

;useful area of det (x,y) 37:238,30:248
x1=37 & x2=238
w2=float(total(w1,2))

; normalise water
w2=w2/(mean(w2(x1:x2)))
; set anything outside usefule area to 1
w2(0:x1-1)=1. & w2(x2+1:xsize-1)=1.
;print,'Max water=',max(w2),'Min water=',min(w2)
return

end

;***************************************************************************************************
;calculates theta

function tth,d0,p0,dr,pr,nx,det

dpr=180./!pi

pcen=139.81/nx
mmpp=1.016*nx

;print,' '
;print,'pcen= ',pcen
;print,'mmpp= ',mmpp
th=(dr+dpr*atan((pcen-pr)*mmpp/det))/2-(d0+dpr*atan((pcen-p0)*mmpp/det))/2
print,'calculated theta= ',th


return,th

end


;********************************************************************************************
;anal2yses data, db=direct beam counts, dber=error on direct beam counts, dbdan=detector angle for direct beam, ref=reflected beam counts, refer=error on reflected beam counts, refdan=detector angle for reflected beam, water=detector counts for watef, r=forground range, bg=background range, wdum=Reflectivity, xdum=q, edum=Reflectivity error, qedum=q resolution, lamda=lamda range, th=theta

pro anal2,db,dber,dbdan,ref,refer,refdan,water,rr,bg,wdum,xdum,edum,qedum,lamda,th

wdum=0
xdum=0
edum=0
qedum=0

; Common block for error flag
common EFLAGS,eflag
common par,par2
common vars0,def
common varpar,tofd,opena,period,lamarr,nx,dett,s2,s3
common varbg,bgrd

; common block for useful xrange of detector and area to find peak in
common detranges,useful,peak_searchx,peak_searchy

; reset error flag: no errors
eflag=0

; db is the direct beam run
; ref is the reflection run
; water is the x effiency of the detector
; q is the array of q(A^-1)
; rr is the range over which to sum the intensity
; bg is the range to sum the background either side of r
; w5 is the output reflectivity with the q (x5) and error (e4)
; fac is the normalisation factor for w2 reflection data ie
; divide the ref data by this to make it normalised to the direct beam

;print,'TOTALS REF DB',total(ref),total(db)


ref1=ref
ref2=ref
db1=db
db2=db
edb=db
ere=ref

; get useful area of detector from common block variable
xmax=useful[1]
xmin=useful[0]

mmpp=1.016*nx

catch,erranal2
if erranal2 ne 0 then begin
	errormessage,'Error anal2ysing data: check settings and try again'
	catch,/cancel
endif

;place info of array db (direct beam data) into z.
;z[0]=no dimensions, z[1]=1st dimension, z[2]=2nd dim, z[3]= type
; z[5]=tot. number of elements
z=size(db)
;print,' '
;print,'db size: ',z(2)
;print,'MEAN WATER',mean(water)

;water correction
for i=0,z(2)-1 do begin
	db1(*,i)=db(*,i)/water
	dber(*,i)=dber(*,i)/water
	ref1(*,i)=ref(*,i)/water
	refer(*,i)=refer(*,i)/water
endfor

;print,' '
;print,'done water correction'

;print,'TOTALS REF DB AFTER WATER',total(ref1),total(db1)

; limit area to search peak to user defined values
x1=peak_searchx[0] & x2=peak_searchx[1]

; if "def" is entered in y range then get the values from the size of the data
if peak_searchy[0] eq -1 then begin
	t1=fix(float(z[2])/2.) & t2=z[2]-1 
endif else begin
	t1=peak_searchy[0] & t2=peak_searchy[1]
end

;sum direct beam data along columns:
; restricted range sum
dbtot=total(db1(x1:x2,t1:t2),2)
; whole sum
dbtot2=total(db1,2)
; max. of direct beam data. pixel number is put in dbm
dbtotm=max(dbtot,dbm)
; add resttricted range offset to dbm
dbm=dbm+x1
;print,'direct beam peak at: ',dbm

;calculate summation ranges: rr=width of foreground sum
f1=dbm-fix((rr-1)/2)
f2=dbm+fix((rr-1)/2)
;print,'sum ranges for db: ',f1,f2

;Centre Of Mass calculation for db
numersum=0.
denomsum=0.
for i=f1,f2 do begin
	numer=dbtot2(i)*float(i)
	denom=dbtot2(i)
	numersum=numer+numersum
	denomsum=denom+denomsum
endfor
;print,'db stuff:', numersum,denomsum
dbcom=numersum/denomsum
;print,'fitted db peak at: ',(dbcom)

;check to see if peak value is near centre of mass if it isn't set it to
; the centre of mass
if fix(dbcom+0.5) ne dbm then dbm=fix(dbcom+0.5)

; redo foreground calculation
f1=dbm-fix((rr-1)/2)
f2=dbm+fix((rr-1)/2)

print,format='("Direct beam sum ranges: Foreground      :",3x,i3,1x,i3)',f1,f2
if bg[0] ne 0 then print,format='(24x,"Background left :",3x,i3,1x,i3)',f1-bg[0]-bg[1],f1-1-bg[1]
if bg[2] ne 0 then print,format='(24x,"Background right:",3x,i3,1x,i3)',f2+bg[2]+bg[3],f2+1+bg[3]
;print,'corrected db peak at: ',dbm

;  finding the peak in the reflection over a limited range 150:230 in x
;and 200 to the max in time channels

reftot=total(ref1(x1:x2,t1:t2),2)
reftot2=total(ref1,2)
reftotm=max(reftot,refm)
refm=refm+x1
;print,'peak in reflection at: ',refm


ff1=refm-fix((rr-1)/2)
ff2=refm+fix((rr-1)/2)
;print,'sum ranges for ref: ',ff1,ff2
;COM for ref
numersum=0.
denomsum=0.

for i=ff1,ff2 do begin
	numer=reftot2(i)*float(i)
	denom=reftot2(i)
	numersum=numer+numersum
	denomsum=denom+denomsum
endfor
refcom=numersum/denomsum
;print,'ref stuff:', numersum,denomsum
;print,'ref peak at: ',(refcom)
if fix(refcom+0.5) ne refm then refm=fix(refcom+0.5)
;print,'corrected ref peak at: ',refm
ff1=refm-fix((rr-1)/2)
ff2=refm+fix((rr-1)/2)


print,' '
print,format='("Ref.   beam sum ranges: Foreground      :",3x,i3,1x,i3)',ff1,ff2
if bg[0] ne 0 then print,format='(24x,"Background left :",3x,i3,1x,i3)',ff1-bg[0]-bg[1],ff1-1-bg[1]
if bg[2] ne 0 then print,format='(24x,"Background right:",3x,i3,1x,i3)',ff2+bg[2]+bg[3],ff2+1+bg[3]

;print,'ref peak at: ',(refcom)

;print,'f1 f2 ff1 ff2',f1,f2,ff1,ff2

; Check that background sum isn't outside range of detector. NB For left side bg[0] is width, bg[1] is shift from edge
; of foreground sum. Similar thing for right in bg[2] and bg[3]
if ((f1-bg[0]-bg[1]) lt xmin) or ((f2+bg[2]+bg[3]) gt xmax) or ((ff1-bg[0]-bg[1]) lt xmin) or ((ff2+bg[2]+bg[3]) gt xmax) then begin
  eflag=6
  errormessage,'Outside useful area of detector.Sum over smaller area'
  return
end



subarr=indgen(z(1))
dbb=fltarr(z(1),z(2))
rb=fltarr(z(1),z(2))
ed=fltarr(z(1),z(2))
eb=fltarr(z(1),z(2))
er=fltarr(z(1),z(2))

; bg is the range to sum background over, therefore if this is gt 0 (i.e. there is
; some background to sum over) then do the sum. NB bg[0] is width of left sum
; bg[2] is width of right sum
if (bg[0] gt 0) or (bg[2] gt 0) then begin
; bgrd is the background subtraction option. If this is zero then use summing method
	if bgrd eq 0 then begin
; loop over z[2]= columns of array db (direct beam)
		for i=0,z(2)-1 do begin
		; background of direct beam is mean of counts in sum range
; IF only left background then average only over left
		if (bg[0] gt 0) and (bg[2] eq 0) then begin 
			bkdb=mean(db1(f1-bg[0]-bg[1]:f1-1-bg[1],i))
			bkref=mean(ref1(ff1-bg[0]-bg[1]:ff1-1-bg[1],i))
		end
; IF only right background then average only over right
		if (bg[0] eq 0) and (bg[2] gt 0) then begin
			bkdb=mean(db1(f2+1+bg[3]:f2+bg[3]+bg[2],i))						
			bkref=mean(ref1(ff2+1+bg[3]:ff2+bg[3]+bg[2],i))
		end
;; IF both background then average over both
		if (bg[0] gt 0) and (bg[2] gt 0) then begin
			bkdb=(mean(db1(f1-bg[0]-bg[1]:f1-1-bg[1],i))+mean(db1(f2+1+bg[3]:f2+bg[3]+bg[2],i)))/2
			bkref=(mean(ref1(ff1-bg[0]-bg[1]:ff1-1-bg[1],i))+mean(ref1(ff2+1+bg[3]:ff2+bg[3]+bg[2],i)))/2
		end
		; subtract background from direct beam counts
				db2(*,i)=db1(*,i)-bkdb
		; ditto for the reflected beam
				ref2(*,i)=ref1(*,i)-bkref

				dbb(*,i)=sqrt(bkdb)
                                rb(*,i)=sqrt(bkref)
				
				ed(*,i)=sqrt(dber(*,i)^2+dbb(*,i)^2)
				er(*,i)=sqrt(refer(*,i)^2+rb(*,i)^2)

		endfor
      
	endif


; If bgrd=1 then the fitting option is chosen for background subtraction
	if bgrd eq 1 then begin
	
; note: Number of background points to use in fit = number at left+number at right=bg[0]+bg[2]
		n_bgpoints=bg[0]+bg[2]
		;subtract backgrd from db
		for i=0,z(2)-1 do begin


; dbgsuarr is an array that will contain the pixel numbers (on the detector) 
; of the pixels to be used in the fit
			dbbgsubarr=intarr(n_bgpoints)
; This array contains the VALUES of the pixels use for fitting
			dbbgarr=fltarr(n_bgpoints,z(2))
			dbbgfit=fltarr(z(1),z(2))

; Now we need the NUMBERS of the pixels that have been requested for use in the fit and also
; their VALUES:
; Subarr contains an array of pixel numbers for the whole detector. Set the pixel NUMBERS
; in dbgsubarr to the pixels selected for fitting. First the left side....
; (NB bg[0]=left width, bg[1]=left shift, f1=left side of foreground range)
			if bg[0] ne 0 then dbbgsubarr(0:bg[0]-1)=subarr(f1-bg[0]-bg[1]:f1-1-bg[1]) 
; ...Now do right hand side. (NB bg[2]=right width,bg[3]=right shift, f2=right hand side
; of foreground
			if bg[2] ne 0 then dbbgsubarr(bg[0]:(n_bgpoints-1))=subarr(f2+1+bg[3]:f2+bg[2]+bg[3]) 

; Now do a similar thing, but this time get the selected pixel VALUES
			if bg[0] ne 0 then dbbgarr(0:bg[0]-1,i)=db1(f1-bg[0]-bg[1]:f1-1-bg[1],i)
			if bg[2] ne 0 then dbbgarr(bg[0]:n_bgpoints-1,i)=db1(f2+1+bg[3]:f2+bg[2]+bg[3],i)

; find the errors on each of the pixel VALUES for use in the least-squares
; fitting process. Size of error array is equal to no. of fitting pixels
			err=fltarr(n_bgpoints)
; loop over number of background fitting pixels and find error by taking sqrt
			for j=0,n_bgpoints-1 do err(j)=sqrt(dbbgarr(j,i)+1)

; least squares fit of background
			dbbgcoeff=linfit(dbbgsubarr,dbbgarr(*,i),sdev=err,chisq=dbchisq,sigma=dbbgerr,/double)
	

; array containing the best fit line of the background
			dbbgfit(*,i)=dbbgcoeff(0)+dbbgcoeff(1)*subarr
; correct data by subtracting the background fit
			db2(*,i)=db1(*,i)-dbbgfit(*,i)
; error on background fit in the SQUARE ROOT of the fit at each point
			dbb(*,i)=sqrt(dbbgfit(*,i))
; to get error on corrected data add in quadrature the errors on the raw data and the background fit
			ed(*,i)=sqrt(dber(*,i)^2+dbb(*,i)^2)

		endfor

		;subtract backgrd from ref
		for i=0,z(2)-1 do begin

			refbgsubarr=intarr(n_bgpoints)
			refbgarr=fltarr(n_bgpoints,z(2))
			refbgfit=fltarr(z(1),z(2))

			if bg[0] ne 0 then refbgsubarr(0:bg[0]-1)=subarr(ff1-bg[0]-bg[1]:ff1-1-bg[1]) 
			if bg[2] ne 0 then refbgsubarr(bg[0]:n_bgpoints-1)=subarr(ff2+1+bg[3]:ff2+bg[2]+bg[3]) 

			if bg[0] ne 0 then refbgarr(0:bg[0]-1,i)=ref1(ff1-bg[0]-bg[1]:ff1-1-bg[1],i)
			if bg[2] ne 0 then refbgarr(bg[0]:n_bgpoints-1,i)=ref1(ff2+1+bg[3]:ff2+bg[2]+bg[3],i)

			err=fltarr(n_bgpoints)
			for j=0,n_bgpoints-1 do err(j)=sqrt(refbgarr(j,i)+1)

			refbgcoeff=linfit(refbgsubarr,refbgarr(*,i),sdev=err,chisq=refchisq,sigma=refbgerr,/double)

			;if refchisq ne 1 then begin
			;refbgcoeff=linfit(refbgsubarr(0:bg-1),refbgarr(0:bg-1,i),sdev=err(0:bg-1),chisq=refchisq,sigma=refbgerr)
			;endif

; Do similar calculation as for direct beam, i.e. take off background. Error in background is sqrt(counts)
; error in corrected data is found by adding in quadrature the errors in raw data and the background errors.
			refbgfit(*,i)=refbgcoeff(0)+refbgcoeff(1)*subarr
			ref2(*,i)=ref1(*,i)-refbgfit(*,i)
			rb(*,i)=sqrt(refbgfit(*,i))
			er(*,i)=sqrt(refer(*,i)^2+rb(*,i)^2)

		endfor

	endif

endif else begin
 
	ref2=ref1
	db2=db1
	ed=dber
	er=refer

endelse

d=total(db2(f1:f2,*),1)
r=total(ref2(ff1:ff2,*),1)

print,' '
if (bg[0] ne 0) and (bg[2] ne 0) then print,'done background correction'

;error calculation
ed=sqrt(total(ed(f1:f2,*)^2,1))
er=sqrt(total(er(ff1:ff2,*)^2,1))

reff=r
ereff=r

dpr=180./!pi

; calculate theta unless specified by user
if th(0) eq 1 then th(1)=tth(dbdan,dbcom,refdan,refcom,nx,dett)

q=4*!pi*sin(th(1)/dpr)/lamarr

for i=0,z(2)-1 do begin
	if (d(i) ne 0.) then begin
		reff(i)=r(i)/d(i)
		ereff(i)=reff(i)*sqrt((er(i)/r(i))^2+(ed(i)/d(i))^2) 
	endif else begin
		ereff(i)=0.
		reff(i)=1.e-11
	endelse
endfor

;resolution (error in q)
etf=((85.e-3)/tofd)+(3956.*opena*period)/(360.*tofd*lamarr)




print,'slit values', par2(93),par2(95)

;if s3 ge s2 then bigslit=s3 else bigslit=s2
;ethf=(bigslit*180)/(th(1)*3.4*!pi)

;s1s2 is the inter-slit distance
s1s2=3500.

ethf=2.*atan((par2(93)+par2(95))/(s1s2*2.))*180./(!pi*th(1))

qe=q*sqrt(etf^2+ethf^2)

;print,etf,ethf,qe/q

; unlogged data for reflectivity is in reff, q is q and error is ereff
if lamda(0) eq 1 then begin

	qmin=(min(q))
	qmax=(max(q))

endif else begin

	qmin=4*!pi*sin(th(1)/dpr)/lamda(2)
	qmax=4*!pi*sin(th(1)/dpr)/lamda(1)

endelse

qrange=where((q ge qmin) and (q le qmax))

wdum=reverse(reff(min(qrange):max(qrange)))
xdum=reverse(q(min(qrange):max(qrange)))
edum=reverse(ereff(min(qrange):max(qrange)))
qedum=reverse(qe(min(qrange):max(qrange)))

catch,/cancel

return
end

;*****************************************************************************************************
; Procedure to group data. Combines data from points which are, within the resolution, too
; close to each other. Also takes logarithm of the data.
; x = q values. y = Reflectivity values. e = error on reflectivity data
; dq = error on q values
; a = groupd reflectivity. b = groupd q values. c = groupd refl. errors
PRO group,x,y,e,dq,a,b,c

; arrays to store groupd data
ny=fltarr(1000)
nx=fltarr(1000)
nee=fltarr(1000)
ndq=fltarr(1000)


print,' '
print,'Bundling nearby data points for plotting:'

; resf = 'Magic' resolution factor. if the difference in q values is less than dq/resf then
; group the points. With resf=2. data points separated by half the error on q are groupd
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
repeat begin

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
while c lt num do begin

; If q values are sufficiently close....
  if (x(c+1)-x(c) lt dq(c)/resf and x(c+1) ne 0) then begin
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
  endif else begin
; If no bundling is necessary then just copy the data into the new arrays.
     ny(new)=y(c)
     nx(new)=x(c)
     nee(new)=e(c)
     ndq(new)=dq(c)
; inc. counter by one
     c=c+1
  endelse

;inc. groupd data array counter
	new=new+1
end

; set old data to new groupd data ready for next iteration
 y=ny
 x=nx
 e=nee
 dq=ndq

	print,pairs,' pairs found on iteration ',i
; increment number of iterations
	i=i+1
	
; stop if no pairs were found
endrep until pairs eq 0

; return results: Log reflectivity data and calc. error on reflectivity.
a=alog10(ny(0:tot-1)) & b=nx(0:tot-1) & c=(nee(0:tot-1)/ny(0:tot-1))/alog(10)

return

end

;*****************************************************************************************************
;outputs to plot and output file

pro output,x,y,e,xe,fil

common varbwid,base

points=size(x)

close,10
openw,10,fil,/append
for i=0,points(1)-1 do begin
	printf,10,x(i),y(i),abs(e(i)),xe(i)
endfor
close,10

plotbase=widget_base(group_leader=base,title='Reflectivity against Q (log plot)',/column)
graph=widget_draw(plotbase,xsize=700,ysize=500,retain=2)
but=widget_button(plotbase,value='OK',uvalue='ok')

widget_control,plotbase,/realize

; negative ref set to 10^-10
y=y > 10.e-10
xe=abs(xe)

nx=fltarr(1000)
ny=fltarr(1000)
ner=fltarr(1000)

; group data. groupd data is nx, ny and ner(new ...)
group,x,y,e,xe,ny,nx,ner

	if N_ELEMENTS(nx) lt 2 then begin
		errormessage,'Data bundled into oblivion!!'
		return
	end
; clip errors to 4. (4 orders of magnitude)
ner=ner<4.

; calc. errors
;errs=e/(y*alog(10.))


  
ploterr,nx,ny,ner
; get rid of negative errors
;errs=abs(errs)

; limits errors to 4 orders of magnitude (on log plot)
;errs=errs<4.
;ploterr,x,alog10(y),errs

xmanager,'message',plotbase

return
end

;**************************************************************************************************
;shows end message window with calculated values
pro endmessage

common varbwid,base
common varwid,lab,txt,but,gap
common varfac,fac1,fac2,fac3
common varth,th1,th2,th3
common varset,norm,bg,r,fil
common varbg,bgrd

res=intarr(50)

if norm eq 0 then nrm='runtime' else nrm='monitor'
if bgrd eq 0 then bgd='average'
if bgrd eq 1 then bgd='fit'

endbase=widget_base(group_leader=base,/floating,title='Finished',column=2)

;column1
res(0)=widget_label(endbase,value='fac1: '+strcompress(string(fac1(1)),/remove_all),/align_left)
res(2)=widget_label(endbase,value='fac2: '+strcompress(string(fac2(1)),/remove_all),/align_left)
res(2)=widget_label(endbase,value='fac3: '+strcompress(string(fac3(1)),/remove_all),/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
res(2)=widget_label(endbase,value='Normalised: '+nrm,/align_left)
res(2)=widget_label(endbase,value='Backgrd used: '+bgd,/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
res(2)=widget_label(endbase,value='Saved to: '''+strcompress(string(fil(1)),/remove_all)+'''',/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
res(2)=widget_label(endbase,value='Go again?')
gap(0)=widget_label(endbase,value='',ysize=0)

;column2
res(1)=widget_label(endbase,value='th1: '+strcompress(string(th1(1)),/remove_all),/align_left)
res(1)=widget_label(endbase,value='th2: '+strcompress(string(th2(1)),/remove_all),/align_left)
res(1)=widget_label(endbase,value='th3: '+strcompress(string(th3(1)),/remove_all),/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
res(2)=widget_label(endbase,value='Tot. Backgrd: '+strcompress(string(bg[0]+bg[2]),/remove_all),/align_left)
res(2)=widget_label(endbase,value='Foregrd: '+strcompress(string(r(1)),/remove_all),/align_left)
gap(0)=widget_label(endbase,value='',ysize=0)
gap(0)=widget_label(endbase,value='',ysize=0)
gap(0)=widget_label(endbase,value='',ysize=0)
endbut1=widget_button(endbase,value='OK',uvalue='ok')
endbut2=widget_button(endbase,value='QUIT',uvalue='quit')

widget_control,endbase,/realize

xmanager,'message',endbase

return
end

;************************************************************************************
;shows error message window
pro errormessage,mes1

common varbwid,base
common EFLAGS,eflag

; If its really an error then display appropriate message
if eflag ne 0 then begin
print,'********************************************************************'
print,'ERROR!! TOF Calculations aborted. Check error message and try again.'
print,'********************************************************************'
errbase=widget_base(group_leader=base,/floating,title='Error Message',/column)
; Otherwise its just a warning
endif else errbase=widget_base(group_leader=base,/floating,title='Warning Message',/column)

mess1=widget_label(errbase,value=mes1)
errbut=widget_button(errbase,value='OK',uvalue='ok')

widget_control,errbase,/realize

xmanager,'message',errbase

return
end

;***********************************************************************************
;handles events from errormessage, endmessage and output
pro message_event,event

common varbwid,base
common varwid,lab,txt,but,gap
common varfac,fac1,fac2,fac3
common varth,th1,th2,th3

widget_control,event.id,get_uvalue=ev,get_value=val

;widget_control,event.top,/destroy

;if ev eq 'yes' then widget_control,base,sensitive=1	

if ev eq 'quit' then widget_control,base,/destroy

if ev eq 'ok' then begin
	widget_control,event.top,/destroy
	widget_control,base,sensitive=1
endif	

return
end

;*************************************************************************
;input TOF window

pro d17tof
common par,par2
common var0,def
common varbwid,base
common varwid,lab,txt,but,gap

;widget types: lab=label,txt=text,but=button,gap=label(null string)
lab=lonarr(50)
txt=lonarr(50)
but=lonarr(50)
gap=lonarr(50)

;don't show maths errors
!except=0

;defaults that appear in the TOF window
def='def'

; If badflag=0 then the default file (if it exists) is okay
; If badflag=1 then the default file has been found to be corrupted by
; ON_IOERROR below
			badflag=0
;dtb*=direct beam run numbers 
setdefaults:		dtb1=''
			dtb2=''
			dtb3=''

;refl*=reflected beam run numbers
			refl1=''
			refl2=''
			refl3=''

; Instrument background numors
			instbg1=''
			instbg2=''
			instbg3=''
;water=water run numbers (or def)
			water=''

;wavelength range
			lambda='2.1,16'

;th*=theta angle (or def=calculated)
			th1=def
			th2=def
			th3=def

;fac*=factor by which to multiply Reflectivity (or def=calculated such that curves match)
			fac1=def
			fac2=def
			fac3=def

;r=Foreground range
;bg=Background range
			r='11'
			bgl='0'
			bgr='0'

;norm=method by which reflected beam is normalised to direct beam (0=Runtime,1=Monitor)
			norm=1
;bgrd=method by which background rate is determined in range bg (0=left side of peak,1=both sides of peak,2=linear fit calc from both sides of peak)
			bgrd=1

;fil=output file name
			fil='tofdat.out'
;path=path of data runs to be anal2ysed
			path='/users/data/'
; useful x range of detector
			useful='30,239'
; area to search for peak
			peak_searchx='30,239'
			peak_searchy='def'
;check default file exists
			filcheck=findfile('tof_defaults.dat',count=checkfil)			
; if file exists and its not already been found to be corrupt
			if (checkfil ne 0) and (badflag eq 0) then begin
			spacer='XXXXXXX'
			ON_IOERROR, corruptdefs
			close,10
			openr,10,'tof_defaults.dat'
			readf,10,spacer
			readf,10,dtb1
			readf,10,spacer
			readf,10,dtb2
			readf,10,spacer
			readf,10,dtb3
;refl*=reflected beam run numbers
			readf,10,spacer
			readf,10,refl1
			readf,10,spacer
			readf,10,refl2			
			readf,10,spacer
			readf,10,refl3

;instbg8=instrument background numors
			readf,10,spacer
			readf,10,instbg1
			readf,10,spacer
			readf,10,instbg2
			readf,10,spacer
			readf,10,instbg3

;water=water run numbers (or def)
			readf,10,spacer
			readf,10,water

;wavelength range
			readf,10,spacer
			readf,10,lambda

;th*=theta angle (or def=calculated)
			readf,10,spacer
			readf,10,th1
			readf,10,spacer
			readf,10,th2
			readf,10,spacer
			readf,10,th3

;fac*=factor by which to multiply Reflectivity (or def=calculated such that curves match)
			readf,10,spacer
			readf,10,fac1
			readf,10,spacer
			readf,10,fac2			
			readf,10,spacer
			readf,10,fac3
;r=Foreground range
;bg=Background range
			readf,10,spacer
			readf,10,r
; Background
; Left
			readf,10,spacer			
			readf,10,bgl
; Right
			readf,10,spacer
			readf,10,bgr

;norm=method by which reflected beam is normalised to direct beam (0=Runtime,1=Monitor)
			readf,10,spacer
			readf,10,norm
;bgrd=method by which background rate is determined in range bg (0=left side of peak,1=both sides of peak,2=linear fit calc from both sides of peak)
			readf,10,spacer
			readf,10,bgrd

;fil=output file name
			readf,10,spacer
			readf,10,fil
;path=path of data runs to be anal2ysed
			readf,10,spacer
			readf,10,path
; useful xrange of detector
			readf,10,spacer
			readf,10,useful
; area to look for peak
			readf,10,spacer
			readf,10,peak_searchx
			readf,10,spacer
			readf,10,peak_searchy
			close,10
			end
			goto,fine
corruptdefs:
			print,'Corrupted defaults file - ignored. Hardwired defaults set.'
			badflag=1
			goto,setdefaults				
;base window divided into 5 columns
fine: base=widget_base(title='TOF Data',uvalue='base',column=6) 

;column 1
gap(10)=widget_label(base,ysize=10,value='')
lab(0)=widget_label(base,value='Run')
lab(1)=widget_label(base,value='set')
gap(0)=widget_label(base,ysize=0,value='')
lab(2)=widget_label(base,value='(1)  ',/align_right)
gap(11)=widget_label(base,ysize=11,value='')
lab(3)=widget_label(base,value='(2)  ',/align_right)
gap(11)=widget_label(base,ysize=11,value='')
lab(4)=widget_label(base,value='(3)  ',/align_right)
gap(23)=widget_label(base,ysize=15,value='')
lab(5)=widget_label(base,value='Water',/align_right)
lab(6)=widget_label(base,value='runs',/align_right)
gap(10)=widget_label(base,ysize=10,value='')
lab(7)=widget_label(base,value='Lamda',/align_right)
lab(8)=widget_label(base,value='range',/align_right)
gap(10)=widget_label(base,ysize=10,value='')
gap(0)=widget_label(base,ysize=0,value='')
lab(9)=widget_label(base,value='Normalise',/align_right)
lab(10)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')

;column 2
gap(10)=widget_label(base,ysize=10,value='')
lab(11)=widget_label(base,value='Direct')
lab(12)=widget_label(base,value='runs')
txt(0)=widget_text(base,value=dtb1,xsize=9,/editable,uvalue='int')
txt(1)=widget_text(base,value=dtb2,xsize=9,/editable,uvalue='int')
txt(2)=widget_text(base,value=dtb3,xsize=9,/editable,uvalue='int')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(3)=widget_text(base,xsize=9,value=water,/editable,uvalue='int')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(4)=widget_text(base,xsize=9,value=lambda,/editable,uvalue='real')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
but(0)=cw_bgroup(base,['Runtime','Monitor'],set_value=norm,/column,/exclusive,uvalue='norm')
gap(36)=widget_label(base,ysize=30,value='')
lab(11)=widget_label(base,value='Data Path:',/align_right)
lab(11)=widget_label(base,value='Output file:',/align_right)

;column 3
gap(10)=widget_label(base,ysize=10,value='')
lab(13)=widget_label(base,value='Reflect')
lab(14)=widget_label(base,value='runs')
txt(5)=widget_text(base,value=refl1,xsize=9,/editable,uvalue='int')
txt(6)=widget_text(base,value=refl2,xsize=9,/editable,uvalue='int')
txt(7)=widget_text(base,value=refl3,xsize=9,/editable,uvalue='int')
gap(16)=widget_label(base,ysize=16,value='')
lab(15)=widget_label(base,value='Foregrd',/align_right)
lab(16)=widget_label(base,value='range',/align_right)
gap(0)=widget_label(base,ysize=10,value='')
lab(17)=widget_label(base,value='Backgrd',/align_right)
lab(18)=widget_label(base,value='range',/align_right)
gap(0)=widget_label(base,ysize=0,value='')
gap(20)=widget_label(base,ysize=20,value='')
lab(19)=widget_label(base,value='Bkgnd sub:',/align_right)
lab(21)=widget_label(base,value='method',/align_right)
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')

txt(8)=widget_text(base,value=path,xsize=9,/editable,uvalue='str')
txt(9)=widget_text(base,value=fil,xsize=9,/editable,uvalue='str')
gap(0)=widget_label(base,ysize=0,value='')

;column 4
gap(10)=widget_label(base,ysize=10,value='')
lab(20)=widget_label(base,value='Instr. Bckgnd.')
lab(23)=widget_label(base,value='runs')
txt(10)=widget_text(base,xsize=4,value=instbg1,/editable,uvalue='real')
txt(11)=widget_text(base,xsize=4,value=instbg2,/editable,uvalue='real')
txt(12)=widget_text(base,xsize=4,value=instbg3,/editable,uvalue='real')
gap(6)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(13)=widget_text(base,xsize=4,value=r,/editable,uvalue='int')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='left')
txt(14)=widget_text(base,xsize=4,value=bgl,/editable,uvalue='int')
gap(35)=widget_label(base,ysize=0,value='')
but(1)=cw_bgroup(base,['average','fit'],set_value=bgrd,/column,/exclusive,uvalue='bgrd')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=10,value='')
gap(0)=widget_label(base,ysize=0,value='')
but(3)=widget_button(base,value='DO IT',uvalue='done')
gap(0)=widget_label(base,ysize=0,value='')



;column 5
gap(10)=widget_label(base,ysize=10,value='')
lab(21)=widget_label(base,value='Factor')
gap(6)=widget_label(base,value='',ysize=6)
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(15)=widget_text(base,xsize=8,value=fac1,/editable,uvalue='real')
txt(16)=widget_text(base,xsize=8,value=fac2,/editable,uvalue='real')
txt(17)=widget_text(base,xsize=8,value=fac3,/editable,uvalue='real')
gap(6)=widget_label(base,ysize=6,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=20,value='')
gap(0)=widget_label(base,ysize=22,value='')
gap(6)=widget_label(base,ysize=10,value='')
gap(10)=widget_label(base,ysize=0,value='right')
txt(18)=widget_text(base,xsize=8,value=bgr,/editable,uvalue='int')
gap(40)=widget_label(base,ysize=40,value='')
gap(20)=widget_label(base,ysize=20,value='')
gap(35)=widget_label(base,ysize=35,value='')
but(2)=widget_button(base,value='QUIT',uvalue='quit')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')

;column 6
gap(10)=widget_label(base,ysize=10,value='')
lab(22)=widget_label(base,value='Theta')
gap(6)=widget_label(base,value='',ysize=6)
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
txt(19)=widget_text(base,xsize=4,value=th1,/editable,uvalue='real')
txt(20)=widget_text(base,xsize=4,value=th2,/editable,uvalue='real')
txt(21)=widget_text(base,xsize=4,value=th3,/editable,uvalue='real')
gap(6)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')


gap(0)=widget_label(base,ysize=0,value='Detector')
gap(6)=widget_label(base,ysize=0,value='ranges')

drange = widget_base(base,/FRAME,/BASE_ALIGN_CENTER,/COLUMN)
gap(40)=widget_label(drange,value='Useful x range')
txt(22)=widget_text(drange,xsize=8,value=useful,/editable)
gap(6)=widget_label(drange,value='Area to look')
gap(0)=widget_label(drange,value=' for peak')
gap(6)=widget_label(drange,value='(xmin,xmax)')
txt(23)=widget_text(drange,xsize=8,value=peak_searchx,/editable)
gap(6)=widget_label(drange,value='(ymin,ymax)')
txt(24)=widget_text(drange,xsize=8,value=peak_searchy,/editable)

gap(6)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')
gap(6)=widget_label(base,ysize=0,value='')
gap(0)=widget_label(base,ysize=0,value='')

;gives the TOF window a margin
baseinfo=widget_info(base,/geometry)
widget_control,base,scr_xsize=baseinfo.scr_xsize+10
widget_control,base,scr_ysize=baseinfo.scr_ysize+10

;creates the TOF window
widget_control,base,/realize

;calls 'pro tof_event'
xmanager,'tof',base

end
