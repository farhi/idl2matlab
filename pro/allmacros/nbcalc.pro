;===============================================================================
; This procedure does all the calculations. It is called by nbcalc_event when 
; the calc. button is pressed. The string containing the chemical formula is
; passed as the argument 'compound'. The procedure first reads the file 
; containing the isotope data. This is inefficient because the file is read 
; each calculation, but it enables the user to modify the file while the 
; programme is still running (e.g. if the user wants to use an element/isotope
; which isn't in the file.) Then the string 'compound' is processed. To 
; determine the isotopes present and how many of each is in the formula.
; Numerous error checks are made. The result of all of this is a 2d array
; with an 'index' number of each isotope encountered and the 'multiplicty'
; i.e. the number of times it occurs in the formula.
;===============================================================================
PRO docalc,compound,density

; Common block to return isotope selected from selector window
COMMON iso,isel

;***************************************
; Open data file and read info. This is
; done each time a 'CALCULATE' is 
; pressed so the user can edit
; data fiel 'nbcalc.dat' while the 
; programme is running.

; open data file for reading
	CLOSE,10
	OPENR,10,'nbcalc.dat',ERROR=err
; if the file cannot be found locally then try remotely
	IF (err NE 0) THEN BEGIN 
		PRINT,'Unable to open nbcalc.dat in local directory. Trying remotely...'
		CLOSE,10
		OPENR,10,'/home/cs/lambda/macros/D17/GENERAL/nbcalc.dat',ERROR=err
; If file can't be found then signal error
		IF (err NE 0) THEN BEGIN
			PRINT,'Error: Cannot open nbcalc.dat'
			errormessage,'Cannot open nbcalc.dat!'
			RETURN
		END
		PRINT,' * Standard nbcalc.dat fetched from remote directory.'
	END
	
; set up catalogue of entries for the data of each isotope (up to 400)
	catalogue=REPLICATE({isotope, z:0, symb:'', m:0.0, b:0.0, a:0.0},400)
	
; read header and discard
	header=''
	READF,10,header
	READF,10,header
	READF,10,header
	
; counter for catalogue
	i=0
; define the types of the various fields in each entry
	z=0
	symb=''
	m=0.0
	b=0.0
	a=0.0
; read the entire file into the catalogue
	WHILE (NOT EOF(10)) AND (i LE N_ELEMENTS(catalogue)-1) DO BEGIN 
; note the strict formatting used in the data file...
		READF,10,z,symb,m,a,b,FORMAT='(I2,1X,A2,5X,F9.5,2X,F9.4,3X,F13.6)'
; set values
		catalogue[i].z=z
		catalogue[i].symb=STRCOMPRESS(symb,/REMOVE_ALL)
		catalogue[i].m=m
		catalogue[i].b=b
		catalogue[i].a=a
; increment counter
		i=i+1
	END
	CLOSE,10	
; number of isotopes read (i.e. maximum index of isotopes)
	nentries=i
	
;***************************************
; Process string to determine chemical 
; formula.

; array to contain up to 100 of (isotope index,number of isotope in formula)
	isotopes=INTARR(100,2)
; counter for isotopes appearing in the formula
	nisotopes=0
	
; remove whitespace and convert string to vector of bytes 
	compound = BYTE(STRCOMPRESS(compound, /REMOVE_ALL))

; IF 1st character of compound is the string terminator then no formula has been entered
	IF compound[0] EQ 0B THEN BEGIN
		errormessage,'Please enter a chemical formula!'
		RETURN
	END

; add a null terminator to the end of the string so can tell when to stop
	compound = [compound,0B]
	
;string index
	strind=0
; done flag
	done = 0
	WHILE NOT done DO BEGIN
	
; Check if current character is the first letter of a symbol (a capital). If not then
; signal error (see else statement below)
		IF compound[strind] GE 65B AND compound[strind] LE 90B THEN BEGIN
		
; Some elements have two characters, e.g. Fe. Check for a second lower case
			IF compound[strind+1] GE 97B AND compound[strind+1] LE 122B THEN BEGIN
; If so make a two character symbol string
				symbol=STRING(compound[strind:strind+1])
				strind=strind+2
			ENDIF ELSE BEGIN
; otherwise its just one char. ,e.g. C (carbon)
				symbol=STRING(compound[strind])
				strind=strind+1
			END

; Now look up in the catalogue of data to find the element 'symbol'
; A temporary array to contain all of the indices of isotopes of element 'symbol'
			iso_index=INTARR(20)
; counts the number of isotopes of the element
			counter=0
; loop over all entries of the catalogue
			FOR i=0,nentries-1 DO BEGIN
; If the element corresponds to symbol then add then save the index of that isotope
				IF symbol EQ catalogue[i].symb THEN BEGIN
					iso_index[counter]=i
					counter=counter+1
				END
			END
; If symbol wasn't in the catalogue then signal unknown element error
			IF counter EQ 0 THEN BEGIN
				errormessage,'Element ' + symbol + ' is unknown. (Add it to nbcalc.dat then press CALCULATE again)'
				RETURN
			END

; If there is only one isotope of element 'symbol' then simply copy the catalogue index
; into the array of isotopes in the formula
			IF counter EQ 1 THEN BEGIN
				isotopes[nisotopes]=iso_index[0]
			ENDIF ELSE BEGIN
; make an array of strings containing the masses of the isotopes for this element
				masses=STRARR(counter)
				masses[*]=STRING(catalogue[iso_index[0:counter-1]].m)
; bring up a window for the user to select the desired isotope which is then
; place in the common variable isel
				isel=-1
				isoselector,symbol,masses,strind-1
; place the selected isotope into the isotope array
				isotopes[nisotopes,0]=iso_index[isel]
			END
			
;Find number of times element occurs in compound (e.g. in H2O (water) number of H's is 2)

; num_len gives the length of the integer following the element (e.g. length=1 for the '2' in 'H2O')
			num_len=0

; look for a number, find its length in characters
			WHILE (compound[strind+num_len] GE 48B) AND (compound[strind+num_len] LE 57B) DO num_len=num_len+1
				
; if there are no 'suffix' numbers then element occurs once by default
			If num_len EQ 0 THEN isotopes[nisotopes,1]=1 ELSE BEGIN 
; otherwise convert to an integer
				num_el=FIX(STRING(compound[strind:strind+num_len]))
; check that it's not zero (that's just silly!)
				IF num_el EQ 0 THEN BEGIN
					errormessage,'Element '+symbol+' has suffix number zero! Check formula (zeroes look a lot like the letter O)'
					RETURN
				END
; store the number 
				isotopes[nisotopes,1]=num_el
			END
; increment string index by number of characters of number
			strind=strind+num_len
			
; set done flag to true if the end of the chemical formula is reached
			IF compound[strind] EQ 0B THEN done=1

; Print out to console which elements and isotopes have been found and how many there are of them
			PRINT,'Element ',symbol,', mass isotope',catalogue[isotopes[nisotopes,0]].m,' found ',isotopes[nisotopes,1],' time(s)'

; increment number of isotopes found in formula
			nisotopes=nisotopes+1

; Expected 1st letter next element to be capital, but it isn't so signal error
		ENDIF ELSE BEGIN
			errormessage,'Illegal character "'+STRING(compound[strind])+'". Element symbol starting with capital letter expected.
			RETURN
		END
	END

; sum up masses, scattering length and a:
; Mass:
	sum_mass=total(catalogue[isotopes[*,0]].m*isotopes[*,1])
; b:
	sum_b=total(catalogue[isotopes[*,0]].b*isotopes[*,1])
; a:
	sum_a=total(catalogue[isotopes[*,0]].a*isotopes[*,1])

; calculation of scattering length density
	nb=(sum_b*1.0e-23*density*6.02214e23)/sum_mass
; calculation of absorption
	ab=1/((sum_a*1.0e-28*density*1.0e6*6.02214e23)/sum_mass)

	results,STRING(compound),density,nb,ab
END

;===============================================================================
; shows isotope selection window and prompts user to select one
; element is a string containing the element symbol. isomasses is an array
; of strings of the isotope masses to label the buttons. strind is the index
; at which the element (whose isotope is desired) occurs. This is used to indicate
; which symbol we want the isotope for.
;===============================================================================
PRO isoselector,element,isomasses,strind

; common block containing base's id
COMMON widgetid,base,formula,dense
; common block containing isotope selector buttons
COMMON alig,bo_selector

	selbase = WIDGET_BASE(GROUP_LEADER=base,/FLOATING,/MODAL,TITLE='Select Isotope',/COLUMN)
	
; display blurb
	dummy = WIDGET_LABEL(selbase,VALUE='Please select isotope of '+element)
	dumbshit = WIDGET_LABEL(selbase,VALUE='Position in formula shown below:')
; get the string containging the formula and remove all whitespace. convert to byte array
	WIDGET_CONTROL,formula,GET_VALUE=compound
	compound = BYTE(STRCOMPRESS(compound[0],/REMOVE_ALL))
; tack some extra space at the end to stop errors due on line below.
	compound = [compound,32B,32B]
; form a string containing the chemical formula with a sodding great arrow pointing to the element in question
	pos_string=STRING(compound[0:strind])+' <===(!)   '+STRING(compound[strind+1:N_ELEMENTS(compound)-1])
; display formula with arrow pointing to element whose isotope is requested
	cleverchap = WIDGET_LABEL(selbase,/ALIGN_LEFT,VALUE=pos_string)
	
; now put up some buttons to prompt user to select an isotope
	bo_selector = CW_BGROUP(selbase,isomasses, /EXCLUSIVE, UVALUE='',/COLUMN,LABEL_TOP='Please choose',/FRAME)
	dummy = WIDGET_LABEL(selbase,VALUE='NB: Natural abundance')
	dummy = WIDGET_LABEL(selbase,VALUE='<should> be  1st in list')
	WIDGET_CONTROL,bo_selector,SET_VALUE=0
; put ok button at bottom. User value 'sel' enables message_event to know that
; the user has just selected an isotope
	ok_butt = WIDGET_BUTTON(selbase,VALUE='OK',UVALUE='sel')
	
; now realise it and leave the procedure message_event to pick up the pieces...
	WIDGET_CONTROL,selbase,/REALIZE
	XMANAGER,'message',selbase
END

;===============================================================================
; Shows results window. form=string containing chemical formula.
;===============================================================================
PRO results,form,density,nb,ab

; common block holding the base's id
COMMON widgetid,base,formula,dense

	result_base = WIDGET_BASE(GROUP_LEADER=base,/COLUMN,/FLOATING,TITLE='Results for '+form)
	dummy = WIDGET_LABEL(result_base,/ALIGN_CENTER,VALUE='Compound '+form+' has density '+STRCOMPRESS(STRING(density),/REMOVE_ALL)+ ' g/cm^3')
	dummy = WIDGET_LABEL(result_base,/ALIGN_CENTER,VALUE='Scattering length density = '+STRCOMPRESS(STRING(nb),/REMOVE_ALL))
	dummy = WIDGET_LABEL(result_base,/ALIGN_CENTER,VALUE='Absorption length = '+STRCOMPRESS(STRING(ab),/REMOVE_ALL))

	big_butt=WIDGET_BUTTON(result_base,VALUE='OK',UVALUE='ok')

	WIDGET_CONTROL,result_base,/REALIZE
	XMANAGER,'message',result_base
END
;===============================================================================
;shows error message window
;===============================================================================
PRO errormessage,messy

; common block holding the base's id
COMMON widgetid,base,formula,dense

; desensitise base
	WIDGET_CONTROL,base,SENSITIVE=0
;create floating message widget
	msg_base=WIDGET_BASE(GROUP_LEADER=base,/FLOATING,TITLE='Error Message',/COLUMN)

; label and give widget a button with 'ok' on
	mess=WIDGET_LABEL(msg_base,VALUE=messy)
	errbut=WIDGET_BUTTON(msg_base,VALUE='OK',UVALUE='ok')

; realise and pass control to event handler
	WIDGET_CONTROL,msg_base,/REALIZE
	XMANAGER,'message',msg_base,/JUST_REG
	RETURN
END

;===============================================================================
;handles events from errormessage, isoselector and results
;===============================================================================
PRO message_event,event

; common block for base widget ID
COMMON widgetid,base,formula,dense
; common block containing isotope selector buttons
COMMON alig,bo_selector
; Common block to return isotope selected
COMMON iso,isel

; get user value from button that caused event
	WIDGET_CONTROL,event.id,GET_UVALUE=ev,GET_VALUE=val
; If Okay button was pressed then resensitise the base widget and destroy
; the message widget
IF ev EQ 'ok' THEN BEGIN
	WIDGET_CONTROL,event.top,/DESTROY
	WIDGET_CONTROL,base,SENSITIVE=1
ENDIF

; If the window is the isotope selector and the ok button is pressed then
; return the isotope number

IF ev EQ 'sel' THEN BEGIN
		WIDGET_CONTROL,bo_selector,GET_VALUE=isel
		WIDGET_CONTROL,event.top,/DESTROY
ENDIF

END

;===============================================================================
; Displayshelp file nbcalc.txt
;===============================================================================
PRO helpwin

; common block for important widget id's	
COMMON widgetid,base,formula,dense

; open help file for reading
	CLOSE,10
	OPENR,10,'nbcalc.txt',ERROR=err
; if the file cannot be found locally then try remotely
	IF (err NE 0) THEN BEGIN 
		PRINT,'Unable to open nbcalc.txt in local directory. Trying remotely...'
		CLOSE,10
		OPENR,10,'/usr/ill/bin/nbcalc.txt',ERROR=err
; If file can't be found then signal error
		IF (err NE 0) THEN BEGIN
			errormessage,'Cannot open help file nbcalc.txt!'
			RETURN
		END
		
	END

; array of strings for each line of the help file
	line=STRARR(200)
; auxilliary string to read in each line. Should be able to just read
; each line into an element of the string array line, but IDL doesn't seem
; to do that!
	lined=''
; line counter
	i=0
	
	WHILE (NOT EOF(10)) AND i LT 200 DO BEGIN
		READF,10,lined
		line[i]=lined
		i=i+1
	END
	CLOSE,10

	help_base = WIDGET_BASE(GROUP_LEADER=base,/COLUMN,/FLOATING,TITLE='Help!')
	helpfile = WIDGET_TEXT(help_base,VALUE=line,/WRAP,YSIZE=30,XSIZE=55,/SCROLL)
	big_butt=WIDGET_BUTTON(help_base,VALUE='OK',UVALUE='ok')

	WIDGET_CONTROL,help_base,/REALIZE
	XMANAGER,'message',help_base
END

;===============================================================================
; Event handler for main base widget
;===============================================================================
PRO nbcalc_event,ev

; common block for important widget id's	
COMMON widgetid,base,formula,dense

	WIDGET_CONTROL,ev.id,GET_UVALUE=option
	IF option EQ 'quit' THEN WIDGET_CONTROL,ev.top,/DESTROY
; If 'CALCULATE' button is pressed then get chemical compound and call docalc
	IF option EQ 'help' THEN BEGIN
		helpwin
		RETURN
	END
	IF option EQ 'do' THEN BEGIN
; desensitize base and get formula and density
		WIDGET_CONTROL,base,SENSITIVE=0
		WIDGET_CONTROL,formula,GET_VALUE=compound
		WIDGET_CONTROL,dense,GET_VALUE=d
		density=FLOAT(STRCOMPRESS(d[0],/REMOVE_ALL))	
; error check for density
		IF density LE 0.0 THEN BEGIN
			errormessage,'Density must be a real number greater than 0.0.'
			RETURN	
		END
		docalc,compound[0],density
		WIDGET_CONTROL,base,SENSITIVE=1
	END
END

;===============================================================================
; Main procedure. Draws user interface.
;===============================================================================
PRO nbcalc

; common block for important widget id's	
COMMON widgetid,base,formula,dense

; Create user interface:
	base = WIDGET_BASE(TITLE='NbCalc',/COLUMN)
	
	dummy = WIDGET_LABEL(base,YSIZE=10,VALUE='')
	dummy = WIDGET_LABEL(base,/ALIGN_CENTER,VALUE='Enter chemical formula below')
; The example is FLaNGe5. In the original programme ArSe5 was the example.
; Other amusing ones are NiCeArSe, PUS5Y, PoON,BiTcH, and the very
; rude Fluorine-Uranium-Carbon-Potassium.
; (GaMoNFLaP5 is also quite funny!)
	dummy = WIDGET_LABEL(base,/ALIGN_CENTER,VALUE=' (e.g. FLaNGe5)')
	formula = WIDGET_TEXT(base,/EDITABLE,VALUE='')
	dummy = WIDGET_LABEL(base,YSIZE=10,VALUE='')
	dummy = WIDGET_LABEL(base,/ALIGN_CENTER,VALUE='Enter substance density below')
	dummy = WIDGET_LABEL(base,/ALIGN_CENTER,VALUE='(g/cm^3)')
	dense = WIDGET_TEXT(base,/EDITABLE,VALUE='')
	do_button = WIDGET_BUTTON(base,VALUE='CALCULATE',UVALUE='do')
	help_button = WIDGET_BUTTON(base,VALUE='HELP',UVALUE='help')
	quit_button = WIDGET_BUTTON(base,VALUE='QUIT',UVALUE='quit')
	
; Realise
	WIDGET_CONTROL,base,/REALIZE
	XMANAGER,'nbcalc',base,/JUST_REG
END
