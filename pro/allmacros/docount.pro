; **************************************************************************
; ** docount macro	Version 1.2.1b					  **
; ** -------------							  **
; ** Mark McCrum 3/8/00							  **
; **									  **
; ** Uses: time.pro time library					  **
; **	   dial_mad_send (Standard GEORGE / MAD interface)	       	  **
; **									  **
; **									  **
; **									  **
; **									  **
; ** New Features: Initial configuration can now be passed as a parameter **
; **									  **
; **		   Actual configuration is returned as well as the system **
; **		   time at the start of scan (-1 if scan was cancelled    **
; **									  **
; **		   Duration is now duration per scan position rather than **
; **		   duration of entire scan. It is believed that this will **
; **		   prove more convenient to the user.			  **
; **									  **
; **		   Durations may now be entered in hours minutes and	  **
; **		   seconds.						  **
; **									  **
; **		  Corrected bug: Output window after count initiated      **
; **				 always assumed duration was in units     **
; **				 of time. This would result in            **
; **				 nonsensical output if duration was in    **
; **				 counts					  **	
; **									  **
; ** MODIFIED 4/8/00: Added NO_RELEASE keyword to unitbuttons BGROUP	  **
; **		      This is necessary to prevent screen update problems **
; **		      under IDL 5.1. It should be removed for earlier     **
; **		      versions						  **
; **									  **
; **		      Minor layout change. (addition of scanFieldBase)	  **
; **									  **
; **		      Removed dialog box informing that scan has started  **
; **		      pending replacement by a non modal version. This is **
; **		      so the stats program can start monitoring a count   **
; **		      as soon as it starts				  **
; **									  **
; ** Known problems:	Help is out of date				  **
; **									  **
; **									  **
; **************************************************************************
; **									  **
; ** This macro provides a graphical interface for setting up		  **
; ** Scans and Counts on D7. The appropriate parameters are gathered	  **
; ** validated then sent to MAD in the appropriate format		  **
; **									  **
; ** This version (b) is designed to be launched from a GEORGE PAD        **
; ** and includes a dialog box explaining that the count has been         **
; ** launched, how long it will last, and when it will finish		  **
; **									  **
; **************************************************************************



FUNCTION calcNumSteps, scanFrom, scanTo, scanBy		;Calculate the number 
;**********************************************		;of steps in a scan with
							;the given parameters

	IF scanBy NE 0 THEN RETURN,round(((scanTo-scanFrom)/scanBy))+1
	RETURN, 0
END




PRO Count_event, event		;This is the top level event handler. All widgets
;**********************		;have event handlers except for the fields as IDL 
				;does not appear to have any facility to give them
				;one. Therefore events for the fields are handled 
				;here
	
	;Variables in 'countConstants' wil be used as constants by various
	;procdures and functions. They are set up in the docount procedure
	;(below)

	COMMON countConstants, NOSCAN, THETASCAN, OMEGASCAN, ROTATIONSCAN, $
			       NOPOL, ZPOL, XYZPOL, MONITORUNIT, TIMEUNIT

	
	FORWARD_FUNCTION sec2time	;Forward function definions for the
	FORWARD_FUNCTION now		;time library (there is probably a 
	FORWARD_FUNCTION addTime	;better way of doing this)
	FORWARD_FUNCTION timeAfter
	FORWARD_FUNCTION time2string
	


	 ;A common block is used to allow procedures easy access to the widgets
	 ;that make up the interface and the 'state' of the application. This
	 ;method was used as it is far easier than holding the state in the uvalue
	 ;of some widget. Using a common block means that only one copy of the
	 ;application can run at a time, but it would not be desirable to run
	 ;more than one anyway

	COMMON countCom, po,duration, unit, repeats, save, title, ratio,$
                          scan, from, to, step, saveButton, scanButtons,$
			  scanFromField, scanToField, scanStepField,    $
			  polButtons, countRatioField, repeatField,     $
			  countsField, hoursField, minsField, secsField,$
			  unitsButtons, scanButtonID, polButtonID,      $
			  unitButtonID, okButton, cancelButton,		$
			  helpButton, titleField, scanNumStepsField,	$
			  numSteps, timeInputBase, startTime ;***ADDITION **


	

	 ;Here we process events for  totalTimeField and repeatField, 
	 ;ensuring that the contents of the fields 
	 ;remain positive. NB this method isn't foolproof so we must 
	 ;still validate elsewhere

	 ;NB need 2 if statements because IDL does not appear to support lazy
	 ;evaluation and not all widgets have a numeric value field.
	
	IF event.ID EQ countsField OR event.ID EQ hoursField OR event.ID EQ minsField $
	   OR event.ID EQ secsField OR event.ID EQ repeatField THEN IF event.Value<0 THEN $
				WIDGET_CONTROL, event.ID, SET_VALUE=-event.Value

	IF event.ID EQ minsField OR event.ID EQ secsField THEN IF event.Value GE 60 THEN $
				WIDGET_CONTROl, event.ID, SET_VALUE=0

	
	
	;Update the numSteps field 
	
	IF (scan EQ OMEGASCAN OR scan EQ ROTATIONSCAN) AND $
	   (event.ID EQ scanFromField OR event.ID EQ scanToField $
	   OR event.ID EQ scanStepField) THEN BEGIN
		
		WIDGET_CONTROL, scanFromField, GET_VALUE=from
		WIDGET_CONTROL, scanToField, GET_VALUE=to
		WIDGET_CONTROL, scanStepField, GET_VALUE=step

		numSteps=calcNumSteps(from, to, step)
		WIDGET_CONTROL, scanNumStepsField, SET_VALUE=numSteps 
	ENDIF


END

PRO get2thetaParams	;Read the 2theta scan parameters from MAD and update
;******************	;the Scan From, To and By fields appropriately

	COMMON countConstants
	COMMON countCom
	params=DIAL_MAD_READ('t_para')
	from=params.bank1(0)
	to=params.bank1(1)
	step=params.bank1(2)
	numSteps=calcNumSteps(from, to, step)
	
	WIDGET_CONTROL, scanFromField, SET_VALUE=from
	WIDGET_CONTROL, scanToField, SET_VALUE=to
	WIDGET_CONTROL, scanStepField, SET_VALUE=step
	WIDGET_CONTROL, scanNumStepsField, SET_VALUE=numSteps
	


END



PRO setDefaults, defaults	; This procedure sets the initial, default,
				; parameter values and sets the interface widgets
			        ; to reflect this
	
	COMMON countCom
	COMMON countConstants


	;do some initialisation
	 countsField=0
	 hoursField=0
	 minsField=0
	 secsField=0
	 startTime=0


	 ;set variables	


	IF (SIZE(defaults))(0) EQ 0 THEN BEGIN
		title=''	;Title to give the scan
		po=NOPOL	;Polarisation
		duration=0L	;Total length of measurement
		unit='t'	;Is the above in counts (m) or seconds (t)?
		repeats=1	;Number of times to do each measurement	
		save=1		;1= save, 0 = don't save results
		ratio=1		;Flip / non-flip count ratio
		scan=NOSCAN	;Type of Scan to perform
		from=0.		;Start scan position
		to=0.		;End scan position
		step=0.		;Step amount
		numSteps=0	;No. of steps in the scan
	END ELSE BEGIN
		title=defaults.title
	
		
		CASE defaults.scan OF
			'2theta': scan=THETASCAN	
			'rotation': scan=ROTATIONSCAN
			'omega':  scan=OMEGASCAN
			ELSE: 	  scan=NOSCAN
		ENDCASE

		CASE defaults.po OF
			'NOP': po=NOPOL
			'ZP': po=ZPOL
			'XYZP': po=XYZPOL
			ELSE: po=NOPOL
		ENDCASE


		duration=defaults.duration
		IF defaults.unit EQ 'm' OR defaults.unit EQ 't' THEN $  
		      unit=defaults.unit $
		ELSE $
      			unit='t'


		repeats=defaults.repeats
		save=defaults.save
		ratio=defaults.ratio
		from=defaults.from
		to=defaults.to
		step=defaults.step
		numSteps=calcNumSteps(from, to, step)
				
	END



	 ;Set controls to match

	WIDGET_CONTROL, titleField, SET_VALUE=title
	
	IF save EQ 1 THEN $
		WIDGET_CONTROL, saveButton, /SET_BUTTON $
	ELSE $
		WIDGET_CONTROL, saveButton, SET_BUTTON=0

	WIDGET_CONTROL, scanFromField, SET_VALUE=from
	WIDGET_CONTROL, scanToField, SET_VALUE=to
	WIDGET_CONTROL, scanStepField, SET_VALUE=step
	WIDGET_CONTROL, scanNumStepsField, SET_VALUE=numSteps
	WIDGET_CONTROL, countRatioField, SET_VALUE=ratio
	WIDGET_CONTROL, repeatField, SET_VALUE=repeats
		

	IF unit EQ 'm' THEN BEGIN
		enableMonitorUnits
		WIDGET_CONTROL, countsField, SET_VALUE=duration
		WIDGET_CONTROL, unitButtonID(1), /SET_BUTTON
	END ELSE BEGIN
		enableTimeUnits
		hours=FIX(duration/3600)
		mins=FIX((duration MOD 3600) / 60)
		secs=FIX(duration MOD 3600)MOD 60
		WIDGET_CONTROL, hoursField, SET_VALUE=hours
		WIDGET_CONTROL, secsField, SET_VALUE=secs	
		WIDGET_CONTROL, minsField, SET_VALUE=mins
		WIDGET_CONTROL, unitButtonID(0), /SET_BUTTON
	END

	CASE scan OF
		NOSCAN: BEGIN
				WIDGET_CONTROL, scanButtonID(0), /SET_BUTTON
				disableScan
			END

		THETASCAN: BEGIN
				WIDGET_CONTROL, scanButtonID(1), /SET_BUTTON
				enableThetaScan
			   END
		OMEGASCAN: BEGIN
				WIDGET_CONTROL, scanButtonID(2), /SET_BUTTON
				enableScan
			   END

		ROTATIONSCAN: BEGIN
				WIDGET_CONTROL, scanButtonID(3), /SET_BUTTON
				enableScan
			      END
	ENDCASE


	CASE po OF
		NOPOL: 	BEGIN
				WIDGET_CONTROL, polButtonID(0), /SET_BUTTON
				disableRatio
			END		

		ZPOL:	BEGIN	
				WIDGET_CONTROL, polButtonID(1), /SET_BUTTON
				enableRatio
			END

		XYZPOL:	BEGIN
				WIDGET_CONTROL, polButtonID(2), /SET_BUTTON
				enableRatio
			END
	ENDCASE
	

	

END

PRO disableScan	;This procedure disables the Scan parameter input fields.
;**************	;This is done if the user selects the 'None' 
		;scan option
	COMMON countCom
	WIDGET_CONTROL, scanFromField, SENSITIVE=0
	WIDGET_CONTROL, scanToField,  SENSITIVE=0
	WIDGET_CONTROL, scanStepField, SENSITIVE=0
	WIDGET_CONTROL, scanNumStepsField, SENSITIVE=0
END 

PRO disableRatio ;This procedure disables the Flip / non-flip ratio field.
;*************** ;Called when the user selects the 'None' polarisation 
		 ;analysis option

	COMMON countCom
	WIDGET_CONTROL, countRatioField, SENSITIVE=0
END

PRO enableScan	;enable scan parameter input. 
;*************	;This is done for Omega and Rotation scans.
		;Theta scan parameters are read directly
		;from MAD
	COMMON countCom
	WIDGET_CONTROL, scanFromField, /SENSITIVE, /EDITABLE
	WIDGET_CONTROL, scanToField, /SENSITIVE, /EDITABLE
	WIDGET_CONTROL, scanStepField, /SENSITIVE, /EDITABLE

         ;Reset the scanNumStepsField to the correct value

	numSteps=calcNumSteps(from,to,step)
	WIDGET_CONTROL, scanNumStepsField, SET_VALUE=numSteps,$
			/SENSITIVE
	
END 	


PRO enableThetaScan	;update scan parameter fields but 
;******************	;don't allow input. Values for the 
			;fields are read from the MAD 2theta
			;parameters

	COMMON countCom
	WIDGET_CONTROL, scanFromField, SENSITIVE=0
	WIDGET_CONTROL, scanToField, SENSITIVE=0
	WIDGET_CONTROL, scanStepField, SENSITIVE=0
	WIDGET_CONTROL, scanNumStepsField, /SENSITIVE
	get2thetaParams ;read MAD parameters into the fields	
END

PRO enableRatio ;re-enable flip / non flip ratio parameter input 
;**************
	COMMON countCom
	WIDGET_CONTROL, countRatioField, /SENSITIVE
END

FUNCTION scanButtons_event, event	; Event handler for the scan selection
;********************************	; button group
	COMMON countCom
	COMMON countConstants

	scan=event.VALUE		;scan should contain current scan selection
	
	;Disable or enable scan parameter fields as appropriate
 	
	CASE scan OF
		THETASCAN: enableThetaScan
		NOSCAN: disableScan
		ELSE: enableScan
	ENDCASE
	
		
END


PRO enableMonitorUnits
	COMMON countCom
	COMMON countConstants

	WIDGET_CONTROL, timeInputBase, UPDATE=0
	countsField=CW_FIELD(timeInputBase, /LONG, $
			     TITLE='Duration: ', XSIZE=6, /ALL_EVENTS, $
				    UVALUE='countsField', VALUE=0)

	WIDGET_CONTROL, hoursField, BAD_ID=rubbish,  /DESTROY
	WIDGET_CONTROL, minsField, BAD_ID=rubbish, /DESTROY
	WIDGET_CONTROL, secsField, BAD_ID=rubbish, /DESTROY
	WIDGET_CONTROL, timeInputBase, BAD_ID=rubbish,/UPDATE	

	unit='m'
END

PRO enableTimeUnits
	COMMON countCom
	COMMON countConstants


	WIDGET_CONTROL, timeInputBase, UPDATE=0	
	hoursField=CW_FIELD(timeInputBase, /LONG, $
			    TITLE='Hours: ', XSIZE=4, /ALL_EVENTS, $
			    UVALUE='hoursField', VALUE=0)

	minsField=CW_FIELD(timeInputBase, /LONG, $
			   TITLE='Mins: ', XSIZE=2, /ALL_EVENTS, $
			   UVALUE='minsField', VALUE=0)
	
	secsField=CW_FIELD(timeInputBase, /LONG, $
			   TITLE='Secs: ', XSIZE=2, /ALL_EVENTS, $
			   UVALUE='secsField', VALUE=0)

	WIDGET_CONTROL, countsField, BAD_ID=rubbish, /DESTROY
	WIDGET_CONTROL, timeInputBase, /UPDATE

	unit='t'
END

FUNCTION polButtons_event, event	; Event handler for the polarisation analysis
;*******************************	; selection button group
	COMMON countCom
	COMMON countConstants	

	po=event.VALUE			;po should contain current polarisation
					;analysis selection.

	;Disbale or enable flip / non flip parameter field as appropriate
	if po EQ NOPOL THEN disableRatio ELSE enableRatio
END

FUNCTION unitButtons_event, event	; Event handler for the unit selection
;********************************	; button group. (used to set the 't' or 'm'
					; keywords for the mad 'co' command)

	COMMON countCom
	COMMON countConstants

	if event.VALUE eq MONITORUNIT THEN BEGIN
		enableMonitorUnits
	END ELSE BEGIN
		enableTimeUnits
	END
		
END

FUNCTION saveButton_event, event	; Event handler for the save / nosave
;*******************************	; button
	COMMON countCom
	save=1-save
END





FUNCTION okButton_event, event		; Event handler for the OK button. This
;*****************************		; function validates the scan and count
					; parameters, calculates appropriate
					; presets, and generates the appropriate
					; MAD commands

	MaxPresetB=300	;Range of acceptable preset_B values
	MinPresetB=150

	COMMON countCom
	COMMON countConstants	
	
	 ;Retrieve widget values

	WIDGET_CONTROL, repeatField, GET_VALUE=repeats


	IF unit EQ 'm' THEN $
		WIDGET_CONTROL, countsField, GET_VALUE=duration $
	ELSE BEGIN
		WIDGET_CONTROL, hoursField, GET_VALUE=hours
		WIDGET_CONTROL, minsField, GET_VALUE=mins
		WIDGET_CONTROL, secsField, GET_VALUE=secs

		duration=secs+60*mins + 3600 * hours
	END

	WIDGET_CONTROL, countRatioField, GET_VALUE=ratio

	WIDGET_CONTROL, scanFromField, GET_VALUE=from
	WIDGET_CONTROL, scanToField, GET_VALUE=to
	WIDGET_CONTROL, scanStepField, GET_VALUE=step
	WIDGET_CONTROL, scanNumStepsField, GET_VALUE=numSteps
	
	WIDGET_CONTROL, titleField, GET_VALUE=title
	If save THEN savetxt='save' ELSE savetxt='nosave'
	
	

   	 ;process polarisation analysis setting
	CASE po OF
		NOPOL:poltxt=' co nopo ' 
		ZPOL:poltxt=' co zpo '
		XYZPOL:poltxt=' co xyzpo '
		ELSE:BEGIN
		      PRINT, "ERROR! Invalid polarisation setting" 
		      RETURN, 0
		     END
	ENDCASE

	 ;process scan setting
	CASE scan OF
		NOSCAN: scantxt='' 
		THETASCAN: scantxt=' scan 2theta '
		OMEGASCAN: scantxt=' scan omega '
		ROTATIONSCAN: scantxt=' scan rotation '		
		ELSE: BEGIN
			PRINT, "ERROR! Invalid scan setting"
			RETURN, 0
		      END
	ENDCASE

	 ;Validate count duration
	IF duration LE 0 THEN BEGIN
		dummy=WIDGET_MESSAGE('Duration of count must be > 0', /ERROR)
		RETURN, 0
	END
	
	 ;Validate repeats
	IF repeats LT 1 THEN BEGIN
		dummy=WIDGET_MESSAGE('Repeats must be >= 1', /ERROR)
		RETURN, 0
	END
	

	 ;Validate scan parameters
	IF scan NE NOSCAN AND scan NE THETASCAN THEN BEGIN
		
		 ;Check for zero step size
		IF step EQ 0 THEN BEGIN
		 	dummy=WIDGET_MESSAGE('Scan step size must be non zero',/ERROR)
			RETURN, 0
		END

		 ;Check to ensure step is in correct direction
		IF ((to-from)/step) LT 0 THEN BEGIN
		   dummy=WIDGET_MESSAGE('Scan step is in wrong direction', /ERROR)
			   RETURN, 0
	        END
		
		 ;ensure that numSteps contains the correct value. (It should already,
		 ;this is just to double-check)

		numSteps=calcNumSteps(from,to,step)

		;Add the scan paramters to the scan command string
		scantxt=scantxt+STRING(from) + ' '+STRING(to) +' '+ STRING(step)+' '
	ENDIF $
        ELSE IF scan EQ THETASCAN THEN BEGIN
		;Check number of steps
		IF numSteps LE 0 THEN BEGIN
			dummy=WIDGET_MESSAGE('Number of scan steps must be > 0', /ERROR)
			RETURN, 0
		END	
	ENDIF

	
	
	 ;If we are not scanning, then we set numSteps to one so that newDuration
	 ; is calculated correctly. (see below)
	
	IF scan EQ NOSCAN THEN numSteps=1
 
	 ;Calculate preset time. 
	 ;This is calculated such that the Duration field will contain
	 ;the total time for the entire scan / count.
	 ;
	 ;We also recalculate the duration based on our new preset to 
	 ;account for rounding. 
	
	
	CASE po OF
		NOPOL: BEGIN
			preset=ROUND(FLOAT(duration)/repeats)
			newDuration=preset*repeats
		       END

		ZPOL: BEGIN
			preset=ROUND(FLOAT(duration)/((ratio+1.)*repeats))
			newDuration=LONG(preset*((ratio+1.)*repeats))
		      END

		XYZPOL: BEGIN
		          preset=ROUND(FLOAT(duration)/(4.*repeats*(ratio+1.)))
			  newDuration=LONG(preset*(4*repeats*(ratio+1.)))
			END
	ENDCASE
	

	 ;Now we check for rounding errors. If such errors exist we present the user
	 ;with a recalculated total duration for the scan and ask him whether to
	 ;proceed with the new value

	IF duration NE newDuration THEN BEGIN
	   IF unit EQ 't' THEN BEGIN
		temp=sec2Time(newDuration)
		newText=STRING(temp(0))+'days'+STRING(temp(1))+'hrs'+$ 
			    STRING(temp(2))+'mins' +STRING(temp(3))+'secs'
	   END ELSE newText=STRING(newDuration)

	   IF WIDGET_MESSAGE(['The duration you entered cannot be reconciled with', $
			      'the repeat, flip non-flip ratio and scan parameters',$
			      'The closest duration is: '+newText, 		    $
			      'Proceed with this new duration?'], /QUESTION) EQ 'No'$
		THEN RETURN, 0
	END
	duration=newDuration




	 ;calculate preset_b. 
	 ;	            preset must be an integer multiple of 
	 ; preset_b. The procedure will attempt to find a suitable
	 ; preset_b number in the range MinPresetB to MaxPresetB.
	 ; If the calculated preset time is < MinPresetB then we
	 ; set preset_b = preset.
	 ;
	 ; NB for some preset values a suitable preset_b may not 
	 ; exist. Care should therefore be taken when specifying
	 ; the total duration.

	preset_b=-1
	IF preset LT MinPresetB  THEN preset_b=preset ELSE $
		FOR i=MaxPresetB, MinPresetB, -1 DO IF (preset MOD i) EQ 0 THEN $
								preset_b=i
	
	IF preset_b EQ -1 THEN BEGIN
		dummy=WIDGET_MESSAGE('No acceptable preset_b value exists. ' + $
			       'please try another duration', /ERROR)
		RETURN, 0
	END

	;build MAD commands

	ratstr=STRTRIM(STRING(ratio),2)
AS0:	n=STRLEN(ratstr)	& i=RSTRPOS(ratstr,'0')
	IF (i EQ n-1) THEN BEGIN
		ratstr=STRMID(ratstr,0,n-1)
		GOTO, AS0
        ENDIF		

	IF po EQ ZPOL THEN $
		madCommand='par preset_z '+ratstr $
	ELSE IF po EQ XYZPOL THEN  $
		madCommand='par preset_xyz '+ratstr 
	
	
	if po EQ NOPOL THEN $
		madCommand='par preset_b '+STRING(preset_b) $
	ELSE $
		madCommand=[[madCommand],'par preset_b '+STRING(preset_b)]
	
	madCommand=[[madCommand],scantxt+poltxt+STRING(preset)+' '+unit+' '+ $ 
		    STRING(repeats)+' '+ savetxt +' title '+ title]

		
	
	PRINT, 'MAD: ', madCommand

	 ;Send the commands to MAD

	dummy=dial_mad_send('',0,madCommand,'')

	
	 ;calculate start and finish times
	
	
	totalDuration=numSteps*duration ;duration is originally time per scan position

	startTime=systime(1)  ;record the time that the scan started

	IF unit EQ 't' THEN BEGIN
		totalDuration=sec2time(totalDuration)	;convert duration into days/
							;hrs/mins/secs

		durationtxt=STRING(totalDuration(0))+'days'+ $
			    STRING(totalDuration(1))+'hrs'+ $ 
			    STRING(totalDuration(2))+'mins' + $
			    STRING(totalDuration(3))+'secs'

	
		durationtxt=STRCOMPRESS(durationtxt)
		finish=timeAfter(now(), totalDuration)

		finishtxt=time2string(finish)

		 ;give the user a message
		message='Command sent to MAD'
		message=[[message],'Scan started: '+systime()]
		message=[[message],'Length of scan is: '+durationtxt]
		message=[[message],'Scan should finish at: '+finishtxt]
		dummy=WIDGET_MESSAGE(message, /INFORMATION)
	END ELSE BEGIN
		dummy=WIDGET_MESSAGE('Scan started: '+systime(), /INFORMATION)
	END

	WIDGET_CONTROL, event.top, /DESTROY

END





FUNCTION cancelButton_event, event	; Event handler for the cancel button
;*********************************
	
	COMMON countCom
	startTime=-1
	WIDGET_CONTROL, event.Top, /DESTROY
END

FUNCTION helpButton_event, event	; Event handler for the help button
;*******************************
	dummy=WIDGET_MESSAGE(['Title: The title to give to the run(s).', $
			      'Save: Save results.', $
			   'Scan: Type of scan to perform.', $
			   'From/To/By: scan parameters for omega and rotation scans',$
			   'Num Steps: Number of scan steps. This is entered',$
			   '           manually for 2theta scans and is calculated',$
			   '           automatically for other scan types',$
			   'Polarisation Analysis: The kind of polarisation analysis',$
			   '                       to perform',$
			   'Flip/non-flip ratio: Ratio of time spent counting', $
			   '                     spin flipped and non spin flipped',$
			   '                     neutrons.',$
			   'Repeat: No. of times to repeat each count', $
			   'Total Duration: Total length of entire scan', $
			   'Units: Specifies whether the above is given in seconds', $
			   '	      or monitor 1 counts'], /INFORMATION)
END



FUNCTION doCount, defaults
;*************************
		 ;The Main function. This procedure creates the interface and 
		 ;registers the application with the IDL XMANAGER
		 ;returns 0 if count started, -1 if count was cancelled
 	
	IF XREGISTERED('Count') THEN BEGIN 	;ensure no other copies running
					PRINT, "docount is already running"
					RETURN, -1
				     END
	
	 ;Setup constants used for radio button values. This is done so that the
	 ;radio button text can be changed here and still be picked up correctly
	 ;by the event handlers

	COMMON countConstants

	NOSCAN='None'
	THETASCAN='2theta'
	OMEGASCAN='Omega'
	ROTATIONSCAN='Rotation'
	
	NOPOL='None'
	ZPOL='Z Only'
	XYZPOL='XYZ'
	MONITORUNIT='Monitor'
	TIMEUNIT='Time'

	
	COMMON countCom

	time	;load the time library (there is probably a better way of doing this)

	
	cancelFlag=0; 	Will be set to -1 if count is cancelled

	  ;Create the interface

	mainWindow=WIDGET_BASE(TITLE='D7 Count parameters', ROW=5)
	
	  ;Output setup

	titleBase=WIDGET_BASE(mainWindow, /ROW)
	titleField=CW_FIELD(titleBase, /STRING, TITLE='Count Title:')
	saveButton=CW_BGROUP(titleBase, ['Save'], /NONEXCLUSIVE, /RETURN_NAME, $
				EVENT_FUNCT='saveButton_event')

	  ; Scan Setup

	scanBase=WIDGET_BASE(mainWindow, /FRAME, COLUMN=2)
	scanButtons=CW_BGROUP(scanBase,[NOSCAN, $
					THETASCAN, $
					OMEGASCAN, $
					ROTATIONSCAN], /EXCLUSIVE, $
			       LABEL_LEFT='Scan: ', / RETURN_NAME, /FRAME, $
			       EVENT_FUNCT='scanButtons_event', $
			       IDS=scanButtonID)
			       
	scanFieldBase=WIDGET_BASE(scanBase, /COL)
	scanFromField=CW_FIELD(scanFieldBase, /FLOATING, TITLE='Scan From:', $
			       	XSIZE=8, /ALL_EVENTS)

	scanToField=CW_FIELD(scanFieldBase, /FLOATING, TITLE='  Scan To:', $
			  	XSIZE=8, /ALL_EVENTS)

	scanStepField=CW_FIELD(scanFieldBase, /FLOATING, TITLE='  Step By:', $
				XSIZE=8, /ALL_EVENTS)

	scanNumStepsField=CW_FIELD(scanFieldBase, /LONG, TITLE='Num Steps:', $
				XSIZE=8, /ALL_EVENTS, /NOEDIT)
	

	  ;Polarisation Analysis setup
	
	polBase=WIDGET_BASE(mainWindow, /COL, /FRAME)
	polButtons=CW_BGROUP(polBase, [NOPOL, $
					  ZPOL, $
					  XYZPOL], /EXCLUSIVE, $
			     LABEL_LEFT='Polarisation Analysis: ',$
			     /RETURN_NAME, /FRAME, $
			     EVENT_FUNCT='polButtons_event', $
			     IDS=polButtonID)
	

	

	countRatioField=CW_FIELD(polBase, /FLOATING, $
				 TITLE='Flip / non-flip ratio: ', XSIZE=4)

	  ;Count time setup

	totalTimeBase=WIDGET_BASE(mainWindow, ROW=2, /FRAME)

	repeatField=CW_FIELD(totalTimeBase, /INTEGER, TITLE='Repeat: ',$
                             XSIZE=4, /ALL_EVENTS, UVALUE='repeatField')

	subBase=WIDGET_BASE(totalTimeBase, ROW=3, /FRAME)

	 ;NB The fields in this base are generated dynamically depending on 
	 ;the state of unitsButtons

	
	
	timeLabel=WIDGET_LABEL(subBase, VALUE='Duration per scan position')


	

	
	timeInputBase=WIDGET_BASE(subBase, /ROW)

	 ;NO_RELEASE keyword added for IDL v. 5.2. Without this keyword
	 ;two events will be generated each time a button is selected.
	 ;This causes either enableMonitorUnits or enableTimeUnits to be
	 ;called twice whcih means that the old fields are not destroyed
	 ;properly when the new ones are dynamically created
	
	unitsButtons=CW_BGROUP(subBase, [TIMEUNIT, MONITORUNIT], $
			       /EXCLUSIVE, LABEL_LEFT='Units:', /FRAME, $
			       IDS=unitButtonID, EVENT_FUNCT='unitButtons_event',$
                               /RETURN_NAME, /NO_RELEASE)
	;Footer setup
	
	footerBase=WIDGET_BASE(mainWindow, /ROW, /FRAME)
	okButton=WIDGET_BUTTON(footerBase, VALUE='OK', EVENT_FUNC='okButton_event')
	cancelButton=WIDGET_BUTTON(footerBase, VALUE='Cancel', $
				    EVENT_FUNC='cancelButton_event')

	helpButton=WIDGET_BUTTON(footerBase, VALUE='Help', /HELP, $
				 EVENT_FUNC='helpButton_event')	


	setDefaults, defaults ;set default values

	;Paint and setup event loop

	WIDGET_CONTROL, mainWindow,	/REALIZE

	CATCH, Error_status	;Establish error handler
	
	;If an error occurs we pop up a message box with the error 
	;message. If the user clicks cancel then we quit the program
	;otherwise execution continues as normal.

	IF Error_status NE 0 THEN BEGIN
		result=WIDGET_MESSAGE(!ERR_STRING, /ERROR, /CANCEL)
		IF result EQ 'Cancel' THEN BEGIN
						WIDGET_CONTROL, /RESET
						RETURN, -1
					   END
	END


	XMANAGER,'Count',mainWindow, /MODAL


	;Fill out return data structure
	CASE scan OF
		NOSCAN: scantxt=''
		THETASCAN: scantxt='2theta'
		OMEGASCAN: scantxt='omega'
		ROTATIONSCAN: scantxt='rotation'
	ENDCASE

	CASE po OF
		NOPOL: poltxt='NOP'
		ZPOl: poltxt='ZP'
		XYZPOL: poltxt='XYZP'
	ENDCASE
	
	RETURN, {title:title, po:poltxt, scan:scantxt, duration:duration, unit: unit,$
		 repeats:repeats, save:save, ratio:ratio, from:from, to:to, $
	         step:step, numSteps: numSteps, startTime:startTime}


END
