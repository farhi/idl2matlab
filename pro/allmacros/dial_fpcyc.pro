;*********************
PRO dial_fpcyc_macro, D
;*********************
;**
;** The Dial macro
;** Input D is the dial structure as defined below by the function dial_fpcyc
;** This macro procedure is called by George every D.frequency seconds

  
  LF=string(10b)
  length=STRLEN(D.search1)
  value=0.0
  error=0.0
  success=0
  IF STRPOS(D.os,'vms') EQ -1 AND STRPOS(D.os,'Win') EQ -1 THEN BEGIN
    list=FINDFILE(D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER-1,/REMOVE)+'.sum',count=test)
    PRINT,D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER-1,/REMOVE)+'.sum'
    IF test EQ 1 OR D.init eq 0  THEN  BEGIN
	  PRINT,'I am running ...'
      IF test EQ 1 THEN BEGIN
	    PRINT,'... not for the first time'
        OPENR,file,D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER-1,/REMOVE)+'.sum',/get_lun
        line=""
        WHILE STRPOS(line,D.search0) EQ -1 AND NOT EOF(file) DO BEGIN
          READF,file,line
        ENDWHILE
        IF STRPOS(line,D.search0) NE -1 THEN BEGIN
          WHILE STRPOS(line,D.search1) EQ -1 AND NOT EOF(file) DO BEGIN
             READF,file,line
          ENDWHILE
          IF STRPOS(line,D.search1) NE -1 THEN BEGIN
            position=STRPOS(line,D.search1)
            text=STRMID(line,position+length,strlen(line)-position-length)
            IF D.newline THEN READF,file,text
            IF D.rderr THEN READS,text,value,error ELSE READS,text,value
            D.value=value
            D.error=error
            D.xvalue=D.Runnumber-1
            PRINT,D.xvalue,D.value,D.error
            success=1
          ENDIF ELSE PRINT,D.search0+' not found in',D.RUNNUMBER-1
        ENDIF ELSE PRINT,D.search1+' not found in',D.RUNNUMBER-1
        FREE_LUN,file
      ENDIF ELSE PRINT,'... for the first time'
      list=FINDFILE(D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER-1,/REMOVE)+'.pcr',count=test)
	  PRINT,D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER-1,/REMOVE)+'.pcr'
	  PRINT,D.RUNNUMBER-1,'=>',D.RUNNUMBER
      IF (test EQ 1 OR D.init eq 0) AND success EQ 1 THEN BEGIN
        IF D.init NE 0 THEN BEGIN
          IF STRPOS(D.os,'MacOS') NE -1 THEN BEGIN
            DO_APPLE_SCRIPT,'tell application "Finder"'+lf+$
                            'duplicate file "'+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER-1,/REMOVE)+'.pcr"'+lf+$
	                    'try'+lf+'delete file "'+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.pcr"'+lf+'end try'+lf+$
  	                    'set name of file "'+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER-1,/REMOVE)+'.pcr copy" to "'+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.pcr"'+lf+$
                            'end tell'
          ENDIF ELSE BEGIN
		    SPAWN,"cp "+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER-1,/REMOVE)+".pcr "+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+".pcr"
		    PRINT,"cp "+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER-1,/REMOVE)+".pcr "+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+".pcr"
          ENDELSE
        ENDIF ELSE PRINT,'nothing to copy yet'
        list=FINDFILE(D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.sum',count=test)
        IF test EQ 1 THEN BEGIN
          IF STRPOS(D.os,'MacOS') NE -1 THEN BEGIN
            DO_APPLE_SCRIPT,'tell application "Finder"'+lf+$
	                    'delete file "'+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.sum"'+lf+$
                            'end tell'
          ENDIF ELSE BEGIN
		    SPAWN,"rm "+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.sum'
		    PRINT,"rm "+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.sum'
	      ENDELSE
        ENDIF
        list=FINDFILE(D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.dat',count=test)
 		PRINT,D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.dat is searched'
        IF test EQ 1 THEN BEGIN
		  PRINT,D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.dat is present'
          list=FINDFILE(D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.pcr',count=test)
          IF test EQ 1 THEN BEGIN
		    PRINT,D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.pcr as well'
            IF STRPOS(D.os,'MacOS') NE -1 THEN BEGIN
              DO_APPLE_SCRIPT,'tell application "Finder"'+lf+$
                              'open file "'+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+'.pcr" using file "HansenG3:Applications (Mac OS 9):JFullProf:FullProf"'+lf+$
                              'end tell'
            ENDIF ELSE BEGIN
			  SPAWN,"fullprof "+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+" &"
			  PRINT,"fullprof "+D.FILEPATH+D.FILENAME+STRCOMPRESS(D.RUNNUMBER,/REMOVE)+" &"
			ENDELSE
            D.runnumber=D.runnumber+1
          ENDIF
        ENDIF ELSE D.onoff=0
      ENDIF
    ENDIF ELSE IF D.init eq 0 THEN BEGIN
    ENDIF
  ENDIF ELSE D.onoff=0
  IF D.init eq 0 THEN  D.init=1  
end



;*********************
FUNCTION dial_fpcyc
;*********************
;**
;** The dial initialisation

   ;Dial Variables (Defaulted if not present in return statement)
   ;--------------
   ;GENERIC='mad'    ;connect to the mad-idl interface
   ;TYPE='monitor'   ;then V=DialNewValue() stands for V=DialNewValue(TYPE='monitor')
    ONOFF=0          ;state of the Dial 1=running
    FREQUENCY=60.    ;the Dial macro is executed each frequency seconds. if =0 then the general frequency is used
    VALUE=0.0        ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var.
    ERROR=0.0        ;value you assign to the Dial. This value is automaticaly plotted. put errors in ERROR var.
    PLOT=10           ;-2=none 0=plot 1=surface 2=contour n>2 means show vector of last n scalar values
    UPPERLIM=0.      ;upper limit of the plot (LOWERLIM for lower limit)
    HISTORY=0        ;=1 to record values in file fpcyc.his
    DURATION=0       ;if >0 then Dial is stopped after running duration seconds
    WUPDATE=0        ;=1 to automaticaly update corresponding workspace, =-1 silent!
                     ;=2 to automaticaly update and plot workspace to the main window
                     ;   0,1,2 are set by pressing the left,middle,right mouse button on the dial snapshot

   ;User Variables (Must be present in return statement to be available)
   ;-------------
    search0='Conventional'
    search1='Rwp:'
    newline=0
    rderr=0
    OS=!VERSION.OS
    LF=string(10b)
    FILENAME='khp'
    FILEPATH=''
    RUNNUMBER=1000        
    INIT=0           ;may be used in fpcyc_macro when started or on reset
    xvalue=0

return, {ERROR:error,VALUE:value,PLOT:plot,ONOFF:onoff,FREQUENCY:frequency,INIT:init,$
         OS:os,FILENAME:filename,FILEPATH:filepath,RUNNUMBER:runnumber,$
         SEARCH0:search0,SEARCH1:search1,RDERR:rderr,NEWLINE:newline,XVALUE:xvalue}
end






;***************************************************************************************

;** Usefull calls to be used in procedure dial_fpcyc_macro :
;** *************
;** V=DialNewValue([/SETVALUE],[COMMENT=txt]   ;Get a new value from DIAL_'generic'_READ
;**                [TYPE='monitor'])           (a request is made to the instrument)
;**                                            (/SETVALUE means D.value is set to V)
;** C=DialControl ('command syntax',[CHECK=.5]);Send a command to the instrument control
;**                                            (CHECK means check every .5 sec till the
;**                                             command  is complete)
;** DialTag   ,    'temp2',TAG='VALUE',GET=V   ;Return V,the value for the tag  'VALUE'
;**                                                                of  the dial 'temp2'
;** DialTag   ,    'temp3',TAG='ONOFF',SET=1   ;Set to 1 the value of  the tag  'ONOFF'
;** DialStart ,    'temp3'                     ;A short  for previous call
;** DialStop  ,    'temp3'                     ;A short  too
;**
;** DialModValue,   V ,[tag='VALUE']           ;Set the new value for current dial or
;** D.value   =     V                          ;modify yourself the tag Value if type &
;**                                            ;dimensions don't change.(same for Error)
;** D.upperlim=   150.                         ;Set upper limit for plotting.
;**
;** R=DialOn ()                                ;Return 0 if Dial has been interrupted
;**                                            (To use inside loops)
;** DialInit,      'template4',[NEW='tmp4']    ;Initiate dial  'template4' from file:
;**                           ,[PATH=path ]                dial_template4.pro
;**                                            (You may change its name to 'tmp4' and
;**                                            (use DialStart,'tmp4' to activate it)
;** DialMacro,     'template4'                 ;Force execution of DIAL_TEMPLATE4_MACRO
;**                                            ('template4'  is keept inactive, ONOFF=0)
;** DialClear,     'template4'                 ;Suppress dial  'template4' from memory
;** DialWSet                                   ;Reserve central draw window for next plot
;**
;** DialsFrequency,[GET=freq],[SET=.5],[/STOP] ;Set  or Get the general frequency value
;**                [DURATION=90.] ,   [/START] ;              (time is in seconds)
;**                                            ;Stop or Start the general process
;**                                            ;Set  Time  limit for the active process
