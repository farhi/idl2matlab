
PRO Build_MacFileListFromArray ,file_array ,macFile_List
;** **************************
;**
	
	; Entree :  ['toto', 'tata', 'titi']  			(IDL Array)
	; Sortie : "{file toto, file tata, file titi}	String
		
	macFile_List = ''
	FOR Index=0,n_elements(file_array)-1 DO BEGIN
	
		IF (INDEX EQ 0) THEN $
			macFile_List = 'file "' + file_array(Index) + '"' $
		ELSE $
			macFile_List = macFile_List + ', file "' + file_array(Index) + '"'

	END
	
	macFile_List = '{' + macFile_List + '}'

	return
	
END ; Build_MacFileListFromArray


PRO Build_MacFileListFromString ,file_list ,macFile_List
;** ***************************
;**
	
	; Entree : "toto tata titi"						String
	; Sortie : "{file toto, file tata, file titi}	String
		
	fileArray = str_sep(file_list, ' ')		; Found in IDL Lib
	
	Build_MacFileListFromArray, fileArray ,macFile_List
	return

END ; Build_MacFileListFromString

PRO Build_ToolServerFileListFromString, sourceFolder, file_list, toolServer_List
;** **********************************
;**
	
	; Entree : 'sourceFolder' 'toto tata titi'										String
	; Sortie : '\"sourceFolder:toto\" \"sourceFolder:tata\" \"sourceFolder:titi\"'	String
		
	fileArray = str_sep(file_list, ' ')		; Found in IDL Lib
	
	toolServer_List = ''
	FOR Index=0,n_elements(fileArray)-1 DO BEGIN
	
		toolServer_List = toolServer_List + '\"' + sourceFolder + fileArray(Index) + '\" '

	END
	
	return
	
END ; Build_ToolServerFileListFromString

PRO lamp_resource, txts
;** *************
;**
txts=[$
"*clientAutoPlace: off","",$

"Idl.visual:  PseudoColor",$
"Idl.retain:  2",$
"Idl.noBorder:True",$
"Idl.colors:  -18","",$

"Idl*Background:  		  paleturquoise",$
"Idl*Foreground:  		  black",$
"Idl*XmText*background:	  	  deepskyblue2",$
"Idl*XmList*background:	  	  deepskyblue3",$
"Idl*XmPushButton*background:	  paleturquoise2",$
"Idl*XmCascadeButton*background:   paleturquoise2",$
"Idl*XmToggleButton*selectColor:   deepskyblue4","",$

"Idl*Annotate*Background:     		paleturquoise",$
"Idl*IDL*Background:     		paleturquoise",$
"Idl*Xloadct*Background:     		paleturquoise","",$

"Idl*scan*Background:     		deepskyblue2",$
"Idl*scan*XmPushButton*background:   	paleturquoise2",$
"Idl*scan*XmPushButton*foreground:	deepskyblue4",$
"Idl*scan*XmCascadeButton*background:	paleturquoise2",$
"Idl*scan*XmCascadeButton*foreground:	deepskyblue4",$
"Idl*scan*XmToggleButton*selectColor:	paleturquoise2",""]
txts=[txts,$
"Idl*lamp*Background:     		paleturquoise",$
"Idl*lamp*XmLabel*foreground:	  	black",$
"Idl*lamp*spelab1*foreground:	  	white",$
"Idl*lamp*spelab2*foreground:	  	deepskyblue1",$
"Idl*lamp*spelab3*foreground:	  	deepskyblue2",$
"Idl*lamp*spelab4*foreground:	  	deepskyblue3",$
"Idl*lamp*spelab5*foreground:	  	deepskyblue4",$
"Idl*lamp*spelab6*foreground:	  	paleturquoise1",$
"Idl*lamp*spelab7*foreground:	  	paleturquoise2",$
"Idl*lamp*spelab8*foreground:	  	paleturquoise3",$
"Idl*lamp*spelab9*foreground:	  	lightblue1",$
"Idl*lamp*discret*Background:		paleturquoise",$
"Idl*lamp*discret*Foreground:		black","",""]
txts=[txts,$
"Idl*lamp*mic*Background: deepskyblue1",$
"Idl*lampmic*Background:  deepskyblue1",$
"Idl*lamp*did*Background: deepskyblue2",$
"Idl*lampdid*Background:  deepskyblue2",$
"Idl*lamp*don*Background: deepskyblue3",$
"Idl*lampdon*Background:  deepskyblue3",$
"Idl*lamp*ben*Background: deepskyblue4",$
"Idl*lampben*Background:  deepskyblue4",""]
txts=[txts,$
"Idl*lamp*mic*geo*Background:			lightblue1",$
"Idl*lamp*mic*geo*XmPushButton*background:	lightblue1",$
"Idl*lamp*mic*geo*XmPushButton*foreground:	deepskyblue4",$
"Idl*lamp*mic*geo*XmLabel*foreground:		deepskyblue4",$
"Idl*lamp*mic*geo*XmText*background:		lightblue1",$
"Idl*lamp*mic*geo*XmText*foreground:		deepskyblue4",$
"Idl*lamp*geo*Background:			lightblue1",$
"Idl*lamp*geo*foreground:			deepskyblue4",$
"Idl*lamp*geo*XmPushButton*background:		lightblue1",$
"Idl*lamp*geo*XmPushButton*foreground:		deepskyblue4",$
"Idl*lamp*geo*XmCascadeButton*Background:	lightblue1",$
"Idl*lamp*geo*XmCascadeButton*foreground:	deepskyblue4",$
"Idl*lamp*did*geo*Background:			lightblue1",$
"Idl*lamp*did*geo*foreground:			deepskyblue4",$
"Idl*lamp*did*geo*XmText*background:		lightblue1",$
"Idl*lamp*did*geo*XmText*foreground:		deepskyblue4",$
"Idl*lamp*don*geo*Background:			lightblue1",$
"Idl*lamp*don*geo*foreground:			deepskyblue4",$
"Idl*lamp*don*geo*XmText*background:		lightblue1",$
"Idl*lamp*don*geo*XmText*foreground:		deepskyblue4",""]
txts=[txts,$
"Idl*lamp*mic*discret*background:	  deepskyblue1",$
"Idl*lamp*mic*discret*foreground:	  deepskyblue4",$
"Idl*lamp*mic*XmPushButton*background:	  paleturquoise1",$
"Idl*lamp*mic*XmPushButton*foreground:	  deepskyblue4",$
"Idl*lamp*mic*XmLabel*foreground:	  deepskyblue4",$
"Idl*lamp*mic*XmToggleButton*selectColor:  paleturquoise1",$
"Idl*lampmic*XmPushButton*background:	  deepskyblue4",$
"Idl*lampmic*XmPushButton*foreground:	  white",$
"Idl*lampmic*XmLabel*foreground:	  	  deepskyblue4",$
"Idl*lampmic*XmCascadeButton*background:	  deepskyblue2",$
"Idl*lampmic*XmCascadeButton*foreground:	  white",$
"Idl*lampmic*XmToggleButton*selectColor:   paleturquoise1",$
"Idl*lampmic*XmText*background:	  	  deepskyblue3",$
"Idl*lampmic*XmText*foreground:	  	  white",$
"Idl*lampmic*red*background:		  white",$
"Idl*lampmic*red*foreground:		  black",""]
txts=[txts,$
"Idl*lamp*did*discret*background:	  deepskyblue2",$
"Idl*lamp*did*discret*foreground:	  deepskyblue4",$
"Idl*lamp*did*XmPushButton*background:	  paleturquoise2",$
"Idl*lamp*did*XmPushButton*foreground:	  deepskyblue4",$
"Idl*lamp*did*XmToggleButton*selectColor:  paleturquoise2",$
"Idl*lampdid*XmPushButton*background:	  paleturquoise2",$
"Idl*lampdid*XmPushButton*foreground:	  deepskyblue4",$
"Idl*lampdid*XmCascadeButton*background:	  paleturquoise2",$
"Idl*lampdid*XmCascadeButton*foreground:	  deepskyblue4",$
"Idl*lampdid*XmToggleButton*selectColor:   deepskyblue4",""]
txts=[txts,$
"Idl*lamp*don*gordon*Background: 		deepskyblue4",$
"Idl*lamp*don*gordon*XmPushButton*background:	paleturquoise3",$
"Idl*lamp*don*gordon*XmPushButton*foreground:	deepskyblue4",$
"Idl*lamp*don*XmPushButton*background:	  deepskyblue4",$
"Idl*lamp*don*XmPushButton*foreground:	  paleturquoise",$
"Idl*lamp*don*XmCascadeButton*background:  deepskyblue4",$
"Idl*lamp*don*XmCascadeButton*foreground:  paleturquoise",$
"Idl*lamp*don*XmToggleButton*selectColor:  deepskyblue4",$
"Idl*lampdon*discret*background:	          deepskyblue3",$
"Idl*lampdon*discret*foreground:	          paleturquoise",$
"Idl*lampdon*XmPushButton*background:	  deepskyblue4",$
"Idl*lampdon*XmPushButton*foreground:	  paleturquoise",$
"Idl*lampdon*XmCascadeButton*background:	  deepskyblue4",$
"Idl*lampdon*XmCascadeButton*foreground:	  paleturquoise",$
"Idl*lampdon*XmToggleButton*selectColor:   deepskyblue4",""]
txts=[txts,$
"Idl*lamp*ben*XmLabel*foreground:	  paleturquoise3",$
"Idl*lamp*ben*foreground:		  white",$
"Idl*lamp*ben*discret*background:	  deepskyblue4",$
"Idl*lamp*ben*discret*foreground:	  paleturquoise3",$
"Idl*lamp*ben*XmPushButton*background:	  paleturquoise3",$
"Idl*lamp*ben*XmPushButton*foreground:	  deepskyblue4",$
"Idl*lamp*ben*XmToggleButton*selectColor:  paleturquoise3",$
"Idl*lamp*ben*XmToggleButton*foreground:   paleturquoise3",$
"Idl*lampben*foreground:			  white",$
"Idl*lampben*XmPushButton*background:	  paleturquoise3",$
"Idl*lampben*XmPushButton*foreground:	  deepskyblue4",$
"Idl*lampben*XmCascadeButton*background:	  paleturquoise3",$
"Idl*lampben*XmToggleButton*selectColor:   paleturquoise3",""]
txts=[txts,$
"Idl*lamptouch*Background:  		  paleturquoise",$
"Idl*lamptouch*Foreground:  		  black",$
"Idl*lamptouch*XmText*background:	  paleturquoise1",$
"Idl*lamptouch*XmList*background:	  deepskyblue3",$
"Idl*lamptouch*XmList*foreground:	  paleturquoise2",$
"Idl*lamptouch*XmPushButton*background:	  paleturquoise2",$
"Idl*lamptouch*XmCascadeButton*background: deepskyblue3",$
"Idl*lamptouch*XmCascadeButton*foreground: white",$
"Idl*lamptouch*XmToggleButton*selectColor: paleturquoise3",$
"Idl*lamptouch*discret*background:	  paleturquoise1",$
"Idl*lamptouch*discret*foreground:	  deepskyblue4",""]
return
end


FUNCTION sys_dep, flag, p1,p2,p3,p4
;******* *******
;**
common c_def,c_init,Lamp_Dir,LF,viewer,ziper,giftrans

os =!VERSION.OS
;Vs=(BYTE(!version.release))(0) - (BYTE('0'))(0)
Vs =fix(strmid(string(!version.release),0,1))
res=0
if !prompt eq 'I2M>' then matlab=1 else matlab=0

IF n_elements(c_init) eq 0 THEN BEGIN c_init=1 & viewer='' & Lamp_Dir='' & ziper=''
	CASE os of
		'MacOS': begin  LF = STRING(13B) & end
		'Win32': begin  & end
		'vms':   begin  ad='sys$login:lamp.resource' & a=findfile(ad,count=cnt)
		                if cnt eq 0 then begin lamp_resource,txts  & u=0 & n=n_elements(txts)-1
		                   ON_ioerror,misrsv & openw,u,ad,/get_lun & for i=0,n do printf,u,txts(i)
		                   free_lun,u & spawn, 'copy/noconf '+ad+' sys$login:decw$xdefaults.dat' & misrsv:
		                endif
		         end
		ELSE:    begin  spawn, 'unalias rm ; unalias cp ; unalias mv'
		                ad=expand_path('~')+'/.lamp.resource' & a=findfile(ad,count=cnt)
		                if cnt eq 0 then begin lamp_resource,txts  & u=0 & n=n_elements(txts)-1
		                     ON_ioerror,misrsu  & openw,u,ad,/get_lun & for i=0,n do printf,u,txts(i)
		                     free_lun,u & cnt=1 & misrsu:
		                endif
		                if (cnt ge 1) and (!D.name ne 'Z') and (!D.name ne 'TEK') then begin
				     scd='/usr/bin/X11/xrdb'     & bid=findfile(scd,count=cnt)
				     ;if cnt lt 1 then begin
				     ;scd='/usr/X11R6/bin/xrdb'   & bid=findfile(scd,count=cnt) & endif
				     if cnt lt 1 then begin
				     scd='/usr/openwin/bin/xrdb' & bid=findfile(scd,count=cnt) & endif

				     if  cnt ge 1 then spawn,  scd + ' -merge '+ad
				endif
		         end
	ENDCASE
END	

CASE flag of

	   ;Add p1 to idl path
	   ;------------------
'ADDPATH': IF strpos(!path,p1) lt 0 then BEGIN CASE os of
      		'vms':   !path=p1+ ',' +!path
     		'MacOS': !path=p1+':,' +!path
      		'Win32': !path=p1+ ';' +!path
      		 ELSE:   !path=p1+ ':' +!path
	   ENDCASE
	   ENDIF

	   ;After Desktop is realized or Stop
	   ;---------------------------------
'AFTER' : BEGIN CASE os of
      		'Win32': if Vs eq 4 then DEVICE,main_window=0
      		 ELSE:
	   ENDCASE
	   END
'AFTES' : BEGIN CASE os of
      		'Win32': if Vs eq 4 then DEVICE,main_window=1
      		 ELSE:
	   ENDCASE
	   END

	   ;Remove all blanks from a string exept for Mac dossiers
	   ;------------------------------------------------------
'BLANKS' : BEGIN CASE os of
      		'MacOS' : res=strtrim    (p1,2) 
      		'Win32' : res=strtrim    (p1,2) 
      		'darwin': res=strtrim    (p1,2) 
      		 ELSE:    res=strcompress(p1,/remove_all)
	   ENDCASE
	   END

	   ;Call a browser
	   ;--------------
'BROWSE' : BEGIN CASE os of
      		'vms':   begin  spawn,'netscape '+p1,/NOWAIT
			         end
      		'Win32': begin  IF !version.release ge '5.4' THEN $
				spawn,/nowait,/hide,'"c:\Program Files\internet explorer\iexplore.exe" '+p1
      		         end
      		'MacOS': begin  spawn, MACCREATOR='MOSS',  p1
      		         end
      		'darwin':begin  spawn,'open '+p1
      		         end
      		 ELSE:   begin  spawn,'netscape -install '+p1 +'&'
			 end
	   ENDCASE
	   END

	   ;Copy files in string p1 (separ. by blancs) from directory p2 to current directory
	   ;----------------------- ------------------ ----------------- --------------------
'COPY':    BEGIN CASE os of
		'vms':   begin
			 tmp=byte(strcompress(strtrim(p1,2)))
			 idx=where(tmp eq 32)
			 if idx(0) gt 0 then tmp(idx)=byte(',')
			 tmp=string(tmp)
      		 	 cd,current=mee
      		 	 if n_elements(p3) gt 0 then mee=mee+p3
			 spawn ,'copy/noconf '+p2+tmp+' '+mee+'*'
			 end
      		'Win32': begin
      		 	 cd,current=mee
      		 	 if n_elements(p3) gt 0 then mei=mee+'\'+p3 else mei=mee
			 IF !version.release ge '5.2' THEN BEGIN
			   files=str_sep(p1,' ') & mei=mei+'\'
			   for ip1 = 0, n_elements (files)-1 do begin
			     if files(ip1) gt ' ' then begin
			        binin =    read_binary(p2 + files(ip1))
			        on_ioerror,free
			        openw   , u, mei+files(ip1), /get_lun
			        writeu  , u, binin
			        free:free_lun, u
			     endif
			   endfor
			 ENDIF ELSE BEGIN
      		 	   cd,p2
      		 	   command_line='cp '+p1+' '+mei
      		 	   spawn, command_line
      		 	   cd,mee
			 ENDELSE
      			 end
      		'MacOS': begin
      			; Get the current dir
      			cd, CURRENT=toFolder
      			; We need to use ToolServer because of the unix WildCard character (*)
				Print, "SD :Copy From " + p2 + " " + p1 + " to folder " + toFolder
				
				; NOTE : WildCard can be very very slow (2 to 5 minutes) on NFS volume mount by "MacNFS" : Don't use it !
				
				; Translate unix WildCard (*) to ToolServer WildCard (Å)
				t1=strcompress(strtrim(p1,2))
				WHILE (STRPOS(t1, '*') NE -1) DO $
					STRPUT, t1, 'Å', Index
				
				Build_ToolServerFileListFromString, p2, t1, toolServerFileList
				toolServerScript = "directory  " + toFolder + " ; " + $
									"duplicate -y " + toolServerFileList + " : ; " + $
									"setfile -t 'ZIVU' -c 'Gzip' Å.Z"
									 
				script = [  'tell application "' + Lamp_Dir + 'Helpers:ToolServer:ToolServer"', $
							'      DoScript "' + toolServerScript + '"', $
							'end tell' ]
							
				DO_APPLE_SCRIPT, script
				; ToolServer ne renvoit pas de rŽsultat pour cette commande ?!?
				res = 1 
      		 	 end
      		 ELSE:   begin
      		 	 cd,current=mee
      		 	 if n_elements(p3) gt 0 then mee=mee+'/'+p3
      		 	 spawn, 'cd '+p2 + '; cp '   + p1 + ' '+mee
      		 	 end
	   ENDCASE
	   END

	   ;Delete file p1
	   ;--------------
'DELET':   BEGIN ON_IOERROR,mis_opd
                 CASE os of
		'vms':   begin OPENR ,out1,p1,/get_lun,/DELETE & FREE_LUN,out1 & end
      		'MacOS': begin OPENR ,out1,p1,/get_lun,/DELETE & FREE_LUN,out1 & end
      		'Win32': begin OPENR ,out1,p1,/get_lun,/DELETE & FREE_LUN,out1 & end
      		'Other': spawn,'rm -f '   +p1
      		 ELSE:   begin OPENR ,out1,p1,/get_lun,/DELETE & FREE_LUN,out1 & end
	   ENDCASE
	   mis_opd:
	   END

	   ;Delete a file list p1
	   ;------------------ --
'DELIST':  BEGIN ON_IOERROR,mis_opdl
                 CASE os of
;		'vms':   begin
;			 ch=p1(0)+';*' & n=n_elements(p1)
;			 if n gt 1 then for i=1,n-1 do ch=ch+','+p1(i)+';*'
;			 spawn,'delete '+ch ,/NOWAIT
;			 end
      		'vms':	 for i=0,n_elements(p1)-1 do begin
      				 OPENR ,out1,p1(i)    ,/get_lun,/DELETE & FREE_LUN,out1 & endfor
      		'MacOS': for i=0,n_elements(p1)-1 do begin
      				 OPENR ,out1,p1(i)    ,/get_lun,/DELETE & FREE_LUN,out1 & endfor
      		'Win32': for i=0,n_elements(p1)-1 do begin
      				 OPENR ,out1,p1(i)    ,/get_lun,/DELETE & FREE_LUN,out1 & endfor
      		 ELSE:   spawn, ['rm' ,'-f' , p1] ,/noshell
	   ENDCASE
	   mis_opdl:
	   END

	   ;List current directory, return a file ascending list
	   ;----------------------  ----------------------------
'DIR':     BEGIN CASE os of
;		'vms':   spawn, 'dir ',p1 ,count=p2
		'vms':   p1=findfile(count=p2)
      		'Win32': begin
      			;CAG - due to the redirection problem in Win32 I am using IDL
      			 cd,current=mee
      			 p1=FINDFILE(mee+'\*',count=p2)
      			 end
       		'MacOS': begin
      			; NOTE : if we pass a non existent reference to the finder,
      			; we will end up in a "dialog with OK button" at the finder level
      			script = [  'tell application "Finder"',$
      						'   if  (exists folder "' + p1 + '") then',$
      						'      name of items in container "' + p1 + '"' + LF,$
      						'   else',$
      						'      set result to ""' + LF,$
      						'   end if' + LF,$
      				     	'end tell' ]
      			DO_APPLE_SCRIPT, script, RESULT=p1
      			res = p1
      			p2 = n_elements(p1)
      		 	end
       		 ELSE:   spawn, 'ls ' ,p1 ,count=p2
	   ENDCASE
	   END

	   ;List directories of current directory with modified date
	   ;---------------- -- ----------------- ---- -------- ----
'DIRD':    BEGIN CASE os of
		'vms':	 spawn,'dir/date' +p1+ '*.dir' ,res
      		'Win32': p3=0 ; CAG need to be implemented
       		'MacOS':
      		 ELSE:   spawn,'ls -ld '  +p1+ '*'    ,res
	   ENDCASE
	   END

	   ;Return the path divider
	   ;-----------------------
'DIVIDER': BEGIN CASE os of
		'vms':   res = ""
      		'Win32': res = "\"
      		'MacOS': res = ":"
      		 ELSE:   res = "/"
	   ENDCASE
	   END

	   ; For IDL versions 4.0.1 and newer, find all label widgets
	   ; rooted and the specified top level widget and set their
	   ; DYNAMIC_RESIZE property.
	   ;------------------------
'DYNLAB':  IF (Vs GE 4) THEN BEGIN

	   IF p2 gt 0 THEN IF !version.release ne '4.0' THEN $
	   CASE os of
      		'Win32':
		 ELSE:	 dynlabel_call, p1
	   ENDCASE
	   
	   CASE os of
		'MacOS': begin	if n_elements(p3) ne 1 then p3=-15
				resizeButton_call, p1, p3
			 end
		 ELSE:
	   ENDCASE
	   ENDIF

	   ;RPC modules entries (ILL use only)
	   ;-------------------
'ENTRY':   BEGIN CASE os of
		'vms':   res='r_mic'
		'sunos': res='r_micc_'    
      		'IRIX':  res='r_micc_'
      		'hp-ux': res='r_micc'
       		 ELSE:   res='r_micc'   
	   ENDCASE
	   END
	   
	   ;RPC modules (ILL use only)
	   ;-----------
'EXEC':    BEGIN CASE os of
		'sunos': res=p1+'/lamp_mac/r_mic_SUN.so'  
      		'IRIX':  res=p1+'/lamp_mac/r_mic_SGI.so'  
      		'hp-ux': res=p1+'/lamp_mac/r_mic_HP.so'  
      		 ELSE:   res=''  
	   ENDCASE
	   END

	   ;EXIT from Lamp application
	   ;--------------------------
'EXIT':    BEGIN CASE os of
		'MacOS': EXIT
		'Win32': EXIT
		 ELSE:	 EXIT
	   ENDCASE
	   END

	   ;Font for drawing area
	   ;---------------------
'FONTD':   BEGIN CASE os of
		'vms':   res='6x13bold'
		'MacOS': res='geneva*9'
		'darwin':res='-schumacher-clean-medium-r-normal--10-*-*-*-*-*-*-*'
		'Win32': res='arial*bold*12'
		 ELSE:	 res='-adobe-courier-medium-r-normal--12-120-*-*-*-*-*-*'
	   ENDCASE
	   END

	   ;Fonts for large(0) middle(1) and small(2) UIs
	   ;---------------------------------------------
'FONTS':   BEGIN
;	   Large UI
     	   propor0   = '-adobe-courier-bold-r-normal--14-140-*-*-*-*-*-*'
     	   biggest0  = '-bitstream-charter-bold-r-normal--19-180-75-75-p-119-*-*' 
     	   bigger0   = '-bitstream-charter-medium-r-normal--17-120-100-100-p-95-*-*'
     	   b_bigger0 = '-bitstream-charter-bold-r-normal--17-120-*-*-*-*-*-*'
     	   normal0   = '-bitstream-charter-medium-r-normal--15-140-75-75-p-84-*-*'
     	   b_normal0 = '-bitstream-charter-bold-i-normal--15-140-75-75-p-93-*-*'
     	   smaller0  = '-bitstream-charter-medium-r-normal--12-120-75-75-p-67-*-*'
     	   smallest0 = '-bitstream-charter-medium-r-normal--10-100-75-75-p-56-*-*'

;	   Middle UI
     	   propor1   = '-adobe-courier-bold-r-normal--14-140-*-*-*-*-*-*'
     	   biggest1  = '-adobe-courier-bold-r-normal--14-140-*-*-*-*-*-*'
     	   bigger1   = '-adobe-courier-medium-r-normal--14-140-*-*-*-*-*-*'
     	   b_bigger1 = '-adobe-courier-bold-r-normal--14-140-*-*-*-*-*-*'
     	   normal1   = '-adobe-times-medium-i-normal--14-140-*-*-*-*-*-*'
     	   b_normal1 = '-adobe-times-bold-i-normal--14-140-*-*-*-*-*-*'
     	   smaller1  = '-adobe-times-medium-r-normal--12-120-*-*-*-*-*-*'
     	   smallest1 = '-adobe-times-medium-r-normal--10-100-*-*-*-*-*-*'

;	   Small UI
    	   propor2   = '-adobe-courier-bold-r-normal--10-100-*-*-*-*-*-*'
    	   biggest2  = propor2
    	   bigger2   = '-adobe-courier-medium-r-normal--10-100-*-*-*-*-*-*'
    	   b_bigger2 = propor2
    	   normal2   = bigger2
    	   b_normal2 = propor2
    	   smaller2  = propor2
    	   smallest2 = bigger2
    	   
    	   res=[[propor0,biggest0,bigger0,b_bigger0,normal0,b_normal0,smaller0,smallest0],$
    	        [propor1,biggest1,bigger1,b_bigger1,normal1,b_normal1,smaller1,smallest1],$
    	        [propor2,biggest2,bigger2,b_bigger2,normal2,b_normal2,smaller2,smallest2]]

 	   CASE os of
		'vms':  begin res(*,0)=res(*,1)
			      res(1,0)='-adobe-times-bold-r-normal--18-180-*-*-*-*-*-*' & end
 		'sunos':begin res(*,0)=res(*,1)
			      res(1,0)='-adobe-times-bold-r-normal--18-180-*-*-*-*-*-*' & end
		'Win32':begin
     			propor0   = 'courier new*bold*14'
     	  		biggest0  = 'arial*bold*19'
     	   		bigger0   = 'arial *17'
     	   		b_bigger0 = 'arial*bold*17'
     	   		normal0   = 'arial *15'
     	   		b_normal0 = 'arial*bold*italic*15'
     	   		smaller0  = 'arial *12'
     	   		smallest0 = 'courier new*12'

                        biggest1  = 'arial*bold*15'
                        b_bigger1 = 'courier new*bold*16'
                        bigger1   = 'arial*bold*14'
                        propor1   = 'courier new*bold*14'
                        normal1   = 'arial*bold*11'
                        b_normal1 = 'arial*bold*14'
                        smaller1  = 'courier new*12'
                        smallest1 =  smaller1

                        biggest2  = 'courier new*bold*16'
                        b_bigger2 = 'courier new*bold*16'
                        bigger2   = 'arial*bold*14'
                        propor2   = 'arial*bold*14'
                        normal2   = 'arial*bold*11'
                        b_normal2 = 'arial*bold*14'
                        smaller2  = 'courier new*12'
                        smallest2 =  smaller2

    	   		res=[[propor0,biggest0,bigger0,b_bigger0,normal0,b_normal0,smaller0,smallest0],$
    	        	     [propor1,biggest1,bigger1,b_bigger1,normal1,b_normal1,smaller1,smallest1],$
    	        	     [propor2,biggest2,bigger2,b_bigger2,normal2,b_normal2,smaller2,smallest2]]
      			end 
      		'MacOS':begin
                        biggest0  = 'geneva*bold*12'
                        b_bigger0 = 'geneva*bold*11'
                        bigger0   = 'geneva*11'
                        propor0   = 'monaco*10'
                        normal0   = 'geneva*10'
                        b_normal0 = 'geneva*bold*10'
                        smaller0  = 'geneva*9'
                        smallest0 = 'geneva*7'

                        biggest1  = 'geneva*bold*12'
                        b_bigger1 = 'geneva*bold*11'
                        bigger1   = 'geneva*11'
                        propor1   = 'monaco*9'
                        normal1   = 'geneva*10'
                        b_normal1 = 'geneva*bold*10'
                        smaller1  = 'geneva*8'
                        smallest1 = 'geneva*7'

                        biggest2  = 'geneva*bold*11'
                        b_bigger2 = 'geneva*bold*10'
                        bigger2   = 'geneva*10'
                        propor2   = 'monaco*9'
                        normal2   = 'monaco*9'
                        b_normal2 = 'geneva*10'
                        smaller2  = 'geneva*8'
                        smallest2 = 'geneva*7'

    	   		res=[[propor0,biggest0,bigger0,b_bigger0,normal0,b_normal0,smaller0,smallest0],$
    	        	     [propor1,biggest1,bigger1,b_bigger1,normal1,b_normal1,smaller1,smallest1],$
    	        	     [propor2,biggest2,bigger2,b_bigger2,normal2,b_normal2,smaller2,smallest2]]
      			end 
               'darwin':begin ; Large UI
                        propor0   = '-schumacher-clean-bold-r-normal--14-140-75-75-c-80-iso646.1991-irv'
                        biggest0  = '-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-15' 
                        bigger0   = '-adobe-helvetica-medium-r-normal--18-180-75-75-p-98-iso8859-15'
                        b_bigger0 = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-14'
                        normal0   = '-adobe-helvetica-medium-r-normal--14-140-75-75-p-77-iso8859-14'
                        b_normal0 = '-adobe-helvetica-bold-o-normal--14-140-75-75-p-82-iso8859-14'
                        smaller0  = '-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-13'
                        smallest0 = '-adobe-helvetica-medium-r-normal--10-100-75-75-p-56-iso8859-10'
                             ; Middle UI
                        propor1   = '-schumacher-clean-bold-r-normal--10-100-75-75-c-60-iso646.1991-irv'
                        biggest1  = '-adobe-helvetica-medium-r-normal--10-*-*-*-*-*-*-*'
                        bigger1   = '-adobe-helvetica-medium-r-normal--10-*-*-*-*-*-*-*'
                        b_bigger1 = '-adobe-helvetica-bold-r-normal--10-*-*-*-*-*-*-*'
                        normal1   = '-adobe-helvetica-medium-r-normal--9-*-*-*-*-*-*-*'
                        b_normal1 = '-adobe-helvetica-bold-r-normal--9-*-*-*-*-*-*-*'
                        smaller1  = '-adobe-helvetica-medium-r-normal--8-*-*-*-*-*-*-*'
                        smallest1 = '-adobe-helvetica-medium-r-normal--8-*-*-*-*-*-*-*'
                             ; Small UI
                        propor2   = '-schumacher-clean-bold-r-normal--10-100-75-75-c-60-iso646.1991-irv'
                        biggest2  = propor2
                        bigger2   = '-schumacher-clean-medium-r-normal--10-100-75-75-c-50-iso646.1991-irv'
                        b_bigger2 = propor2
                        normal2   = bigger2
                        b_normal2 = propor2
                        smaller2  = propor2
                        smallest2 = bigger2

                        res=[[propor0,biggest0,bigger0,b_bigger0,normal0,b_normal0,smaller0,smallest0],$
                             [propor1,biggest1,bigger1,b_bigger1,normal1,b_normal1,smaller1,smallest1],$
                             [propor2,biggest2,bigger2,b_bigger2,normal2,b_normal2,smaller2,smallest2]]
                        end 
       		 ELSE:
	   ENDCASE
	   if matlab then begin
    	   		res=[['ft_propor','ft_biggest','ft_bigger','ft_b_bigger','ft_normal','ft_b_normal','ft_smaller','ft_smallest'],$
    	        	     ['ft_propor','ft_biggest','ft_bigger','ft_b_bigger','ft_normal','ft_b_normal','ft_smaller','ft_smallest'],$
    	        	     ['ft_propor','ft_biggest','ft_bigger','ft_b_bigger','ft_normal','ft_b_normal','ft_smaller','ft_smallest']]
	   endif
	   END

	   ;Environment variable p1: 'LAMP_DIR','USER','HOST','DISPLAY'
	   ;----------------------------------------------------------
'GETENV':  BEGIN res=''
           CASE os of
		'MacOS': begin
		            if (p1 eq 'LAMP_DIR') then begin i = strpos (strlowcase(!path),"lamp_mac")
				      if i le 0       then       i =rstrpos (strlowcase(!path),"lamp")+5
					if i eq 4       then begin i =findfile('lamp*.sav',count=nn)
		                             if nn gt 0 then begin cd,current=res 
		                                        !path=res+':,'+res+':lamp_mac:,' +!path
					           endif else begin
		                                        i  = findfile(!Dir+   ':lamp*.sav',count=nn)
					                      if nn gt 0 then res = !Dir
					           endelse
			            endif else begin res=strmid (!path,0,i-1)
					                 i  =rstrpos( res ,",")
		                             if i ge 0 then  res=strmid ( res ,i+1,80)
					           i  = strpos   (strlowcase(!path),"lamp_mac")
					           if i lt 0 then !path=res+ ':,'+res+':lamp_mac:,' +!path
		                  endelse
					Lamp_Dir = res
		      	endif else if (p1 eq 'USER') then begin res=""
		      				
				;	script =['tell application "' + Lamp_Dir + 'Helpers:ToolServer:ToolServer"'+LF,$
				;		'DoScript "echo -n {user}"' + LF, $
				;		'end tell' ]
				;	DO_APPLE_SCRIPT, script, RESULT=res
      						
		      	endif else if p1 eq 'HOST' then res = "" $
		      	else  if p1 eq 'LAMP_EXEC' then res = "" $
		      	else  if p1 eq 'DISPLAY'   then res = ":0"
		      	end
		'Win32': if p1 eq 'LAMP_DIR' then begin i  = strpos  (strlowcase(!path),"lamp_mac")
		                  if i le 0  then       i  =rstrpos  (strlowcase(!path),"lamp")+5
				  if i eq 4  then begin i  = findfile('..\lamp*.sav',count=nn)
							CD,current=mee
		                             if nn gt 0 then cd,'..' $
		                             else       i  = findfile(   'lamp*.sav',count=nn)
		                             if nn gt 0 then begin cd,current=res 
		                                        !path=res+';'+res+'\lamp_mac;' +!path
					     endif else begin 
		                                        i  = findfile(!Dir+   '\lamp*.sav',count=nn)
					                if nn gt 0 then res = !Dir $
							else begin
					                        i  = findfile(!Dir+'\..\lamp*.sav',count=nn)
					                        if nn gt 0 then begin
					                         cd,!Dir+'\..'  &   cd,current=res & endif
					                endelse
					     endelse
					     CD,mee
		                  endif else begin      res= strmid (!path,0,i-1)
		                                        i  = rstrpos( res ,";")
		                             if i ge 0  then res=strmid ( res ,i+1,80)
					     i  = strpos   (strlowcase(!path),"lamp_mac")
					     if i lt 0 then !path=!path+';'+res+';'+res+'\lamp_mac'
				 endelse
				 Lamp_Dir = res
		         endif else res = getenv(p1)
      	ELSE:    res = getenv(p1)
	   ENDCASE
	   END

	   ;Make transparent gif
	   ;--------------------
'GIFTRANS':BEGIN CASE os of
		'vms':
      		'Win32':
      		'MacOS':
      		 ELSE: begin if n_elements(giftrans) eq 0 then begin
				spawn,'whereis giftrans',ret
      		 	 	cnt=strpos(ret,'/giftrans')
      		 	 	if cnt(0) lt 0 then giftrans=0 else giftrans=1
			     endif
			     if giftrans then $
				spawn,'giftrans -t 0 '+P1+' > '+P1+'t; mv '+P1+'t '+P1
		       end
	   ENDCASE
	   END


	   ;Used for cd,res for home directory
	   ;----------------------------------
'HOME':    BEGIN CASE os of
		'vms':   res='sys$login:'
      		'Win32': cd , current=res
      		'MacOS': cd , current=res
      		 ELSE:   res=expand_path('~')
	   ENDCASE
	   END


	   ;Add LAMP_DIR and LAMP_DIR/lamp_mac to idl path
	   ;-------------------------------------------------
'IDLPATH': IF strpos(!path,p1) lt 0 then BEGIN
 		CASE os of
		'vms':   expa=expand_path('+'+strmid(p1,0,p2-1)+'.lamp_mac]')
		'MacOS': expa=expand_path('+'+p1+':lamp_mac:')
		'Win32': expa=expand_path('+'+p1+'\lamp_mac' )
		 ELSE:   expa=expand_path('+'+p1+'/lamp_mac' )
		ENDCASE
		CASE os of
		'vms':   !path=!path+','     +p1+',' +strmid(p1,0,p2-1)+'.lamp_mac]'+',sys$login:'+','+expa
		'MacOS':;!path=!path+','     +p1+':,'+p1+':lamp_mac:'     +','+expa
		'Win32':;!path=!path+';'     +p1+';' +p1+'\lamp_mac'      +';'+expa
		 ELSE:   !path=!path+':'     +p1+':' +p1+'/lamp_mac' +':~'+':'+expa
		ENDCASE
          ENDIF

	   ;Insert a sub_dir p2 in path p1
	   ;------------------------------
'INSUB':   BEGIN CASE os of
		'vms':   res =strmid(p1,0,strlen(p1)-1)+ '.' +p2 +']'
      		'Win32': res =       p1       +p2 +'\' 
      		'MacOS': res =       p1       +p2 +':' 
      		 ELSE:   res =       p1       +p2 +'/' 
	   ENDCASE
	   END
	   
	   ;Get graphic function number for GXinvert
	   ;----------------------------------------
'INVERT':  BEGIN CASE os of
		'vms':   res=10
      		'Win32': res=10
      		'MacOS': res= 6
      		 ELSE:   res=10
	   ENDCASE
	   END

	   ;Return machine type
	   ;-------------------
'MACHINE': BEGIN CASE os of
		'vms':   res='vms'
      		'Win32': res='win'
      		'MacOS': res='mac'
      		 ELSE:   res='unix'
	   ENDCASE
	   END

	   ;Show manual with a browser
	   ;--------------------------
'MANUAL' : BEGIN CASE os of
      		'vms':   begin  res='netscape [lamp.manual]front.htm'
					cdd=strmid(p1,0,strlen(p1)-1)+'.manual]'
					id=findfile(cdd+'front.htm',count=cnt)
					if (cnt eq 0) and (n_elements(p2) eq 1) then  begin
			                cdd=strmid(p2,0,strlen(p1)-1)+'.manual]'
			                id =findfile(cdd+'front.htm',count=cnt) & endif
		   			if  cnt gt 0 then begin res='netscape '+cdd+'front.htm'
					    spawn,res,/NOWAIT
					endif else res='http://www.ill.fr/data_treat/lamp/front.html'
			         end
      		'Win32': begin  res=p1+'\manual\front.htm'
					cdd=p1+'\manual\'
			    		id=findfile(cdd+'front.htm',count=cnt)
					if (cnt eq 0) and (n_elements(p2) eq 1) then  begin
			                cdd=p2+'\manual\'
			                id =findfile(cdd+'front.htm',count=cnt) & endif
		   	    		if  cnt gt 0 then begin res=cdd+'front.htm'
					endif else res='http://www.ill.fr/data_treat/lamp/front.html'
					IF !version.release ge '5.4' THEN $
					spawn,/nowait,'"c:\Program Files\internet explorer\iexplore.exe" '+res
      		         end
      		'MacOS': begin
      			    	res='MOSS'	; signature de l'application "NetScape"
      			    	cdd = p1+':manual:'
			    		id=findfile(cdd+'front.htm',count=cnt)
					if (cnt eq 0) and (n_elements(p2) eq 1) then  begin
			                cdd=p2+':manual:'
			                id =findfile(cdd+'front.htm',count=cnt) & endif
		   	    		if  cnt gt 0 then begin res=cdd+'front.htm'
					    spawn, MACCREATOR='MOSS', res
					endif else res='http://www.ill.fr/data_treat/lamp/front.html'
      		         end
      		 ELSE:   begin	res='http://www.ill.fr/data_treat/lamp/front.htm'
      				ic ='netscape'
      				spawn,'whereis netscape',ret
      				cnt=strpos(ret,'/netscape')
      				if cnt(0) lt 0 then begin
      		 		   spawn,'whereis mosaic',ret
      				   cnt=strpos(ret,'/mosaic')
      				   if cnt(0) ge 0 then ic ='mosaic' & endif
      				if cnt(0) lt 0 then begin
      		 		   spawn,'whereis mozilla',ret
      				   cnt=strpos(ret,'/mozilla')
      				   if cnt(0) ge 0 then ic ='mozilla'
      				   cnt=[0] & endif
				if os eq 'darwin' then ic ='open'
				if cnt(0) ge 0 then begin
				   if strpos(ic,'netscape') ge 0 then ic=ic+' -install'
		   		   cdd   =p1+'/manual/'
				   id=findfile(cdd+'front.htm',count=cnt)
				   if (cnt eq 0) and (n_elements(p2) eq 1) then begin
			               cdd=p2+'/manual/'
			               id =findfile(cdd+'front.htm',count=cnt) & endif
				   if cnt gt 0 then res=cdd+'front.htm'
				   spawn ,ic + ' ' + res + ' &'
				endif
			 end
	   ENDCASE
	   END

	   ;MAP or not MAP (some devices have problems to add widgets after realizing-> res=1)
	   ;--------------
'MAP':	   BEGIN CASE os of
      		'MacOS': res=1
      		'Win32': IF !version.release lt '5.1' THEN res=1
      		 ELSE:   res=0
	   ENDCASE
	   IF !version.release lt '3.6' THEN res=-1
	   IF res eq 0 THEN $
	      IF (Vs GE 4) THEN $
	   	   IF !version.release ne '4.0' THEN res=2
	   END

	   ;Create a directory
	   ;------------------
'MKDIR':   BEGIN CASE os of
			'vms':   spawn,'create/dir ' +p1
      		'MacOS': begin
      			; D'abord, verifier si c'est possible
      			ii=findfile(p1,count=cnt)
				
				; Si le dossier existe deja, on sort
      			if (cnt gt 0) then return, res
      			      			
      			FileStart = RSTRPOS(p1, ':')+1
      			container = STRMID(p1,0,FileStart)
      			folderName = STRMID(p1, FileStart, 1000)
			 	
      			script = [  'tell application "Finder"', $
      						'	if not (exists folder "' + p1 + '") then', $
      						'    	make new folder at container "' + container + '" with properties {name:"' + folderName + '"}' + LF, $
      						'	end if' + LF, $
      				     	'end tell' ]
      			DO_APPLE_SCRIPT, script
      			res=1
      		 	end
      		'Win32': begin
      			 command_line='mkdir '+p1
			 if !version.release ge '5.4' then key=',/hide' else key=''
      			 res=execute('spawn, command_line'+key)
      			 end
      		 ELSE:   spawn,'mkdir '       +p1
	   ENDCASE
	   END

	   ;Return /home/sub_dir
	   ;--------------------
'NEWDIR':  BEGIN CASE os of
		'vms':   res = '[' +p1+ '.'  +p2 +']'
      		'Win32': begin cd,current=mee
		         res = mee +    '\'  +p2 & end
      		'MacOS': res =      p1+ ':'  +p2
      		 ELSE:   res = '~' +p1+ '/'  +p2
	   ENDCASE
	   END

	   ;Return  path/sub_dir/
	   ;---------------------
'NEWSUB':  BEGIN CASE os of
		'vms':   res =strmid(p1,0,strlen(p1)-1)+ '.' +p2 +']'
      		'Win32': res =       p1+ '\'  +p2 +'\' 
      		'MacOS': res =       p1+ ':'  +p2 +':' 
      		 ELSE:   res =       p1+ '/'  +p2 +'/' 
	   ENDCASE
	   END

	   ;Start player p1 file_path is p2/p3
	   ;----------------------------------
'PLAY_ON': BEGIN CASE os of
		'vms':   begin p4=0
			 spawn, 'playaifc '+p2+p3,/NOWAIT
			 end
      		'Win32': p4=0 ;CAG I haven't got a sound card
      		'MacOS': begin
      			 script = [  'tell application "' + Lamp_Dir + 'Helpers:SoundMachine 2.1"', $
      						'    open file "' + p2 + ':' + p3 + '"' + LF, $
      				     	'end tell' ]
      			 DO_APPLE_SCRIPT, script
				p4=0
      			 res=1
      		 	 end
      		 ELSE:   spawn, p1+p2+'/'+p3+' &' ,PID=p4
	   ENDCASE
	   END

	   ;Kill player
	   ;-----------
'PLAY_OF': BEGIN CASE os of
		'vms':   
      		'Win32': 
      		'MacOS': begin
      			script = [  'tell application "' + Lamp_Dir + 'Helpers:SoundMachine 2.1"', $
      						'    quit', $
      				     	'end tell' ]
      			DO_APPLE_SCRIPT, script
      			res=1
      		 	end
      		 ELSE:   spawn, ['kill' , string(p4)] ,/noshell   
	   ENDCASE
	   END

	   ;return "path_for_player"
	   ;------------------------
'PLAYER':  BEGIN CASE os of
		'vms':   res=getenv('playaifc')
      		'Win32': res=''
      		'MacOS': res='' ;res='SoundMachine 2.1'
		'IRIX':  begin   res=''
				 spawn,'whereis playaifc',str
				 if strpos(str(0),'/playaifc') gt 0 then res='playaifc -rq '
			 end
      		 ELSE:   res=getenv('playaifc')
	   ENDCASE
	   END
	   

	   ;File extension fs problems (vms)
	   ;--------------------------
'POT':     BEGIN CASE os of
		'vms':   p1=p1+'.'
		 ELSE:
	   ENDCASE
	   END
'POT+':    BEGIN CASE os of
		'vms':begin
				j = STRPOS(p1, '.;')
		       	        if j gt 0 then   p1=STRMID(p1,0,j)+p2+'.' $
			        else     begin
					 if p3 eq 1 then begin
					    j =STRPOS(p1, ';')
				            if j gt 0 then p1=STRMID(p1,0,j)+p2 else p1=p1+p2
				         endif else begin
					    j =STRPOS(p1, '.')
				            if j gt 0 then p1=STRMID(p1,0,j+1)+p2
				         endelse
			        endelse
		      end
		 ELSE:p1=p1+p2
	   ENDCASE
	   END
	   
	   
	   ;Print a PS file p2 to a specified printer_name p1
	   ;-------------------------------------------------
'PRINT':   BEGIN CASE os of
		'vms':   spawn,'print/notify/queue='+p1+' '+p2
      		'Win32':begin kp=!D.name & set_plot,"WIN"
			      DEVICE,print_file=p2 & set_plot,kp & end
      		'MacOS':begin
      			;;Do you really want to choose a printer ???
      			;script = [  'tell application "Finder"',$
      			;	    'print file "' + p2 + '"'  ,$
      			;	    'end tell' ]
      			;DO_APPLE_SCRIPT, script, RESULT=res
			;HANSEN!!
      			lf=STRING(13B)
      			CD,CURRENT=current_directory
			if strpos(p2,':') lt 0 then p3=current_directory+p2 else p3=p2
      			PRINT,'print ',p3,' on ', p1
      			script=['tell application "Finder"'+lf+'copy file "'+p3+'" to item "'+p1+'"'+lf+'end tell']
      			PRINT,script
      			DO_APPLE_SCRIPT, script;, RESULT=res
      		 	end
		'linux':begin
			print,'lpr -P'+p1+' '+p2
			spawn,'lpr -P'+p1+' '+p2
			end
      		 ELSE:  begin
			 if strpos(p2,'_s.') gt 0 then print,'lp -c -d'+p1+' '+p2 $
						  else print,'lp -d'   +p1+' '+p2
			 if strpos(p2,'_s.') gt 0 then spawn,'lp -c -d'+p1+' '+p2 $
						  else spawn,'lp -d'   +p1+' '+p2
			end
	   ENDCASE
	   END
	   
	   ;Print a file p1 to the default printer
	   ;--------------------------------------
'PRT_DEF': BEGIN CASE os of
		'vms':   spawn,'print '+p1
      		'Win32':begin kp=!D.name & set_plot,"WIN"
			      DEVICE,print_file=p1 & set_plot,kp & end
      		'MacOS':begin
      			;script = [  'tell application "Finder"', $
      			;	    'print file "' + p1 +'"'   , $
      			;	    'end tell' ]
      			;DO_APPLE_SCRIPT, script, RESULT=res
			;HANSEN!!
      			lf=STRING(13B)
      			CD,CURRENT=current_directory
			if strpos(p1,':') lt 0 then p3=current_directory+p1 else p3=p1
      			PRINT,'print ', p3
      			script = ['tell application "Finder"'+lf+'print file "'+p3+'"'+lf+'end tell']
      			PRINT,script
      			DO_APPLE_SCRIPT, script, RESULT=res
      		 	end
		'linux':spawn,'lpr '  +p1
      		 ELSE:  spawn,'lp '   +p1
	   ENDCASE
	   END

	   ;Color: type of visual
	   ;---------------------
'PSEUDO':  BEGIN CASE os of
      		'Win32': 
      		'linux':;if (!D.name ne 'Z') and (!D.name ne 'TEK') then device, pseudo_color = 8
      		 ELSE:	 if (!D.name ne 'Z') and (!D.name ne 'TEK') then device, pseudo_color = 8
	   ENDCASE
	   if (!D.name ne 'Z') and (!D.name ne 'TEK') then device, decomposed   = 0
	   END

	   ;Byte Order compatibility
	   ;------------------------
'SWAPER':  BEGIN CASE os of
		'vms':   if (p1 eq 'uni') or (p1 eq 'mac') then res=1
      		'Win32': if (p1 eq 'uni') or (p1 eq 'mac') then res=1
		'linux': if (p1 eq 'uni') or (p1 eq 'mac') then res=1
      		'MacOS': if (p1 eq 'vms') or (p1 eq 'win') or (p1 eq 'lin') then res=1
      		 ELSE:   if (p1 eq 'vms') or (p1 eq 'win') or (p1 eq 'lin') then res=1
	   ENDCASE
	   END

	   ;Zip file p1
	   ;-------- --
'ZIP':    BEGIN CASE os of
		'vms':   res=0
      		'Win32': begin
      			 command_line='gzip -fq "'+p1+'"' ;relies on gzip.exe being in the path
			 if !version.release ge '5.4' then key=',/hide' else key=''
      			 res=execute('spawn, command_line'+key)
      			 end
      		'MacOS': res=0
      		 ELSE:   begin	if ziper eq ''  then begin
				   spawn,'whereis zip',str
				   if strpos(str(0),'/zip') gt 0 then ziper='zip' else ziper='-'
				endif
				res=1
				if ziper ne '-' then begin
				   fzip=strmid(p1,0,strpos(p1,'.'))
				   if p2 ne '' then spawn, 'cd '+p2+' ; zip -mq ' +fzip+' ' +p1 
				   if p2 eq '' then spawn,             'zip -mq ' +fzip+' ' +p1 
				endif else res=0
			 end
	   ENDCASE
	   END

	   ;UnZip file p1
	   ;---------- --
'UNZIP':  BEGIN CASE os of
		'vms':   res=0
      		'Win32': begin	if ziper eq ''  then begin
				bid= findfile(!Dir+'\gzip.exe',count=nn)
				if nn gt 0 then ziper=!Dir+'\gzip' else ziper='gzip'
				endif
      			 command_line=ziper+' -dfq "'+p1+'"' ;relies on gzip.exe being in the path
			 if !version.release ge '5.4' then key=',/hide' else key=''
      			 res=execute('spawn, command_line'+key)
      			 end
      		'MacOS': res=0
      		 ELSE:   begin	if ziper eq ''  then begin
				   spawn,'whereis unzip',str
				   if strpos(str(0),'/unzip') gt 0 then ziper='zip' else ziper='-'
				endif
				res=1
				if ziper ne '-' then spawn, 'cd '+p2+' ; unzip -qqux ' +p1 else res=0
			 end
	   ENDCASE
	   END

	   ;Compress file p1
	   ;-------- ---- --
'DO_Z':    BEGIN CASE os of
		'vms':   begin
			 spawn, 'compress -f '+p1
			 spawn, 'rename '+p1+'_Z '+p1+'Z' ,/NOWAIT
			 end
      		'Win32': begin
      			 command_line='gzip -fq "'+p1+'"' ;relies on gzip.exe being in the path
			 if !version.release ge '5.4' then key=',/hide' else key=''
      			 ii=execute('spawn, command_line'+key)
      			 end
      		'MacOS': 
      		 ELSE:   spawn, 'compress -f '   +p1
	   ENDCASE
	   END

	   ;UnCompress file p1
	   ;---------- -------
'UN_Z':    BEGIN CASE os of
		'vms':   spawn, 'gunzip -dfq '+p1  
      		'Win32': begin	if ziper eq ''  then begin
				bid= findfile(!Dir+'\gzip.exe',count=nn)
				if nn gt 0 then ziper=!Dir+'\gzip' else ziper='gzip'
				endif
      			 command_line=ziper+' -dfq "'+p1+'"' ;relies on gzip.exe being in the path
			 if !version.release ge '5.4' then key=',/hide' else key=''
      			 ii=execute('spawn, command_line'+key)
      			 end
      		'MacOS':begin
      			; 'LZIV' = signature de l'application "MacCompress3.2"
      			; spawn, MACCREATOR='LZIV', p1
      			
      			script = [  'tell application "' + Lamp_Dir + 'Helpers:MacGzip¥PPC"',$
							'    open file "' + p1 + '"',$
							'end tell' ]
							
				DO_APPLE_SCRIPT, script
				
				; MacGzip works in the background
				; so we need to wait for the uncompressed files
				uncompressedFile = STRMID(p1, 0, STRLEN(p1)-2) ; Strip ".Z" suffixe
				dummy = FindFile(uncompressedFile, Count=AreYouThere)
				WHILE (AreYouThere EQ 0) DO BEGIN
					Print, "Waiting..."
					Wait, 0.5
					dummy = FindFile(uncompressedFile, Count=AreYouThere)
				ENDWHILE				
				
				res=1

      		 	 end
      		 ELSE:   spawn, 'uncompress -f ' +p1
	   ENDCASE
	   END

	   ;UnCompress file list p1
	   ;---------- ------------
'UN_ZLIS': BEGIN CASE os of
		'vms':   BEGIN
			 ch=p1(0) & n=n_elements(p1)-1
			 if n gt 0 then for i=1,n do ch=ch+' '+p1(i)
			 spawn, 'gunzip -dfq '+ch
			 END
      		'Win32': begin	if ziper eq ''  then begin
				bid= findfile(!Dir+'\gzip.exe',count=nn)
				if nn gt 0 then ziper=!Dir+'\gzip' else ziper='gzip'
				endif
      			 command_line=ziper+' -dfq' ;relies on gzip.exe being in the path
			 if !version.release ge '5.4' then key=',/hide' else key=''
      			 for i=0,n_elements(p1)-1 do begin com_line=command_line+' "'+p1(i)+'"'
      			                          ii=execute('spawn, com_line'+key) & endfor
      			 end
      		'MacOS':begin
      			; Get the current dir
      			cd, CURRENT=homeFolder
				
				; Add path 
				fileArray = homeFolder + p1

				lastFile = fileArray(n_elements(fileArray)-1)
				
      			; Prepare the file list in AppleScript format
      			Build_MacFileListFromArray, fileArray ,macFileList
      			
      			; Print, "SD :UN_ZLIS :" + macFileList

      			script = [  'tell application "' + Lamp_Dir + 'Helpers:MacGzip¥PPC"',$
							'    open ' + macFileList + LF,$
							'end tell' ]
				
				; Print, "Quitting IDL..."
				
				DO_APPLE_SCRIPT, script
				
				; Print, "Returning..."
		
				; MacGzip works in the background
				; so we need to wait for the last uncompressed file
				lastFile = STRMID(lastFile, 0, STRLEN(lastFile)-2) ; Strip ".Z" suffixe
				dummy = FindFile(lastFile, Count=AreYouThere)
				WHILE (AreYouThere EQ 0) DO BEGIN
					Print, "Waiting..."
					Wait, 0.5
					dummy = FindFile(lastFile, Count=AreYouThere)
				ENDWHILE
				
				; Print, "OK"
				res=1

      		 	 end
      		 ELSE:   spawn,['uncompress','-f',p1],/noshell
	   ENDCASE
	   END

'DEMO':     if  float(!version.release) ge 5.1 then ii=execute('res=LMGR(/DEMO)<1')     else res=0
'STUDENT':  if  float(!version.release) ge 5.1 then ii=execute('res=LMGR(/STUDENT)<1')  else res=0
'RUNTIME':  if  float(!version.release) ge 5.1 then ii=execute('res=LMGR(/RUNTIME)<1')  else res=0
'EMBEDDED': if  float(!version.release) ge 5.1 then ii=execute('res=LMGR(/EMBEDDED)<1') else res=0
'VERSION':  res=float(!version.release)
'MATLAB':   res=matlab

	   ;Start a VIEWER (vrml file is lamp.wrl)
	   ;--------------------------------------
'VIEWER':  BEGIN CASE os of
		'vms':  begin if viewer eq ''  then begin
				 viewer=getenv('VrmlViewer')
				 if viewer eq '' then begin viewer='-'
				    print,string(7b)+'(no logical VrmlViewer) !!!' & endif
			      endif
			      res=1
			      if viewer ne '-' then begin
				 if n_elements(p1) eq 1 then spawn,'VrmlViewer '+p1,/NOWAIT
			      endif
			end
      		'Win32':res=1
      		'MacOS':res=1
      		 ELSE:  begin if viewer eq ''  then begin
				 spawn,'whereis webspace',str
				 if strpos(str(0),'/webspace') gt 0 then viewer='webspace -remote ' $
				 else begin spawn,'whereis modelviewer',str
				 if strpos(str(0),'/modelvie') gt 0 then viewer='modelviewer -remote ' $
				 else begin spawn,'whereis vrweb',str
				 if strpos(str(0),'/vrweb')    gt 0 then viewer='vrweb  -remote ' $
				 else begin spawn,'whereis ivview',str
				 if strpos(str(0),'/ivview')   gt 0 then viewer='ivview  '
				 endelse & endelse & endelse
				 if viewer eq '' then viewer=getenv('VrmlViewer')
				 if viewer eq '' then begin viewer='-'
				    print,string(7b)+'(webspace,modelviewer,vrweb,ivview,$VrmlViewer) not found !!!' & endif
			      endif
			      res=1
			      if viewer ne '-' then begin
				 if n_elements(p1) eq 1 then spawn,viewer+' '+p1+' &'
			      endif
			end
	   ENDCASE
	   END

ELSE:
ENDCASE

RETURN,res
END ; sys_dep
