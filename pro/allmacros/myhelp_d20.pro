;	***************************
	     pro myhelp_d20, TEXT
;	***************************

;This is effective when the string-array TEXT is more than one element.

text=[$
'**************************************************************************',$
'* THIS IS A HELP FOR D20 USERS OF LAMP (X11 version)                      ',$
'* For all information, please contact Thomas HANSEN (hansen@ill.fr, 7044) ',$
'*                                                                         ',$
"* Please use the users manual you'll find via a WWW-browser like Netscape ",$
'* on http://www.ill.fr/YellowBook/D20 (click on D20 information)          ',$
'* Use the toolchest to open a Web Browser (pop up menu Internet)          ',$
"* In Netscape's Options - Network Preferences - Proxies define no proxy   ",$
'* for ill.fr (www.ill.fr etc.) or use server http://proxy.ill.fr/proxy.pac',$
'*                                                                         ',$
'**************************************************************************',$
'',$
'READING DATA:',$
'	Chose the correct path with the data acces button',$
' The base "C_Year 1998 L" is stored locally on d20sgi.ill.fr for fast access',$
' If recent data are still not transfered, On_Line accesses directly d20.ill.fr',$
'',$
'	Select the workspace with the arrows',$
'	Type the correct numor or numor range in the window and press "read"',$
'	A numor range to be added up is specified with a ">" sign (Ex: 20010>20014)',$
'	A numor range to be read in a 3D-workspace with a ":" sign (Ex: 20010:20014)',$
'	"::" will read in only every second numor (Ex: 20010::20014 reads only even numors)',$
'',$
'	The data may be automatically calibrated with the right file in d20sgi.ill.fr:~lambda/CALIBRATION',$
'	If you set the right flag: type "flag,/eff"',$
'',$
'	Bad detector cells may be excluded by taking information from the right file in  d20sgi.ill.fr:~lambda/BAD_CELLS',$
'	If you set the right flag: type "flag,/bad"',$
' They will be linearly interpolated then by choosing "flag,/int"',$
' or excluded by "flag,/noint"',$
'',$
' You may opt for monitor normalisation during reading in by flag,/nor or flag,nor=10000',$
' This will be applied to each acquisition BEFORE eventually beeing added up',$
'',$
' Use the selector access button that opens a window called filter to read data',$
' into Workspace W20 (you can copy it afterwards, e.g. "W6=W20"). Using this you',$
' may choose the option M. for monitor normalisation AFTER adding up several numors',$
' (e.g. 44500>44600)',$
'',$
' Use GK_fit for simple peak fitting',$
' Use SuperPlot for superposing plots',$
' Use UserMacros for an overview of existing macros',$
' Use DataParameters for looking at fixed parameters for a workspace',$
' Use BeGood for changing titles, plot options and printer',$
' Use the command "?" to open an exhausing help-window on IDL - the original macro-language to LAMP',$
'',$
'	PROCEDURES:',$
' Please use the WWW D20 Manual or the "User Macros" button',$
' In the nowindows version of lamp (lamp -nw) you may type "procedures"',$
'',$
'	FUNCTIONS:',$
' Please use the WWW D20 Manual or the "User Macros" button',$
' In the nowindows version of lamp (lamp -nw) you may type "functions"',$
'',$
'		w_accu, accu=n1,add=n2',$
'		  Accumulates the data in workspace n2 into workspace n1.',$
'		  Statistics are properly accounted for.',$
'		  This is useful to add data from different ranges or with different "dud".',$		      	
'		  Also, it can be used to subtract data sets.',$
'		  Ex:	w_accu, accu=w10,add=w6',$
'			w7=-w7',$
'			w_accu, accu=w10,add=w7',$
'			This series of commands subtracts w7 from w6',$
'			and stores the result in w10'$

]

end
