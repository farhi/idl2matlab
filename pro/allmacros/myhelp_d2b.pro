;	***************************
	     pro myhelp_d2b, TEXT
;	***************************

;This is effective when the string-array TEXT is more than one element.

text=[$
'*****************************************************************************',$
'* THIS IS A HELP FOR D2B USERS OF LAMP                                        ',$
'* For information on LAMP, please contact Thomas HANSEN (hansen@ill.fr, 7044) ',$
'*****************************************************************************',$
'',$
'READING DATA:',$
'	Chose the correct path with the data acces button',$
'   If recent data are still not transfered, On_Line accesses directly d2b.ill.fr',$
'',$
'	Select the workspace with the arrows',$
'	Type the correct numor or numor range in the window and press "read"',$
'	A numor range to be merged is specified with a ">" sign (Ex: 51568>51572)',$
'',$
'	The data will be automatically calibrated with the file d2b.cal ~lambda/CALIBRATION',$
'   Type "calibration" in a formula window and "DO" to show a list of possible calibration files',$
'   and choose the file you want for calibration with "load"',$
'',$
'	Bad detector cells may be excluded by typing, e.g. "dud,det=32,pts=0" in a formula window',$
'',$
'   If the "raw" button beside the "read" button is active, no monitor normalization will be done',$
'   which may give strange merged diagrams from several numors',$
'   Deactivating this radio button will normalize a range of numors always to the average monitor counting rate',$
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
' Use Options... for changing titles, plot options and printer',$
' Use the command "?" to open an exhausing help-window on IDL - the original macro-language to LAMP',$
'',$
'	PROCEDURES:',$
' Please use the the "User Macros" button',$
' In the nowindows version of lamp (lamp -nw) you may type "procedures"',$
'',$
'	FUNCTIONS:',$
' Please use the "User Macros" button',$
' In the nowindows version of lamp (lamp -nw) you may type "functions"',$
'',$
' Some ancient stuff ...: ',$
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
