function corel,wkin ,sigma=sigma
;******* *****
;**
;** Standard deviation over spectra:return sigma/Error or sigma if keyword_set
;** -------- --------- ---- -------

wkout =0
dim   =SIZE(wkin)					

If dim(1) gt 1 then $
	Case dim(0) of
			
		2 : Begin		  		  
		      np    =dim(2)			;Get number of runs
		      wkmean=TOTAL(wkin,2) / np		;Get mean values
		      wkout =wkmean * 0				      
		      For i =0,np-1 do $
			  wkout = (wkin(*,i)  -wkmean)^2 + wkout  
			   			 	;This is SIGMA^2
		    End	
			     
		3 : Begin     						     
		      np    =dim(3)			;Get number of runs
		      wkmean=TOTAL(wkin,3) / np		;Get mean values
		      wkout =wkmean * 0				      
		      For i =0,np-1 do $
			  wkout = (wkin(*,*,i)-wkmean)^2 + wkout  
			      				;This is SIGMA^2
		    End							     
		
		else :return,wkout
	Endcase
	f=.5/np
	if keyword_set(sigma) then    $ 
	     wkout =Sqrt(wkout/(np-1))$			;Return SIGMA
	else wkout =Sqrt(wkout/(np-1)/(wkmean>f))	;Return SIGMA/Error
						    
Return ,wkout
End							  
