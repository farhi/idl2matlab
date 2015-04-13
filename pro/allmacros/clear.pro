PRO clreff
common calibration
inf_d20=0
print,'List of calibration files cleared, will be re-created and re-read'
END

PRO clrbad
common calibration
common d20
psd_d20=['empty']
print,'List of bad_cells files cleared, will be re-created and re-read'
END

PRO clear
;+
; NAME:
;	CLEAR
;
; PURPOSE:
;	Resets automatic efficiency and angular correction file and bad cells
;   To be used whenever a new calibration file is provided during an open
;   LAMP session for a D20 experiment in order to take into account these
;   new files for any new data read in. It avoids restarting LAMP.
;   Attention: Any data already read in in a workspace has been corrected
;   according to the previous list of correction files.
;
; CATEGORY:
;	User
;
; CALLING SEQUENCE:
;	clear
;
; INPUTS:
;	none
;
; OPTIONAL INPUTS:
;	none.
;	
; KEYWORD PARAMETERS: 
;   none
;
; OUTPUTS:
;	none
;
; OPTIONAL OUTPUTS:
;	none
;
; COMMON BLOCKS:
;	none
;
; SIDE EFFECTS:
;	none
;
; RESTRICTIONS:
;	none
;
; PROCEDURE:
;	clreff and clrbad
;
; EXAMPLE:
;  clear
;
; MODIFICATION HISTORY:
; 	Written by:	Thomas Hansen
;	September, 2002	comments
;-
clreff
clrbad
END
