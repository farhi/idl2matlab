; $Id: beta.pro,v 1.1 1995/07/05 15:33:04 idl Exp $

; Copyright (c) 1995, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

;+
; NAME:
;       BETA
; PURPOSE:
;	The introduction of the BETA function as a built in system routine
;	in IDL 4.0 caused inconvenience to customers with existing code
;	in which BETA had been used as a variable, because IDL system
;	routines have precedence over variable names. To minimize this
;	problem, RSI has renamed BETA back to NR_BETA (its old name).
;
;	This wrapper serves to make NR_BETA available under the name
;	BETA as documented in the IDL Reference Manual. However, since
;	IDL library routines have lower precedence than variables, programs
;	that use BETA as a variable name will work as before.
;
;	See the documentation for BETA in the IDL Reference manual for details
;	on arguments, keywords, and results.
; 
;
; MODIFICATION HISTORY:
;	3 July 1995, AB, RSI.
;-

function beta, z, w, _EXTRA=EXTRA_KW

  return, NR_BETA(z, w, _EXTRA=EXTRA_KW)

end
