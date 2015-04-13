PRO cellno,i
;+
; PURPOSE: 
; This little procedure prints out the corresponding box- and amplifier-number for a given PSD-cell on D20
; INPUT:
; PSD cell number
; OUTPUT:
; Prints out a line in the basic IDL terminal window
; EXAMPLE: 
; cellno,120
;-
print,'cell',i,', box',i/32,', ampli',(i mod 32)*(((i/32) mod 2)ne 0)+(31 - i mod 32)*(((i/32) mod 2) eq 0)

END
