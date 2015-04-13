PRO HOUSE                             ;Define a procedure to draw a house.
house_x = [0, 16, 16, 8, 0, 0, 16, 16, 8, 0];X coordinates of 10 vertices. First 5 are front face, second 5 are back face. The range is 0 to 16.
house_y = [0, 0, 10, 16, 10, 0, 0, 10, 16, 10];The corresponding y values range from 0 to 16.
house_z = [54, 54, 54, 54, 54, 30, 30, 30, 30, 30];The z values range from 30 to 54.
min_x = -4 & max_x = 20.      ;Define max and min xy values to scale. Slightly larger than data range.
!X.S = [-(-4), 1.]/(20 - (-4));Set x data scale to range from -4 to 20. 
!Y.S = !X.S                          ;Same for y.
!Z.S = [-10, 1.]/(70 - 10)   ;The z range is from 10 to 70. 
face = [INDGEN(5), 0]           ;Indices of front face.
PLOTS, house_x[face], house_y[face], $
   house_z[face], /T3D, /DATA;Draw front face.
PLOTS, house_x[face + 5], house_y[face + 5], $
   house_z[face + 5], /T3D, /DATA;Draw back face.
FOR I = 0, 4 DO PLOTS, [house_x[i], house_x[i + 5]], $
   [house_y[i], house_y[i + 5]], $
   [house_z[i], house_z[i + 5]], /T3D, /DATA;Connecting lines from front to back.
XYOUTS, house_x[3], house_y[3], Z = house_z[3], 'Front', $
   /T3D, /DATA, SIZE = 2      ;Annotate front peak.
XYOUTS, house_x[8], house_y[8], Z = house_z[8], 'Back', $
   /T3D, /DATA, SIZE = 2      ;Annotate back.
END                                      ;End of house procedure.

;The HOUSE procedure could be called from the IDL command line to produce a number of different plots. For example:
;T3D, /RESET & HOUSE              ;Set up no rotation, scale, and draw house.
;H = [0.5, 0.5, 0.5]              ;Create a handy constant.
;T3D, /RESET, TRANS = -H, ROT = [30, 30, 0] & $
;T3D, TR = H & HOUSE              ;Straight projection after rotating 30 degrees about x and y axes. 
;T3D, /RESET, TRANS = -H, ROT=[0, 0, 0], OBLIQUE=[.5, -45] & $
;T3D, TR = H & HOUSE              ;No rotation, oblique projection, z factor = 0.5, angle = 45. 
;T3D, /RESET, TR=-H, ROT=[-6, 6, 0], PERS=4 & $
;T3D, TR=H & HOUSE;Rotate 6 degrees about x and y, then apply perspective.
