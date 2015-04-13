; $Id: search3d.pro,v 1.4 1995/06/01 16:50:53 dan Exp $

; Copyright (c) 1992-1993, Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;
;+
; NAME:
;	SEARCH3D
;
; PURPOSE:
;	This function finds "objects" or regions of similar data
;       values within a 3-D array of data. Given a starting location
;       and a range of values to search for, SEARCH3D will find all
;       the cells within the volume that are within the specified range
;       of values, and have some path of connectivity through these cells
;       to the starting location. In addition to searching for cells
;       within a global range of data values, SEARCH3D can also search
;       for adjacent cells whose values deviate from their neighbors within
;       specified tolerances. See the procedure "SEARCH2D" for the
;       two dimensional case.
;
;       This function returns a list of the array subscripts that define
;       the selected object or region.
;
; CATEGORY:
;       Data subsetting.
;	Volume manipulation.
;
; CALLING SEQUENCE:
;       Region = SEARCH3D(Array, Xpos, Ypos, Zpos, Min_val, Max_val)
;
; INPUTS:
;       Array:      The 3-D volume of data to search.
;                   Data type : Any 3-D array except string or structure.
;       Xpos:       The X coordinate (first subscript into the 3-D Array)
;                   of the starting cell for the search.
;                   Data type : Long.
;       Ypos:       The Y coordinate (second subscript into the 3-D Array)
;                   of the starting cell for the search.
;                   Data type : Long.
;       Zpos:       The Z coordinate (third subscript into the 3-D Array)
;                   of the starting cell for the search.
;                   Data type : Long.
;       Min_val:    The minimum data value to search for. All cells that
;                   are connected to the starting cell, and have a value
;                   greater than or equal to Min_val and less that or equal
;                   to Max_val, will be considered part of the "object".
;       Max_val:    The maximum data value to search for.
;
; KEYWORD PARAMETERS:
;       DECREASE:   If the DECREASE or INCREASE keywords are specified,
;                   then SEARCH3D creates an internal copy of Array.
;                   This internal copy is then processed to enhance the
;                   object edges by using an algorithm similar to the
;                   "SOBEL" edge enhancement process (in 3-D). Any
;                   adjacent cells will be found if their corresponding
;                   data value in the edge enhanced array is greater
;                   than DECREASE and less than INCREASE. In any case,
;                   the adjacent cells will NEVER be selected if their
;                   data value is not between Min_val and Max_val.
;                   The default is 0.0 if INCREASE is specified.
;                   Otherwise, the default is no edge checking.
;                   Data type : Int or Float (usually less than zero).
;       INCREASE:   The maximum value in the edge enhanced array for
;                   a cell to be considered part of the selected object.
;                   Some savings in execution time and memory usage result
;                   when DECREASE and INCREASE are omitted.
;                   See DECREASE above.
;                   The default is 0.0 if DECREASE is specified.
;                   Otherwise, the default is no edge checking.
;                   Data type : Int or Float (usually greater than zero).
;       LPF_BAND:   This keyword indicates what (if any) Low Pass Filtering
;                   is performed on the edge enhanced array before the
;                   search begins. If LPF_BAND is set to 3 or higher
;                   then the edge enhanced array will be smoothed using
;                   LPF_BAND as the width of the smoothing window.
;                   If LPF_BAND is less than 3 then no smoothing is
;                   performed. This keyword only has effect when the
;                   DECREASE or INCREASE keywords are supplied.
;                   See DECREASE above.
;                   The default is zero (no smoothing).
;                   Data type : Int.
;       DIAGONAL:   Normally, cells are considered adjacent only when
;                   cubes surrounding the cells share a common face.
;                   If a non-zero value is passed to DIAGONAL then
;                   SEARCH3D will also locate cells meeting the search
;                   criteria whose surrounding cubes share a common
;                   edge or corner. Specifying diagonal search mode
;                   requires more memory and execution time.
;                   The default is no diagonal searching.
;                   Data type : int
;
; OUTPUTS:
;       This function returns a list of the indices into the 3-D array
;       that are part of the located object or region. This list is
;       returned as a LONARR(n) where n is the number of cells found.
;
;       If the returned array of indices is called Region, and the
;       size of the 3-D volume of data is size_x by size_y by size_z,
;       then the actual X, Y, and Z indices can be obtained by using
;       the following algorithm :
;
;          index_z = Region / (size_x * size_y)
;          index_y = (Region - (index_z * size_x * size_y)) / size_x
;          index_x = (Region - (index_z * size_x * size_y)) - (index_y * size_x)
;
;       The object within the 3-D Array could then be subscripted as :
;
;          Array(Region)
;       OR
;          Array(index_x, index_y, index_z)
;
; EXAMPLE:
;       Find all the indices corresponding to an object contained in a
;       3-D volume of data.
;
;       ; Create some data.
;          vol = RANDOMU(s, 40, 40, 40)
;          vol(3:13, 1:15, 17:33) = 1.3
;          vol(15:25, 5:25, 15:25) = 0.2
;          vol(5:30,17:38,7:28) = 1.3
;          vol(9:23, 16:27, 7:33) = 1.5
;
;       ; Search for an object starting at (6, 22, 16) whose data values
;       ; are between (1.2) and (1.4)..
;          Region = SEARCH3D(vol, 6, 22, 16, 1.2, 1.4, /DIAGONAL)
; 
;       ; Scale the background cells into the range 0 to 127.
;          vol = BYTSCL(vol, TOP=127B)
;
;       ; Highlight the object region by setting it to 255.
;          vol(Region) = 255B
; 
;       ; Set up a 3-D view.
;          Window, 0, Xsize=640, Ysize=512, Retain=2
;          Create_View, Xmax=39, Ymax=39, Zmax=39, ax=(-30), az=30, zoom=0.8
; 
;       ; Display the volume with the highlighted object in it.
;          TVSCL, PROJECT_VOL(vol, 64, 64, 40, Depth_Q=0.4)
;
; MODIFICATION HISTORY:
;       Written by:     Daniel Carr. Thu Sep  3 17:36:04 MDT 1992
;-

FUNCTION Search3d, array, xpos, ypos, zpos, min_val, max_val, $
                   Decrease=decrease, Increase=increase, $
                   Lpf_band=smooth_band, Diagonal=diagonal
; *** Test inputs

size_array = Size(array)
IF (size_array(0) NE 3L) THEN BEGIN
   Print, 'Array must have three dimensions'
   STOP
ENDIF
x_size = size_array(1)
y_size = size_array(2)
z_size = size_array(3)

xpos = Long(xpos(0))
ypos = Long(ypos(0))
zpos = Long(zpos(0))
IF (xpos LT 0L) THEN BEGIN
   Print, 'Xpos must be >= 0'
   STOP
ENDIF
IF (xpos GE x_size) THEN BEGIN
   Print, 'Xpos must be < array size'
   STOP
ENDIF
IF (ypos LT 0L) THEN BEGIN
   Print, 'Ypos must be >= 0'
   STOP
ENDIF
IF (ypos GE y_size) THEN BEGIN
   Print, 'Ypos must be < array size'
   STOP
ENDIF
IF (zpos LT 0L) THEN BEGIN
   Print, 'Zpos must be >= 0'
   STOP
ENDIF
IF (zpos GE z_size) THEN BEGIN
   Print, 'Zpos must be < array size'
   STOP
ENDIF

min_val = min_val(0)
max_val = max_val(0)

IF (max_val LT min_val) THEN BEGIN
   Print, 'Max value must be >= min value'
   STOP
ENDIF

start_val = array(xpos,ypos,zpos)
IF ((start_val LT min_val) OR (start_val GT max_val)) THEN BEGIN
   Print, 'Value of array at (xpos,ypos,zpos) must be >= min_val and <= max_val'
   STOP
ENDIF

dec = 0.0
inc = 0.0
range = 0B
IF (N_Elements(decrease) GT 0L) THEN BEGIN
   dec = Float(decrease(0))
   range = 1B
ENDIF
IF (N_Elements(increase) GT 0L) THEN BEGIN
   inc = Float(increase(0))
   range = 1B
ENDIF

sb = 0
IF (N_Elements(smooth_band) GT 0L) THEN sb = Fix(smooth_band(0))
IF ((sb GE x_size) OR (sb GE y_size)) THEN BEGIN
   Print, 'Smooth band must be < size of array'
   STOP
ENDIF

diag = 0B
IF (N_Elements(diagonal) GT 0L) THEN diag = Byte(diagonal(0))

IF (range) THEN BEGIN

   ; *** Calculate the edge enhanced array

   IF (diag) THEN BEGIN
      diff_array = Float(array)
      diff_array = diff_array < $
                     (diff_array - Shift(array,  0,  1,  0)) < $
                     (diff_array - Shift(array,  1,  1,  0)) < $
                     (diff_array - Shift(array,  1,  0,  0)) < $
                     (diff_array - Shift(array,  1, -1,  0)) < $
                     (diff_array - Shift(array,  0, -1,  0)) < $
                     (diff_array - Shift(array, -1, -1,  0)) < $
                     (diff_array - Shift(array, -1,  0,  0)) < $
                     (diff_array - Shift(array, -1,  1,  0)) < $
                     (diff_array - Shift(array,  0,  1,  1)) < $
                     (diff_array - Shift(array,  1,  1,  1)) < $
                     (diff_array - Shift(array,  1,  0,  1)) < $
                     (diff_array - Shift(array,  1, -1,  1)) < $
                     (diff_array - Shift(array,  0, -1,  1)) < $
                     (diff_array - Shift(array, -1, -1,  1)) < $
                     (diff_array - Shift(array, -1,  0,  1)) < $
                     (diff_array - Shift(array, -1,  1,  1)) < $
                     (diff_array - Shift(array,  0,  1, -1)) < $
                     (diff_array - Shift(array,  1,  1, -1)) < $
                     (diff_array - Shift(array,  1,  0, -1)) < $
                     (diff_array - Shift(array,  1, -1, -1)) < $
                     (diff_array - Shift(array,  0, -1, -1)) < $
                     (diff_array - Shift(array, -1, -1, -1)) < $
                     (diff_array - Shift(array, -1,  0, -1)) < $
                     (diff_array - Shift(array, -1,  1, -1)) < $
                     (diff_array - Shift(array,  0,  0,  1)) < $
                     (diff_array - Shift(array,  0,  0, -1))
      IF (sb GT 0) THEN diff_array = Smooth(diff_array, sb)
   ENDIF ELSE BEGIN
      diff_array = Float(array)
      diff_array = diff_array < $
                     (diff_array - Shift(array,  0,  1,  0)) < $
                     (diff_array - Shift(array,  1,  0,  0)) < $
                     (diff_array - Shift(array,  0, -1,  0)) < $
                     (diff_array - Shift(array, -1,  0,  0)) < $
                     (diff_array - Shift(array,  0,  0,  1)) < $
                     (diff_array - Shift(array,  0,  0, -1))
      IF (sb GT 0) THEN diff_array = Smooth(diff_array, sb)
   ENDELSE
ENDIF

; *** Set up the required variables

similar_val = 1B
connect_val = 2B

c_array = Bytarr(x_size, y_size, z_size)
c_array(Where((array GE min_val) AND (array LE max_val))) = similar_val

x_size_m1 = x_size - 1L
y_size_m1 = y_size - 1L
z_size_m1 = z_size - 1L

x_ind = xpos
y_ind = ypos
z_ind = zpos
just_found = (z_ind * y_size * x_size) + (y_ind * x_size) + x_ind

c_array(just_found) = connect_val
num_found = 1L

; *** Start the search

IF (diag EQ 0B) THEN BEGIN   ; *** No diagonal mode
   nsew_ind = Lonarr(6, 1)
   nsew_ind(0, *) = (z_ind * y_size * x_size) + $
                    (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
   nsew_ind(1, *) = (z_ind * y_size * x_size) + $
                    (((y_ind - 1L) > 0L) * x_size) + x_ind
   nsew_ind(2, *) = (z_ind * y_size * x_size) + $
                    (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
   nsew_ind(3, *) = (z_ind * y_size * x_size) + $
                    (y_ind * x_size) + ((x_ind - 1L) > 0L)
   nsew_ind(4, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                    (y_ind * x_size) + x_ind
   nsew_ind(5, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                    (y_ind * x_size) + x_ind

   cc_array = c_array(nsew_ind(*))
   just_found = Where(cc_array EQ similar_val)

   ; *** Loop while cells are still being found

   WHILE (just_found(0) GE 0L) DO BEGIN
      cc_array(just_found) = connect_val
      c_array(nsew_ind(just_found)) = cc_array(just_found)
      z_ind = nsew_ind(just_found) / (y_size * x_size)
      y_ind = (nsew_ind(just_found) - (z_ind * y_size * x_size)) / (x_size)
      x_ind = (nsew_ind(just_found) - (z_ind * y_size * x_size)) - $
              (y_ind * x_size)

      num_found = N_Elements(just_found)
      nsew_ind = Lonarr(6, num_found, /Nozero)

      nsew_ind(0, *) = (z_ind * y_size * x_size) + $
                       (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
      nsew_ind(1, *) = (z_ind * y_size * x_size) + $
                       (((y_ind - 1L) > 0L) * x_size) + x_ind
      nsew_ind(2, *) = (z_ind * y_size * x_size) + $
                       (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
      nsew_ind(3, *) = (z_ind * y_size * x_size) + $
                       (y_ind * x_size) + ((x_ind - 1L) > 0L)
      nsew_ind(4, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                       (y_ind * x_size) + x_ind
      nsew_ind(5, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                       (y_ind * x_size) + x_ind

      nsew_ind = nsew_ind(Sort(nsew_ind(*)))
      nsew_ind = nsew_ind(Uniq(nsew_ind))

      cc_array = c_array(nsew_ind(*))

      IF (range) THEN BEGIN
         t_array = diff_array(nsew_ind(*))
         just_found = Where((cc_array EQ similar_val) AND $
                           ((t_array GE dec) AND $
                            (t_array LE inc)))
      ENDIF ELSE BEGIN
         just_found = Where(cc_array EQ similar_val)
      ENDELSE
   ENDWHILE
ENDIF ELSE BEGIN   ; *** Diagonal mode
   nsew_ind = Lonarr(26, 1)
   nsew_ind( 0, *) = (z_ind * y_size * x_size) + $
                     (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
   nsew_ind( 1, *) = (z_ind * y_size * x_size) + $
                     (((y_ind - 1L) > 0L) * x_size) + x_ind
   nsew_ind( 2, *) = (z_ind * y_size * x_size) + $
                     (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
   nsew_ind( 3, *) = (z_ind * y_size * x_size) + $
                     (y_ind * x_size) + ((x_ind - 1L) > 0L)
   nsew_ind( 4, *) = (z_ind * y_size * x_size) + $
                     (((y_ind + 1L) < y_size_m1) * x_size) + $
                     ((x_ind + 1L) < x_size_m1)
   nsew_ind( 5, *) = (z_ind * y_size * x_size) + $
                     (((y_ind - 1L) > 0L) * x_size) + $
                     ((x_ind - 1L) > 0L)
   nsew_ind( 6, *) = (z_ind * y_size * x_size) + $
                     (((y_ind + 1L) < y_size_m1) * x_size) + $
                     ((x_ind - 1L) > 0L)
   nsew_ind( 7, *) = (z_ind * y_size * x_size) + $
                     (((y_ind - 1L) > 0L) * x_size) + $
                     ((x_ind + 1L) < x_size_m1)
   nsew_ind( 8, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                     (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
   nsew_ind( 9, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                     (((y_ind - 1L) > 0L) * x_size) + x_ind
   nsew_ind(10, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                     (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
   nsew_ind(11, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                     (y_ind * x_size) + ((x_ind - 1L) > 0L)
   nsew_ind(12, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                     (((y_ind + 1L) < y_size_m1) * x_size) + $
                     ((x_ind + 1L) < x_size_m1)
   nsew_ind(13, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                     (((y_ind - 1L) > 0L) * x_size) + $
                     ((x_ind - 1L) > 0L)
   nsew_ind(14, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                     (((y_ind + 1L) < y_size_m1) * x_size) + $
                     ((x_ind - 1L) > 0L)
   nsew_ind(15, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                     (((y_ind - 1L) > 0L) * x_size) + $
                     ((x_ind + 1L) < x_size_m1)
   nsew_ind(16, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                     (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
   nsew_ind(17, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                     (((y_ind - 1L) > 0L) * x_size) + x_ind
   nsew_ind(18, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                     (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
   nsew_ind(19, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                     (y_ind * x_size) + ((x_ind - 1L) > 0L)
   nsew_ind(20, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                     (((y_ind + 1L) < y_size_m1) * x_size) + $
                     ((x_ind + 1L) < x_size_m1)
   nsew_ind(21, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                     (((y_ind - 1L) > 0L) * x_size) + $
                     ((x_ind - 1L) > 0L)
   nsew_ind(22, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                     (((y_ind + 1L) < y_size_m1) * x_size) + $
                     ((x_ind - 1L) > 0L)
   nsew_ind(23, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                     (((y_ind - 1L) > 0L) * x_size) + $
                     ((x_ind + 1L) < x_size_m1)
   nsew_ind(24, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                     (y_ind * x_size) + x_ind
   nsew_ind(25, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                     (y_ind * x_size) + x_ind

   cc_array = c_array(nsew_ind(*))
   just_found = Where(cc_array EQ similar_val)

   ; *** Loop while cells are still being found

   WHILE (just_found(0) GE 0L) DO BEGIN
      cc_array(just_found) = connect_val
      c_array(nsew_ind(just_found)) = cc_array(just_found)
      z_ind = nsew_ind(just_found) / (y_size * x_size)
      y_ind = (nsew_ind(just_found) - (z_ind * y_size * x_size)) / (x_size)
      x_ind = (nsew_ind(just_found) - (z_ind * y_size * x_size)) - $
              (y_ind * x_size)

      num_found = N_Elements(just_found)
      nsew_ind = Lonarr(26, num_found, /Nozero)
      nsew_ind( 0, *) = (z_ind * y_size * x_size) + $
                        (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
      nsew_ind( 1, *) = (z_ind * y_size * x_size) + $
                        (((y_ind - 1L) > 0L) * x_size) + x_ind
      nsew_ind( 2, *) = (z_ind * y_size * x_size) + $
                        (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
      nsew_ind( 3, *) = (z_ind * y_size * x_size) + $
                        (y_ind * x_size) + ((x_ind - 1L) > 0L)
      nsew_ind( 4, *) = (z_ind * y_size * x_size) + $
                        (((y_ind + 1L) < y_size_m1) * x_size) + $
                        ((x_ind + 1L) < x_size_m1)
      nsew_ind( 5, *) = (z_ind * y_size * x_size) + $
                        (((y_ind - 1L) > 0L) * x_size) + $
                        ((x_ind - 1L) > 0L)
      nsew_ind( 6, *) = (z_ind * y_size * x_size) + $
                        (((y_ind + 1L) < y_size_m1) * x_size) + $
                        ((x_ind - 1L) > 0L)
      nsew_ind( 7, *) = (z_ind * y_size * x_size) + $
                        (((y_ind - 1L) > 0L) * x_size) + $
                        ((x_ind + 1L) < x_size_m1)
      nsew_ind( 8, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                        (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
      nsew_ind( 9, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                        (((y_ind - 1L) > 0L) * x_size) + x_ind
      nsew_ind(10, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                        (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
      nsew_ind(11, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                        (y_ind * x_size) + ((x_ind - 1L) > 0L)
      nsew_ind(12, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                        (((y_ind + 1L) < y_size_m1) * x_size) + $
                        ((x_ind + 1L) < x_size_m1)
      nsew_ind(13, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                        (((y_ind - 1L) > 0L) * x_size) + $
                        ((x_ind - 1L) > 0L)
      nsew_ind(14, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                        (((y_ind + 1L) < y_size_m1) * x_size) + $
                        ((x_ind - 1L) > 0L)
      nsew_ind(15, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                        (((y_ind - 1L) > 0L) * x_size) + $
                        ((x_ind + 1L) < x_size_m1)
      nsew_ind(16, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                        (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
      nsew_ind(17, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                        (((y_ind - 1L) > 0L) * x_size) + x_ind
      nsew_ind(18, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                        (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
      nsew_ind(19, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                        (y_ind * x_size) + ((x_ind - 1L) > 0L)
      nsew_ind(20, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                        (((y_ind + 1L) < y_size_m1) * x_size) + $
                        ((x_ind + 1L) < x_size_m1)
      nsew_ind(21, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                        (((y_ind - 1L) > 0L) * x_size) + $
                        ((x_ind - 1L) > 0L)
      nsew_ind(22, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                        (((y_ind + 1L) < y_size_m1) * x_size) + $
                        ((x_ind - 1L) > 0L)
      nsew_ind(23, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                        (((y_ind - 1L) > 0L) * x_size) + $
                        ((x_ind + 1L) < x_size_m1)
      nsew_ind(24, *) = (((z_ind + 1L) < z_size_m1) * y_size * x_size) + $
                        (y_ind * x_size) + x_ind
      nsew_ind(25, *) = (((z_ind - 1L) > 0L) * y_size * x_size) + $
                        (y_ind * x_size) + x_ind

      nsew_ind = nsew_ind(Sort(nsew_ind(*)))
      nsew_ind = nsew_ind(Uniq(nsew_ind))

      cc_array = c_array(nsew_ind(*))

      IF (range) THEN BEGIN
         t_array = diff_array(nsew_ind(*))
         just_found = Where((cc_array EQ similar_val) AND $
                           ((t_array GE dec) AND $
                            (t_array LE inc)))
      ENDIF ELSE BEGIN
         just_found = Where(cc_array EQ similar_val)
      ENDELSE
   ENDWHILE
ENDELSE

; *** Clean up and return

x_ind = 0
y_ind = 0
nsew_ind = 0
cc_array = 0
t_array = 0
diff_array = 0

index = Where(c_array EQ connect_val)

RETURN, index
END
