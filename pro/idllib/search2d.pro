; $Id: search2d.pro,v 1.2 1993/10/06 16:51:53 doug Exp $

; Copyright (c) 1992-1993, Research Systems, Inc. All rights reserved.
;	Unauthorized reproduction prohibited.
;
;+
; NAME:
;	SEARCH2D
;
; PURPOSE:
;	This function finds "objects" or regions of similar data
;       values within a 2-D array of data. Given a starting location
;       and a range of values to search for, SEARCH2D will find all
;       the cells within the array that are within the specified range
;       of values, and have some path of connectivity through these cells
;       to the starting location. In addition to searching for cells
;       within a global range of data values, SEARCH2D can also search
;       for adjacent cells whose values deviate from their neighbors within
;       specified tolerances. See the procedure "SEARCH3D" for the
;       three dimensional case.
;
;       This function returns a list of the array subscripts that define
;       the selected object or region.
;
; CATEGORY:
;       Data subsetting.
;	Image manipulation.
;
; CALLING SEQUENCE:
;       Region = SEARCH2D(Array, Xpos, Ypos, Min_val, Max_val)
;
; INPUTS:
;       Array:      The 2-D array of data to search.
;                   Data type : Any 2-D array except string or structure.
;       Xpos:       The X coordinate (first subscript into the 2-D Array)
;                   of the starting cell for the search.
;                   Data type : Long.
;       Ypos:       The Y coordinate (second subscript into the 2-D Array)
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
;                   then SEARCH2D creates an internal copy of Array.
;                   This internal copy is then processed to enhance the
;                   object edges by using an algorithm similar to the
;                   "SOBEL" edge enhancement process. Any adjacent
;                   cells will be found if their corresponding data value
;                   in the edge enhanced array is greater than DECREASE and
;                   less than INCREASE. In any case, the adjacent cells
;                   will NEVER be selected if their data value is not
;                   between Min_val and Max_val.
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
;                   squares surrounding the cells share a common edge.
;                   If a non-zero value is passed to DIAGONAL then
;                   SEARCH2D will also locate cells meeting the search
;                   criteria whose surrounding squares share a common
;                   corner. Specifying diagonal search mode requires
;                   more memory and execution time.
;                   The default is no diagonal searching.
;                   Data type : int
;
; OUTPUTS:
;       This function returns a list of the indices into the 2-D array
;       that are part of the located object or region. This list is
;       returned as a LONARR(n) where n is the number of cells found.
;
;       If the returned array of indices is called Region, and the
;       size of the 2-D array of data is size_x by size_y, then the
;       actual X and Y indices can be obtained by using the following
;       algorithm :
;
;          index_y = Region / size_x
;          index_x = Region - (index_y * size_x)
;
;       The object within the 2-D Array could then be subscripted as :
;
;          Array(Region)
;       OR
;          Array(index_x, index_y)
;
; EXAMPLE:
;       Find all the indices corresponding to an object contained in a
;       2-D array of data.
;
;       ; Create some data.
;          img = FLTARR(512, 512)
;          img(3:503, 9:488) = 0.7
;          img(37:455, 18:438) = 0.5
;          img(144:388, 90:400) = 0.7
;          img(200:301, 1:255) = 1.0
;          img(155:193, 333:387) = 0.3
;
;       ; Display the image.
;          TVSCL, img
;
;       ; Search for an object starting at (175, 300) whose data values
;       ; are between (0.6) and (0.8).
;          Region = SEARCH2D(img, 175, 300, 0.6, 0.8, /DIAGONAL)
; 
;       ; Scale the background cells into the range 0 to 127.
;          img = BYTSCL(img, TOP=127B)
;
;       ; Highlight the object region by setting it to 255.
;          img(Region) = 255B
; 
;       ; Display the array with the highlighted object in it.
;          TVSCL, img
;
; MODIFICATION HISTORY:
;       Written by:     Daniel Carr. Thu Sep  3 15:36:17 MDT 1992
;-

FUNCTION Search2d, array, xpos, ypos, min_val, max_val, $
                   Decrease=decrease, Increase=increase, $
                   Lpf_band=smooth_band, Diagonal=diagonal

; *** Test inputs

size_array = Size(array)
IF (size_array(0) NE 2L) THEN BEGIN
   Print, 'Array must have two dimensions'
   STOP
ENDIF
x_size = size_array(1)
y_size = size_array(2)

xpos = Long(xpos(0))
ypos = Long(ypos(0))
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

min_val = min_val(0)
max_val = max_val(0)

IF (max_val LT min_val) THEN BEGIN
   Print, 'Max value must be >= min value'
   STOP
ENDIF

start_val = array(xpos,ypos)
IF ((start_val LT min_val) OR (start_val GT max_val)) THEN BEGIN
   Print, 'Value of array at (xpos,ypos) must be >= min_val and <= max_val'
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
      diff_array = diff_array < (diff_array - Shift(diff_array,  0,  1)) < $
                                (diff_array - Shift(diff_array,  1,  1)) < $
                                (diff_array - Shift(diff_array,  1,  0)) < $
                                (diff_array - Shift(diff_array,  1, -1)) < $
                                (diff_array - Shift(diff_array,  0, -1)) < $
                                (diff_array - Shift(diff_array, -1, -1)) < $
                                (diff_array - Shift(diff_array, -1,  0)) < $
                                (diff_array - Shift(diff_array, -1,  1))
      IF (sb GT 0) THEN diff_array = Smooth(diff_array, sb)
   ENDIF ELSE BEGIN
      diff_array = Float(array)
      diff_array = diff_array < (diff_array - Shift(diff_array,  0,  1)) < $
                                (diff_array - Shift(diff_array,  1,  0)) < $
                                (diff_array - Shift(diff_array,  0, -1)) < $
                                (diff_array - Shift(diff_array, -1,  0))
      IF (sb GT 0) THEN diff_array = Smooth(diff_array, sb)
   ENDELSE
ENDIF

; *** Set up the required variables

similar_val = 1B
connect_val = 2B

c_array = Bytarr(x_size, y_size)
c_array(Where((array GE min_val) AND (array LE max_val))) = similar_val

x_size_m1 = x_size - 1L
y_size_m1 = y_size - 1L

x_ind = xpos
y_ind = ypos
just_found = (y_ind * x_size) + x_ind

c_array(just_found) = connect_val
num_found = 1L

; *** Start the search

IF (diag EQ 0B) THEN BEGIN   ; *** No diagonal mode
   nsew_ind = Lonarr(4, 1)
   nsew_ind(0, *) = (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
   nsew_ind(1, *) = (((y_ind - 1L) > 0L) * x_size) + x_ind
   nsew_ind(2, *) = (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
   nsew_ind(3, *) = (y_ind * x_size) + ((x_ind - 1L) > 0L)

   cc_array = c_array(nsew_ind(*))
   just_found = Where(cc_array EQ similar_val)

   ; *** Loop while cells are still being found

   WHILE (just_found(0) GE 0L) DO BEGIN
      cc_array(just_found) = connect_val
      c_array(nsew_ind(just_found)) = cc_array(just_found)
      y_ind = nsew_ind(just_found) / (x_size)
      x_ind = nsew_ind(just_found) - (y_ind * x_size)

      num_found = N_Elements(just_found)
      nsew_ind = Lonarr(4, num_found, /Nozero)

      nsew_ind(0, *) = (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
      nsew_ind(1, *) = (((y_ind - 1L) > 0L) * x_size) + x_ind
      nsew_ind(2, *) = (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
      nsew_ind(3, *) = (y_ind * x_size) + ((x_ind - 1L) > 0L)

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
   nsew_ind = Lonarr(8, 1)
   nsew_ind(0, *) = (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
   nsew_ind(1, *) = (((y_ind - 1L) > 0L) * x_size) + x_ind
   nsew_ind(2, *) = (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
   nsew_ind(3, *) = (y_ind * x_size) + ((x_ind - 1L) > 0L)
   nsew_ind(4, *) = (((y_ind + 1L) < y_size_m1) * x_size) + $
                     ((x_ind + 1L) < x_size_m1)
   nsew_ind(5, *) = (((y_ind - 1L) > 0L) * x_size) + $
                     ((x_ind - 1L) > 0L)
   nsew_ind(6, *) = (((y_ind + 1L) < y_size_m1) * x_size) + $
                     ((x_ind - 1L) > 0L)
   nsew_ind(7, *) = (((y_ind - 1L) > 0L) * x_size) + $
                     ((x_ind + 1L) < x_size_m1)

   cc_array = c_array(nsew_ind(*))
   just_found = Where(cc_array EQ similar_val)

   ; *** Loop while cells are still being found

   WHILE (just_found(0) GE 0L) DO BEGIN
      cc_array(just_found) = connect_val
      c_array(nsew_ind(just_found)) = cc_array(just_found)
      y_ind = nsew_ind(just_found) / (x_size)
      x_ind = nsew_ind(just_found) - (y_ind * x_size)

      num_found = N_Elements(just_found)
      nsew_ind = Lonarr(8, num_found, /Nozero)

      nsew_ind(0, *) = (((y_ind + 1L) < y_size_m1) * x_size) + x_ind
      nsew_ind(1, *) = (((y_ind - 1L) > 0L) * x_size) + x_ind
      nsew_ind(2, *) = (y_ind * x_size) + ((x_ind + 1L) < x_size_m1)
      nsew_ind(3, *) = (y_ind * x_size) + ((x_ind - 1L) > 0L)
      nsew_ind(4, *) = (((y_ind + 1L) < y_size_m1) * x_size) + $
                        ((x_ind + 1L) < x_size_m1)
      nsew_ind(5, *) = (((y_ind - 1L) > 0L) * x_size) + $
                        ((x_ind - 1L) > 0L)
      nsew_ind(6, *) = (((y_ind + 1L) < y_size_m1) * x_size) + $
                        ((x_ind - 1L) > 0L)
      nsew_ind(7, *) = (((y_ind - 1L) > 0L) * x_size) + $
                        ((x_ind + 1L) < x_size_m1)

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
