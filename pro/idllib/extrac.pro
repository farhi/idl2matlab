; $Id: extrac.pro,v 1.2 1993/10/04 20:59:53 doug Exp $

function EXTRAC, Array, P0,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15
;+
; NAME:
;	EXTRAC
;
; PURPOSE:
;	The EXTRAC function returns as its result any rectangular sub-matrix
;	or portion of the parameter array.  When parts of the specified
;	subsection lie outside the bounds of the array, zeros are
;	entered into these outlying elements.
;
;	EXTRAC was originally a built-in system procedure in the PDP-11
;	version of IDL, and was retained in that form in the original VAX/VMS
;	IDL for compatibility.  Most applications of the EXTRAC function
;	are more concisely written using subscript ranges (e.g., X(10:15)).  In
;	the current release of IDL, EXTRAC has been rewritten as a User Library
;	function that provides the same interface as the previous versions.
;
; CATEGORY:
;	Array Manipulation.
;
; CALLING SEQUENCE:
;	Result = EXTRAC(Array, C1, C2, ..., Cn, S1, S2, ..., Sn)
;
; INPUTS:                 
;	Array:	The array from which the subarray will be copied.
;
;	Ci:	The starting subscript in Array for the subarray. There
;		should be one Ci for each dimension of Array.
;
;	Si:	The size of each dimension.  The result will have dimensions
;		of (S1, S2, ..., Sn). There should be one Si for each
;		dimension of Array.
;
; OUTPUTS:
;	This function returns a two-dimensional, floating-point,
;	interpolated array.
;
; RESTRICTIONS:
;	In order to make the most typical cases run quickly, little error 
;	checking is done on the input.  In particular, the Ci and Si arguments
;	must all be scalar integers, and the Si must be non-negative.
;
;	If you know that the subarray will never lie beyond the edges of
;	the array, it is more efficient to use array subscript ranges
;	to extract the data instead of EXTRAC. 
;
; PROCEDURE:
;	If the subarray lies entirely inside the Array argument, the
;	standard array subscript-range mechanism is used to do the work.
;	Otherwise, a zeroed array of the correct type and size is
;	created, and the overlapping subarray is copied into it.
;
; EXAMPLES:
;	EXAMPLE 1:
;	Define a 1000 point vector with each element initialized to
;	its subscript.  Extract a 300 pt. vector, starting at A(200) and
;	going to A(499).  B(0) will be equal to A(200), B(299) will be
;	equal to A(499).  Enter:
;
;	    A = FINDGEN(1000)
;	    B = EXTRAC(A, 200, 300)
;
;	EXAMPLE 2:
;	Here, the first 49 points extracted (B(0) to B(49)) lie outside
;	the bounds of the vector and are set to 0.  B(50) is set to A(0),
;	B(51) is set to A(1) which is 1, ... Enter:
;
;	    A = FINDGEN(1000)
;	    B = EXTRAC(A, -50, 100)
;
;	EXAMPLE 3:
;	The following commands illustrate the use of EXTRAC with multi-
;	dimensional arrays.  Enter:
;
;	    A = INTARR(64,64)	;Make a 64X64 matrix to play with
;
;	Take out a 32X32 portion starting at A(20,30) by entering:
;
;	    B = EXTRAC(A, 20, 30, 32, 32)
;
;	A better way to perform the same operation as the previous line is:
;
;	    B = A(20:51, 30:61)
;
;	Extract the 20th column and 32nd row of A:
;
;	    B = EXTRAC(A, 19, 0, 1, 64)	; Extract 20th column of A
;	    B = EXTRAC(A, 0, 31, 64, 1)	; Extract 32nd row of A
;
;	Take a 32X32 matrix from A starting at A(40,50).
;
;	    B = EXTRAC(A, 40, 50, 32, 32)
;
;	NOTE: Those points beyond the boundaries of A are set to 0.
;
; REVISION HISTORY:
;	July 18, 1989	Written AB, RSI
;-

on_error, 2			; Return to caller on error

asize = SIZE(Array)
ndim = asize(0)
orig_dims = asize(1:ndim)

; Is it an array?
if (ndim eq 0) then message, 'Target argument must be an array.'

; Is there an appropriate number of arguments present?
if (n_params() ne (ndim * 2 + 1)) then message, 'Wrong number of arguments.'

; Convert the arguments to a more convenient form.
args = intarr(2 * ndim)
CASE (ndim) of
; 8: BEGIN & args(15) = P15 & args(14) = P14 & GOTO, do_seven & END
  7: do_seven: BEGIN & args(13) = P13 & args(12) = P12 & GOTO, do_six & END
  6: do_six: BEGIN & args(11) = P11 & args(10) = P10 & GOTO, do_five & END
  5: do_five: BEGIN & args(9) = P9 & args(8) = P8 & GOTO, do_four & END
  4: do_four: BEGIN & args(7) = P7 & args(6) = P6 & GOTO, do_three & END
  3: do_three: BEGIN & args(5) = P5 & args(4) = P4 & GOTO, do_two & END
  2: do_two: BEGIN & args(3) = P3 & args(2) = P2 & GOTO, do_one & END
  1: do_one: BEGIN & args(1) = P1 & args(0) = P0 & END
ENDCASE
srt = args(0:ndim-1)
dims = args(ndim:*)

; Determine if the subarray extends beyond the edges of the original.
; If not, a simple expression will do the job.
srt_over = where(srt lt 0, s_cnt)
dims_over = where((srt + dims) gt orig_dims, b_cnt)

if ((s_cnt eq 0) and (b_cnt eq 0)) then begin
  ; The extracted array does not go beyond the array boundaries. Use the
  ; normal expression to extract the result.
  bnd = dims + srt - 1
  case (ndim) of
    1: result =  Array(P0:bnd(0))
    2: result =  Array(P0:bnd(0),P1:bnd(1))
    3: result =  Array(P0:bnd(0),P1:bnd(1),P2:bnd(2))
    4: result =  Array(P0:bnd(0),P1:bnd(1),P2:bnd(2),P3:bnd(3))
    5: result =  Array(P0:bnd(0),P1:bnd(1),P2:bnd(2),P3:bnd(3),P4:bnd(4))
    6: result =  Array(P0:bnd(0),P1:bnd(1),P2:bnd(2),P3:bnd(3),P4:bnd(4), $
		       P5:bnd(5))
    7: result =  Array(P0:bnd(0),P1:bnd(1),P2:bnd(2),P3:bnd(3),P4:bnd(4), $
		       P5:bnd(5),P6:bnd(6))
;   8: result =  Array(P0:bnd(0),P1:bnd(1),P2:bnd(2),P3:bnd(3),P4:bnd(4), $
;		       P5:bnd(5),P6:bnd(6),P7:bnd(7))
   endcase
  goto, done
endif

; If we get this far, the sub array extends beyond the source array
; dimensions. Get a zeroed array of the correct type and extract the
; non-zero part of the original into it.
result = make_array(type=asize(ndim + 1), dimension=dims)  ; Get a result array

; Determine the insertion point for the subarray.
isrt = intarr(ndim)
if (s_cnt ne 0) then isrt(srt_over) = abs(srt(srt_over))


; If any of the starting points exceed the dimensions, then we're done.
dims_over = where(isrt ge dims, b_cnt)
srt_over = where(srt ge orig_dims, s_cnt)
if ((b_cnt ne 0) or (s_cnt ne 0)) then goto, done

; Determine the size of the subarray to be inserted. This is the
; lesser of the original size and the room for insertion in the target.
;
; dims - isrt is the availible room in the result array
; orig_dims - srt - is the largest possible subarray we can pull out of ARRAY

t1 = dims - isrt
bnd = (t1 < (orig_dims - srt)) < t1	; Minimum of the two sizes
srt = srt > 0				; Clip starting point to non-negative
bnd = srt + bnd - 1			; Calcualte the actual outer boundary

; Insert the subarray from ARRAY into RESULT
case (ndim) of
  1: result(isrt(0)) =  Array(srt(0):bnd(0))
  2: result(isrt(0),isrt(1)) =  Array(srt(0):bnd(0),srt(1):bnd(1))
  3: result(isrt(0),isrt(1),isrt(2)) = Array(srt(0):bnd(0),srt(1):bnd(1),srt(2):bnd(2))
  4: result(isrt(0),isrt(1),isrt(2),isrt(3)) =  $
	Array(srt(0):bnd(0),srt(1):bnd(1),srt(2):bnd(2),srt(3):bnd(3))
  5: result(isrt(0),isrt(1),isrt(2),isrt(3),isrt(4)) =  $
	Array(srt(0):bnd(0),srt(1):bnd(1),srt(2):bnd(2),srt(3):bnd(3), $
	      srt(4):bnd(4))
  6: result(isrt(0),isrt(1),isrt(2),isrt(3),isrt(4),isrt(5)) = $
	Array(srt(0):bnd(0),srt(1):bnd(1),srt(2):bnd(2),srt(3):bnd(3), $
	      srt(4):bnd(4), srt(5):bnd(5))
  7: result(isrt(0),isrt(1),isrt(2),isrt(3),isrt(4),isrt(5),isrt(6)) = $
	Array(srt(0):bnd(0),srt(1):bnd(1),srt(2):bnd(2),srt(3):bnd(3), $
	      srt(4):bnd(4), srt(5):bnd(5),srt(6):bnd(6))
; 8: result(isrt(0),isrt(1),isrt(2),isrt(3),isrt(4),isrt(5),isrt(6),isrt(7))= $
;	Array(srt(0):bnd(0),srt(1):bnd(1),srt(2):bnd(2),srt(3):bnd(3), $
;	      srt(4):bnd(4), srt(5):bnd(5),srt(6):bnd(6),srt(7):bnd(7))
endcase


done:
  return, result
end







; The rest of this file exists in order to test the EXTRAC procedure above.
; Normally, it won't be compiled when EXTRAC is used because when
; a procedure is automatically pulled out of the user library, only
; enough is compiled to get the desired routine, the rest of the file
; is ignored. Use ".run extrac" to compile everything.


pro extrac_errprint, n, is, want
; extrac_errprint tests a result against the desired value and
; reports it if they don't agree
;
; entry:
;	n - Error identification number.
;	is - Result value.
;	want - Correct value for is.
;
; exit:
;	if (is eq want) then errprint returns quietly. Otherwise,
;	it sends a report to stderr.
;
if (size(is))(0) eq 0 then begin
  if is ne want then begin
    printf,-2,format='($,"ERROR(",a,"): ")', strtrim(n,2)
    printf,-2,format='(" is(",a,"), want(",a,")")',is,want
  endif else goto, is_ok
endif else begin
  s = size(is)
  if s(s(0)+1) eq 7 then $	;Strings?
    x = total(is ne want) $
  else x = total(abs(is-want))
    if x ne 0. then begin
      printf,-2,format='($,"ERROR(",a,"): ")', strtrim(n,2)
      printf,-2,format='(" ARRAY total is(",a,"), want(",a,")")',x,0.0
  endif else goto, is_ok
endelse

return

is_ok:
  printf, -2, format='("OK(", I0, ")")', n

end







pro test_extrac
; Test the EXTRAC procedure. These tests are hardly exhaustive ---
; especially with dimensions above 2.



;;;;;;;;;;; PART 1 --- Vector case
a = findgen(10)			; Test data

; Desired subarray is completely to left of the array
extrac_errprint, 1, extrac(a, -100, 60), fltarr(60)

; Subarray is completely to right of the array
extrac_errprint, 2, extrac(a, 100, 60), fltarr(60)

; Subarray overlaps partly on left
correct = fltarr(10)
correct(5) = a(0:4)
extrac_errprint, 3, extrac(a, -5, 10), correct

; Subarray overlaps partly on right
correct = fltarr(10)
correct(0) = a(5:9)
extrac_errprint, 4, extrac(a, 5, 10), correct

; Border Condition - Just left of array
extrac_errprint, 5, extrac(a, -10, 10), fltarr(10)

; Condition Border - One column overlaps on right
correct = fltarr(10)
correct(9) = a(0)
extrac_errprint, 6, extrac(a, -9, 10), correct

; Border Condition - Just right of array
extrac_errprint, 7, extrac(a, 10, 10), fltarr(10)

; Condition Border - One column overlaps on left
correct = fltarr(10,10)
correct(0) = a(9)
extrac_errprint, 8, extrac(a, 9, 10), correct

; Trivial case --- extract the entire array
extrac_errprint, 9, extrac(a, 0, 10), a

; Extract a completely interior region. This is what the subscript op does
extrac_errprint, 10, extrac(a, 2, 5), a(2:6)


;;;;;;;;;;; PART 2 --- 2D case


a = findgen(10,10)		; Test data

; Desired subarray is completely below the array
extrac_errprint, 11, extrac(a, -100, -100, 60, 60), fltarr(60, 60)

; Subarray is completely above the array
extrac_errprint, 12, extrac(a, 100, 100, 60, 60), fltarr(60, 60)

; Subarray overlaps partly on left
correct = fltarr(10,10)
correct(5,0) = a(0:4, 0:*)
extrac_errprint, 13, extrac(a, -5, 0, 10, 10), correct

; Subarray overlaps partly on right
correct = fltarr(10,10)
correct(0,0) = a(5:9, 0:*)
extrac_errprint, 14, extrac(a, 5, 0, 10, 10), correct

; Subarray overlaps partly on top
correct = fltarr(10,10)
correct(0,5) = a(0:*, 0:4)
extrac_errprint, 15, extrac(a, 0, -5, 10, 10), correct

; Subarray overlaps partly on bottom
correct = fltarr(10,10)
correct(0,0) = a(*, 5:9)
extrac_errprint, 16, extrac(a, 0, 5, 10, 10), correct

; Border Condition - Just left of array
extrac_errprint, 17, extrac(a, -10, 0, 10, 10), fltarr(10,10)

; Condition Border - One column overlaps on right
correct = fltarr(10,10)
correct(9,0) = a(0,*)
extrac_errprint, 18, extrac(a, -9, 0, 10, 10), correct

; Border Condition - Just right of array
extrac_errprint, 19, extrac(a, 10, 0, 10, 10), fltarr(10,10)

; Condition Border - One column overlaps on left
correct = fltarr(10,10)
correct(0,0) = a(9,*)
extrac_errprint, 20, extrac(a, 9, 0, 10, 10), correct

; Trivial case --- extract the entire array
extrac_errprint, 21, extrac(a, 0, 0, 10, 10), a

; Extract a completely interior region. This is what the subscript op does
extrac_errprint, 22, extrac(a, 2, 3, 5, 6), a(2:6,3:8)



;;;;;;;;;;; PART 3 --- 3D case


a = findgen(10,10,10)		; Test data

; Desired subarray is completely below the array
extrac_errprint, 23, extrac(a,-100,-100,-100,60,60,60), fltarr(60,60,60)

; Subarray is completely above the array
extrac_errprint, 24, extrac(a, 100, 100, 100, 60, 60, 60), fltarr(60,60,60)

; Subarray overlaps partly on left
correct = fltarr(10,10,10)
correct(5,0,0) = a(0:4, 0:*, 0:*)
extrac_errprint, 25, extrac(a, -5, 0, 0, 10, 10, 10), correct

; Subarray overlaps partly on right
correct = fltarr(10,10,10)
correct(0,0,0) = a(5:9, 0:*, 0:*)
extrac_errprint, 26, extrac(a, 5, 0, 0, 10, 10, 10), correct

; Subarray overlaps partly on top
correct = fltarr(10,10,10)
correct(0,5,0) = a(0:*, 0:4, 0:*)
extrac_errprint, 27, extrac(a, 0, -5, 0, 10, 10, 10), correct

; Subarray overlaps partly on bottom
correct = fltarr(10,10,10)
correct(0,0,0) = a(*, 5:9,*)
extrac_errprint, 28, extrac(a, 0, 5, 0, 10, 10, 10), correct

; Border Condition - Just left of array
extrac_errprint, 29, extrac(a, -10, 0, 0, 10, 10, 10), fltarr(10,10,10)

; Condition Border - One column overlaps on right
correct = fltarr(10,10,10)
correct(9,0,0) = a(0,*,*)
extrac_errprint, 30, extrac(a, -9, 0, 0, 10, 10, 10), correct

; Border Condition - Just right of array
extrac_errprint, 31, extrac(a, 10, 0, 0, 10, 10, 10), fltarr(10,10,10)

; Condition Border - One column overlaps on left
correct = fltarr(10,10,10)
correct(0,0,0) = a(9,*,*)
extrac_errprint, 32, extrac(a, 9, 0, 0, 10, 10, 10), correct

; Trivial case --- extract the entire array
extrac_errprint, 33, extrac(a, 0, 0, 0, 10, 10, 10), a

; Extract a completely interior region. This is what the subscript op does
extrac_errprint, 34, extrac(a, 2, 3, 4, 5, 6, 7), a(2:6,3:8,4:9)

end
