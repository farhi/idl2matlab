; $Id: threed.pro,v 1.1 1993/04/02 19:43:31 idl Exp $

PRO 	THREED,A,SP, Title = title, Xtitle = Xtitle, Ytitle=Ytitle
;
;+
; NAME:
;	THREED
;
; PURPOSE:
;	Plot a 2D array as a pseudo 3D plot.
;
; CATEGORY:
;	J7 Plotting, graphics, multi-dimensional.
;
; CALLING SEQUENCE:
;	THREED, A [, Sp]
;
; INPUTS:
;	A:	The two-dimensional array to plot.
;
; OPTIONAL INPUT PARAMETERS:
;	Sp:	Spacing between plot lines.  If sp is omitted, the spacing 
;		is set to: 
;			(MAX(A)-MIN(A))/ROWS
;		If Sp is negative, hidden lines are not removed.
;
; KEYWORDS:
;	TITLE:	The main plot title.
;
;	XTITLE:	The X axis title.
;
;	YTITLE:	The Y axis title.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A plot is produced on the currently selected graphics device.
;
; RESTRICTIONS:
;	Orientation of data is fixed.
;	Use the built-in procedure SURFACE for a more comprehensive surface 
;	plotting routine.
;
; PROCEDURE:
;	Straightforward.
;	A vector is maintained of the previous maximum plotted Y.
;	The PLOT and OPLOT procedures are used.
;
; MODIFICATION HISTORY:
;	VERSION 1.2, CREATED BY D. LINDLER, AUGUST 5,1980
;	VERSION 1.3, CREATED BY I. DEAN AHMAD, JANUARY 28, 1981
;	MODIFIED FOR VAX, DMS, SEPT, 1982.
;	Modified for Unix, DMS, 2/15/88.
;-
;
ON_ERROR,2		;RETURN TO CALLER IF ERROR
;
; DETERMINE SIZE OF A
;
IDEM=SIZE(A) & M=IDEM(1) & N=IDEM(2)
IF N_ELEMENTS(A) NE N*M THEN message, 'Sorry, not two dimensional.'
;
IF N_PARAMS(0) EQ 1 THEN SP = (MAX(A)-MIN(A))/FLOAT(N) ;FAKE SPACING
;
; DEFINE ARRAYS FOR THE X AND Y AXIS
;
X=INDGEN(M)
;
; DETERMINE PLOT SCALE
;
ADD = A+(INDGEN(M,N)/M)*ABS(SP) ;ARRAY + INCREMENT
YMAX=MAX(ADD)
YMIN=MIN(ADD)
!C=0		;DISABLE CURSOR
;
; DETERMINE IF HIDDEN LINES SHOULD BE PLOTTED
;
OLDY=ADD(*,0)
		;PLOT THE SCALE AND THE FIRST LINE
if n_elements(title) eq 0 then title = ''
if n_elements(xtitle) eq 0 then xtitle = ''
if n_elements(ytitle) eq 0 then ytitle = ''

PLOT,X,OLDY,xrange=[0.,m],yrange=[ymin,ymax], title=title, xtitle=xtitle,$
	ytitle=ytitle
;
  FOR I=1,N-1 DO BEGIN           ;LOOP TO PLOT EACH LINE
  Y=ADD(*,I)			;GET ROW
  IF SP GE 0. THEN Y=Y>OLDY     ; REMOVE HIDDEN LINES
  OPLOT,X,Y                    ;PLOT IT
  OLDY=Y
  END  ; FOR LOOP
RETURN
END  ;  THREED

