FUNCTION newfit,ww,a,$
                 PRINT=printing,PLOT=plot,file=file,nowindow=nowindow,$
                 xx=xx,ee=ee,yy=yy,datpp=datpp,$
                 xmin=xmin,xmax=xmax,$
                 nbgterms=nbgterms,npeaks=npks,$
                 amplitude=amplitude,bg=bg,position=position,width=width,intensity=intensity,eta=eta,$
                 H=H,S=S,L=L,sig_H=sig_H,sig_S=sig_S,sig_L=sig_L,$
                 sig_amplitude=sig_amplitude,sig_bg=sig_bg,sig_position=sig_position,sig_width=sig_width,sig_intensity=sig_intensity,sig_eta=sig_eta,$
                 TOL=TOL,ITMAX=ITMAX,$
                 UVW=UVW,pVoigt=pVoigt,click=click,guess=guess,search=search,$
                 inputfile=inputfile,outputfile=outputfile,maxwidth=maxwidth,maxsearchwidth=maxsearchwidth,minwidth=minwidth,$
                 damping=damping,iterations=iterations,bragg=bragg_shape,rectangle=rectangle_shape,HR=HR,sig_HR=sig_HR,$
		 GUI=GUI,backwards=backwards,initialiterations=initialiterations,widget=widget,noderivative=noderivative,func=func
;+
; Started: 04-Nov-96 by Th.Hansen, ILL-Grenoble
; Gaussian fit for sequential diagrams and multiple peaks - now also Convolution with Bragg, Lorentz (pseudo-Voigt) and asymmetry (Finger-Cox-Jephcoat)
;
; w is a LAMP workspace, it might be three-dimensional 
; - in that case sequential fits will be performed
; a is a vector 
;
; new: the vector a contains npeaks * peakpars (amplitude, position and FWHM, evtl. eta) + nbg numbers, 
;      with nbg = 1 (constant bg), 2 (linear bg.) or 3 (square bg.), etc.
;
; 'a' can be given as a vector, read from a file ('inputfile'), made graphically interactively ('click') or created automatically if not given, or set to zero
; The number of peaks to be found automatically has to be given ('npeaks'), except for graphical search ('click')
; The number of background polynom terms ('nbgterms') has to be given, also with keyword 'click'
; The refined values and sigmas can be overtaken with keyword parameters such as 'intensity', 'amplitude','bg', 'sig_intensity', 'sig_amplitude', or 'sig_bg', etc.
; These parameters will be two-dimensional (number of diagrams * number of peaks/background polynom terms)
; IF FWHM have to be determined from automatic peak-search, then they can be taken optionally from a Cagliotti function, 'UVW' then has to be given as a vector
; With the keyword 'pVoigt' set, peaks will be fitted as pseudo-Voigt function instead of pure Gauss
; The initial guess can be written out to a file ('outputfile') or a variable ('guess')
; Some output can be written to screen ('print') and/or to a file ('file')
; Graphical output will be created when 'plot' is set, 'nowindow' will force this output to the last used window and open no new one
; If not working in a LAMP environment, the necessary additional workspace data X (x-axis) and E (errors) can (must?) be given via the keywords 'xx' and 'ee',
; or - if inside a structure datp containing datp.x and datp.e - as a structure via the keyword 'datpp'
; The default damping factor of 0.5 and number of iterative (10) least square curve fits can be changed ('damping', 'iterations')
; If taken from a inputfile, each parameter has an individual damping factor, found in every second line, as written to the file with 'outputfile'
; 'backwards' starts from the last diagram of a fiven workspace instead the first one
;
; Well, all that widget stuff (keyword 'click' or 'GUI') has not yet been documented, well, as it SHOULD be self-explaining ;-)
; You can until the clicking on background points (which you can undo as well now) change the order of the polynom
; As long as you have not stopped the background determination (by clicking on the corresponding button), you can change some settings 
; of the peakshape function (asymmetry after Finger-Cox-Jephcoat, pseudo-Voig or simple Gauss, Rectangle convolution, Bragg-formula convolution),
; later I shall try to allow for those changes at a later state as well (which is difficult however, as it might change the number of parameters on the road).
;
; If you work with an inputfile, you may define peaks as well which will emerge only later, giving an amplitude of zero
; These peaks won`t be taken into account during the refinement of the first pattern, from than on, before each run of the 
; least squares routine (iterations+1 times for each pattern, containing itmax proper least sqare iterations each - or less, damping applied inside),
; the peaks are checked: if they are non-zero and their presence improves the fit, then they are taken as they are as inital guess,
; if not, then the maximum difference count rate at the presumed peak position plus minus three sigma is taken, 
; if this (minus the background and the other peaks) is less than the counting error, 
; the peak is not taken, otherwise, it is checked, if the peak with this counting improves chi2 more than 10% only, only then, 
; the peak is taken into account.
;
; Modification 22-May-97 by Th. HANSEN:; fit.pro - sequential fit of multiple Gauss peaks (wrongly successively treated!)
; Modification 08-Sep-97 by Th. HANSEN:; several bugs fixed (array-indexing for a and x, etc.)
; Modification 04-Nov-97 by Th. HANSEN:; Voigt function and asymmetry correction ... ? Not really a success ...
; Modification 17-Nov-00 by Th. HANSEN:; new gaussfunc: gauss_square/poly.pro - new name: 'newfit' replaces 'fit'
; Modification 20-Nov-00 by Th. HANSEN:; everything changes ... (experiment M. Huber, E. Peters, B. Walk, T. Fehr, S. Zuern)
; Modification 05-Dec-00 by Th. HANSEN:; GUI replaces click, XREGISTERED, workspace numbers for output parameter keywords, y-axis ...
; Modification 06-Dec-00 by Th. HANSEN:; y values in file (W. Kuhs), OPLOTERR for error bars
; Modification 08-Dec-00 by Th. HANSEN:; call_procedure, noderivative - preparing Finger, Cox, Jephcoat: fcj.pro
; Modification 12-Dec-00 by Th. HANSEN:; use of an own lsfit.pro instead of IDL`s curvefit.pro for the least squares algorithm
; Modification 13-Dec-00 by Th. HANSEN:; Rectangle convolution and Bragg formula in gauss-poly.pro, tried to implement derivatives without success
; Modification 14-Dec-00 by Th. HANSEN:; Emerging peaks, keep them at zero as long as necessary ...
; Modification 15-Dec-00 by Th. HANSEN:; Emerging and disapearing peaks: performing limited peaksearches before each least-squares run, some documentation
; Modification 17-Dec-00 by Th. HANSEN:; Don`t consider emerging just beside existing peaks ... 
; Modification 19-Dec-00 by Th. HANSEN:; Tune recognition of emerging and of disappearing peaks: some tests are done AFTER least-squares to throw away badly refined peaks again
; Modification 23-Dec-00 by Th. HANSEN:; emerging and of disappearing peaks: debugging ...
; Modification 24-Dec-00 by Th. HANSEN:; Differential Peaksearch, first tries
; Modification 26-Dec-00 by Th. HANSEN:; Differential Peaksearch, as a new option ('search'), instead of taking last fit as initial guess
;
; to be done: 
;             constraints (UVW) on peak widths
;             crystallographical constraints on peak positions? (this would appproach a Rietveld style full profile fit)
;             zooming, and fixing xmin/max interactively
;             Gauss-Legendre integration over cellwidth
;             exponential 'wing'-effect of Pierre Convert
;             emergency exit if least squares block
;             handle new, emerging peaks - this is not so easy - same applies for disappearing peaks, of course:
;                 one 'solution' consists in looking at the peak`s contribution to the goodness of fit before each least-squares call
;                 the peak`s intensity is guessed at it`s position, if it is negative, or the peak does not improve chi2, 
;                 then set it to zero intensity and exclude it from fitting this round
;                 Up to now, emerging peaks must already exist somehow in the list of peaks, 
;                 an automatic peak-search for supplementary peaks could be a nice option
;                 Also could we try to avoid the polynominal background becoming negative
;                 An additional criterion on width could be applied to declare peaks 'broadened' out
;             GUI-widget: newfit should be called (but then as a procedure ?!) with a simple button click, so without any parameter or keyword
;                 This would mean the possibility of specifying an input-workspace, an output workspace, the x-range 
;                 and optional additional outputs in workspaces, using the widget.
;                 Consequently some parameters (xmin, xmax, peakpars, etc.) should kept changeable for longer than it is now
;
;-
;on_error,2              ;Return to caller if error - that`s for later (and in lsfit, fcj and gauss_poly as well!), as soon as theree is less need of debugging ...
;catch,error_status
error_status=0
COMMON fit,voigt,nterms,fitflag,key,undo,npeaks,rectangle,bragg,peakpars
COMMON clickwidget_cbk,clickwidget,title_field,numbers,nterms_label,up_button,down_button,bg_no,npeaks_label,peak_field,option_field,peak_no,up_peak,down_peak,$
    peak_ampli,peak_pos,peak_wid,peak_eta,voigt_field,voigt_button,buttons,next_button,undo_button,text_field,plot_zone
IF NOT KEYWORD_SET(maxwidth) THEN minwidth=0.2
IF NOT KEYWORD_SET(maxwidth) THEN maxwidth=5
IF NOT KEYWORD_SET(maxsearchwidth) THEN maxsearchwidth=0.15
IF KEYWORD_SET(bragg_shape) THEN bragg=1 ELSE bragg=0
IF KEYWORD_SET(rectangle_shape) THEN rectangle=1 ELSE rectangle=0
IF KEYWORD_SET(npks) THEN npeaks=npks ELSE npeaks=0
IF KEYWORD_SET(GUI) THEN click=1
; ################## are the output parameters numbers of workspaces ? ################################################
W_intensity=0
W_position=0
W_amplitude=0
W_width=0
W_eta=0
W_bg=0
W_H=0
W_S=0
W_L=0
W_HR=0
IF KEYWORD_SET(intensity) THEN IF N_ELEMENTS(intensity) EQ 1 THEN IF intensity GT 0 AND intensity LE 20 THEN W_intensity=intensity 
IF KEYWORD_SET(position) THEN IF N_ELEMENTS(position) EQ 1 THEN IF position GT 0 AND position LE 20 THEN W_position=position 
IF KEYWORD_SET(width) THEN IF N_ELEMENTS(width) EQ 1 THEN IF width GT 0 AND width LE 20 THEN W_width=width
IF KEYWORD_SET(eta) THEN IF N_ELEMENTS(eta) EQ 1 THEN IF eta GT 0 AND eta LE 20 THEN W_eta=eta 
IF KEYWORD_SET(amplitude) THEN IF N_ELEMENTS(amplitude) EQ 1 THEN IF amplitude GT 0 AND amplitude LE 20 THEN W_amplitude=amplitude
IF KEYWORD_SET(bg) THEN IF N_ELEMENTS(bg) EQ 1 THEN IF bg GT 0 AND bg LE 20 THEN W_bg=bg 
IF KEYWORD_SET(H) THEN IF N_ELEMENTS(H) EQ 1 THEN IF H GT 0 AND H LE 20 THEN W_H=H 
IF KEYWORD_SET(S) THEN IF N_ELEMENTS(S) EQ 1 THEN IF S GT 0 AND S LE 20 THEN W_S=S 
IF KEYWORD_SET(L) THEN IF N_ELEMENTS(L) EQ 1 THEN IF L GT 0 AND L LE 20 THEN W_L=L 
IF KEYWORD_SET(HR) THEN IF N_ELEMENTS(HR) EQ 1 THEN IF HR GT 0 AND HR LE 20 THEN W_HR=HR 
; ########################## Initialize a widget for output of what would have been printed into a terminal - maybe usefull for Windows-Runtime #########
IF KEYWORD_SET(widget) THEN BEGIN
  w0=WIDGET_BASE(title='NewFit.Pro Logging')
  w1=widget_text(w0)
  WIDGET_CONTROL,w0,/realize
  WIDGET_CONTROL,w1,SET_VALUE='Starting newfit'
ENDIF
; ################### Start sequential fitting from the first or the last diagram ##################################
IF NOT KEYWORD_SET(backwards) THEN BEGIN
  start=0 
  step=1
  last=N_ELEMENTS(ww(0,*))-1
ENDIF ELSE BEGIN
  last=0 
  step=-1
  start =N_ELEMENTS(ww(0,*))-1
ENDELSE
; ################### The variable a contains the parameters to be fitted, it must be defined #########################
IF N_PARAMS() LT 2 THEN a=0
; ################### The (general) damping factor to be applied to parameter shifts ##################################
IF NOT KEYWORD_SET(damping) THEN damping=0.5
; ################### Number of repeated calls to the least squares procedure CURVEFIT and damping of shifts ##########
IF NOT KEYWORD_SET(iterations) THEN iterations=5
; ################### just a flag - not really used, I think ##########################################################
fitflag=0
; ################### 
IF NOT KEYWORD_SET(ref) THEN ref=0
IF NOT KEYWORD_SET(noderivative) THEN noderivative=0
IF NOT KEYWORD_SET(func) THEN func="GAUSS_POLY" 
tmp_S=0.050
tmp_H=0.150
tmp_L=1.471
IF STRPOS(func,'fcj') GE 0 THEN BEGIN
    PRINT,'Finger Cox Jephcoat asymmetry'
    furtherterms=3
    IF KEYWORD_SET(H) THEN IF H(0) GT 0 THEN tmp_H=H
    IF KEYWORD_SET(S) THEN IF S(0) GT 0 THEN tmp_S=S
    IF KEYWORD_SET(L) THEN IF L(0) GT 0 THEN tmp_L=L
    a=[a,tmp_S,tmp_H,tmp_L]
ENDIF ELSE furtherterms=0
; ################## READING IN FROM AN INPUTFILE #######################
IF KEYWORD_SET(inputfile) THEN BEGIN
  OPENR,unit,inputfile,/GET_LUN
  npeaks=0
  voigt=0
  nbgterms=0
  furtherterms=0
  line=' '
  READF,unit,line
  line=line +' '+STRING(0)+' '+STRING(0)+' '+STRING(0)
  READS,line,npeaks,nbgterms,voigt,furtherterms
  IF voigt EQ 1 THEN BEGIN
    pvoigt=1
    peakpars=4
  ENDIF ELSE BEGIN
    peakpars=3 
    pVoigt=0
  ENDELSE
  IF rectangle EQ 1 THEN rectangle=rectangle+1
  a=FLTARR(npeaks*peakpars+nbgterms)
  ref=a 
  b=FLTARR(peakpars)
  FOR i=0,npeaks-1 DO BEGIN
    READF,unit,line
    line=line+' '+STRING(0.0)
    READS,line,b
    IF i EQ 0 THEN a=b ELSE a=[a,b]
    PRINT,b
    IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE=STRING(b)
    READF,unit,line
    line=line+' '+STRING(0.0)
    READS,line,b
    IF i EQ 0 THEN ref=b ELSE ref=[ref,b]
  ENDFOR
  IF nbgterms GT 0 THEN BEGIN
    b=FLTARR(nbgterms)
    READF,unit,line
    line=line+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)
    READS,line,b
    PRINT,b
    IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE=STRING(b)
    IF npeaks EQ 0 THEN a=b ELSE a=[a,b]
    READF,unit,line
    line=line+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)
    READS,line,b
    PRINT,b
    IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE=STRING(b)
    IF npeaks EQ 0 THEN ref=b ELSE ref=[ref,b]
  ENDIF
  PRINT,furtherterms,' further terms from inputfile - If you want the Finger-Cox-Jephcoat function used for asymmetry, call newfit with func="fcj"'
  IF furtherterms GT 0 THEN BEGIN
    b=FLTARR(furtherterms)
    READF,unit,line
    line=line+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)
    READS,line,b  
    PRINT,b
    IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE=STRING(b)  
    IF npeaks+nbgterms EQ 0 THEN a=b ELSE a=[a,b]
    READF,unit,line
    line=line+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)+' '+STRING(0.0)
    READS,line,b
    PRINT,b
    IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE=STRING(b)
    IF npeaks+nbgterms EQ 0 THEN ref=b ELSE ref=[ref,b]
    FREE_LUN,unit
  ENDIF
ENDIF
;HELP,a
IF KEYWORD_SET(nbgterms) THEN nterms=nbgterms ELSE nterms=1
nbgterms=nterms
IF KEYWORD_SET(pvoigt) THEN BEGIN 
  peakpars=4
  voigt=1
ENDIF ELSE BEGIN
  peakpars=3
  voigt=0
ENDELSE
IF NOT KEYWORD_SET(eta) THEN eta=0.1 ELSE eta=eta(0)
sig2fwhm=2.*SQRT(2.*ALOG(2.))
IF (KEYWORD_SET(plot) OR KEYWORD_SET(click)) THEN BEGIN
  IF NOT (KEYWORD_SET(nowindow) OR KEYWORD_SET(click)) THEN BEGIN
    WINDOW,0,XSIZE=800,YSIZE=500
    IF NOT KEYWORD_SET(click) THEN WINDOW,1,XSIZE=800,YSIZE=200,YPOS=500
    WSET,0
  ENDIF 
  TVLCT,[0,255,0,0],[0,0,255,0],[0,0,0,255]
  PRINT,'Default Color',!P.COLOR
  !P.COLOR=0
ENDIF
IF NOT KEYWORD_SET(datpp) THEN take_datp,datp ELSE datp=datpp
IF N_PARAMS() LE 1 AND NOT KEYWORD_SET(inputfile) THEN a=0
a=REFORM(a(*,0),N_ELEMENTS(a(*,0)))
;HELP,a
IF NOT KEYWORD_SET(npeaks) AND NOT KEYWORD_SET(click) THEN BEGIN
  npeaks=(N_ELEMENTS(a)-1-nterms-furtherterms)/3
  PRINT,npeaks, ' peaks will be treated'
  IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE=STRING(npeaks)+' peaks will be treated'
ENDIF
nterms=(nterms>1)<6                      ; minimum 1 term (constant bg), maximum 3 terms (square)
a=[a,FLTARR(npeaks*peakpars+nterms+furtherterms)]
;HELP,a
IF voigt EQ 0 THEN a=a(0:npeaks*peakpars+nterms+furtherterms-1) ELSE a=a(0:npeaks*peakpars+nterms+furtherterms-1)                  ; shape the parameter vector a
IF NOT KEYWORD_SET(xx) THEN xx=datp.x 
IF NOT KEYWORD_SET(ee) THEN ee=datp.e 
IF NOT KEYWORD_SET(yy) THEN IF N_ELEMENTS(datp.y) EQ N_ELEMENTS(ww(0,*)) THEN yy=datp.y ELSE yy=INDGEN(N_ELEMENTS(ww(0,*)))
IF N_ELEMENTS(yy) NE N_ELEMENTS(ww(0,*)) THEN yy=INDGEN(N_ELEMENTS(ww(0,*)))
IF N_ELEMENTS(xx(0,*)) GT 1 THEN BEGIN
  PRINT,'X is 2D - I will use the X for the first diagram for all diagrams - this is maybe not correct!!'
  IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='X is 2D - I will use the X for the first diagram for all diagrams - this is maybe not correct!!'
ENDIF
IF NOT KEYWORD_SET(maxexclusion) THEN maxexclusion=1.0
IF NOT KEYWORD_SET(overlap) THEN overlap=3.
IF NOT KEYWORD_SET(xmin) THEN xmin=MIN(xx)
IF NOT KEYWORD_SET(xmax) THEN xmax=MAX(xx)
index=WHERE(xx(*,0) GE xmin AND xx(*,0) LE xmax AND ww(*,start) GT 0,count)
IF count LE 0 THEN BEGIN
  PRINT,'There is a problem with the x-range for the peak searching loop'
  IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='There is a problem with the x-range for the peak searching loop'
  RETURN,0
ENDIF
w=ww(index,*)
e=ee(index,*)
y=yy
x=REFORM(xx(index,0),count)
; ##################### Graphical, interactive peak search ###################################
IF KEYWORD_SET(click) THEN BEGIN
  undo=0
  plot=1 ; That is just logic - if I am working interactively, I should show what is happening all the time
  peak_number=0
  IF NOT XREGISTERED('NewFit') THEN BEGIN
    clickwidget =WIDGET_BASE(title='NewFit',/COL,XSIZE=1000)
    title_field =WIDGET_TEXT(clickwidget,VALUE='Choose background points with mouse clicks! Bugs to hansen@ill.fr!')
    numbers     =WIDGET_BASE(clickwidget,/ROW)
    nterms_label=WIDGET_LABEL(numbers,VALUE=STRCOMPRESS(nterms,/RE)+' polynom terms')
    up_button   =WIDGET_BUTTON(numbers,VALUE='^')
    down_button =WIDGET_BUTTON(numbers,VALUE='v')
    bg_no       =WIDGET_LABEL(numbers,VALUE=STRCOMPRESS(0,/RE)+' bg. pnts.')
    npeaks_label=WIDGET_LABEL(numbers,VALUE=STRCOMPRESS(nterms,/RE)+' peaks')
    fcj_field =WIDGET_BASE(numbers,/NONEXCLUSIVE)
    fcj_button=WIDGET_BUTTON(fcj_field,VALUE='F.C.J.-Asym.')
    fcj_S  =WIDGET_TEXT(numbers,VALUE=STRCOMPRESS(tmp_S,/RE),/EDITABLE)
    fcj_H  =WIDGET_TEXT(numbers,VALUE=STRCOMPRESS(tmp_H,/RE),/EDITABLE)
    fcj_L  =WIDGET_TEXT(numbers,VALUE=STRCOMPRESS(tmp_L,/RE),/EDITABLE)
    bragg_field  =WIDGET_BASE(numbers,/NONEXCLUSIVE)
    bragg_button  =WIDGET_BUTTON(bragg_field,VALUE='Bragg')
    IF STRPOS(func,'fcj') GE 0 THEN WIDGET_CONTROL,FCJ_button,/SET_BUTTON
    IF bragg THEN WIDGET_CONTROL,Bragg_button,/SET_BUTTON
    peak_field = WIDGET_BASE(clickwidget,/ROW)
    peak_no     =WIDGET_LABEL(peak_field,VALUE='Pk. no.'+STRCOMPRESS(peak_number,/RE)+':')
    up_peak     =WIDGET_BUTTON(peak_field,VALUE='^')
    down_peak   =WIDGET_BUTTON(peak_field,VALUE='v')
    peak_ampli  =WIDGET_TEXT(peak_field,XSIZE=10,VALUE='0.0',/EDITABLE)
    peak_pos    =WIDGET_TEXT(peak_field,XSIZE=10,VALUE='0.0',/EDITABLE)
    peak_wid    =WIDGET_TEXT(peak_field,XSIZE=10,VALUE='0.0',/EDITABLE)
    peak_eta    =WIDGET_TEXT(peak_field,XSIZE=10,VALUE='0.0',/EDITABLE)
    peak_HR     =WIDGET_TEXT(peak_field,XSIZE=10,VALUE='0.0',/EDITABLE)
    ;option_field = WIDGET_BASE(clickwidget,/ROW)
    voigt_field =WIDGET_BASE(peak_field,/NONEXCLUSIVE)
    voigt_button=WIDGET_BUTTON(voigt_field,VALUE='ps.-Voigt')
    IF voigt THEN WIDGET_CONTROL,voigt_BUTTON,/SET_BUTTON
    rect_field =WIDGET_BASE(peak_field,/NONEXCLUSIVE)
    rect_button=WIDGET_BUTTON(rect_field,VALUE='rect.convol.')
    IF rectangle THEN WIDGET_CONTROL,rect_BUTTON,/SET_BUTTON
    buttons     =WIDGET_BASE(clickwidget,/ROW)
    next_button =WIDGET_BUTTON(buttons,VALUE='Stop background determination - go to peak search!')
    undo_button =WIDGET_BUTTON(buttons,VALUE='Undo last point of background!')
    text_field  =WIDGET_TEXT(buttons)
    plot_zone   =WIDGET_DRAW(clickwidget,/BUTTON_EVENTS,XSIZE=1000,YSIZE=600)
    WIDGET_CONTROL,clickwidget,/REALIZE
    XMANAGER,'NewFit',clickwidget,Event_Handler='LAMP_EVENT_PARSER',/just_reg
  ENDIF ELSE BEGIN
    WIDGET_CONTROL,title_field,SET_VALUE='Choose background points with mouse clicks! Bugs to hansen@ill.fr!'
    WIDGET_CONTROL,nterms_label,SET_VALUE=STRCOMPRESS(nterms,/RE)+' polynom terms'
    WIDGET_CONTROL,up_button,SET_VALUE='^'
    WIDGET_CONTROL,down_button,SET_VALUE='v'
    WIDGET_CONTROL,bg_no,SET_VALUE=STRCOMPRESS(0,/RE)+' bg. pnts.'
    WIDGET_CONTROL,npeaks_label,SET_VALUE=STRCOMPRESS(nterms,/RE)+' peaks'
    WIDGET_CONTROL,peak_no,SET_VALUE='Pk. no.'+STRCOMPRESS(peak_number,/RE)+':'
    WIDGET_CONTROL,up_peak,SET_VALUE='^'
    WIDGET_CONTROL,down_peak,SET_VALUE='v'
    WIDGET_CONTROL,peak_ampli,SET_VALUE='0.0'
    WIDGET_CONTROL,peak_pos,SET_VALUE='0.0'
    WIDGET_CONTROL,peak_wid,SET_VALUE='0.0'
    WIDGET_CONTROL,peak_eta,SET_VALUE='0.0'
    WIDGET_CONTROL,voigt_button,SET_VALUE='ps.-Voigt'
    IF voigt THEN WIDGET_CONTROL,voigt_BUTTON,/SET_BUTTON
    IF rectangle THEN WIDGET_CONTROL,rect_BUTTON,/SET_BUTTON
    IF STRPOS(func,'fcj') GE 0 THEN WIDGET_CONTROL,fcj_BUTTON,/SET_BUTTON
    WIDGET_CONTROL,next_button,SET_VALUE='Stop background determination - go to peak search!'
    WIDGET_CONTROL,undo_button,SET_VALUE='Undo last point of background!'
  ENDELSE
  WIDGET_CONTROL,plot_zone, GET_VALUE = win_num
  WSET,win_num
  npeaks=0
  bgterms=0
  j=0
  x1=xmin
  x2=xmax
  y1=0
  y2=MAX(w)
  x0=x1
  y0=y1
  PLOT,x,w(*,start),TITLE='Click on background positions!',YRANGE=[0,MAX(w)],background=255,xstyle=1,ystyle=1,color=0
  PRINT,nterms,' background terms'
  OPLOTERR,x,w(*,start),e(*,start),0
  REPEAT BEGIN 
    widget_result=WIDGET_EVENT([up_button,down_button,undo_button,next_button,plot_zone,voigt_button,rect_button,fcj_button,bragg_button])
    IF widget_result.ID EQ bragg_button THEN BEGIN
      IF bragg THEN bragg=0 ELSE bragg=1
      PRINT,'Bragg Convolution:',bragg
    ENDIF
    IF widget_result.ID EQ voigt_button THEN BEGIN
      IF voigt THEN voigt=0 ELSE voigt=1
      PRINT,'pseudo Voigt:',voigt
    ENDIF
    IF widget_result.ID EQ rect_button THEN BEGIN
      IF rectangle THEN rectangle=0 ELSE rectangle=1
      PRINT,'Rectangle Convolution:',rectangle
    ENDIF
    IF widget_result.ID EQ fcj_button THEN BEGIN
      IF STRPOS(func,'fcj') GE 0 THEN func='GAUSS_POLY' ELSE func='fcj'
      PRINT,'Function used: ',func
    ENDIF
    IF widget_result.ID EQ up_button THEN BEGIN
      nterms=nterms+1
      bgterms=N_ELEMENTS(bg_w)<nterms
      WIDGET_CONTROL,nterms_label,SET_VALUE=STRCOMPRESS(nterms,/RE)+' polynom terms'
    ENDIF
    IF widget_result.ID EQ down_button THEN BEGIN
      nterms=nterms-1
      bgterms=N_ELEMENTS(bg_w)<nterms
      WIDGET_CONTROL,nterms_label,SET_VALUE=STRCOMPRESS(nterms,/RE)+' polynom terms'
    ENDIF
    IF widget_result.ID EQ plot_zone THEN BEGIN
      PRINT,'Event in plot zone'
      IF widget_result.PRESS THEN BEGIN
        PRINT,'Press in plot zone'
        resultxy=convert_coord(widget_result.X,widget_result.Y,/device,/to_data)
        x0=resultxy(0)
        y0=resultxy(1)
        WIDGET_CONTROL,text_field,SET_VALUE=STRCOMPRESS(x0,/RE)+STRCOMPRESS(y0)
        PRINT,x0,y0
        IF x0 GE x1 AND x0 LE x2 AND y0 GE y1 AND y0 LE y2 THEN BEGIN 
          pos=MIN(ABS(x0-x),i)
          PRINT,STRING(pos)+STRING(x0)+STRING(i)+STRING(min(x))+STRING(max(x))
          IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE=STRING(pos)+STRING(x0)+STRING(i)+STRING(min(x))+STRING(max(x))
          bgterms=(bgterms+1)<nterms
          PLOT,x,w(*,start),TITLE='Click on background positions!',YRANGE=[0,MAX(w)],background=255,color=0,xstyle=1,ystyle=1
          OPLOTERR,x,w(*,start),e(*,start),0
          PLOTS,[x0,x0],[MIN(w(*,start)),w(i,start)],COLOR=2
          xyouts,x0,w(i),STRCOMPRESS((x0))+STRCOMPRESS(ROUND(w(i))),COLOR=1,CHARSIZE=.8 
          PRINT,'Background',j,' at',x0,' degrees,',w(i,start),' counts'
          IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='Background'+STRING(j)+' at'+STRING(x0)+' degrees,'+STRING(w(i,start))+' counts'
          IF j EQ 0 THEN BEGIN
            bg_x=x0
            bg_w=w(i,start)
            a=w(i,start)
            background=x*0.0+w(i,start)
          ENDIF ELSE BEGIN
            bg_x=[bg_x,x0]
            bg_w=[bg_w,w(i,start)]
            IF bgterms GT 1 THEN a=POLY_FIT(bg_x,bg_w,bgterms-1) ELSE a=[TOTAL(bg_w)/N_ELEMENTS(bg_w)]
            a=REFORM(a,N_ELEMENTS(a))
            PRINT,'Polynom: '+STRING(a)
            IF bgterms GT 1 THEN background=POLY(x,a) ELSE background=bg_x*0.0+a(0)
          ENDELSE
          OPLOT,x,background,COLOR=3,LINE=0
          j=j+1
        ENDIF ELSE PRINT,x1,y1,x2,y2
      ENDIF
      xyouts,MIN(x),MIN(w),'x',COLOR=1,CHARSIZE=1 
    ENDIF
    IF widget_result.ID EQ undo_button THEN BEGIN
      undo=0
      PLOT,x,w(*,start),TITLE='Click on background positions!',YRANGE=[0,MAX(w)],background=255,color=0,xstyle=1,ystyle=1
      OPLOTERR,x,w(*,start),e(*,start),0
      bgterms=(bgterms-1)>0
      IF j EQ 1 THEN BEGIN
        j=0
        a=MIN(w(*,start))
        background=x*0.0+MIN(w(*,start))
      ENDIF ELSE BEGIN
        j=j-1
        bg_x=bg_x(0:N_ELEMENTS(bg_x)-2)
        bg_w=bg_w(0:N_ELEMENTS(bg_w)-2)
        IF bgterms GT 1 THEN BEGIN
          a=POLY_FIT(bg_x,bg_w,bgterms-1) 
          a=REFORM(a,N_ELEMENTS(a))
          background=POLY(x,a)
        ENDIF ELSE BEGIN
          a=bg_w(0)
          background=x*0.0+a
        ENDELSE
      ENDELSE
      OPLOT,x,background,COLOR=3,LINE=0
    ENDIF
    WIDGET_CONTROL,bg_no,SET_VALUE=STRCOMPRESS(N_ELEMENTS(bg_x),/RE)+' bg. poinpnts.ts'
  ENDREP UNTIL widget_result.ID EQ next_button
  PRINT,'Finished background ...',nterms,' terms'
  WIDGET_CONTROL,up_button,SET_VALUE='';/DESTROY
  WIDGET_CONTROL,down_button,SET_VALUE='';/DESTROY
  IF bgterms EQ 0 THEN a=MIN(w(*,start))
  j=0
  IF N_ELEMENTS(a) EQ 1 THEN background=x*0.0+a(0) ELSE background=POLY(x,a)
  ; ******************* Now searching Peaks interactively ... ***************************************************
  PLOT,x,w(*,start),TITLE='Click on peaks maximums!',YRANGE=[0,MAX(w)],background=255,color=0,xstyle=1,ystyle=1
  OPLOTERR,x,w(*,start),e(*,start),0
  OPLOT,x,background,COLOR=3,LINE=1
  IF STRPOS(func,'fcj') GE 0 THEN BEGIN
    PRINT,'Finger Cox Jephcoat asymmetry'
    furtherterms=3
    IF KEYWORD_SET(H) THEN IF H(0) GT 0 THEN tmp_H=H
    IF KEYWORD_SET(S) THEN IF S(0) GT 0 THEN tmp_S=S
    IF KEYWORD_SET(L) THEN IF L(0) GT 0 THEN tmp_L=L
    a=[a,tmp_S,tmp_H,tmp_L]
  ENDIF ELSE BEGIN
    furtherterms=0
  ENDELSE
  a=[a,FLTARR(nterms+furtherterms)]
  a=a(0:nterms+furtherterms-1)
  WIDGET_CONTROL,title_field,SET_VALUE='Choose peaks by clicking on peak maximum and releasing in the slope!'
  WIDGET_CONTROL,next_button,SET_VALUE='Stop peak search and go on to fit of initial guess!'
  PRINT,'pseudo-Voigt Button Value: ',voigt
  IF voigt THEN peakpars=4 ELSE BEGIN
    peakpars=3
    WIDGET_CONTROL,peak_eta,SET_VALUE='';,/DESTROY
  ENDELSE
  IF rectangle EQ 1 THEN peakpars=peakpars+1
  print,peakpars,' parameters per peak'
  print,a
  REPEAT BEGIN 
    IF voigt THEN widget_result=WIDGET_EVENT([undo_button,next_button,plot_zone,up_peak,down_peak,peak_ampli,peak_pos,peak_wid,peak_eta,peak_HR,fcj_S,fcj_H,fcj_L,bragg_button]) ELSE BEGIN
      widget_result=WIDGET_EVENT([undo_button,next_button,plot_zone,up_peak,down_peak,peak_ampli,peak_pos,peak_wid,peak_HR,fcj_S,fcj_H,fcj_L,bragg_button]) 
    ENDELSE
    IF widget_result.ID EQ bragg_button THEN BEGIN
      IF bragg THEN bragg=0 ELSE bragg=1
      PRINT,'Bragg Convolution:',bragg
    ENDIF
    IF widget_result.ID EQ up_peak THEN BEGIN
      peak_number=(peak_number+1)<(npeaks-1)
    ENDIF
    IF widget_result.ID EQ down_peak THEN BEGIN
      peak_number=(peak_number-1)>0
    ENDIF
    IF widget_result.ID EQ plot_zone THEN IF widget_result.PRESS THEN BEGIN
      resultxy=convert_coord(widget_result.X,widget_result.Y,/device,/to_data)
      PRINT,resultxy
      x0=resultxy(0)
      y0=resultxy(1)
      PRINT,x0,y0
      WIDGET_CONTROL,text_field,SET_VALUE=STRCOMPRESS(x0,/RE)+STRCOMPRESS(y0)
      IF x0 GE x1 AND x0 LE x2 AND y0 GE y1 AND y0 LE y2 THEN BEGIN 
        pos=MIN(ABS(x0-x),i)
        CALL_PROCEDURE,func,x,a,g
        npeaks=npeaks+1
        IF rectangle THEN BEGIN
          IF voigt EQ 0 THEN a=[w(i,start)-g(i),x0,0.,0.,a] ELSE a=[w(i,start)-g(i),x0,0.,eta,0.,a] 
        ENDIF ELSE IF voigt EQ 0 THEN a=[w(i,start)-g(i),x0,0.,a] ELSE a=[w(i,start)-g(i),x0,0.,eta,a] 
        PLOTS,[x0,x0],[MIN(w(*,start)),w(i,start)],COLOR=2
        xyouts,x0,w(i,start),STRCOMPRESS((x0))+STRCOMPRESS(ROUND(w(i,start))),COLOR=1,CHARSIZE=.8 
        REPEAT widget_result=WIDGET_EVENT([plot_zone]) UNTIL widget_result.RELEASE
        IF widget_result.RELEASE THEN BEGIN
          resultxy=convert_coord(widget_result.X,widget_result.Y,/device,/to_data)
          PRINT,resultxy
          x0=resultxy(0)
          y0=resultxy(1)
        ENDIF
        pos=MIN(ABS(x0-x),i)
        a(2)=ABS((x0-a(1))/SQRT(-2*ALOG((w(i,start)-g(i))/a(0))))
        WHILE -2*ALOG((w(i,start)-g(i))/a(0)) LE 0 OR x0 EQ a(1)  DO BEGIN
          PRINT,'Try again to click on another point of peak!',-2*ALOG((w(i)-g(i))/a(0))
          xyouts,MIN(x),(MAX(w)-MIN(w))/2.,'Click again on another point of the peak!',COLOR=1,CHARSIZE=.8 
          IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='Try again to click on another point of peak!'+STRING(-2*ALOG((w(i)-g(i))/a(0)))
          REPEAT widget_result=WIDGET_EVENT([plot_zone]) UNTIL widget_result.PRESS
          IF widget_result.PRESS THEN BEGIN
            resultxy=convert_coord(widget_result.X,widget_result.Y,/device,/to_data)
            PRINT,resultxy
            x0=resultxy(0)
            y0=resultxy(1)
          ENDIF
          pos=MIN(ABS(x0-x),i)
          a(2)=ABS((x0-a(1))/SQRT(-2*ALOG((w(i,start)-g(i))/a(0))))
        ENDWHILE
        PRINT,'Peak',j,' at',a(1),' degrees, amplitude',a(0),' counts, FWHM',a(2)*sig2fwhm,' degrees'
        IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='Peak'+STRING(j)+' at'+STRING(a(1))+' degrees, amplitude'+STRING(a(0))+' counts, FWHM'+STRING(a(2)*sig2fwhm)+' degrees'
        j=j+1
      ENDIF ELSE CURSOR,x0,y0,/UP 
      xyouts,MIN(x),0,'X',COLOR=1,CHARSIZE=1
      IF rectangle THEN a(peakpars-1)=a(2)/10.
    ENDIF 
    IF widget_result.ID eq undo_button THEN BEGIN
      npeaks=npeaks-1
      a=a(peakpars:N_ELEMENTS(a)-1)
    ENDIF
    IF widget_result.ID EQ peak_ampli THEN BEGIN
      WIDGET_CONTROL,peak_ampli,GET_VALUE=valstr
      a(peak_number*peakpars)=FLOAT(valstr(0))
    ENDIF
    IF widget_result.ID EQ peak_pos THEN BEGIN
      WIDGET_CONTROL,peak_pos,GET_VALUE=valstr
      a(peak_number*peakpars+1)=FLOAT(valstr(0))
    ENDIF
    IF widget_result.ID EQ peak_wid THEN BEGIN
      WIDGET_CONTROL,peak_wid,GET_VALUE=valstr
      a(peak_number*peakpars+2)=FLOAT(valstr(0))
    ENDIF
    IF voigt THEN IF widget_result.ID EQ peak_eta THEN BEGIN
      WIDGET_CONTROL,peak_eta,GET_VALUE=valstr
      a(peak_number*peakpars+3)=FLOAT(valstr(0))
    ENDIF
    IF rectangle THEN IF widget_result.ID EQ peak_HR THEN BEGIN
      WIDGET_CONTROL,peak_HR,GET_VALUE=valstr
      a(peak_number*peakpars+3+voigt)=FLOAT(valstr(0))
    ENDIF
    IF widget_result.ID EQ fcj_S THEN BEGIN
      WIDGET_CONTROL,fcj_S,GET_VALUE=valstr
      a(nterms+npeaks*peakpars)=FLOAT(valstr(0))
    ENDIF
    IF widget_result.ID EQ fcj_H THEN BEGIN
      WIDGET_CONTROL,fcj_H,GET_VALUE=valstr
      a(nterms+npeaks*peakpars+1)=FLOAT(valstr(0))
    ENDIF
    IF widget_result.ID EQ fcj_L THEN BEGIN
      WIDGET_CONTROL,fcj_L,GET_VALUE=valstr
      a(nterms+npeaks*peakpars+2)=FLOAT(valstr(0))
    ENDIF
    PLOT,x,w(*,start),TITLE='Click on peaks maximums, release in its slope!',YRANGE=[0,MAX(w)],background=255,color=0,xstyle=1,ystyle=1
    OPLOTERR,x,w(*,start),e(*,start),0
    CALL_PROCEDURE,func,x,a,g 
    OPLOT,x,g,COLOR=2,LINE=0
    OPLOT,x,w(*,start)-g+MIN(w)/2.,color=3
    OPLOT,x,g*0.0+MIN(w)/2.,color=0
    IF N_ELEMENTS(e) EQ N_ELEMENTS(w) THEN BEGIN
          OPLOT,x,w(*,start)-g+e(*,start)+MIN(w)/2.,color=2,LINE=0
          OPLOT,x,w(*,start)-g-e(*,start)+MIN(w)/2.,color=2,LINE=0
    ENDIF
    tmp=npeaks
    npeaks=1
    FOR j=0,npeaks-1 DO BEGIN
        b=[a((j*peakpars)+INDGEN(peakpars)),a(npeaks*peakpars+INDGEN(nterms+furtherterms))]
        CALL_PROCEDURE,func,x,b,g
        OPLOT,x,g,COLOR=j+3,LINE=1
    ENDFOR
    npeaks=tmp
    IF npeaks THEN BEGIN
      WIDGET_CONTROL,peak_no,SET_VALUE='Pk. no.'+STRCOMPRESS(peak_number,/RE)+':'
      WIDGET_CONTROL,npeaks_label,SET_VALUE=STRCOMPRESS(npeaks,/RE)+' peaks'
      WIDGET_CONTROL,peak_ampli,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars),/RE)
      WIDGET_CONTROL,peak_pos,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+1),/RE)
      WIDGET_CONTROL,peak_wid,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+2),/RE)
      IF voigt THEN WIDGET_CONTROL,peak_eta,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+3),/RE)
      IF rectangle THEN WIDGET_CONTROL,peak_HR,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+voigt+3),/RE)
      IF furtherterms GE 3 THEN BEGIN
        WIDGET_CONTROL,fcj_S,SET_VALUE=STRCOMPRESS(a(nterms+npeaks*peakpars),/RE)
        WIDGET_CONTROL,fcj_H,SET_VALUE=STRCOMPRESS(a(nterms+npeaks*peakpars+1),/RE)
        WIDGET_CONTROL,fcj_L,SET_VALUE=STRCOMPRESS(a(nterms+npeaks*peakpars+2),/RE)
      ENDIF
    ENDIF
  ENDREP UNTIL widget_result.ID EQ next_button
  WIDGET_CONTROL,title_field,SET_VALUE='Click on Stop button when the fit of the inital guess becomes satisfying!'
  WIDGET_CONTROL,next_button,SET_VALUE='Stop fit of initial guess and start sequential fitting!'
ENDIF
f = 0.0*w  ; resulting (fitted) values
IF NOT KEYWORD_SET(UVW) THEN UVW=0
; ############### Automatic Peak search - and supplementary search for incomplete input (except inputfile given) ########################################
IF a(npeaks*peakpars) EQ 0 THEN a(npeaks*peakpars)=MIN(w(*,start)) ; Set at least constant background to minimum counting rate
PRINT,'Constant background term: ',a(npeaks*peakpars),',',nterms,' background terms'
IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='Constant background term: '+STRING(a(npeaks*peakpars))+','+STRING(nterms)+' background terms'
IF NOT KEYWORD_SET(inputfile) THEN FOR j=0,npeaks-1 DO BEGIN
  call_procedure,func,x,a,g 
  pos=a(j*peakpars+1)
  IF voigt EQ 1 THEN IF a(j*peakpars+3) LE 0 THEN a(j*peakpars+3)=eta  ; no eta given for this peak
  IF a(j*peakpars+0) LE 0 THEN BEGIN    ; no peak amplitude given for this peak
    IF a(j*peakpars+1) EQ 0 THEN BEGIN  ; no peak position given neither
      a(j*peakpars+0) = MAX(w(*,start)-g,i)
      a(j*peakpars+1) = x(i)
      pos=a(j*peakpars+1)
    ENDIF ELSE BEGIN             ; peak position already given ...
      pos=MIN(ABS(x-a(j*peakpars+1)),i)
      a(j*peakpars+0)=(w(i,start)-g(i))>0 ; only positive peaks taken into account
    ENDELSE
  ENDIF
  IF a(j*peakpars+2) LE 0 THEN BEGIN    ; no peak width given for this peak
    PRINT,'Peakwidth to be determinated for peak no.',j
    IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='Peakwidth to be determinated for peak no.'+STRING(j)
    IF N_ELEMENTS(UVW) LT 3 THEN BEGIN
      i=WHERE(x GT pos AND w(*,start)-g LE a(j*peakpars+0)/2.0,count)
      IF count EQ 0 THEN pos2=MAX(x) ELSE pos2=x(i(0))
      i=WHERE(x LT pos AND w(*,start)-g LE a(j*peakpars+0)/2.0,count)
      IF count EQ 0 THEN pos1=MIN(x) ELSE pos1=x(i(count-1))
      a(j*peakpars+2)=(pos2-pos1)/sig2fwhm
    ENDIF ELSE a(j*peakpars+2)=FWHM(UVW,a(j*peakpars+1))
    IF rectangle THEN IF a((j+1)*peakpars-1) LE 0 THEN a((j+1)*peakpars-1)=a(j*peakpars+2)/10.
  ENDIF
  PRINT,'Peak', j,' (ampl., pos., sig.=FWHM/(2sqrt(2ln2)):',a(j*peakpars+INDGEN(peakpars))
  IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='Peak'+STRING(j)+' (ampl., pos., sig.=FWHM/(2sqrt(2ln2)):'+STRING(a(j*peakpars+[0,1,2]))
ENDFOR  
PRINT,STRCOMPRESS(npeaks),' peaks, Voigt:',STRCOMPRESS(voigt),',',STRCOMPRESS(peakpars),' parameters/peak,',STRCOMPRESS(nterms),' background polynom terms,',STRCOMPRESS(furtherterms),' further terms (Finger-Cox-Jephcoat)'
PRINT,a(npeaks*peakpars+INDGEN(nterms+furtherterms))
IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE=STRING(a(npeaks*peakpars+INDGEN(nterms+furtherterms)))
n=N_ELEMENTS(w(0,*))
m=N_ELEMENTS(w(*,start))
H=FLTARR(n)
S=H
L=H
sig_H=H
sig_S=H
sig_L=H
amplitude=FLTARR(n,npeaks)
intensity=amplitude
width=amplitude
HR=amplitude
eta=amplitude
position=amplitude
bg=FLTARR(n,nterms)
sig_amplitude=amplitude
sig_intensity=amplitude
sig_width=amplitude
sig_position=amplitude
sig_eta=amplitude
sig_HR=amplitude
sig_bg=bg
IF KEYWORD_SET(printing) THEN PRINT,'Step Pk Intensity   Peak-Heigth    Position 2sqrt(2ln2)*Sigma (Eta)  (HR)  Residu.' 
IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='Step Pk Peak-Heigth    Position 2sqrt(2ln2)*Sigma Residu.' 
sigmaa=a*0.0
IF KEYWORD_SET(file) THEN OPENW,output,file,/get_lun
IF KEYWORD_SET(plot) THEN BEGIN
  PLOT,x,w(*,start),TITLE='Guess for first diagram',YRANGE=[0,MAX(w)],background=255,color=0
  OPLOTERR,x,w(*,start),e(*,start),0
ENDIF
IF N_ELEMENTS(ref) NE N_ELEMENTS(a) THEN ref=a*0.0+damping
nbgterms=nterms
; ################# Writing out the inital guess ###########################
IF KEYWORD_SET(outputfile) THEN BEGIN
  OPENW,unit,outputfile,/GET_LUN
  PRINTF,unit,npeaks,nbgterms,voigt,furtherterms
  FOR i=0,npeaks-1 DO BEGIN
    PRINTF,unit,a(i*peakpars+INDGEN(peakpars))
    PRINTF,unit,ref(i*peakpars+INDGEN(peakpars))
  ENDFOR
  IF nterms GT 0 THEN BEGIN
    PRINTF,unit,  a(i*peakpars+INDGEN(nterms))
    PRINTF,unit,ref(i*peakpars+INDGEN(nterms))
  ENDIF
  IF furtherterms GT 0 THEN BEGIN
    PRINTF,unit,  a(i*peakpars+nterms+INDGEN(furtherterms))
    PRINTF,unit,ref(i*peakpars+nterms+INDGEN(furtherterms))
  ENDIF
  FREE_LUN,unit
ENDIF
guess=a
IF KEYWORD_SET(plot) THEN BEGIN
	call_procedure,func,x,a,g
        OPLOT,x,g,COLOR=1
ENDIF
; ############### NOW THE GUESSING IS FINISHED, FITTING CAN START! ###############################
IF KEYWORD_SET(click) THEN initial_fit_finished=0 ELSE initial_fit_finished=1
IF NOT KEYWORD_SET(initialiterations) THEN initialiterations=40
loops=initialiterations 
fitflag=1
refbak=ref
IF KEYWORD_SET(nowait) THEN nowait=0 ELSE nowait=1
PRINT,peakpars,' parameters per peak'
FOR i=LONG(start),LONG(last),LONG(step) DO BEGIN
  weight=REPLICATE(1.,m)
  IF N_ELEMENTS(e) EQ N_ELEMENTS(w) THEN weight=e(*,i)
  ITER=0
  CHI2=0
  ref=refbak
  ; ****** some peaksearching ... try out some future new peak appearance/disappearing management ******
  ;smoothed=SMOOTH(w(*,i),3)
  smoothed=w(*,i) ; finally ... not smoothed at all ...
  index=INDGEN(N_ELEMENTS(smoothed))
  denominator=x((index+1)<(N_ELEMENTS(smoothed)-1))-x((index-1)>0)
  stepx=TOTAL(denominator(index(1:N_ELEMENTS(smoothed)-2)))/(N_ELEMENTS(smoothed)-2)
  denominator=denominator/stepx
  differentiated=(smoothed((index+1)<(N_ELEMENTS(smoothed)-1))-smoothed((index-1)>0))/denominator
  differentiated(0)=0
  differentiated(N_ELEMENTS(smoothed)-1)=0
  error=(e((index+1)<(N_ELEMENTS(smoothed)-1),i)+e((index-1)>0,i))/denominator
  twoprime=differentiated((index+1)<(N_ELEMENTS(smoothed)-1))-differentiated((index-1)>0)/denominator
  twoprime(0)=0
  twoprime(N_ELEMENTS(smoothed)-1)=0
  newx=FINDGEN(N_ELEMENTS(x)*10)*(MAX(x)-MIN(x))/N_ELEMENTS(x)/10.+MIN(x)
  ;foundpeaks=WHERE(differentiated LE error*3. AND differentiated((index-1)>0) GT error*3.,numberofoundpeaks)
  ;IF numberofoundpeaks GE 1 THEN PRINT,x(foundpeaks)
  twoprime      =INTERPOL(twoprime      ,x,newx)
  differentiated=INTERPOL(differentiated,x,newx)
  smoothed      =INTERPOL(smoothed      ,x,newx)
  error         =INTERPOL(error         ,x,newx)
  foundpeaks=WHERE(differentiated LE 0 AND differentiated((INDGEN(N_ELEMENTS(newx))-1)>0) GT 0 AND differentiated((INDGEN(N_ELEMENTS(newx))-1)>0)-differentiated GT error+error((INDGEN(N_ELEMENTS(newx))-1)>0),numberofoundpeaks)
  ;steep=(differentiated((foundpeaks-1)>0)-differentiated(foundpeaks))/error(foundpeaks)
  ;foundpeaks=foundpeaks(REVERSE(SORT(steep)))
  IF NOT KEYWORD_SET(click) AND KEYWORD_SET(plot) AND NOT KEYWORD_SET(now) THEN BEGIN
      WSET,1
      PLOT,newx,smoothed,YR=[MIN([smoothed,differentiated,twoprime]),MAX([smoothed,differentiated,twoprime])],BACK=255
      OPLOTERR,newx,smoothed,error,0
      OPLOT,newx,differentiated,COLOR=1
      ;OPLOTERR,x,differentiated,error,0
      OPLOT,newx,twoprime,COLOR=2
      WSET,0
  ENDIF
  peaks2fit=0
  FOR foundpeak=0,numberofoundpeaks-1 DO BEGIN
    ;part=differentiated(foundpeaks(foundpeak))/(differentiated(foundpeaks(foundpeak))-differentiated(foundpeaks(foundpeak)-1))
    ;maxx=
    idx=INDGEN(foundpeaks(foundpeak))
    left=MAX(WHERE(twoprime(idx) GE 0))
    leftpart=twoprime(left)/(twoprime(left)-twoprime(left+1))
    ;left=left-1
    leftx=leftpart*    newx(left)+(1-leftpart)*    newx(left+1)
    leftw=leftpart*smoothed(left)+(1-leftpart)*smoothed(left+1);-MIN(w(*,i))
    idx=INDGEN(N_ELEMENTS(smoothed)-foundpeaks(foundpeak))+foundpeaks(foundpeak)
    right=MIN(WHERE(twoprime(idx) GE 0))+foundpeaks(foundpeak)
    rightpart=twoprime(right)/(twoprime(right)-twoprime(right-1))
    ;right=right-1
    rightx=rightpart*    newx(right)+(1-rightpart)*    newx(right-1)
    rightw=rightpart*smoothed(right)+(1-rightpart)*smoothed(right+1);-MIN(w(*,i))
    IF KEYWORD_SET(search) THEN BEGIN
      peakwidth=(rightx-leftx)*sig2fwhm/2.
      ;PRINT,peakwidth
      IF peakwidth GE minwidth THEN BEGIN
        ;peakamplitude= MAX(smoothed(left:right),maxidx)-(POLY(newx(left+maxidx),a(npeaks*peakpars+INDGEN(nterms)))>MIN(w(*,i)))
        peakamplitude=(MAX(smoothed(left:right),maxidx)-(leftw+rightw)/2.0)/(1.0-EXP(-0.5))
        PRINT,'**',peaks2fit,peakamplitude,newx(left+maxidx),rightx-leftx
        ;PRINT,'* ',(MAX(smoothed(left:right))-(leftw+rightw)/2.0)/(1.0-EXP(-0.5))
        ;PRINT,'* ',leftw,rightw
        IF NOT KEYWORD_SET(click) AND KEYWORD_SET(plot) AND NOT KEYWORD_SET(nowindows) THEN BEGIN
          PLOTS,[            leftx,            leftx],[0,MAX([smoothed(left       )])],COLOR=3
          PLOTS,[newx(left+maxidx),newx(left+maxidx)],[0,MAX([smoothed(left+maxidx)])],COLOR=3
          PLOTS,[           rightx,           rightx],[0,MAX([smoothed(right      )])],COLOR=3
        ENDIF
        IF peaks2fit LT npeaks THEN BEGIN
            IF peaks2fit EQ 0 THEN BEGIN
              b=[peakamplitude,newx(left+maxidx),peakwidth/sig2fwhm]
            ENDIF ELSE BEGIN
              b=[b,peakamplitude,newx(left+maxidx),peakwidth/sig2fwhm]
            ENDELSE
            IF voigt     THEN b=[b,0.2]
            IF rectangle THEN b=[b,peakwidth/10.0/sig2fwhm]
            peaks2fit=peaks2fit+1
        ENDIF
      ENDIF
    ENDIF
  ENDFOR
  IF KEYWORD_SET(search) THEN BEGIN
    IF npeaks GT peaks2fit THEN BEGIN
      IF N_ELEMENTS(b) GT 1 THEN b=[b,FLTARR((npeaks-peaks2fit)*peakpars)]    
    ENDIF
    IF N_ELEMENTS(b) GT 1 THEN b=[b,a(npeaks*peakpars+INDGEN(nterms+furtherterms))] ELSE  b=a(npeaks*peakpars+INDGEN(nterms+furtherterms))
    refflag=INTARR(N_ELEMENTS(A))
    IF peaks2fit GT 0 THEN refflag(0:peakpars*peaks2fit-1)=1
    refflag(peakpars*npeaks:N_ELEMENTS(a)-1)=1
    a=b
  ENDIF
  ; ************************* Least Square Iterations ***************************************
  FOR j=0,loops-1 DO BEGIN
    aa=a
    IF bragg OR rectangle OR (STRPOS(func,'fcj') GE 0) THEN noderivative=1 ; no derivatives yet (correctly) implemented for these cases!
    ; +++++++++++++ First, we want to exclude non-existing peaks from any refinment (except the first ...) +++++++++++++++++++++++++
    IF NOT KEYWORD_SET(search) THEN BEGIN
      b=a
      peaks2fit=npeaks
      refflag=INTARR(N_ELEMENTS(A))+1
      FOR peak=0,npeaks-1 DO BEGIN
        IF NOT (i EQ start AND j EQ 0) THEN BEGIN
          ;PLOT,x,w(*,i),COLOR=0,back=255
          b(peak*peakpars)=0
          IF a(peak*peakpars) AND j NE 0 NE 0 THEN CALL_PROCEDURE,func,x,b,g0 ;ELSE g0=f(*,i-step); otherwise, g0 is already known!
          ;oPLOT,x,g0,COLOR=2,LINE=1
          IF a(peak*peakpars) GT 0 THEN BEGIN
            ; Don`t make the further stuff, if already everything looks fine for this peak as it is
            b(peak*peakpars)=a(peak*peakpars)
            CALL_PROCEDURE,func,x,b,g1
            chisqr0=TOTAL((g0-w(*,i))^2)/(N_ELEMENTS(w(*,i))-N_ELEMENTS(a)+peakpars)
            chisqr1=TOTAL((g1-w(*,i))^2)/(N_ELEMENTS(w(*,i))-N_ELEMENTS(a))
            IF chisqr0 LE 1.1*chisqr1 OR b(peak*peakpars) LE 0 THEN b(peak*peakpars)=0
            IF b(peak*peakpars) EQ 0 AND KEYWORD_SET(printing) AND j EQ 0 THEN PRINT,'Peak',peak,' to be checked, : amplitude=', b(peak*peakpars),', chi2:',chisqr1,'+10%>',chisqr0
          ENDIF 
          IF b(peak*peakpars) LE 0 THEN BEGIN ; no or only slight improvement to fit by using this peak - or peak equal zero from beginning
            bid=MIN(ABS(x-a(peak*peakpars+1)),xindex)
            b(peak*peakpars+2)=b(peak*peakpars+2)<maxwidth
            peakintegral=WHERE(x LE (a(peak*peakpars+1)+(b(peak*peakpars+2)<maxsearchwidth)) AND x GE (a(peak*peakpars+1)-(b(peak*peakpars+2)<maxsearchwidth)),peakcells)
            IF peakcells LE 4 THEN BEGIN
              peakintegral=INDGEN(5<N_ELEMENTS(x))+(xindex-2)>0 
              peakcells=5
            ENDIF
            IF peakcells GT 0 THEN BEGIN
              b(peak*peakpars) =  Min(w(peakintegral,i)-g0(peakintegral),peakposition)
              ;IF KEYWORD_SET(printing) AND j EQ 0 THEN PRINT,'Peak',peak,': Minimum of', b(peak*peakpars),'+/-',e(peakintegral(peakposition),i),' at',x(peakintegral(peakposition))
              b(peak*peakpars) = MAX(w(peakintegral,i)-g0(peakintegral),peakposition)
              ;IF KEYWORD_SET(printing) AND j EQ 0 THEN PRINT,'Peak',peak,': Maximum of', b(peak*peakpars),'+/-',e(peakintegral(peakposition),i),' at',x(peakintegral(peakposition))
              IF peakposition LT 0 OR peakposition GT (peakcells-1) THEN BEGIN
                IF a(peak*peakpars) EQ 0 AND KEYWORD_SET(printing) THEN PRINT,'0-Peak',peak,' still not refined, as maximum lies on the border of search range at',x(peakintegral(peakposition))
                IF a(peak*peakpars) GT 0 AND KEYWORD_SET(printing) THEN PRINT,'Old Pk',peak,' no longer refined, as maximum lies on the border of search range at',x(peakintegral(peakposition))
                b(peak*peakpars) =0 ; maximum lies on the border of search range -> don`t consider this peak ...
              ENDIF ELSE BEGIN
                FOR neighbour=0,npeaks-1 DO BEGIN
                  bid=MIN(ABS(x-b(neighbour*peakpars+1)),neighbour_xindex)
                  IF neighbour NE peak AND ABS(neighbour_xindex-peakintegral(peakposition)) LE 1 THEN BEGIN
                  ;IF a(neighbour*peakpars+1) LE x(peakintegral(peakposition))+0.1 AND a(neighbour*peakpars+1) GE x(peakintegral(peakposition))-0.1 THEN BEGIN
                    IF a(neighbour*peakpars+2)-a(peak*peakpars+2)*1.5 AND a(neighbour*peakpars+2) GE a(peak*peakpars+2)/1.5 THEN BEGIN
                      b(peak*peakpars)=0
                      IF a(peak*peakpars) EQ 0 AND KEYWORD_SET(printing) THEN PRINT,'0-Peak',peak,' still not refined, as overlapping with neighbour peak',neighbour 
                      IF a(peak*peakpars) GT 0 AND KEYWORD_SET(printing) THEN PRINT,'Old Pk',peak,' no longer refined, as overlapping with neighbour peak',neighbour 
                      neighbour=npeaks
                    ENDIF
                  ENDIF
                ENDFOR
                IF b(peak*peakpars) GT 0 THEN b(peak*peakpars+1) = x(peakintegral(peakposition))
              ENDELSE
              IF b(peak*peakpars) LE 3.*e(peakintegral(peakposition)) THEN BEGIN
                IF a(peak*peakpars) EQ 0 AND b(peak*peakpars) NE 0 AND KEYWORD_SET(printing) THEN PRINT,'0-Peak',peak,' still not refined: amplitude',b(peak*peakpars),'<3 error bars (', e(peakintegral(peakposition)),') at',x(peakintegral(peakposition)) 
                IF a(peak*peakpars) GT 0 AND b(peak*peakpars) NE 0 AND KEYWORD_SET(printing) THEN PRINT,'Old Pk',peak,' no longer refined: amplitude',b(peak*peakpars),'<3 error bars (', e(peakintegral(peakposition)),') at',x(peakintegral(peakposition)) 
                b(peak*peakpars) =0
              ENDIF
            ENDIF ELSE BEGIN
              IF KEYWORD_SET(printing) THEN PRINT,'Peak',peak,' covers no detector cells!'
              b(peak*peakpars)=0;a(peak*peakpars)
            ENDELSE
            IF b(peak*peakpars) LE 0 THEN BEGIN
                ;IF KEYWORD_SET(printing) THEN PRINT,'Peak ',peak,' will not be refined: amplitude=', b(peak*peakpars)
                b(peak*peakpars)=0
                aa(peak*peakpars)=0
            ENDIF ELSE BEGIN
                CALL_PROCEDURE,func,x,b,g1
                ;oPLOT,x,g1,COLOR=1,LINE=2
                chisqr1=TOTAL((g1-w(*,i))^2)/(N_ELEMENTS(w(*,i))-N_ELEMENTS(a))
                IF chisqr0 LE chisqr1 THEN BEGIN
                  IF KEYWORD_SET(printing) THEN PRINT,'Peak ',peak,' will not be refined: amplitude=', b(peak*peakpars),', chi2:',chisqr1,'>',chisqr0
                  b(peak*peakpars)=0
                  aa(peak*peakpars)=0
                ENDIF ELSE IF chisqr0 LE 1.1*chisqr1 THEN BEGIN
                  IF KEYWORD_SET(printing) THEN PRINT,'Peak ',peak,' will not be refined: amplitude=', b(peak*peakpars),', chi2:',chisqr1,'+10%>',chisqr0
                  b(peak*peakpars)=0
                  aa(peak*peakpars)=0
                 ENDIF 
            ENDELSE
            IF KEYWORD_SET(printing) AND b(peak*peakpars) GT 0 THEN IF (a(peak*peakpars+1) LT x((peakintegral(peakposition)-1)>0) OR a(peak*peakpars+1) GT x((peakintegral(peakposition)+1)<(N_ELEMENTS(x)-1)))  THEN BEGIN
                PRINT,'Peak ',peak,' shifts from', a(peak*peakpars+1),' to', b(peak*peakpars+1)
            ENDIF
          ENDIF
        ENDIF ELSE b(peak*peakpars)=a(peak*peakpars)
        IF b(peak*peakpars) EQ 0 THEN BEGIN
            refflag(peak*peakpars+INDGEN(peakpars))=0
            peaks2fit=peaks2fit-1
        ENDIF ELSE BEGIN
          IF a(peak*peakpars) EQ 0 THEN BEGIN
            refflag(peak*peakpars+INDGEN(peakpars))=MAX(refflag(peak*peakpars+INDGEN(peakpars)))+1
            IF KEYWORD_SET(printing) THEN PRINT,'Former 0-peak ',peak,' now in, with amplitude=', b(peak*peakpars),'+/-',e(peakintegral(peakposition)),', and position=',b(peak*peakpars+1)
          ENDIF
        ENDELSE
      ENDFOR
      IF KEYWORD_SET(printing) THEN PRINT,peaks2fit,' peaks remaining for fit'
    ENDIF ELSE BEGIN ; keyword 'search' set
      IF KEYWORD_SET(printing) THEN PRINT,peaks2fit,' peaks initially found for fit'
    ENDELSE
    refindex=WHERE(refflag GE 1)
    a=b(refindex)
    sigmaa=1;FLTARR(N_ELEMENTS(a))
    tmp=npeaks
    npeaks=peaks2fit
    fitflag=0
    ; +++++++++++++++++ Now, do the least squares refinment only on peaks to be refined! ++++++++++++++++++++++++++++++++++
    IF KEYWORD_SET(search) AND STRPOS(func,'fcj') GE 0 THEN BEGIN
      c=a(0:npeaks*peakpars+nterms-1)
      IF rectangle OR bragg THEN noder=1 ELSE noder=0
      f(*,i) = lsfit(x,w(*,i),weight,c,sigmaa,function_name='gauss_poly',TOL=TOL,ITMAX=ITMAX,ITER=ITER,CHI2=CHI2,noderivative=noder,PLOT=(i EQ start),print=(i EQ start),damp=ref(refindex))
      a(0:npeaks*peakpars+nterms-1)=c
    ENDIF
    f(*,i) = lsfit(x,w(*,i),weight,a,sigmaa,function_name=func,TOL=TOL,ITMAX=ITMAX,ITER=ITER,CHI2=CHI2,noderivative=noderivative,PLOT=(i EQ start),print=(i EQ start),damp=ref(refindex))
    ; ****** some peaksearching ... try out some future (?) new peak appearance/disappearing management ******
    smoothed=SMOOTH(w(*,i)-f(*,i),7)
    ;smoothed=w(*,i) ; finally ... not smoothed at all ...
    index=INDGEN(N_ELEMENTS(smoothed))
    denominator=x((index+1)<(N_ELEMENTS(smoothed)-1))-x((index-1)>0)
    stepx=TOTAL(denominator(index(1:N_ELEMENTS(smoothed)-2)))/(N_ELEMENTS(smoothed)-2)
    denominator=denominator/stepx
    differentiated=(smoothed((index+1)<(N_ELEMENTS(smoothed)-1))-smoothed((index-1)>0))/denominator
    differentiated(0)=0
    differentiated(N_ELEMENTS(smoothed)-1)=0
    error=(e((index+1)<(N_ELEMENTS(smoothed)-1),i)+e((index-1)>0,i))/denominator
    twoprime=differentiated((index+1)<(N_ELEMENTS(smoothed)-1))-differentiated((index-1)>0)/denominator
    twoprime(0)=0
    twoprime(N_ELEMENTS(smoothed)-1)=0
    newx=FINDGEN(N_ELEMENTS(x)*10)*(MAX(x)-MIN(x))/N_ELEMENTS(x)/10.+MIN(x)
    ;foundpeaks=WHERE(differentiated LE error*3. AND differentiated((index-1)>0) GT error*3.,numberofoundpeaks)
    ;IF numberofoundpeaks GE 1 THEN PRINT,x(foundpeaks)
    twoprime      =INTERPOL(twoprime      ,x,newx)
    differentiated=INTERPOL(differentiated,x,newx)
    smoothed      =INTERPOL(smoothed      ,x,newx)
    error         =INTERPOL(error         ,x,newx)
    ;foundpeaks=WHERE(differentiated LE error*3. AND differentiated((INDGEN(N_ELEMENTS(newx))-1)>0) GT error*3.,numberofoundpeaks)
    foundpeaks=WHERE(differentiated LE 0 AND differentiated((INDGEN(N_ELEMENTS(newx))-1)>0) GT 0 AND differentiated((INDGEN(N_ELEMENTS(newx))-1)>0)-differentiated GT 0.02*(error+error((INDGEN(N_ELEMENTS(newx))-1)>0)),numberofoundpeaks)
    IF numberofoundpeaks GE 1 THEN PRINT,newx(foundpeaks)
    IF NOT KEYWORD_SET(click) AND KEYWORD_SET(plot) AND NOT KEYWORD_SET(now) THEN BEGIN
        WSET,1
        PLOT,x,w(*,i)-f(*,i),YR=[MIN([w(*,i)-f(*,i),differentiated,twoprime]),MAX([w(*,i)-f(*,i),differentiated,twoprime])],BACK=255
        OPLOTERR,x,w(*,i)-f(*,i),e(*,i),0
        OPLOT,newx,smoothed
        OPLOT,newx,differentiated,COLOR=2
        ;OPLOTERR,x,differentiated,error,0
        OPLOT,newx,twoprime,COLOR=3
        WSET,0
    ENDIF
    IF KEYWORD_SET(search) THEN BEGIN
      IF npeaks GT 0 THEN b=a(0:npeaks*peakpars-1) ELSE b=0
      b_tmp=a(npeaks*peakpars:npeaks*peakpars+nterms+furtherterms-1)
    ENDIF
    FOR foundpeak=0,numberofoundpeaks-1 DO BEGIN
      ;part=differentiated(foundpeaks(foundpeak))/(differentiated(foundpeaks(foundpeak))-differentiated(foundpeaks(foundpeak)-1))
      ;maxx=
      idx=INDGEN(foundpeaks(foundpeak))
      left=MAX(WHERE(twoprime(idx) GE 0))
      leftpart=twoprime(left)/(twoprime(left)-twoprime(left+1))
      ;left=left-1
      leftx=leftpart*    newx(left)+(1-leftpart)*    newx(left+1)
      leftw=leftpart*smoothed(left)+(1-leftpart)*smoothed(left+1)
      idx=INDGEN(N_ELEMENTS(smoothed)-foundpeaks(foundpeak))+foundpeaks(foundpeak)
      right=MIN(WHERE(twoprime(idx) GE 0))+foundpeaks(foundpeak)
      rightpart=twoprime(right)/(twoprime(right)-twoprime(right-1))
      ;right=right-1
      rightx=rightpart*    newx(right)+(1-rightpart)*    newx(right-1)
      rightw=rightpart*smoothed(right)+(1-rightpart)*smoothed(right+1)
      IF KEYWORD_SET(search) THEN BEGIN
        peakwidth=(rightx-leftx)*sig2fwhm/2.
        ;peakamplitude=MAX(smoothed(left:right),maxidx)
        peakamplitude=(MAX(smoothed(left:right),maxidx)-(leftw+rightw)/2.0)/(1.0-EXP(-0.5))
        ;PRINT,peakwidth
        IF peakwidth GE minwidth*2. AND peakamplitude GT 4.*error(maxidx) THEN BEGIN
          ;peakamplitude= MAX(smoothed(left:right),maxidx)-(POLY(newx(left+maxidx),a(npeaks*peakpars+INDGEN(nterms)))>MIN(w(*,i)))
          PRINT,'##',peaks2fit,peakamplitude,newx(left+maxidx),rightx-leftx,error(maxidx)
          PRINT,'# ',(MAX(smoothed(left:right))-(leftw+rightw)/2.0)/(1.0-EXP(-0.5)),leftw,rightw
          IF NOT KEYWORD_SET(click) AND KEYWORD_SET(plot) AND NOT KEYWORD_SET(nowindows) THEN BEGIN
            WSET,1
            PLOTS,[ leftx, leftx],[MIN([twoprime(left),differentiated(left),smoothed(left)]), MAX([twoprime(left),differentiated(left),smoothed(left)])],COLOR=3
            PLOTS,[newx(left+maxidx),newx(left+maxidx)],[MIN([twoprime(left+maxidx),differentiated(left+maxidx),smoothed(left+maxidx)]), MAX([twoprime(left+maxidx),differentiated(left+maxidx),smoothed(left+maxidx)])],COLOR=1
            PLOTS,[rightx,rightx],[MIN([twoprime(right),differentiated(right),smoothed(right)]), MAX([twoprime(right),differentiated(right),smoothed(right)])],COLOR=3
            WSET,0
          ENDIF
          IF peaks2fit LT tmp THEN BEGIN
            IF peaks2fit EQ 0 THEN BEGIN
              b=[peakamplitude,newx(left+maxidx),peakwidth/sig2fwhm,b]
            ENDIF ELSE BEGIN
              b=[b,peakamplitude,newx(left+maxidx),peakwidth/sig2fwhm]
            ENDELSE
            IF voigt     THEN b=[b,0.2]
            IF rectangle THEN b=[b,peakwidth/10.0/sig2fwhm]
            peaks2fit=peaks2fit+1
          ENDIF
        ENDIF
      ENDIF
    ENDFOR
    IF KEYWORD_SET(search) AND peaks2fit GT npeaks THEN BEGIN
          HELP,b,b_tmp
          PRINT,b_tmp
          b=[b,b_tmp]
          refflag(0:peakpars*peaks2fit-1)=1
          refindex=WHERE(refflag GE 1)
          a=b;(refindex)
          sigmaa=1;FLTARR(N_ELEMENTS(a))
          npeaks=peaks2fit
          fitflag=0
          PRINT,npeaks
          HELP,a
          FOR peak=0,npeaks-1 DO PRINT,a(peak*peakpars+INDGEN(peakpars))
          IF nterms GT 0 THEN PRINT,a(npeaks*peakpars+INDGEN(nterms))
          IF furtherterms GT 0 THEN PRINT,a(npeaks*peakpars+nterms+INDGEN(furtherterms))
          CALL_PROCEDURE,func,x,a,g
          IF NOT KEYWORD_SET(click) AND KEYWORD_SET(plot) AND NOT KEYWORD_SET(nowindows) THEN BEGIN
            WSET,1
            OPLOT,x,w(*,i)-MIN(w(*,i)),COLOR=0
            OPLOTERR,x,w(*,i)-MIN(w(*,i)),e(*,i)
            OPLOT,x,g-MIN(w(*,i)),COLOR=1
            WSET,0
          ENDIF
          ;IF KEYWORD_SET(search) AND STRPOS(func,'fcj') GE 0 THEN BEGIN
            ;c=a(0:npeaks*peakpars+nterms-1)
            ;IF rectangle OR bragg THEN noder=1 ELSE noder=0
            ;f(*,i) = lsfit(x,w(*,i),weight,c,sigmaa,function_name='gauss_poly',TOL=TOL,ITMAX=ITMAX,ITER=ITER,CHI2=CHI2,noderivative=noder,PLOT=(i EQ start),print=(i EQ start),damp=ref(refindex))
            ;a(0:npeaks*peakpars+nterms-1)=c
          ;ENDIF
          f(*,i) = lsfit(x,w(*,i),weight,a,sigmaa,function_name=func,TOL=TOL,ITMAX=ITMAX,ITER=ITER,CHI2=CHI2,noderivative=noderivative,PLOT=(i EQ start),print=(i EQ start),damp=ref(refindex))
    ENDIF
    bid=MIN(w(*,i)-f(*,i),minpos)
    PRINT,'The residual shows a minimum of',bid,'+/-',e(minpos,i),' counts at',x(minpos),' degrees'
    bid=MAX(w(*,i)-f(*,i),maxpos)
    PRINT,'The residual shows a maximum of',bid,'+/-',e(maxpos,i),' counts at',x(maxpos),' degrees'
    ;closest_peak=0
    ;closest_zero_peak=0
    ;For peak=1,tmp-1 DO BEGIN
    ;  IF 
    ;ENDFOR
    IF fitflag THEN PRINT,'Least-squares becomes instable!'
    REPEAT BEGIN
      ;fitflag=0
      IF fitflag GE 3 THEN fitflag=fitflag-3
      FOR peak=0,npeaks-1 DO BEGIN
        ; check for NAN sigmas and put the corresponding peaks to their starting values - with zero amplitude - otherwise they shift around!
        bid=MIN(ABS(x-a(peak*peakpars+1)),peak_xindex)
        IF a(peak*peakpars) EQ 0 OR NOT FINITE(a(peak*peakpars)) THEN BEGIN ; lsfit sets the amplitude to zero, if there is a problem!
          ;PRINT,a(peak*peakpars+INDGEN(peakpars))
          ;PRINT,refindex(INDGEN(tmp)*peakpars),peak,peakpars
          IF KEYWORD_SET(printing) THEN PRINT,'Not to be refined peak as it became negative or infinite: ',refindex(peak*peakpars)/peakpars
          fitflag=2 ; will repeat least square after exclusion of this peak!
          a=b(refindex) ; go back to start!
          a(peak*peakpars+INDGEN(peakpars))=aa(refindex(peak*peakpars+INDGEN(peakpars))) ; set back to initial values
          a(peak*peakpars)=0
          peaks2fit=peaks2fit-1
          refflag(refindex(peak*peakpars)+INDGEN(peakpars))=0
          peak=npeaks
        ENDIF ELSE BEGIN
          ;intensity_tmp=a(peak*peakpars)
          ;sig_intensity_tmp=sigmaa(peak*peakpars)
          ;IF KEYWORD_SET(printing) THEN PRINT,'Peak',refindex(peak*peakpars)/peakpars,': amplitude=',intensity_tmp,'+/-',sig_intensity_tmp
          intensity_tmp=a(peak*peakpars)*a(peak*peakpars+2)*SQRT(!PI/ALOG(2.))/2.*sig2fwhm
          sig_intensity_tmp  =(ABS(sigmaa(peak*peakpars)*a(peak*peakpars+2))+ABS(a(peak*peakpars)*sigmaa(peak*peakpars+2)))*SQRT(!PI/ALOG(2.))/2.*sig2fwhm
          ;sig_intensity(i,j)=(ABS(sig_amplitude(i,j)   *width(i,j)       )+ABS(amplitude(i,j)  *sig_width(i,j)         ))*SQRT(!PI/ALOG(2.))/2.
          IF KEYWORD_SET(printing) THEN PRINT,'Peak',refindex(peak*peakpars)/peakpars,': intensity=',intensity_tmp,'+/-',sig_intensity_tmp
          IF intensity_tmp LE 3.*sig_intensity_tmp THEN BEGIN
            IF KEYWORD_SET(printing) THEN PRINT,'Peak',refindex(peak*peakpars)/peakpars,' finally not to be refined as intensity (',intensity_tmp,') < 3*sigma(intensity) (',sig_intensity_tmp,')'
            fitflag=2 ; will repeat least square after exclusion of this peak!
            a=b(refindex) ; go back to start!
            a(peak*peakpars+INDGEN(peakpars))=aa(refindex(peak*peakpars+INDGEN(peakpars))) ; set back to initial values
            a(peak*peakpars)=0
            peaks2fit=peaks2fit-1
            refflag(refindex(peak*peakpars)+INDGEN(peakpars))=0
            peak=npeaks
          ENDIF ELSE FOR neighbour=peak+1,npeaks-1 DO BEGIN ; check for peaks at same position -> they have to be merged!
            bid=MIN(ABS(x-a(neighbour*peakpars+1)),neighbour_xindex)
            IF ABS(neighbour_xindex-peak_xindex) LE 2 THEN BEGIN
            ;IF a(neighbour*peakpars+1) LE a(peak*peakpars+1)+0.01 AND a(neighbour*peakpars+1) GE a(peak*peakpars+1)-0.01 THEN BEGIN
              IF a(neighbour*peakpars+2) LE a(peak*peakpars+2)*1.5 AND a(neighbour*peakpars+2) GE a(peak*peakpars+2)/1.5 THEN BEGIN
                IF KEYWORD_SET(printing) THEN PRINT,'Overlapping peaks to be merged: ',refindex(peak*peakpars)/peakpars,refindex(neighbour*peakpars)/peakpars
                a=b(refindex)
                a(peak*peakpars)=a(peak*peakpars)+a(neighbour*peakpars) ; put all intensity to first peak!
                a(neighbour*peakpars+INDGEN(peakpars))=aa(refindex(neighbour*peakpars+INDGEN(peakpars)))
                a(neighbour*peakpars)=0
                neighbour=npeaks
                refflag(refindex(neighbour*peakpars)+INDGEN(peakpars))=0
                fitflag=2
                peaks2fit=peaks2fit-1
                peak=npeaks
              ENDIF
            ENDIF
          ENDFOR
        ENDELSE
      ENDFOR
      IF fitflag EQ 2 THEN BEGIN
        IF KEYWORD_SET(plot) THEN BEGIN
          PLOT,x,w(*,i),TITLE='Refused fit of diagram no.'+STRCOMPRESS(i)+STRCOMPRESS(y(i)),YRANGE=[0,MAX(w)],background=255,color=0
          OPLOTERR,x,w(*,i),e(*,i),0
          OPLOT,x,f(*,i),color=1
          OPLOT,x,w(*,i)-f(*,i)+MIN(w)/2.,color=3
          OPLOT,x,f(*,i)*0.0+MIN(w)/2.,color=0
          IF N_ELEMENTS(e) EQ N_ELEMENTS(w) THEN BEGIN
            OPLOT,x,w(*,i)-f(*,i)+e(*,i)+MIN(w)/2.,color=2,LINE=0
            OPLOT,x,w(*,i)-f(*,i)-e(*,i)+MIN(w)/2.,color=2,LINE=0
          ENDIF
        ENDIF
        b=aa
        b(refindex)=a
        refindex=WHERE(refflag GE 1)
        a=b(refindex)
        PRINT,'Repeat fitting for',peaks2fit,' remaining peaks'
        npeaks=peaks2fit
        fitflag=0
        sigmaa=1
        f(*,i) = lsfit(x,w(*,i),weight,a,sigmaa,function_name=func,TOL=TOL,ITMAX=ITMAX,ITER=ITER,CHI2=CHI2,noderivative=noderivative,PLOT=(i EQ start OR i EQ (start+step)),print=(i EQ start OR i EQ (start+step)),damp=ref(refindex))
        fitflag=fitflag+3
        smoothed=SMOOTH(w(*,i)-f(*,i),5)
        index=INDGEN(N_ELEMENTS(smoothed))
        differentiated=smoothed-smoothed((index-1)>0)
        foundpeaks=WHERE(differentiated LE 2*e(*,i) AND differentiated((index-1)>0) GT 2*e(index-1,i),numberofoundpeaks)
        IF numberofoundpeaks GE 1 THEN BEGIN
          PRINT,'res.peaksearch (5,2): ',x(foundpeaks)
        ENDIF
        bid=MIN(w(*,i)-f(*,i),minpos)
        PRINT,'The residual shows a minimum of',bid,'+/-',e(minpos,i),' counts at',x(minpos),' degrees'
        bid=MAX(w(*,i)-f(*,i),maxpos)
        PRINT,'The residual shows a maximum of',bid,'+/-',e(maxpos,i),' counts at',x(maxpos),' degrees'
      ENDIF
    ENDREP UNTIL fitflag LE 1
    IF fitflag THEN BEGIN ; check for instable least squares
      PRINT,'Least-squares becomes instable, so reducing concerned damping factors and continue iterative least square runs ...'
      fitflag=0
      j=0
      ref(refindex)=ref(refindex)/2.
      IF MAX(ref(refindex)) LT 0.1 THEN BEGIN
        j=loops
        fitflag=1
        PRINT,'Okay, we shall give up and quit further refinments, as even with damping factors all less 10%, least-squares are still unstable!'
      ENDIF
    ENDIF
    ;IF (NOT KEYWORD_SET(click) OR i NE start) AND NOT KEYWORD_SET(search) THEN BEGIN
    IF (NOT KEYWORD_SET(click)) AND NOT KEYWORD_SET(search) AND N_ELEMENTS(peakintegral) GT 0 THEN BEGIN
      ;IF N_ELEMENTS(peakintegral) LE 0 THEN peakintegral=INDGEN(N_ELEMENTS(x))
      IF b(peak*peakpars) GT 0 THEN b(peak*peakpars+1) = x(peakintegral(peakposition))
    ENDIF
    npeaks=tmp
    b=a
    a=aa
    a(refindex)=b
    b=sigmaa
    sigmaa=FLTARR(N_ELEMENTS(a))
    sigmaa(refindex)=b
    IF Error_status NE 0 THEN BEGIN
      j=loops
      PRINT, 'Error index: ', Error_status
      PRINT, 'Error message:', !ERR_STRING
    ENDIF
    IF KEYWORD_SET(click) THEN BEGIN
      WIDGET_CONTROL,npeaks_label,SET_VALUE=STRCOMPRESS(npeaks,/RE)+' peaks'
      WIDGET_CONTROL,peak_ampli,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars),/RE)
      WIDGET_CONTROL,peak_pos,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+1),/RE)
      WIDGET_CONTROL,peak_wid,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+2),/RE)
      IF voigt THEN WIDGET_CONTROL,peak_eta,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+3),/RE)
      IF rectangle THEN WIDGET_CONTROL,peak_HR,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+voigt+3),/RE)
      IF furtherterms GE 3 THEN BEGIN
        WIDGET_CONTROL,fcj_S,SET_VALUE=STRCOMPRESS(a(nterms+npeaks*peakpars),/RE)
        WIDGET_CONTROL,fcj_H,SET_VALUE=STRCOMPRESS(a(nterms+npeaks*peakpars+1),/RE)
        WIDGET_CONTROL,fcj_L,SET_VALUE=STRCOMPRESS(a(nterms+npeaks*peakpars+2),/RE)
      ENDIF
    ENDIF
    IF j EQ loops THEN BEGIN
      shift=a-aa
      shift=MAX(shift/a,maxshift)
      IF maxshift LE npeaks*peakpars THEN BEGIN
        text=', peak'+STRCOMPRESS(maxshift/peakpars)
        IF (maxshift MOD peakpars) EQ 0 THEN text=text+' (amplitude)'
        IF (maxshift MOD peakpars) EQ 1 THEN text=text+' (position)'
        IF (maxshift MOD peakpars) EQ 2 THEN text=text+' (FWHM)'
        IF voigt THEN IF (maxshift MOD peakpars) EQ 3 THEN text=text+' (eta)'
        IF rectangle THEN IF (maxshift MOD peakpars) EQ (3+voigt) THEN text=text+' (HR)'
      ENDIF ELSE text=', background polynom or further parameter no.'+STRCOMPRESS(maxshift-npeaks*peakpars)
      IF KEYWORD_SET(printing) THEN PRINT,'Max. shift of',100.*shift,'% for parameter',maxshift,text
      IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE='Max. shift of'+STRING(100.*shift)+'% for parameter'+STRING(maxshift)+text
    ENDIF
  ENDFOR
  loops=iterations
  ; ***************************** Graphical output of fitting result for this diagram ********************
  IF KEYWORD_SET(plot) THEN BEGIN
        PLOT,x,w(*,i),TITLE='Fit of diagram no.'+STRCOMPRESS(i)+STRCOMPRESS(y(i)),YRANGE=[0,MAX(w)],background=255,color=0
        OPLOTERR,x,w(*,i),e(*,i),0
        OPLOT,x,f(*,i),color=1
        OPLOT,x,w(*,i)-f(*,i)+MIN(w)/2.,color=3
        OPLOT,x,f(*,i)*0.0+MIN(w)/2.,color=0
        IF N_ELEMENTS(e) EQ N_ELEMENTS(w) THEN BEGIN
          OPLOT,x,w(*,i)-f(*,i)+e(*,i)+MIN(w)/2.,color=2,LINE=0
          OPLOT,x,w(*,i)-f(*,i)-e(*,i)+MIN(w)/2.,color=2,LINE=0
        ENDIF
  ENDIF
  ; ************************ Put the resulting parameters to the corresponding variables ************************
  IF nterms GT 0 THEN BEGIN
    bg(i,*)=a(npeaks*peakpars+INDGEN(nterms))
    sig_bg(i,*)=sigmaa(npeaks*peakpars+INDGEN(nterms))
  ENDIF
  IF furtherterms GE 3 AND STRPOS(func,'fcj') GE 0 THEN BEGIN
    S(i,*)=a(npeaks*peakpars+nterms)
    H(i,*)=a(npeaks*peakpars+nterms+1)
    L(i,*)=a(npeaks*peakpars+nterms+2)
    sig_S(i,*)=sigmaa(npeaks*peakpars+nterms)
    sig_H(i,*)=sigmaa(npeaks*peakpars+nterms+1)
    sig_L(i,*)=sigmaa(npeaks*peakpars+nterms+2)
  ENDIF
  dy=w(*,i)-f(*,i)
  Rp=TOTAL(ABS(dy))/TOTAL(w)
  Rwp=SQRT(TOTAL(e*dy^2)/TOTAL(e*w^2))
  IF KEYWORD_SET(printing) THEN PRINT,FORMAT='(I8,2I18,4G18.6)',i,y(i),ITER,chi2,Rp,Rwp,SQRT(TOTAL(e*e^2)/TOTAL(e*w^2))
  IF KEYWORD_SET(widget) THEN WIDGET_CONTROL,w1,/APPEND,SET_VALUE=STRING(i)+STRING(ITER)+STRING(chi2)+STRING(Rp)+STRING(Rwp)+STRING(SQRT(TOTAL(e*e^2)/TOTAL(e*w^2)))
  IF KEYWORD_SET(file) THEN PRINTF,output,FORMAT='(I8,2I18,4G18.6)',i,y(i),ITER,chi2,Rp,Rwp,SQRT(TOTAL(e*e^2)/TOTAL(e*w^2))
  ; *********************** Plot each individual peak as dotted line and print its parameters *******************************************
  tmp=npeaks
  npeaks=1
  FOR j=0,tmp-1 DO BEGIN
    IF KEYWORD_SET(plot) THEN BEGIN
        b=[a((j*peakpars)+INDGEN(peakpars))]
        IF nterms+furtherterms GT 0 THEN b=[b,a(tmp*peakpars+INDGEN(nterms+furtherterms))]
        CALL_PROCEDURE,func,x,b,g
        OPLOT,x,g,COLOR=j+3,LINE=1
    ENDIF
    a(j*peakpars+2)=ABS(a(j*peakpars+2))
    amplitude(i,j)=a(j*peakpars+0)
    position(i,j) =a(j*peakpars+1)
    width(i,j)     =a(j*peakpars+2)*sig2fwhm
    sig_amplitude(i,j)=sigmaa(j*peakpars+0)
    sig_position(i,j) =sigmaa(j*peakpars+1)
    sig_width(i,j)     =sigmaa(j*peakpars+2)*sig2fwhm
    ; ############################# Calculate intensity for amplitude and sigma ########################################
    intensity(i,j)=amplitude(i,j)*width(i,j)*SQRT(!PI/ALOG(2.))/2.
    sig_intensity(i,j)=(ABS(sig_amplitude(i,j)*width(i,j))+ABS(amplitude(i,j)*sig_width(i,j)))*SQRT(!PI/ALOG(2.))/2.
    IF voigt EQ 1 THEN BEGIN
      sin_eta=SIN(!PI/2.*a(3+peakpars*j))
      true_eta=sin_eta^2
      eta(i,j)=true_eta
      sig_eta(i,j)=sigmaa(j*peakpars+3)*sin_eta*!PI*COS(!PI/2.*a(3+peakpars*j))
    ENDIF
    IF rectangle THEN BEGIN
      HR(i,j)=a(j*peakpars+voigt+3)
      sig_HR(i,j)=sigmaa(j*peakpars+voigt+3)
    ENDIF
  ENDFOR
  npeaks=tmp
  IF KEYWORD_SET(click) THEN BEGIN
    IF voigt THEN widget_result=WIDGET_EVENT([next_button,undo_button,up_peak,down_peak,peak_ampli,peak_pos,peak_wid,peak_eta,peak_HR,fcj_S,fcj_H,fcj_L,bragg_button],NOWAIT=nowait) ELSE BEGIN
      widget_result=WIDGET_EVENT([next_button,undo_button,up_peak,down_peak,peak_ampli,peak_pos,peak_wid,peak_HR,fcj_S,fcj_H,fcj_L,bragg_button],NOWAIT=nowait) 
    ENDELSE
    IF widget_result.ID EQ bragg_button THEN BEGIN
      IF bragg THEN bragg=0 ELSE bragg=1
      PRINT,'Bragg Convolution:',bragg
    ENDIF
    IF widget_result.ID EQ up_peak THEN BEGIN
      peak_number=(peak_number+1)<(npeaks-1)
    ENDIF
    IF widget_result.ID EQ down_peak THEN BEGIN
      peak_number=(peak_number-1)>0
    ENDIF
    IF widget_result.ID EQ peak_ampli THEN BEGIN
      WIDGET_CONTROL,peak_ampli,GET_VALUE=valstr
      a(peak_number*peakpars)=FLOAT(valstr(0))
    ENDIF
    IF widget_result.ID EQ peak_pos THEN BEGIN
      WIDGET_CONTROL,peak_pos,GET_VALUE=valstr
      a(peak_number*peakpars+1)=FLOAT(valstr(0))
    ENDIF
    IF widget_result.ID EQ peak_wid THEN BEGIN
      WIDGET_CONTROL,peak_wid,GET_VALUE=valstr
      a(peak_number*peakpars+2)=FLOAT(valstr(0))
    ENDIF
    IF voigt THEN IF widget_result.ID EQ peak_eta THEN BEGIN
      WIDGET_CONTROL,peak_eta,GET_VALUE=valstr
      a(peak_number*peakpars+3)=FLOAT(valstr(0))
    ENDIF
    IF widget_result.ID EQ undo_button THEN BEGIN
      IF nowait EQ 1 THEN BEGIN
        nowait=0
        WIDGET_CONTROL,undo_button,SET_VALUE='Continue'
      ENDIF ELSE BEGIN
        nowait=1
        WIDGET_CONTROL,undo_button,SET_VALUE='Pause'
      ENDELSE
    ENDIF
    WIDGET_CONTROL,peak_no,SET_VALUE='Pk. no.'+STRCOMPRESS(peak_number,/RE)+':'
    WIDGET_CONTROL,npeaks_label,SET_VALUE=STRCOMPRESS(npeaks,/RE)+' peaks'
    WIDGET_CONTROL,peak_ampli,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars),/RE)
    WIDGET_CONTROL,peak_pos,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+1),/RE)
    WIDGET_CONTROL,peak_wid,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+2),/RE)
    IF rectangle THEN WIDGET_CONTROL,peak_HR,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+voigt+3),/RE)
    IF furtherterms GE 3 THEN BEGIN
      WIDGET_CONTROL,fcj_S,SET_VALUE=STRCOMPRESS(a(nterms+npeaks*peakpars),/RE)
      WIDGET_CONTROL,fcj_H,SET_VALUE=STRCOMPRESS(a(nterms+npeaks*peakpars+1),/RE)
      WIDGET_CONTROL,fcj_L,SET_VALUE=STRCOMPRESS(a(nterms+npeaks*peakpars+2),/RE)
    ENDIF
    IF voigt THEN WIDGET_CONTROL,peak_eta,SET_VALUE=STRCOMPRESS(a(peak_number*peakpars+3),/RE)
    IF i EQ start AND NOT initial_fit_finished THEN BEGIN
      IF widget_result.ID NE next_button THEN BEGIN
        i=start
        initial_fit_finished=0
      ENDIF ELSE BEGIN
        initial_fit_finished=1
        WIDGET_CONTROL,title_field,SET_VALUE='Click on Stop button when the fit of the inital guess becomes satisfying!'
        WIDGET_CONTROL,next_button,SET_VALUE='Stop sequential fitting!'
        WIDGET_CONTROL,undo_button,SET_VALUE='Pause'
        nowait=1
      ENDELSE
    ENDIF ELSE BEGIN
      IF widget_result.ID EQ next_button THEN BEGIN
        i=last
      ENDIF
    ENDELSE
  ENDIF
  ; ******************* Print the peak parameters **********************
  FOR j=0,npeaks-1 DO BEGIN
      IF KEYWORD_SET(printing) THEN BEGIN
        IF voigt EQ 0 THEN BEGIN
          IF rectangle THEN BEGIN
            PRINT,FORMAT='(4X,I4,8G18.6)',j,intensity(i,j),amplitude(i,j),position(i,j),width(i,j),HR(i,j),sig_intensity(i,j),sig_amplitude(i,j),sig_position(i,j),sig_width(i,j),sig_HR(i,j)
          ENDIF ELSE BEGIN
            PRINT,FORMAT='(4X,I4,8G18.6)',j,intensity(i,j),amplitude(i,j),position(i,j),width(i,j),sig_intensity(i,j),sig_amplitude(i,j),sig_position(i,j),sig_width(i,j)
          ENDELSE
        ENDIF ELSE BEGIN
          IF rectangle THEN BEGIN
            PRINT,FORMAT='(4X,I4,10G18.6)',j,intensity(i,j),amplitude(i,j),position(i,j),width(i,j),eta(i,j),HR(i,j),sig_intensity(i,j),sig_amplitude(i,j),sig_position(i,j),sig_width(i,j),sig_eta(i,j),sig_HR(i,j)
          ENDIF ELSE BEGIN
            PRINT,FORMAT='(4X,I4,10G18.6)',j,intensity(i,j),amplitude(i,j),position(i,j),width(i,j),eta(i,j),sig_intensity(i,j),sig_amplitude(i,j),sig_position(i,j),sig_width(i,j),sig_eta(i,j)
          ENDELSE
        ENDELSE
      ENDIF
      IF initial_fit_finished EQ 1 AND KEYWORD_SET(file) THEN BEGIN
        IF voigt EQ 0 THEN BEGIN
          PRINTF,output,FORMAT='(4X,I4,6G18.6)',j,amplitude(i,j),position(i,j),width(i,j),sig_amplitude(i,j),sig_position(i,j),sig_width(i,j)
        ENDIF ELSE BEGIN
          PRINTF,output,FORMAT='(4X,I4,8G18.6)',j,amplitude(i,j),position(i,j),width(i,j),eta(i,j),sig_amplitude(i,j),sig_position(i,j),sig_width(i,j),sig_eta(i,j)
        ENDELSE
      ENDIF
  ENDFOR
  ; ******************* Print the background parameters **********************
  IF nterms GT 0 THEN BEGIN
      IF KEYWORD_SET(printing) THEN BEGIN
          fmt='(8X,'+STRCOMPRESS(2*nterms, /REMOVE_ALL)+'G18.6)'
          PRINT,FORMAT=fmt,bg(i,*),sig_bg(i,*)
      ENDIF   
      IF initial_fit_finished EQ 1 AND KEYWORD_SET(file) THEN BEGIN
          fmt='(8X,'+STRCOMPRESS(2*nterms,/REMOVE_ALL)+'G18.6)'
          PRINTF,output,FORMAT=fmt,bg(i,*),sig_bg(i,*)
      ENDIF
  ENDIF
  ; ******************* Print further parameters **********************
  IF furtherterms GT 0 THEN BEGIN
      IF KEYWORD_SET(printing) THEN BEGIN
            fmt='(8X,'+STRCOMPRESS((2*furtherterms), /REMOVE_ALL)+'G18.6)'
            PRINT,FORMAT=fmt,a(npeaks*peakpars+nterms+INDGEN(furtherterms)),sigmaa(npeaks*peakpars+nterms+INDGEN(furtherterms))
      ENDIF
      IF initial_fit_finished EQ 1 AND KEYWORD_SET(file) THEN BEGIN
            fmt='(8X,'+STRCOMPRESS((2*furtherterms), /REMOVE_ALL)+'G18.6)'
            PRINTF,output,FORMAT=fmt,a(npeaks*peakpars+nterms+INDGEN(furtherterms)),sigmaa(npeaks*peakpars+nterms+INDGEN(furtherterms))
      ENDIF  
  ENDIF
  IF KEYWORD_SET(file) THEN FLUSH,output
  IF fitflag THEN BEGIN ; check for instable least squares
      PRINT,'Least-squares becomes instable, so quit here ...'
      i=last
  ENDIF
ENDFOR
IF KEYWORD_SET(click) THEN WIDGET_CONTROL,next_button,SET_VALUE='';,/DESTROY
IF KEYWORD_SET(file) THEN FREE_LUN,output
; ######################## Calculate a positive FWHM from sigma ####################################################
;FOR j=0,npeaks-1 DO  a(j*peakpars+2)=ABS(a(j*peakpars+2))*sig2fwhm
;###################### Prepare return of workspace containing fitted diagrams and its data structure ##############
mod_datp,datp,'x',x
IF NOT KEYWORD_SET(datpp) THEN BEGIN
  give_datp,datp
ENDIF
mod_datp,datp,'x',y
mod_datp,datp,'y',0
w_tit=datp.w_tit
IF W_intensity GT 0 THEN BEGIN
  datp.w_tit='peak intensity from NewFit: '+w_tit
  mod_datp,datp,'e',sig_intensity
  give_w,intensity,w=W_intensity
  give_datp,datp,w=w_intensity
ENDIF
IF W_bg GT 0 THEN BEGIN
  datp.w_tit='background polynom from NewFit: '+w_tit
  mod_datp,datp,'e',sig_bg
  give_w,bg,w=W_bg
  give_datp,datp,w=w_bg
ENDIF
IF W_position GT 0 THEN BEGIN
  datp.w_tit='peak position from NewFit: '+w_tit
  mod_datp,datp,'e',sig_position
  give_w,position,w=W_position
  give_datp,datp,w=w_position
ENDIF
IF W_width GT 0 THEN BEGIN
  datp.w_tit='peak full width half maximum from NewFit: '+w_tit
  mod_datp,datp,'e',sig_width
  give_w,width,w=W_width
  give_datp,datp,w=w_width
ENDIF
IF W_eta GT 0 THEN BEGIN
  datp.w_tit='pseuto-Voigt eta from NewFit: '+w_tit
  mod_datp,datp,'e',sig_eta
  give_w,eta,w=W_eta
  give_datp,datp,w=w_eta
ENDIF
IF W_amplitude GT 0 THEN BEGIN
  datp.w_tit='peak amplitude from NewFit: '+w_tit
  mod_datp,datp,'e',sig_amplitude
  give_w,amplitude,w=W_amplitude
  give_datp,datp,w=w_amplitude
ENDIF
IF W_H GT 0 THEN BEGIN
  datp.w_tit='PSD height from NewFit: '+w_tit
  mod_datp,datp,'e',sig_H
  give_w,H,w=W_H
  give_datp,datp,w=w_H
ENDIF
IF W_S GT 0 THEN BEGIN
  datp.w_tit='Sample height from NewFit: '+w_tit
  mod_datp,datp,'e',sig_S
  give_w,S,w=W_S
  give_datp,datp,w=w_S
ENDIF
IF W_L GT 0 THEN BEGIN
  datp.w_tit='PSD-sample distance from NewFit: '+w_tit
  mod_datp,datp,'e',sig_L
  give_w,L,w=W_L
  give_datp,datp,w=w_L
ENDIF
IF W_HR GT 0 THEN BEGIN
  datp.w_tit='FWHM of rectangle part from NewFit: '+w_tit
  mod_datp,datp,'e',sig_HR
  give_w,HR,w=W_HR
  give_datp,datp,w=w_HR
ENDIF
return,f
END

