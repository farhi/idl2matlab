
TODO:	if then else in i2m_tr
	!error
	matlabrename in i2m_tr
	sz=(size(basl))(3) -> sz=i2m_index(sizz(basl),'1+3') ... seems ok
	d1_array, d2_array ... do some tests

	GLOBAL verify prefix always i2m prefix (axes_wd ...)
	device2data.m -> i2m_f_device2data.m etc... !!!
	axes_wd!!! -> rewrite my_delete delete_tv erase image_cont multiplots multiplotsAfter
	                      tv tvrd tvscl wset wdelete i2m_init widget_draw widget_i2mrec ...??
	!d.flags -> warning not translated
	!d.window is an axes, not a figure when a widget
	device,font= missing keyword
	device,copy bugs + tv also ...
	TV TVscl surface ... no loop + no axes_wd + (<= mini)=NaN -> (<= mini)=NaN

	HEADER for passing parameters in Scilab & Matlab (try same code)
	idl2matlab -> search lib from exe path + line in log is wrong
                      + traduct several files!
        undeclared variables -> ''
        comments bad places if before an IF (logo.m ...)
        I2M_pos -> last occurence if same names
                      
        /frame for widget_base
	motion_events
	byteorder uniq smooth randomu help (to write)

WIDGETS modified by Nadege:
	widget_base    : i2m_wcolor figure('color',...
	widget_resource: idem
	widget_draw    : axes_xd + color + new uicontrol... bug=set(wid<-wi0
	widget_font    : fsize=11 -> 9 , fname='Arial' -> 'Courier new'
	widget_i2mrec  : axes_wd ...
	Xmanager       :event_ -> _event

CHANGES I2M (code.c):
	     -~strcomp -> strcne
	     -(32b)    -> byte(32) (replicate etc...)
*****	     -string(i)=string in Idl but one char in Matlab !!!  ?
*****	     -M(i)=V(i:j) ok in Idl but not in Matlab !!!  ?
*****	     -M=V sometimes V must be transposed !!!  ?

TEST DIAL_XBU
	 DialModValue must pass DD
	-dd=dial_xbu_macro(dd) (no I2M_out xbu_macro)
	-dialmodvalue -> dd.x=... (xbu_macro*4 , xbu_window*2)
	-ll=''-> if ll(0 +1) <= ' ') --> ll=' '
	-i2mvs_error = 0 -> =lasterr (dial_xbu_macro, xbu_event)
	-i2m_init
	-xbu_window 350->250 18->10 lamp_b1=[]
	-xbu_event fifi->1 line->1
	-basl((1 +1):(14 +1),2 +1) = transpos(d1_array ...)
	-generic 'mad' -> 'lamp'

TEST BYGEORGE
	-generic 'mad' -> 'lamp'
	-dial_name() -> dial_name (dial_bygeorge)
	-dd=dial_name_macro(dd)   (by_timer)

TEST RDID
	-wout()=buf'
	-vparam=xxx'

TEST LAMP
	-p_screen -> screen=[]
	-p_did_event -> i=1
	-lamp_ -> bas_geo2=1
	
	-logo (did_pix did_pio) p_tremble (did_buf) lamp_ (lob)
	
	-exit has no effect
	-map=0 for fig has no effect
	-lamp.m   -> george=0

DEBUG	warning on
	warning backtrace
	dbstop if all error
	dbcont
	dbup
	dbquit	