
TODO:	if then else
	!error
	DialModValue must pass DD
	sz=(size(basl))(3) -> sz=i2m_index(sizz(basl),'1+3')
	
	byteorder uniq smooth randomu

CHANGES I2M:
	     -~strcomp -> strcne
	     -(32b)    -> byte(32) (replicate etc...)
*****	     -string(i)=string in Idl but one char in Matlab !!!  ?
	   
TEST DIAL_XBU
	-dd=dial_xbu_macro(dd) (no I2M_out xbu_macro)
	-dialmodvalue -> dd.x=... (xbu_macro*4 , xbu_window*2)
	-ll=''; if ll(0 +1) <= ' ') --> ll=' '
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
	-p_screen -> size
	-lamp.m   -> george=0
	