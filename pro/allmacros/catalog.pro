pro catalog, path , year_trim , inst ,return_cat ,return_mod ,return_fix
;** *******
;** Used by the TOUCH_BASE interface.
;** This procedure returns a catalog of data-file series formatted as follow:
;**	"004278 004862 16-JAN-95 exper_nam1"
;**	"004863 004901 21-FEB-95 exper_nam2"
;**	   .
;**	   .
;**	   .
;** The lenght of numors is defined by the Magic-number in CUSTOMIZE (6 in this case).
;** The date is 9 char length.
;** The experiment name is 10 char length.

;** return_mod=1 if TOUCH_BASE must add the sub_directory "inst" to the path (path/inst)
;**      else =0 (NOTE:in CUSTOMIZE, if a database label contains the string "Cycle" then LAMP does the same)
;** return_fix(0) is a prefix for numors or '' (ex: spec004863)
;** return_fix(1) is a suffix for numors or '' (ex: spec004863.dat)

;** path is defined in TOUCH_MANAGE but generaly you have to change its content.
;** inst is the choosen format(or instrument).
;** year_trim is "963" for the third cycle of 1996.

;THE FOLLOWING CODE IS USED AT I.L.L.
;*** ********* **** ** **** ** ******
;SPAWN,'cat '+path+'/DATA_CATALOG |grep " '+year_trim + ' " |grep " '+inst+' "' ,return_cat
;return_mod   =1
;return_fix(0)=''
;return_fix(1)=''

;THE FOLLOWING CODE IS USED AT H.M.I.
;*** ********* **** ** **** ** ******
;return_mod   = 0
;return_fix(0)='V5_'
;return_fix(1)='.dat'
;path	      ='[PAPPAS.LOCAL_DATA]'
;return_cat   = ['04278 04862 16-JAN-97 exper_nam1',
;		 '04863 04901 21-FEB-97 exper_nam2'] ;See that magic-number=5

PRINT,string(7b)+' YOU MAY UPDATE catalog.pro IN THE DIRECTORY lamp/lamp_mac !!!!!!'
PRINT,string(7b)+' THE PUZZLE ...'
end
