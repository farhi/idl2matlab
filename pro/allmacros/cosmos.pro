; Cosmos
; version 2, Aug 2002
; written by Mark Laver
; interprets raw data from D17 runs
;  version 2 : uses errors instead of min/max values
; fundamental changes from d17tof :
;  errors = sqrt(count + 1)
;  angle / tof distance correction

; declare global variables
pro init_vars

; declare global variables
common global, widget_id, options
common table_head, table_labels
common analysis, constants, anal
common water, water_file, water_array
common direct, direct1_files, direct2_files, direct3_files, direct1_array, direct2_array, direct3_array, direct1_params, direct2_params, direct3_params
common waits, waittime
common dialogtxt, warnings

widget_id = { main:0L, datapath:0L, waterrun:0L, table:0L, columns:0L, filldown:0L, fillinc:0L, clear:0L, insertrow:0L, insertno:0L, calopt:0L, detopt:0L, macopt:0L,$
 save:0L, reset:0L, quit:0L, go:0L, calc:0L, foregd:0L, backgdleft:0L, backgdright:0L, lambda:0L, normalise:0L, backmeth:0L, group:0L, groupvar:0L, plotgraph:0L, $
verbose:0L, calc_cancel:0L, calc_ok:0L, detect:0L, usefulx:0L, searchx:0L, searchy:0L, detect_cancel:0L, detect_ok:0L, mach:0L, poff:0L, d0:0L, openoff:0L, pixelwidth:0L, $ 
mach_cancel:0L, mach_ok:0L, column:0L, colthirdang:0L, colinstrbackgd:0L, column_cancel:0L, column_ok:0L, dialogue:0L, dialogue_text:0L, dialogue_cancel:0L, dialogue_ok:0L, $
plot:0L, plot_area:0L, plot_close:0L }
options = { optstruct, path:'', waterrun:'', rowno:20L, lambda:'2.2, 19', foregd:'13', backgdleft:'10', backgdright:'10',$
normalise:0, backmeth:0, group:0, groupvar:2., plotgraph:0, verbose:0, usefulx:'30, 255', searchx:'', searchy:'auto',$
poff:279.4, d0:4.1685, openoff:1.02, pixelwidth:1.0213, colthirdang:0, colinstrbackgd:0 }

table_labels = [ 'Direct 1', 'Direct 2', 'Reflect 1', 'Reflect 2', 'Factor', 'Theta 1', 'Theta 2', 'Out file', 'Out LAMP' ]

constants = { chopsep:85e-3, chopwin:45., interslit:3500., pixels_x:286L, pixels_y:276L, pixels_t:500L, planckperkg:3.956e-7 }
anal = { quit:0, row:0L, usefulx:lonarr(2), searchx:lonarr(2), searchy:lonarr(2), foregd:lonarr(1), backgdleft:lonarr(2), backgdright:lonarr(2), lambda:fltarr(2) }

water_file = strarr(1)
water_array = make_array(constants.pixels_x, /float, value = 1.)

waittime = { none:0.05, short:0.1, long:2. }

warnings = strarr(1)

; define (other) global structures
temppar = { parameters, monitor:0L, runtime:0., dan:0., san:0., size_x:0L, size_t:0L, x_min:0L, x_max:0L, openangle:0., period:0., slits:0., tofd:0., delay:0., channelwidth:0., pixeldensity:0L }

direct1_files = strarr(1)
direct2_files = strarr(1)
direct3_files = strarr(1)
direct1_array = lonarr(constants.pixels_x, constants.pixels_t)
direct2_array = lonarr(constants.pixels_x, constants.pixels_t)
direct3_array = lonarr(constants.pixels_x, constants.pixels_t)
direct1_params = { parameters }
direct2_params = { parameters }
direct3_params = { parameters }

return
end


; append datasets [ x, err_x, y, err_y ]
function anal_append, array1, array2
return, transpose([ transpose(array1), transpose(array2) ])
end


; subtract averaged background on array [ x, t ] with error [ x, t ]
pro anal_backgroundaverage, arr, err, mask

for i = 0, n_elements(arr[0, *]) - 1 do begin
	background = mean(arr[mask, i])
	arr[*, i] = arr[*, i] - background
	err[*, i] = sqrt(err[*, i]^2 + abs(background)) ; add error in quadrature
endfor

return
end


; searches for peak in array, returning thresholds of peak
function anal_peaksearchbasic, array

result = lonarr(2)
temp2 = max(array, temp1)
result[0] = temp1 ; start at maximum
temp2 = (temp2 + min(array)) / 2. ; threshold of peak
while (array[result[0]] ge temp2) and (result[0] gt 0) do result[0] = result[0] - 1
result[1] = temp1
while (array[result[1]] ge temp2) and (result[1] lt n_elements(array) - 1) do result[1] = result[1] + 1

return, result
end


; refined search for peak in array using previous search and foreground range (0 is no range) and guess, returning peak centre
function anal_peaksearchrefined, array, search, foregd

temp = max(array[search[0]:search[1]], guess_new)
guess_new = guess_new + search[0]
repeat begin
guess_old = guess_new
low = (guess_old - fix((1. + foregd) / 2.)) > search[0]
high = ((guess_old + fix((1. + foregd) / 2.)) < search[1]) > (low + 1)
; calculated weighted mean
temp = total(array[low:high])
if temp gt 0. then guess_new = round(total(array[low:high] * (lindgen(high - low + 1) + low)) / temp)
endrep until guess_old eq guess_new

return, guess_new
end


; sort dataset [ x, err_x, y, err_y ] by x value
function anal_sort, dataset
result = dataset[*, sort(dataset[0, *])]
return, result
end


; normalise 1D array
function array_norm, array
return, array / (mean(array))
end


; convert number to string as printed
function str_make, value
return, strcompress(string(value, /print), /remove_all)
end


; separate string into a 1D array of component strings (separator is ',' or ':', keeping the separator at the front of each string)
function str_split, original

w = where((byte(original) eq (byte(','))[0]) or (byte(original) eq (byte(':'))[0]), count)
arr = strarr(count + 1)
spos = strlen(original)
for i = 0, count - 1 do begin
	arr[count - i] = strmid(original, w[count - i - 1], spos - w[count - i - 1])
	spos = w[count - i - 1]
endfor
arr[0] = strmid(original, 0, spos)

return, arr
end


; convert string to string plus number (floating or integer)
pro str_value, original, remainder, value, floating = f, integer = i

pos = strlen(original) - 1

on_ioerror, str_value_exit

while pos ge 0 do begin
if keyword_set(i) then value = long(strmid(original, pos, strlen(original) - pos)) else value = float(strmid(original, pos, strlen(original) - pos))
pos = pos - 1
end

str_value_exit:
if keyword_set(i) then value = long(strmid(original, pos + 1, strlen(original) - pos - 1)) else value = float(strmid(original, pos + 1, strlen(original) - pos - 1))
remainder = strmid(original, 0, pos + 1)

return
end


; 0
; -----------------------------------------------------------------------------
; 1


; calculate angle offset for flat detector surface
function anal_calcangleoffset, peakpos, xoff, density, distance
common global, widget_id, options
common analysis, constants, anal
return, atan(((peakpos + xoff) * density - (constants.pixels_x / 2.)) * (options.pixelwidth / 1000.), distance)
end


; returns factor to multiply dataset 1 [ x, err x, y, err y ] in order to match dataset 2
function anal_factor, array_a, array_b, linear = l, spline = s

result = fltarr(2)

; sort arrays
a = anal_sort(array_a)
b = anal_sort(array_b)
; extract cross over, returning zero if no cross over
overa = where(a[0, *] ge min(b[0, *]), count)
if count gt 0 then a = a[*, overa] else return, result
b = b[*, where(b[0, *] le max(a[0, *]))]
; produce interpolation arrays
if keyword_set(l) then begin
inter = interpol(a[2, *], a[0, *], b[0, *])
endif else begin
deriv = spl_init(a[0, *], a[2, *])
inter = spl_interp(a[0, *], a[2, *], deriv, b[0, *])
endelse
; find factor using sum of least squares
result[0] = total(b[2, *] * inter) / float(total(inter * inter))
result[1] = result[0]

return, result
end


; close open subwindows
pro close_subwins
common global, widget_id, options

if widget_id.calc ne 0 then widget_control, widget_id.calc, /destroy
if widget_id.detect ne 0 then widget_control, widget_id.detect, /destroy
if widget_id.mach ne 0 then widget_control, widget_id.mach, /destroy
if widget_id.column ne 0 then widget_control, widget_id.column, /destroy

return
end


; add text to dialogue window
pro dialogue_add, txt
common global, widget_id, options

if widget_id.dialogue ne 0 then begin
widget_control, widget_id.dialogue_text, get_value = textarr
textarr = [textarr, txt]
if n_elements(textarr) ge 30 then widget_control, widget_id.dialogue_text, set_text_top_line = n_elements(textarr) - 30, set_value = textarr else widget_control, widget_id.dialogue_text, set_value = textarr
endif

return
end


; add final warning to dialogue window
pro dialogue_warn, txt
common dialogtxt, warnings
common analysis, constants, anal

dialogue_add, ' Warning : ' + txt
temp = ' Row ' + str_make(anal.row) + ': ' + txt
warnings = [warnings, temp]

return
end


; clear requested
pro ev_clear
common global, widget_id

select = widget_info(widget_id.table, /table_select)
widget_control, widget_id.table, set_value = strarr(abs(select[2]-select[0])+1, abs(select[3]-select[1])+1), /use_table_select

end


; insert row requested
pro ev_insertrow
common global, widget_id

widget_control, widget_id.insertno, get_value = no
if no gt 0 then widget_control, widget_id.table, insert_rows = no, /use_table_select else widget_control, widget_id.insertno, set_value = 0L

end


; quit requested
pro ev_quit
common global, widget_id

if dialog_message('Are you sure you want to quit?', /cancel, /default_cancel, dialog_parent = widget_id.main, title = 'Cosmos : Warning') eq 'OK' then widget_control, widget_id.main, /destroy
end


; update calculation options
pro ev_updatecalc
common global, widget_id, options

widget_control, widget_id.foregd, get_value = txtarr1
options.foregd = txtarr1[0]
widget_control, widget_id.backgdleft, get_value = txtarr2
options.backgdleft = txtarr2[0]
widget_control, widget_id.backgdright, get_value = txtarr3
options.backgdright = txtarr3[0]
widget_control, widget_id.lambda, get_value = txtarr4
options.lambda = txtarr4[0]
widget_control, widget_id.normalise, get_value = val
options.normalise = val
widget_control, widget_id.backmeth, get_value = val
options.backmeth = val
widget_control, widget_id.group, get_value = val
options.group = val
widget_control, widget_id.groupvar, get_value = flt
options.groupvar = flt
widget_control, widget_id.plotgraph, get_value = val
options.plotgraph = val
widget_control, widget_id.verbose, get_value = val
options.verbose = val
widget_control, widget_id.calc, /destroy

end


; update detection range options
pro ev_updatedetect
common global, widget_id, options
common water, water_file, water_array

widget_control, widget_id.usefulx, get_value = txtarr1
options.usefulx = txtarr1[0]
widget_control, widget_id.searchx, get_value = txtarr2
options.searchx = txtarr2[0]
widget_control, widget_id.searchy, get_value = txtarr3
options.searchy = txtarr3[0]
widget_control, widget_id.detect, /destroy
water_file = strarr(1)

end


; update machine options
pro ev_updatemach
common global, widget_id, options

if dialog_message('Changing the offsets may have serious consequences.' + string(10b) + 'Are you sure you want to do this?', /cancel, /default_cancel, dialog_parent = widget_id.mach, title = 'Cosmos : Warning') eq 'OK' then begin
widget_control, widget_id.poff, get_value = val
options.poff = val
widget_control, widget_id.d0, get_value = val
options.d0 = val
widget_control, widget_id.openoff, get_value = val
options.openoff = val
widget_control, widget_id.pixelwidth, get_value = val
options.pixelwidth = val
endif
widget_control, widget_id.mach, /destroy

end


; window closed
pro ev_winclose, window_id
common global, widget_id
case window_id of
widget_id.calc : widget_id.calc = 0
widget_id.detect : widget_id.detect = 0
widget_id.mach : widget_id.mach = 0
widget_id.column : widget_id.column = 0
widget_id.dialogue : widget_id.dialogue = 0
widget_id.plot : widget_id.plot = 0
else :
endcase

end


; write to lamp
pro lamp_write, workspace, array, row

if (workspace ge 1) and (workspace le 23) then begin
common c_lamp_w, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17, w18, w19, w20, w21, w22, w23, wtb, wintb, w_in, w_out, w_buf, w_min, w_max, w_numor
common c_lamp_x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x_in, x_out, x_buf
common c_lamp_y, y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y_in, y_out, y_buf
common c_lamp_z, z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15, z16, z17, z18, z19, z20, z21, z22, z23, z_in, z_out, z_buf
common c_lamp_e, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e_in, e_out, e_buf
common c_lamp_n, n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16, n17, n18, n19, n20, n21, n22, n23, n_in, n_out, n_buf
common c_lamp_p, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, npars, p_in, p_out, p_buf, par_txt, par_txt_all
common c_lamp_pv, pv0, pv1, pv2, pv3, pv4, pv5, pv6, pv7, pv8, pv9, pv10, pv11, pv12, pv13, pv14, pv15, pv16, pv17, pv18, pv19, pv20, pv21, pv22, pv23, pv_in, pv_out, pv_buf
common c_lamp_par, par1, par2, par3, par4, par5, par6, par7, par8, par9, par10, par11, par12, par13, par14, par15, par16, par17, par18, par19, par20, par21, par22, par23
common c_lamp_tit, x_tit, y_tit, z_tit, w_tit, other_tit, head_tit
dialogue_add, ' Writing to LAMP workspace w' + str_make(workspace)
wrksp = str_make(workspace)
write_x = reform(array[0, *])
write_w = reform(array[2, *])
write_e = reform(array[3, *])
write_wmin = min(write_w)
write_wmax = max(write_w)
temp = execute('x' + wrksp + ' = write_x')
temp = execute('y' + wrksp + ' = 0')
temp = execute('z' + wrksp + ' = 0')
temp = execute('w' + wrksp + ' = write_w')
temp = execute('w_max[' + wrksp + '] = write_wmax')
temp = execute('w_min[' + wrksp + '] = write_wmin')
temp = execute('e' + wrksp + ' = write_e')
temp = execute('n' + wrksp + ' = 0')
temp = execute('p' + wrksp + ' = 0')
temp = execute('par' + wrksp + ' = 0')
temp = execute('pv' + wrksp + ' = 0')
temp = execute('w_tit[' + wrksp + '] = "Reflectivity output from Cosmos, row ' + str_make(row) + '"')
temp = execute('x_tit[' + wrksp + '] = "q"')
temp = execute('y_tit[' + wrksp + '] = ""')
temp = execute('z_tit[' + wrksp + '] = ""')
temp = execute('other_tit[' + wrksp + '] = ""')
endif

return
end


; get main window options
pro main_getvars
common global, widget_id, options

widget_control, widget_id.datapath, get_value = temp
options.path = temp[0]
widget_control, widget_id.waterrun, get_value = temp
options.waterrun = temp[0]

return
end


; add graph to plot window, returning wait time to allow user to freeze plot
function plot_add, array, row
common global, widget_id, options
common waits, waittime

waitval = waittime.none
if widget_id.plot ne 0 then begin
widget_control, widget_id.plot_close, get_value = textarr
if textarr[0] eq ' Freeze ' then begin
widget_control, widget_id.plot_area, get_value = win_id
wset, win_id
pts = where((array[2, *] - array[3, *]) gt 0., num)
if num gt 0 then begin
	plot, reform(array[0, pts]), alog10(reform(array[2, pts])), title = 'Row ' + str_make(row), xtitle = 'q', /ynozero, ytitle = 'log( Reflectivity )'
	errplot, reform(array[0, pts]), alog10(reform(array[2, pts] - array[3, pts])), alog10(reform(array[2, pts] + array[3, pts]))
endif else begin
	erase
	xyouts, 200, 300, 'Empty plot for row ' + str_make(row), /device
endelse
waitval = waittime.long
endif
endif

return, waitval
end


; finish plot window
pro plot_finish
common global, widget_id, options

if widget_id.plot ne 0 then widget_control, widget_id.plot_close, set_value = ' Close '

return
end


; save options and table values
pro save_vars
common global, widget_id, options
common table_head, table_labels

save_name = 'cosmos_defaults'
print_name = 'cosmos_table.csv'
openw, save_lun, save_name, error = temp, /get_lun
if temp eq 0 then begin
on_ioerror, save_exit
widget_control, widget_id.table, get_value = table
; check for blank rows - blank[i] = -1 if row is blank
blanks = intarr(n_elements(table[0,*]))
for i = 0, (n_elements(table[0,*]) - 1) do begin
	temp2 = where(table[*,i] ne '', temp)
	if temp le 0 then blanks[i] = -1
endfor
save_rows = where(blanks ne -1, count)
save_options = options
if count ne 0 then begin
	save_table = strarr(n_elements(table_labels), count)
	for i = 0, (count - 1) do begin
	save_table[*, i] = table[*, save_rows[i]]
	endfor
	save_options.rowno = n_elements(save_table[0,*])
endif else save_options.rowno = 0
save_tags = strlowcase(tag_names(save_options))
; write options
printf, save_lun, 'Options'
for i = 0, (n_tags(save_options) - 1) do begin
	printf, save_lun, '' ; spacer
	printf, save_lun, save_tags[i]
	printf, save_lun, save_options.(i)
endfor
if save_options.rowno ne 0 then begin
	printf, save_lun, '' ; spacer
	printf, save_lun, 'Table'
	for i = 0, (n_elements(save_table[0,*]) - 1) do begin
		for j = 0, (n_elements(save_table[*,0]) - 1) do printf, save_lun, save_table[j,i]
	endfor
endif
close, save_lun
free_lun, save_lun

; save printable table for user in .csv format
openw, print_lun, print_name, error = temp, /get_lun
if temp eq 0 then begin
printf, print_lun, 'Table from Cosmos at ' + systime(0) + ':' + string(13b)
if save_options.rowno ne 0 then begin
	frmt = '($,' + str_make(n_elements(table_labels) - 1) + '(a, ","), a, a2)'
	printf, print_lun, format = frmt, table_labels, string(13b) + string(10b)
	for i = 0, (n_elements(save_table[0,*]) - 1) do printf, print_lun, format = frmt, save_table[*,i], string(13b) + string(10b)
endif
printf, print_lun, string(13b) ; spacer
printf, print_lun, 'Options:' + string(13b)
for i = 0, (n_tags(save_options) - 1) do printf, print_lun, format = '($, a, ",", a, a2)', save_tags[i], save_options.(i), string(13b) + string(10b)
close, print_lun
free_lun, print_lun
endif

endif

save_exit:
return
end


; outputs string corresponding to theta value
function str_formtheta, angle
case angle of
-1 : str = 'san'
-2 : str = 'dan'
else : str = str_make(angle)
endcase
return, str
end


; parse input string, returning array
function str_parse, original, float = f, integer = i,  flt_range = fr, int_range = ir, string_set = s

substrs = str_split(original)
if keyword_set(f) then begin
	str_value, substrs[n_elements(substrs) - 1], remainder, value, /floating
	array = [ value ]
	return, array
endif
if keyword_set(i) then begin
	str_value, substrs[n_elements(substrs) - 1], remainder, value, /integer
	array = [ value ]
	return, array
endif
if keyword_set(fr) or keyword_set(ir) then begin
	if n_elements(substrs) eq 1 then begin
		if keyword_set(ir) then begin
			str_value, substrs[0], remainder, value, /integer
			array = [ value, -1L ]
			return, array
		endif else begin
			str_value, substrs[0], remainder, value, /floating
			array = [ value, -1.]
			return, array
		endelse
	endif else begin
		if keyword_set(ir) then begin
			str_value, substrs[n_elements(substrs) - 2], remainder, int1, /integer
			str_value, substrs[n_elements(substrs) - 1], remainder, int2, /integer
			array = [ int1, int2 ]
			return, array
		endif else begin
			str_value, substrs[n_elements(substrs) - 2], remainder, flt1, /floating
			str_value, substrs[n_elements(substrs) - 1], remainder, flt2, /floating
			array = [ flt1, flt2 ]
			return, array
		endelse
	endelse
endif
; default or string_set
array = [ substrs[0] ]
curstr = 1
str_value, substrs[0], rempre, intpre, /integer
while curstr lt n_elements(substrs) do begin
str_value, substrs[curstr], remcur, intcur, /integer
remcur = strmid(remcur, 1, strlen(remcur) - 1)
if (strmid(substrs[curstr], 0, 1) eq ':') and (rempre eq remcur) then begin
	if intcur gt intpre then begin
		for i = intpre + 1, intcur do array = [ array, strcompress(remcur + string(i), /remove_all) ]
	endif
endif else array = [ array, strmid(substrs[curstr], 1, strlen(substrs[curstr]) - 1) ] ; ','
curstr = curstr + 1
rempre = remcur
intpre = intcur
end

return, array
end


; updates table labels
pro table_labelupdate
common global, widget_id, options
common table_head, table_labels

if options.colthirdang eq 0 then begin
	if options.colinstrbackgd eq 0 then table_labels = [ 'Direct 1', 'Direct 2', 'Reflect 1', 'Reflect 2', 'Factor', 'Theta 1', 'Theta 2', 'Out file', 'Out LAMP' ] else table_labels = [ 'Direct 1', 'Direct 2', 'Reflect 1', 'Reflect 2', 'Instr 1', 'Instr 2', 'Factor', 'Theta 1', 'Theta 2', 'Out file', 'Out LAMP' ]
endif else begin
	if options.colinstrbackgd eq 0 then table_labels = [ 'Direct 1', 'Direct 2', 'Direct 3', 'Reflect 1', 'Reflect 2', 'Reflect 3', 'Factor 1-2', 'Factor 2-3', 'Theta 1', 'Theta 2', 'Theta 3', 'Out file', 'Out LAMP' ] else table_labels = [ 'Direct 1', 'Direct 2', 'Direct 3', 'Reflect 1', 'Reflect 2', 'Reflect 3', 'Instr 1', 'Instr 2', 'Instr 3', 'Factor 1-2', 'Factor 2-3', 'Theta 1', 'Theta 2', 'Theta 3', 'Out file', 'Out LAMP' ]
endelse

return
end


; 1
; -----------------------------------------------------------------------------
; 2


; subtract fitted background
pro anal_backgroundfit, arr, err, mask, str

tsize = n_elements(arr[0, *])
xsize = n_elements(arr[*, 0])
tlim = tsize - 1
fit = fltarr(2, tsize)

for i = 0, tlim do begin
	fit(*, i) = linfit(mask, arr[mask, i])
	background = findgen(xsize) * fit[1, i] + fit[0, i] ; 1D array in x
	arr[*, i] = arr[*, i] - background
	err[*, i] = sqrt(err[*, i]^2 + abs(background)) ; add error in quadrature
endfor

dialogue_add, ' Background fitted to ' + str + ' runs with regression factors  y = a x + b  :'
dialogue_add, string(9b) + 'min' + string([9b, 9b]) + 'max'
dialogue_add, '  a' + string(9b) + str_make(min(fit[1, *])) + string(9b) + str_make(max(fit[1, *]))
dialogue_add, '  b' + string(9b) + str_make(min(fit[0, *])) + string(9b) + str_make(max(fit[0, *]))

return
end


; groups data using bundle algorithm (cf bundle.pro)
pro anal_bundle, result, resf

; unpackage data for bundle module
x = result[0, *] ; q values
y = result[2, *] ; reflectivity values
e = result[3, *] ; error on reflectivity values
dq = result[1, *] ; resolution of q values

; arrays to store grouped data
ny = fltarr(1000)
nx = fltarr(1000)
nee = fltarr(1000)
ndq = fltarr(1000)

dialogue_add, ' Grouping data using bundle : (factor ' + str_make(resf) + ')'

; number of elements in data
tot = n_elements(x) - 1

i = 1
repeat begin

c = 0 ; address to data
num = tot - 1
new = 0 ; address to grouped data
pairs = 0

while c lt num do begin
	if ((x[c + 1] - x[c]) lt (dq[c] / resf)) and (x[c + 1] ne 0) then begin
		ny[new] = (y[c + 1] / e[c + 1]^2. + y[c] / e[c]^2.) / (e[c + 1]^(-2.) + e[c]^(-2.))
		nee[new] = sqrt(e[c]^2. + e[c + 1]^2.) / 2.
		nx[new] = (x[c] + x[c + 1]) / 2.
		ndq[new] = (x[c + 1] + dq[c + 1] - x[c] + dq[c]) / 2.
		pairs = pairs + 1
		c = c + 2
		tot = tot - 1
	endif else begin
		ny[new] = y[c]
		nx[new] = x[c]
		nee[new] = e[c]
		ndq[new] = dq[c]
		c = c + 1
	endelse
	new = new + 1
end

y = ny
x = nx
e = nee
dq = ndq

dialogue_add, '  ' + str_make(pairs) + ' pairs found on iteration ' + str_make(i)
i = i + 1
endrep until pairs eq 0

result = fltarr(4, tot)
result[0, *] = nx[0 : tot - 1]
result[1, *] = ndq[0 : tot - 1]
result[2, *] = ny[0 : tot - 1]
result[3, *] = nee[0 : tot - 1]

return
end


; calculate corrected angle for flat detector surface
function anal_correctangle, angle, dirpeakpos, refpeakpos, dirxoff, refxoff, density, dirdist, refdist
return, (2. * angle - anal_calcangleoffset(dirpeakpos, dirxoff, density, dirdist) + anal_calcangleoffset(refpeakpos, refxoff, density, refdist)) / 2.
end


; calculate corrected time of flight distance for flat detector surface
function anal_correctdistance, peakpos, xoff, density, distance
return, distance / cos(anal_calcangleoffset(peakpos, xoff, density, distance))
; no correction
; return, distance
end


; create dialogue box
pro dialogue_create
common global, widget_id, options
common dialogtxt, warnings

if widget_id.dialogue eq 0 then begin
if options.verbose eq 0 then begin
; create dialogue window
widget_id.dialogue = widget_base(/base_align_center, /col, group_leader = widget_id.main, title = 'Cosmos')
widget_id.dialogue_text = widget_text(widget_id.dialogue, /frame, /scroll, value = 'Starting calculation...', xsize = '64', ysize = '32')
panel1 = widget_base(widget_id.dialogue, /align_right, /grid_layout, /row)
widget_id.dialogue_cancel = widget_button(panel1, value = ' Abandon ')
widget_id.dialogue_ok = widget_button(panel1, value = ' Quiet ')

widget_control, widget_id.dialogue, /realize
xmanager, 'Cosmos', widget_id.dialogue, cleanup = 'ev_winclose', event_handler = 'ev_poll', /no_block
warnings = strarr(1)
endif

endif else begin
if options.verbose eq 0 then begin
widget_control, widget_id.dialogue_text, set_value = 'Starting calculation...'
widget_control, widget_id.dialogue_cancel, sensitive = 1
widget_control, widget_id.dialogue_ok, set_value = ' Quiet '
widget_control, widget_id.dialogue, show = 1
warnings = strarr(1)
endif else widget_control, widget_id.dialogue, /destroy
endelse

return
end


; finish dialogue window
pro dialogue_finish
common global, widget_id, options
common dialogtxt, warnings

if n_elements(warnings) le 1 then dialogue_add, 'No warnings found during calculation.' else warnings[0] = 'Warnings found :'
dialogue_add, warnings
if widget_id.dialogue ne 0 then begin
widget_control, widget_id.dialogue_cancel, sensitive = 0
widget_control, widget_id.dialogue_ok, set_value = ' Close '
endif

return
end


; fill down requested
; any numbers found in top row of selected cells are incremented down table
; increment can be of floating type only for factor and theta
pro ev_filldown
common global, widget_id
common table_head, table_labels

select = widget_info(widget_id.table, /table_select)
if (select[0] ne -1) and ((select[3] - select[1]) gt 0) then begin

; get values for selected cells
; note USE_TABLE_SELECT keyword does not work!!
; produces segmentation and bus errors
; use at your peril!!
widget_control, widget_id.table, get_value = cells, sensitive = 0
widget_control, widget_id.fillinc, get_value = inc_req

for column = select[0], select[2] do begin

; obtain tag name of current column
tag_name = table_labels[column]

if (strpos(tag_name, 'Theta') ne -1) or (strpos(tag_name, 'Factor') ne -1) then inc_act = float(inc_req) else inc_act = long(inc_req)

str = strcompress(cells[column, select[1]], /remove_all)
split = str_split(str)

for row = select[1] + 1, select[3] do begin
cells[column, row] = ''

if float(inc_act) eq 0. then cells[column, row] = str else begin

for i = 0, n_elements(split) - 1 do begin
	if (strpos(tag_name, 'Theta') ne -1) or (strpos(tag_name, 'Factor') ne -1) then str_value, split[i], remainder, value, /floating else str_value, split[i], remainder, value, /integer
	cells[column, row] = cells[column, row] + remainder + str_make(value + (inc_act * (row - select[1])))
endfor

endelse

cells[column, row] = strcompress(cells[column, row], /remove_all)

endfor
endfor

widget_control, widget_id.table, set_value = cells, sensitive = 1

endif
end


; open calculation options window requested
pro ev_opencalcwindow
common global, widget_id, options

if widget_id.calc eq 0 then begin

; create calculation options window
widget_id.calc = widget_base(/base_align_center, /col, group_leader = widget_id.main, title = 'Calculation Options')
panel1 = widget_base(widget_id.calc, /base_align_center, /col, /frame, xsize = 350)
widget_id.foregd = cw_field(panel1, /column, title = 'Foreground Range', value = options.foregd, xsize = '16')
panel1 = widget_base(widget_id.calc, /base_align_center, /col, /frame, xsize = 350)
temp = widget_label(widget_id.calc, value = 'Background Range')
panel2 = widget_base(widget_id.calc, col = 2, /grid_layout)
widget_id.backgdleft = cw_field(panel2, /column, title = 'Left', value = options.backgdleft, xsize = '10')
widget_id.backgdright = cw_field(panel2, /column, title = 'Right', value = options.backgdright, xsize = '10')
panel1 = widget_base(widget_id.calc, /base_align_center, /col, /frame, xsize = 350)
widget_id.lambda = cw_field(panel1, /column, title = 'Lambda Range', value = options.lambda, xsize = '16')
panel1 = widget_base(widget_id.calc, /base_align_center, /col, /frame, xsize = 350)
widget_id.normalise = cw_bgroup(panel1, [' Runtime  ', ' Monitor '], /exclusive, label_top = ' Normalise ', /row, set_value = options.normalise)
panel1 = widget_base(widget_id.calc, /base_align_center, /col, /frame, xsize = 350)
widget_id.backmeth = cw_bgroup(panel1, [' Average  ', ' Fit '], /exclusive, label_top = ' Background Method ', /row, set_value = options.backmeth)
panel1 = widget_base(widget_id.calc, /base_align_center, /col, /frame, xsize = 350)
panel2 = widget_base(panel1, col = 2)
widget_id.group = cw_bgroup(panel2, [' Bundle ', ' None '], /col, /exclusive, label_top = ' Grouping ', set_value = options.group)
widget_id.groupvar = cw_field(panel2, /column, /floating, title = string(10b) + 'Factor:', value = options.groupvar, xsize = 8)
panel1 = widget_base(widget_id.calc, /base_align_center, /col, /frame, xsize = 350)
widget_id.plotgraph = cw_bgroup(panel1, ' Plot Graphs ', /nonexclusive, /row, set_value = options.plotgraph)
panel1 = widget_base(widget_id.calc, /base_align_center, /col, /frame, xsize = 350)
widget_id.verbose = cw_bgroup(panel1, [' Verbose  ', ' Quiet '], /exclusive, /row, set_value = options.verbose)
panel1 = widget_base(widget_id.calc, /align_right, /grid_layout, /row)
widget_id.calc_cancel = widget_button(panel1, value = ' Cancel ')
widget_id.calc_ok = widget_button(panel1, value = ' OK ')

widget_control, widget_id.calc, /realize
xmanager, 'Cosmos', widget_id.calc, cleanup = 'ev_winclose', event_handler = 'ev_poll', /no_block

endif else widget_control, widget_id.calc, show = 1

end


; open column options window requested
pro ev_opencolumnwindow
common global, widget_id, options

if widget_id.column eq 0 then begin

widget_id.column = widget_base(/base_align_center, /col, group_leader = widget_id.main, title = 'Setup Columns')
panel1 = widget_base(widget_id.column, /base_align_center, /col, /frame, xsize = 300)
widget_id.colthirdang = cw_bgroup(panel1, ' Third Angle ', /nonexclusive, /row, set_value = options.colthirdang)
panel1 = widget_base(widget_id.column, /base_align_center, /col, /frame, xsize = 300)
widget_id.colinstrbackgd = cw_bgroup(panel1, ' Instrument Backgrounds ', /nonexclusive, /row, set_value = options.colinstrbackgd)
panel1 = widget_base(widget_id.column, /align_right, /grid_layout, /row)
widget_id.column_cancel = widget_button(panel1, value = ' Cancel ')
widget_id.column_ok = widget_button(panel1, value = ' OK ')

widget_control, widget_id.column, /realize
xmanager, 'Cosmos', widget_id.column, cleanup = 'ev_winclose', event_handler = 'ev_poll', /no_block

endif else widget_control, widget_id.column, show = 1

end


; open detection options window requested
pro ev_opendetectwindow
common global, widget_id, options

if widget_id.detect eq 0 then begin

; create detection options window
widget_id.detect = widget_base(/base_align_center, /col, group_leader = widget_id.main, title = 'Detector Ranges')
panel1 = widget_base(widget_id.detect, /base_align_center, /col, /frame, xsize = 280)
widget_id.usefulx = cw_field(panel1, /column, title = ' Useful Range in x ', value = options.usefulx, xsize = 16)
panel1 = widget_base(widget_id.detect, /base_align_center, /col, /frame, xsize = 280)
temp = widget_label(panel1, value = ' Search Range For Peak ')
widget_id.searchx = cw_field(panel1, title = ' x: ', value = options.searchx, xsize = 16)
widget_id.searchy = cw_field(panel1, title = ' t: ', value = options.searchy, xsize = 16)
panel1 = widget_base(widget_id.detect, /align_right, /grid_layout, /row)
widget_id.detect_cancel = widget_button(panel1, value = ' Cancel ')
widget_id.detect_ok = widget_button(panel1, value = ' OK ')

widget_control, widget_id.detect, /realize
xmanager, 'Cosmos', widget_id.detect, cleanup = 'ev_winclose', event_handler = 'ev_poll', /no_block

endif else widget_control, widget_id.detect, show = 1

end


; open machine options window requested
pro ev_openmachwindow
common global, widget_id, options

if widget_id.mach eq 0 then begin

; create machine options window
widget_id.mach = widget_base(/base_align_center, /col, group_leader = widget_id.main, title = 'Machine Options')
panel1 = widget_base(widget_id.mach, /base_align_center, /grid_layout, /col, /frame)
widget_id.poff = cw_field(panel1, /floating, title = '    Offset Poff: ', value = options.poff, xsize = 8)
widget_id.d0 = cw_field(panel1, /floating, title = '      Offset d0: ', value = options.d0, xsize = 8)
widget_id.openoff = cw_field(panel1, /floating, title = ' Opening Offset: ', value = options.openoff, xsize = 8)
widget_id.pixelwidth = cw_field(panel1, /floating, title = '    Pixel Width: ', value = options.pixelwidth, xsize = 8)
panel1 = widget_base(widget_id.mach, /align_right, /grid_layout, /row)
widget_id.mach_cancel = widget_button(panel1, value = ' Cancel ')
widget_id.mach_ok = widget_button(panel1, value = ' OK ')

widget_control, widget_id.mach, /realize
xmanager, 'Cosmos', widget_id.mach, cleanup = 'ev_winclose', event_handler = 'ev_poll', /no_block

endif else widget_control, widget_id.mach, show = 1

end


; freeze or close button pressed on plot window
pro ev_plotevent
common global, widget_id, options
common analysis, constants, anal

widget_control, widget_id.plot_close, get_value = textarr
widget_control, widget_id.plot_close, /clear_events
if textarr eq ' Freeze ' then plot_finish else widget_control, widget_id.plot, /destroy

end


; save requested
pro ev_save

; update main window options
main_getvars

; save to file
save_vars

; save to readable file
save_vars

end


; update column labels request
pro ev_updatecolumns
common global, widget_id, options
common table_head, table_labels

if dialog_message('Changing the column setup will cause distortion of table data.' + string(10b) + 'Are you sure you want to continue?', /cancel, /default_cancel, dialog_parent = widget_id.column, title = 'Cosmos : Warning') eq 'OK' then begin

widget_control, widget_id.colthirdang, get_value = val
options.colthirdang = val
widget_control, widget_id.colinstrbackgd, get_value = val
options.colinstrbackgd = val

widget_control, widget_id.table, get_value = table

table_labelupdate
widget_control, widget_id.table, column_labels = table_labels, table_xsize = n_elements(table_labels)

if n_elements(table[*, 0]) lt n_elements(table_labels) then table = [ table, strarr(n_elements(table_labels) - n_elements(table[*, 0]), n_elements(table[0, *])) ] else table = table[0 : n_elements(table_labels) - 1, *]

widget_control, widget_id.table, set_value = table

endif

widget_control, widget_id.column, /destroy

end


; write to file
pro file_write, outfile, result, direct1, direct2, direct3, reflect1, reflect2, reflect3, instr1, instr2, instr3, theta1, theta2, theta3, water_file, aft = keyaft, dat = keydat, out = keyout, raw = keyraw
common global, widget_id, options
common analysis, constants, anal

outname = str_sep(outfile, '.', /remove_all)
outname = outname[0]
if outname eq '' then begin
	if strlen(reflect1[0]) gt 1 then outname = reflect1[0] else outname = 'cosmos' + str_make(anal.row)
endif
if keyword_set(keyaft) then outfile = outname + '.aft'
if keyword_set(keydat) then outfile = outname + '.dat'
if keyword_set(keyraw) then outfile = outname + '.raw'
if keyword_set(keyout) then outfile = outname + '.out'
openw, write_lun, outfile, error = temp1, /get_lun
if temp1 eq 0 then begin
on_ioerror, write_exit
dialogue_add, ' Writing to file ' + outfile
if keyword_set(keyout) then begin
printf, write_lun, ' Direct beam 1: ', direct1
printf, write_lun, ' Reflect beam 1: ', reflect1
printf, write_lun, ' Instrument background 1: ', instr1
printf, write_lun, ' Theta 1: ', str_formtheta(theta1)
printf, write_lun, ' Direct beam 2: ', direct2
printf, write_lun, ' Reflect beam 2: ', reflect2
printf, write_lun, ' Instrument background 2: ', instr2
printf, write_lun, ' Theta 2: ', str_formtheta(theta2)
printf, write_lun, ' Direct beam 3: ', direct3
printf, write_lun, ' Reflect beam 3: ', reflect3
printf, write_lun, ' Instrument background 3: ', instr3
printf, write_lun, ' Theta 3: ', str_formtheta(theta3)
printf, write_lun, ' Water: ', water_file
printf, write_lun, ' Foreground: ', options.foregd
printf, write_lun, ' Background:  Left: ', options.backgdleft
printf, write_lun, '             Right: ', options.backgdright
printf, write_lun, ' Lambda Range: ', options.lambda
printf, write_lun, ' Useful Pixel Range: ', options.usefulx
printf, write_lun, ' Search Range, x: ', options.searchx
printf, write_lun, ' Search Range, t: ', options.searchy
printf, write_lun, '' ; spacer
printf, write_lun, string(9b) + 'q' + string(9b) + '   refl' + string([9b, 9b]) + 'refl err' + string(9b) + 'q res'
for i = 0, n_elements(result[0, *]) - 1 do printf, write_lun, result[0, i], result[2, i], result[3, i], result[1, i]
endif else if keyword_set(keyaft) then begin
printf, write_lun, 'AFIT' + string(13b)
if strlen(reflect1[0]) gt 0 then printf, write_lun, 'Results for reflect beam ' + reflect1[0] + string(13b) else printf, write_lun, 'Cosmos Results' + string(13b)
printf, write_lun, format = '($,f4.2,a2)', 2.07, string(13b) + string(10b)
printf, write_lun, format = '($,i0,a2)', n_elements(result[0, *]), string(13b) + string(10b)
for i = 0, n_elements(result[0, *]) - 1 do printf, write_lun, format = '($,g10.4,10x,g10.4,10x,g10.4,a2)', result[0, i], result[2, i], result[3, i], string(13b) + string(10b)
endif else if keyword_set(keydat) then begin
for i = 0, n_elements(result[0, *]) - 1 do printf, write_lun, result[0, i], string(9b), result[2, i], string(9b), result[3, i], string(13b)
endif else begin
for i = 0, n_elements(result[0, *]) - 1 do printf, write_lun, result[0, i], string(9b), result[2, i], string(9b), result[3, i], string(9b), result[1, i]
endelse
close, write_lun
free_lun, write_lun
endif
write_exit:

return
end


; load options and table values
function load_vars
common global, widget_id, options
common table_head, table_labels

load_name = 'cosmos_defaults'
openr, load_lun, load_name, error = temp, /get_lun
if temp eq 0 then begin
; on_ioerror, load_exit
load_options = { optstruct }
temp = ''
readf, load_lun, temp ; 'Options'
if temp eq 'Options' then begin
	; read options
	for i = 0, (n_tags(load_options) - 1) do begin
		readf, load_lun, temp ; spacer
		readf, load_lun, temp ; tag
		var = load_options.(i)
		readf, load_lun, var
		load_options.(i) = var
	endfor
endif else load_options = options
options = load_options
; update table labels
table_labelupdate
if options.rowno lt 20 then table = strarr(n_elements(table_labels), 20) else table = strarr(n_elements(table_labels), options.rowno)
if options.rowno gt 0 then begin
	readf, load_lun, temp ; ''
	readf, load_lun, temp ; 'Table'
	if temp eq 'Table' then begin
		for i = 0, (options.rowno - 1) do begin
			for j = 0, (n_elements(table[*,0]) - 1) do begin
				readf, load_lun, temp
				table[j,i] = temp
			endfor
		endfor
	endif
endif
close, load_lun
free_lun, load_lun
options.rowno = n_elements(table[0,*])
endif else begin
load_exit:
table = strarr(n_elements(table_labels), 20)
options.rowno = 20
endelse
return, table
end


; create plot window
pro plot_create
common global, widget_id, options

if widget_id.plot eq 0 then begin
if options.plotgraph ne 0 then begin
widget_id.plot = widget_base(/base_align_center, /col, group_leader = widget_id.main, title = 'Cosmos')
widget_id.plot_area = widget_draw(widget_id.plot, /frame, xsize = 700, ysize = 500, retain = 2)
panel1 = widget_base(widget_id.plot, /align_right, /row)
widget_id.plot_close = widget_button(panel1, value = ' Freeze ')

widget_control, widget_id.plot, /realize

widget_control, widget_id.plot_area, get_value = win_id
wset, win_id
xyouts, 200, 300, 'Waiting for data...', /device

xmanager, 'Cosmos', widget_id.plot, cleanup = 'ev_winclose', event_handler = 'ev_poll', /no_block
endif

endif else begin
if options.plotgraph ne 0 then begin
widget_control, widget_id.plot_close, set_value = ' Freeze '
widget_control, widget_id.plot, show = 1
endif else widget_control, widget_id.plot, /destroy
endelse

return
end


; reads from raw data files (specified by array of strings), returning
;  summed detector counts (int array (x, t))
;  parameters :
;   params.monitor : summed monitor counts (long) which is 0 if no files found
;   params.runtime : summed runtime (seconds) (float)
;   params.dan : detector angle (dan) (degrees) (float)
;   params.san : sample angle (san) (degrees) float)
;   params.size_x : number of pixels in x direction (long)
;   params.size_t : number of time channels (long)
;   params.x_min : lower limit of pixels used
;   params.x_max : upper limit of pixels used
;   params.tofd : time of flight distance (float)
;   params.delay : time delay (float)
;   params.channelwidth : channel width (float)
;   params.pixeldensity : pixel density
pro raw_read, files, counts, params
common global, widget_id, options
common analysis, constants, anal

flag_defined = 0
for f = 0, n_elements(files) - 1 do begin
on_ioerror, raw_read_exit1
if files[f] ne '' then begin
if (str_make(long(files[f])) eq files[f]) and (strlen(files[f]) lt 6) then begin
	file_name = '000000'
	strput, file_name, files[f], 6 - strlen(files[f])
endif else begin
raw_read_exit1:
	file_name = files[f]
endelse
file_name = options.path + file_name

on_ioerror, raw_read_exit2

openr, raw_lun, file_name, error = tempval, /get_lun
if tempval eq 0 then begin
dialogue_add, ' Reading ' + file_name
tempstr = ''
; read in 34 lines of text
for i = 1, 34 do readf, raw_lun, tempstr
; read 1st set of parameters
par1 = fltarr(128)
readf, raw_lun, par1
; read 2 line spacer
for i = 1, 2 do readf, raw_lun, tempstr
; read 2nd set of parameters
par2 = fltarr(256)
readf, raw_lun, par2
; useful parameter information :
;  params1[ ] :
;   2 : run time (in tenths of a second)
;   4 : total monitor counts
;   94 : number of channels
;   95 : channel width (micro seconds)
;   96 : tof delay (micro seconds)
;   97 : x1
;   98 : x2
;   99 : y1
;   100 : y2
;   101 : nx, detector grouping factor in x (ie nx = 1 => base detector has 286 pixels in x, nx = 2 => base detector has 144 pixels in x)
;   102 : ny, detector grouping factor in y (ny = 1 => 276 pixels)
;  params2[ ] :
;   2 : san, sample angle
;   15 : sample to detector distance (millimetres)
;   16 : dan, detector angle
;   40 : requested chopper 1 speed (revs / min)
;   41 : requested chopper 1 phase (degrees)
;   42 : requested chopper 2 speed
;   43 : requested chopper 2 phase
;   44 : actual chopper 1 speed
;   45 : actual chopper 1 phase
;   46 : actual chopper 2 speed
;   47 : actual chopper 2 phase
;   93 : slit value
;   95 : slit value
temppar = { parameters }
temppar.monitor = long(par1[4])
temppar.runtime = float(par1[2] / 10.)
temppar.dan = float(par2[16])
temppar.san = float(par2[2])
temppar.size_x = long(par1[98] - par1[97] + 1)
temppar.size_t = long(par1[94])
temppar.x_min = long(par1[97])
temppar.x_max = long(par1[98])
temppar.slits = (par2[93] + par2[95]) / 2.
temppar.tofd = float(par2[15] * 1e-3) + options.d0 - (constants.chopsep / 2.)
temppar.openangle = constants.chopwin - (par2[47] - par2[45]) - options.openoff
temppar.period = 60. / par2[44]
tempdelayangle = (options.poff - temppar.openangle) / 2.
temppar.delay = par1[96] * 1e-6 - (tempdelayangle / 360.) * temppar.period
temppar.channelwidth = par1[95] * 1e-6
temppar.pixeldensity = par1[101]
size_y = long(par1[100] - par1[99] + 1)
if flag_defined eq 0 then begin
	params = temppar
	params.monitor = 0l
	params.runtime = 0.
	counts = lonarr(temppar.size_x, temppar.size_t)
	flag_defined = 1
endif
; compare with old parameters and output warning if different
if abs(temppar.dan - params.dan) gt 0.1 then dialogue_warn, 'Variation in detector angle detected between files.'
if abs(temppar.san - params.san) gt 0.1 then dialogue_warn, 'Variation in sample angle detected between files.'
if (temppar.size_x ne params.size_x) or (temppar.size_t ne params.size_t) then dialogue_add, '  Error : Unable to read file. Incompatible data array dimensions.' else begin

; augment parameters
params.monitor = params.monitor + temppar.monitor
params.runtime = params.runtime + temppar.runtime
; read 3 line spacer
for i = 1, 3 do readf, raw_lun, tempstr
; read total number of pixels in detector
readf, raw_lun, tempvar
; construct detector array of specified size
if size_y le 1 then detarr = lonarr(params.size_x, params.size_t) else detarr = lonarr(size_y, params.size_x, params.size_t)
; read detector data
readf, raw_lun, detarr

; augment counts
if size_y le 1 then counts = counts + detarr else counts = counts + total(detarr, 1)

endelse
close, raw_lun
free_lun, raw_lun
endif
endif
raw_read_exit2 :
endfor

if flag_defined eq 0 then begin
	counts = fltarr(constants.pixels_x, constants.pixels_t)
	params = { parameters }
	params.monitor = 0
	params.runtime = 0.
	params.size_x = constants.pixels_x
	params.size_t = constants.pixels_t
endif
return
end


; parse 'theta' string, returning float between 0 and 360 or -1 for san (default), -2 for dan
function str_theta, string
case strlowcase(string) of
'' : value = -1.
'san' : value = -1.
'dan' : value = -2.
else : value = ((str_parse(string, /float) mod 360) + 360.) mod 360 ; ensure 0 < value < 360
endcase
return, value
end


; 2
; -----------------------------------------------------------------------------
; 3


; read direct beam files, using previously read values if any
pro direct_read, newfiles, cntdir, pardir, runno
common direct, direct1_files, direct2_files, direct3_files, direct1_array, direct2_array, direct3_array, direct1_params, direct2_params, direct3_params

case runno of
1 : temp2 = direct1_files
2 : temp2 = direct2_files
3 : temp2 = direct3_files
endcase
temp1 = where(newfiles ne temp2, count) ; compare existing direct files
if count gt 0 then begin ; read new direct files
	raw_read, newfiles, counts, params
	case runno of
	1 : begin
		direct1_files = newfiles
		direct1_array = counts
		direct1_params = params
	end
	2 : begin
		direct2_files = newfiles
		direct2_array = counts
		direct2_params = params
	end
	3 : begin
		direct3_files = newfiles
		direct3_array = counts
		direct3_params = params
	end
	endcase
endif
; return appropriate values
case runno of
1 : begin
	cntdir = direct1_array
	pardir = direct1_params
end
2 : begin
	cntdir = direct2_array
	pardir = direct2_params
end
3 : begin
	cntdir = direct3_array
	pardir = direct3_params
end
endcase

return
end


; 3
; -----------------------------------------------------------------------------
; 4


; analyze run, returning array (q, q res, reflectivity, reflectivity err) or -1 if error
function anal_run, direct, reflect, instr, theta, runno
common global, widget_id, options
common analysis, constants, anal
common water, water_file, water_array

; local variables :
;  tsize, xsize ; size of count arrays
;  trangedir, trangeref, xrangedir, xrangeref ; ranges for peak searching
;  xlim ; useful limits of detector
;  peakdir, peakref ; centre of peaks in x direction
;  fgdsizedir, fgdsizeref ; size of foreground window
;  fgddir, fgdref ; ranges of foreground
;  bgddir, bgdref ; arrays of background pixels
;  bgddirno, bgdrefno ; number of background pixels
;  angle_centre ; angle to centre of detector
;  angle_bragg ; Bragg angle

; read data
direct_read, direct, cntdir, pardir, runno
if (pardir.monitor le 0) and (pardir.runtime le 0) then begin
	dialogue_add, ' Error: No direct counts read. Unable to calculate for run ' + str_make(runno) + '.'
	return, -1
endif
raw_read, reflect, cntref, parref
raw_read, instr, cntins, parins
; detector size consistency
if (parref.monitor le 0) and (parref.runtime le 0) then begin ; assume direct beam only required
	dialogue_warn, 'No reflect beam read (run ' + str_make(runno) + '). Calculating for direct beam only...'
	cntref = cntdir
	cntdir = fltarr(n_elements(cntref[*, 0]), n_elements(cntref[0, *]))
	cntins = fltarr(n_elements(cntref[*, 0]), n_elements(cntref[0, *]))
	parref = pardir
endif
tsize = n_elements(cntref[0, *])
xsize = n_elements(cntref[*, 0])
if n_elements(cntdir) ne n_elements(cntref) then begin
	dialogue_add, ' Warning : Incompatible size of data array for direct runs. Resizing...'
	cntdir = congrid(cntdir, xsize, tsize, /minus_one, /interp)
endif
if n_elements(cntref) ne n_elements(cntins) then begin
	if parins.monitor gt 0 then dialogue_add, ' Warning : Incompatible size of data array for instrument background runs. Resizing...'
	cntins = congrid(cntins, xsize, tsize, /minus_one, /interp)
endif

; errors (arrays of floats formed)
cntdir = float(cntdir)
cntref = float(cntref)
cntins = float(cntins)
cntdir_err = sqrt(cntdir + 1.)
cntref_err = sqrt(cntref + 1.)
cntins_err = sqrt(cntins + 1.)

; normalise to direct beam
if options.normalise eq 0 then begin ; runtime
	if float(pardir.runtime) le 0. then begin
		dialogue_warn, 'Unable to normalise. Direct beam runtime read as ' + str_make(pardir.runtime) + '.'
		temp1 = 1.
		temp2 = 1.
	endif else begin
		temp1 = parref.runtime / float(pardir.runtime)
		temp2 = parins.runtime / float(pardir.runtime)
	endelse
endif else begin ; monitor
	if float(pardir.monitor) le 0. then begin
		dialogue_warn, 'Unable to normalise. Direct beam monitor read as ' + str_make(pardir.monitor) + '.'
		temp1 = 1.
		temp2 = 1.
	endif else begin
		temp1 = parref.monitor / float(pardir.monitor)
		temp2 = parins.monitor / float(pardir.monitor)
	endelse
endelse
cntref = cntref / temp1
cntref_err = cntref_err / temp1
if (parins.monitor gt 0) or (parins.runtime gt 0) then begin
	cntins = cntins / temp2
	cntins_err = cntins_err / temp2
	; subtract instrument backgrounds
	cntref = cntref - cntins
	cntref_err = sqrt(cntref_err^2 + cntins_err^2) ; add errors in quadrature
	cntdir = cntdir - cntins
	cntdir_err = sqrt(cntdir_err^2 + cntins_err^2)
endif

; water correction
for i = 0, tsize - 1 do begin
cntdir[*, i] = cntdir[*, i] / water_array[pardir.x_min:pardir.x_max]
cntdir_err[*, i] = cntdir_err[*, i] / water_array[pardir.x_min:pardir.x_max]
cntref[*, i] = cntref[*, i] / water_array[pardir.x_min:pardir.x_max]
cntref_err[*, i] = cntref_err[*, i] / water_array[pardir.x_min:pardir.x_max]
endfor

; useful limits
xlim = [ (anal.usefulx[0] - pardir.x_min) > 0L, (anal.usefulx[1] - pardir.x_min) < (xsize - 1) ] ; assumes x_min/max same for direct and reflect runs

; search for peak
if anal.searchy[1] lt 0 then begin ; 'auto' option
	; range as default in previous version of d17tof
	trangedir = [ tsize / 2, tsize - 1 ]
	trangeref = [ tsize / 2, tsize - 1 ]
	; optional code to use for peak search in time:
	; trangedir = anal_peaksearchbasic(total(cntdir, 1)) ; total counts as function of t
	; trangeref = anal_peaksearchbasic(total(cntref, 1))
	dialogue_add, ' Auto search : Search area for peak in time channels found as :'
	dialogue_add, '  Direct beam  : ' + str_make(trangedir[0]) + ' to ' + str_make(trangedir[1])
	dialogue_add, '  Reflect beam : ' + str_make(trangeref[0]) + ' to ' + str_make(trangeref[1])
endif else begin
	trangedir = anal.searchy
	trangeref = anal.searchy
endelse
if anal.searchx[1] lt 0 then begin ; 'auto' option
	xrangedir = anal_peaksearchbasic(total(cntdir, 2)) ; total counts as function of x
	xrangeref = anal_peaksearchbasic(total(cntref, 2))
	dialogue_add, ' Auto search : Search area for peak in x (pixels) found as :'
	dialogue_add, '  Direct beam  : ' + str_make(xrangedir[0]) + ' to ' + str_make(xrangedir[1])
	dialogue_add, '  Reflect beam : ' + str_make(xrangeref[0]) + ' to ' + str_make(xrangeref[1])
endif else begin
	xrangedir = anal.searchx
	xrangeref = anal.searchx
endelse
xrangedir = [ xrangedir[0] > xlim[0], xrangedir[1] < xlim[1] ]
xrangeref = [ xrangeref[0] > xlim[0], xrangeref[1] < xlim[1] ]

; check foreground window size
if anal.foregd[0] le 0 then begin ; 'auto' foreground
	temp1 = anal_peaksearchbasic(total(cntdir, 2))
	fgdsizedir = temp1[1] - temp1[0]
	temp1 = anal_peaksearchbasic(total(cntref, 2))
	fgdsizeref = temp1[1] - temp1[0]
endif else begin
	fgdsizedir = anal.foregd[0]
	fgdsizeref = anal.foregd[0]
endelse

; refine search in x direction
peakdir = anal_peaksearchrefined(total(cntdir[*, trangedir[0]:trangedir[1]], 2), xrangedir, fgdsizedir)
peakref = anal_peaksearchrefined(total(cntref[*, trangeref[0]:trangeref[1]], 2), xrangeref, fgdsizeref)

; foreground and background ranges
fgddir = lonarr(2)
fgddir[0:1] = [ (peakdir - fix((fgdsizedir - 1) / 2.)) > xlim[0], (peakdir + fix((fgdsizedir - 1) / 2.)) < xlim[1] ]
fgdref = lonarr(2)
fgdref[0:1] = [ (peakref - fix((fgdsizeref - 1) / 2.)) > xlim[0], (peakref + fix((fgdsizeref - 1) / 2.)) < xlim[1] ]
bgddir = intarr(xsize) ; mask for background
bgdref = intarr(xsize)
if (anal.backgdleft[0] le 0L) and (anal.backgdleft[1] lt 0L) then begin ; 'auto' left background
	if fgddir[0] gt xlim[0] then bgddir[xlim[0] : fgddir[0] - 1] = 1
	if fgdref[0] gt xlim[0] then bgdref[xlim[0] : fgdref[0] - 1] = 1
endif else begin
	temp1 = fgddir[0] - (anal.backgdleft[1] > 0L) - 1
	if (temp1 ge xlim[0]) and (anal.backgdleft[0] gt 0) then bgddir[ (temp1 - anal.backgdleft[0] + 1) > xlim[0] : temp1 ] = 1
	temp1 = fgdref[0] - (anal.backgdleft[1] > 0L) - 1
	if (temp1 ge xlim[0]) and (anal.backgdleft[0] gt 0) then bgdref[ (temp1 - anal.backgdleft[0] + 1) > xlim[0] : temp1 ] = 1
endelse
if (anal.backgdright[0] le 0L) and (anal.backgdright[1] lt 0L) then begin ; 'auto' right background
	if fgddir[1] lt xlim[1] then bgddir[fgddir[1] + 1 : xlim[1] ] = 1
	if fgdref[1] lt xlim[1] then bgdref[fgdref[1] + 1 : xlim[1] ] = 1
endif else begin
	temp1 = fgddir[1] + (anal.backgdright[1] > 0L) + 1
	if (temp1 le xlim[1]) and (anal.backgdright[0] gt 0) then bgddir[ temp1 : (temp1 + anal.backgdright[0] - 1) < xlim[1] ] = 1
	temp1 = fgdref[1] + (anal.backgdright[1] > 0L) + 1
	if (temp1 le xlim[1]) and (anal.backgdright[0] gt 0) then bgdref[ temp1 : (temp1 + anal.backgdright[0] - 1) < xlim[1] ] = 1
endelse

; background arrays
bgddir = where(bgddir eq 1, bgddirno)
if bgddirno le 0 then dialogue_warn, 'No background to subtract on direct runs.'
bgdref = where(bgdref eq 1, bgdrefno)
if bgdrefno le 0 then dialogue_warn, 'No background to subtract on reflect runs.'

; output information
dialogue_add, ' Beam sum ranges :' + string(9b) + 'Direct beam' + string(9b) + 'Reflect beam'
dialogue_add, '  Foreground :' + string([9b, 9b]) + str_make(fgddir[0]) + ' ' + str_make(fgddir[1]) + string([9b, 9b]) + str_make(fgdref[0]) + ' ' + str_make(fgdref[1])
dialogue_add, '  Total background :' + string(9b) + str_make(bgddirno) + string([9b, 9b]) + str_make(bgdrefno)

; subtract backgrounds
if options.backmeth eq 0 then begin ; 'average'
if bgddirno gt 0 then anal_backgroundaverage, cntdir, cntdir_err, bgddir
if bgdrefno gt 0 then anal_backgroundaverage, cntref, cntref_err, bgdref
endif else begin ; 'fit'
if bgddirno gt 0 then anal_backgroundfit, cntdir, cntdir_err, bgddir, 'direct'
if bgdrefno gt 0 then anal_backgroundfit, cntref, cntref_err, bgdref, 'reflect'
endelse

; sum over x
cntdir = total(cntdir[fgddir[0] : fgddir[1], *], 1)
cntdir_err = sqrt(total(cntdir_err[fgddir[0] : fgddir[1], *]^2, 1)) ; add errors in quadrature
cntref = total(cntref[fgdref[0] : fgdref[1], *], 1)
cntref_err = sqrt(total(cntref_err[fgdref[0] : fgdref[1], *]^2, 1))

; calculate angle
case theta of
-1. : angle_bragg = parref.san * !pi / 180. ; convert to radians
-2. : begin
	angle_centre = ((parref.dan + pardir.dan) / 2.) * !pi / 180.
	angle_bragg = anal_correctangle(angle_centre, peakdir, peakref, pardir.x_min, parref.x_min, parref.pixeldensity, pardir.tofd, parref.tofd)
	dialogue_add, ' Angle to centre of detector found as ' + str_make(angle_centre * 180. / !pi)
end
else : angle_bragg = theta
endcase
dialogue_add, ' Bragg angle to centre of peak calculated as ' + str_make(angle_bragg * 180. / !pi)

; form lambda[t] (Angstroms) array from reflect runs
temp1 = anal_correctdistance(peakref, parref.x_min, parref.pixeldensity, parref.tofd)
temp2 = abs(temp1 - anal_correctdistance(peakdir, pardir.x_min, pardir.pixeldensity, pardir.tofd))
dialogue_add, ' Difference in corrected TOF distance between direct and reflect beams is ' + str_make(temp2)
if (temp2 / temp1) gt 0.01 then dialogue_warn, ' Row ' + str_make(anal.row) + ': Run no. ' + str_make(runno) + ': Different TOF distances from direct and reflect runs.'
lambda = 1e10 * (constants.planckperkg * ((findgen(tsize) + 0.5) * parref.channelwidth + parref.delay) / temp1)

; write lambda[t] array to file
temp1 = 0
openw, lambda_lun, 'cosmos_lambdaconv' + str_make(runno) + '.dat', error = temp1, /get_lun
if temp1 eq 0 then begin
on_ioerror, lambda_exit
printf, lambda_lun, 'Time Pixel,   Wavelength (Angstroms)'
for i = 0, n_elements(lambda) - 1 do printf, lambda_lun, i, ',', lambda[i]
close, lambda_lun
free_lun, lambda_lun
endif
lambda_exit:

if anal.lambda[1] eq 0 then lambda_range = where(lambda gt 0., result_size) else lambda_range = where((lambda gt anal.lambda[0]) and (lambda lt anal.lambda[1]), result_size)

; calculate q and reflectivity
if result_size gt 0 then begin
err_res = (constants.chopsep + (constants.planckperkg * parref.openangle * parref.period / (360. * lambda[lambda_range] * 1.e-10))) / parref.tofd ; wavelength resolution
err_ray = 2. * atan(parref.slits / constants.interslit) / angle_bragg ; angular resolution
result = fltarr(4, result_size)
result[0, *] = 4. * !pi * sin(angle_bragg) / lambda[lambda_range]
result[1, *] = result[0, *] * sqrt(err_res^2 + err_ray^2)
if max(cntdir[lambda_range]) le 0. then begin
	result[2, *] = cntref[lambda_range]
	result[3, *] = cntref_err[lambda_range]
endif else begin
	result[2, *] = cntref[lambda_range] / cntdir[lambda_range]
	result[3, *] = abs(result[2, *]) * sqrt((cntref_err[lambda_range] / cntref[lambda_range])^2 + (cntdir_err[lambda_range] / cntdir[lambda_range])^2)
endelse
; tidy floating point errors
temp1 = where((finite(result[2, *]) eq 1) and (finite(result[3, *]) eq 1), temp2)
if temp2 gt 0 then result = result[*, temp1] else result = -1
endif else result = -1
return, result
end


; reset requested
pro ev_reset
common global, widget_id, options
common table_head, table_labels

; load variables
table = load_vars()

; update table
widget_control, widget_id.table, column_labels = table_labels, table_xsize = n_elements(table_labels)
widget_control, widget_id.table, set_value = table, table_ysize = n_elements(table[0, *])

; update fields of main window
widget_control, widget_id.datapath, set_value = options.path
widget_control, widget_id.waterrun, set_value = options.waterrun

; close open subwindows
close_subwins

end


; 4
; -----------------------------------------------------------------------------
; 5


; do calculation in background
pro ev_background
common global, widget_id, options
common table_head, table_labels
common analysis, constants, anal
common water, water_file, water_array
common waits, waittime

widget_control, widget_id.table, get_value = table
table = strcompress(table, /remove_all)

if (anal.quit eq 0) and (anal.row lt n_elements(table[0, *])) then begin

dialogue_add, 'Analysing row ' + str_make(anal.row) + '...'
direct3 = strarr(1)
reflect3 = strarr(1)
instr1 = strarr(1)
instr2 = strarr(1)
instr3 = strarr(1)
factor2 = fltarr(1)
theta3 = 0.
for col = 0, n_elements(table[*, 0]) - 1 do begin
case table_labels[col] of
'Direct 1' : direct1 = str_parse(table[col, anal.row], /string_set)
'Direct 2' : direct2 = str_parse(table[col, anal.row], /string_set)
'Direct 3' : direct3 = str_parse(table[col, anal.row], /string_set)
'Reflect 1' : reflect1 = str_parse(table[col, anal.row], /string_set)
'Reflect 2' : reflect2 = str_parse(table[col, anal.row], /string_set)
'Reflect 3' : reflect3 = str_parse(table[col, anal.row], /string_set)
'Instr 1' : instr1 = str_parse(table[col, anal.row], /string_set)
'Instr 2' : instr2 = str_parse(table[col, anal.row], /string_set)
'Instr 3' : instr3 = str_parse(table[col, anal.row], /string_set)
'Factor' : factor1 = str_parse(table[col, anal.row], /float)
'Factor 1-2' : factor1 = str_parse(table[col, anal.row], /float)
'Factor 2-3' : factor2 = str_parse(table[col, anal.row], /float)
'Theta 1' : theta1 = str_theta(table[col, anal.row])
'Theta 2' : theta2 = str_theta(table[col, anal.row])
'Theta 3' : theta3 = str_theta(table[col, anal.row])
'Out file' : begin
	outfile = str_parse(table[col, anal.row], /string_set)
	outfile = outfile[n_elements(outfile) - 1]
end
'Out LAMP' : outlamp = str_parse(table[col, anal.row], /integer)
else :
endcase
endfor

; produce [ qmin, qmax, reflectivity min, reflectivity max ] arrays
if direct1[0] ne '' then res1 = anal_run(direct1, reflect1, instr1, theta1, 1) else res1 = -1
if direct2[0] ne '' then res2 = anal_run(direct2, reflect2, instr2, theta2, 2) else res2 = -1
if direct3[0] ne '' then res3 = anal_run(direct3, reflect3, instr3, theta3, 3) else res3 = -1

rescheck1 = n_elements(res1) gt 1
rescheck2 = n_elements(res2) gt 1
rescheck3 = n_elements(res3) gt 1
factor1 = factor1[0]
factor2 = factor2[0]
; match results
if rescheck1 and rescheck2 then begin
	if factor1 eq 0. then begin
		temp1 = anal_factor(res1, res2)
		if temp1[0] eq 0. then begin
			factor1 = 1.
			dialogue_warn, 'No overlap between run 1 and run 2'
		endif else begin
			factor1 = 1. / mean(temp1)
			dialogue_add, ' Auto factor 1-2 found as ' + str_make(factor1)
		endelse
	endif
endif
if rescheck2 and rescheck3 then begin
	if factor2 eq 0. then begin
		temp1 = anal_factor(res2, res3)
		if temp1[0] eq 0. then begin
			factor2 = 1.
			dialogue_warn, 'No overlap between run 2 and run 3'
		endif else begin
			factor2 = 1. / mean(temp1)
			dialogue_add, ' Auto factor 2-3 found as ' + str_make(factor2)
		endelse
	endif
endif
if rescheck1 and rescheck3 then begin
	if (factor1 * factor2) eq 0. then begin
		temp1 = anal_factor(res1, res3)
		factor2 = 1.
		if temp1[0] eq 0. then begin
			factor1 = 1.
			dialogue_warn, 'No overlap between run 1 and run 3'
		endif else begin
			factor1 = 1. / mean(temp1)
			dialogue_add, ' Auto factor 1-3 found as ' + str_make(factor1)
		endelse
	endif
endif
case (ishft(rescheck1, 2) + ishft(rescheck2, 1) + rescheck3) of
0 : result = -1
1 : result = res3
2 : result = res2
3 : begin
	res3[2:3, *] = res3[2:3, *] * factor2
	result = anal_append(res2, res3)
end
4 : result = res1
5 : begin
	res3[2:3, *] = res3[2:3, *] * factor1 * factor2
	result = anal_append(res1, res3)
end
6 : begin
	res2[2:3, *] = res2[2:3, *] * factor1
	result = anal_append(res1, res2)
end
7 : begin
	res2[2:3, *] = res2[2:3, *] * factor1
	res3[2:3, *] = res3[2:3, *] * factor1 * factor2
	result = anal_append(anal_append(res1, res2), res3)
end
else : result = -1
endcase

if n_elements(result) gt 1 then begin
result = anal_sort(result)

; write unbundled data to .out file
file_write, outfile, result, direct1, direct2, direct3, reflect1, reflect2, reflect3, instr1, instr2, instr3, theta1, theta2, theta3, water_file, /out

; group data
if options.group eq 0 then anal_bundle, result, options.groupvar

; plot
waitplot = plot_add(result, anal.row)

; write bundled data to file(s) and logged bundled data to lamp
if options.group ne 1 then begin
	file_write, outfile, result, direct1, direct2, direct3, reflect1, reflect2, reflect3, instr1, instr2, instr3, theta1, theta2, theta3, water_file, /aft
	file_write, outfile, result, direct1, direct2, direct3, reflect1, reflect2, reflect3, instr1, instr2, instr3, theta1, theta2, theta3, water_file, /dat
endif
pts = where((result[2, *] - result[3, *]) gt 0., temp1)
if temp1 gt 0 then begin
	resultlog = fltarr(4, temp1)
	resultlog[0, *] = reform(result[0, pts])
	resultlog[1, *] = reform(result[1, pts])
	resultlog[2, *] = alog10(reform(result[2, pts]))
	resultlog[3, *] = (alog10(reform(result[2, pts] + result[3, pts])) - alog10(reform(result[2, pts] - result[3, pts]))) / 2.
	lamp_write, outlamp[0], resultlog, anal.row
endif else dialogue_warn, 'Logged dataset empty.'

endif else waitplot = waittime.none

anal.row = anal.row + 1
if widget_id.dialogue ne 0 then waitval = waittime.short > waitplot else waitval = waittime.none > waitplot
widget_control, widget_id.go, timer = waitval

endif else begin

if anal.quit ne 0 then dialogue_add, 'Interrupted at row ' + str_make(anal.row - 1) else dialogue_add, 'End of calculation.'

dialogue_finish
plot_finish
widget_control, widget_id.main, sensitive = 1

endelse

; clear math errors
temp1 = check_math()

end


; setup calculation
pro ev_go
common global, widget_id, options
common table_head, table_labels
common analysis, constants, anal
common water, water_file, water_array
common direct, direct1_files, direct2_files, direct3_files, direct1_array, direct2_array, direct3_array, direct1_params, direct2_params, direct3_params
common waits, waittime

close_subwins
main_getvars
widget_control, widget_id.main, sensitive = 0
plot_create
dialogue_create
anal.quit = 0
; reset direct read files (file path may have changed)
direct1_files = strarr(1)
direct2_files = strarr(1)
direct3_files = strarr(1)

; parse detector ranges
anal.usefulx = str_parse(options.usefulx, /int_range)
if anal.usefulx[0] lt 0L then anal.usefulx[0] = 0L ; lower limit of useful pixels in x direction of detector
if (anal.usefulx[1] gt (constants.pixels_x - 1)) or (anal.usefulx[1] le 0L) then anal.usefulx[1] = (constants.pixels_x - 1) ; upper limit of useful pixels in y direction of detector
anal.usefulx = anal.usefulx[sort(anal.usefulx)]
dialogue_add, 'Useful range in x read as ' + str_make(anal.usefulx[0]) + ' to ' + str_make(anal.usefulx[1])
anal.searchx = str_parse(options.searchx, /int_range)
if anal.searchx[0] lt anal.usefulx[0] then anal.searchx[0] = anal.usefulx[0]
if (anal.searchx[1] gt anal.usefulx[1]) then anal.searchx[1] = anal.usefulx[1]
if anal.searchx[1] gt 0L then begin
	anal.searchx = anal.searchx[sort(anal.searchx)]
	dialogue_add, 'Search range in x read as ' + str_make(anal.searchx[0]) + ' to ' + str_make(anal.searchx[1])
endif ; else 'auto' value -1 has been selected
anal.searchy = str_parse(options.searchy, /int_range)
if anal.searchy[0] lt 0L then anal.searchy[0] = 0L ; lower limit of pixels to search for peak in detector time
if anal.searchy[1] gt (constants.pixels_t - 1) then anal.searchy[1] = (constants.pixels_t - 1) ; upper limit of pixels to search for peak in detector time
if anal.searchy[1] gt 0L then begin
	anal.searchy = anal.searchy[sort(anal.searchy)]
	dialogue_add, 'Search range in y read as ' + str_make(anal.searchy[0]) + ' to ' + str_make(anal.searchy[1])
endif ; else 'auto' value -1 has been selected

; parse calculation ranges
anal.foregd = str_parse(options.foregd, /integer)
if anal.foregd[0] lt 0L then anal.foregd[0] = 0L ; width of foreground (0 is default)
dialogue_add, 'Width of foreground read as ' + str_make(anal.foregd[0])
anal.backgdleft = str_parse(options.backgdleft, /int_range)
if anal.backgdleft[0] lt 0L then anal.backgdleft[0] = 0L ; width of background on left of peak, anal.backgdleft[1] is shift in background (-1 implies no shift), backgd = [0, 0] implies no background
anal.backgdright = str_parse(options.backgdright, /int_range)
if anal.backgdright[0] lt 0L then anal.backgdright[0] = 0L ; width of background on right of peak
dialogue_add, 'Background read as : ' + string(9b) + 'left' + string(9b) + 'right'
dialogue_add, string(9b) + 'Width : ' + string(9b) + str_make(anal.backgdleft[0]) + string(9b) + str_make(anal.backgdright[0])
dialogue_add, string(9b) + 'Shift : ' + string(9b) + str_make(anal.backgdleft[1]) + string(9b) + str_make(anal.backgdright[1])
anal.lambda = str_parse(options.lambda, /flt_range)
if anal.lambda[0] lt 0. then anal.lambda[0] = 0. ; lower limit of wavelength window
if anal.lambda[1] lt 0. then anal.lambda[1] = 0. ; upper limit of wavelength window (0 is no limit)
anal.lambda = anal.lambda[sort(anal.lambda)]
dialogue_add, 'Wavelength range read as ' + str_make(anal.lambda[0]) + ' to ' + str_make(anal.lambda[1])

; water run(s)
temp2 = str_parse(options.waterrun, /string_set)
temp1 = where(temp2 ne water_file, temp3) ; compare existing water file
if temp3 gt 0 then begin
	raw_read, temp2, counts, params
	if params.runtime gt 0 then begin
		water_file = temp2
		dialogue_add, 'Using ' + water_file + ' as water file'
		; total over t and store in water array
		water_array = fltarr(constants.pixels_x)
		water_array[params.x_min : params.x_max] = total(counts, 2)
		water_array[0 : anal.usefulx[0] - 1] = 1.
		water_array[anal.usefulx[0] : anal.usefulx[1]] = array_norm(water_array[anal.usefulx[0] : anal.usefulx[1]])
		water_array[anal.usefulx[1] + 1 : constants.pixels_x - 1] = 1.
		if min(water_array) le 0. then begin
			dialogue_warn, 'Zero efficiency pixels detected in water file.'
			dialogue_add, 'Please redefine useful detector range.'
			water_file = strarr(1)
			anal.quit = 1
		endif
	endif
endif

; clear math errors
temp1 = check_math()

anal.row = 0L
widget_control, widget_id.go, timer = waittime.none

end


; 5
; -----------------------------------------------------------------------------
; 6


; handle gui events
pro ev_poll, event
common global, widget_id, options
common analysis, constants, anal

case event.id of
; main window events
widget_id.columns : ev_opencolumnwindow
widget_id.filldown : ev_filldown
widget_id.fillinc : ev_filldown
widget_id.clear : ev_clear
widget_id.insertrow : ev_insertrow
widget_id.insertno : ev_insertrow
widget_id.calopt : ev_opencalcwindow
widget_id.detopt : ev_opendetectwindow
widget_id.macopt : ev_openmachwindow
widget_id.save : ev_save
widget_id.reset : ev_reset
widget_id.quit : ev_quit
widget_id.go : if strlowcase(tag_names(event, /structure_name)) eq 'widget_timer' then ev_background else ev_go
; calculation window events
widget_id.calc_cancel : widget_control, widget_id.calc, /destroy
widget_id.calc_ok : ev_updatecalc
; detection window events
widget_id.detect_cancel : widget_control, widget_id.detect, /destroy
widget_id.detect_ok : ev_updatedetect
; machine window events
widget_id.mach_cancel : widget_control, widget_id.mach, /destroy
widget_id.mach_ok : ev_updatemach
; column setup window events
widget_id.column_cancel : widget_control, widget_id.column, /destroy
widget_id.column_ok : ev_updatecolumns
; dialogue window events
widget_id.dialogue_cancel : begin
	anal.quit = 1
	widget_control, widget_id.dialogue_cancel, /clear_events
end
widget_id.dialogue_ok : widget_control, widget_id.dialogue, /destroy
; plot window events
widget_id.plot_close : ev_plotevent
else :
endcase

end


; 6
; -----------------------------------------------------------------------------


; initialise - note this procedure must be at end of file so above functions are compiled.
pro cosmos

; initialise variables
init_vars

common global, widget_id, options
common table_head, table_labels

table = load_vars()

; create main window
widget_id.main = widget_base(/base_align_center, /col, title = 'Cosmos')
; path fields
panel1 = widget_base(widget_id.main, /row)
widget_id.datapath = cw_field(panel1, /frame, title = 'Data Path: ', value = options.path, xsize = 12)
temp = widget_base(panel1, xsize = 100)
widget_id.waterrun = cw_field(panel1, /frame, title = 'Water Runs: ', value = options.waterrun, xsize = 12)
temp = widget_base(panel1, xsize = 130)
widget_id.columns = widget_button(panel1, /frame, value  = '  Setup Columns  ')
; panel for table and toolbar
panel1 = widget_base(widget_id.main, /base_align_center, /col, /frame)
; toolbar
panel2 = widget_base(panel1, col = 3)
panel3 = widget_base(panel2, /align_left, /frame, /row)
	widget_id.filldown = widget_button(panel3, value = '  Fill Down  ')
	widget_id.fillinc = cw_field(panel3, /floating, /return_events, title = 'Increment:', value = 0., xsize = 8)
panel3 = widget_base(panel2, xsize = 140)
panel3 = widget_base(panel2, /align_right, /frame, /row)
	widget_id.clear = widget_button(panel3, value =   '    Clear    ')
	panel4 = widget_base(panel3, xsize = 32)
	widget_id.insertrow = widget_button(panel3, value = ' Insert Rows ')
	widget_id.insertno = cw_field(panel3, /long, /return_events, title = 'No:', value = 0L, xsize = 8)
; table
widget_id.table = widget_table(panel1, alignment = 2, column_labels = table_labels, /editable, /frame, /resizeable_columns, /scroll, value = table, xsize = n_elements(table_labels), x_scroll_size = 9, y_scroll_size = 12, ysize = options.rowno)
; panel for action buttons
panel1 = widget_base(widget_id.main, col = 3)
panel2 = widget_base(panel1, /align_left, /frame, /grid_layout, /row)
widget_id.calopt = widget_button(panel2, value = ' Calculation ' + string(10b) + ' Options ')
widget_id.detopt = widget_button(panel2, value = ' Detector ' + string(10b) + ' Ranges ')
widget_id.macopt = widget_button(panel2, value = ' Machine ' + string(10b) + ' Options ')
temp = widget_base(panel1, xsize = 200)
panel2 = widget_base(panel1, /align_right, /frame, /grid_layout, /row)
widget_id.save = widget_button(panel2, value = ' Save ')
widget_id.reset = widget_button(panel2, value = ' Reset ')
widget_id.quit = widget_button(panel2, value = ' Quit ')
widget_id.go = widget_button(panel2, value = ' Go ')

; open main window
widget_control, widget_id.main, /realize
xmanager, 'Cosmos', widget_id.main, event_handler = 'ev_poll', /no_block

end
