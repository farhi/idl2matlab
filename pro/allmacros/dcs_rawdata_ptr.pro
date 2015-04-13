; Written by J.R.D. Copley.  Last revision 07/29/01.
;************************************************************************************************
pro dcs_rawdata_ptr,datafile_ptr=datafile_ptr
;************************************************************************************************
;
compile_opt strictarr
;
datafile_ptr=ptr_new({rawdata,$
	badbrd:-1.0d,$
	baddet:-1.0d,$
	ch_bid:fltarr(7)+1.e4,$
	ch_delay:fltarr(7)-1.0d,$
	ch_dis:fltarr(7)+1.e4,$
	ch_input:fltarr(6)-1.0d,$
	ch_ms:-1.0d,$
	ch_phase:fltarr(7)-1.0d,$
	ch_res:-1,$
	ch_slots:fltarr(7,3)-1.0d,$
	ch_srdenom:-1,$
	ch_srmode:-1,$
	ch_wl:-1.0d,$
	coll_amp:-1.0d,$
	coll_mean:-1.0d,$
	coll_osc:-1,$
	command:"xxx",$
	comments:"xxx",$
	datamax:-1.0d,$
	det_dis:+1.d4,$;
	detsum:ulonarr(931)-1,$
	duration:-1.0d,$
	fc_dis:fltarr(3)+1.e4,$
	grandsum:ulonarr(8)-1,$
	highmax:-1.0d,$
	highsource:strarr(17)+"xxx",$
	histodata:ulonarr(1024,913)-1,$
	histohigh:ulonarr(1024,18)-1,$
	motor_pos:fltarr(8)+1.e4,$
	nframes:-1l,$
	repeats:ulonarr(2,32)-1,$
	resets:ulonarr(2,32)-1,$
	sample_desc:strarr(10)+"xxx",$
;	sample_geom:fltarr(10),$
;	sample_envi:fltarr(10),$
	start_date:"xxx",$
	startchoice:"xxx",$
	stop_date:"xxx",$
	tchanlook:ulonarr(2,1024)-1,$
	temp_control:fltarr(500)-1.0,$
	temp_sample:fltarr(500)-1.0,$
	temp_setpoint:-1.0d,$
	timsum:ulonarr(1024)-1,$
	totals:ulonarr(2,32)-1,$
	tsdmin:-1.0d},/no_copy)
;
end