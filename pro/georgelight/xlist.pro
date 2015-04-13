;-------------------------------------------------------------
;+
; NAME:
;       XLIST
; PURPOSE:
;       Pop-up list selection widget.
; CATEGORY:
; CALLING SEQUENCE:
;       out = xlist(list)
; INPUTS:
;       list = string array of possible selections.  in
; KEYWORD PARAMETERS:
;       Keywords:
;         TITLE=txt  title text or text array (def=Select item).
;         MAXSCROLL=n Max allowed lines before scrolling list used
;           (def=20).
;         HIGHLIGHT=i Line to highlight (def=none).
;         TOP=j       Line to make be the top of the list.
;         INDEX=indx  Returned index of selected item.
;         /WAIT  means wait for a selection before returning.
;           Needed if called from another widget routine.
; OUTPUTS:
;       out = selected element.                      out
;         Null if Cancel button pressed.
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner, 11 Nov, 1993
;
; Copyright (C) 1993, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	pro xlist_event, ev
 
	widget_control, ev.id, get_uval=cmd	; Get command (or list entry).
	widget_control, ev.top, get_uval=res	; Get result address.
 
	if cmd(0) eq 'CANCEL' then begin	; CANCEL button.
	  widget_control, res, set_uval={t:'',i:0}	; Return null string.
	  widget_control, ev.top, /dest		; Destroy list widget.
	  return
	endif
 
	txt = cmd(ev.index)			; Selected list entry.
	widget_control, res, set_uval={t:txt,i:ev.index}	; Return it.
	widget_control, ev.top, /dest		; Destroy list widget.
	return
 
	end 
 
 
;===================================================================
;	xlist.pro = Pop-up list selection widget.
;	R. Sterner, 11 Nov, 1993
;===================================================================
 
	function xlist, list, title=title, help=hlp, maxscroll=maxs, $
	  index=indx, wait=wait, highlight=hi, top=ltop
 
	if (n_params(0) eq 0) or keyword_set(hlp) then begin
	  print,' Pop-up list selection widget.'
	  print,' out = xlist(list)'
	  print,'   list = string array of possible selections.  in'
	  print,'   out = selected element.                      out' 
	  print,'     Null if Cancel button pressed.'
	  print,' Keywords:'
	  print,'   TITLE=txt  title text or text array (def=Select item).'
	  print,'   MAXSCROLL=n Max allowed lines before scrolling list used'
	  print,'     (def=20).'
	  print,'   HIGHLIGHT=i Line to highlight (def=none).'
	  print,'   TOP=j       Line to make be the top of the list.'
	  print,'   INDEX=indx  Returned index of selected item.'
	  print,'   /WAIT  means wait for a selection before returning.'
	  print,'     Needed if called from another widget routine.'
	  return,''
	endif
 
	;--------  Set defaults  ------------
	if n_elements(maxs) eq 0 then maxs=20
	if n_elements(title) eq 0 then title = 'Select item'
 
	;--------  Set up widget  ----------------
	result = widget_base(modal=wait)
	widget_control, result, set_uval=''
	top = widget_base(/column, uvalue=result, title=' ')
	for i=0, n_elements(title)-1 do t = widget_label(top,val=title(i))
	b = widget_base(top, /row)
	t = widget_button(b, val='Cancel', uval='CANCEL')
	lst = widget_list(top, val=list, uval=list,ysize=n_elements(list)<maxs)
	widget_control, top, /real
	if n_elements(hi) ne 0 then widget_control,lst,set_list_select=hi
	if n_elements(ltop) ne 0 then widget_control,lst,set_list_top=ltop
 
	;--------- Register  ---------
	if n_elements(wait) eq 0 then wait=0
	xmanager, 'xlist', top
	widget_control, result, get_uvalue=out
  if size(out, /type) ne 8 then return, ''
	indx = out.i
	txt = out.t
 
	return, txt
	end
