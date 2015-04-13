pro error

a=fltarr(10)

catch, error_status

if Error_status ne 0 then begin

print, 'error', error_status
print, 'message', !err_string

a=fltarr(12)
endif
a(11)=12

end
