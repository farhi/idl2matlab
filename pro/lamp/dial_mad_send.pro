;HACKED by Mark to remove GEORGE dependencies


FUNCTION DIAL_MAD_SEND, DialType, Check, Commd, DialName
;******* *************
;**
common geoMad_sface, geoMadPth, geoMadmin
R=3.14
;**********
;First call
;**********
if n_elements(geoMadPth) eq 0 then begin geoMadPth='/users/mad/IDL_MAD_SERVER/ex1.so'
					 bid=FINDFILE(geoMadPth,count=nn)
					 if nn lt 1 then geoMadPth=''
					 geoMadmin=0.1 & endif                ; Minimum check waiting time
if n_elements(DialName)  ne 1 then DialName=''
;**********
;Send Commd
;**********
;Care with Check !!!!!!!!!!!!!!!!!!
;***************

IF  geoMadPth ne '' then  $
FOR i=0,n_elements(Commd)-1 do BEGIN
    if R ge 0 then begin
      ;print,'Sending: '+Commd(i)
       R= CALL_EXTERNAL(geoMadPth,'put_mad_command',Commd(i))                 ; Send the command
      ;print,'Returned status from Mad-Idl:',R
    endif
    if R ge 0 then $
    if Check gt 0 then begin chk=Check > geoMadmin & V=-1
			 if DialType eq 'PAD' then   V= 0 $                   ; At this time, no wait from the Pad !
					      else wait,chk < geoMadmin       ; Get Mad running
			 while (V ne 0) do begin
			     V=DIAL_MAD_READ('flagus')
			     if DialName gt ' ' then $
			     if not DialOn(0) then begin R=3.14 & V=0 & endif ; User stopped the Dial !
			     if V lt 0        then begin R=-1   & V=0 & endif ; Mad is not running  !
			     if V ne 0        then wait, chk                  ; wait if not Idle  !
			 endwhile
		   endif
ENDFOR
return, R
end
