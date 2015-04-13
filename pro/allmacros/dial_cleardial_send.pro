FUNCTION dial_cleardial_send, dummy1,dummy2, text, button
;*******
;**
;** Called from the Pad, generaly to start and stop Dials

Dials=strlowcase(strtrim(str_sep(text(0),'~'),2))

FOR i=0,n_elements(Dials)-1 DO BEGIN
    Dialstop ,Dials(i)
    Dialclear,Dials(i)
ENDFOR

return,0
end
