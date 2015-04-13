FUNCTION mib_group,w_in
;** ***************************************************************** **;
;** S. Rols 11/01 srols@anl.gov                                       **;
;** The call is w6=mib-grouping(...)                                  **;
;** This program performs a grouping of different angles of mibemol   **;
;** data using the classical scheme in defining 10 different groups   **;
;** ***************************************************************** **;


COMMON c_lamp_access, inst
COMMON printing, iprint, outstring
ON_ERROR,1
take_datp, datp
par=datp.p

x_in=datp.x
y_in=datp.y
e_in=datp.e
IF N_ELEMENTS(y_in) NE 71 THEN message,'ERROR: Dimension of Y NE 71'

; ** w_buf ... = working variables
w_buf=FLTARR(N_ELEMENTS(x_in),10) & e_buf=w_buf & y_buf=FLTARR(10)

;** First Group : from detector#0(23.5) to detector#3(27.5)
w_buf(*,0)=TOTAL(w_in(*,0:3),2)/4. & y_buf(0)=TOTAL(y_in(0:3))/4. & e_buf(*,0)=SQRT(TOTAL((e_in(*,0:3))^2,2))/4.
;** Second Group : from detector#4(37.6) to detector#8(42.6)
w_buf(*,1)=TOTAL(w_in(*,4:8),2)/5. & y_buf(1)=TOTAL(y_in(4:8))/5. & e_buf(*,1)=SQRT(TOTAL((e_in(*,4:8))^2,2))/5.
;** Third Group : from detector#9(44.6) to detector#13(49.6)
w_buf(*,2)=TOTAL(w_in(*,9:13),2)/5. & y_buf(2)=TOTAL(y_in(9:13))/5. & e_buf(*,2)=SQRT(TOTAL((e_in(*,9:13))^2,2))/5.
;** Fourth Group : from detector#14(52.6) to detector#18(57.6)
w_buf(*,3)=TOTAL(w_in(*,14:18),2)/5. & y_buf(3)=TOTAL(y_in(14:18))/5. & e_buf(*,3)=SQRT(TOTAL((e_in(*,14:18))^2,2))/5.
;** Fith Group : from detector#19(60.5) to detector#23(65.6)
w_buf(*,4)=TOTAL(w_in(*,19:23),2)/5. & y_buf(4)=TOTAL(y_in(19:23))/5. & e_buf(*,4)=SQRT(TOTAL((e_in(*,19:23))^2,2))/5.
;** Sixth Group : from detector#24(69.5) to detector#33(80.5)
w_buf(*,5)=TOTAL(w_in(*,24:33),2)/10. & y_buf(5)=TOTAL(y_in(24:33))/10. & e_buf(*,5)=SQRT(TOTAL((e_in(*,24:33))^2,2))/10.
;** Seventh Group : from detector#34(84.5) to detector#43(95.5)
w_buf(*,6)=TOTAL(w_in(*,34:43),2)/10. & y_buf(6)=TOTAL(y_in(34:43))/10. & e_buf(*,6)=SQRT(TOTAL((e_in(*,34:43))^2,2))/10.
;** Eighth Group : from detector#44(100.2) to detector#53(111.7)
w_buf(*,7)=TOTAL(w_in(*,44:53),2)/10. & y_buf(7)=TOTAL(y_in(44:53))/10. & e_buf(*,7)=SQRT(TOTAL((e_in(*,44:53))^2,2))/10.
;** Nineth Group : from detector#54(117) to detector#62(127)
w_buf(*,8)=TOTAL(w_in(*,54:62),2)/9. & y_buf(8)=TOTAL(y_in(54:62))/9. & e_buf(*,8)=SQRT(TOTAL((e_in(*,54:62))^2,2))/9.
;** Tenth Group : from detector#63(133) to detector#70(141.8)
w_buf(*,9)=TOTAL(w_in(*,63:70),2)/8. & y_buf(9)=TOTAL(y_in(63:70))/8. & e_buf(*,9)=SQRT(TOTAL((e_in(*,63:70))^2,2))/4.
;
;** Return the values and the modified parameters (errors and Ymean)
y_out=y_buf
x_out=x_in
e_out=e_buf
w_out=w_buf
mod_datp, datp, "e", e_out
mod_datp, datp, "y", y_out
give_datp, datp
Print,'** Grouping MiBeMol Data into the classical 11 groups',y_out
return, w_out
; ** End of program
end
