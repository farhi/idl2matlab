function free_lun(lun)
%PRO free_lun, lun
%*** ********
%***

global I2Mflun I2Mflu0 I2Mflu1 I2Mflu2 I2Mflu3

if isempty(I2Mflun), I2Mflun=lonarr(29)+1; end;

lan=lun-99;
if (lan >= 1) & (lan <= 29), I2Mflun(lan)=1; end;

cloze(lun);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
