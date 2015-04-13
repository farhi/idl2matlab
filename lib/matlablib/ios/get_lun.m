function unit=get_lun(unit)
%*******      *******

global I2Mflun

if isempty(I2Mflun), I2Mflun=lonarr(29)+1; end;

idx=find(I2Mflun);

if ~isempty(idx), unit=idx(1)+99; I2Mflun(idx(1))=0;
else,             unit=-1; end;
