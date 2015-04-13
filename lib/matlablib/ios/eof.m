function res=eof(lun)
%*******     ***
%**
global I2Mflu1 I2Mflu2

idx=where(I2Mflu1 == lun); idx=idx+1;
if idx(1) >0, res =feof(I2Mflu2(idx)); else res=1; end;
