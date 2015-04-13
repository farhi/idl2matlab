function res=fstat(lun)
%*******     *****
%**

global I2Mflu1 I2Mflu2 I2Mflu4

res=[];
idx=where(I2Mflu1 == lun); idx=idx+1;
if idx(1) >0, res=I2Mflu4{idx}; res.cur_ptr=ftell(I2Mflu2(idx)); end;
