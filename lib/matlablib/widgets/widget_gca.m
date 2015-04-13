function wi0=widget_gca(wid)
%*******     **********
%**
%** Return the second axes linked to axes wid

global I2Mfig

wi0= 0; fii=int16(wid);
s  = size(I2Mfig);
if   s(1) >= fii, if I2Mfig(fii,1) == wid, wi0=I2Mfig(fii,21); end; end;
