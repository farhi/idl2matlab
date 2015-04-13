function num=xregistered(name)
%******* ***************
%**
global I2Mfig I2Mfevn I2Mfgap I2Mreg

num=0;
if isstruct(I2Mreg),
   f=eval('strmatch(lower(name),fieldnames(I2Mreg))','[]');
   if ~isempty(f), f=getfield(I2Mreg,lower(name)), if eval('I2Mfig(int16(f),6) == 3','0'); num=1; end; end;
end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
