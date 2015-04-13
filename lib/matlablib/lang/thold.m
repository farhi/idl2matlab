function  val=thold(val ,sens,threshold)
%function thold(mat, '>'|'<' ,threshold)
%******** *****
%**
if sens == '>'; val(val < threshold)=threshold;
else;           val(val > threshold)=threshold; end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
