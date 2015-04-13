function  lst=rstrpos(exp,str,pos)
%function rstrpos(exp,str,pos)
%******** *******
%**
if (nargin < 3), pos=0; end;
lst=strpos('I2M_a1',exp,'I2M_a2',str,'I2M_a3',pos,'reverse_search',1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
