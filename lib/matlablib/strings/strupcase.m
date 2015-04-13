function  str=strupcase(in)
%function strupcase(str)
%******** *********
%**
if isa(in,'i2mstr'); str=i2mstr(upper(in(:))); else str=upper(in); end;
