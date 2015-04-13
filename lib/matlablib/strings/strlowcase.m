function  str=strlowcase(in)
%function strlowcase(str)
%******** **********
%**
if isa(in,'i2mstr'); str=i2mstr(lower(in(:))); else str=lower(in); end;
