function c=strcne(a,b)

if isa(a,'i2mstr'); a=cellstr(a(:)); end;
if isa(b,'i2mstr'); b=cellstr(b(:)); end;
if     isnumeric(a), c= (a ~= str2num(b));
elseif isnumeric(b), c= (str2num(a) ~= b);

else,  c=~strcmp(a,b); end;
