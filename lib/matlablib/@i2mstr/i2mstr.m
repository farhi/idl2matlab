function s=i2mstr(a)

%Class STRARR

if nargin == 0, s.cell={''}; s = class(s,'i2mstr');

elseif isa(a,'i2mstr'),      s = a;

else,           s.cell=a;    s = class(s,'i2mstr');

end
