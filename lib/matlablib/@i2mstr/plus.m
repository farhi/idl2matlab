function c = plus(a,b)

if ischar(b),       c=i2mstr(strcat(a.cell,b));
elseif ischar(a),   c=i2mstr(strcat(a,b.cell));
else,               c=i2mstr(strcat(a.cell,b.cell)); end;
