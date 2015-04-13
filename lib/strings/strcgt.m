function c=strcgt(a,b)

if     isnumeric(a), c= (a > str2num(b));
elseif isnumeric(b), c= (str2num(a) > b);

else, al=length(a); bl=length(b);

	pa=uint8(a); pb=uint8(b);
	ln=al; if bl < al, ln=bl; end;
	j=1; c=1; d=0;
	while (c & j <= ln), c= pa(j) >= pb(j); if ~d, d= pa(j) > pb(j); end; j=j+1; end;
	if (c & ~d), c=al>bl; end;
end;
