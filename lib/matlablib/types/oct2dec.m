function d = oct2dec(o)
%OCT2DEC Convert octal string to decimal integer.


if iscellstr(o), o = char(o); end
if isempty(o), d = []; return, end

[m,n]=size(o);


if ~isempty(find(o==' ' | o==0)),
  o = strjust(o);

  % Replace any leading blanks and nulls by 0.
  o(find(cumsum(o ~= ' ' & o ~= 0,2) == 0)) = '0';
else
  o = reshape(o,m,n);
end

% Check for out of range values
if any(o < '0' | o > '7' )
  error('Input string found with characters other than 0-7');
end

octbase = 8;
p = fliplr(cumprod([1 octbase(ones(1,n-1))]));
p = p(ones(m,1),:);

d = o <= 64; % Numbers
o(d) = o(d) - 48;


d = sum(o.*p,2);
