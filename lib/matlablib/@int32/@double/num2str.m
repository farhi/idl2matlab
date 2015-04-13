function s = num2str(x, f)
%NUM2STR Convert number to string.
%   T = NUM2STR(X) converts the matrix X into a string representation T
%   with about 4 digits and an exponent if required.  This is useful for
%   labeling plots with the TITLE, XLABEL, YLABEL, and TEXT commands.
%
%   T = NUM2STR(X,N) converts the matrix X into a string representation
%   with a maximum N digits of precision.  The default number of digits is
%   based on the magnitude of the elements of X.
%
%   T = NUM2STR(X,FORMAT) uses the format string FORMAT (see SPRINTF for
%   details). 
%
%   Example:
%       num2str(randn(2,2),3) produces the string matrix
%
%       '-0.433    0.125'
%       ' -1.67    0.288'
%
%   See also INT2STR, SPRINTF, FPRINTF.

%   Copyright (c) 1984-98 by The MathWorks, Inc.
%   $Revision: 5.19 $  $Date: 1998/07/09 18:15:02 $

if isstr(x)
   s = x;
   return
end

if nargin < 2 & ~isempty(x) & all(all(x==fix(x)))
   d = min(12,max(1,max(ceil(log10(abs(x(:))+(x(:)==0))))));
   f = ['%' sprintf('%d',d+2) 'd'];
   fi = ['%-' sprintf('%d',d+2) 's'];
elseif nargin < 2
   d = min(11,max(1,max(ceil(log10(abs(x(:))+(x(:)==0))))))+4;
   f = ['%' int2str(d+7) '.' int2str(d) 'g'];
   fi = ['%-' int2str(d+7) 's'];
elseif ~isstr(f)
   fi = ['%-' int2str(f+7) 's'];
   f = ['%' int2str(f+7) '.' int2str(f) 'g'];
else
  % Sanity check on format
  k = find(f=='%');
  if isempty(k), error(sprintf('''%s'' is an invalid format.',f)); end
  d = sscanf(f(k(1)+1:end),'%f');
  if isempty(d), error('Format must contain field width.'); end
  fi = ['%-' int2str(d) 's'];
end
   
[m,n] = size(x);
s = '';
for i = 1:m,
   t = [];
   for j = 1:n,
      u = sprintf(f, real(x(i,j)));
      if ~isreal(x) & imag(x(i,j)) == 0,
          u = [u '+' formatimag(f,fi,0)];
      elseif imag(x(i,j)) > 0
          u = [u '+' formatimag(f,fi,imag(x(i,j)))];
      elseif imag(x(i,j)) < 0
          u = [u '-' formatimag(f,fi,-imag(x(i,j)))];
      end
      t = [t u];
   end
   s = strvcat(s,t);
end

s = lefttrim(s);

% If it's a scalar remove the trailing blanks too.
if length(x)==1,
  s = deblank(s);
end

%-----------------------
function v = formatimag(f,fi,x)
% Format imaginary part
v = [sprintf(f,x) 'i'];
v = lefttrim(v);
v = sprintf(fi,v);


%-----------------------
function s = lefttrim(s)
% Remove leading blanks
if ~isempty(s)
  [r,c] = find(builtin('ne',s,  ' '));
  if ~isempty(c)
    s = s(:,min(c):end);
  end
end
