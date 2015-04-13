function i = strmatch(str,strs,flag)
%STRMATCH Find possible matches for string.
%   I = STRMATCH(STR,STRS) looks through the rows of the character
%   array or cell array of strings STRS to find strings that begin
%   with string STR, returning the matching row indices.  STRMATCH is
%   fastest when STRS is a character array.
%
%   I = STRMATCH(STR,STRS,'exact') returns only the indices of the
%   strings in STRS matching STR exactly.
%
%   Examples
%     i = strmatch('max',strvcat('max','minimax','maximum'))
%   returns i = [1; 3] since rows 1 and 3 begin with 'max', and
%     i = strmatch('max',strvcat('max','minimax','maximum'),'exact')
%   returns i = 1, since only row 1 matches 'max' exactly.
%   
%   See also FINDSTR, STRVCAT, STRCMP, STRNCMP.

%   Mark W. Reichelt, 8-29-94
%   Copyright (c) 1984-98 by The MathWorks, Inc.
%   $Revision: 1.17 $  $Date: 1997/11/21 23:47:39 $

% The cell array implementation is in @cell/strmatch.m


[m,n] = size(strs);
len = length(str);
null = setstr(0); space = ' ';
if (nargin < 3)
  exactFlag = 0;
else
  exactFlag = 1;
end

% Special treatment for empty STR or STRS to avoid
% warnings and error below
if len==0
  str = reshape(str,1,len);
end 
if n==0
  strs = reshape(strs,max(m,1),n);
  [m,n] = size(strs);
end
  
if len > n
  i = [];
else
  if exactFlag  % if 'exact' flag, pad str with blanks or nulls
    % Use nulls if anything in the last column is a null.
    if ~isempty(strs) & any(strs(:,end)==null), 
      str = [str null(ones(1,n-len))];
    else
      str = [str space(ones(1,n-len))];
    end
    len = n;
  end
  i = find(~sum(builtin('ne',strs(:,1:len),str(ones(m,1),:)),2));
end
