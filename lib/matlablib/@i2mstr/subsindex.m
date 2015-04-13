%SUBSINDEX Subscript index.
%   I = SUBSINDEX(A) is called for the syntax 'X(A)' when A is an
%   object and X is one of the built-in types (most commonly
%   'double').  SUBSINDEX must return the value of the object as a
%   zero-based integer index (I must contain integer values in the
%   range 0 to prod(size(X))-1).  SUBSINDEX is called by the default
%   SUBSREF and SUBSASGN functions and you may call it yourself if you
%   overload these functions.
%
%   SUBSINDEX is invoked separately on all the subscripts in an
%   expression such as X(A,B). 
%
%   See also SUBSREF, SUBSASGN.

%   Copyright 1984-2001 The MathWorks, Inc. 
%   $Revision: 1.10 $  $Date: 2001/04/15 12:00:30 $

%As it should be (D.R. 06/2002)
%*****************************
function I = subsindex(A)

for j=1:numel(A.cell); I(j)=str2num(A.cell{j}); end;
I=I-1;
