%.\  Left array divide.
%   A.\B denotes element-by-element division.  A and B
%   must have the same dimensions unless one is a scalar.
%   A scalar can be divided with anything.
%
%   C = LDIVIDE(A,B) is called for the syntax 'A .\ B' when A or B is an
%   object.
%
%   See also RDIVIDE, MRDIVIDE, MLDIVIDE.

%   Copyright 1984-2001 The MathWorks, Inc. 
%   $Revision: 1.9 $  $Date: 2001/04/15 12:00:22 $

%Used to call xor function when both arguments are scalar (D.R. 09/2002)
function c=ldivide(a,b)

if length(a)+length(b) > 2, c=builtin('ldivide',a,b);

else, c=bitxor(a,b); end;
