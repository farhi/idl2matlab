%&  Logical AND.
%   A & B is a matrix whose elements are 1's where both A and B
%   have non-zero elements, and 0's where either has a zero element.
%   A and B must have the same dimensions unless one is a scalar.
%
%   C = AND(A,B) is called for the syntax 'A & B' when A or B is an
%   object.

%   Copyright 1984-2001 The MathWorks, Inc. 
%   $Revision: 1.9 $  $Date: 2001/04/15 12:00:27 $

function c=and(a,b)

if length(b)==1, if b>1, c=bitand(a,b); return; end; end;
c=builtin('and',a,b);
