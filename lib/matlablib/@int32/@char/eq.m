%==  Equal.
%   A == B does element by element comparisons between A and B
%   and returns a matrix of the same size with elements set to one
%   where the relation is true and elements set to zero where it is
%   not.  A and B must have the same dimensions unless one is a
%   scalar. A scalar can be compared with anything.
%
%   C = EQ(A,B) is called for the syntax 'A == B' when A or B is an
%   object.

%   Copyright 1984-2001 The MathWorks, Inc. 
%   $Revision: 1.8 $  $Date: 2001/04/15 12:00:29 $


%As it should be (D.R. 02/2002)
function c=eq(a,b)

 c=strcmp(a,b);

