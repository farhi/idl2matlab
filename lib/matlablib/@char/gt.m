%>  Greater than.
%   A > B does element by element comparisons between A and B
%   and returns a matrix of the same size with elements set to one
%   where the relation is true and elements set to zero where it is
%   not.  A and B must have the same dimensions unless one is a
%   scalar. A scalar can be compared with anything.
%
%   C = GT(A,B) is called for the syntax 'A > B' when A or B is an
%   object.

%   Copyright 1984-2001 The MathWorks, Inc. 
%   $Revision: 1.8 $  $Date: 2001/04/15 12:00:26 $

%As it should be (D.R. 02/2002)
function c=gt(a,b)

al=length(a); bl=length(b); ok=(al==1 | bl==1); ab=al*bl;
if (al==bl | ok) & ab>0,    ok=1;
        if bl==1, if b==' ',ok=0; end; end; if ok, c=builtin('gt',a,b); return; end;
end;
	pa=uint8(a); pb=uint8(b);
	ln=al; if bl < al, ln=bl; end;
	j=1; c=1; d=0;
	while (c & j <= ln), c= pa(j) >= pb(j); if ~d, d= pa(j) > pb(j); end; j=j+1; end;
	if (c & ~d), c=al>bl; end;
