%END Terminate scope of FOR, WHILE, SWITCH, TRY, and IF statements.
%   Without END's, FOR, WHILE, SWITCH, TRY, and IF wait for further input.
%   Each END is paired with the closest previous unpaired FOR, WHILE,
%   SWITCH, TRY or IF and serves to terminate its scope.
%
%   END can also serve as the last index in an indexing expression.  In
%   that context, END = SIZE(X,k) when used as part of the k-th index.
%   Examples of this use are, X(3:end) and X(1,1:2:end-1).  When using END
%   to grow an array, as in X(end+1) = 5, make sure X exists first.
%
%   END(A,K,N) is called for indexing expressions involving the object A
%   when END is part of the K-th index out of N indices.  For example,
%   the expression A(end-1,:) calls A's END method with END(A,1,2).
%
%   See also FOR, WHILE, SWITCH, TRY, IF.

%   Copyright 1984-2001 The MathWorks, Inc. 
%   $Revision: 5.15 $  $Date: 2001/04/15 12:00:02 $
%   Built-in function.

%As it should be (D.R. 06/2002)
function E=end(A,k,n)
E=1;
s=sizz(A);
if s(1) >= k, if n==s(1), E=s(k+1); else, E=n_elements(A(:)); end; end;
