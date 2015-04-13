function  [mat,p]=transpos(mat,p)
%*******          ********
%**

if nargin==1, p=reverse(findgen(ndims(mat))); end;

mat=permute(mat,p+1);
