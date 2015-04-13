function idx=sortt(mat)
%*******     *****
%**

if isa(mat,'i2mstr'), mat=mat(:); end;
nel=prod( size(mat));

if nel==size(mat,2), [mot,idx]=sort(mat);
else,                [mot,idx]=sort(reshape(mat,[1 nel])); end;

idx=idx-1;
