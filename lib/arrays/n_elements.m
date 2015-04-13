function n=n_elements(var)
%function n_elements(var)
%******** **********
%**
t=0;
if isa(var,'i2mstr'); var=var(:); t=1; end;
res=size(var); c=ischar(var); s=length(res);
n  =1;
if  c; res(2)=1; if isempty(var) & ~t, n=0; end; return; end;

for i=1:s; n=n*res(i); end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

