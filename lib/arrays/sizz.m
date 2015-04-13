function s=sizz(var,index)
%function sizz(var)
%******** ****
%**

if nargin >2, disp('!!! sizz.m has to be completed...'); end;

if isa(var,'i2mstr'); var=var(:); end;
n_e=n_elements(var);
if ~n_e; s=[0 0 1]; return; end;
res=whos('var');
sz =res.size; if strcmp(res.class,'char'); sz=sz(1:length(sz)-1); end;
if sz(1) <= 1;  sz=sz(2:end); end;
f=length(sz);

switch res.class
case 'single';    if res.bytes/n_e == 4; s=[f sz 4  n_e]; else s=[f sz 6  n_e]; end;
case 'double';    if res.bytes/n_e == 8; s=[f sz 5  n_e]; else s=[f sz 9  n_e]; end;
case 'int8';      s=[f sz 1  n_e];
case 'uint8';     s=[f sz 1  n_e];
case 'int16';     s=[f sz 2  n_e];
case 'uint16';    s=[f sz 12 n_e];
case 'int32';     s=[f sz 3  n_e];
case 'uint32';    s=[f sz 13 n_e];
case 'char';      s=[f sz 7  n_e];
case 'cell';      s=[f sz 7  n_e];
case 'struct';    s=[f sz 8  n_e];
otherwise;        s=[f sz 0  n_e];
end

if nargin ==2, s=s(index); end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

