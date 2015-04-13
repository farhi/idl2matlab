function  str=strmid(in,pos,len)
%function strmid(str, pos [,length])
%******** ******
%**
if nargin >3, disp('!!! strmid.m has to be completed...'); end;

if isa(in,'i2mstr'); in=cellstr(in(:)); end;
if isempty(in), str=in([]); return;     end;
if (nargin < 3), len=0; end;
if iscell(in) , str=cell(size(in)); for i=prod(size(str)):-1:1;
        str{i}=strmidd(in{i},pos,len); end
else,   str   =strmidd(in   ,pos,len); end;
if  iscellstr(str); if length(str) == 1; str=str{1}; else str=i2mstr(str); end; end;

function str=strmidd(in,pos,len)
%*******
str=[]; sz=size(in); sz=sz(2); ln=len;
if  pos < sz;
    if pos < 0, pos=0; end;
    if (ln & (pos+ln > sz)); ln=0; end;
    if  ln; str=in(:,pos+1:pos+len); else str=in(:,pos+1:end); end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
