function  len=strlen(str)
%function strlen(str)
%******** ******
%**
len=0;
if isa(str,'i2mstr'); str=cellstr(str(:)); end;
if ~isempty(str);
    if ~ischar(str) & ~iscellstr(str); str=strtrim(strung(str),2); end;
    sz=size(str);
    if iscell(str); len=zeros(sz);
                    for i=prod(size(str)):-1:1; len(i)=length(str{i})  ; end;
    else,           for i=sz(1):-1:1;           len(i)=length(str(i,:)); end; end;
end
