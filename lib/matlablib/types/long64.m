function  num=long64(val)
%function long64(val)
%******** ******
%**
if nargin ~=1, disp('!!! long64.m has to be completed...'); end;
num=0;
if ~isempty(val);
    if ~isnumeric(val);
    	if isa(val,'i2mstr'), val=val(:); end;
        if ischar(val);       num=fix(str2num(val));
        elseif iscell(val);   num=(zeros(size(val)));
        	for i=prod(size(val)):-1:1;
                if     ischar(val{i});    num(i)=fix(str2num(val{i}));
                elseif isnumeric(val{i}); num(i)=fix        (val{i}); end; end;
        end
    else num=fix(val); end;
num=double(num)
end
