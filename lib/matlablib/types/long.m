function  num=long(val)
%function long(val)
%******** ****
%**
if nargin ~=1, disp('!!! long.m has to be completed...'); end;
num=0;
if ~isempty(val);
    if ~isnumeric(val);
    	if isa(val,'i2mstr'), val=val(:); end;
        if ischar(val);       num=int32(str2num(val));
        elseif iscell(val);   num=int32(zeros(size(val)));
        	for i=prod(size(val)):-1:1;
                if     ischar(val{i});    num(i)=str2num(val{i});
                elseif isnumeric(val{i}); num(i)=int32  (val{i}); end; end;
        end
    else num=int32(val); end;
num=double(num);
end
