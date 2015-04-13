function  num=fixt(val)
%function fixt(val)
%******** ***
%**
if nargin ~=1, disp('!!! fixt.m has to be completed...'); end;
num=0;
if ~isempty(val);
    if ~isnumeric(val);
    	if isa(val,'i2mstr'), val=val(:); end;
        if ischar(val);       num=int16(str2num(val));
        elseif iscell(val);   num=int16(zeros(size(val)));
        	for i=prod(size(val)):-1:1;
                if     ischar(val{i});    num(i)=str2num(val{i});
                elseif isnumeric(val{i}); num(i)=int16  (val{i}); end; end;
        end
    else num=int16(val); end;
num=double(num);
end
