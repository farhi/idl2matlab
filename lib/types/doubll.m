function  num=doubll(val)
%function doubll(val)
%******** ******
%**
if nargin ~=1, disp('!!! doubll.m has to be completed...'); end;
num=0;
if ~isempty(val);
    if ~isnumeric(val);
    	if isa(val,'i2mstr'), val=val(:); end;
        if ischar(val);       num=double(str2num(val));
        elseif iscell(val);   num=double(zeros(size(val)));
        	for i=prod(size(val)):-1:1;
                if     ischar(val{i});    num(i)=str2num(val{i});
                elseif isnumeric(val{i}); num(i)=double (val{i}); end; end;
        end
    else num=double(val); end;
end
