function  str=strtrim(in,flag)
%function strtrim(str [,0|1|2])
%******** *******
%**

if isa(in,'i2mstr'), in =cellstr(in(:));  end;
if isempty(in),      str=in([]); return;  end;
if isnumeric(in),    in =strung(in);      end;

if (nargin == 1),    str=deblank(in);  return;

elseif (~flag) | (flag ==2); str=deblank(in); else str=in; end;
if (flag == 1) | (flag ==2);
    if iscell(str), stm=cell(size(str));
        for i=prod(size(str)):-1:1, stm{i} = deleading(str{i}); end; str=stm;
    else, str=deleading(str); end
end
if  iscellstr(str); if length(str) == 1; str=str{1}; else str=i2mstr(str); end; end;

function str=deleading(in)

   [r,c] = find( ~isspace(in) );
   if isempty(c), str = in([]); else str=in(:,min(c):end); end
