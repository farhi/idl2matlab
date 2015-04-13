function  res=strput(dest, sourc, pos)
%function strput(dest, sourc, pos)
%******** ******
%**
if (nargin < 3), pos=0; elseif pos < 0; pos=0;   end;
if isa(dest ,'i2mstr'); dest =cellstr(dest(:));  end;
if isa(sourc,'i2mstr'); sourc=cellstr(sourc(:)); end;
res=dest;  pos=pos+1;

sz =size (dest); sl=length(sourc);
if iscell(dest); for i=prod(size(dest)):-1:1; len=length(dest{i});
        if pos <= len; a2=len-pos+1; if a2 > sl; a2=sl; end;
            res{i}(pos:pos+a2-1)=sourc(1:a2); end; end;
else,            for i=sz(1):-1:1;            len=length(dest(i,:));
        if pos <= len; a2=len-pos+1; if a2 > sl; a2=sl; end;
            res(i,pos:pos+a2-1) =sourc(1:a2); end; end;
end

if  iscellstr(res); if length(res) == 1; res=res{1}; else res=i2mstr(res); end; end;
