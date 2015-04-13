function  res=d1_array(varargin)
%function res=[var1,var2,var3...]
%******** ******
%**
global I2M_array
if isempty(I2M_array), I2M_array=1; end;

lv =length(varargin);
res=varargin{1};
if lv == 1, if ischar(res), res=i2mstr({res}); end; I2M_array=1; return; end;

if ischar(res);       res=cellstr(res);    end;
if isa(res,'i2mstr'); res=cellstr(res(:)); end;

szr=size(res); if szr(1) > 1 & szr(2) == 1, res=res'; szr=size(res); end;
if I2M_array ==2, if szr(1) == 1; res=res'; end; end;

for i=2:lv; ris=varargin{i};
    if ischar(ris);       if isnumeric(res), ris=str2num(ris); else, ris=cellstr(ris); end; end;
    if isa(ris,'i2mstr'); ris=cellstr(ris(:)); end;
    k=2;
    sze=size(ris); if sze(1) > 1 & sze(2) == 1, ris=ris'; sze=size(ris); end;
    szr=size(res);
    if I2M_array ==1, if sze(1) >  1 | szr(1) > 1; k=1; end; end;
    if I2M_array ==2, if sze(1) == 1; ris=ris'; end; end;
    if I2M_array ==3, k=3; end;
    
    res=cat(k,res,ris);
    end;

%if  iscellstr(res); if length(res) == 1; res=res{1}; else res=i2mstr(res); end; end;
 if  iscellstr(res); res=i2mstr(res); end;

I2M_array=1;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
