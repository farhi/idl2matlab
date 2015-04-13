function  res=d1_array(varargin)
% function res=[var1,var2,var3...]
% ******** ******
% **
global I2M_array
if isempty(I2M_array), I2M_array=1; end;

lv =length(varargin);
res=varargin{1};
if lv == 1, if ischar(res), res=i2mstr({res}); end; szr=size(res); if szr(1)==1 & szr(2)>1 & I2M_array ==1 & ~i2msizz, res=res'; end; I2M_array=1; return; end;

if ischar(res);       res=cellstr(res);    end;
if isa(res,'i2mstr'); res=cellstr(res(:)); end;

szr=size(res);
if szr(1)==1 & szr(2)>1 & I2M_array ==1 & ~i2msizz, res=res'; end;
if (I2M_array ==2 & szr(1) ==1), res=res'; 
elseif i2msizz(szr(1),szr(2))  , res=res'; end;
szr=size(res);

for i=2:lv; ris=varargin{i};
    if ischar(ris);       if isnumeric(res), ris=str2num(ris); else, ris=cellstr(ris); end; end;
    if isa(ris,'i2mstr'); ris=cellstr(ris(:)); end;
    k=2;
    sze=size(ris);
    if sze(1)==1 & sze(2)>1 & I2M_array ==1 & ~i2msizz, ris=ris'; end;
    if  I2M_array ==2 & sze(1) == 1, ris=ris'; 
    elseif i2msizz(sze(1),sze(2))  , ris=ris'; end;
    sze=size(ris);
    szr=size(res);
    
    if  I2M_array ==1, if (sze(1) >  1) | (szr(1) > 1) | (~i2msizz), k=1; end; end;
    if  I2M_array ==3, k=3; end;
    
    res=cat(k,res,ris);
    end;

%if  iscellstr(res); if length(res) == 1; res=res{1}; else res=i2mstr(res); end; end;
 if  iscellstr(res); res=i2mstr(res); end;

I2M_array=1;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
