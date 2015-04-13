% tvrd
% --------------------------------------------
% Equivalent to :
% function  Result = TVRD([X0 [, Y0 [, Nx [, Ny ]]]])
%                    [,/ORDER]
% in IDL

function [res]=tvrd(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' ,'I2M_a5' , 'channel','order','true','words', 'I2M_pos');
    I2Mkwv=    {'d1' ,     'd2' , 'd3'     , 'd4'     ,'chanArg', 'chanKG' ,'order','truK','words', 'I2M_pos'};
    
d1=[]; d2=[]; d3=[]; d4=[]; chanArg=[]; order=[]; words=[]; chanKG=[]; truK = [];

I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
%fin du passage des parametres

global i2mvs_order
    
if isempty(order), order=i2mvs_order; end; %1 top-down , 0 bottom-up

wi0=widget_gca(gca);

if wi0 >  0, currWin = wi0; end;
if wi0 <  0, currWin = gca; end;
if wi0 == 0, currWin = gcf; end;

posCurrWin = get(currWin,'position');

if ~isempty(d1), x0=d1; else, x0=1; end
if ~isempty(d2), y0=d2; else, y0=1; end
if ~isempty(d3), Nx=d3; else, Nx=posCurrWin(3)-x0; end
if ~isempty(d4), Ny=d4; else, Ny=posCurrWin(4)-y0; end

if ~isempty(truK), tmp = getframe(currWin,[x0,y0,Nx,Ny]); res=tmp.cdata;
else          [res,map]= getframe(currWin,[x0,y0,Nx,Ny]); end;

if isempty(chanKG), chanKG = chanArg; if isempty(chanKG), chanKG =[1 2 3]; end; end;

if length(size(res)) == 3,
    if order, res = res(:,end:-1,chanKG); elseif length(chanKG) == 1, res = res(:,:,chanKG); end;
elseif order, res = res(:,end:-1:1); end;

if length(size(res)) == 3, mx=0; if isempty(truK), truK=3;  mx=1;  end;
                       if truK  == 1, res = permute(res,[3 2 1]);  end;
                       if truK  == 2, res = permute(res,[2 3 1]);  end;
                       if truK  == 3, res = permute(res,[2 1 3]);  end;
                       if mx,        [res,Map] = frame2im(tmp);    end;
else, res = permute(res,[2 1]); end;
res=double(res);

if I2M_out; eval(I2M_out); end;
