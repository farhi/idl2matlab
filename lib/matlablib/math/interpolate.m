function  [res,varargout]=interpolate(varargin)
%*******                  ***********
%**
    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' , 'cubic' , 'grid' , 'missing' , 'I2M_pos');
    I2Mkwv=    {'P'      , 'X'      , 'Y'      , 'Z'      , 'cub'   , 'grid' , 'mis'     , 'I2M_pos'};
    P=[]; X=[]; Y=[]; Z=[]; cub=[]; grid=[]; mis=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

sz=size(P); if sz(1)==1, sz=sz(2:end); elseif sz(2)==1, sz=sz(1); end;
if isempty(cub) , cub =0; end;
if isempty(grid), grid=0; end;

%Linear
%******
if     length(sz)==1, xo=0:1:sz(1)-1;
                      if ~(cub) & isempty(mis), res=interp1(xo,P,X,'spline','extrap');
                      elseif      ~(cub),       res=interp1(xo,P,X,'spline', mis);
                      elseif      isempty(mis), res=interp1(xo,P,X,'cubic' ,'extrap');
                      else,                     res=interp1(xo,P,X,'cubic' , mis);    end;
%Bilinear
%********
elseif length(sz)==2, if grid, [xf,yf]=meshgrid(Y+1,X+1);  else, xf=Y+1; yf=X+1;      end;
                      if ~(cub),                res=interp2(P,xf,yf);
                      else,                         xo=1:1:sz(2); yo=1:1:sz(1);
                                                res=interp2(xo,yo,P,xf,yf,'cubic');   end;

%Trilinear
%*********
elseif length(sz)==3, if grid, [xf,yf,zf]=meshgrid(Y+1,X+1,Z+1); else, xf=Y+1; yf=X+1; zf=Z+1; end;
                      if ~(cub),                res=interp3(P,xf,yf,zf);
                      else,                         xo=1:1:sz(2); yo=1:1:sz(1); zo=1:1:sz(3);
                                                res=interp3(xo,yo,zo,P,xf,yf,zf,'cubic');      end;

%Not implemented (see rebin)
%***************,
else, res=P; end;

if I2M_out, eval(I2M_out); end;
