function  [res,varargout]=rebin(varargin)
%*******                  *****
%**
    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' , 'I2M_a5' , 'I2M_a6' , 'I2M_a7' , 'I2M_a8' , 'I2M_a9' , 'sample' , 'I2M_pos');
    I2Mkwv=    {'mat'    , 'X'      , 'Y'      , 'Z'      , 'd4'     , 'd5'     , 'd6'     , 'd7'     , 'd8'     , 'sample' , 'I2M_pos'};
    mat=[]; X=[]; Y=[]; Z=[]; d4=[]; d5=[]; d6=[]; d7=[]; d8=[]; sample=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

sz=size(mat); if sz(1)==1, sz=sz(2:end); end;
cub=0;

                           xx=0:(sz(1)-1)/(X -1):sz(1)-1;
if isempty(Y),  yy=0; else yy=0:(sz(2)-1)/(Y -1):sz(2)-1; end;
if isempty(Z),  zz=0; else zz=0:(sz(3)-1)/(Z -1):sz(3)-1; end;

%Linear, bilinear, trilinear
%******  ********  *********
if isempty(d4),
   res=interpolate('I2M_a1',mat, 'I2M_a2',xx, 'I2M_a3',yy, 'I2M_a4',zz, 'grid',1, 'cubic',cub);

%More dimensions
%**** **********
else, xx=xx+1; yy=yy+1; zz=zz+1;
                     s4=1:(sz(4)-1)/(d4-1):sz(4); [r1,r2,r3,r4]=ndgrid(xx,yy,zz,s4);
if ~isempty(d5),     s5=1:(sz(5)-1)/(d5-1):sz(5); [r1,r2,r3,r4,r5]=ndgrid(xx,yy,zz,s4,s5);
if ~isempty(d6),     s6=1:(sz(6)-1)/(d6-1):sz(6); [r1,r2,r3,r4,r5,r6]=ndgrid(xx,yy,zz,s4,s5,s6);
if ~isempty(d7),     s7=1:(sz(7)-1)/(d7-1):sz(7); [r1,r2,r3,r4,r5,r6,r7]=ndgrid(xx,yy,zz,s4,s5,s6,s7);
if ~isempty(d8),     s8=1:(sz(8)-1)/(d8-1):sz(8); [r1,r2,r3,r4,r5,r6,r7,r8]=ndgrid(xx,yy,zz,s4,s5,s6,s7,s8);
                     end; end; end; end;
if      isempty(d5), res=interpn(mat,r1,r2,r3,r4);
elseif  isempty(d6), res=interpn(mat,r1,r2,r3,r4,r5);
elseif  isempty(d7), res=interpn(mat,r1,r2,r3,r4,r5,r6);
elseif  isempty(d8), res=interpn(mat,r1,r2,r3,r4,r5,r6,r7);
else,                res=interpn(mat,r1,r2,r3,r4,r5,r6,r7,r8); end;

end;

if I2M_out, eval(I2M_out); end;
