function  [res,varargout]=congrid(varargin)
%*******                  *******
%**
    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' , 'cubic' , 'interp' , 'minus_one' , 'I2M_pos');
    I2Mkwv=    {'mat'    , 'X'      , 'Y'      , 'Z'      , 'cub'   , 'interp' , 'min1'      , 'I2M_pos'};
    mat=[]; X=[]; Y=[]; Z=[]; cub=[]; interp=[]; min1=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

sz=size(mat); if sz(1)==1, sz=sz(2:end); end;
if isempty(cub), cub=0; end;

                          xx=0:(sz(1)-1)/(X-1):sz(1)-1;
if isempty(Y), yy=0; else yy=0:(sz(2)-1)/(Y-1):sz(2)-1; end;
if isempty(Z), zz=0; else zz=0:(sz(3)-1)/(Z-1):sz(3)-1; end;

res=interpolate('I2M_a1',mat, 'I2M_a2',xx, 'I2M_a3',yy, 'I2M_a4',zz, 'grid',1, 'cubic',cub);

if I2M_out, eval(I2M_out); end;
