function [res,varargout]=product(varargin)
%Function product
%******** *******
%**

    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'cumulative' , 'I2M_pos');
    I2Mkwv=    {'vect'   , 'dim'    , 'cumu'       , 'I2M_pos'};
    vect=[]; dim=[]; cumu=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

if (dim);
      if size(mat,1)==1 & dim==1, dim=dim+1; end;
      if cumu, res=cumprod(mat,dim); else res=prod(mat,dim); end;

else, if cumu, res=cumprod(mat)    ; else res=prod(mat); if length(res) > 1, res=prod(res); end; end; end;

res=squeeze(res);

if ~isempty(I2M_out), eval(I2M_out);  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
