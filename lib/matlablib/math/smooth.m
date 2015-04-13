function  res=smooth(varargin)
%********     ******
%**
I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'edge_truncate' , 'missing' ,'nan' , 'I2M_pos');
I2Mkwv=    {'mat'    , 'width'  , 'edge'          , 'miss'    ,'non' , 'I2M_pos'};
    
mat=[]; width=[]; edge=[]; miss=[]; non=[];

I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
%fin du passage des parametres

if isempty(width),width=3; end;
r=floor(width/2);
b=[findgen(r);reverse(findgen(width-r))]+1;
b=b/total(b);

sz=size(mat);
if sz(1) > 1 & sz(2) > 1, b=b*b'; end

res=convn(mat,b,'same');
