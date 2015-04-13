function [s,varargout]=sizz(varargin)
%function sizz(var)
%******** ****
%**

    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'dimensions' , 'n_dimensions' , 'n_elements' , 'structure' , 'tname' , 'type' , 'I2M_pos');
    I2Mkwv=    {'var'    , 'index'  , 'dimensions' , 'n_dimensions' , 'n_elem    ' , 'structur'  , 'tnome' , 'tope' , 'I2M_pos'};
    var=[]; index=[]; dimensions=[]; n_dimensions=[]; n_elem=[]; structur=[]; tnome=[]; tope=[]; I2M_pos=[];

      I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
  for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1; I2M_ok=0; break; end; eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end;
  if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
  if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

if isa(var,'i2mstr'); keep=var; var=var(:); else, keep=[]; end;
n_e=n_elements(var);
if ~n_e; s=[0 0 1]; return; end;
res=whos('var');
sz =res.size;
if strcmp(res.class,'char'), sz=sz(1:length(sz)-1);
elseif sz(1) <= 1, sz=sz(2:end); elseif length(sz)==2, if sz(2) ==1, sz=sz(1); end; end;
f=length(sz);

switch res.class
case 'single';    if res.bytes/n_e == 4; s=[f sz 4  n_e]; type=4; tname='Floating point';            else s=[f sz 6  n_e]; type=6; tname='Complex floating'; end;
case 'double';    if res.bytes/n_e == 8; s=[f sz 5  n_e]; type=5; tname='Double-precision floating'; else s=[f sz 9  n_e]; type=9; tname='Double-precision complex'; end;
case 'int8';      s=[f sz 1  n_e]; type=1;  tname='Byte';
case 'uint8';     s=[f sz 1  n_e]; type=1;  tname='Byte';
case 'int16';     s=[f sz 2  n_e]; type=2;  tname='Integer';
case 'uint16';    s=[f sz 12 n_e]; type=12; tname='Unsigned Integer';
case 'int32';     s=[f sz 3  n_e]; type=3;  tname='Longword integer';
case 'uint32';    s=[f sz 13 n_e]; type=13; tname='Unsigned Longword Integer';
case 'char';      s=[f sz 7  n_e]; type=7;  tname='String';
case 'cell';      s=[f sz 7  n_e]; type=7;  tname='String';
case 'struct';    s=[f sz 8  n_e]; type=8;  tname='Structure';
otherwise;        s=[f sz 0  n_e]; type=0;  tname='Undefined';
end

if ~isempty(index),        s=s(index); end;
if ~isempty(dimensions),   s=sz;       end;
if ~isempty(n_dimensions), s=f;        end;
if ~isempty(n_elem),       s=n_e;      end;
if ~isempty(tnome),        s=tname;    end;
if ~isempty(tope),         s=type;     end;

if ~isempty(keep); var=keep; end;

if I2M_out, eval(I2M_out); end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

