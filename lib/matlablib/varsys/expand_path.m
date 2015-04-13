function  [str,varargout]=expand_path(varargin)
%******** *******
%**
    I2Mkwn=char('I2M_a1' , 'all_dirs' , 'array' , 'count' , 'dlm' , 'help' , 'I2M_pos');
    I2Mkwv=    {'strin'  , 'alld'     , 'arey'  , 'cnt'   , 'dlm' , 'hlp'  , 'I2M_pos'};
    strin=[]; alld=[]; arey=[]; cnt=[]; dlm=[]; hlp=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

if ispc, dl=';'; else, dl=':'; end;
stp=str_sep(strin,dl);
nl =n_elements(stp);

for i=1:nl;
   if nl > 1, str=stp(i); else str=stp(i); end;
   
   if ~isempty(strfind(str,'+')), str=strrep(str,'+',''); gen=1; else, gen=0; end;
   star=strfind(str,'*'); if ~isempty(star), str=str(1:star(1)-1); gen=1; end;
   if gen, str=genpath(str); end;
   
   if nl > 1, stp(i)=str; else stp=str; end;
end;

if nl == 1, str=stp; else, str=stp(1); for i=2:nl, str=[str dl stp(i)]; end; end;

strarey=str_sep(str,dl); if isempty(strarey(1)), cnt=0; else, cnt=n_elements(strarey); end;

if arey, str=strarey; end;

if I2M_out, eval(I2M_out); end;
