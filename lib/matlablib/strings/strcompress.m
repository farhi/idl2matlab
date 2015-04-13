function  [str,varargout]=strcompress(varargin)
%function strcompress(str, /remove_all)
%******** ***********
%**
    I2Mkwn=char('I2M_a1','remove_all' , 'I2M_pos');
    I2Mkwv=    {'str'   ,'remo'       , 'I2M_pos'};
    str=[]; remo=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

if isa(str,'i2mstr'); str=cellstr(str(:)); end;

if ~isempty(str)  & (ischar(str) | iscellstr(str));
    sz=size(str); if ischar(str) & (sz(1) > 1); str=cellstr(str); sz=size(str); end;
    str=strrep(str,char(9),' ');
    if keyword_set(remo); str=strrep(str,' ','');
    else; if  ischar(str);      while strfind(str   ,'  '); str   =strrep(str   ,'  ',' '); end;
        else; for i=1:prod(sz); while strfind(str{i},'  '); str{i}=strrep(str{i},'  ',' '); end; end;
        end
    end
    if  iscellstr(str); if length(str) == 1; str=str{1}; else str=i2mstr(str); end; end;
end

if I2M_out, eval(I2M_out); end;
