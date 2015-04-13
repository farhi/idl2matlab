function  [lst,varargout]=strpos(varargin)
%function strpos(exp, str, pos, /reverse_offset, /reverse_search)
%******** ******
%**
    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'reverse_offset' , 'reverse_search' , 'I2M_pos');
    I2Mkwv=    {'exp'    , 'str'    , 'pos'    , 'rvoff'          , 'rvstr'          , 'I2M_pos'};
    exp=[]; str=[]; pos=[]; rvoff=[]; rvstr=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~= 1, I2M_ok=0; break; end; eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

lst=0;
if isa(exp,'i2mstr'); exp=cellstr(exp(:)); end;
if ~isempty(exp);
    if  isempty(pos); pos=0; end;
    if ~ischar(exp) & ~iscellstr(exp); exp=strtrim(strung(exp),2); end;

    sz=size(exp);
    if iscell(exp); lst=zeros(sz);
            for i=prod(size(exp)):-1:1; len=length(exp{i});   fx=[]; if rvoff; px=len-pos; else px=pos+1; end;
               if (px <= len); fx=strfind(exp{i},str);               end;  fx=fx(fx>=px);
               if fx; if rvstr; lst(i)=fx(end);  else lst(i)= fx(1); end;  else lst(i)=0;  end; end;
        
    else,   for i=sz(1):-1:1;           len=length(exp(i,:)); fx=[]; if rvoff; px=len-pos; else px=pos+1; end;
               if (px <= len); fx=strfind(exp(i,:),str);             end;  fx=fx(fx>=px); 
               if fx; if rvstr; lst(i)=fx(end);   else lst(i)=fx(1); end;  else lst(i)=0;  end; end;
    end;
end
lst=lst-1;

if I2M_out, eval(I2M_out); end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
