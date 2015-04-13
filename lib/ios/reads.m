function varargout=reads(varargin)
%PRO reads, input, exp1,...,expn ,format=fmt
%*** *****
%***
%format is not implemented !!!

    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' , 'I2M_a5' , 'I2M_a6' ,'I2M_a7' , 'I2M_a8' , 'I2M_a9' , 'I2M_b1' , 'I2M_b2' , 'I2M_b3' ,'I2M_b4' , 'I2M_b5' , 'I2M_b6' , 'I2M_b7' , 'format' , 'I2M_pos');
    I2Mkwv=    {'input'  , 'exp1'   , 'exp2'   , 'exp3'   , 'exp4'   , 'exp5'   , 'exp6'  , 'exp7'   , 'exp8'   , 'exp9'    , 'exp10'   , 'exp11'   , 'exp12'  , 'exp13'   , 'exp14'   , 'exp15'   , 'fmt'    , 'I2M_pos'};
    input=[]; exp1=[]; exp2=[]; exp3=[]; exp4=[]; exp5=[]; exp6=[]; exp7=[]; exp8=[]; exp9=[]; exp10=[]; exp11=[]; exp12=[]; exp13=[]; exp14=[]; exp15=[]; fmt=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

nx=15; str=input;
for i=1:nx;
    val=eval(['exp' strtrim(strung(i),2)]);
    if ~isempty(val) | ischar(val);
	if ischar(val)   ,     [val,cnt,err,nxt]=sscanf(str,'%c');            str=str(nxt:end);
	elseif isnumeric(val), [val,cnt,err,nxt]=sscanf(str,'%g' ,size(val)); str=str(nxt:end); end;
	
	eval(['exp' strtrim(strung(i),2) '=val;']);
    else, break; end;
end;

if I2M_out, eval(I2M_out);  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
