function [varargout]=printt(varargin)
% PRO print, exp1,...,expn ,format=fmt
% *** *****
% ***

    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' , 'I2M_a5' , 'I2M_a6' ,'I2M_a7' , 'I2M_a8' , 'I2M_a9' , 'I2M_b1' , 'I2M_b2' , 'I2M_b3' ,'I2M_b4' , 'I2M_b5' , 'I2M_b6' , 'format' , 'I2M_pos');
    I2Mkwv=    {'exp1'   , 'exp2'   , 'exp3'   , 'exp4'   , 'exp5'   , 'exp6'  , 'exp7'   , 'exp8'   , 'exp9'   , 'exp10'   , 'exp11'   , 'exp12'  , 'exp13'   , 'exp14'   , 'exp15'   , 'fmt'    , 'I2M_pos'};
    exp1=[]; exp2=[]; exp3=[]; exp4=[]; exp5=[]; exp6=[]; exp7=[]; exp8=[]; exp9=[]; exp10=[]; exp11=[]; exp12=[]; exp13=[]; exp14=[]; exp15=[]; fmt=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global I2Mflu1 I2Mflu2

if ~isempty(fmt), fmtc=i2m_format(fmt); disp(['printt:format ' fmt 'not implemented!']); else fmtc=''; end;

nx =15; cr=1;
   for i=1:nx;
    val=eval(['exp' strtrim(strung(i),2)]);
    if ~isempty(val) | ischar(val);
	if isa(val,'i2mstr') , for i=1:n_elements(val), fprintf('%s\n',val(i)); end; cr=1;
	elseif ischar(val)   ,                          fprintf('%s',val);           cr=0;
	elseif isnumeric(val), ln=numel(val);           fprintf(' %g %g %g %g %g %g %g %g %g %g\n' ,val);
	                       if ln > 1; lnd=ln/10;                                 cr=1;
	                          if lnd ~= fix(lnd);   fprintf('\n'); end;    else, cr=0; end; end;
    else, break; end;
   end;
   if ~cr, fprintf('\n'); end;

if I2M_out, eval(I2M_out);  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
