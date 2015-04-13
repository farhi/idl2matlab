function  [varargout]=spawn(varargin)
%*******              *****
%**
%Unit not yet available !!!

    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'count' , 'exit_status' , 'hide' , 'log_output' , 'noshell' , 'nowait' , 'unit' , 'I2M_pos');
    I2Mkwv=    {'cmd'    , 'res'    , 'err'    , 'cnt'   , 'stat'        , 'hide' , 'log'        , 'nosh'    , 'nowait' , 'unit' , 'I2M_pos'};
    cmd=[]; res=[]; err=[]; cnt=[]; stat=[]; hide=[]; log=[]; nosh=[]; nowait=[]; unit=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

err='';
if log | nosh | nowait, hide=1; elseif isempty(hide), hide=0; end;

% '&' for pc:   if consoleProgram->open console, else run in background.
% *** for unix: run in background
if nowait | (ispc & ~hide), com=[cmd ' & ']; else, com=cmd; end;

if ispc, if hide, [stat,res]= dos (com); else, [stat,res]= dos (com, '-echo'); end;
else,    if hide, [stat,res]= unix(com); else, [stat,res]= unix(com, '-echo'); end; end;

res=str_sep(res,char(10)); cnt=n_elements(res);

if I2M_out, eval(I2M_out); end;
