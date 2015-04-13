function [varargout]=resolve_routine(varargin)

    I2Mkwn=char('I2M_a1' , 'compile_full_file' , 'either' , 'is_function' , 'no_recompile' , 'I2M_pos');
    I2Mkwv=    {'name'   , 'c_f_f'             , 'either' , 'is_f'        , 'no_re'        , 'I2M_pos'};
    name=[]; c_f_f=[]; either=[]; is_f=[]; no_re=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

nl=n_elements(name);
for k=1:nl;
    if nl == 1, nome=name; else, nome=name(k); end;
    ii =1;  eval('pcode(nome,''-inplace'');','ii=0;');
    if ~ii, disp(lasterr); end;
end;
if I2M_out, eval(I2M_out);  end;
