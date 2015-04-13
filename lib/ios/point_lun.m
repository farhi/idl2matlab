function [varargout]=point_lun(varargin)
%*******             *********
%**
    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_pos');
    I2Mkwv=    {'lun'    , 'pos'    , 'I2M_pos'};
    lun=[]; pos=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global I2Mflu1 I2Mflu2 I2Mflu4

if lun <0, pos=0; end;
idx=where(I2Mflu1 == abs(lun)); idx=idx+1;
if idx(1) >0, lan=I2Mflu2(idx); if lun > 0, fseek(lan,pos,'bof'); else, pos=ftell(lan); end; end;

if I2M_out, eval(I2M_out);  end;
