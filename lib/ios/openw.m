function [varargout]=openw(varargin)
%PRO openw, lun, file, get_lun=getl, append=appd, error=err, delete=del
%*** *****
%***

    I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'get_lun' , 'append' , 'error' , 'delete' , 'I2M_pos');
    I2Mkwv=    {'lun'    , 'file'   , 'recl'   , 'getl'    , 'appd'   , 'err'   , 'del'    , 'I2M_pos'};
    lun=[]; file=[]; recl=[]; getl=[]; appd=[]; err=[]; del=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

k_err=0;
if I2M_ok, for I2M=1:2:lv; I2M_tmp=varargin{I2M}; I2Mx=strmatch(I2M_tmp,'error'); if ~isempty(I2Mx), k_err=1; end; end; end;

global I2Mflun I2Mflu0 I2Mflu1 I2Mflu2 I2Mflu3 I2Mflu4

if isempty(I2Mflun), I2Mflun=lonarr(29)+1; end;

if appd; perm='at'; else perm='wt'; end;

err=0;
lan=fopen(file,perm);
if lan < 0; err=1; if ~k_err; eval(I2M_out); UnableToOpenTheFile; end; end;

if ~err,
    if getl, lun=get_lun; else, lon=lun-99; if (lon >= 1) & (lon <= 29), I2Mflun(lon)=0; end; end;
    I2Mflu0{end+1}=file;
    I2Mflu1(end+1)=lun;
    I2Mflu2(end+1)=lan;
    if del,  I2Mflu3(end+1)=1; else, I2Mflu3(end+1)=0; end;
    
    if lun >0, isat=0; else, isat=1; end;
    f=dir(file); dat=(datenum(f.date)-datenum('01-Jan-1970'))*24*3600; sz=f.bytes;
    fst.unit=lun; fst.name=file;  fst.open=1; fst.isatty=isat; fst.isagui=isat; fst.interactive=isat;
    fst.xdr =0;   fst.compress=0; fst.read=0; fst.write=1;     fst.atime=dat;   fst.ctime=dat;  fst.mtime=dat;
    fst.transfer_count=0; fst.cur_ptr=0;      fst.size=sz;     fst.rec_len=0;
    I2Mflu4{end+1}=fst;
end;

if I2M_out, eval(I2M_out);  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
