function [event,varargout]=widget_event(varargin)
%PRO widget_event, base, ...
%*** ********
%***
%DEFAULT: nowait
%UNUSED : yield_to_tty

    I2Mkwn=char('I2M_a1' , 'bad_id' , 'nowait' , 'save_hourglass' , 'yield_to_tty' , 'I2M_pos');
    I2Mkwv=    {'base'   , 'bad_id' , 'nowait' , 'save_hourglass' , 'yield'        , 'I2M_pos'};
    base=[]; bad_id=[]; nowait=[]; save_hourglass=[]; yield=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

global I2Mfig
global I2Mlastevent

bad_id=0;
if isempty(base)          , base          =0; end;
if isempty(nowait)        , nowait        =0; end;
if isempty(save_hourglass), save_hourglass=0; end;
fii  =double(int16(base));
even0=struct('id',0,'top',0,'handler',0);

event=even0;
lm   =size  (I2Mfig); lm=lm(1);
if base(1)>0,for i=1:length(fii), if fii(i) <0 | fii(i) > lm, ok=0; else,  ok=1; end;
                                  if  ok, if I2Mfig(fii(i)) <=0,    ok=0;   end; end;
                                  if ~ok, bad_id=base(i); eval(I2M_out); return; end; end; end;

if ~nowait, waitfor(base(1),'Callback'); end;
                                  
if isstruct(I2Mlastevent), event=I2Mlastevent; end;

if I2M_out, eval(I2M_out); end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
