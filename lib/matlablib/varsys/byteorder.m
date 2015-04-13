function varargout=byteorder(varargin)
%PRO byteorder, val, /lswap, /l64swap, /sswap, /swap_if_big_endian, /swap_if_little_endian
%*** ********
%***

    I2Mkwn=char('I2M_a1' , 'lswap' , 'l64swap' , 'sswap' , 'swap_if_big_endian' , 'swap_if_little_endian' , 'I2M_pos');
    I2Mkwv=    {'val'    , 'lswap' , 'l64swap' , 'sswap' , 'swap_big'           , 'swap_little'           , 'I2M_pos'};
    val=[]; lswap=[]; l64swap=[]; sswap=[]; swap_big=[]; swap_little=[]; I2M_pos=[];

    I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

%		'vms':   if (p1 eq 'uni') or (p1 eq 'mac') then res=1
%      		'Win32': if (p1 eq 'uni') or (p1 eq 'mac') then res=1
%		'linux': if (p1 eq 'uni') or (p1 eq 'mac') then res=1
%      		'MacOS': if (p1 eq 'vms') or (p1 eq 'win') or (p1 eq 'lin') then res=1
%      		 ELSE:   if (p1 eq 'vms') or (p1 eq 'win') or (p1 eq 'lin') then res=1


if ~isempty(I2M_out), eval(I2M_out);  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
