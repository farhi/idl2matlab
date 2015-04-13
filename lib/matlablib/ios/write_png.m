function [varargout] = write_png(varargin)

  I2Mkwn=char('I2M_a1', 'I2M_a2', 'I2M_a3', 'I2M_a4', 'I2M_a5', 'transparent', 'order' , 'I2M_pos');
  I2Mkwv={'file', 'image', 'r', 'g', 'b', 'trans', 'order' , 'I2M_pos'};
  file=[]; image=[]; r=[]; g=[]; b=[]; trans=[]; order=[]; I2M_pos=[];

  I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
  for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1; I2M_ok=0; break; end; eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end;
  if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
  if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

map='';
if ~isempty(r), sz=size(r); if sz(1)==1, map=[r',g',b']; else map=[r,g,b]; end; end;

mn=0; [mx mn]=maax('I2M_a1',image ,'min',mn  ,'I2M_pos',[2]);
img=uint8((image-mn)/(mx-mn)*255);
if ~order, img=img(:,end:1); end;

if isempty(trans),
      if isempty(map), imwrite(img,    file);
      else,            imwrite(img,map,file); end;
else, if isempty(map), imwrite(img,    file,'Transparency',trans);
      else,            imwrite(img,map,file,'Transparency',trans); end;
end;

if ~isempty(I2M_out), eval(I2M_out); end;

