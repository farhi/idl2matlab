% tv
% --------------------------------------------
% Equivalent to :
% function  TV, Image [, Position]
%    ou
%           TV, Image [, X, Y [, Channel]]
%                   /CENTIMETERS
%                   /CHANNEL
%                   /INCHES
%                   /ORDER ... (Position & T3D & Z not yet unavailable)

function [varargout]=tv(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4'   ,'centimeters' ,'channel','inches','order','true','words','xsize','ysize','t3d'  ,'data','normal','device','z','nan' ,'top' ,'scale','I2M_pos');
I2Mkwv=    {'a1'     , 'd2'     , 'd3'     , 'chanArg'  ,'cent'        ,'chanKG' ,'inches','order','truK','words','sizex','sizey','t3dKG','data','normal','device','z','nanK','topK','scale','I2M_pos'};

a1=[]; d2=[]; d3=[]; chanArg=[]; cent=[]; chanKG=[]; inches=[]; order=[]; truK=[]; words=[]; sizex=[]; sizey=[]; t3dKG=[]; data=[]; normal=[]; device=[]; z=[]; nanK=[]; topK=[]; scale=[]; 

    I2M_pos=[]; I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres

global i2mvs_order

sz=size(a1);
if sz(1) > 1,

   if isempty(order), order=i2mvs_order; end; %1 top-down , 0 bottom-up
   if isempty(d3)   , d2=0; d3=0;        end; %position not yet available
   
   my_unit = 'pixels'; fctx=1; fcty=1;
   if     ~isempty(cent) ,  my_unit = 'centimeters'; fctx=2.54/72; fcty=fctx;    elseif ~isempty(inches), my_unit = 'inches'; fctx=1/72; fcty=fctx;
   elseif ~isempty(normal), my_unit = 'normalized' ; fctx=1/sz(1); fcty=1/sz(2); elseif ~isempty(data),   my_unit = ''; end ;
   if strcmp(my_unit,'pixels'), dd2=d2+1; dd3=d3+1 ; else, dd2=d2; dd3=d3; end;

   if isempty(sizex), sizex=sz(1)*fctx; end; if isempty(sizey), sizey=sz(2)*fcty; end;
   
   if isempty(chanKG), chanKG = chanArg; if isempty(chanKG), chanKG =0; end; end; 

   d1=a1; cdmap='direct';
   if ~isempty(scale), mn=0; [mx mn]=maax('I2M_a1',d1 ,'min',mn  ,'I2M_pos',[2]);
                       if ~isempty(nanK), d1(isnan(d1)) = mn; end;
                       d1=(d1-mn)/(mx-mn)*255;
                       if ~isempty(topK), if topK < 255, d1(d1 > topK) = topK; end; end; end;
   d1=uint8(d1); per=1;

   if length(sz) == 3, if sz(3)  > 1, if isempty(truK), truK=3; end; per=0;
                       if truK  == 1, d1 = permute(d1,[3 2 1]); end;
                       if truK  == 2, d1 = permute(d1,[3 1 2]); end;
                       if truK  == 3, d1 = permute(d1,[2 1 3]); end;
                       if chanKG== 1, d1(:,:,2:3)   = 0;        end;
                       if chanKG== 2, d1(:,:,[1 3]) = 0;        end;
                       if chanKG== 3, d1(:,:,1:2)   = 0;        end; end; end;
   if per, d1=permute(d1,[2 1]); end;
   
   if isempty(findobj(0,'type','axes')), wi0 = -1; else wi0=widget_gca(gca); if wi0 < 0, wi0=gca; end; end;
   
   if wi0  < 0, wi0=axes('units','pixels','position',[d2,d3,sizex,sizey],'FontSize',8,'FontWeight','bold','NextPlot','add','color','black','xtick',[],'ytick',[],'box','off','xlim',[dd2 sizex],'ylim',[dd3 sizey],'DataAspectRatioMode','manual');
   end;
   
   if wi0 == 0, wset(gca); else, wset(wi0); end;
   
              xdata=[dd2,d2+sizex];
   if ~order, ydata=[dd3,d3+sizey]; else, ydata=[d3+sizey,dd3]; end;

   image('CData',d1,'CDataMapping',cdmap,'Xdata',xdata ,'Ydata',ydata);

end;
if ~isempty(I2M_out), eval(I2M_out); end;

