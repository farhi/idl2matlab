% image_cont
% --------------------------------------------
% Equivalent to :
% function IMAGE_CONT, A 
% in IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
%--------------------------------------------------------
%   ILL (Institut Laue Langevin)
%   
%   38000 GRENOBLE Cedex
%--------------------------------------------------------
% Fonction : IMAGE_CONT
%            Permet de tracer une image avec les lignes de niveaux par dessus.
% Auteurs :
%                 Szczuczak Nadege
%                 Bourtembourg Reynald
% Date creation : 17 / 05 / 2003
% Modifications : 07 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarques

% KEYWORDS NON TRADUITS MAIS AVEC PEUT-ETRE EQUIVALENT MATLAB :
% ASPECT, INTERP, WINDOW_SCALE

function [varargout] = image_cont(varargin)

%%%% Initialization of parameters
  I2Mkwn=char('I2M_a1', 'window_scale', 'aspect', 'interp', 'I2M_pos');
  I2Mkwv={'a', 'window_scale', 'aspect', 'interp', 'I2M_pos'};
  
	% Variables globales
	global itsAnOplot i2mvs_p
	% Keywords specifiques
	a=[]; window_scale=[]; aspect=[]; interp=[];

   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
%%%% End of parameters initialization

% pour le traitement des plots multiples dans une meme figure
multiplots
% Mise a jour eventuelle des couleurs de la figure courante
i2m_majColormap;
% Traitement du cas ou on a !D.NAME = PS
devicePS;

      % On inverse la matrice a afficher car dans idl2matlab, on travaille toujours avec les matrices inverses.
      image=a';

      % Affichage de l'image
      % [a] = tvscl('I2M_a1', image, 'I2M_pos', [1]);
      r=size(image);
      if ndims(image) == 3
        image(1:r(1),:,:)=image(r(1):-1:1,:,:);
      else
        image(1:r(1),:)=image(r(1):-1:1,:);
      end;
      h=imagesc(image);
      set(gca,'YDir','normal');
      set(h,'Deletefcn','delete_tv');
     % global axes_wd I2Mfig I2Mfcar
      %if ~isempty(axes_wd) & sum(axes_wd(1,:)==gca)~=0
       % fii=double(int16(gca));
       % ppa=I2Mfig(fii,7:10);
       % [i,j]=find(axes_wd(1,:) == gca);
       % if i==1
       %      ppa=ppa+[I2Mfcar(1)*8,I2Mfcar(2)*3,-I2Mfcar(1)*10,-I2Mfcar(2)*5];
       %end;
       % set(gca,'position',ppa);
       %else
       %   'coucou'
       % set(gca,'units','normalized');
       % set(gca,'position',get(0,'defaultAxesPosition'));
        %set(gca,'units','pixels');
        %end;

      hold on
			itsAnOplot = 1;
			i2mvs_p.multioplot = 1;

			% Affichage du contour
      % On demande d'afficher le contour d'une couleur bien particuliere.
      [a] = contourr('I2M_a1', image,'c_colors',[255],'I2M_pos', [1]);

      % On demande que les axes soient visibles. Dans la fonction tvscl, cette propriete est mise a off.
      set(gca,'Visible','on');

			if i2mvs_p.noerase == 0
				hold off
				itsAnOplot = 0;
				% pour le traitement des plots multiples dans une meme figure
				i2mvs_p.multioplot = 0; % pour retablir l'effacement automatique de la figure courante dans le cas ou !P.MULTI[0] = 0
            end

      non_traite(interp,aspect,window_scale);

  eval(I2M_out); return;

eval(I2M_out);
return;
% end of function image_cont

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: NON_TRAITE(interp,aspect,window_scale)
% Permet de traiter les keywords INTERP, ASPECT, WINDOW_SCALE
% Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(interp,aspect,window_scale)

if ~isempty(interp)
    warning('Function IMAGE_CONT: keyword INTERP not translated');
end;

if ~isempty(aspect)
    warning('Function IMAGE_CONT: keyword ASPECT not translated');
end;

if ~isempty(window_scale)
    warning('Function IMAGE_CONT: keyword WINDOW_SCALE not translated');
end;  
