% polar_contour
% --------------------------------------------
% Equivalent to :
% function POLAR_CONTOUR, Z, Theta, R, C_ANNOTATION=['string1','string2'....]
%                           , C_CHARSIZE=real
%                           , C_COLORS=[...]
%                           , C_LINESTYLE=[0..5 , 0..5 , ...]
%                           , C_THICK = [int, int, ...]
%                           , /CELL_FILL
%                           , /FILL
%                           , /ISOTROPIC
%                           , LEVELS=[real, real, ...]
%                           , MAX_VALUE=real
%                           , MIN_VALUE=real
%                           , NLEVELS = int
%                           , /OVERPLOT
%                           , /XLOG
%                           , /YLOG
%                           , SHOW_TRIANGULATION=int

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
% Fonction : POLAR_CONTOUR
%            Permet de tracer des lignes de niveaux en coordonnees plaires
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 14 / 04 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarques:

% Revoir la fonction pour verifier si on travaille bien avec les
% matrices inverses a IDL.


% KEYWORDS SANS EQUIVALENT MATLAB :
% C_CHARTHICK : pas possible de le faire en Matlab => aucune action
% C_ORIENTATION : pas possible de le faire en Matlab => aucune action
% C_SPACING : pas possible de le faire en Matlab => aucune action
% /CLOSED : pas possible de le faire en Matlab => aucune action

% KEYWORDS NON TRADUITS MAIS AVEC PEUT-ETRE EQUIVALENT MATLAB :
% /PATH_DATA_COORDS : pas traduit
% /PATH_DOUBLE : pas traduit

% A FAIRE :
% /IRREGULAR : pas encore traduit => A FAIRE
% TRIANGULATION : pas encore traduit => A FAIRE
% /ZAXIS : pas encore traduit => A FAIRE

% COMMENTAIRES SUR LES KEYWORDS TRADUITS :
% C_ANNOTATION : le texte est ecrit horizontalement precede d'un + sur la ligne de niveau concernee
% C_CHARSIZE : on prend la conversion, charsizeMatlab = charsizeIDL*10
% C_LINESTYLE : - pas tout a fait les memes equivalents
%              - le cas (v lt 0.0) pas traite
% /CELL_FILL et /FILL : traite de la meme maniere
% MAX_VALUE : valeur >= pas mises a NaN mais a la valeur de MAX_VALUE
% MIN_VALUE : idem
% NLEVELS : =6 par defaut
function [varargout] = polar_contour(varargin)

%%%% Initialization of parameters
  I2Mkwn=char('I2M_a1', 'I2M_a2', 'I2M_a3', 'show_triangulation',  'irregular' ,'triangulation' ,'overplot' ,'max_value' ,'min_value','c_charsize', 'c_colors' ,'c_thick' ,'c_charthick','c_annotation','c_linestyle' ,'c_orientation' , 'c_spacing' ,'closed' ,'levels'  , 'nlevels',  'isotropic'  ,'cell_fill','fill'  ,'path_data_coords','path_double','xlog' , 'ylog' , 'zaxis' ,'I2M_pos');
  I2Mkwv=    {'z'     , 'theta' , 'r'     , 'show'              ,  'irregularr','triangulationn','multiplot','maxi'      ,'mini'     ,'charsize'  , 'colors'   ,'thick'   ,'charthick'  ,'annotation'  ,'styleligne'  ,'orientation'   , 'spacing'   ,'closedd','niveaux' , 'nblevel',  'axis_equal' ,'rempliBis','rempli','data_coords'     ,'double'     ,'logx' , 'logy' , 'zaxiss','I2M_pos'};
  z=[]; theta=[]; r=[]; show=[]; dither=[]; e=[];
	
   I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
%%%% End of parameters initialization

  % Creation of undeclared variables of functions parameters
  count=1; xmin=1; ymin=1; tr=1; 
  
  s = eval('sizz(z)','[0 0 1]');
  nz = eval('n_elements(z)','0');
  
  if ((s(1) == 2) & (eval('n_elements(theta)','0') == s(3)) & (eval('n_elements(r)','0') == s(2)))
    tt = replicate(1,eval('n_elements(r)','0'))'*theta;
    zeroes=find(r==0);
    count=numel(zeroes);
    if (count ~= 0)
      range = maax(abs(r));
      rtemp = r;
      rtemp(zeroes +1) = range ./ 1.0e5;
      rr = rtemp'*replicate(1,eval('n_elements(theta)','0'));
    else
      rr = r'*replicate(1,eval('n_elements(theta)','0'));
    end;
    x = rr .* cos(tt);
    y = rr .* sin(tt);
  else
    x = r .* cos(theta);
    y = r .* sin(theta);
  end;
  
  if (keyword_set(dither))
    % Dither amount
    eps = 5.0e5;
    % Get extent
    [xmax, x, xmin] = maax('I2M_a1', x, 'min', xmin, 'I2M_pos', [1, 2]);
    [ymax, y, ymin] = maax('I2M_a1', y, 'min', ymin, 'I2M_pos', [1, 2]);
    scale = (max((xmax - xmin), (ymax - ymin))) ./ eps;
    tr=delaunay(x + randomu(seed,eval('n_elements(x)','0')) .* scale,y + randomu(seed,eval('n_elements(y)','0')) .* scale);
  else
    tr=delaunay(x,y);
  end;
  
  % Si I2M_a2 et I2M_a3 n'etaient pas passe en parametres, on les rajoute dans I2M_pos.
  % L'appel a la fonction contourr se fera toujours avec x, y et z.
  % A la fin: g=I2M_pos modifie.
  s=size(varargin);
  if varargin{3}~='I2M_a2'
      s(2)=s(2)+4;
      sbis=length(varargin{s(2)});
      g(1)=varargin{s(2)}(1);
      g(2)=2;
      g(3)=3;
      for i=2:sbis
          g(i+2)=varargin{s(2)}(i);
      end;
  else
      g=varargin{s(2)};
  end;

  % Creation de la chaine d'arguments avec laquelle on appelle contourr
  % On remplace theta et r par x et y. On remplace triangulationn par tr.
  % On remplace I2M_pos par g.
  args=cell(1,s(2));
  tri=0; n=1; ok=1;
  while n<=s(2)
      if ((n==3 & ok)| tri | n==s(2))
          if n==3 & ok
              args{3}='I2M_a2';
              args{4}=x;
              args{5}='I2M_a3';
              args{6}=y;
              if varargin{3}=='I2M_a2'
                  n=7; 
              else
                  ok=0;
              end;
          end;
          if tri
              args{n}=tr;
              tri=0;
              n=n+1;
          end;
          if n==s(2)
              args{n}=g;
              n=n+1;
          end;
      else
          if varargin(n)=='triangulation'
              tri=1;
          end;
          args{n} = varargin{n};
          n=n+1;
      end;
  end;
  
  % Appel de la fonction contourr avec la chaine d'arguments creee precedemment.
  [z, x, y] = contourr(args{:});
  
  if (eval('n_elements(show)','0') == 1)
    % Show the triangulation?
    oplot(x,y,1,show);
    for i = 0:eval('n_elements(tr)','0') ./ 3 - 1,
      t = d1_array(tr(:,i +1),tr(0 +1,i +1));
      plots(x(t +1),y(t +1),show);
    end;
  end;

% Traitement des keywords non traduit
non_traite(irregularr,triangulationn,zaxiss)

eval(I2M_out);
return;
% end of function polar_contour


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: NON_TRAITE(irregularr,triangulationn,zaxiss)
% Permet de traiter les keywords IRREGULAR,TRIANGULATION, ZAXIS
% Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(irregularr,triangulationn,zaxiss)

if ~isempty(irregularr)
    warning('Function POLAR_CONTOUR: keyword IRREGULAR not translated');
end;
if ~isempty(triangulationn)
    warning('Function POLAR_CONTOUR: keyword TRIANGULATION not translated');
end;
if ~isempty(zaxiss)
    warning('Function POLAR_CONTOUR: keyword ZAXIS not translated');
end;
