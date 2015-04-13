% polyshade
% --------------------------------------------
% Equivalent to :
% function Result = POLYSHADE( Vertices, Polygons)
%           or
%          Result = POLYSHADE(X, Y, Z, Polygons)
%
%                           , POLY_SHADES=array
%
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
% Fonction : POLYSHADE
%            
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 28 / 04 / 2003
% Modifications : 13 / 06 / 2003
%                              
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%remarques:

%POLY_SHADES : ne fonctionne pas tres bien. => A REVOIR

%A faire:
%XSIZE, YSIZE, T3D, SHADES, DATA, NORMAL, TOP

function [img,varargout]=polyshade(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4' ,'data' , 'normal' ,'poly_shades','shades' ,'t3d' ,'top' ,'xsize' ,'ysize' ,'I2M_pos');
I2Mkwv=    {'x'      , 'y'      , 'z'      , 'p'      ,'dataa', 'normall','pshades'    ,'shadess','t3dd','topp','xsizee','ysizee','I2M_pos'};
   
%variables utilisées
%keywords
x=[];y=[];z=[];p=[];
pshades=[]; shadess=[];topp=[];xsizee=[];ysizee=[];
face=[];vertice=[];
%keywords présent si =1
dataa='';normall=''; t3d='';

I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;

% traitement du cas ou on a X, Y, Z et P
if length(z)~=0
    vertice(:,1)=x;
    vertice(:,2)=y;
    vertice(:,3)=z;
else
    % Traitement du cas ou on a vertice et polygon
    vertice=x;
    p=y;
end;

j=1;
i=1;
max=0;
   
%En IDL, il peut y avoir des polygones avec un nombre differents de cotes
%=> On determine le nombre m=max de cote d'un polygone
%=> La matrice necessaire a Matlab pour l'appel de patch aura m colonnes. 
%   On mettra les valeurs absentes egales a un des coins du polygone
while i<=length(p)
    nb=y(i);
    if nb>max
        max=nb;
     end;
     i=i+nb+1;
end;
    
i=1;
while i<=length(p)
    nb=p(i);
    face(j,1:nb)=p(i+1:i+nb);
    if nb~=max
       face(j,nb+1:max)=p(i+1);
    end;
    i=i+nb+1;    
    j=j+1;
end;


% Creation de l'image et mise en memoire dans la variable de retour img
figure(10000)
global light_axe light_ambient light_diffuse light_strength light_exponent light_reflection
h=patch('Vertices',x,'Faces',face);
set(gcf,'Visible','off');

% Traitement de pshades
if length(pshades)~=0
    ok=0;
    j=1;
    nb2=1;
    if (length(pshades)~=length(p))
        warning('Fonction POLYSHADE : poly_shades doit etre de la meme taille que p');
    else
        for i=1:length(pshades)
            if ~ok
                nb=p(i);
                ok=1;
                if i~=1
                    col(nb2)=(m-mod(m,nb))/nb;
                    nb2=nb2+1;
                end;
                m=0;
            else
                ok=1;
                if j==nb
                    ok=0;
                    j=0;
                    
                end;
                m=m+pshades(i);
                j=j+1;
            end;
        end;
        col(nb2)=(m-mod(m,nb))/nb;
    end;
    set(h,'FaceColor','flat');
    set(h,'EdgeColor','none');
    set(h,'FaceVertexCData',col');
else
    %Creation d'une lumiere
    light('Position',light_axe);
end;


material([light_ambient,light_diffuse,light_strength,light_exponent,light_reflection])
set(gca,'Visible','off');
set(h,'EdgeLighting','Gouraud');
set(h,'FaceLighting','Gouraud');

g=getframe;
[img2,map]=frame2im(g);
set(gcf,'Visible','off');
s=size(img2);

for i=1:s(1)
    img(i,:,:)=img2(s(1)+1-i,:,:);
end;

delete(10000)

% Traitement de XSIZE, YSIZE, T3D, SHADES, DATA, NORMAL, TOP
non_traite(xsizee,ysizee,t3dd,shadess,dataa,normall,topp)

if I2M_out; eval(I2M_out); end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction interne: NON_TRAITE(xsizee,ysizee,t3dd,shadess,dataa,normall,topp)
% Permet de traiter les keywords XSIZE, YSIZE, T3D,SHADES,DATA,NORMAL,TOP
% Affiche un Warning pour indiquer que le keyword n'est pas traduit.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function non_traite(xsizee,ysizee,t3dd,shadess,dataa,normall,topp)
% XSIZE, YSIZE, T3D, SHADES, DATA, NORMAL, TOP
if ~isempty(xsizee)
    warning('Function POLYSHADE: keyword XSIZE not translated');
end;
if ~isempty(ysizee)
    warning('Function POLYSHADE: keyword YSIZE not translated');
end;
if ~isempty(t3dd)
    warning('Function POLYSHADE: keyword T3D not translated');
end;
if ~isempty(shadess)
    warning('Function POLYSHADE: keyword SHADES not translated');
end;
if ~isempty(dataa)
    warning('Function POLYSHADE: keyword DATA not translated');
end;
if ~isempty(normall)
    warning('Function POLYSHADE: keyword NORMAL not translated');
end;
if ~isempty(topp)
    warning('Function POLYSHADE: keyword TOP not translated');
end;
