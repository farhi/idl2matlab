
% tvlct
% --------------------------------------------
% Equivalent to :
%                TVLCT, V(1), V(2), V(3) [, Start] [, /GET] [, /HLS | , /HSV]
%                or
%                TVLCT, V [, Start] [, /GET]
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
% Fonction : tvlct
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 05 / 05 / 2003
% Modifications : 13 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% faire attention au cas ou on passe un vecteur vide
% /HLS non traduits pour l'instant
% /HSV pas traduit correctement

function [varargout]=tvlct(varargin)

I2Mkwn=char('I2M_a1' , 'I2M_a2' , 'I2M_a3' , 'I2M_a4', 'get' ,     'hls',      'hsv', 'I2M_pos');
    I2Mkwv=    {'v1'     , 'v2' ,     'v3' , 'start','gett','hls_mode', 'hsv_mode','I2M_pos'};

		% variables utilisées
    v1=[]; v2=[]; v3=[]; start=[];
		% keywords présent si =1
		depart=''; gett=''; hls_mode=''; hsv_mode='';
    % keywords graphiques
		% Variables locales
		nbvar = 0;

I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;
% fin du passage des parametres


global i2mColorTable i2mvs_d

% calcul du nombre d'arguments
if ~isempty (start)
	% start n'est pas vide
	nbvar = 3;
else
	% start est vide
	if ~isempty(v3)
		nbvar = 3;
		start = 0;
	else
		if ~isempty(v2)
			nbvar = 1;
			start = v2;
		else
			if ~isempty(v1)
				nbvar = 1;
				start = 0;
			else
				error('tvlct accepts 4 arguments maximum')
			end
		end
	end
end

% traitement de [start] (le 4eme argument ou le 2eme si il n'y a que 2 arguments

	c = i2mColorTable ; % On recupere l'actuelle table des couleurs IDL


if (~isempty(gett) & (gett == 1))
	% traitement de /GET, on ne s'occupe pas des keywords /HSV et /HLS
	if nbvar == 3
		% On doit stocker les valeurs de l'actuelle table des couleurs dans les variables v1,v2 et v3 correspondant aux composantes rouge, verte et bleue de la table des couleurs.
		v1 = transpose(c(start+1:length(c),1) * 255.0); % On met dans v1 le vecteur correspondant aux composantes rouges de l'actuelle table de couleurs
		v2 = transpose(c(start+1:length(c),2) * 255.0); % idem pour le vert
		v3 = transpose(c(start+1:length(c),3) * 255.0); % idem pour le bleu
	else
		% On ne peut pas avoir un seul argument si /GET est precise
		error('TVLCT: Expression must be an array in this context')
	end
else
	% traitement normal, GET n'est pas passe en keyword


	v1 = double(v1)/255.0;
	v2 = double(v2)/255.0;
	v3 = double(v3)/255.0;
	if nbvar == 3
		% nbvar = 3
		if(start+1 >= length(c))
			c(length(c),:) = [v1(1) v2(1) v3(1)]; % On ne change que la derniere valeur de la table de couleurs
		else
			miniLength = min (length(v1),min(length(v2),length(v3))); % on recupere la longueur minimum des 3 vecteurs v1,v2 et v3
			if (start + miniLength) < length(c)
				c(start+1:start+miniLength,:) =[transpose(v1(start+1:start+miniLength)) transpose(v2(start+1:start+miniLength)) transpose(v3(start+1:start+miniLength))];
			else
				c(start+1:length(c),:)=[transpose(v1(1:length(c)-start)) transpose(v2(1:length(c)-(start))) transpose(v3(1:length(c)-(start)))];
			end
		end
	else
		% nbvar = 1
		if (start + 1) > length(c)
			c(length(c),:) = transpose(v1(:,1));
		else
			if (start+length(v1)) <= length(c)
				c(start+1:length(v1),:) = transpose(v1);
			else
				c(start+1:length(c),:) = transpose(v1(:,1:length(c)-start));
			end
		end % if (start+1)>length(c)
	end % if nbvar = 3
	

% Traitement de /HSV
	if ~isempty(hsv_mode) & hsv_mode ~= 0
		% /HSV a ete presice
		% c = hsv2rgb(c); % NE FONCTIONNE PAS CORRECTEMENT =(
		warning('Function TVLCT: keyword HSV not translated');
	end
	
% Traitement de /HLS
	if ~isempty(hls_mode) & hls_mode ~= 0
		% /HLS a ete presice
		warning('Function TVLCT: keyword HLS not translated');
	end
% A FAIRE !!!!!!!!

	i2mColormap(c); % On change la table de couleurs IDL par defaut
end

if I2M_out; eval(I2M_out); end;
