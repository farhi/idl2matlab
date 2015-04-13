
% set_plot
% --------------------------------------------
% Equivalent to :
% function SET_PLOT, Device {'X','WIN','MAC','PS','Z'}
%                 [, /COPY]
%
% in IDL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                    IDL2SCILAB Project
%
% --------------------------------------------------------
%   ILL (Institut Laue Langevin)
%
%   38000 GRENOBLE Cedex
% --------------------------------------------------------
% Fonction : fonction set_plot
% Auteur :
%                 Bourtembourg Reynald
% Date creation : 26 / 05 / 2003
% Modifications : 13 / 06 / 2003
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ATTENTION pour 'Z', 'PS', [etc...], ne pas oublier de traiter les keywords lors de l'appel a la fonction gestionInvDev
% Keyword interpolate non traduit

function [varargout]=set_plot(varargin)

I2Mkwn=char('I2M_a1','copy','interpolate','I2M_pos');
    I2Mkwv={'dev'   ,'copy','interpolate','I2M_pos'};

	% Variables utilisees
	dev=[];
	% variables SYSTEME
	global i2mvs_d i2mZbuff i2mOldGcf i2mvs_version i2mPs
	% initialisation des valeurs des keywords specifiques
	copy = ''; interpolate = '';

	I2M_pos=[];I2M_lst={}; I2M_out=''; lv=length(varargin); if rem(lv,2) ~= 0, I2M_ok=0; else, I2M_ok=1;
for I2M=1:2:lv; I2M_tmp=varargin{I2M}; if ~ischar(I2M_tmp); I2M_ok=0; break; end; I2Mx=strmatch(I2M_tmp,I2Mkwn); if length(I2Mx) ~=1, if I2M==1, I2M_ok=0; break; end; else, eval([I2Mkwv{I2Mx} '=varargin{I2M+1};']); I2M_lst{(I2M+1)/2}=I2Mkwv{I2Mx}; end; end; end;
if ~I2M_ok; for I2M=1:lv; eval([I2Mkwv{I2M} '=varargin{I2M};']); end; end;
if ~isempty(I2M_pos); for I2M=1:length(I2M_pos); I2Ms=num2str(I2M); I2M_out=[I2M_out 'varargout{' I2Ms '}=' I2M_lst{I2M_pos(I2M)} '; ']; end; end;


% Traitement du cas set_plot,'Z'
if strcmpi(dev,'Z')
	gestionInvDev('Z');
end

% Traitement du cas set_plot,'PS'
if strcmpi(dev,'PS')
	gestionInvDev('PS');
end

if strcmpi(dev,'X') | strcmpi(dev,'WIN')
	% On change la valeur de !D.NAME
	if strcmpi(i2mvs_version.os_family,'Windows')
		i2mvs_d.name = 'WIN';
	else
		i2mvs_d.name = 'X';
	end
	if ~isempty(i2mOldGcf)
		if ishandle(i2mOldGcf)
			% la figure i2mOldGcf existe bien
			set(0,'CurrentFigure',i2mOldGcf);
		else
			% la figure i2mOldGcf n'existe pas on la recree
			i2mOldGcf = figure(i2mOldGcf);
		end
	else
		% On est oblige de creer une figure pour que la figure courante soit differente de celle du device precedent
		window;
		i2mOldGcf = i2mvs_d.window+1;
	end
end

% Traitement de /INTERPOLATE
if ~isempty(interpolate)
	warning('Function SET_PLOT: keyword INTERPOLATE not translated');
end


if I2M_out; eval(I2M_out); end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fonction qui va nous permettre de gerer les devices qui ont besoin d'utiliser une fenetre invisible
% Ex : Z ou PS
% ATTENTION : IL EST IMPORTANT DE TOUJOURS APPELER CETTE FONCTION AVEC UN NOM DE DEVICE EN MAJUSCULES !!
function gestionInvDev(devName)

global i2mZbuff i2mPs i2mvs_d

% ATTENTION, CI-DESSOUS, IL EST IMOPORTANT QUE reservedFigures(i) corresponde au device invisibleDevices(i) !!
reservedFigures = [i2mZbuff i2mPs]; % tableau contenant les indices des figures reservees pour traiter les devices comme le Z-buffer ou ps
invisibleDevices = {'Z','PS'}; % les devices que l'on va gerer a l'aide d'une figure invisible

if strmatch(devName,invisibleDevices)
	% On veut passer sur la device Z ou PS
	if strcmpi(i2mvs_d.name,'X') |  strcomp(i2mvs_d.name,'WIN')
		% On stocke la figure courante pour pouvoir la reprendre quand on fera a nouveau set_plot, 'x' ou 'win'
		if ~isempty(get(0,'CurrentFigure'))
			% Il y a une figure courante
			if isempty(find(reservedFigures == gcf))
				% Ce n'est pas une figure reservee pour les autres devices
				i2mOldGcf = gcf; % On stocke la figure courante
			else
				% c'est une figure reservee pour les autres devices
				i2mOldGcf = [];
			end
		else
			% il n'y a pas de figure courante
			i2mOldGcf = [];
		end
	end
	% On cree la figure invisible associee au device si elle n'existe pas
	invFig = reservedFigures(strmatch(devName,invisibleDevices));
	if ishandle(invFig)
		% La figure invisible existe deja
		% elle doit devenir la figure courante
		set(0,'CurrentFigure',invFig);
	else
		% La figure invisible n'existe pas, on la creee
		figure(invFig);
	end
	set(invFig,'Visible','off'); % On rend la figure invisible
	% On change la valeur de !D.NAME
	i2mvs_d.name = devName;
end
