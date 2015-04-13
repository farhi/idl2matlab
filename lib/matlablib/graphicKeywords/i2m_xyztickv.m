% i2m_xyztickv.m
% --------------------------------------------
% Equivalent to :
% graphic keyword [XYZ]TICKV = ...
% and graphic keyword [XYZ]TICKS = ...
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
% Fonction : fonction i2m_xyztickv
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 16 / 06 / 2003
% Modifications : 24 / 07 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_xyztickv(xtickv,xticks,ytickv,yticks,ztickv,zticks)
% function i2m_xyztickv(xtickv,ytickv,ztickv)

global i2mvs_x i2mvs_y i2mvs_z


%%%%%%%%%%% TRAITEMENT DE XTICKV ET XTICKS %%%%%%%%%%%%%%%%%%%%
if ~isempty(xtickv)
	% Le KG XTICKV a ete passe en parametres
	if ~isempty(xticks)
		% Le KG XTICKS a ete passe en parametres
		if xticks <= 0
			% KG XTICKS <= 0
			% On ne tient pas compte de la valeur de XTICKV
			set(gca,'XTickMode','auto');
		else
			% KG XTICKS > 0
			if sum(xtickv == 0) ~= 60
				% Il faut tenir compte des xticks+1 premieres valeurs de XTICKV
				% eliminer les doublons et trier le vecteur forme par les XTICKS+1 premieres valeurs de xtickv
				set(gca,'XTick',unique(sort(xtickv(1:min(length(xtickv),xticks+1)))));
			else
				set(gca,'XTickMode','auto');
			end
		end
	else
		% Le KG XTICKS n'a pas ete passe en parametres
		% On regarde la valeur de la variable systeme !X.TICKS
		if i2mvs_x.ticks <= 0
			% !X.TICKS <= 0
			% On ne tient pas compte de la valeur de XTICKV => rien a faire la propriete Matlab par defaut va gerer ce cas
		else
			% !X.TICKS > 0
			if sum(xtickv == 0) ~= 60
				% Il faut tenir compte des i2mvs_x.ticks+1 premieres valeurs de XTICKV
				% eliminer les doublons et trier le vecteur forme par les XTICKS+1 premieres valeurs de xtickv
				set(gca,'XTick',unique(sort(xtickv(1:min(length(xtickv),i2mvs_x.ticks+1)))));
			else
				set(gca,'XTickMode','auto');
			end
		end
	end
else
	% Le KG XTICKV n'a pas ete passe en parametres
	if ~isempty(xticks)
		% Le KG XTICKS a ete passe en parametres
		if xticks <= 0
			% KG XTICKS <= 0
			% On ne tient pas compte de la valeur de !X.TICKV
			set(gca,'XTickMode','auto');
		else
			% KG XTICKS > 0
			if sum(i2mvs_x.tickv == 0) ~= 60
				% Il faut tenir compte des xticks+1 premieres valeurs de !X.TICKV
				% eliminer les doublons et trier le vecteur forme par les XTICKS+1 premieres valeurs de xtickv
				tickxv = i2mvs_x.tickv;
				set(gca,'XTick',unique(sort(tickxv(1:min(length(tickxv),xticks+1)))));
			else
				set(gca,'XTickMode','auto');
			end
		end
	else
		% Les KG XTICKS et XTICKV n'ont pas ete passe en parametres
		% Les valeurs par defaut des proprietes Matlab associees a ces variables systemes
		% vont permettre de gerer ce cas
	end
end


%%%%%%%%%%% TRAITEMENT DE YTICKV ET YTICKS %%%%%%%%%%%%%%%%%%%%
if ~isempty(ytickv)
	% Le KG YTICKV a ete passe en parametres
	if ~isempty(yticks)
		% Le KG YTICKS a ete passe en parametres
		if yticks <= 0
			% KG YTICKS <= 0
			% On ne tient pas compte de la valeur de YTICKV
			set(gca,'YTickMode','auto');
		else
			% KG YTICKS > 0
			if sum(ytickv == 0) ~= 60
				% Il faut tenir compte des yticks+1 premieres valeurs de YTICKV
				% eliminer les doublons et trier le vecteur forme par les YTICKS+1 premieres valeurs de ytickv
				set(gca,'YTick',unique(sort(ytickv(1:min(length(ytickv),yticks+1)))));
			else
				set(gca,'YTickMode','auto');
			end
		end
	else
		% Le KG YTICKS n'a pas ete passe en parametres
		% On regarde la valeur de la variable systeme !Y.TICKS
		if i2mvs_y.ticks <= 0
			% !Y.TICKS <= 0
			% On ne tient pas compte de la valeur de YTICKV => rien a faire
		else
			% !Y.TICKS > 0
			if sum(ytickv == 0) ~= 60
				% Il faut tenir compte des i2mvs_y.ticks+1 premieres valeurs de YTICKV
				% eliminer les doublons et trier le vecteur forme par les YTICKS+1 premieres valeurs de ytickv
				set(gca,'YTick',unique(sort(ytickv(1:min(length(ytickv),i2mvs_y.ticks+1)))));
			else
				set(gca,'YTickMode','auto');
			end
		end
	end
else
	% Le KG YTICKV n'a pas ete passe en parametres
	if ~isempty(yticks)
		% Le KG YTICKS a ete passe en parametres
		if yticks <= 0
			% KG YTICKS <= 0
			% On ne tient pas compte de la valeur de !Y.TICKV
			set(gca,'YTickMode','auto');
		else
			% KG YTICKS > 0
			if sum(i2mvs_y.tickv == 0) ~= 60
				% Il faut tenir compte des yticks+1 premieres valeurs de !Y.TICKV
				% eliminer les doublons et trier le vecteur forme par les YTICKS+1 premieres valeurs de ytickv
				tickyv = i2mvs_y.tickv;
				set(gca,'YTick',unique(sort(tickyv(1:min(length(tickyv),yticks+1)))));
			else
				set(gca,'YTickMode','auto');
			end
		end
	else
		% Le KG YTICKS n'a pas ete passe en parametres
		% Les valeurs par defaut des proprietes Matlab associees a ces variables systemes
		% vont permettre de gerer ce cas
	end
end



%%%%%%%%%%% TRAITEMENT DE ZTICKV ET ZTICKS %%%%%%%%%%%%%%%%%%%%
if ~isempty(ztickv)
	% Le KG ZTICKV a ete passe en parametres
	if ~isempty(zticks)
		% Le KG ZTICKS a ete passe en parametres
		if zticks <= 0
			% KG ZTICKS <= 0
			% On ne tient pas compte de la valeur de ZTICKV
			set(gca,'ZTickMode','auto');
		else
			% KG ZTICKS > 0
			if sum(ztickv == 0) ~= 60
				% Il faut tenir compte des zticks+1 premieres valeurs de ZTICKV
				% eliminer les doublons et trier le vecteur forme par les ZTICKS+1 premieres valeurs de ztickv
				set(gca,'ZTick',unique(sort(ztickv(1:min(length(ztickv),zticks+1)))));
			else
				set(gca,'ZTickMode','auto');
			end
		end
	else
		% Le KG ZTICKS n'a pas ete passe en parametres
		% On regarde la valeur de la variable systeme !Z.TICKS
		if i2mvs_z.ticks <= 0
			% !Z.TICKS <= 0
			% On ne tient pas compte de la valeur de ZTICKV => rien a faire
		else
			% !Z.TICKS > 0
			if sum(ztickv == 0) ~= 60
				% Il faut tenir compte des i2mvs_z.ticks+1 premieres valeurs de ZTICKV
				% eliminer les doublons et trier le vecteur forme par les ZTICKS+1 premieres valeurs de ztickv
				set(gca,'ZTick',unique(sort(ztickv(1:min(length(ztickv),i2mvs_z.ticks+1)))));
			else
				set(gca,'ZTickMode','auto');
			end
		end
	end
else
	% Le KG ZTICKV n'a pas ete passe en parametres
	if ~isempty(zticks)
		% Le KG ZTICKS a ete passe en parametres
		if zticks <= 0
			% KG ZTICKS <= 0
			% On ne tient pas compte de la valeur de !Z.TICKV
			set(gca,'ZTickMode','auto');
		else
			% KG ZTICKS > 0
			if sum(i2mvs_z.tickv == 0) ~= 60
				% Il faut tenir compte des zticks+1 premieres valeurs de !Z.TICKV
				% eliminer les doublons et trier le vecteur forme par les ZTICKS+1 premieres valeurs de ztickv
				tickzv = i2mvs_z.tickv;
				set(gca,'ZTick',unique(sort(tickzv(1:min(length(tickzv),zticks+1)))));
			else
				set(gca,'ZTickMode','auto');
			end
		end
	else
		% Le KG ZTICKS n'a pas ete passe en parametres
		% Les valeurs par defaut des proprietes Matlab associees a ces variables systemes
		% vont permettre de gerer ce cas
	end
end
