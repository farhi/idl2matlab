
% data
% --------------------------------------------
% Equivalent to :
% graphic keyword DATA 
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
% Fonction : fonction data
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 20 / 05 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_data(data,h,funcName)
% data = contenu du keyword
% h = handle a consider
% funcName = nom de la fonction qui a appele color

global i2mvs_x i2mvs_y i2mvs_z
if isempty(data)
	% Le Keyword DATA n'est pas precise
else
	% DATA a ete passe en keyword
	switch funcName
		case 'polyfill'
            set(gca,'Visible','off');
            if ~(i2mvs_x.crange(1)==0 & i2mvs_x.crange(2)==0)
                set(gca,'XLim',[i2mvs_x.crange(1),i2mvs_x.crange(2)]);
                set(gca,'YLim',[i2mvs_y.crange(1),i2mvs_y.crange(2)]);
                set(gca,'ZLim',[i2mvs_z.crange(1),i2mvs_z.crange(2)]);
            end;
		% end case 'polyfill'
		otherwise
			warning('Graphic keyword DATA not translated for this function');
	end
end

