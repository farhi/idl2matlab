
% normal
% --------------------------------------------
% Equivalent to :
% graphic keyword NORMAL 
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
% Fonction : fonction i2m_normal
% Auteurs :
%                 Szczuczak Nadege
% Date creation : 20 / 05 / 2003
% Modifications : 16 / 06 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function i2m_normal(normal,h,funcName)
% normal = contenu du keyword
% h = handle a consider
% funcName = nom de la fonction qui a appele color

global i2mvs_x i2mvs_y i2mvs_z
if isempty(normal)
	% Le Keyword NORMAL n'est pas precise
else
	% NORMAL a ete passe en keyword
	switch funcName
		case 'polyfill'
            %set(gca,'Visible','off');
            if ~(i2mvs_x.crange(1)==0 & i2mvs_x.crange(2)==0)
                set(gca,'XLim',[i2mvs_x.crange(1),i2mvs_x.crange(2)]);
                set(gca,'YLim',[i2mvs_y.crange(1),i2mvs_y.crange(2)]);
                set(gca,'ZLim',[i2mvs_z.crange(1),i2mvs_z.crange(2)]);
            else
                [res]=convert_coord('I2M_a1',get(gca,'XLim'),'I2M_a2',get(gca,'XLim'),'normal',1,'to_data',1)
            end;
		% end case 'polyfill'
		otherwise
			warning('Graphic keyword NORMAL not translated for this function');
	end
end

