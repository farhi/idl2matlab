% i2m_charsize
% --------------------------------------------
% Equivalent to :
% graphic keyword CHARSIZE = ...
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
% Fonction : fonction i2m_charsize
% Auteurs :
%                 Bourtembourg Reynald
% Date creation : 25 / 04 / 2003
% Modifications : 12 / 08 / 2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remarque : xyouts ne fait pas appel a cette fonction
% Cette fonction va egalement permettre de gerer ![XYZ].CHARSIZE

function i2m_charsize(size,font,xsize,ysize,zsize)

global i2mvs_p i2mvs_x i2mvs_y i2mvs_z coefTailleCar

ok = 0;
if ~isempty(font)
    % le KG FONT a ete passe en parametres
    if font ~= 0
        ok = 1;
    end
else
    % Le KG FONT n'a pas ete passe en parametres
    if i2mvs_p.font ~= 0
        ok = 1;
    end
end

if ok == 1
    % CHARSIZE et [XYZ]CHARSIZE ont un sens (font != 0 )
    if isempty(size)
    	% Le Keyword CHARSIZE n''est pas precise'
    	% On ne fait rien
    	% L'objet i2mvs_p va gerer tout seul le comportement par defaut de Matlab
    	% sauf pour les axes X, Y et Z ... (Il faut tenir compte de la valeur de ![XYZ].charsize ou du KG XCHARSIZE,YCHARSIZE,ZCHARSIZE
    	xtitle = get(gca,'XLabel'); % On recupere l'objet texte associe au titre de l'axe des X
    	ytitle = get(gca,'YLabel'); % On recupere l'objet texte associe au titre de l'axe des Y
    	ztitle = get(gca,'ZLabel'); % On recupere l'objet texte associe au titre de l'axe des Z
    	if ~isempty(xsize)
            % Le KG XCHARSIZE a ete precise'
            if i2mvs_p.charsize <= 0
    			set(xtitle,'FontSize',coefTailleCar*xsize);
    		else
    			set(xtitle,'FontSize',coefTailleCar*i2mvs_p.charsize*xsize);
    		end
        else
            % On regarde la variable systeme !X.CHARSIZE'
            if i2mvs_x.charsize ~= 0
    		    if i2mvs_p.charsize <= 0
    			    set(xtitle,'FontSize',coefTailleCar*i2mvs_x.charsize);
    		    else
    			    set(xtitle,'FontSize',coefTailleCar*i2mvs_p.charsize*i2mvs_x.charsize);
    		    end
    	    end
        end
        if ~isempty(ysize)
            % Le KG YCHARSIZE a ete precise'
            if i2mvs_p.charsize <= 0
    			set(ytitle,'FontSize',coefTailleCar*ysize);
    		else
    			set(ytitle,'FontSize',coefTailleCar*i2mvs_p.charsize*ysize);
    		end
        else
            % On regarde la variable systeme !Y.CHARSIZE'
        	if i2mvs_y.charsize ~= 0
    		    if i2mvs_p.charsize <= 0
    			    set(ytitle,'FontSize',coefTailleCar*i2mvs_y.charsize);
        		else
        			set(ytitle,'FontSize',coefTailleCar*i2mvs_p.charsize*i2mvs_y.charsize);
        		end
        	end
        end
    	if ~isempty(zsize)
            % Le KG ZCHARSIZE a ete precise'
            if i2mvs_p.charsize <= 0
    			set(ztitle,'FontSize',coefTailleCar*zsize);
    		else
    			set(ztitle,'FontSize',coefTailleCar*i2mvs_p.charsize*zsize);
    		end
        else
            % On regarde la variable systeme !Z.CHARSIZE'
            if i2mvs_z.charsize ~= 0
        		if i2mvs_p.charsize <= 0
        			set(ztitle,'FontSize',coefTailleCar*i2mvs_z.charsize);
        		else
        			set(ztitle,'FontSize',coefTailleCar*i2mvs_p.charsize*i2mvs_z.charsize);
        		end
        	end
        end
    else
    	% CHARSIZE a ete passe en keyword, on utilise la valeur du keyword
    	set(gca,'FontUnits','points'); % si jamais la valeur par defaut de FontUnits avait ete modifiee
    	% h = findobj(gca,'Type','text');
    	xtitle = get(gca,'XLabel'); % On recupere l'objet texte associe au titre de l'axe des X
    	ytitle = get(gca,'YLabel'); % On recupere l'objet texte associe au titre de l'axe des Y
    	ztitle = get(gca,'ZLabel'); % On recupere l'objet texte associe au titre de l'axe des Z
    	if size <= 0
               size = 1;
           end
    	set(gca,'FontSize',coefTailleCar*size);
        % TRAITEMENT de XCHARSIZE ou !X.CHARSIZE
        if ~isempty(xsize)
            % le KG XCHARSIZE a ete passe en parametres
            if xsize <= 0
    	        set(xtitle,'FontSize',coefTailleCar*size);
            else
       			set(xtitle,'FontSize',coefTailleCar*size*xsize);
       		end
        else
            % On tient compte de la variable systeme !X.CHARSIZE
    	    if i2mvs_x.charsize <= 0
    		    set(xtitle,'FontSize',coefTailleCar*size);
            else
    			set(xtitle,'FontSize',coefTailleCar*size*i2mvs_x.charsize);
            end
        end
        % TRAITEMENT de YCHARSIZE ou !Y.CHARSIZE
        if ~isempty(ysize)
            % le KG YCHARSIZE a ete passe en parametres
            if ysize <= 0
    			set(ytitle,'FontSize',coefTailleCar*size);
           	else
       			set(ytitle,'FontSize',coefTailleCar*size*ysize);
       		end
        else
            % On tient compte de la variable systeme !Y.CHARSIZE
    		if i2mvs_y.charsize <= 0
    		    set(ytitle,'FontSize',coefTailleCar*size);
    		else
    		    set(ytitle,'FontSize',coefTailleCar*size*i2mvs_y.charsize);
    		end
        end
        if ~isempty(zsize)
            % le KG ZCHARSIZE a ete passe en parametres
            if zsize <= 0
    			set(ztitle,'FontSize',coefTailleCar*size);
           	else
       			set(ztitle,'FontSize',coefTailleCar*size*zsize);
       		end
        else
            % On tient compte de la variable systeme !Z.CHARSIZE
    		if i2mvs_z.charsize <= 0
    		    set(ztitle,'FontSize',coefTailleCar*size);
    		else
    		    set(ztitle,'FontSize',coefTailleCar*size*i2mvs_z.charsize);
    		end
        end
    end
end
