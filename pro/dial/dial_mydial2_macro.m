%***************************
function d=dial_mydial2_macro(d)
%***************************
%**

    if d.init == 0; d.init=1; disp([d.name 'First call']);
        nam=''; nam=dialtag('I2M_a1','mydial','tag','name','get',nam,'I2M_pos',3);
    	if isempty(nam), dialinit ('mydial'); else, dialstart('mydial'); end;
        d.frequency=2.;
    end;

    if (xregistered(d.name) <= 0) | (d.fig <= 0);
	bas  =widget_base ('title',d.name);
	d.fig=widget_draw ('I2M_a1',bas,'xsize',300,'ysize',150);
	widget_control('I2M_a1',bas, 'realize',1);
	xmanager('I2M_a1',d.name, 'I2M_a2',bas);
    end;
    d.value=d.cnt/10.;

    plot(cos([d.cnt:d.cnt+50]./5),'-rs','LineWidth',2,'MarkerEdgeColor','k','MarkerFaceColor','g','parent',d.fig);
	set(d.fig,'FontWeight','bold');
	axes(d.fig);
	xlabel('Stage UFR-IMA')
	ylabel('cos(\Theta)')
	title('Plot of cos(\Theta)')
    d.cnt=d.cnt+1;
