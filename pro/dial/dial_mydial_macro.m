%***************************
function d=dial_mydial_macro(d)
%***************************
%**

    if d.init == 0; d.init=1; disp([d.name 'First call']);
       d.frequency=2.;
    end;
    if (xregistered(d.name) <= 0) | (d.fig <= 0);
	bas  =widget_base ('title',d.name);
	d.fig=widget_draw ('I2M_a1',bas,'xsize',300,'ysize',150);
	widget_control('I2M_a1',bas, 'realize',1);
	xmanager('I2M_a1',d.name, 'I2M_a2',bas);
    end;
    v=dialnewvalue('type','status','setvalue',1); %big problem of setvalue as the macro returns d !!!!!
    d.value=v;
    c=dialcontrol('test');
    
    plot(sin([d.cnt:d.cnt+50]),'-r','LineWidth',2,'parent',d.fig); d.cnt=d.cnt+1;
	set(d.fig,'FontWeight','bold');
	axes(d.fig);
	xlabel('Stage UFR-IMA');
	ylabel('sin(\Theta)');
	title('Plot of sin(\Theta)');
