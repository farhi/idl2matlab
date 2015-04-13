%***************************
function d=dial_mydial3_macro(d)
%***************************
%**

    if d.init == 0; d.init=1;
    	 d.frequency=2.;
    	 disp([d.name 'First call']); end;

    if (xregistered(d.name) <= 0) | (d.fig <= 0);
	bas  =widget_base ('title',d.name);
	d.fig=widget_draw ('I2M_a1',bas,'xsize',300,'ysize',150);
	widget_control('I2M_a1',bas, 'realize',1);
	xmanager('I2M_a1',d.name, 'I2M_a2',bas);
	
        d.h=surf(d.peaks);
	set(d.fig,'FontWeight','bold');
	axes(d.fig);
	xlabel('Stage UFR-IMA');
	zlabel('elevation');
	title('Peaks(20)');
    end;
    d.value=d.cnt*2;
    d.cnt=d.cnt+1;
    
	zdir = [1 0 0];
	center = [10 10 0];
	rotate(d.h,zdir,d.value,center);