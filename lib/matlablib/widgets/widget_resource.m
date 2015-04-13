function bgc=widget_resource(wid,bgc,resource_name,map,sens)
%******* *******************
%**

global i2m_wcolor i2m_wcolorOn

if resource_name,  switch resource_name,
	case 'lamp'; bgc=1; case 'ben';    bgc=2; case 'mic';    bgc=3; case 'don';    bgc=4;
	case 'touch';bgc=5; case 'lampben';bgc=6; case 'lampmic';bgc=7; case 'lampdon';bgc=8;
	case 'lampdid';bgc=9;
	otherwise; end;
	end;

if ~map;  set(wid,'visible','off'); end;
if ~sens; set(wid,'enable' ,'off'); end;

typ=get(wid,'type');
if typ == 'uicontrol', styl=get(wid,'style'); switch styl,
    case 'frame';       
        if i2m_wcolorOn == 1
            set(wid,'BackgroundColor',i2m_wcolor);
        else
            set(wid,'BackgroundColor',[0 1 1]);
        end
    case 'pushbutton';
        if i2m_wcolorOn == 1
            set(wid,'BackgroundColor',i2m_wcolor ,'ForegroundColor',[0 0 0]);
        else
            set(wid,'BackgroundColor',[.2 .6 .9] ,'ForegroundColor',[0 0 0]);
        end
    case 'radiobutton';
        if i2m_wcolorOn == 1
            set(wid,'BackgroundColor',i2m_wcolor);
        else
            set(wid,'BackgroundColor',[0 1 1]);
        end
    case 'checkbox';
        if i2m_wcolorOn == 1
            set(wid,'BackgroundColor',i2m_wcolor);
        else
            set(wid,'BackgroundColor',[0 1 1]);
        end
    case 'edit';
        if i2m_wcolorOn == 1
            set(wid,'BackgroundColor',i2m_wcolor);
        else
            set(wid,'BackgroundColor',[0.5 1 1]);
        end
    case 'text';
        if i2m_wcolorOn == 1
            set(wid,'BackgroundColor',i2m_wcolor);
        else
            set(wid,'BackgroundColor',[0 1 1]);
        end
    case 'slider';      set(wid,'BackgroundColor',[0.6 1 1]);
    case 'listbox';     set(wid,'BackgroundColor',[0.8 1 1]);
    case 'popupmenu';   set(wid,'BackgroundColor',[0 0.5 1]);
    otherwise; end;
end;

