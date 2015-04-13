function widget_i2mrec(wid,flag)
%*********************
%**
global I2Mfig I2Mfevn I2Mnotr I2Mreg I2Mfgap I2Mfcar axes_wd

%I2Mfig(wid,*)= 1=wid, 2=parent, 3=fig, 4=lay, 5=map, 6=uic, 7:10=pos, 11=top ,12=seq, 13=freq, 14=rest, 15=sens ,16=but ,17=grp ,18=bgc
%lay= 1:column, 2:row, 3:none
%uic= 1:base,   2:uic, 3:fig
%but= 0:normal  1:excl 2:nonexcl 3:menu 4:bar

if wid > 0; fii=double(int16(wid));
idx=find(I2Mfig(:,2) == wid);
if (idx);
    [tmp,srt]=sort(I2Mfig(idx,12)); idx=idx(srt); end;
    lay=I2Mfig(fii,4);
    if flag == 'basmap'; if ~I2Mfig(fii, 5); if I2Mfig(fii,6) ~= 3; flag='nomap';   end; end; end;
    if flag == 'basens'; if ~I2Mfig(fii,15); flag='nosens'; end;
                         if ~isempty(I2Mnotr{fii}), eval([I2Mnotr{fii} '(wid)'],'');end; end;
    for i=1:n_elements(idx);
        if flag == 'baspos';
            moi=idx(i);
            I2Mfig(moi,8)=I2Mfig(fii,8)+I2Mfig(moi,8);
            I2Mfig(moi,7)=I2Mfig(fii,7)+I2Mfig(moi,7);
            if i > 1; if lay ==1; I2Mfig(moi,8)=I2Mfgap+I2Mfig(idx(i-1),8)+I2Mfig(idx(i-1),10); end;
                      if lay ==2; I2Mfig(moi,7)=I2Mfgap+I2Mfig(idx(i-1),7)+I2Mfig(idx(i-1), 9); end; end;
        end;
        down=I2Mfig(idx(i),1);
        widget_i2mrec(down,flag); %Recursive process over bases and uics.
                                  %*************************************
    end;
end;
switch flag;
case 'del';   try, delete(wid); catch; end;
              if I2Mfig(fii,6) == 3; try, delete(I2Mfig(fii,3)); catch; end;
                 if isstruct(I2Mreg), names=fieldnames(I2Mreg); else names=[]; end;
                 for i=1:length(names), woo=getfield(I2Mreg,names{i}); if woo == wid, I2Mreg=rmfield(I2Mreg,names{i}); end; end;
                 idd=find(I2Mfig(:,17) == fii);
                 if (idd), for i=1:n_elements(idd);
                           widget_i2mrec(I2Mfig(idx(i),1),flag); end; end; %Recursive process (group_leader)
              end;                                                         %*****************
              I2Mfig(fii,:)=0; I2Mfevn{fii}='';
case 'map';   if I2Mfig(fii,6) == 2; if I2Mfig(fii, 5); try, set(wid,'visible','on'); catch; end; end; end;
              if I2Mfig(fii,6) == 3; try, set(I2Mfig(fii,3),'visible','on'); catch; end; end;
case 'nomap'; if I2Mfig(fii,6) == 2; try, set(wid,'visible','off'); catch; end; end;
              if I2Mfig(fii,6) == 3; try, set(I2Mfig(fii,3),'visible','off'); catch; end; end;
case 'sens';  if I2Mfig(fii,6) == 2; if I2Mfig(fii,15); try, set(wid,'enable','on');   catch; end; end; end;
case 'nosens';if I2Mfig(fii,6) == 2; try, set(wid,'enable','off');  catch; end; end;
case 'create';if I2Mfig(fii,6) == 2 & I2Mfig(fii,16)  ~=4;
				      I2Mfig(fii,19:20)=0;
				     widget_i2mpos(int16(I2Mfig(fii,2)), I2Mfig(fii,7:10), 'create'); end;
case 'basdim';if I2Mfig(fii,6) == 1; widget_i2mpos(int16(I2Mfig(fii,2)), I2Mfig(fii,7:10), 'basdim'); end;
case 'revers';if I2Mfig(fii,6) ~= 0; widget_i2mpos(fii,  0, 'revers');
                                     try, tmp=get(wid,'type'); if (tmp == 'uicontrol') | (tmp == 'axes'),
                                     if I2Mfig(fii,9)  < 1, I2Mfig(fii,9) =1; end;
                                     if I2Mfig(fii,10) < 1, I2Mfig(fii,10)=1; end;
                                     ppa=I2Mfig(fii,7:10); 
                                     if tmp == 'axes'
                                         [i,j]=find(axes_wd == wid);
                                         if i==1
                                            ppa=ppa+[I2Mfcar(1)*8,I2Mfcar(2)*3,-I2Mfcar(1)*10,-I2Mfcar(2)*5]; 
                                        end;
                                         set(wid,'position',ppa,'xcolor',get(wid,'color'),'ycolor',get(wid,'color'));
                                     else
                                        set(wid,'position',ppa); 
                                    end; end; catch; end; end;
case 'event';
otherwise;
end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
