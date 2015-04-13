function widget_i2mpos(ibas,pos,flag)
%******* *************
%**
global I2Mfig I2Mfgap

%I2Mfig(wid,*)= 1=wid, 2=parent, 3=fig, 4=lay, 5=map, 6=uic, 7:10=pos, 11=top, 12=seq, 13=freq, 14=rest, 15=sens ,16=but ,17=grp ,18=bgc
%lay= 1:column, 2:row, 3:none
%uic= 1:base,   2:uic, 3:fig
%but= 0:normal  1:excl 2:nonexcl 3:menu 4:bar

lay =I2Mfig(ibas,4); bos=I2Mfig(ibas,7:10);

switch flag;
case 'create';	% BASE container of UICs
    if lay == 1; bos(4)=bos(4)+pos(4)+I2Mfgap; end;
    if lay == 2; bos(3)=bos(3)+pos(3)+I2Mfgap; end;
    if bos(3) <  pos(3);bos(3)=pos(3); end;
    if bos(4) <  pos(4);bos(4)=pos(4); end;
    I2Mfig(ibas,7:10)=bos;
case 'basdim';	% container of BASE
    if lay == 1; bos(4)=bos(4)+pos(4)+I2Mfgap; end;
    if lay == 2; bos(3)=bos(3)+pos(3)+I2Mfgap; end;
    if bos(3) <  pos(3);bos(3)=pos(3); end;
    if bos(4) <  pos(4);bos(4)=pos(4); end;
    I2Mfig(ibas,7:10)=bos;
case 'revers';
    hight=I2Mfig(I2Mfig(ibas,11),10); highb=I2Mfig(ibas,10);
    I2Mfig(ibas,8)=hight-I2Mfig(ibas,8)-highb;
otherwise;
end;
