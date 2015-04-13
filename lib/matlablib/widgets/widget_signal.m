function widget_signal(base,freq)
%*********************
%**
%TO DO:
%Problem, when signal used and mousebutton used + interupt while execute a command line

global I2Mfig I2Mfevn I2Mftim I2Mserial

%I2Mfig(wid,*)= 1=wid, 2=parent, 3=fig, 4=lay, 5=map, 6=uic, 7:10=pos, 11=top ,12=seq ,13=freq, 14=rest, 15=sens
%lay= 1:column, 2:row, 3:none
%uic= 1:base,   2:uic, 3:fig

if base > 0; %COME FROM WIDGET_CONTROL.
	wid=double(int16(base));
	if freq <= 0; I2Mfig(wid,13:14)=0;
	else;	I2Mfig(wid,13:14)=freq;
		if I2Mftim(1) == 0;
			freq=max([freq,0.01]);
			I2Mftim(1)=wid; I2Mftim(2)=freq;
			if isempty(I2Mserial);
			   freq=round(freq); if freq ==0; freq=1; end;
			   I2Mftim(2)=freq ; setSignal(num2str(freq));
			else, try,
				%***TIMER  callback method***
				%if  I2Mserial.Status == 'open',    fclose(I2Mserial); end;
				%set(I2Mserial,'TimerPeriod',freq); fopen (I2Mserial);
				
				%***TIMOUT callback method***
				 if  I2Mserial.Status ~= 'open',    fopen(I2Mserial); end;
				 set(I2Mserial,'Timeout',freq); readasync(I2Mserial);
				 
			      catch, I2Mftim(1)=0; disp(lasterr); fclose(I2Mserial); end; end;
		end;
	end;

else;              %COME FROM SIGNAL INTERUPT
     fii=I2Mftim(1);
     if fii > 0;
	I2Mfig(fii,13:14)=0;
	widget_i2mevent(struct('structure_name','WIDGET_TIMER','id',fii));
	
	kep=I2Mfig(fii,13);         I2Mfig(fii,13)=0;
	idx=find(I2Mfig(:,13) > 0); I2Mfig(fii,13)=kep;
	if (idx);
	  I2Mfig(idx,14)=I2Mfig(idx,14)-I2Mftim(2);
	  for i=1:length(idx);
		if I2Mfig(idx(i),14) <= 0; widget_i2mevent(struct('structure_name','WIDGET_TIMER','id',idx(i))); end;
	  end;
	end;
	I2Mftim(1)=0;
	idx=find(I2Mfig(:,14) > 0);
	if (idx);
		[tmp,srt]=sort(I2Mfig(idx,14));
		fii =idx(srt(1));
		rest=I2Mfig(fii,14);
		rest=max([rest,0.01]);
		I2Mftim(1)=fii; I2Mftim(2)=rest;
		if isempty(I2Mserial);
		   rest=round(rest); if rest ==0; rest=1; end;
		   I2Mftim(2)=rest; setSignal(num2str(rest));
		else, try,
				%***TIMER  callback method***
				%if  I2Mserial.Status == 'open',    fclose(I2Mserial); end;
				%set(I2Mserial,'TimerPeriod',rest); fopen (I2Mserial);
				
				%***TIMOUT callback method***
				 if  I2Mserial.Status ~= 'open',    fopen(I2Mserial); else, stopasync(I2Mserial); end;
				 set(I2Mserial,'Timeout',rest); readasync(I2Mserial);
				 
		      catch, I2Mftim(1)=0; disp(lasterr); fclose(I2Mserial); end; end;
	end;
     else, fclose(I2Mserial); end;
end;
