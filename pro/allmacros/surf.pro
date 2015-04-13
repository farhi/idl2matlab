	pro surf4, plx,ply,dm,vsz,ni,ah,nc,lev,flg,bg,tare,erey,sbox
;...	---------- -----
;...
	bh=0. & dt =0. & ff=0. & hd=0. & hk=0. & kd=0. & ke=0. & kp=0.
	ks=0. & ni1=0. & np=0. & ns=0. & pv=0.
;	implicit  integer*4 (a-z)
;	tare=intarr(plx/(ni+1) , dm , vsz)
;	erey=intarr(plx	    , ply)
;	sbox=lonarr(  4	    ,  3 )
	xare=intarr(plx  	,  3 )
	z   =0B

	catch,stat & if stat ne 0 then begin print,!err_string & return & endif

;...Prepare the loop
;...------- --- ----
	dm1=dm-1
	dlx=plx /(ni+1)
	bh =float( ply)    /dm   * ah  /90.
        ho =fix  ( ply-1 -  dm1  * bh)
	ho1= ho
	if  (ho gt ply/2) then ho1=3*ply/4 - ho/2
	hd = float(ho1)  /  nc
;	dpl= ply1 - (ply/4) * (float(90-ah)/90)
	ff = float(ho)   /  dm1
	fi = fix  (ff)
	ic =nc
	ic1=ic-1
;
	iv =3
	nd =nc/iv
	dt =float(nc-nd)/nc
;
	nd =nd-1
	nv =nd/2 +1
	nw =nv*5/4
	nw1=nv*3/2
	t  =nv*4/5
	t1 =t+1
	if (hd gt 0) then begin
			  np=2./(3.*hd)
			  ns=1./(3.*hd)
		     endif else begin
			  hd=0.001
			  np=0.
			  ns=0.
		     endelse
	ply1= ply-1
	dlx1= dlx-1
	ni1 = ni+1.
	ho2 = ho/2
	ho3 = ho/3
;...	First line
;...	----- ----
	if (bg ne 0) then begin
	    di= 0
	    p = ply1
	    od= 1
	    dz= 0
	    if ((flg eq 6) or (bg eq 1)) then od=0
	    if  (flg eq 6) then dz= vsz-1
	    FOR dx=0,dlx1 do begin
		dj=dm1
		v =0
		while ((dj gt 0) and (v eq 0)) do begin
		   v =tare(dx,dj,dz)
		   dj=dj-1
		endwhile
		if (dj eq 0) then begin
				dv=ply1
				v =1
			     endif else begin
				if (lev lt 0)	then     $
						 dj=dj+1 $
						else     $
						 FOR cg=0,vsz-1 do $
						 tare(dx,dj+1,cg)= 0
				v =tare (dx, dj, 0)
				kd=(dm1- dj)
				l =fix(ply1-(kd* bh))
				if (od eq 1) then begin
				    hk=(1. - kd/(kd+dm1)) * hd
				    dv= l  -(v * hk)
				endif else $
				    dv= l  -(v * hd)
				v =nd
				if (p eq ply1) then v=1
			     endelse
		pv=(dv-p) /ni1
		FOR cg=ni,0,-1 do begin
		    d =fix(dv -(cg*pv))>0
		    xare(di,2) = d
		    erey(di,d) = v
		    di=di+1
		ENDFOR
		p=d
	    ENDFOR
	endif else begin
;		FOR di=0,plx-1 do begin
;		    xare(di,2)=ply1
;		    erey(di,ply1)=1
;	        ENDFOR
		xare(*,2)=ply1
		erey(*,ply1)=1
	endelse
	kd=0.
	od=0
	ke=1.
	vsz1= vsz-1
;...
;...	Surfaces.
;...	--------
	if (flg eq 6) then begin
	cs =nd*2/vsz
	FOR dj= dm1 ,0,-1 do begin
	    l = fix(ply1 - (kd*bh))>0
	 FOR dz= 0 , vsz1 do begin
	    v = tare(0,  dj ,dz)
	    p = fix(l-(v*hd))>0
	    di= 0
	    z = (dz eq vsz1)
	    cz= nd + cs*(dz+1)
	    FOR dx=0,dlx1 do begin
		v = tare(dx,dj,dz)
		dv= l - (v *hd)
		pv=(dv-p) /ni1
		FOR cg=ni,0,-1 do begin
		    dd=fix(dv -(cg*pv))>0
		    d =dd
		    if (z) then $
			    if  (d gt 0) then d=d-1
		    if (xare(di,2) gt d) then begin
		         if ((erey(di,d) lt nw) and (d lt l)) then begin
				if (z)   then $
					 erey(di,d)=nw $
				else $
					 erey(di,d)=nw1
				e=d +1
				while (e le l) do begin
				  if     (erey(di,e) lt nw) then begin
				   if    ((e lt p)  and z)  then $
					  erey(di,e )=nw $
					else $
					  erey(di,e )=cz
				   e=e+1
				  endif else $
				   e=l+1
				endwhile
			 endif
		    endif
		    if ((d-1 gt p) and (p lt xare(di,2))) then begin
		    e=p+1
		    while ((erey(di,e) lt nw) and (e lt d)) do begin
			       erey(di,e )=nw
			       e=e+1
		    endwhile
		    endif
		    p =dd
		    di=di+1
		ENDFOR
	    ENDFOR
	 ENDFOR
	 kd=kd + 1.
	ENDFOR
	endif
;...
;...	4D Surface.
;...	----------
	if (flg eq 8) then begin
	FOR dj= dm1 ,0,-1 do begin
	    l = fix(ply1 - (kd*bh))>0
	    if (bg ne 1) then ke= 1. -kd/(kd+dm1)
	    hk= hd  *ke
	    kp= np  /ke
	    ks= ns  /ke
	    v = tare(0,  dj, 0)
	    p = fix(l-(v*hk))>0
	    di= 0
	    hi= fix(ho-(dj*ff))
	    hj= hi+ fi
	    FOR dx=0,dlx1 do begin
		va= v
		v = tare(dx,dj,0)
		vh= v/iv -2
		dv= l - (v *hk)
		pv=(dv-p) /ni1
		FOR cg=ni,0,-1 do begin
		    d =fix(dv -(cg*pv))>0
		    od=-od+4
		    if (xare(di,2) gt d)  then begin
			if ((erey(di,d) lt nd) and (d lt l)) then begin
			        erey(di,d)= nd
				e=d +1
				t1=tare(dx,dj,1)*dt
				t =0
				while (e le l) do begin
				  if     (erey(di,e) lt nd) then begin
				   if    (e lt p)   then $
					  erey(di,e )=nd $
					else begin
					  v=t1-t + nd-od
					  if (v lt nd) then v=nd
					  erey(di,e) = v
					endelse
				   t=t+1
				   e=e+1
				  endif else $
				   e=l+1
				endwhile
			endif
		    endif else begin
			if ((v gt 0) and (va gt 0) and $
					 (xare(di,2) lt d)) then begin
			    t1=tare(dx,dj,1)/iv
			    t =0
			    e =d
			    while (erey(di,e) eq 0) do begin
				      v=t1+t + od
				      if (v ge nd) then v=nd-1
				      erey(di,e)=  v
				      t=t+1
				      e=e-1
			    endwhile
                        endif
		    endelse
		    if ((d-1 gt p) and (p lt xare(di,2))) then begin
		      e= p+1
		      while  ((erey(di,e) lt nd) and (e lt d)) do begin
				  erey(di,e ) = nd
				  e=e+1
		      endwhile
		    endif
		    if (bg eq 2) then begin
                    FOR oz=hi,hj  do  begin
		        if (erey(di,oz) lt vh) then $
			if (xare(di,2)  gt oz) then erey(di,oz)=vh
		    ENDFOR
		    endif
		    p =d
		    di=di+1
		ENDFOR
	    ENDFOR
	    kd=kd + 1.
	ENDFOR
	endif

	return
	end








	pro surf,  plx,ply,dm,vsz,ni,ah,nc,lev,flg,bg,tare,erey,sbox
;...	---------- -----
;...
	bh=0. & dt =0. & ff=0. & hd=0. & hk=0. & kd=0. & ke=0. & kp=0.
	ks=0. & ni1=0. & np=0. & ns=0. & pv=0.
;	implicite integer*4 (a-z)
;	tare=intarr(plx/(ni+1) , dm )
;	erey=intarr(plx	    , ply)
;	sbox=lonarr(  4	    ,  3 )
	xare=intarr(plx	    ,  3 )
	z=0B & z2=0B

	catch,stat & if stat ne 0 then begin print,!err_string & return & endif

;...Prepare the loop
;...------- --- ----
	dm1=dm-1
	dlx=plx /(ni+1)
	bh =float( ply)  /  dm   * ah  /90.
	ho =fix  ( ply-1 -  dm1  * bh)
	ho1= ho
	if  (ho gt ply/2) then ho1=3*ply/4 - ho/2
	hd = float(ho1)  /  nc
;	dpl= ply1 - (ply/4) * (float(90-ah)/90)
;	ho = ho- (ply1-dpl)
	ff = float(ho)   /  dm1
	fi = fix  (ff)
	ic =nc
	ic1=ic-1

	iv =3
	if (flg eq 7) then iv=5
	nd =nc/iv
	dt =float(nc-nd)/nc

	nd =nd -1
	nv =nd/2 +1
	nw =nv*5/4
	nw1=nv*3/2
	t  =nv*4/5
	t1 =t+1
	if (hd gt 0) then begin
			  np=2./(3.*hd)
			  ns=1./(3.*hd)
	endif else begin
			  hd=0.001
			  np=0.
			  ns=0.
	endelse
	ply1= ply-1
	dlx1= dlx-1
	ni1 = ni+1.
	ho2 = ho/2
	ho3 = ho/3
	if (vsz gt 1)   then begin
	    surf4,plx,ply,dm,vsz,ni,ah,nc,lev,flg,bg,tare,erey,sbox
	endif else begin

;...First line
;...----- ----
	if ((flg ne 4) and (flg ne 10) and (flg ne 11)) then begin
	 if (bg  ne 0) then begin
	    di= 0
	    p = ply1
	    od= 1
	    if ((flg eq 0) or (flg eq 3) or (bg eq 1))  then od=0
		FOR  dx=0,dlx1 do begin
		dj=dm1
		v =0
		while ((dj gt 0) and (v eq 0)) do begin
		   v =tare(dx , dj)
		   dj=dj-1
		endwhile
		if (dj eq 0) then begin
				dv=ply1
				v =1
		endif else begin
				if (lev lt 0)	then begin
						dj = dj+2
		    				if  (dj gt dm1) then dj=dm1 $
						else        tare(dx,dj+1)=0
				endif
				v =tare (dx, dj)
				kd=(dm1- dj)
				l =ply1-(kd* bh)
				if (od eq 1) then begin
				    hk=(1. - kd/(kd+dm1)) * hd
				    dv= l  -(v * hk)
				endif else $
				    dv= l  -(v * hd)
				v =nd
				if (p eq ply1) then v=1
		endelse
		pv=(dv-p) /ni1
		FOR cg=ni,0,-1 do begin
		    d =fix(dv -(cg*pv))>0
		    xare(di,2) = d
		    erey(di,d) = v
		    di=di+1
		ENDFOR
		p=d
	    ENDFOR
	 endif else begin
		xare(*,2)   =ply1
		erey(*,ply1)=1
		;FOR di=0,plx-1 do begin
		;    xare(di,2)=ply1
		;    erey(di,ply1)=1
		;ENDFOR
	 endelse
	endif
	kd=0.
	od=0
	ke=1.
;...
;...	Filled vectors loop.
;...	------ ------- ----
	if (flg eq 1) then begin
	l=ply1
	FOR dj= dm1 ,0,-1 do begin
	    lp= l
	    l = fix(ply1 -  (kd*bh))>0
	    ld= lp-l
	    if (bg ne 1) then ke= 1. -kd/(kd+dm1)
	    hk= hd  *ke
	    kp= np  /ke
	    ks= ns  /ke
	    v = tare(0,  dj)
	    p = fix(l-(v*hk))>0
	    di= 0
	    hi= ho-fix(dj*ff)
	    hj= hi+ fi
	    FOR dx=0,dlx1 do begin
		 va= v
		 v = tare(dx,dj)
		 vh= v/iv -2
		 dv= l - (v *hk)
		 pv=(dv-p) /ni1
		 FOR cg=ni,0,-1 do begin
		    d =fix(dv -(cg*pv))>0
		    od=-od+4
		    if (xare(di,2) gt d)  then begin
			if ((erey(di,d) lt nd) and (d lt l)) then begin
			     erey(di,d)  = nd
			     e=d +1
			     while (e lt lp) do begin
				  if     (erey(di,e) lt nd) then begin
				   if    (e lt p)  then $
					   erey(di,e )=nd   $
				   else begin
					  if (e-d  lt  ld) then   $
					   erey(di,e )=(l-d)*kp +nd  $
					  else $
					   erey(di,e )=(l-e)*kp +nd-od
				   endelse
				   e=e +1
				  endif else $
				   e=lp+1
			     endwhile
			endif
		    endif else begin
			if ((v gt 0) and (va gt 0) and $
					 (xare(di,2) lt d)) then begin
			    e =d
			    while (erey(di,e) eq 0) do begin
				      erey(di,e)=  (l-e)*ks +1 + od
				      e=e-1
			    endwhile
               endif
		    endelse
		    if ((d-1 gt p) and (p lt xare(di,2))) then begin
		      e= p+1
		      while  ((erey(di,e) lt nd) and (e lt d)) do begin
				  erey(di,e ) = nd
				  e=e+1
		      endwhile
		    endif
		    if (bg eq 2) then begin
                  FOR oz=hi,hj  do begin
		            if (erey(di,oz) lt vh) then $
			       if (xare(di,2)  gt oz) then erey(di,oz)=vh
		        ENDFOR
		    endif
		    p =d
		    di=di+1
		 ENDFOR
	    ENDFOR
	    kd=kd + 1.
	ENDFOR
	endif
;...
;...	Filled Loop.
;...	------ ----
	if (flg eq 5) then begin
	l=ply1
	FOR dj= dm1 ,0,-1 do begin
	    ld= l
	    l = fix(ply1 -  (kd*bh))>0
	    ld= ld-l
	    if (bg ne 1) then ke= 1. -kd/(kd+dm1)
	    hk= hd  *ke
	    kp= np  /ke
	    ks= ns  /ke
	    v = tare(0,  dj)
	    p = fix(l-(v*hk))>0
	    di= 0
	    hi= ho-fix(dj*ff)
	    hj= hi+ fi
	    FOR dx=0,dlx1 do begin
		va= v
		v = tare(dx,dj)
		vh= v/iv -2
		dv= l - (v *hk)
		pv=(dv-p) /ni1
		FOR cg=ni,0,-1 do begin
		    d =fix(dv -(cg*pv))>0
		    od=-od+4
		    if (xare(di,2) gt d)  then begin
			if ((erey(di,d) lt nd) and (d lt l)) then begin
				e=d
				while (e le l) do begin
				  if     (erey(di,e) lt nd)  then begin
					  if (e-d  lt  ld) then $
 					    erey(di,e)=(l-d)  *kp+nd $
 					  else $
					    erey(di,e)=(l-e-1)*kp+nd-od
 					  e=e+1
				  endif else $
					  e=l+1
				endwhile
			endif
		    endif else begin
			   if ((v gt 0) and (va gt 0) and $
					 (xare(di,2) lt d)) then begin
			          e =d
			          while (erey(di,e) eq 0) do begin
				       erey(di,e)=  (l-e)*ks +1 + od
				       e=e-1
			          endwhile
               endif
		    endelse
		    if (bg eq 2) then begin
                    FOR oz=hi,hj do begin
		             if (erey(di,oz) lt vh) then $
			         if (xare(di, 2) gt oz) then erey(di,oz)=vh
		            ENDFOR
		    endif
		    p =d
		    di=di+1
		ENDFOR
	    ENDFOR
	    kd=kd + 1.
	ENDFOR
	endif
;...
;...	Shaded Loop.
;...	------ ----
	if (flg eq 7) then begin
;	FOR dx=0,plx-1  do begin
;	    xare(dx,0)=0
;	    xare(dx,1)=nd
;	ENDFOR
    xare(*,0)=0
    xare(*,1)=nd
	FOR dj= dm1 ,0,-1 do begin
	    l = fix(ply1 -  (kd*bh))>0
	    if (bg ne 1) then ke= 1. -kd/(kd+dm1)
	    hk= hd  *ke
	    kp= np  /ke
	    v = tare(0,  dj)
	    p = fix(l-(v*hk))>0
	    di= 0
	    hi= ho-fix(dj*ff)
	    hj= hi+ fi
	    FOR dx=0,dlx1 do begin
		va= v
		v = tare(dx,dj)
		vh= v/iv -2
		dv= l - (v *hk)
		pv=(dv-p) /ni1
		FOR cg=ni,0,-1 do begin
		    d =fix(dv -(cg*pv))>0
;...
		    ks=xare(di,0)-d-1
		    xare(di,0)=d
		    v1=xare(di,1)
		    if (ks le 0.)  then ks=0.
		    v2=ic1-(ks*ks/hk)
		    if (v2 lt nd)  then v2=nd
		    xare(di,1)=v2
		    if (ks gt 0.)  then ks=(v1-v2)/ks
;...
		    if (xare(di,2) gt d)  then begin
			if ((erey(di,d) lt nd) and (d lt l)) then begin
				e =d
				v1=0
				while (e le l) do begin
				  if     (erey(di,e) lt nd) then begin
					  erey(di,e)=v2 + (v1*ks)
					  e =e +1
					  v1=v1+1
				  endif else $
					  e=l+1
				endwhile
			endif
		    endif else begin
			if ((v gt 0) and (va gt 0) and $
					 (xare(di,2) lt d)) then begin
			     e =d
			     v1=0
			     while (erey(di,e) eq 0) do begin
				       erey(di,e)=v1*ks
				       e =e -1
				       v1=v1+1
			     endwhile
                        endif
		    endelse
		    if (bg eq 2) then begin
                    FOR oz=hi,hj do begin
		        if (erey(di,oz) lt vh) then $
			if (xare(di,2)  gt oz) then erey(di,oz)=vh
		    ENDFOR
		    endif
		    p =d
		    di=di+1
		ENDFOR
	    ENDFOR
	    kd=kd + 1.
	ENDFOR
	endif
;...
;...	Simple Loop
;...	------ ----
	if (flg eq 0) then begin
	FOR dj= dm1 ,0,-1 do begin
	    l = fix(ply1  -(kd *bh))>0
	    v = tare(0 ,dj)
	    p = fix(l - (v *hd))>0
	    di= 0
	    FOR dx=0,dlx1 do begin
		v =tare (dx,dj)
		dv= l - (v *hd)
    		pv=(dv-p) /ni1
		FOR cg=ni,0,-1 do begin
		    d =fix(dv - (cg*pv))>0
		    if (xare(di,2) gt d)  then begin
			if ((erey(di,d) lt nw) and (d lt l)) then begin
				erey(di,d)=nw
				e=d +1
				while (e le l) do begin
				  if   (erey(di,e) lt nw) then begin
				   if  (e lt p) then $
						erey(di,e)=nw $
						else $
						erey(di,e)=(l-e)*np+nd
				   e=e+1
				  endif else $
				   e=l+1
				endwhile
			endif
		    endif
		    if ((d-1 gt p) and (p lt xare(di,2))) then begin
		      e= p+1
		      while  ((erey(di,e) lt nw) and (e lt d)) do begin
				  erey(di,e ) = nw
				  e=e+1
		      endwhile
		    endif
		    p =d
		    di=di+1
		ENDFOR
	    ENDFOR
	    kd=kd + 1.
	ENDFOR
	endif
;...
;...	Vectors loop
;...	------- ----
	if (flg eq 3) then begin
	FOR dj= dm1 ,0,-1 do begin
	    l = fix(ply1  -(kd *bh))>0
	    v = tare(0 ,dj)
	    p = fix(l - (v *hd))>0
	    di= 0
	    FOR dx=0,dlx1 do begin
		va= v
		v = tare(dx,dj)
		dv= l - (v *hd)
    		pv=(dv-p) /ni1
		FOR cg=ni,0,-1 do begin
		    d =fix(dv - (cg*pv))>0
		    if (xare(di,2) gt d) then begin
			 if (d lt l)	 then begin
			    if (erey(di,d) eq 0 )  then begin
				erey(di,d)=(l-d)*np +nd
				e=d +1
				while (e le l) do begin
				  if   (erey(di,e) eq 0 ) then begin
				   if  (e lt p) then $
						erey(di,e)=(l-e)*np+nd  $
						else $
						if (cg eq 2) then $
						 erey(di,e)=(l-e)*np+nd $
						 else $
						 erey(di,e)=t1
				   e=e+1
				  endif else $
				   e=l+1
				endwhile
                endif
			    if ((xare(di,2) lt p) and (va gt 0)) then begin
				e=p-1
				while (erey(di,e) eq 0) do begin
					  erey(di,e)=(l-e)*ns +1
					  e=e-1
				endwhile
			    endif
		     endif
		    endif else begin
			if ((v gt 0) and (va gt 0)) then begin
			     e=d
			     if (d lt p) then e=p-1
			     if (xare(di,2) lt e) then begin
			      while (erey(di,e) eq 0) do begin
				    if (e ge d) then $
					erey(di,e)=(l-e)*ns +1 $
				    else $
					erey(di,e)=t1
				    e=e-1
			      endwhile
			     endif
			endif
		    endelse
		    e=d-1
		    if  ((e gt p) and (v gt 0)) then begin
		      if (e gt xare(di,2)) then begin
			    while (erey(di,e) eq 0) do begin
				  erey(di,e )=(l-e)*ns +1
				  e=e-1
			    endwhile
		      endif
		      e=p+1
		      while  ((erey(di,e) eq 0) and (e lt d)) do begin
				  erey(di,e )=(l-e)*np +nd
				  e=e+1
		      endwhile
		    endif
		    p =d
		    di=di+1
		ENDFOR
	    ENDFOR
	    kd=kd + 1.
	ENDFOR
	endif
;...
;...	Solid
;...	-----
	if (flg eq 4) then begin
	FOR dj= dm1 ,0,-1 do begin
	    l = fix(ply1 -  (kd*bh))>0
	    if (bg ne 1) then ke= 1. -kd/(kd+dm1)
	    hk= hd  *ke
	    kp= np  /ke
	    v = tare(0,  dj)
	    p = fix(l-(v*hk))>0
	    di= 0
	    FOR dx=0,dlx1 do begin
		  v = tare(dx,dj)
		  dv= l - (v *hk)
		  pv=(dv-p) /ni1
		  FOR cg=ni,0,-1 do begin
		    d =fix(dv -(cg*pv))>0
		    od=4-od
		    f =(l-d+1)/2
		    if (f gt 0) then begin
				e=l-ho2-f
				f=(l-ho2+f)<ply1
				g=(f-e-1)*kp +nd-od
				while (e le f) do begin
				   if (erey(di,e) eq 0) then erey(di,e)=g
				   e=e+1
				endwhile
		    endif
		    p =d
		    di=di+1
		  ENDFOR
	    ENDFOR
	    kd=kd + 1.
	ENDFOR
	endif
;...
;...	Levels:filled
;...	------ ------
	if (flg eq 10) then begin
	ic =(ic - nd +1)/lev
	ic1= ic / 3
	if  (ic lt 1) then ic=1
	FOR dj= dm1 ,0,-1 do begin
	    l = fix(ply1  -(kd *bh))>0
	    di= 0
	    FOR dx=0,dlx1 do begin
		v = fix(tare(dx,dj) *dt)
		if (v gt ic1)  then begin
			 v = (v/ic)*ic
			 d = fix(l - (v*hd))>0
			 v =  v +  nd
		endif else begin
			 v =  1
			 d =  l
		endelse
		FOR cg=ni,0,-1 do begin
		    if  (erey(di,d) lt 1) then begin
				if (di gt 0) then $
				  if (erey(di-1,d) ne v) then $
				  if (erey(di-1,d) ne 0) then $
						erey(di-1,d)=nw
				erey(di,d)=v
				e=d +1
				while  (e lt ply) do begin
				  if (erey(di,e) lt 1) then begin
				      if  (di    gt 0) then $
				  	 if  (erey(di-1,e) ne v) then $
					 if  (erey(di-1,e) ne 0) then $
						 erey(di-1,e)=nw
				      erey(di,e)=v
				      e=e+1
				  endif else begin
				      if  (erey(di,e) ne v)  then begin
				       if (erey(di,e) ne nw) then begin
					   erey(di,e-1) =nw
					   if (erey(di,e) lt v) then $
						 erey(di,e)=nw
				       endif
				      endif
				      e=ply
				  endelse
				endwhile
		    endif
		    di=di+1
		ENDFOR
	    ENDFOR
	    kd=kd + 1.
	ENDFOR
	endif
;...
;...	Levels:lines
;...	------ -----
	if (flg eq 11) then begin
;	FOR dx=0,dlx1 do begin
;	    xare(dx,0)=0
;	ENDFOR
	xare(*,0)=0
        ic =(ic - nd +1)/lev
	ic1= ic / 3
	if  (ic lt 1) then ic=1
	FOR dj= dm1 ,0,-1 do begin
	    l = fix(ply1  -(kd *bh))>0
	    di= 0
	    z2= 0
	    FOR dx=0,dlx1 do begin
		v = fix(tare(dx,dj) *dt)
		if (v gt ic1)  then begin
			 v = (v/ic)*ic
			 d =  fix(l - (v *hd))>0
			 v =  v +  nd
		endif else begin
			 v =  1
			 d =  l
		endelse
		z =(xare(dx,0) ne v)
		if (dx gt 0) then z2=(xare(dx-1,0) ne v)
		FOR cg=ni,0,-1 do begin
		    if  (erey(di,d) lt 1) then begin
				if (z2)   then begin
					  erey(di  ,d)=v
					  erey(di-1,d)=xare(dx-1,0)
				endif else $
					  erey(di,d)=1
				e=d +1
				while (e lt ply) do begin
				  if     (erey(di,e) lt 1) then begin
				      if (z2)  then begin
					    erey(di  ,e)=v
					    erey(di-1,e)=xare(dx-1,0)
				      endif else $
					    erey(di,e)=1
				      e=e+1
				  endif else begin
				   if (z) then begin
					  erey(di,e-1)=v
					  erey(di,e)  =xare(dx,0)
				   endif
				   e=ply
				  endelse
				endwhile
		    endif
		    di=di+1
		ENDFOR
	        xare(dx,0)=v
	    ENDFOR
	    kd=kd + 1.
	ENDFOR
	endif

	endelse

;...Horizon
;...*******
	if ((bg eq 2) and (ho gt 2) and (flg ne 4)) then begin
	     plx1 =plx -1
;	     g    =nv*4/5
	     ke   =float(nv-nv/3)/(ho+1)
		 FOR dj=0,ho  ,2 do begin
		 g =nv - dj*ke
		 FOR dx=0,plx1,2 do begin
		   if(erey(dx,dj) eq 0) then erey(dx,dj)=g
		 ENDFOR
		 ENDFOR
;...
;	     g    =nv*2/3
	     ke   =float(nv-nv/3)/(ply1-ho)
	     FOR dj=ho+1,ply1,2  do begin
		 g =nv -(ply1-dj)*ke
		 FOR dx=0   ,plx1,2  do begin
		   if(erey(dx,dj) le 1) then erey(dx,dj)=g
		 ENDFOR
		 ENDFOR
	endif
;...Deep
;...****
	if ((bg eq 2) and (ho gt 0) and (flg eq 4)) then begin
	     plx1 =plx -1
	     g    =nv
	     dx2  =plx1/2
	     dy2  =ply1/2
	     ke   =float(g)/(dx2+dy2)
	     FOR dj=0,ply1,2 do begin
		 l =dj-dy2
		 if(l lt 0) then l=-l
		 FOR dx=0,plx1,2 do begin
		   if(erey(dx,dj) eq 0) then begin
		      e=dx-dx2
		      if(e lt 0) then e=-e
		      erey(dx,dj) =(e+l)*ke
		   endif
		 ENDFOR
		 ENDFOR
	endif
;...Box a b
;...*** e c
	if ((bg eq 1) and (sbox(0,2) ge 0) and (ho1 gt 0)) then begin
	     ax   =sbox(sbox(0,2),0)
	     ay   =sbox(sbox(0,2),1)
	     bx   =sbox(sbox(1,2),0)
	     by   =sbox(sbox(1,2),1)
	     cx   =sbox(sbox(2,2),0)
	     cy   =sbox(sbox(2,2),1)
	     ex   =sbox(sbox(3,2),0)
	     ey   =sbox(sbox(3,2),1)
	     ho2  =(ply1-ey)/2
;...
	    if (ax lt bx) then begin
;...a -> b
		dt=float(ay-by)/(bx-ax)
		dy2=(ay-ho1)
		   FOR dj=dy2 , ay do begin
			erey(ax,dj)=nw
		   ENDFOR
		kd=dt
		FOR dx=ax+1,bx-1,3 do begin
		   FOR dj=dy2-kd , ay-kd,3 do begin
			if (erey(dx,dj) le 1) then erey(dx,dj)=nv
		   ENDFOR
		   kd =kd+3*dt
		ENDFOR
		   FOR dj=(dy2-kd)>0 , ay-kd do begin
			if (erey(bx,dj) le 1) then erey(bx,dj)=nw
		   ENDFOR
;...e -> c
		dt=float(ey-cy)/(cx-ex)
		kd=0.
		dy2=ey+ho2
		FOR dx=ex,cx,3 do begin
		   FOR dj=ey-kd , dy2-kd,3 do begin
			erey(dx,dj)=nv
		   ENDFOR
		   kd =kd+3*dt
		ENDFOR
	    endif

	    if (bx lt cx) then begin
;...b -> c
		dt=float(by-cy)/(cx-bx)
		kd=0.
		dy2=by-ho1
		FOR dx=bx,cx,3 do begin
		   FOR dj=dy2-kd , by-kd,3 do begin
			if (erey(dx,dj) le 1) then erey(dx,dj)=nv
		   ENDFOR
		   kd =kd+3*dt
		ENDFOR
;...a -> e
		dt=float(ay-ey+1)/(ex-ax+1)
		kd=0.
		dy2=ay+ho2
		FOR dx=ax,ex,3 do begin
		   FOR dj=ay-kd , dy2-kd,3 do begin
			erey(dx,dj)=nv
		   ENDFOR
		   kd =kd+3*dt
		ENDFOR

	    endif else begin

		FOR dj=by,cy do begin
			erey(ax,dj)=nv
			erey(bx,dj)=nv
		ENDFOR
		endelse
	endif
return
end

