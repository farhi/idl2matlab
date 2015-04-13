FUNCTION concdef,W1,W2
      Wold=W1
      Wout=W2
      take_datp,datpold
      take_datp,datpout,/second
      datp=datpout
      newy=datpout.y
      oldy=datpold.y
      xout=datpout.x
      oldx=datpold.x
      datp=datpout
      Eold=datpold.e
      Yold=oldy
      Xold=oldx
      IF MIN(newy) LT MIN(oldy) THEN y1=MIN(oldy) ELSE y1=MIN(newy)
      IF MAX(newy) LT MAX(oldy) THEN y2=MAX(newy) ELSE y2=MAX(oldy)
      PRINT,'Cells ',MIN(oldy),' to',MAX(oldy),' and',MIN(newy),' to',MAX(newy)
      PRINT,'Overlap between cells',y1,' to',y2
      oldyi=WHERE(oldy GT y1 AND oldy LT y2,oldyc)
      newyi=WHERE(newy GT y1 AND newy LT y2,newyc)
      PRINT,'overlap: ',oldyc,' cells from previous and',newyc,' from new data '
      stepwidth=[0.01,0.001,0.0001]
      steps=20
      squaredeviation=FLTARR(steps)
      scale=FLTARR(steps)
      IF oldyc*newyc GT 0 THEN BEGIN
        FOR counter=0,2 DO BEGIN
          step=0
          FOR shift=-stepwidth(counter)*9.5,stepwidth(counter)*9.5,stepwidth(counter) DO BEGIN
            xout=newx+shift
            IF MIN(xout) LT MIN(oldx) THEN x1=MIN(oldx) ELSE x1=MIN(xout)
            IF MAX(xout) LT MAX(oldx) THEN x2=MAX(xout) ELSE x2=MAX(oldx)
            oldi=WHERE(oldx GT x1 AND oldx LT x2,oldc)
            newi=WHERE(xout GT x1 AND xout LT x2,newc)
            IF oldc*newc*oldyc*newyc GT 0 THEN BEGIN
              overlap=[oldx(oldi),xout(newi)]
              overlap=overlap(SORT(overlap))
              oldtmp=FLTARR(oldyc,oldc)
              newtmp=FLTARR(newyc,newc)
              FOR j=0,oldyc-1 DO oldtmp(j,*)=Wold(j,oldi)
              FOR j=0,newyc-1 DO newtmp(j,*)=Wout(j,newi)
              oldinterpolated=FLTARR(oldyc+newyc,oldc+newc)
              newinterpolated=FLTARR(oldyc+newyc,oldc+newc)
              FOR j=0,oldyc-1 DO oldinterpolated(j,*) = INTERPOL(Wold(oldyi(j),*), oldx, overlap)
              FOR j=0,newyc-1 DO newinterpolated(j,*) = INTERPOL(Wout(newyi(j),*), xout, overlap)
              index=WHERE(oldinterpolated GT 0,elements)
              scale(step)=TOTAL(TOTAL(newinterpolated,1)/TOTAL(oldinterpolated,1))/N_ELEMENTS(newinterpolated(0,*))
              deviation=newinterpolated/scale(step)-oldinterpolated
              deviation=deviation*deviation
              squaredeviation(step)=TOTAL(deviation)/N_ELEMENTS(deviation)
              step=step+1
            ENDIF
          ENDFOR
          PRINT,'Least square deviation: ',MIN(squaredeviation,step),', shift:',(step-9.5)*stepwidth(counter),', scale:',scale(step) 
          shift=(step-9.5)*stepwidth(counter) 
          newx=newx+shift
        ENDFOR
        Wnew=[[Wold],[fltarr(N_ELEMENTS(Wold(*,0)),N_elements(Wout(0,*)))]]
        Enew=[[Eold],[fltarr(N_ELEMENTS(Wold(*,0)),N_elements(Wout(0,*)))]]
        FOR y=0,N_ELEMENTS(newy)-1 DO BEGIN
          index=WHERE(oldy EQ newy(y), exists)
          IF exists EQ 0 THEN BEGIN
            Wnew=[Wnew,FLTARR(1,N_ELEMENTS(Wnew(0,*)))]
            Enew=[Enew,FLTARR(1,N_ELEMENTS(Wnew(0,*)))]
            index=N_ELEMENTS(Wnew(*,0))-1
            oldy=[oldy,newy(y)]
          ENDIF
          Wnew(index,N_ELEMENTS(Wold(0,*)):N_ELEMENTS(Wnew(0,*))-1)=Wout(y,*)/scale(step)
          Enew(index,N_ELEMENTS(Wold(0,*)):N_ELEMENTS(Wnew(0,*))-1)=datp.e(y,*)/scale(step)
        ENDFOR
        sorted=SORT([oldx,newx])
        FOR y=0,N_ELEMENTS(oldy)-1 DO BEGIN
          Wnew(y,*)=Wnew(y,sorted)
          Enew(y,*)=Enew(y,sorted)
        ENDFOR
        sorted=SORT([oldy])
        oldy=oldy(sorted)
        FOR y=0,N_ELEMENTS([oldx,newx])-1 DO BEGIN
          Wnew(*,y)=Wnew(sorted,y)
          Enew(*,y)=Enew(sorted,y)
        ENDFOR
        x1=MIN([oldx,newx])
        x2=MAX([oldx,newx])
        x1=CEIL (x1*100.)/100.
        x2=FLOOR(x2*100.)/100.
        PRINT,'Interpolate for',x1,' to',x2,', new X from',MIN(Xold),' to',MAX(Xold)
        Yold=oldy
        Xold=FINDGEN(ROUND(100.*(x2-x1))+1)/100.+x1
        Wold=FLTARR(N_ELEMENTS(Yold),N_ELEMENTS(Xold))
        Eold=Wold
        Xtmp=[oldx,newx]
        Xtmp=Xtmp(SORT(Xtmp))
        FOR j=0,N_ELEMENTS(Wnew(*,0))-1 DO Wold(j,*) = INTERPOL(Wnew(j,*), Xtmp, Xold)
        FOR j=0,N_ELEMENTS(Wnew(*,0))-1 DO Eold(j,*) = INTERPOL(Enew(j,*), Xtmp, Xold)
        plotd,Wold,Yold,pos(i),e,datp=datp,threshold,bg=bg,max=max,/nop,x=Xold
  mod_datp,datp,'y',Yold
  mod_datp,datp,'x',Xold
  Eold=ROTATE(Eold,180)
  Wold=ROTATE(Wold,180)
  mod_datp,datp,'e',Eold
  give_datp,datp
RETURN,Wold
END
