FUNCTION tif2charge,tif,r=r,g=g,b=b

IF NOT KEYWORD_SET(r) THEN r=1./255.
IF NOT KEYWORD_SET(b) THEN b=-1./255.
IF NOT KEYWORD_SET(g) THEN g=-r-b
image=tif
xdim=N_ELEMENTS(image[0,*,0])
ydim=N_ELEMENTS(image[0,0,*])
dim=xdim*ydim

image=REFORM(image[0,*,*]*r,xdim,ydim)+REFORM(image[1,*,*]*g,xdim,ydim)+REFORM(image[2,*,*]*b,xdim,ydim)
charge=LONARR(4,dim)
charge[3,*]=ROUND(REFORM(image,dim))
charge[2,*]=0L
image=LINDGEN(dim)
charge[0,*]=image mod xdim
charge[1,*]=image  /  xdim
return,charge 

END
