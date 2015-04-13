PRO plotnzoom,w,new=new

TVLCT,[0,255,0,0],[0,0,255,0],[0,0,0,255]
IF KEYWORD_SET(new) THEN plot,w,xstyle=1,ystyle=1
cursor,x1,y1,/down 
plots,[0,N_ELEMENTS(w)-1],[y1,y1]
plots,[x1,x1],[min(w),max(w)]
cursor,x2,y2,/up 
plot,w,xstyle=1,xr=[x1,x2],yr=[y1,y2]

END
