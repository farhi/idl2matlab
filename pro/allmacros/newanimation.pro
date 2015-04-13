PRO NewAnimation,win,nframes,xsize,ysize
WINDOW, /FREE, COLORS=-16,xsize=xsize,ysize=ysize
win=!D.WINDOW
IF NOT KEYWORD_SET ( nframes ) THEN nframes = 160
XINTERANIMATE, SET=[xsize, ysize, nframes] 
END

PRO AddNewFrame,win,frame
XINTERANIMATE, FRAME=frame, WINDOW=win
END


PRO CloseAnimation,win
WDELETE,win
XINTERANIMATE
END
