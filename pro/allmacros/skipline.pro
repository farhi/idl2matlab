pro skipline , in,line,cnt,nvers=nvers,bid               ; ###### modified 11/3
;** ********
;**
	ON_IOERROR,nocnt
	cnt=0L & bid=0L & READS,line+' 0 0',cnt,bid,nvers   ; ###### modified 11/3
	IF bid gt 0 THEN FOR i=1,bid DO READF,in,line
	nocnt:
RETURN
END


