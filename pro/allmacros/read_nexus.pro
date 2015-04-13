function read_nexus, INST , PATH , FILENAME , STATUS , DATP
;******* **********
;**
;**	Standard call for a data-read function interfacing LAMP.

STATUS=24
DATA  =0
if !version.release lt '4.0' then begin print,"IDL version is too low !!!",string(7b) & return,DATA & endif
FileN=PATH+FILENAME(0)
if n_elements(FILENAME) gt 1 then img=FILENAME(1) else img=1

i=strpos(FileN,'{') & if i gt 1 then begin j=strpos(FileN,'}')
                         if j gt i+1  then img=strmid(FileN,i+1,j-i-1)
                         FileN=strmid(FileN,0,i) & endif

read_hdf, FileN    , Data, XC=x, YC=y , ZC=z , E=e  , N=n        $
                         , PR=p, PV=pv, PAR_TXT=p_txt            $
                         , W_tit=wt   , X_tit=xt , Y_TIT=yt      $
                         , Z_tit=zt   , OTHER_TIT=ot , SNAP=snap $
                         , SOURCE=src , Img=img

if n_elements(Data) gt 1 then STATUS=0

DATP={X:      x   , Y:y , Z:z , E:e , N:n   ,$
      W_TIT:  wt  , X_TIT:xt  , Y_TIT:yt    ,$
      Z_TIT:  zt  , OTHER_TIT:ot            ,$
      P:      p   ,    $
      PAR_TXT:p_txt,   $
      PV:     pv       }
;     **********************

 RETURN, DATA
;************
 END

