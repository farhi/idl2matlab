pro testvarsysP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;!P.CHARTHICK = 2

a = findgen(100)

;plot,a

;!P.CHARTHICK = 1
;xyouts,5,25,'!!P.CHARTHICK = 1'

;!P.CHARTHICK = 0
;xyouts,5,35,'!!P.CHARTHICK = 0'

;!P.CHARTHICK = 2
;xyouts,5,45,'!!P.CHARTHICK = 2'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;window,1

;!P.MULTI = [0,2,3,0,1]

;plot, a^2
;oplot,15*a

;plot,a^2
;oplot, 2*a^2

;plot,a
;xyouts,15,25,'testxyouts'
;oplot,2*a

;!P.MULTI = [1,2,3,0,1]
;plot, a, title = 'dernier plot'



;window,2

;!P.MULTI = [0,2,3,0,0]

;plot, a^2
;oplot,15*a

;plot,a^2
;oplot, 2*a^2

;plot,a
;xyouts,15,25,'testxyouts'
;oplot,2*a

;!P.MULTI = [1,2,3,0,0]
;plot, a, title = 'dernier plot'


;window,3

;!P.MULTI = 0
;plot,-a

;window,4
;!P.MULTI = [0,2,0,0,0]
;plot, 2*a
;oplot, a+15
;plot, -2*a
;oplot, -2*a -10

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;window,5
;!P.MULTI = [0,2,3,0,0]

;plot, a
;errplot, 3*a/2,a/2

;ploterr, 2*a, a

;window, 6

;!P.MULTI = [0,2,3,0,1]
;!P.MULTI[4] = 1

;b = replicate(1,100)

;plot,a
;oploterr, a,b

;plot_io, a^2

;ploterr, -a, 2*b

;plot_oo, a

;plot_io, a

;plot, 5*a
;oploterr,5*a, b

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TEST !P.POSITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!P.POSITION = 0
!P.REGION = 0

;window, 7
;plot,a, title = '!!P.POSITION = 0'

;window,8
;!P.POSITION = [0,0,1,1]
;plot,a + 1,title = '!!P.POSITION = [0,0,1,1]'
;oplot,a + 5

;window,9
;!P.POSITION = [0.2,0.5,0.8,0.9]
;plot,a^2, title = 'test',xtitle='axe des x'
;plot,a^2+1, title = 'numero2', xtitle = 'abscisses 2' , ytitle = 'axe des ordonnes'

;window, 10
;!P.POSITION = [0.2,0.8,0.2,0.4]
;plot,a,title = 'x0 = x1'

;window, 11
;!P.POSITION = [0.2,0.5,0.8,0.5]
;plot, a , title = 'y0 = y1'

;!P.POSITION = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERACTION !P.MULTI et !P.POSITION ;;;;;;;;;;;;;;

;window,12

;!P.MULTI = [0,2,3,2,1]

;plot,a
;!P.POSITION = [0.2,0.4,0.8,0.8]
;plot,2*a

!P.POSITION = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; INTERACTION !P.POSITION et !P.REGION ;;;;;;;;;;;

!P.POSITION = 0
!P.REGION = 0

;window, 13
;!P.POSITION = [0,0,1,1]
;plot,a + 1,title = '!!P.POSITION = [0,0,1,1] !!P.REGION = 0'
;oplot,a + 5

;window, 14
;!P.POSITION = [0,0,1,1]
;!P.REGION = [0,0,1,1]
;plot,a + 1,title = '!!P.POSITION = [0,0,1,1] !!P.REGION = [0,0,1,1]'
;oplot,a + 5

;window, 15
;!P.POSITION = 0
;!P.REGION = [0,0,1,1]
;plot,a + 1,title = '!!P.POSITION = 0 !!P.REGION = [0,0,1,1]'
;oplot,a + 5

;window, 16
;!P.POSITION = [-5,0.2,5,0.6]
;!P.REGION = [-5,0.2,5,0.6]
;plot,a + 1,title = '!!P.POSITION = [-5,0.2,5,0.6] !!P.REGION = [-5,0.2,5,0.6]'
;oplot,a + 5

window, 17
!P.POSITION = 0
!P.REGION = [-5,0.2,5,0.6]
plot,a + 1,title = '!!P.POSITION = 0 !!P.REGION = [-5,0.2,5,0.6]'
oplot,a + 5

window, 18
!P.REGION = 0
!P.POSITION = [0.2,-1.0,0.9,1.5]
plot,a + 1,title = '!!P.POSITION = 0 !!P.REGION = [-5,0.2,5,0.6]'
oplot,a + 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; !P.CHARSIZE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;!P.MULTI = [0,2,1,0,0]

;!P.CHARSIZE = 1

;plot,a, title = 'charsize = 1'

;!P.CHARSIZE = 2
;plot, -a , title = 'charsize = 2'


end
