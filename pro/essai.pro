pro essai

a = findgen(100)
plot, a, title = 'titre'
b = 20
c = 15

;xyouts,'test 1'
;xyouts, 15, 70, 'test 2'
;xyouts, 10, 50, a
;xyouts, b, c, b

;xyouts, 'test1', alignment = 0.5
;xyouts, 'test1', alignment = 0.0
;xyouts, 'test1', alignment = 1.0
;xyouts, 'test1', alignment = 1.5
;xyouts, 'test1', alignment = -0.5

;xyouts, 30 , 50, 0.5, alignment = 0.5
;xyouts, 30 , 45, 0.0, alignment = 0.0
;xyouts, 30 , 55, 1.0, alignment = 1.0
;xyouts, 30 , 40, 0.25, alignment = 0.25
;xyouts, 30 , 35, 0.24, alignment = 0.24
;xyouts, 30 , 30, 0.75, alignment = 0.75
;xyouts, 30 , 25, 0.76, alignment = 0.76
;xyouts, 30 , 20, -0.1, alignment = -0.1
;xyouts, 30 , 15, 1.1, alignment = 1.1

xyouts, 70, 10, 'testage', charsize = -1
xyouts, 70, 30, 'testage', charsize = 2
xyouts, 70, 35, 'testage', charsize = 1.45
xyouts, 70, 40, 'testage', charsize = 3.5
xyouts, 70, 60, 'testage', charsize = 3
xyouts, 70, 70, 'testage', charsize = 0

end
