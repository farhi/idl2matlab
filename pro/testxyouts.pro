pro testxyouts


;!P.psym = 5

a = findgen(100)
y = 2*a+5
;plot, a, y
b = 20
c = 15
t = 'test!Cvariable!!'

;!P.LINESTYLE = 2


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

;xyouts, 70, 10, 'testage', charsize = -1
;xyouts, 70, 20, 'testage!Cplusieurs lignes !!', charsize = 2
;xyouts, 70, 35, 'testage', charsize = 1.45
;xyouts, 70, 40, 'testage', charsize = 3.5
;xyouts, 70, 60, 'testage', charsize = 3
;xyouts, 70, 70, 'testage', charsize = 0
;xyouts,t
;xyouts, 30,70,t,charsize = 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;!P.font = 0

;xyouts, 70, 10, 'testage', charsize = -1
;xyouts, 70, 20, 'testage!Cplusieurs lignes !!', charsize = 2
;xyouts, 70, 35, 'testage', charsize = 1.45
;xyouts, 70, 40, 'testage', charsize = 3.5
;xyouts, 70, 60, 'testage', charsize = 3, text_axes = 3
;xyouts, 70, 70, 'testage', charsize = 0
;xyouts,t
;xyouts, 70,90,t,charsize = 2

;!P.FONT = -1

;xyouts, 20, 10, 'testage', charsize = -1
;xyouts, 20, 20, 'testage!Cplusieurs lignes !!', charsize = 2
;xyouts, 20, 35, 'testage', charsize = 1.45
;xyouts, 20, 40, 'testage', charsize = 3.5
;xyouts, 20, 60, 'testage', charsize = 3, text_axes = 3
;xyouts, 20, 70, 'testage', charsize = 0
;xyouts,t
;xyouts, 20,90,t,charsize = 2


;!P.PSYM = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;xyouts, 20, 70, 'testage', width = e

;xyouts, 20, 70, 'testage', charsize = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TABLEAUX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

red = [0,1,1,0,0,1]
green = [0,1,0,1,0,1]
blue=[0,1,0,0,1,0]
loadct,35
plot, a, y, color = 100
;tvlct,255*red,255*green,255*blue

;plot, a, y

;xyouts, [10,10,10,10,10],[20,30,40,50,60],['blanc','blanc','blanc','blanc','blanc']

xyouts, [10,10,10,10,10],[20,30,40,50,60],['rouge','vert','bleu','blanc','jaune'],color = [2,3,4,1,5]

;xyouts, [10,10,10,10,10],[20,30,40,50,60,45],['rouge','vert','bleu','blanc','jaune'],color = [2,3,4,1,5]

;xyouts, [10,10,10,10,10,10],[20,30,40,50,60,45],['rouge','vert','bleu','blanc','jaune'],color = [2,3,4,1,5]

;xyouts, [10,10,10,10,10,10],[20,30,40,50,60],['rouge','vert','bleu','blanc','jaune'],color = [2,3,4,1,5]

;xyouts, [10,10,10,10],[20,30,40,50,60],['rouge','vert','bleu','blanc','jaune'],color = [2,3,4,1,5]

;xyouts, [10,10,10,10,10],[20,30,40,50],['rouge','vert','bleu','blanc','jaune'],color = [2,3,4,1,5]

;xyouts, [10,10,10,10,10,10],[20,30,40,50,60],['rouge','vert','bleu'],color = [2,3,4,1,5]

;xyouts, [10,10,10,10,10],[20,30,40,50],['jaune1','jaune2','jaune3','jaune4'],color = 5

;xyouts, [10,10,10,10,10],[20,30,40,50],['jaune1','jaune2','jaune3','jaune4'],color = [1,2,3]

loadct,0

end
