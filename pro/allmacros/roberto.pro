PRO roberto 



f10scan,.17,10,0.01,200


d17mad,'temp'
d17mad,'pte 35'
wait,180
d17mad,'pte 37'
wait,30
d17mad,'pte 38'
wait,30
d17mad,'pte 39'
wait,30
d17mad,'pte 40'
wait,60
; total time to change the temperature 10 min.s
f10scan,.17,10,0.01,200

d17mad,'temp'
d17mad,'pte 55'
wait,180
d17mad,'pte 57'
wait,30
d17mad,'pte 58'
wait,30
d17mad,'pte 59'
wait,30
d17mad,'pte 60'
wait,60
; total time to change the temperature 10 min.s
f10scan,.17,10,0.01,200

d17mad,'temp'
d17mad,'pte 65'
wait,30
d17mad,'pte 67'
wait,30
d17mad,'pte 68'
wait,30
d17mad,'pte 69'
wait,30
d17mad,'pte 70'
wait,60
; total time to change the temperature 6 min.s
f10scan,.17,10,0.01,200

; putting magnetic field to 0
d17mad,'rmg'
wait,2
d17mad,'pmg 0.0 1 1000'


;heating up the sample at 120K
d17mad,'temp'
d17mad,'pte 90'
wait,600


end

