
pro testtv

D = BYTSCL(DIST(256))

window,1
TV, D

window,2
tv, D,0.1,0.1, /centimeters, channel=2

window,3
TV, D, 1

window,4
d=bytscl(indgen(200,200)) & tv,d

window,5
d=bytscl(indgen(200,200)) & tv,d, /order

end
