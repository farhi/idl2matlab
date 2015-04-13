
pro testtvscl


window,1
D=DIST(200)
TVSCL, D,20,20,2,top=105

window,2
d=bytscl(indgen(200,200)) & tvscl,d

window,3
d=bytscl(indgen(200,200)) & tvscl,d, /order

end
