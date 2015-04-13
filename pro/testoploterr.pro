pro testoploterr

a=findgen(100)
y=3*a+11
z=2*a+14

window,1
plot,y
oploterr,a,z,y

window,2
plot,y
oploterr,a,sin(y),z

window,3
plot,y
oploterr,a,z

window,4
plot,y
oploterr,a,z,5

window,5
plot,y
oploterr,a,a*sin(z),y,5

window,6
plot,y
oploterr,a,z,y,2

end
