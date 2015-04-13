
pro testplotts

plots,2,2.2,0

t=findgen(3,3)
t(0,0)=1.1
t(1,0)=2
t(2,0)=1

t(0,1)=1
t(1,1)=3
t(2,1)=4

t(0,2)=2
t(1,2)=4
t(2,2)=3

plots,t,/continue

PLOTS, [2.4,3.1], [2.4,3.1], /CONTINUE

plots,1.4,1.9

plots,2.4,2.9,/continue


plots,1.2,1.5,0,/continue

end
