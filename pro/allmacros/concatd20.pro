function concatd20,w1,w2

COMMON C_LAMP_INFO

take_datp,d1,w=two
take_datp,d2,w=three
print,two,three
d=d1
w=[[w1],[w2]]
x=[[d1.x],[d2.x]]
pv=[[d1.pv],[d2.pv]]
n=[[[d1.n]],[[d2.n]]]
y=[d1.y,d2.y]
e=[[d1.e],[d2.e]]
mod_datp,d,'x',x
mod_datp,d,'e',e
mod_datp,d,'n',n
mod_datp,d,'y',y
mod_datp,d,'pv',pv
d.w_tit=d.w_tit+' concat.'
give_datp,d
return,w
end
