
pro testtvrd

x=findgen(100)
y=2*x+14

plot,y,y, thick=2, min_value=10, /ylog, /ynozero

T = TVRD()

TV,T

end
