del y.tab.h
byacc -d -t -v idl2matlab.y
flex idl2matlab.l
rename idl2matlab.h y.tab.h
