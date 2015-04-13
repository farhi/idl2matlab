Pro SHOWVOL, v,p,vol
;Display the contour surface of a volume.
s = SIZE(vol)	;Get the dimensions of the volume.
SCALE3, XRANGE=[0, S(1)/5.], YRANGE=[0, S(2)], ZRANGE=[0, S(3)]
	;Use SCALE3 to establish the 3D transformation and coordinate ranges.
TV, POLYSHADE(v,p,/T3D)
END
