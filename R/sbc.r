sbcpdf = function (x)
{	y = rep (0, length (x) )
	#subinterval 1
	I = (x > -1 & x < -0.5)
	y [I] = 2 + 4 * x [I] + 2 * x [I] ^ 2
	#center and subintervals 2 and 3
	I = (x >= -0.5 & x <= 0.5)
	y [I] = 1 - 2 * x [I] ^ 2
	#subinterval 4
	I = (x > 0.5 & x < 1)
	y [I] = 2 - 4 * x [I] + 2 * x [I] ^ 2
	y
}

sbccdf = function (x)
{	y = rep (0, length (x) )
	#subinterval 1
	I = (x > -1 & x < -0.5)
	y [I] = 2 / 3 + 2 * x [I] + 2 * x [I] ^ 2 + 2 / 3 * x [I] ^ 3
	#center and subintervals 2 and 3
	I = (x >= -0.5 & x <= 0.5)
	y [I] = 0.5 + x [I] - 2 / 3 * x [I] ^ 3
	#subinterval 4
	I = (x > 0.5 & x < 1)
	y [I] = 1 / 3 + 2 * x [I] - 2 * x [I] ^ 2 + 2 / 3 * x [I] ^ 3
	#x >= 1
	I = (x >= 1)
	y [I] = 1
	y
}
