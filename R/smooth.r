.esmooth = function (preserve, nhood, x)
{	dx = dy = (diff (x) + 1) ^ 0.3333
	ints = 1:length (dx)
	z = cbind (1, ints)
	for (i in ints)
	{	intx = 2 * (ints - i) / nhood
		intw = sbcpdf (intx)
		intw = intw / sum (intw)
		params = lm.wfit (z, dx, intw)$coefficients
		dy [i] = sum (params * c (1, i) )
	}
	dy = dy ^ (3.0003) - 1
	y = cumsum (c (0, dy) )
	if (preserve == "mean")
		.preserve.mean (x, y)
	else if (preserve == "range")
		.preserve.range (x, y)
	else
		stop ("invalid preserve value")
}

#and preserve variance
.preserve.mean = function (x, y)
{	y = sqrt (var (x) / var (y) ) * y
	mean (x) - mean (y) + y
}

.preserve.range = function (x, y)
	x [1] + .size (x) / .size (y) * y

.size = function (x)
	diff (range (x) )
