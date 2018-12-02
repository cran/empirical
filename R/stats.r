emode = function (f, include.boundaries=TRUE, all=FALSE, warning=FALSE)
{	if (!inherits (f, "epdfuv") )
		stop ("need epdfuv object")
	. = attributes (f)
	n = .$n

	#pdf (not cdf) y and t
	y = .$t
	t.left = t.right = numeric (n)
	for (i in 1:(n - 1) )
	{	t.left [i] = .intt.2nd.derivative (.$x [i], .$x [i + 1], .$y [i], .$y [i + 1], .$t [i], .$t [i + 1], .$x [i])
		t.right [i] = .intt.2nd.derivative (.$x [i], .$x [i + 1], .$y [i], .$y [i + 1], .$t [i], .$t [i + 1], .$x [i + 1])
	}

	modes = values = numeric (0)
	if (include.boundaries)
	{	if (y [1] > y [2] && t.left [1] <=0)
		{	modes = .$x [1]
			values = .$y [1]
		}
		if (y [n - 1] < y [n] && t.right [n - 1] >=0)
		{	modes = c (modes, .$x [n])
			values = c (values, .$y [n])
		}
	}
	for (i in 1:(n - 1) )
	{	if ( (t.right [i] > 0 && t.left [i + 1] < 0) ||
			(t.right [i] >= 0 && t.left [i + 1] <= 0 && y [i] < y [i + 1] && y [i + 1] > y [i + 2]) ||
			(t.right [i] > 0 && t.left [i + 1] == 0 && y [i] < y [i + 1]) ||
			(t.right [i] == 0 && t.left [i + 1] < 0 && y [i + 1] > y [i + 2]) )
		{	modes = c (modes, .$x [i + 1])
			values = c (values, .$t [i + 1])
		}
	}
	for (i in 1:(n - 1) )
	{	if ( (t.left [i] > 0 && t.right [i] < 0) ||
			(t.left [i] == 0 && t.right [i] < 0 && y [i] < y [i + 1]) ||
			(t.left [i] > 0 && t.right [i] == 0 && y [i] > y [i + 1]) )
		{	mode = .intt.argmax (.$x [i], .$x [i + 1], .$y [i], .$y [i + 1], .$t [i], .$t [i + 1])
			value = .intt.derivative (.$x [i], .$x [i + 1], .$y [i], .$y [i + 1], .$t [i], .$t [i + 1], mode)
			modes = c (modes, mode)
			values = c (values, value)
		}
	}
	mode.order = order (modes)
	modes = modes [mode.order]
	values = values [mode.order]

	if (warning)
	{	if (length (modes) == 0)
			warning ("no modal points")
		if (length (modes) > 1)
			warning ("multiple modal points")
	}
	if (all)
		modes
	else
		modes [which.max (values)]
}
