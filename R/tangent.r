.intt = function (cx1, cx2, cy1, cy2, ct1, ct2, x)
{   if (!missing (cx1) && !missing (cx2) )
    {   dx = cx2 - cx1
        ct1 = dx * ct1
        ct2 = dx * ct2
        x = (x - cx1) / dx
    }
    cy1 * (2 * x ^ 3 - 3 * x ^ 2 + 1) +
		cy2 * (-2 * x ^ 3 + 3 * x ^ 2) +
		ct1 * (x ^ 3 - 2 * x ^ 2 + x) +
		ct2 * (x ^ 3 - x ^ 2)
}

.intt.derivative = function (cx1, cx2, cy1, cy2, ct1, ct2, x)
{   has.cx = (!missing (cx1) && !missing (cx2) )
	if (has.cx)
	{   dx = cx2 - cx1
        ct1 = dx * ct1
        ct2 = dx * ct2
        x = (x - cx1) / dx
    }
    y = cy1 * (6 * x ^ 2 - 6 * x) +
		cy2 * (-6 * x ^ 2 + 6 * x) +
		ct1 * (3 * x ^ 2 - 4 * x + 1) +
		ct2 * (3 * x ^ 2 - 2 * x)
	if (has.cx)
		y = y / dx
	y
}

.intt.2nd.derivative = function (cx1, cx2, cy1, cy2, ct1, ct2, x)
{   has.cx = (!missing (cx1) && !missing (cx2) )
	if (has.cx)
	{   dx = cx2 - cx1
        ct1 = dx * ct1
        ct2 = dx * ct2
        x = (x - cx1) / dx
    }
    y = cy1 * (12 * x - 6) +
		cy2 * (-12 * x + 6) +
		ct1 * (6 * x - 4) +
		ct2 * (6 * x - 2)
	if (has.cx)
		y = y / dx / dx
	y
}

.intt.argmax = function (cx1, cx2, cy1, cy2, ct1, ct2)
{   has.cx = (!missing (cx1) && !missing (cx2) )
	if (has.cx)
	{   dx = cx2 - cx1
        ct1 = dx * ct1
        ct2 = dx * ct2
    }
	x = (3 * cy1 - 3 * cy2 + 2 * ct1 + ct2) / (6 * cy1 - 6 * cy2 + 3 * ct1 + 3 * ct2)
	if (has.cx)
		x = cx1 + dx * x
	x
}
