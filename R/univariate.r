.euv = function (x, derandomize, preserve, drp, nhood, bind, randomize, w)
{   x = as.numeric (x)
	n = length (x)
	if (any (is.na (x) ) )
        stop ("no missing values allowed")
	if (length (unique (x) ) < 10)
        stop ("needs 10 or more unique original x values")

	weighted = (!is.na (w [1]) )
	if (weighted)
	{	w = as.numeric (w)
		if (n != length (w) )
			stop ("length(w) not equal length(x)")
		if (any (is.na (w) ) )
			stop ("no missing values allowed")
		if (round (sum (w), 1) != 1)
			stop ("sum(w) must be approx 1")
		if (any (w <= 0) )
			stop ("all w need to be > 0")
	}

	if (bind)
	{	n = n + 2
		x = .euvbind (x)
		if (weighted)
			w = c (0, w, 0)
	}

    all.unique = (n == length (unique (x) ) )
    if (!all.unique)
    {   if (randomize)
        {   sd = sd (x) / 1000
            while (!all.unique)
            {   x = x + rnorm (n, sd=sd)
                all.unique = (n == length (unique (x) ) )
            }
        }
		else
			stop ("\nmodels need unique x values\n\nset randomize to TRUE")
    }
	
	x.order = order (x)
	x = x [x.order]
	if (weighted)
		w = w [x.order]

	if (derandomize)
    {	if (missing (nhood) )
			nhood = drp * (n - 1)
		else
			drp = NA
		if (nhood > 1)
			x = .esmooth (preserve, nhood, x)
	}
	else
		drp = nhood = NA

	t = NA
	if (weighted)
	{	intw = (w [-n] + w [-1]) / 2
		if (!bind)
		{	outw = (w [1] + w [n]) / 2
			intw =  intw / (1 - outw)
		}
		y = cumsum (c (0, intw) )
	}
	else
		y = ( (1:n) - 1) / (n - 1)
	t = .tangents (n, x, y)

	list (drp=drp, nhood=nhood, weighted=weighted, n=n, x=x, y=y, t=t, w=w)
 }
 
 .tangents = function (n, x, y)
 {	x1 = x [1:(n - 2)]
    x2 = x [3:n]
    y1 = y [1:(n - 2)]
    y2 = y [3:n]
    t = (y2 - y1) / (x2 - x1)
    c (0, t, 0)
 }

epdfuv = function (x, derandomize=TRUE, preserve="mean", drp=0.5, nhood, bind=TRUE, randomize=TRUE, w=NA)
{   e = .euv (x, derandomize, preserve, drp, nhood, bind, randomize, w)
	epdfuv.f = function (x) {.epdfuv.eval (x)}
	attributes (epdfuv.f) = list (
		class="epdfuv",
		derandomize=derandomize, preserve=preserve, bind=bind, weighted=e$weighted,
		drp=e$drp, nhood=e$nhood,
		n=e$n, x=e$x, y=e$y, t=e$t, w=e$w)
    epdfuv.f
}

.epdfuv.eval = function (x)
{   . = attributes (sys.function (-1) )
    n = length (x)
    y = numeric (n)
    minx = .$x [1]
    maxx = .$x [.$n]
	for (i in 1:n)
    {   if (x [i] < minx || x [i] > maxx)
            y [i] = 0
        else
        {   nI = sum (.$x <= x [i])
			if (x [i] == .$x [nI])
				y [i] = .$t [nI]
			else
				y [i] = .intt.derivative (.$x [nI], .$x [nI + 1], .$y [nI], .$y [nI + 1], .$t [nI], .$t [nI + 1], x [i])
		}
    }
    y
}

ecdfuv = function (x, derandomize=TRUE, preserve="mean",
	drp=0.5, nhood, bind=TRUE, randomize=TRUE, w=NA)
{   e = .euv (x, derandomize, preserve, drp, nhood, bind, randomize, w)
	ecdfuv.f = function (x) {.ecdfuv.eval (x)}
	attributes (ecdfuv.f) = list (
		class="ecdfuv",
		derandomize=derandomize, preserve=preserve, bind=bind, weighted=e$weighted,
		drp=e$drp, nhood=e$nhood,
		n=e$n, x=e$x, y=e$y, t=e$t, w=e$w)
	ecdfuv.f
}

.ecdfuv.eval = function (x)
{   . = attributes (sys.function (-1) )
    n = length (x)
    y = numeric (n)
    for (i in 1:n)
    {   nI = sum (.$x <= x [i])
        if (nI == 0)
            y [i] = 0
        else if (nI == .$n)
            y [i] = 1
        else
        {   y1 = .$y [nI]
			y2 = .$y [nI + 1]
            if (x [i] == .$x [nI])
                y [i] = y1
            else
				y [i] = .intt (.$x [nI], .$x [nI + 1], y1, y2, .$t [nI], .$t [nI + 1], x [i])
        }
    }
    y
}

ecdfuv.inverse = function (x, derandomize=TRUE, preserve="mean", drp=0.5, nhood, bind=TRUE, randomize=TRUE, w=NA)
{  e = .euv (x, derandomize, preserve, drp, nhood, bind, randomize, w)
	ecdfuv.f.inverse = function (y) {.ecdfuv.inverse.eval (y)}
  	attributes (ecdfuv.f.inverse) = list (
		class="ecdfuv.inverse",
		derandomize=derandomize, preserve=preserve, bind=bind, weighted=e$weighted,
		drp=e$drp, nhood=e$nhood,
		n=e$n, x=e$x, y=e$y, t.inverse=1 / e$t, w=e$w)
    ecdfuv.f.inverse
}

.ecdfuv.inverse.eval = function (y)
{   . = attributes (sys.function (-1) )
    n = length (y)
    x = numeric (n)
    for (i in 1:n)
    {   if (y [i] < 0 || y [i] > 1)
            stop ("y values must be between 0 and 1")
        else if (y [i] == 0)
            x [i] = .$x [1]
        else if (y [i] == 1)
            x [i] = .$x [.$n]
		else
		{   nI = sum (.$y <= y [i])
			if (y [i] == .$y [nI])
				x [i] = .$x [nI]
			else
				x [i] = .intt (.$y [nI], .$y [nI + 1], .$x [nI], .$x [nI + 1], .$t.inverse [nI], .$t.inverse [nI + 1], y [i])
        }
    }
    x
}
