.emv = function (x, rsp, bw, bind, w)
{   if (!is.matrix (x) )
		stop ("multivariate models need matrix")
	names = colnames (x)
	dims = dim (x)
	x = as.numeric (x)
	dim (x) = dims
	colnames (x) = names
	n = nrow (x)
    m = ncol (x)
	if (any (is.na (x) ) )
        stop ("no missing values allowed")
	for (j in 1:m)
		if (length (unique (x [,j]) ) < 10)
			stop ("each column of x needs\nto have 10 or more unique original values")

	weighted = (!is.na (w [1]) )
	if (weighted)
	{	w = as.numeric (w)
		if (n != length (w) )
			stop ("length(w) not equal nrow(x)")
		if (any (is.na (w) ) )
			stop ("no missing values allowed")
		if (round (sum (w), 1) != 1)
			stop ("sum(w) must be approx 1")
		if (any (w <= 0) )
			stop ("all w need to be > 0")
	}

	if (missing (bw) )
	{	bw = numeric (m)
		for (j in 1:m)
			bw [j] = rsp * diff (range (x [,j]) )
	}
	else
		rsp = NA

	if (bind)
	{	n = n + 2
		x = .emvbind (x, bw)
		if (weighted)
			w = c (0, w, 0)
	}
	else
		stop ("currently, multivariate models need bind=TRUE")

	list (rsp=rsp, bw=bw, weighted=weighted, n=n, m=m, x=x, w=w)
}

epdfmv = function (x, restack.pdf=sbcpdf, restack.cdf=sbccdf, rsp=0.5, bw, bind=TRUE, w=NA)
{	e = .emv (x, rsp, bw, bind, w)
	epdfmv.f = function (x) {.epdfmv.eval (x)}
	attributes (epdfmv.f) = list (
		class="epdfmv",
		restack.pdf=restack.pdf, restack.cdf=restack.cdf, bind=bind, weighted=e$weighted,
		rsp=e$rsp, bw=e$bw,
		n=e$n, m=e$m, x=e$x, w=e$w)
	if (e$m == 1)
		stop ("multivariate models need 2 or more variables")
    epdfmv.f
}

.epdfmv.eval = function (x)
{   . = attributes (sys.function (-1) )
    if (!is.matrix (x) )
		x = rbind (x)
    n = nrow (x)
    m = ncol (x)
    if (.$m != m)
        stop ("incorrect number of columns")
	y = numeric (n)
	for (i in 1:n)
		y [i] = .epdfmv.eval.ext (., .$restack.pdf, x [i,])
	y
}

ecdfmv = function (x, restack.pdf=sbcpdf, restack.cdf=sbccdf, rsp=0.5, bw, bind=TRUE, w=NA)
{   e = .emv (x, rsp, bw, bind, w)
	ecdfmv.f = function (x) {.ecdfmv.eval (x)}
    attributes (ecdfmv.f) = list (
		class="ecdfmv",
		restack.pdf=restack.pdf, restack.cdf=restack.cdf, bind=bind, weighted=e$weighted,
		rsp=e$rsp, bw=e$bw,
		n=e$n, m=e$m, x=e$x, w=e$w)
	if (e$m == 1)
		stop ("multivariate models need 2 or more variables")
    ecdfmv.f
}

.ecdfmv.eval = function (x)
{	 . = attributes (sys.function (-1) )
	if (!is.matrix (x) )
		x = rbind (x)
    n = nrow (x)
	m = ncol (x)
    if (.$m != m)
        stop ("incorrect number of columns")
    y = numeric (n)
	for (i in 1:n)
		y [i] = .ecdfmv.eval.ext (., .$restack.cdf, x [i,])
	y
}

.epdfmv.eval.ext = function (., l, data.point)
{	y = 0
	for (i in 2:(.$n - 1) )
	{	y.local = 1
		for (j in 1:.$m)
		{	dist = data.point [j] - .$x [i, j]
			lx = 2 / .$bw [j] * l (2 / .$bw [j] * dist)
			y.local = y.local * lx
		}
		if (.$weighted)
			y = y + .$w [i] * y.local
		else
			y = y + y.local
	}
	if (.$weighted)
		y
	else
		y / (.$n - 2)
}

.ecdfmv.eval.ext = function (., L, data.point)
{	y = 0
	for (i in 2:(.$n - 1) )
	{	y.local = 1
		for (j in 1:.$m)
		{	dist = data.point [j] - .$x [i, j]
			Lx = L (2 / .$bw [j] * dist)
			y.local = y.local * Lx
		}
		if (.$weighted)
			y = y + .$w [i] * y.local
		else
			y = y + y.local
	}
	if (.$weighted)
		y
	else
		y / (.$n - 2)
}
