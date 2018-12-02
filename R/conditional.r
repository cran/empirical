.ec.indices = function (rv, conditions, x)
{	if (length (rv) != 1)
		stop ("length (rv) must equal 1")
	if (length (conditions) < 1)
		stop ("length (conditions) must be >= 1")

	names.conditions = toupper (names (conditions) )
	names.x = toupper (colnames (x) )
	if (any (rv == names.conditions) )
		stop ("rv can not be the same as conditioning variables")
	if (any (names.conditions == "") )
		stop ("conditions need to be named")
	if (any (names.x == "") )
		stop ("x needs named columns")
	if (length (unique (names.conditions) ) != length (names.conditions) || length (unique (names.x) ) != length (names.x) )
		stop ("all names must be unique")

	I = match (toupper (rv), names.x)
	J = match (names.conditions, names.x)

	list (I, J, c (J, I) )
}

.ec.string = function (conditions)
	paste (names (conditions), "=", conditions, sep="", collapse=", ")

.ec = function (rv, conditions, x, restack.pdf, rsp, bw, bind, w, npoints)
{	I = .ec.indices (rv, conditions, x)
	. = .emv (x [,I [[3]] ], rsp, bw, bind, w)
	xrng = range (.$x [,.$m])
	x = seq (xrng [1], xrng [2], length.out=npoints)

	dy.top = numeric (npoints)
	for (i in 1:npoints)
		dy.top [i] = .epdfmv.eval.ext (., restack.pdf, c (conditions, x [i]) )

	.$m = length (conditions)
	dy.btm = .epdfmv.eval.ext (., restack.pdf, conditions)

	dy = dy.top / dy.btm

	dy = (dy [-npoints] + dy [-1]) / 2
	y = c (0, cumsum (dy) )

	.$n = npoints
	.$x = x
	.$y = y / y [npoints]
	.$t = .tangents (npoints, x, .$y)
	.$w = NA

	.
}

epdfc = function (rv, conditions, x, restack.pdf=sbcpdf, rsp=0.5, bw, bind=TRUE, w=NA, is.string=FALSE, npoints=30)
{   if (!is.string)
		rv = as.character (substitute (rv) )
	e = .ec (rv, conditions, x, restack.pdf, rsp, bw, bind, w, npoints)
	ecpdf.f = function (x) {.epdfuv.eval (x)}
	attributes (ecpdf.f) = list (
		class=c ("ecpdf", "epdfuv"),
		rv=rv, conditions=.ec.string (conditions),
		restack.pdf=restack.pdf, bind=bind, weighted=e$weighted,
		rsp=e$rsp, bw=e$bw,
		n=e$n, x=e$x, y=e$y, t=e$t, w=e$w)
    ecpdf.f
}

ecdfc = function (rv, conditions, x, restack.pdf=sbcpdf, rsp=0.5, bw, bind=TRUE, w=NA, is.string=FALSE, npoints=30)
{	if (!is.string)
		rv = as.character (substitute (rv) )
	e = .ec (rv, conditions, x, restack.pdf, rsp, bw, bind, w, npoints)
	eccdf.f = function (x) {.ecdfuv.eval (x)}
		attributes (eccdf.f) = list (
		class=c ("eccdf", "ecdfuv"),
		rv=rv, conditions=.ec.string (conditions),
		restack.pdf=restack.pdf, bind=bind, weighted=e$weighted,
		rsp=e$rsp, bw=e$bw,
		n=e$n, x=e$x, y=e$y, t=e$t, w=e$w)
    eccdf.f
}

ecdfc.inverse = function (rv, conditions, x, restack.pdf=sbcpdf, rsp=0.5, bw, bind=TRUE, w=NA, is.string=FALSE, npoints=30)
{   if (!is.string)
		rv = as.character (substitute (rv) )
	e = .ec (rv, conditions, x, restack.pdf, rsp, bw, bind, w, npoints)
	eccdf.f.inverse = function (y) {.ecdfuv.inverse.eval (y)}
	attributes (eccdf.f.inverse) = list (
		class=c ("eccdf.inverse", "ecdfuv.inverse"),
		rv=rv, conditions=.ec.string (conditions),
		restack.pdf=restack.pdf, bind=bind, weighted=e$weighted,
		rsp=e$rsp, bw=e$bw,
		n=e$n, x=e$x, y=e$y, t.inverse=1 / e$t, w=e$w)
    eccdf.f.inverse
}
