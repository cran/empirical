plot.epdfmv = function (x, use.plot3d=FALSE, xlab="x1", ylab="x2", npoints=30, ..., all=FALSE)
{	epdfmv.f = x

	. = attributes (epdfmv.f)
	n = .$n
	if (.$m != 2)
		stop ("can only plot epdfmv if bivariate")
	if (all)
	{	par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        ecdfmv.f = function (x) {.ecdfmv.eval (x)}
		attributes (ecdfmv.f) = attributes (epdfmv.f)
        plot (epdfmv.f, FALSE, xlab, ylab, npoints, drawlabels=FALSE, ...)
        plot (epdfmv.f, TRUE, xlab, ylab, npoints, ...)
        plot (ecdfmv.f, FALSE, xlab, ylab, npoints, drawlabels=FALSE, ...)
        plot (ecdfmv.f, TRUE, xlab, ylab, npoints, ...)
        par (p0)
	}
	else
	{	x = seq (min (.$x [,1]), max (.$x [,1]), length.out=npoints)
		y = seq (min (.$x [,2]), max (.$x [,2]), length.out=npoints)
		z = outer (x, y, .mix, epdfmv.f)
		if (use.plot3d)
			plot3d.surf (x, y, z, xlab=xlab, ylab=ylab, ...)
		else
			contour (z=z, xlab=xlab, ylab=ylab, ...)
	}
	
}

plot.ecdfmv = function (x, use.plot3d=FALSE, xlab="x1", ylab="x2", npoints=30, ...)
{	ecdfmv.f = x

	. = attributes (ecdfmv.f)
	n = .$n
	if (.$m != 2)
		stop ("can only plot ecdfmv if bivariate")
	x = seq (min (.$x [,1]), max (.$x [,1]), length.out=npoints)
	y = seq (min (.$x [,2]), max (.$x [,2]), length.out=npoints)
	z = outer (x, y, .mix, ecdfmv.f)
	if (use.plot3d)
		plot3d.surf (x, y, z, xlab=xlab, ylab=ylab, ...)
	else
		contour (z=z, xlab=xlab, ylab=ylab, ...)
}

.mix = function (x, y, f)
	f (cbind (x, y) )
