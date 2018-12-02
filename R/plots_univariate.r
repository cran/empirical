plot.epdfuv = function (x, plot.points=FALSE, ...)
{   epdfuv.f = x

    . = attributes (epdfuv.f)
    cx = .$x
    cy = .$t
    x = seq (cx [1], cx [.$n], length.out=200)
    y = epdfuv.f (x)
    plot (x, y, type="l", ...)
    if (plot.points)
        points (cx, cy, pch=16)
}

lines.epdfuv = function (x, ...)
{   epdfuv.f = x

    . = attributes (epdfuv.f)
    x = seq (.$x [1], .$x [.$n], length.out=200)
    y = epdfuv.f (x)
    lines (x, y, type="l", ...)
}

plot.ecdfuv = function (x, plot.points=FALSE, ...)
{   ecdfuv.f = x

	. = attributes (ecdfuv.f)
    cx = .$x
    cy = .$y
    x = seq (cx [1], cx [.$n], length.out=200)
	y = ecdfuv.f (x)
    plot (x, y, type="l", ...)
    if (plot.points)
		points (cx, cy, pch=16)
}

lines.ecdfuv = function (x, ...)
{   ecdfuv.f = x

    . = attributes (ecdfuv.f)
    x = seq (.$x [1], .$x [.$n], length.out=200)
	y = ecdfuv.f (x)
	lines (x, y, type="l", ...)
}

plot.ecdfuv.inverse = function (x, plot.points=FALSE, ...)
{   ecdfuv.f.inverse = x

    . = attributes (ecdfuv.f.inverse)
    y = seq (0, 1, length.out=200)
    x = ecdfuv.f.inverse (y)
    plot (y, x, type="l", ...)
    if (plot.points)
        points (.$y, .$x, pch=16)
}

lines.ecdfuv.inverse = function (x, ...)
{   ecdfuv.f.inverse = x

    . = attributes (ecdfuv.f.inverse)
    y = seq (0, 1, length.out=200)
    x = ecdfuv.f.inverse (y)
    lines (y, x, type="l", ...)
}
