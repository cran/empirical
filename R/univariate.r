ebind = function (x)
{   os = sd (x)
    c (min (x) - os, x, max (x) + os)
}

euvpdf = function (x, smoothness, randomize=TRUE)
{   euvpdf.f = function (x) {.euvpdf.eval (x)}
    euvcdf.f = euvcdf (x, TRUE, randomize=randomize)
    n = attributes (euvcdf.f)$n
    x = attributes (euvcdf.f)$x
    if (missing (smoothness) )
        smoothness = 2 / sqrt (n)
    interval.size = smoothness * (x [n] - x [1])

    x1 = x - interval.size / 2
    x2 = x + interval.size / 2
    y1 = euvcdf.f (x1)
    y2 = euvcdf.f (x2)
    y = (y2 - y1) / (x2 - x1)

    x1 = x [1:(n - 2)]
    x2 = x [3:n]
    y1 = y [1:(n - 2)]
    y2 = y [3:n]
    t = (y2 - y1) / (x2 - x1)
    t = c ( (y [2] - y [1]) / (x [2] - x [1]), t, (y [n] - y [n - 1]) / (x [n] - x [n - 1]) )

    attributes (euvpdf.f) = list (class="euvpdf", smoothness=smoothness, n=n, x=x, y=y, t=t)
    euvpdf.f
}

.euvpdf.eval = function (x)
{   this = attributes (sys.function (-1) )
    n = length (x)
    y = numeric (n)
    minx = this$x [1]
    maxx = this$x [this$n]
    for (i in 1:n)
    {   if (x [i] < minx || x [i] > maxx)
            y [i] = 0
        else
        {   I = (this$x <= x [i])
            nI = sum (I)
            if (x [i] == this$x [nI])
                y [i] = this$y [nI]
	    else
            {   fa = this$y [nI]
                fb = this$y [nI + 1]
                dx = this$x [nI + 1] - this$x [nI]
                u = (x [i] - this$x [nI]) / dx
                y [i] = (2 * u ^ 3 - 3 * u ^ 2 + 1) * fa + (u ^ 3 - 2 * u ^ 2 + u) * dx * this$t [nI] + (-2 * u ^ 3 + 3 * u ^ 2) * fb + (u ^ 3 - u ^ 2) * dx * this$t [nI + 1]
            }
        }
    }
    y
}

euvcdf = function (x, continuous=FALSE, randomize=TRUE)
{   euvcdf.f = if (continuous) function (x) {.euvcdf.continuous.eval (x)}
        else function (x) {.euvcdf.step.eval (x)}
    x = as.numeric (x)
    x = sort (x)
    n = length (x)
    if (n < 2)
        stop ("euvcdf requires n >= 3")
    if (any (is.na (x) ) )
        stop ("no missing values allowed")
    all.unique = (n == length (unique (x) ) )
    if (continuous && !all.unique)
    {   if (!randomize)
            stop ("continuous euvcdf requires unique x values")
        else
        {   sd = sd (x) / 1000
            while (!all.unique)
            {   x = x + rnorm (n, sd=sd)
                all.unique = (n == length (unique (x) ) )
            }
        }
    }
    attributes (euvcdf.f) = list (class="euvcdf", continuous=continuous, n=n, x=x)
    if (continuous)
    {   x1 = x [1:(n - 2)]
        x2 = x [3:n]
        y = ( (1:n) - 1) / (n - 1)
        y1 = y [1:(n - 2)]
        y2 = y [3:n]
        t = (y2 - y1) / (x2 - x1)
        t = c (0, t, 0)

        attributes (euvcdf.f)$t = t
    }
    euvcdf.f
}

.euvcdf.step.eval = function (x)
{   this = attributes (sys.function (-1) )
    n = length (x)
    y = numeric (n)
    for (i in 1:n)
        y [i] = sum (this$x <= x [i]) / this$n
    y
}

.euvcdf.continuous.eval = function (x)
{   m = c (2, -3, 0, 1, -2, 3, 0, 0, 1, -2, 1, 0, 1, -1, 0, 0)
    m = matrix (m, nrow=4)
    this = attributes (sys.function (-1) )
    n = length (x)
    y = numeric (n)
    for (i in 1:n)
    {   I = (this$x <= x [i])
        nI = sum (I)
        if (nI == 0)
            y [i] = 0
        else if (nI == this$n)
            y [i] = 1
        else
        {   Fa = (nI - 1) / (this$n - 1)
            Fb = (nI) / (this$n - 1)
            if (x [i] == this$x [nI])
                y [i] = Fa
            else
            {   dx = this$x [nI + 1] - this$x [nI]
                u = (x [i] - this$x [nI]) / dx
                y [i] = (2 * u ^ 3 - 3 * u ^ 2 + 1) * Fa + (u ^ 3 - 2 * u ^ 2 + u) * dx * this$t [nI] + (-2 * u ^ 3 + 3 * u ^ 2) * Fb + (u ^ 3 - u ^ 2) * dx * this$t [nI + 1]
            }
        }
    }
    y
}

euvcdf.inverse = function (x, continuous=FALSE)
{   euvcdf.f.inverse = if (continuous) function (y) {.euvcdf.inverse.continuous.eval (y)}
        else function (y) {.euvcdf.inverse.step.eval (y)}
    x = as.numeric (x)
    x = sort (x)
    if (any (is.na (x) ) )
        stop ("no missing values allowed")
    attributes (euvcdf.f.inverse) = list (class="euvcdf.inverse", continuous=continuous, n=length (x), x=x)
    euvcdf.f.inverse
}

.euvcdf.inverse.step.eval = function (y)
{   this = attributes (sys.function (-1) )
    n = length (y)
    x = numeric (n)
    for (i in 1:n)
    {   miny = 1 / this$n
        if (y [i] < miny || y [1] > 1)
            stop ("y values must be between 1/n and 1")
        else if (y [i] == miny)
            x [i] = this$x [1]
        else if (y [i] == 1)
            x [i] = this$x [this$n]
        else
            x [i] = this$x [floor (this$n * y [i])]
    }
    x
}

.euvcdf.inverse.continuous.eval = function (y)
{   this = attributes (sys.function (-1) )
    n = length (y)
    x = numeric (n)
    for (i in 1:n)
    {   if (y [i] < 0 || y [i] > 1)
            stop ("y values must be between 0 and 1")
        else if (y [i] == 0)
            x [i] = this$x [1]
        else if (y [i] == 1)
            x [i] = this$x [this$n]
        else
        {   k = 1 + (this$n - 1) * y [i]
            nI = floor (k)
            a = this$x [nI]
            b = this$x [nI + 1]
            Fa = (a - 1) / (this$n - 1)
            w = (y [i] - Fa) / (this$n - 1)
            x [i] = (1 - w) * a + w * b
        }
    }
    x
}

print.euvpdf = function (x, ...)
{   f = x

    n = length (attributes (f)$x)
    if (n > 30)
    {   attributes (f)$x = NULL
        attributes (f)$y = NULL
        attributes (f)$t = NULL
    }
    environment (f) = .GlobalEnv
    print.function (f, ...)
    if (n > 30)
        cat ("note that some attributes not printed")
}

print.euvcdf = function (x, ...) print.euvpdf (x, ...)

print.euvcdf.inverse = function (x, ...) print.euvpdf (x, ...)

plot.euvpdf = function (x, points=FALSE, ...)
{   euvpdf.f = x

    this = attributes (euvpdf.f)
    cx = this$x
    cy = this$y
    x = seq (cx [1], cx [this$n], length.out=200)
    y = euvpdf.f (x)
    plot (x, y, type="l", ...)
    if (points)
        points (cx, cy, pch=16)
}

plot.euvcdf = function (x, points=FALSE, ...)
{   euvcdf.f = x

    this = attributes (euvcdf.f)
    n = this$n
    if (this$continuous)
    {   cx = this$x
        cy = ( (1:n) - 1) / (n - 1)
        x = seq (cx [1], cx [n], length.out=200)
        y = euvcdf.f (x)
        plot (x, y, type="l", ...)
        if (points)
            points (cx, cy, pch=16)
    }
    else
    {   x = this$x
        y = (1:n) / n
        plot (x, y, pch=16, ...)
        for (i in 1:(n - 1) )
        {   lines (x [i:(i + 1)], rep (y [i], 2) )
            lines (rep (x [i + 1], 2), y [i:(i + 1)])
        }
    }
}

plot.euvcdf.inverse = function (x, ...)
{   this = attributes (x)

    n = this$n
    x = this$x
    if (this$continuous)
    {   y = ( (1:n) - 1 ) / (n - 1)
        plot (y, x, type="l", ...)
        points (y, x, pch=16)
    }
    else
    {   y = (1:n) / n
        plot (y, x, pch=16, ...)
        for (i in 1:(n - 1) )
        {   lines (y [i:(i + 1)], rep (x [i], 2) )
            lines (rep (y [i + 1], 2), x [i:(i + 1)])
        }
    }
}
