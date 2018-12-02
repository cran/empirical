.euvbind = function (x)
{   os = sd (x)
    c (min (x) - os, x, max (x) + os)
}

.emvbind = function (x, bw)
{   if (!is.matrix (x) )
        stop ("embind requires matrix")
    nc = ncol (x)
    a = b = numeric (nc)
    for (j in 1:nc)
    {   os = bw [j] / 2
        a [j] = min (x [,j]) - os
        b [j] = max (x [,j]) + os
    }
    x = rbind (a, x, b)
    rownames (x) = NULL
    x
}
