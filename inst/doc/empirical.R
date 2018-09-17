### R code from vignette source 'empirical.Rnw'

###################################################
### code chunk number 1: empirical.Rnw:34-39
###################################################
options(continue=" ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 2.6, 1.6), cex=0.7, cex.main=1)))

set.seed (2)


###################################################
### code chunk number 2: empirical.Rnw:63-64
###################################################
library (empirical)


###################################################
### code chunk number 3: empirical.Rnw:76-77
###################################################
x = rnorm (2000, 4) ^ 2 


###################################################
### code chunk number 4: empirical.Rnw:81-85
###################################################
getOption("SweaveHooks")[["fig"]]()
ebx = ebind (x)
f = euvpdf (ebx)
f
plot (f)


###################################################
### code chunk number 5: empirical.Rnw:90-91
###################################################
f (16)


###################################################
### code chunk number 6: empirical.Rnw:97-99
###################################################
getOption("SweaveHooks")[["fig"]]()
f = euvpdf (ebx, 0.25)
plot (f)


###################################################
### code chunk number 7: empirical.Rnw:116-117
###################################################
x = rnorm (30, 4) ^ 2


###################################################
### code chunk number 8: empirical.Rnw:121-124
###################################################
getOption("SweaveHooks")[["fig"]]()
F = euvcdf (x)
F
plot (F)


###################################################
### code chunk number 9: empirical.Rnw:129-130
###################################################
F (16)


###################################################
### code chunk number 10: empirical.Rnw:144-147
###################################################
getOption("SweaveHooks")[["fig"]]()
ebx = ebind (x)
F = euvcdf (ebx, TRUE)
plot (F)


###################################################
### code chunk number 11: empirical.Rnw:157-159
###################################################
getOption("SweaveHooks")[["fig"]]()
F.inverse = euvcdf.inverse (x)
plot (F.inverse)


###################################################
### code chunk number 12: empirical.Rnw:165-167
###################################################
getOption("SweaveHooks")[["fig"]]()
F.inverse = euvcdf.inverse (ebx, TRUE)
plot (F.inverse)


