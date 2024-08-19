### R code from vignette source 'Mfuzz.Rnw'

###################################################
### code chunk number 1: Mfuzz.Rnw:43-44
###################################################
  library(Mfuzz) 


###################################################
### code chunk number 2: Mfuzz.Rnw:55-56
###################################################
  data(yeast) 


###################################################
### code chunk number 3: Mfuzz.Rnw:64-65
###################################################
yeast.r <- filter.NA(yeast, thres=0.25)


###################################################
### code chunk number 4: Mfuzz.Rnw:74-75
###################################################
yeast.f <- fill.NA(yeast.r,mode="mean")


###################################################
### code chunk number 5: Mfuzz.Rnw:93-94 (eval = FALSE)
###################################################
##   tmp <- filter.std(yeast.f,min.std=0)  


###################################################
### code chunk number 6: Mfuzz.Rnw:117-118
###################################################
yeast.s <- standardise(yeast.f)


###################################################
### code chunk number 7: Mfuzz.Rnw:157-159 (eval = FALSE)
###################################################
##  cl <- mfuzz(yeast.s,c=16,m=1.25)
##  mfuzz.plot(yeast.s,cl=cl,mfrow=c(4,4),time.labels=seq(0,160,10))


###################################################
### code chunk number 8: Mfuzz.Rnw:189-191 (eval = FALSE)
###################################################
## m1 <- mestimate(yeast.s)
## m1 # 1.15


###################################################
### code chunk number 9: Mfuzz.Rnw:234-236 (eval = FALSE)
###################################################
##    cl2 <- mfuzz(yeast.s,c=16,m=1.35)
##    mfuzz.plot(yeast.s,cl=cl2,mfrow=c(4,4),time.labels=seq(0,160,10))


###################################################
### code chunk number 10: Mfuzz.Rnw:265-267 (eval = FALSE)
###################################################
## O <- overlap(cl)
## Ptmp <- overlap.plot(cl,over=O,thres=0.05)


###################################################
### code chunk number 11: Mfuzz.Rnw:273-277 (eval = FALSE)
###################################################
## cl3 <- mfuzz(yeast.s,c=10,m=1.25)
## mfuzz.plot(yeast.s,cl=cl3,mfrow=c(3,4))
## O3 <- overlap(cl3)
## overlap.plot(cl3,over=O3,P=Ptmp,thres=0.05)


###################################################
### code chunk number 12: Mfuzz.Rnw:290-294 (eval = FALSE)
###################################################
## cl4 <- mfuzz(yeast.s,c=25,m=1.25)
## mfuzz.plot(yeast.s,cl=cl4,mfrow=c(5,5))
## O4 <- overlap(cl4)
## overlap.plot(cl4,over=O4,P=Ptmp,thres=0.05)


