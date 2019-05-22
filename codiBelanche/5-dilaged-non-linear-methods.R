# -----------------------------------------
# STEP 5: DA with non-linear methods
# -----------------------------------------

options(digits = 4)
library(randomForest)
library(varSelRF)

set.seed(2019)

# help()

#varSelRF(xdata, Class, c.sd = 1, mtryFactor = 1, ntree = 5000,
#         ntreeIterat = 2000, vars.drop.num = NULL, vars.drop.frac = 0.2,
#         whole.range = TRUE, recompute.var.imp = FALSE, verbose = FALSE,
#         returnFirstForest = TRUE, fitted.rf = NULL, keep.forest = FALSE)

# my usage

# rf.h <- varSelRF (BIGM[,modeling.vars], BIGM$CLASS,
                  #ntree = 1000, ntreeIterat = 500,
                  #vars.drop.num = 1, vars.drop.frac = NULL,
                  #recompute.var.imp = TRUE, returnFirstForest = FALSE, verbose = TRUE)

BIGM <-subset(read.csv ("BIGMATRIX-<x=x10.csv"), select=-X)
#BIGM <-subset(read.csv ("BIGMATRIX-x10.csv"), select=-X)
rownames(BIGM) <- 1:nrow(BIGM)

modeling.vars <- 1:21
modeling.ratios <- 28:40

# The molecular variables are:
#   HMBif, CWBif, PGNeo, PLBif, TLBif, BacR, Pig2Bac, AllBac, HF183TaqMan, FEqPCR
#   HMMit, CWMit, PGMit, PLMit, Adeno, NoV

mollecs <- 8:21
mollecs.ratios <- 31:40

myplot.varSelRF <- function (x, ...) 
{
  op <- par(las = 1); on.exit(par(op))
  
  nvar <- length(x$initialOrderedImportances)
  #ylim <- c(0, max(0.1, 100*(1-x$selec.history$OOB)))
  accs <- 100*(1-x$selec.history$OOB)
  ylim <-  c( 100*min( (1-x$selec.history$OOB)) -5, 100)
  
  plot (x$selec.history$Number.Variables, accs,
        xaxt  = "n", yaxt="n", pch=20,
        type = "b", xlab = "Number of variables used", ylab = "Prediction accuracy (%)", 
        log = "x", ylim = ylim, ...)
  
  #Add horizontal grid  
  axis (2, at=seq(round(100*min( (1-x$selec.history$OOB))-5), 100, by=1), tck = 1, lty = 3, labels = TRUE)
  
  #Add vertical grid
  axis(1, at = 2:nvar, tck = 1, lty = 2, col = "grey", labels = TRUE)
  
  # plot the vars alongside the plot
  ini.vars <- as.character(x$selec.history$Vars.in.Forest[nvar-1])
  text (2, (1-x$selec.history$OOB[nvar-1])*100, ini.vars, pos=1, offset=1, cex = .7, col="darkgreen")
  
  gs <- gsub(" ", "", ini.vars, fixed = TRUE)
  u.old <- unlist(strsplit(as.character(gs), "\\+"))
  for (v in (nvar-2):1)
  {
    gs <- gsub(" ", "", x$selec.history$Vars.in.Forest[v], fixed = TRUE)
    u <- unlist(strsplit(as.character(gs), "\\+"))
    newvar <- u[!u %in% u.old]
    
    text (length(u), (1-x$selec.history$OOB[v])*100, newvar, pos=-2*(v%%2)+3, offset=1, cex = .7, col="darkgreen")
    u.old <- u
  }
  # pa mejorar el plot
  #  plot(1:length(rwt$err),rwt$err,type="b",xaxt="n",yaxt="n",xlab="",ylab="",pch=1,main=title,cex.main=3)
  #  mtext("error", side=2, line=2, cex=2)
  #  mtext("variable dropped", side=1, line=2, cex=2)
  #  axis(2,cex.axis=2.9)
  #  axis(1, at=seq(1,length(rwt$err) , by=1), labels = FALSE)  
  #  vdrops <- append(rep("",times = length(rwt$drop) - 5), rwt$drop[(length(rwt$drop) - 4):length(rwt$drop)])
  #  text(seq(1, length(rwt$err), by=1), 0.0005, labels = vdrops, srt = 90, pos = 1, xpd = TRUE, cex=cexval, font=2, col=30)  
}


# We should use this function twice: for HnH and for the 4S
# (to just create the plots from the experiments: use justplots=TRUE)
GO <- function (target, path="resultats/BIGMATRIX-x10/", suffix, justplots=FALSE) 
{
  # set path
  basepath <- "/home/belanche/N O W/AQV MAY 2019 - Eli/Code/"
  oldpath <- getwd()
    
  setwd ( paste(basepath,path,sep = "/") )
  
  if (!justplots) {
    # fixed parameters for varSelRF
    pars <- list( Class = BIGM[,target], ntree = 1000, ntreeIterat = 500,
                  vars.drop.num = 1, vars.drop.frac = NULL,
                  recompute.var.imp = TRUE, returnFirstForest = FALSE, verbose = TRUE )
  
    # add data and call for all configs
    pars$xdata <- BIGM[,modeling.vars]
    rf.h1 <- do.call (varSelRF, pars)
  } else { load (file = paste("RFs-",suffix,".RData", sep="")) }
  
  png (paste("RF-all-singles-",suffix,".png",sep=""), width = 14, height = 8, units = 'in', res = 1000)
  myplot.varSelRF (rf.h1, main="Diluted and aged samples using single variables: Human/not Human")
  
  if (!justplots) {
    pars$xdata <- BIGM[,c(modeling.vars,modeling.ratios)]
    rf.h2 <- do.call (varSelRF, pars)
  }
  png (paste("RF-all-singles+ratios-",suffix,".png",sep=""), width = 14, height = 8, units = 'in', res = 1000)
  myplot.varSelRF (rf.h2, main="Diluted and aged samples using single variables plus ratios: Human/not Human")

  if (!justplots) { 
    pars$xdata <- BIGM[,mollecs]
    rf.h3 <- do.call (varSelRF, pars)
  }
  png (paste("RF-all-mollecs-",suffix,".png",sep=""), width = 14, height = 8, units = 'in', res = 1000)
  myplot.varSelRF (rf.h3, main="Diluted and aged samples using mollecular variables: Human/not Human")
  
  if (!justplots) { 
    pars$xdata <- BIGM[,c(mollecs, mollecs.ratios)]
    rf.h4 <- do.call (varSelRF, pars)
  }
  png (paste("RF-all-mollecs+ratios-",suffix,".png",sep=""), width = 14, height = 8, units = 'in', res = 1000)
  myplot.varSelRF (rf.h4, main="Diluted and aged samples using mollecular variables plus ratios: Human/not Human")
  
  dev.off()
  if (!justplots) save (rf.h1,rf.h2,rf.h3,rf.h4, file = paste("RFs-",suffix,".RData",sep=""))
  setwd (oldpath)
}

# run the experiments
GO ("CLASS", suffix = "HnH")
GO ("TARGETtype", suffix = "4S")

# just the plots
GO ("CLASS", suffix = "HnH", justplots = TRUE)
GO ("CLASS", suffix = "4S", justplots = TRUE)


# zero checking

plot.res <- function (r)
{
  barplot(r, ylim = c(0, 100), las=2, border = "dark blue", axes=FALSE)
  axis (2, at=seq(0, 100, by=10), tck = 1, lty = 3, labels = TRUE)
  title(main = "Percentage of zeros", font.main = 4)
}

res <- colSums( subset(BIGM, select=-c(Site,SAMPLES,CLASS,TARGETtype,Date,SEASON)) ==0.0)/nrow(BIGM)*100
mean(res); plot.res (res)

# around 54% for BIGMATRIX-x=0.csv
# around 12% for BIGMATRIX-x10.csv

# si no usem ratios
res <- colSums( BIGM[,modeling.vars] ==0)/nrow(BIGM)*100
mean(res); plot.res (res)

# around 65% for BIGMATRIX-x=0.csv
# around 12% for BIGMATRIX-x10.csv
