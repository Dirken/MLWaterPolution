# -----------------------------------------
# STEP 4: DA-HH-SS i SR
# -----------------------------------------

library(MASS)
source ("FSS.R")
options(digits = 4)

BIGM <-subset(read.csv ("BIGMATRIX-<x=x10.csv"), select=-X)
rownames(BIGM) <- 1:nrow(BIGM)

modeling.vars <- 1:21
modeling.ratios <- 28:40

# The molecular variables are:
#   HMBif, CWBif, PGNeo, PLBif, TLBif, BacR, Pig2Bac, AllBac, HF183TaqMan, FEqPCR
#   HMMit, CWMit, PGMit, PLMit, Adeno, NoV

mollecs <- 8:21
mollecs.ratios <- 31:40

### defineixo una fn. de wrap
lin.wrapper <- function (vars, target, target.name, sizes=c(), sinky)
{
  if (!missing(sinky))
    sink (file = paste(sinky,".txt",sep = ""), append = FALSE, type = "output")

  #### per FSS
  FSS.data <<- cbind( BIGM[,vars], target )
  colnames(FSS.data)[ncol(FSS.data)] <<- target.name
  
  FSS.target <<- target
  FSS.target.name <<- target.name
  
  try ({lda.tmp <- lda (x=BIGM[,vars], grouping=target, CV=TRUE);
        tab <- table(Truth=target, Prediction=lda.tmp$class);
        print(paste("LDA full (LOOCV): ", round(100*sum(diag(tab)) / sum(tab), 3)))})

  # try ({qda.tmp <- qda (x=BIGM[,vars], grouping=target, CV=TRUE);
  #       tab <- table(Truth=target, Prediction=qda.tmp$class);
  #       print(paste("QDA full (LOOCV): ", round(100*sum(diag(tab)) / sum(tab), 3)))})
  
  cat("\n*************** LDA (FW) **************************************")
  try ({subset <- forward.search (names(BIGM[,vars]), evaluator.accuracy.LDA);
        (as.simple.formula(subset, target.name))})
  
  # cat("\n*************** LDA (BW) **************************************")
  # try ({subset <- backward.search (names(BIGM[,vars]), evaluator.accuracy.LDA);
  #       (as.simple.formula(subset, target.name))})

  # cat("\n*************** QDA (FW) **************************************")
  # try ({subset <- forward.search (names(BIGM[,vars]), evaluator.accuracy.QDA);
  #       (as.simple.formula(subset, target.name))})
  
  # cat("\n*************** QDA (BW) **************************************")
  # try ({subset <- backward.search (names(BIGM[,vars]), evaluator.accuracy.QDA);
  #       (as.simple.formula(subset, target.name))})
  
  for (s in sizes)
  {
    cat("\n*********************** SIZE ", s, " ******************************\n")
    try ({subset <- bounded.exh.search (names(BIGM)[vars], evaluator.accuracy.LDA, s);
          cat("\nLDA best {", subset$vars, "} ", subset$acc, "\n")})
    # try ({subset <- bounded.exh.search (names(BIGM)[vars], evaluator.accuracy.QDA, s);
    #       cat("\nQDA best {", subset$vars, "} ", subset$acc, "\n")})
  }
 
  # if (nlevels(target)>2)
  # {
  #   cat("\n*********************** Multinom ****************************")
  #   suppressWarnings( glm.tmp <- step (multinom (target ~ ., data=BIGM[,vars])) )
  #   tab <- table(Truth=target, Prediction=predict(glm.tmp))
  #   print(paste("Multinom/step (TR): ", round(100*sum(diag(tab)) / sum(tab), 3)))
  # } else
  # {
  #   cat("\n*********************** LogReg ****************************")
  #   suppressWarnings( glm.tmp <- step (glm (target ~ ., data=BIGM[,vars], family = binomial)) )
  #   
  #   preds <- predict(glm.tmp, type="response")
  #   preds[preds<0.5] <- 0; preds[preds>0.5] <- 1
  # 
  #   preds <- factor(preds); levels(preds) <- c("Non-human","Human")
  # 
  #   tab <- table(Truth=target, Prediction=preds)
  #   print(paste("LogReg/step (TR): ", round(100*sum(diag(tab)) / sum(tab), 3)))
  # }
  if (!missing(sinky)) sink ()
}


# ------------------------------------------------------
# e) singles i ratios amb linear methods, HH
# ------------------------------------------------------

# singles only
plot (lda (x=BIGM[,modeling.vars], grouping=BIGM$CLASS, CV=FALSE))
lin.wrapper (modeling.vars, BIGM$CLASS, "CLASS", 2:4, sinky = "Linear-HnotH-All-singles")

# singles + ratios
plot (lda (x=BIGM[,c(modeling.vars, modeling.ratios)], grouping=BIGM$CLASS, CV=FALSE))
lin.wrapper (c(modeling.vars, modeling.ratios), BIGM$CLASS, "CLASS", 2:4,  sinky = "Linear-HnotH-All-singles+ratios")

# ratios only
plot (lda (x=BIGM[,modeling.ratios], grouping=BIGM$CLASS, CV=FALSE))
lin.wrapper (modeling.ratios, BIGM$CLASS, "CLASS", 2:5, sinky = "Linear-HnotH-All-ratios-only")

# ------------------------------------------------------
# f) singles i ratios amb linear methods HH (mollecs only)
# ------------------------------------------------------

# mol·lecs only
plot (lda (x=BIGM[,mollecs], grouping=BIGM$CLASS, CV=FALSE))
lin.wrapper (modeling.vars, BIGM$CLASS, "CLASS", 2:4, sinky = "Linear-HnotH-mollecs-singles")

# mol·lecs i els seus ratios
plot (lda (x=BIGM[,c(mollecs, mollecs.ratios)], grouping=BIGM$CLASS, CV=FALSE))
lin.wrapper (c(mollecs, mollecs.ratios), BIGM$CLASS, "CLASS", 2:4, sinky = "Linear-HnotH-mollecs-singles+ratios")

# mol·lecs ratios only
plot (lda (x=BIGM[,c(mollecs.ratios)], grouping=BIGM$CLASS, CV=FALSE))
lin.wrapper (c(mollecs.ratios), BIGM$CLASS, "CLASS", 2:5,  sinky = "Linear-HnotH-mollecs-ratios-only")

# ------------------------------------------------------
# g) singles i ratios amb linear methods (4S)
# ------------------------------------------------------

library(nnet)

lda.g <- lda (x=BIGM[,modeling.vars], grouping=BIGM$TARGETtype, CV=FALSE)
plot(lda.g) # no es veu res

idict <- as.numeric(BIGM$TARGETtype)

library("rgl")

# singles only
aqua.pred <- predict (lda.g, BIGM[,modeling.vars])

# surt preciós
plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (diluted and aged samples)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.03)

#text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=BIG.A$Site, adj = c(0.85, 0.85), cex=1)
rgl.postscript("Linear-4S-All-singles.pdf","pdf")

lin.wrapper (modeling.vars, BIGM$TARGETtype, "TARGETtype", 2:4, sinky = "Linear-4S-All-singles")

# singles + ratios
# (tb podria fer el plot)
lin.wrapper (c(modeling.vars, modeling.ratios), BIGM$TARGETtype, "TARGETtype", 2:4,  sinky = "Linear-4S-All-singles+ratios")

# mol·lecs only
lda.g <- lda (x=BIGM[,mollecs], grouping=BIGM$TARGETtype, CV=FALSE)
aqua.pred <- predict (lda.g, BIGM[,mollecs])

# surt preciós
plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (diluted and aged samples)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.03)

#text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=BIG.A$Site, adj = c(0.85, 0.85), cex=1)
rgl.postscript("Linear-4S-molecs-singles.pdf","pdf")

lin.wrapper (mollecs, BIGM$TARGETtype, "TARGETtype", 2:4, sinky = "Linear-4S-molecs-singles")

