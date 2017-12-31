

################################## 
# SCENARIO 5-8: repeat 1 to 4 using 
# BIG_MATRIX A (ALL) and B (MOLLECS)
##################################

BIG.A <- read.csv ("processed-datasets/BIGMATRIXcaseA.csv")

BIG.A <-subset(BIG.A, select=-X)

modeling.vars <- 1:30
modeling.ratios <- 34:48

lda.HnoH.PS.ALLVARS <- lda (x=BIG.A[,modeling.vars], grouping=BIG.A$CLASS, CV=FALSE)
plot(lda.HnoH.PS.ALLVARS)


lda.HnoH.PS.ALLVARS <- lda (x=BIG.A[,modeling.vars], grouping=BIG.A$CLASS, CV=TRUE)

(tab <- table(Truth=BIG.A$CLASS, Prediction=lda.HnoH.PS.ALLVARS$class))

100*sum(diag(tab)) / sum(tab)

#### Ara caldria anar treient variables

attach (BIG.A)

source ("FSS.R")

FSS.data <- BIG.A[,c(modeling.vars)]

#FSS.data <- aqua17[,c(modeling.vars,modeling.ratios)]
FSS.target <- BIG.A$CLASS
FSS.target.name <- "CLASS"

subset <- forward.search (names(FSS.data), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)
subset <- exhaustive.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)

subset <- bounded.exh.search (names(FSS.data), evaluator.accuracy.LDA, 1)

f <- as.simple.formula(subset$vars, "CLASS")
print(f)
subset$acc

glm.HnoH.PS.ALLVARS <- glm (CLASS ~ ., data=BIG.A[,modeling.vars], family=binomial)
# da 93.81

glm.HnoH.PS.ALLVARS <- step(glm.HnoH.PS.ALLVARS)

preds <- predict(glm.HnoH.PS.ALLVARS, type="response")
preds[preds<0.5] <- 0
preds[preds>0.5] <- 1

table(Truth=BIG.A$CLASS, Prediction=preds)

100*sum(diag(tab)) / sum(tab)
# da 93.81

## amb totes (ie, tb els ratios)

lda.HnoH.PS.ALLVARS <- lda (x=BIG.A[,c(modeling.vars,modeling.ratios)], grouping=BIG.A$CLASS, CV=TRUE)

tab <- table(Truth=BIG.A$CLASS, Prediction=lda.HnoH.PS.ALLVARS$class)

100*sum(diag(tab)) / sum(tab)

FSS.data <- BIG.A[,c(modeling.ratios,modeling.vars)]

subset <- bounded.exh.search (names(FSS.data), evaluator.accuracy.LDA, 1)

#################################
### ara amb els 4 sources
#################################

lda.4S.DILAGED.ALLVARS <- lda (x=BIG.A[,modeling.vars], grouping=BIG.A$TARGETtype, CV=FALSE)
plot(lda.4S.DILAGED.ALLVARS)

idict <- as.numeric(BIG.A$TARGETtype)

library("rgl")

aqua.pred <- predict (lda.4S.DILAGED.ALLVARS, BIG.A[,modeling.vars])

# surt preciós
plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (diluted and aged samples)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.03)

#text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=BIG.A$Site, adj = c(0.85, 0.85), cex=1)

aqua.lda <- lda (BIG.A$TARGETtype ~ ., BIG.A[,modeling.vars], CV=TRUE)
aqua.lda$posterior
aqua.lda$class

#lda.HnoH.PS.ALLVARS <- lda (x=BIG.A[,modeling.vars], grouping=BIG.A$CLASS, CV=TRUE)

(tab <- table(Truth=BIG.A$TARGETtype, Prediction=aqua.lda$class))

100*sum(diag(tab)) / sum(tab)

#### Ara caldria anar treient variables

attach (BIG.A)

source ("FSS.R")

FSS.data <- BIG.A[,c(modeling.vars)]

#FSS.data <- aqua17[,c(modeling.vars,modeling.ratios)]
FSS.target <- BIG.A$TARGETtype
FSS.target.name <- "TARGETtype"

subset <- forward.search (names(FSS.data), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)
subset <- exhaustive.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)

subset <- bounded.exh.search (names(FSS.data), evaluator.accuracy.LDA, 1)

f <- as.simple.formula(subset$vars, "TARGETtype")
print(f)
subset$acc

aqua.multinom <- multinom (TARGETtype ~ ., data=BIG.A[,modeling.vars])

(tab <- table(Truth=BIG.A$TARGETtype, Prediction=predict(aqua.multinom)))
100*sum(diag(tab)) / sum(tab)
# da 89.82%

aqua.multinom2 <- step(aqua.multinom)

(tab <- table(Truth=BIG.A$TARGETtype, Prediction=predict(aqua.multinom2)))
100*sum(diag(tab)) / sum(tab)
# da 89.80%

## amb totes (ie, tb els ratios)

lda.4S.DILAGED.ALLVARS <- lda (x=BIG.A[,c(modeling.vars,modeling.ratios)], grouping=BIG.A$TARGETtype, CV=FALSE)
plot(lda.4S.DILAGED.ALLVARS)

idict <- as.numeric(BIG.A$TARGETtype)

library("rgl")

aqua.pred <- predict (lda.4S.DILAGED.ALLVARS, BIG.A[,c(modeling.vars,modeling.ratios)])

# surt preciós (diria que un pèl millor)
plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (diluted and aged samples)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.03)

#text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=BIG.A$Site, adj = c(0.85, 0.85), cex=1)

aqua.lda <- lda (BIG.A$TARGETtype ~ ., BIG.A[,c(modeling.vars,modeling.ratios)], CV=TRUE)
aqua.lda$posterior
aqua.lda$class

(tab <- table(Truth=BIG.A$TARGETtype, Prediction=aqua.lda$class))

100*sum(diag(tab)) / sum(tab)

#### Ara caldria anar treient variables

attach (BIG.A)

source ("FSS.R")

FSS.data <- BIG.A[,c(c(modeling.vars,modeling.ratios))]
FSS.target <- BIG.A$TARGETtype
FSS.target.name <- "TARGETtype"

subset <- forward.search (names(FSS.data), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)
subset <- exhaustive.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)

subset <- bounded.exh.search (names(FSS.data), evaluator.accuracy.LDA, 1)

f <- as.simple.formula(subset$vars, "TARGETtype")
print(f)
subset$acc

aqua.multinom <- multinom (TARGETtype ~ ., data=BIG.A[,modeling.vars])

(tab <- table(Truth=BIG.A$TARGETtype, Prediction=predict(aqua.multinom)))
100*sum(diag(tab)) / sum(tab)
# da 89.82%

aqua.multinom2 <- step(aqua.multinom)

(tab <- table(Truth=BIG.A$TARGETtype, Prediction=predict(aqua.multinom2)))
100*sum(diag(tab)) / sum(tab)
# da 89.80%

################# proves EXTRA

# provo {NoV, PGMit, CWBif, HMBif, TLBif/CWBif} (donava 79.1% en RF OLD)

sweety <- subset (BIG.A, select=c("NoV", "PGMit", "CWBif", "HMBif", "TLBif.CWBif"))
aqua.lda <- lda (BIG.A$TARGETtype ~ ., data=sweety, CV=TRUE)
aqua.lda$posterior
aqua.lda$class

(tab <- table(Truth=BIG.A$TARGETtype, Prediction=aqua.lda$class))

100*sum(diag(tab)) / sum(tab)
# da 64.1%
#################  