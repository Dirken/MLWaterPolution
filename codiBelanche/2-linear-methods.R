#---------------------------------------------------------------------------------
# STEP 2: LINEAR MODELS
#---------------------------------------------------------------------------------

# SCENARIO a) H/notH - POINT SOURCE - SINGLES and RATIOS

# LDA

library (MASS)

# pel plot
lda.a <- lda (x=aqua18[,modeling.vars], grouping=aqua18$CLASS, CV=FALSE)
plot(lda.a)

lda.a <- lda (x=aqua18[,modeling.vars], grouping=aqua18$CLASS, CV=TRUE)
lda.a$posterior
table(Truth=aqua18$CLASS, Prediction=lda.a$class)

#plot(aqua18$HMBif, aqua18$HF183TaqMan)
#text (aqua18$HMBif, aqua18$HF183TaqMan, col=as.numeric(aqua18$CLASS), cex=0.75, adj=c(0.85, 0.85))

#### FSS for a)

source ("FSS.R")

FSS.data <- cbind( aqua18[,modeling.vars], CLASS=aqua18$CLASS )

FSS.target <- aqua18$CLASS
FSS.target.name <- "CLASS"

subset <- forward.search (names(aqua18[,modeling.vars]), evaluator.accuracy.LDA)
subset <- backward.search (names(aqua18[,modeling.vars]), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua18)[modeling.vars], evaluator.accuracy.LDA)
#subset <- exhaustive.search (names(aqua18)[modeling.vars], evaluator.accuracy.LDA)

(as.simple.formula(subset, "CLASS"))

subset <- bounded.exh.search (names(aqua18)[modeling.vars], evaluator.accuracy.LDA, 2)

print(as.simple.formula(subset$vars, "CLASS"))
print(as.simple.formula(subset$acc, "CLASS"))

# LogReg
glm.a <- glm (CLASS ~ ., data=aqua18[,modeling.vars], family=binomial)
glm.a <- step(glm.a)

preds <- predict(glm.a, type="response")
preds[preds<0.5] <- 0
preds[preds>0.5] <- 1

preds <- factor(preds)     
levels(preds) <- c("Non-human","Human")

table(Truth=aqua18$CLASS, Prediction=preds) # da 100% en TR

# Ara GLM amb totes ... dóna de nou {HMBif,  HF183TaqMan} i 100% en TR

glm.a <- glm (CLASS ~ ., data=aqua18[,c(modeling.vars,modeling.ratios)], family=binomial)
glm.a <- step(glm.a)

preds <- predict(glm.a, type="response")
preds[preds<0.5] <- 0
preds[preds>0.5] <- 1

preds <- factor(preds)     
levels(preds) <- c("Non-human","Human")

table(Truth=aqua18$CLASS, Prediction=preds)

# SCENARIO b) H/notH - POINT SOURCE - SINGLES and RATIOS - MOLLECS ONLY

#the molecular variables are:
#   HMBif, CWBif, PGNeo, PLBif, TLBif, BacR, Pig2Bac, AllBac, HF183TaqMan, FEqPCR
#   HMMit, CWMit, PGMit, PLMit, Adeno, NoV

mollecs <- c(11:24)
mollecs.ratios <- c(31:40)

lda.b <- lda (x=aqua18[,mollecs], grouping=aqua18$CLASS, CV=FALSE)
plot(lda.b)

lda.b <- lda (x=aqua18[,mollecs], grouping=aqua18$CLASS, CV=TRUE)
lda.b$posterior

table(Truth=aqua18$CLASS, Prediction=lda.b$class)

#### FSS for b)

FSS.data <- cbind( aqua18[,mollecs], CLASS=aqua18$CLASS )

FSS.target <- aqua18$CLASS
FSS.target.name <- "CLASS"

subset <- forward.search (names(aqua18[,mollecs]), evaluator.accuracy.LDA)
subset <- backward.search (names(aqua18[,mollecs]), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua18)[mollecs], evaluator.accuracy.LDA)
#subset <- exhaustive.search (names(aqua18)[mollecs], evaluator.accuracy.LDA)

(as.simple.formula(subset, "CLASS"))

subset <- bounded.exh.search (names(aqua18)[mollecs], evaluator.accuracy.LDA, 2)

print(as.simple.formula(subset$vars, "CLASS"))
print(as.simple.formula(subset$acc, "CLASS"))

# LogReg
glm.a <- glm (CLASS ~ ., data=aqua18[,mollecs], family=binomial)
glm.a <- step(glm.a)

preds <- predict(glm.a, type="response")
preds[preds<0.5] <- 0
preds[preds>0.5] <- 1

preds <- factor(preds)     
levels(preds) <- c("Non-human","Human")

table(Truth=aqua18$CLASS, Prediction=preds) # da 100% en TR

# Ara GLM amb totes ... dóna de nou {HMBif,  HF183TaqMan} i 100% en TR

glm.a <- glm (CLASS ~ ., data=aqua18[,c(modeling.vars,modeling.ratios)], family=binomial)
glm.a <- step(glm.a)

preds <- predict(glm.a, type="response")
preds[preds<0.5] <- 0
preds[preds>0.5] <- 1

preds <- factor(preds)     
levels(preds) <- c("Non-human","Human")

table(Truth=aqua18$CLASS, Prediction=preds)

# SCENARIO c) 4 Sources - POINT SOURCE - SINGLES and RATIOS

lda.c <- lda (x=aqua18[,modeling.vars], grouping=aqua18$TARGETtype, CV=FALSE)
plot(lda.c)

idict <- as.numeric(aqua18$TARGETtype)

library("rgl")

aqua.pred <- predict (lda.c, aqua18[,modeling.vars])

# surt preciós
plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (original variables)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.03)

text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=aqua18$Site, adj = c(0.85, 0.85), cex=0.75)

########## plot de només HMBif + CWBif + BacR + Pig2Bac
lda.c3 <- lda (x=subset(aqua18, select=c("HMBif","CWBif","BacR","Pig2Bac")), grouping=aqua18$TARGETtype, CV=FALSE)
aqua.pred <- predict (lda.c3)

plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (HMBif + CWBif + BacR + Pig2Bac)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.1, size = 5)

text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=aqua18$Site, adj = c(0.85, 0.85), cex=0.75)
##########

lda.c <- lda (aqua18$TARGETtype ~ ., aqua18[,modeling.vars], CV=TRUE)
lda.c$posterior
lda.c$class

table(Truth=aqua18$TARGETtype, Prediction=lda.c$class)

#### FSS for c)

FSS.data <- cbind( aqua18[,modeling.vars], CLASS=aqua18$TARGETtype )

FSS.target <- aqua18$TARGETtype
FSS.target.name <- "TARGETtype"

subset <- forward.search (names(aqua18[,modeling.vars]), evaluator.accuracy.LDA)
subset <- backward.search (names(aqua18[,modeling.vars]), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua18)[modeling.vars], evaluator.accuracy.LDA)
#subset <- exhaustive.search (names(aqua18)[modeling.vars], evaluator.accuracy.LDA)

(as.simple.formula(subset, "TARGETtype"))

subset <- bounded.exh.search (names(aqua18)[modeling.vars], evaluator.accuracy.LDA, 2)

print(as.simple.formula(subset$vars, "TARGETtype"))
print(as.simple.formula(subset$acc, "TARGETtype"))

# MultinomReg

library(nnet)

mnl.c <- multinom (TARGETtype ~ ., data=aqua18[,modeling.vars])
mnl.c <- step(mnl.c)

preds <- predict(mnl.c, type="class")

table(Truth=aqua18$TARGETtype, Prediction=preds) # da 100% en TR

# Ara amb totes ... (ratios)

# (lo hago adaptando el código precedente)

# SCENARIO d) 4 Sources - POINT SOURCE - SINGLES and RATIOS - MOLLECS ONLY

lda.d <- lda (x=aqua18[,mollecs], grouping=aqua18$TARGETtype, CV=FALSE)
plot(lda.d)

idict <- as.numeric(aqua18$TARGETtype)

library("rgl")

aqua.pred <- predict (lda.d, aqua18[,mollecs])

# surt preciós
plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (mollecular variables only)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.1, size = 2)

text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=aqua18$Site, adj = c(0.85, 0.85), cex=1)

lda.d <- lda (aqua18$TARGETtype ~ ., aqua18[,mollecs], CV=TRUE)
lda.d$posterior
lda.d$class

table(Truth=aqua18$TARGETtype, Prediction=lda.d$class)

#### FSS for d)

FSS.data <- cbind( aqua18[,mollecs], CLASS=aqua18$TARGETtype )

FSS.target <- aqua18$TARGETtype
FSS.target.name <- "TARGETtype"

subset <- forward.search (names(aqua18[,mollecs]), evaluator.accuracy.LDA)
subset <- backward.search (names(aqua18[,mollecs]), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua18)[mollecs], evaluator.accuracy.LDA)
#subset <- exhaustive.search (names(aqua18)[modeling.vars], evaluator.accuracy.LDA)

(as.simple.formula(subset, "TARGETtype"))

subset <- bounded.exh.search (names(aqua18)[mollecs], evaluator.accuracy.LDA, 2)

print(as.simple.formula(subset$vars, "TARGETtype"))
print(as.simple.formula(subset$acc, "TARGETtype"))

# MultinomReg

library(nnet)

mnl.c <- multinom (TARGETtype ~ ., data=aqua18[,mollecs])
mnl.c <- step(mnl.c)

preds <- predict(mnl.c, type="class")

table(Truth=aqua18$TARGETtype, Prediction=preds) # da 100% en TR

# Ara amb totes ... (ratios)

# (lo hago adaptando el código precedente)
