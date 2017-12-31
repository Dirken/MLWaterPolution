################################## 
# SCENARIO 1
# H/noH - POINT SOURCE - ALL VARS
##################################

library (MASS)

# amb els ratios dóna error i a més no calen els ratios (ja dóna 100%)
#lda.HnoH.PS.ALLVARS <- lda (CLASS ~ . -Site-SAMPLES-Date-TARGETtype-SEASON, data=aqua17)

#lda.HnoH.PS.ALLVARS <- lda (CLASS ~ ., data=aqua17[,modeling.vars], CV=FALSE)

lda.HnoH.PS.ALLVARS <- lda (x=aqua17[,modeling.vars], grouping=aqua17$CLASS, CV=FALSE)
plot(lda.HnoH.PS.ALLVARS)

lda.HnoH.PS.ALLVARS <- lda (x=aqua17[,modeling.vars], grouping=aqua17$CLASS, CV=TRUE)
lda.HnoH.PS.ALLVARS$posterior

table(Truth=aqua17$CLASS, Prediction=lda.HnoH.PS.ALLVARS$class)

plot(aqua17$HMBif, aqua17$HF183TaqMan)
text (aqua17$HMBif, aqua17$HF183TaqMan, col=as.numeric(aqua17$CLASS), cex=0.75, adj=c(0.85, 0.85))

#### Ara caldria anar treient variables

source ("C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/toIntegrate/RicardM/FSS.R")

FSS.data <- aqua17[,c(modeling.vars)]

#FSS.data <- aqua17[,c(modeling.vars,modeling.ratios)]
FSS.target <- aqua17$CLASS
FSS.target.name <- "CLASS"

#subset <- forward.search (names(FSS.data), evaluator.accuracy.LDA) aqui hi havia un 2 LDA2

subset <- forward.search (names(FSS.data), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)
subset <- exhaustive.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)

subset <- bounded.exh.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA, 1)

f <- as.simple.formula(subset$vars, "CLASS")
print(f)
subset$acc

glm.HnoH.PS.ALLVARS <- glm (CLASS ~ ., data=aqua17[,modeling.vars], family=binomial)

glm.HnoH.PS.ALLVARS <- step(glm.HnoH.PS.ALLVARS)

preds <- predict(glm.HnoH.PS.ALLVARS, type="response")
preds[preds<0.5] <- 0
preds[preds>0.5] <- 1

table(Truth=aqua17$CLASS, Prediction=preds)
# da 100%

# pillo para LDA las 2 de GLM
lda.HnoH.PS.ALLVARS.glm <- lda (CLASS ~ ., data=aqua17[,c("HMBif","HF183TaqMan")], CV=TRUE)

lda.HnoH.PS.ALLVARS.glm$posterior

table(Truth=aqua17$CLASS, Prediction=lda.HnoH.PS.ALLVARS.glm$class)
# da 99.04%

# Ara GLM amb només els ratios ...

# pillo para LDA las 2 de GLM (2 ratios)
lda.HnoH.PS.ALLVARS.glm <- lda (CLASS ~ ., data=aqua17[,c("TLBif.HMBif","AllBac.HF183Taqman")], CV=TRUE)

lda.HnoH.PS.ALLVARS.glm$posterior

table(Truth=aqua17$CLASS, Prediction=lda.HnoH.PS.ALLVARS.glm$class)

# Ara GLM amb totes ... dóna de nou {HMBif,  HF183TaqMan}


################################## 
# SCENARIO 2
# H/noH - POINT SOURCE - MOLLECS
##################################

mollecs <- c(14:27,32:33)
mollecs.ratios <- c(42:51)

lda.HnoH.PS.MOLLECS <- lda (x=aqua17[,mollecs], grouping=aqua17$CLASS, CV=FALSE)
plot(lda.HnoH.PS.MOLLECS)

lda.HnoH.PS.MOLLECS <- lda (x=aqua17[,mollecs], grouping=aqua17$CLASS, CV=TRUE)
lda.HnoH.PS.MOLLECS$posterior

table(Truth=aqua17$CLASS, Prediction=lda.HnoH.PS.MOLLECS$class)

#### Ara caldria anar treient variables

FSS.data <- aqua17[,c(mollecs)]

#FSS.data <- aqua17[,c(modeling.vars,modeling.ratios)]
FSS.target <- aqua17$CLASS
FSS.target.name <- "CLASS"

subset <- forward.search (names(FSS.data), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)
subset <- exhaustive.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)

subset <- bounded.exh.search (names(aqua17)[mollecs], evaluator.accuracy.LDA, 1)

f <- as.simple.formula(subset$vars, "CLASS")
print(f)
subset$acc

glm.HnoH.PS.MOLLECS <- glm (CLASS ~ ., data=aqua17[,mollecs], family=binomial)

glm.HnoH.PS.MOLLECS <- step(glm.HnoH.PS.MOLLECS)

preds <- predict(glm.HnoH.PS.MOLLECS, type="response")
preds[preds<0.5] <- 0
preds[preds>0.5] <- 1

table(Truth=aqua17$CLASS, Prediction=preds)
# da 100% (es lo = de antes)

# Ara GLM amb totes ... 

glm.HnoH.PS.MOLLECS <- glm (CLASS ~ ., data=aqua17[,c(mollecs,mollecs.ratios)], family=binomial)

glm.HnoH.PS.MOLLECS <- step(glm.HnoH.PS.MOLLECS)

################################## 
# SCENARIO 3
# 4 SOURCES - POINT SOURCE - ALL VARS
##################################

lda.4S.PS.ALLVARS <- lda (x=aqua17[,modeling.vars], grouping=aqua17$TARGETtype, CV=FALSE)
plot(lda.4S.PS.ALLVARS)

idict <- as.numeric(aqua17$TARGETtype)

library("rgl")

aqua.pred <- predict (lda.4S.PS.ALLVARS, aqua17[,modeling.vars])

# surt preciós
plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (original variables)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.03)

text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=aqua17$Site, adj = c(0.85, 0.85), cex=0.75)

########## plot de només HMBif + BacR + Pig2Bac

plot3d (aqua17$HMBif, aqua17$BacR, aqua17$Pig2Bac,
        col = idict,
        xlab = "HMBif", ylab = "BacR", zlab = "Pig2Bac", 
        main = "Four sources (original variables)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.03)

# no s'acaba de veure bé, potser es veu millor la projecció LDA

lda.4S.PS.ALLVARS <- lda (x=subset(aqua17, select=c("HMBif","BacR","Pig2Bac")), grouping=aqua17$TARGETtype, CV=FALSE)
aqua.pred <- predict (lda.4S.PS.ALLVARS)#, aqua17[,modeling.vars])

plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (HMBif + BacR + Pig2Bac)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.03)

# una mica millor ...

text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=aqua17$Site, adj = c(0.85, 0.85), cex=0.75)
##########

aqua.lda <- lda (aqua17$TARGETtype ~ ., aqua17[,modeling.vars], CV=TRUE)
aqua.lda$posterior
aqua.lda$class

table(Truth=aqua17$TARGETtype, Prediction=aqua.lda$class)

#### Ara caldria anar treient variables

FSS.data <- aqua17[,c(modeling.vars)]

#FSS.data <- aqua17[,c(modeling.vars,modeling.ratios)]
FSS.target <- aqua17$TARGETtype
FSS.target.name <- "TARGETtype"

subset <- forward.search (names(FSS.data), evaluator.accuracy.LDA)
subset <- best.first.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)
subset <- exhaustive.search (names(aqua17)[modeling.vars], evaluator.accuracy.LDA)

subset <- bounded.exh.search (names(aqua17)[mollecs], evaluator.accuracy.LDA, 1)

f <- as.simple.formula(subset$vars, "TARGETtype")
print(f)
subset$acc


################################## 
# SCENARIO 4
# 4 SOURCES - POINT SOURCE - MOLLECS
##################################

lda.4S.PS.MOLLECS <- lda (x=aqua17[,mollecs], grouping=aqua17$TARGETtype, CV=FALSE)
plot(lda.4S.PS.MOLLECS)

idict <- as.numeric(aqua17$TARGETtype)

library("rgl")

aqua.pred <- predict (lda.4S.PS.MOLLECS, aqua17[,mollecs])

# surt preciós
plot3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], 
        col = idict,
        xlab = "LD1", ylab = "LD2", zlab = "LD3", 
        main = "Four sources (original variables)", 
        sub = "CW = black | HM = red | PG = green | PL = blue",
        top = TRUE, aspect = FALSE, expand = 1.03)

text3d (aqua.pred$x[,1], aqua.pred$x[,2], aqua.pred$x[,3], texts=aqua17$Site, adj = c(0.85, 0.85), cex=1)

aqua.lda <- lda (aqua17$TARGETtype ~ ., aqua17[,mollecs], CV=TRUE)
aqua.lda$posterior
aqua.lda$class

table(Truth=aqua17$TARGETtype, Prediction=aqua.lda$class)

#### Ara caldria anar treient variables

FSS.data <- aqua17[,c(mollecs)]

#FSS.data <- aqua17[,c(modeling.vars,modeling.ratios)]
FSS.target <- aqua17$TARGETtype
FSS.target.name <- "TARGETtype"

subset <- forward.search (names(FSS.data), evaluator.accuracy.LDA)

subset <- bounded.exh.search (names(aqua17)[mollecs], evaluator.accuracy.LDA, 1)

f <- as.simple.formula(subset$vars, "TARGETtype")
print(f)
subset$acc
