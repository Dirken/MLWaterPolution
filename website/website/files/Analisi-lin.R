# Aquavalens final analysis July 2017

aqua17 <- read.csv(file="C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/toIntegrate/RicardM/AQV_Fase1_final_10ml_Totes_variables_170619.csv",
                   header = TRUE, sep = ";", dec=",", check.names = TRUE, 
                   stringsAsFactors = FALSE)

#---------------------------------------------------------------------------------
# STEP 1:  PREPROCESS DATA FRAME (which is the retouched csv version of sheet one of AQV_Fase1_final_10ml_Totes_variables_170619.xlsx)
#---------------------------------------------------------------------------------

# remove UB's E27HM and E28HM because of lack of a date

aqua17 <- aqua17[-c(27,28),]

# remove last column (X)

aqua17 <- subset(aqua17, select=-c(X))

# add site

Site <- c(rep("UB",times=26),rep("TU WIEN",24),rep("IST",25),rep("DVGW",25),rep("UH",18))

aqua17 <- cbind(Site,aqua17)
colnames(aqua17)
#[1] "Site"        "SAMPLES"     "Date"        "EC"          "FE"          "CP"         
#[7] "SomPhg"      "HMBactPhg"   "CWBactPhg"   "PGBactPhg"   "PLBactPhg"   "BifSorb"    
#[13] "BifTot"      "HMBif"       "CWBif"       "PGNeo"       "PLBif"       "TLBif"      
#[19] "BacR"        "Pig2Bac"     "AllBac"      "HF183TaqMan" "FEqPCR"      "HMMit"      
#[25] "CWMit"       "PGMit"       "PLMit"       "Acesulfame"  "Cyclamate"   "Saccharain" 
#[31] "Sucralose"   "Adeno"       "NoV"        

#HM is human (WW, if any, is wastewater, so also human), CW is cow , PG is pig , PL is poultry)   
#The rest of the animals are not to be considered

# create target classes (CLASS for human/non-human, TARGETtype for 4 sources)

aqua17$CLASS <- -1
aqua17$TARGETtype <- as.factor(substr(aqua17$SAMPLES,4,5))
summary(aqua17$TARGETtype)
# BD CT CW DG GO HM HO PG PL RA 
#  1  2 23  2  1 33  7 24 24  1 

aqua17[which(grepl("HM",aqua17$SAMPLES)),]$CLASS <- 1     #1 REFERS TO HUMAN, -1 TO ANIMALS

aqua17$CLASS <- factor(aqua17$CLASS)     
levels(aqua17$CLASS) <- c("nonhuman","human")

# create SEASON

get.season <- function (s)
{ 
  #April to September is SUMMER; the rest is WINTER) 
  season = "WINTER"
  month <- strtoi(strsplit(s,"/")[[1]][2],base=10)
  if (month > 3 && month < 10) {season = "SUMMER"}
  season
}

aqua17$SEASON <- as.factor(sapply(aqua17$Date, get.season))

# NOW TAKE CARE OF WEIRD VALUES and NAs

# what to do about NA in NoV column?
which(is.na(aqua17$NoV))
#[1]  32  38  41  43  44  50  74  75  80  85  89  94  99 100

# Solution: set these to zero
aqua17[which(is.na(aqua17$NoV)),]$NoV <- 0

# Replace the "<x" values with 0. Go through columns EC(4) to column NoV (33) and make all these columns numeric along the way

modeling.vars <- 4:33

for (i in modeling.vars)
{
  aqua17[ which(grepl("<",aqua17[,i])),i ] <- 0
  aqua17[,i] <- as.numeric(aqua17[,i])
}

summary(aqua17)
dim(aqua17)

#---------------------------------------------------------------------------------
#STEP 2 HANDLE DETECTIONS  ("Det limit" sheet in the Excel)
#In this sheet you will find the detection limit (call it threshold or Tau). 
#When you dilute and/or age a value, if it goes below the Tau for the variable, then make it 0. 
#The Tau should be the same for all the observations for the same variable
#---------------------------------------------------------------------------------

#These are the log values:

detectors  <- read.table("C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/toIntegrate/RicardM/log-detectors.tsv",header = TRUE, sep = "\t", dec=".", stringsAsFactors = FALSE)

colnames(detectors)[1] <- "Site"

detectors[1:26,1] <- "UB"; detectors[27:50,1] <- "TU WIEN"; detectors[51:75,1] <- "IST"; detectors[76:100,1] <- "DVGW"; detectors[101:118,1] <- "UH"

colnames(detectors)
# [1] "Site"        "SAMPLES"     "Date"        "EC"          "FE"          "CP"          "SomPhg"      "HMBactPhg"   "CWBactPhg"  
# [10] "PGBactPhg"   "PLBactPhg"   "BifSorb"     "BifTot"      "HMBif"       "CWBif"       "PGNeo"       "PLBif"       "TLBif"      
# [19] "BacR"        "Pig2Bac"     "AllBac"      "HF183TaqMan" "FEqPCR"      "HMMit"       "CWMit"       "PGMit"       "PLMit"      
# [28] "Acesulfame"  "Cyclamate"   "Saccharain"  "Sucralose"   "Adeno"       "NoV"

# in fact if you look at the log of the detectors, they are all the same for each column so we just need that vector
#colMeans(detectors[,attributes(aslr$sum)$names])
detectors <- detectors[1,]

#---------------------------------------------------------------------------------
#STEP 3 HANDLE ASSAYS ( from Persist_150114.xls)
#---------------------------------------------------------------------------------

#EVERYTHING IS IDENTICAL EXCEPT NoV tab, I just needed to change the NoV files in the "persist" folder

aged_samples_lr <- function( season , orig_data , correction , mean_coef , plot ){
  aged_samples_lr_list <- list()      # list that will be returned
  if ( plot ){ par( ask = TRUE ) }   # if plot is TRUE: requiring user key event to continue interpreting code
  for ( attr in c("EC","FE","CP","SomPhg","HMBactPhg","CWBactPhg","PGBactPhg","PLBactPhg","BifSorb", "BifTot",
                  "HMBif", "CWBif", "PGNeo", "PLBif","TLBif", "NoV") ){
    
    # reading all the samples for the corresponding attribute and season, BifMol tab
    aged_samples_file_name <- paste( "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/website/persist/mediterrani/" , attr , "-" , season , ".txt" , sep = "" )
    if(attr %in% c("TLBif", "HMBif", "CWBif", "PGNeo", "PLBif") ){
      aged_samples_file_name <- paste( "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/website/persist/mediterrani/" , "BifMol" , "-" , season , ".txt" , sep = "" )
    }
    # reading all the samples for the corresponding attribute and season, BactPhg tab
    if(attr %in% c("HMBactPhg","CWBactPhg","PGBactPhg","PLBactPhg") ){
      aged_samples_file_name <- paste( "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/website/persist/mediterrani/" , "BactPhg" , "-" , season , ".txt" , sep = "" )
    }
    
    aged_samples <- read.csv( blank.lines.skip=TRUE, file = aged_samples_file_name , header = FALSE , sep = "" , col.names = c( "time" , "value" ), comment.char="#",dec = ","  )    
    essays_coef <- list() # where the assays LR coefficients (slope and increment) will be stored
    
    # obtaining in which indexes of the aged samples start each one of the essays (new essay => t=0)
    essay_splits <- c( which( aged_samples$time == 0 ) , length( aged_samples$time ) + 1 )
    essay_splits <- essay_splits[ -1 ] # removing the first (there will always be a zero on first aged_samples row)
    last_split <- 1 # initializing last split variable to the beginning of th first essay
    
    # for each one of the essays of the current attribute and season
    for ( split in essay_splits ){      # obtaining the samples of the current essay      
      essay_samples <- aged_samples[ last_split : ( split - 1 ) , ] # selecting only the essay_samples
      
      #TODO: will we use this? probably not, but if so we need to move HUMAN constant to top
      if ( correction ){    #values are considered slightly diluted and therefore are corrected
        # correction is done using the log10 of the median of the attribute human rows of the original matrix
        essay_samples[ , 2 ] <- essay_samples[ , 2 ] + 
          log10( median( orig_data[ which( orig_data$CLASS == HUMAN ) , attr ] ) ) - aged_samples[ last_split , 2 ]  
      }
      
      # performing LR over the current essay values and storing in the list of all the essays LR coefficients
      essays_coef <- c( essays_coef , list( lm( value ~ time , data = essay_samples )$coefficients ) )
      
      if ( plot ){ plot( essay_samples[ , 1 ] , essay_samples[ , 2 ], main = attr ) }  #plotting the samples and the line
      if ( plot ){ abline( coef = lm( value ~ time , data = essay_samples )$coefficients ) } #corresponding to the regression
      
      last_split <- split# updating last_split variable for the next iteration
    }
    
    # converting essay_coef list to a matrix, will have two columns (intercept and slope) and as rows as essays
    essays_coef <- do.call( rbind , essays_coef )
    
    # if mean_coef is TRUE, the mean of all essays slope and increment is returned, 
    # otherwise a matrix with the slopes and increments for each one of the essays is returned
    if ( mean_coef ){ aged_samples_lr_list[[ attr ]] <- c( mean( essays_coef[ , 1 ] ) , mean( essays_coef[ , 2 ] ) )} 
    else { aged_samples_lr_list[[ attr ]] <- essays_coef }    
  }
  
  aged_samples_lr_list    # returning list with all the results
}

# STEP4.1 computing logistic regressions for ageing essays on both seasions (assuming no correction) to get slope and increment

aslr <- c()
aslr$sum <- aged_samples_lr( "SUMMER" , c() , correction=FALSE , mean_coef=TRUE , plot=FALSE )
aslr$win <- aged_samples_lr( "WINTER" , c() , correction=FALSE , mean_coef=TRUE , plot=FALSE )

#STEP4.2 calculate slope/intercepts for non-assays variables where t90 or t99 given

#Columns with K values ( in tab Bact qPCR Austria) -> BacR, Pig2Bac, AllBac, HF183TaqMan, FEqPCR
#HMMit,CWMit, PGMit, PLMit and Adeno  (given in Mitoc Portugal tab)

#IMPORTANT: some times are in days, so be sure to unify everything to hours

# K is exactly the slope. Now K = -1/T90 = -2/T99, whichever is given
# (intercept, slope) for each variable and season
aslr$sum$BacR        <- c(0, -1/0.64); aslr$win$BacR         <- c(0, -1/3.57);
aslr$sum$Pig2Bac     <- c(0, -1/0.68); aslr$win$Pig2Bac      <- c(0, -1/2.22);
aslr$sum$AllBac      <- c(0, -1/5.88); aslr$win$AllBac       <- c(0, -1/20.0);
aslr$sum$HF183TaqMan <- c(0, -1/0.50); aslr$win$HF183TaqMan  <- c(0, -1/1.59);
aslr$sum$FEqPCR      <- c(0, -1/0.77); aslr$win$FEqPCR       <- c(0, -1/2.50);

# PORTUGAL
# in T_99 for summer and T_90 for winter
aslr$sum$HMMit <- c(0, -2/(15*24));   aslr$win$HMMit <- c(0, -1/(15*24));     
aslr$sum$CWMit <- c(0, -2/(1*24));    aslr$win$CWMit <- c(0, -1/(9*24));      
aslr$sum$PGMit <- c(0, -2/(1*24));    aslr$win$PGMit <- c(0, -1/(6*24));      
aslr$sum$PLMit <- c(0, -2/(1*24));    aslr$win$PLMit <- c(0, -1/(6*24));   
aslr$sum$Adeno <- c(0, -2/(290*24));  aslr$win$Adeno <- c(0, -1/(290*24));   

# AUSTRIA
# slope = (2 - log10(k))/T
aslr$sum$Acesulfame  <- c(0, 0)            ;aslr$win$Acesulfame    <- c(0, 0);  
aslr$sum$Cyclamate   <- c(0, 0.005555676)  ;aslr$win$Cyclamate     <- c(0, 0.005555676);  
aslr$sum$Saccharain  <- c(0, 0.004166757)  ;aslr$win$Saccharain    <- c(0, 0.004166757);
aslr$sum$Sucralose   <- c(0, 0)            ;aslr$win$Sucralose     <- c(0, 0);


#----- At this point 
colnames(aqua17[,modeling.vars])
#[1] "EC"          "FE"          "CP"          "SomPhg"      "HMBactPhg"   "CWBactPhg"  
#[7] "PGBactPhg"   "PLBactPhg"   "BifSorb"     "BifTot"      "HMBif"       "CWBif"      
#[13] "PGNeo"       "PLBif"       "TLBif"       "BacR"        "Pig2Bac"     "AllBac"     
#[19] "HF183TaqMan" "FEqPCR"      "HMMit"       "CWMit"       "PGMit"       "PLMit"      
#[25] "Acesulfame"  "Cyclamate"   "Saccharain"  "Sucralose"   "Adeno"       "NoV"     

attributes(aslr$sum)  #same for aslr$winter

#----------------------------------------------------------------
# STEP 5: remove little animals, create ratios, take logs
#----------------------------------------------------------------

attach(aqua17)
indexes <- (TARGETtype == 'CW' | TARGETtype == 'HM' | TARGETtype == 'PG' | TARGETtype == 'PL')

aqua17.4S <- aqua17[indexes,]

# re-factor
aqua17.4S$TARGETtype <- factor(aqua17.4S$TARGETtype)
detach(aqua17)

aqua17 <- aqua17.4S
rm (aqua17.4S)

# dim(aqua17)
# [1] 104 36

### MAKE a COPY for later (BIG MATRIX)
aqua17.preBIG <- aqua17

# Anicet's ratios
attach(aqua17)

aqua17$SomPhg.HMBactPhg <- (aqua17$SomPhg + 1)/(aqua17$HMBactPhg + 1)
aqua17$SomPhg.CWBactPhg <- (aqua17$SomPhg + 1)/(aqua17$CWBactPhg + 1)
aqua17$SomPhg.PGBactPhg <- (aqua17$SomPhg + 1)/(aqua17$PGBactPhg + 1)
aqua17$SomPhg.PLBactPhg <- (aqua17$SomPhg + 1)/(aqua17$PLBactPhg + 1)

aqua17$BifTot.BifSorb <- (aqua17$BifTot + 1)/(aqua17$BifSorb + 1)

aqua17$TLBif.HMBif      <- (aqua17$TLBif  + 1)/(aqua17$HMBif  + 1)
aqua17$TLBif.CWBif      <- (aqua17$TLBif  + 1)/(aqua17$CWBif  + 1)
aqua17$TLBif.PGNeo      <- (aqua17$TLBif  + 1)/(aqua17$PGNeo  + 1)
aqua17$TLBif.PLBif      <- (aqua17$TLBif  + 1)/(aqua17$PLBif  + 1)

aqua17$AllBac.BacR     <- (aqua17$AllBac + 1)/(aqua17$BacR   + 1)
aqua17$AllBac.Pig2Bac  <- (aqua17$AllBac + 1)/(aqua17$Pig2Bac + 1)
aqua17$AllBac.HF183Taqman  <- (aqua17$AllBac + 1)/(aqua17$HF183TaqMan + 1)

# Andreas's ratios
aqua17$FEqPCR.BacR     <- (aqua17$FEqPCR + 1)/(aqua17$BacR   + 1)
aqua17$FEqPCR.Pig2Bac  <- (aqua17$FEqPCR + 1)/(aqua17$Pig2Bac + 1)
aqua17$FEqPCR.HF183Taqman  <- (aqua17$FEqPCR + 1)/(aqua17$HF183TaqMan + 1)

modeling.ratios <- 37:51

# take logs

for (attr in modeling.vars)
  aqua17[, attr] <- log10(aqua17[, attr] + 1)    #add one and take logs for SINGLE

for (attr in modeling.ratios)
  aqua17[, attr] <- log10(aqua17[, attr])    # we already added one, so safe

# good moment to save the data into a file (OJO genera NAs)

write.csv(aqua17, file="aqua17-july17.csv")
rm(aqua17)
aqua17 <- read.csv(file="aqua17-july17.csv")
aqua17 <- subset(aqua17, select=-X)

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

#---------------------------------------------------------------------------------
# STEP 6: CREACIO DE LA BIGMATRIX
#---------------------------------------------------------------------------------

# do AGE-DILUTION

age_dataset <- function( dataset, age_lr  ){  
  list_aged_data <- list()    # list to store all the diluted datasets
  AGE_SECTIONS = c(0)         #, 25, 50, 75, 100, 125, 150)   #Q3:  Are these the correct AGE SECTIONS to use?
  for ( time in AGE_SECTIONS ){      # for all the values that are in dilution_degrees
    # a data.frame containing dataset diluted by dilution_degree is added at the end of the returning list
    list_aged_data[[ length( list_aged_data ) + 1 ]] <- 
      data.frame( do.call( "rbind" , by( data = dataset , simplify = FALSE , INDICES = 1 : nrow( dataset ) , FUN = age_instance , time , age_lr ) ) )
  }
  list_aged_data    # returning list
}

age_instance <- function(instance, time, age_lr) 
{
  aging_related_attrs <- attributes(aslr$sum)$names
  SLOPE <- 2    
  for (attr in aging_related_attrs){    
    val <- instance[, attr]
    if (val < 0) {
      cat(paste("Error: Reading a negative value of", val, "for attribute", attr))
    } else if (val > 0) {
      aged_val <- max(10^(log10(val+1) + age_lr[[attr]][SLOPE]*time), 0)
      instance[, attr] <- aged_val
    }
  }
  instance
}

aged_data_summer <- aqua17[which(aqua17.preBIG$SEASON == "SUMMER"),]
aged_data_winter <- aqua17[which(aqua17.preBIG$SEASON == "WINTER"),]


# take into account detectors, add ratios,and APPLY logs

capalog10 <- function(x)
{
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (x[i] <= 0) { x[i] <- 0 } 
      else { x[i] <- log10(x[i]) }
    }
  }
  x
}

process <- function(data)
{
  # diluting 3 logs amounts to subtract 3 to the current value
  # aging 2h amounts to adding a*2 to the current value, being 'a' the slope for the variable and season
  
  initial.data <- data
  
  #HANDLE PRE-RATIO
  data.pre.ratio <- data
  for (attr in attributes(aslr$sum)$names) { 
    data.pre.ratio[, attr] <- capalog10(data.pre.ratio[, attr] + 1)    #add one and take logs
  }
  
  #ADD RATIOS (after aging !)
  # We have inverted the ratios (and renamed variables to match those in the main excel file):
  #In all cases, in order to avoid singularities, add +1 to both numerator and denominator
  # before performing the quotient (thus is done after the <x are converted to 0).
  # Finally take the log10 of the quotient.
  # SomPhg / HMBactPhg ,  SomPhg / CWBactPhg,   SomPhg / PGBactPhg ,   SomPhg / PLBactPhg
  # BifTot / BifSorb
  # TLBif / HMBif,        TLBif / CWBif,        TLBif / PGNeo,         TLBif / PLBif
  # Allbac / BacR ,       Allbac / Pig2Bac,     Allbac / HF183Taqman
  
  #a) using all the original variables + Anicet's ratios + Andreas' ratios
  
  # Anicet's ratios
  data$SomPhg.HMBactPhg <- (data$SomPhg + 1)/(data$HMBactPhg + 1)
  data$SomPhg.CWBactPhg <- (data$SomPhg + 1)/(data$CWBactPhg + 1)
  data$SomPhg.PGBactPhg <- (data$SomPhg + 1)/(data$PGBactPhg + 1)
  data$SomPhg.PLBactPhg <- (data$SomPhg + 1)/(data$PLBactPhg + 1)
  
  data$BifTot.BifSorb <- (data$BifTot + 1)/(data$BifSorb + 1)
  
  data$TLBif.HMBif      <- (data$TLBif  + 1)/(data$HMBif  + 1)
  data$TLBif.CWBif      <- (data$TLBif  + 1)/(data$CWBif  + 1)
  data$TLBif.PGNeo      <- (data$TLBif  + 1)/(data$PGNeo  + 1)
  data$TLBif.PLBif      <- (data$TLBif  + 1)/(data$PLBif  + 1)
  
  data$AllBac.BacR     <- (data$AllBac + 1)/(data$BacR   + 1)
  data$AllBac.Pig2Bac  <- (data$AllBac + 1)/(data$Pig2Bac + 1)
  data$AllBac.HF183Taqman  <- (data$AllBac + 1)/(data$HF183TaqMan + 1)
  
  # Andreas's ratios
  data$FEqPCR.BacR     <- (data$FEqPCR + 1)/(data$BacR   + 1)
  data$FEqPCR.Pig2Bac  <- (data$FEqPCR + 1)/(data$Pig2Bac + 1)
  data$FEqPCR.HF183Taqman  <- (data$FEqPCR + 1)/(data$HF183TaqMan + 1)
  
  ratios <- c("SomPhg.HMBactPhg", "SomPhg.CWBactPhg", "SomPhg.PGBactPhg", "SomPhg.PLBactPhg",
  "BifTot.BifSorb", "TLBif.HMBif", "TLBif.CWBif", "TLBif.PGNeo", "TLBif.PLBif", "AllBac.BacR",
  "AllBac.Pig2Bac", "AllBac.HF183Taqman", "FEqPCR.BacR", "FEqPCR.Pig2Bac", "FEqPCR.HF183Taqman")
  
  for (attr in attributes(aslr$sum)$names) {
    data[, attr] <- capalog10(data[, attr] + 1) 
  }      
  
  for (attr in ratios) { 
    data[, attr] <- capalog10(data[, attr]) 
    
    #Take into account detectors (values in detectors are already log based)
    #When you dilute and/or age a value, if it goes below the tau for the variable, then make it 0. 
    #The Tau should be the same for all the observations for the same variable (this was not true initially, hence matri). 
    if(attr %in% attributes(aslr$sum)$names){
      data[which(data[,attr] < detectors[,attr]),attr] <- 0 #detectors[,attr]
    }
  }
  #SO NOW "data"  contains all case A
  caseA <- data
  
  #NOW FOR CASE B
  #b) using the molecular variables only + Andreas' ratios
  #the molecular variables are:
  #   HMBif, CWBif, PGNeo, PLBif, TLBif, BacR, Pig2Bac, AllBac, HF183TaqMan, FEqPCR
  #   HMMit, CWMit, PGMit, PLMit, Adeno, NoV
  caseB <- as.data.frame(cbind(initial.data$HMBif, initial.data$CWBif, initial.data$PGNeo, initial.data$PLBif, initial.data$TLBif, initial.data$BacR, initial.data$Pig2Bac, initial.data$AllBac, initial.data$HF183TaqMan, initial.data$FEqPCR, initial.data$HMMit, initial.data$CWMit, initial.data$PGMit, initial.data$PLMit, initial.data$Adeno, initial.data$NoV))
  
  # Anicet's ratios
  caseB$TLBif.HMBif      <- (initial.data$TLBif  + 1)/(initial.data$HMBif  + 1)
  caseB$TLBif.CWBif      <- (initial.data$TLBif  + 1)/(initial.data$CWBif  + 1)
  caseB$TLBif.PGNeo      <- (initial.data$TLBif  + 1)/(initial.data$PGNeo  + 1)
  caseB$TLBif.PLBif      <- (initial.data$TLBif  + 1)/(initial.data$PLBif  + 1)
  
  caseB$AllBac.BacR     <- (initial.data$AllBac + 1)/(initial.data$BacR   + 1)
  caseB$AllBac.Pig2Bac  <- (initial.data$AllBac + 1)/(initial.data$Pig2Bac + 1)
  caseB$AllBac.HF183Taqman  <- (initial.data$AllBac + 1)/(initial.data$HF183TaqMan + 1)
  
  # Andreas's ratios
  caseB$FEqPCR.BacR     <- (initial.data$FEqPCR + 1)/(initial.data$BacR   + 1)
  caseB$FEqPCR.Pig2Bac  <- (initial.data$FEqPCR + 1)/(initial.data$Pig2Bac + 1)
  caseB$FEqPCR.HF183Taqman  <- (initial.data$FEqPCR + 1)/(initial.data$HF183TaqMan + 1)
  
  colnames(caseB)[1:16] <- c("HMBif", "CWBif", "PGNeo", "PLBif", "TLBif", "BacR", "Pig2Bac", "AllBac", "HF183TaqMan", "FEqPCR", "HMMit", "CWMit", "PGMit", "PLMit", "Adeno", "NoV" )
  
  #now take logs for caseB
  for (attr in attributes(caseB)$names) { 
    caseB[, attr] <- capalog10(caseB[, attr]) 
    
    #Take into account detectors (values in detectors are already log based)
    #When you dilute and/or age a value, if it goes below the tau for the variable, then make it 0. 
    #The Tau should be the same for all the observations for the same variable (this was not true initially, hence matri). 
    if(attr %in% attributes(aslr$sum)$names){
      caseB[which(caseB[,attr] < detectors[,attr]),attr] <- 0 #detectors[,attr]
    }
  }
  caseB$CLASS <- initial.data$CLASS
  caseB$TARGETtype <- initial.data$TARGETtype
  
  #list( data.w.ratios = data , data.no.ratios = data.pre.ratio)
  list( caseA = caseA, caseB = caseB, data.no.ratios = data.pre.ratio)
}

#dim(aged_data_summer) #54 51
#dim(aged_data_winter) #50 51
combined <- rbind(aged_data_summer,aged_data_winter)
#104 51

#process data now 
aged_processed_data <- process( combined )
aged_processed_data.no.ratios <- aged_processed_data$data.no.ratios
aged_processed_data.caseA <- aged_processed_data$caseA
aged_processed_data.caseB <- aged_processed_data$caseB

colnames(aged_processed_data.caseA)
colnames(aged_processed_data.caseB)

#write.csv(aged_processed_data.caseA, 
#          file="/Users/dolano/Documents/UPC/belanche-research/jan2015/processed-datasets/aged_processed_data_caseA.csv")

#write.csv(aged_processed_data.caseB,
#          file="/Users/dolano/Documents/UPC/belanche-research/jan2015/processed-datasets/aged_processed_data_caseB.csv")

aged_data_summer.big <- age_dataset( aqua17.preBIG[which(aqua17.preBIG$SEASON == "SUMMER"),] , aslr$sum )  
aged_data_winter.big <- age_dataset( aqua17.preBIG[which(aqua17.preBIG$SEASON == "WINTER"),] , aslr$win )  

combined.big <- rbind(aged_data_summer.big[[1]],aged_data_winter.big[[1]])     #104 36

#process data now 
aged_processed_data.big <- process( combined.big )                            
aged_processed_data.no.ratios.big <- aged_processed_data.big$data.no.ratios

M <- 10000

#Let "alpha" denote the dilution degree of the observation and "t" denote the time in water.

alphas <- rlnorm(M, 0, 0.5)
times  <- rexp(M, 2)*100

hist(alphas,probability = TRUE,xlab="dilution degree",breaks=30); lines(density(alphas),col="red",lwd=1)
hist(times,probability = TRUE,xlab="time in water",breaks=30); lines(density(times),col="red",lwd=1)

dataf.comb.no.ratios   <- aged_processed_data.no.ratios.big   #104 36
                        #rbind(aged_processed_data_winter.no.ratios,aged_processed_data_summer.no.ratios)

dataf.comb.nr.class.target <- dataf.comb.no.ratios[,c(4:34,35,36)]   #104 33

#Save intercepts/slopes
write.csv(aslr$sum,file="intercepts/summer-intercept-slopes.csv",row.names=FALSE)
write.csv(aslr$win,file="intercepts/winter-intercept-slopes.csv",row.names=FALSE)

#big matrix CLASS and TARGET no ratio(nr)   
#   GENERATE BIG MATRIX
big.matrix.nr.class.target <- dataf.comb.nr.class.target[sample(1:nrow(dataf.comb.nr.class.target),size=M,replace=TRUE),]        #10000 33

#age and then derive new matrices
bm.vars <- big.matrix.nr.class.target[,append("SEASON",attributes(aslr$sum)$names)]
bm.vars.sum <- bm.vars[which(bm.vars$SEASON == "SUMMER"),-1]
bm.vars.win <- bm.vars[which(bm.vars$SEASON == "WINTER"),-1]

#age summer
for (ro in 1:nrow(bm.vars.sum))
{
  curr <- bm.vars.sum[ro,]
  for (co in 1:ncol(bm.vars.sum))
  {
    curval <- curr[co]
    varname <- colnames(bm.vars.sum)[co]
    slope <- aslr$sum[varname][[1]][2]
    curtau <- detectors[varname]
    curalpha <- alphas[ro]
    curtime <- times[ro]      
    newval  <- (curval - curalpha) + (slope * curtime)   
    if(newval < curtau){ newval <- 0}#curtau}      
    bm.vars.sum[ro,co] <- newval
  }
}
dim(bm.vars.sum)   #5226 30

#age winter
for (ro in 1:nrow(bm.vars.win))
{
  curr <- bm.vars.win[ro,]
  for(co in 1:ncol(bm.vars.win)){
    curval <- curr[co]
    varname <- colnames(bm.vars.sum)[co]
    slope <- aslr$win[varname][[1]][2]
    curtau <- detectors[varname]
    curalpha <- alphas[ro]
    curtime <- times[ro]      
    newval  <- (curval - curalpha) + (slope * curtime)   
    if(newval < curtau){ newval <- 0}#curtau}      
    bm.vars.win[ro,co] <- newval
  }
}
dim(bm.vars.win)  #4774 30


#add back season, class, target etc  and construct 3 matrices below
bigsummer.class.nr <- cbind(bm.vars.sum, big.matrix.nr.class.target[which(big.matrix.nr.class.target$SEASON == "SUMMER"),c(31,32,33)])
bigwinter.class.nr <- cbind(bm.vars.win, big.matrix.nr.class.target[which(big.matrix.nr.class.target$SEASON == "WINTER"),c(31,32,33)])
combined.class.nr <- rbind(bigsummer.class.nr,bigwinter.class.nr)
dim(combined.class.nr)
#10000 33
write.csv(combined.class.nr, file="BIGMATRIXcombined-preratio.csv")


#NOW CONSTRUCT CASEA AND CASEB sets
add.caseA.ratios <- function (data)
{
  # Anicet's ratios
  data$SomPhg.HMBactPhg <- (data$SomPhg + 1)/(data$HMBactPhg + 1)
  data$SomPhg.CWBactPhg <- (data$SomPhg + 1)/(data$CWBactPhg + 1)
  data$SomPhg.PGBactPhg <- (data$SomPhg + 1)/(data$PGBactPhg + 1)
  data$SomPhg.PLBactPhg <- (data$SomPhg + 1)/(data$PLBactPhg + 1)
  
  data$BifTot.BifSorb <- (data$BifTot + 1)/(data$BifSorb + 1)
  
  data$TLBif.HMBif      <- (data$TLBif  + 1)/(data$HMBif  + 1)
  data$TLBif.CWBif      <- (data$TLBif  + 1)/(data$CWBif  + 1)
  data$TLBif.PGNeo      <- (data$TLBif  + 1)/(data$PGNeo  + 1)
  data$TLBif.PLBif      <- (data$TLBif  + 1)/(data$PLBif  + 1)
  
  data$AllBac.BacR     <- (data$AllBac + 1)/(data$BacR   + 1)
  data$AllBac.Pig2Bac  <- (data$AllBac + 1)/(data$Pig2Bac + 1)
  data$AllBac.HF183Taqman  <- (data$AllBac + 1)/(data$HF183TaqMan + 1)
  
  # Andreas's ratios
  data$FEqPCR.BacR     <- (data$FEqPCR + 1)/(data$BacR   + 1)
  data$FEqPCR.Pig2Bac  <- (data$FEqPCR + 1)/(data$Pig2Bac + 1)
  data$FEqPCR.HF183Taqman  <- (data$FEqPCR + 1)/(data$HF183TaqMan + 1)
  
  data
}

add.caseB.ratios <- function (initial.data)
{
  caseB <- as.data.frame(cbind(initial.data$HMBif, initial.data$CWBif, initial.data$PGNeo, initial.data$PLBif, initial.data$TLBif, initial.data$BacR, initial.data$Pig2Bac, initial.data$AllBac, initial.data$HF183TaqMan, initial.data$FEqPCR, initial.data$HMMit, initial.data$CWMit, initial.data$PGMit, initial.data$PLMit, initial.data$Adeno, initial.data$NoV))
  
  # Anicet's ratios
  caseB$TLBif.HMBif      <- (initial.data$TLBif  + 1)/(initial.data$HMBif  + 1)
  caseB$TLBif.CWBif      <- (initial.data$TLBif  + 1)/(initial.data$CWBif  + 1)
  caseB$TLBif.PGNeo      <- (initial.data$TLBif  + 1)/(initial.data$PGNeo  + 1)
  caseB$TLBif.PLBif      <- (initial.data$TLBif  + 1)/(initial.data$PLBif  + 1)
  
  caseB$AllBac.BacR     <- (initial.data$AllBac + 1)/(initial.data$BacR   + 1)
  caseB$AllBac.Pig2Bac  <- (initial.data$AllBac + 1)/(initial.data$Pig2Bac + 1)
  caseB$AllBac.HF183Taqman  <- (initial.data$AllBac + 1)/(initial.data$HF183TaqMan + 1)
  
  # Andreas's ratios
  caseB$FEqPCR.BacR     <- (initial.data$FEqPCR + 1)/(initial.data$BacR   + 1)
  caseB$FEqPCR.Pig2Bac  <- (initial.data$FEqPCR + 1)/(initial.data$Pig2Bac + 1)
  caseB$FEqPCR.HF183Taqman  <- (initial.data$FEqPCR + 1)/(initial.data$HF183TaqMan + 1)
  
  colnames(caseB)[1:16] <- c("HMBif", "CWBif", "PGNeo", "PLBif", "TLBif", "BacR", "Pig2Bac", "AllBac", "HF183TaqMan", "FEqPCR", "HMMit", "CWMit", "PGMit", "PLMit", "Adeno", "NoV" )
  caseB
}

bigmatrix.caseA <- add.caseA.ratios(combined.class.nr)
dim(bigmatrix.caseA)
#[1] 10000    48
bigmatrix.caseB <- add.caseB.ratios(combined.class.nr)
dim(bigmatrix.caseB)
#10000    26

write.csv(bigmatrix.caseA, file="processed-datasets/BIGMATRIXcaseA.csv")
write.csv(bigmatrix.caseB, file="processed-datasets/BIGMATRIXcaseB.csv")


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
