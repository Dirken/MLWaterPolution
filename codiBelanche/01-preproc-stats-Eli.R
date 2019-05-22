#####################################
#####################################
#
# AQUAVALENS FINAL ANALYSIS May 2019
#
#####################################
#####################################


setwd("~/N O W/AQV MAY 2019 - Eli/Code")

#---------------------------------------------------------------------------------
# STEP 0: LOAD & PREPROCESS DATA FRAME
#---------------------------------------------------------------------------------

# this csv file is obtained by saving from the xls file directly (; and ") and removing the last (empty) rows

aqua18 <- read.csv (file="../DATA/AQV_Fase1_final_variables_Fase2_170619.csv",
                    header = TRUE, sep = ";", dec=",", check.names = TRUE, 
                    stringsAsFactors = FALSE)



# remove non-useful rows (the 10ml header and several empty lines) and renumber

aqua18 <- aqua18[-c(1,30,55,56,82,108),]
rownames(aqua18) <- 1:nrow(aqua18)

# add site for all obs

aqua18 <- subset(aqua18, select=-c(X))
Site <- c(rep("UB",times=28),rep("TU WIEN",24),rep("IST",25),rep("DVGW",25),rep("UH",18))

aqua18 <- cbind(Site,aqua18)
colnames(aqua18)
# [1] "Site"    "SAMPLES" "Date"    "EC"      "FE"      "CP"      "SOMCPH"  "GA17PH"  "CWPH"    "PGPH"   
# [11] "HMBif"   "CWBif"   "PGNeo"   "PLBif"   "TLBif"   "BacR"    "Pig2Bac" "AllBac"  "HF183"   "FEqPCR" 
# [21] "CWMit"   "PGMit"   "PLMit"   "NoV"    

dim(aqua18)
# 120 obs, 24 vars

# HM is human (WW, if any, is wastewater, so also human), CW is cow , PG is pig , PL is poultry)   
# The rest of the animals are not to be considered

# create target classes (CLASS for human/non-human, TARGETtype for 4 sources)

aqua18$CLASS <- -1
aqua18$TARGETtype <- as.factor(substr(aqua18$SAMPLES,4,5))
summary(aqua18$TARGETtype)
# BD CT CW DG GO HM HO PG PL RA 
#  1  2 23  2  1 35  7 24 24  1 

aqua18[which(grepl("HM",aqua18$SAMPLES)),]$CLASS <- 1     # 1 REFERS TO HUMAN, -1 TO NON-HUMANS

aqua18$CLASS <- factor(aqua18$CLASS)     
levels(aqua18$CLASS) <- c("Non-human","Human")

# create SEASON

get.season <- function (s)
{ 
  #April to September is SUMMER; the rest is WINTER) 
  season = "WINTER"
  month <- strtoi(strsplit(s,"/")[[1]][2],base=10)
  if (month > 3 && month < 10) {season = "SUMMER"}
  season
}

aqua18$SEASON <- as.factor(sapply(aqua18$Date, get.season))

# NOW TAKE CARE OF WEIRD VALUES and NAs

# what to do about NA in NoV column?
which(is.na(aqua18$NoV))
#[1] 34  40  43  45  46  52  76  77  82  87  91  96 101 102

# Solution: set these to zero
aqua18[which(is.na(aqua18$NoV)),]$NoV <- 0

# Replace the "<x" values with 0. Go through columns EC(4) to column NoV (24) and make all these columns numeric along the way

modeling.vars <- 4:24

# alternatives are 0 or x/10
for (i in modeling.vars)
{
  ## <x ---> 0
  #aqua18[ which(grepl("<",aqua18[,i])),i ] <- 0
  #aqua18[,i] <- as.numeric(sub(",", ".", aqua18[,i]))
  
  ## <x ---> x/10
  ind <- which(grepl("<",aqua18[,i]))
  aqua18[ind,i] <- sub("<", "", aqua18[ind,i])

  aqua18[,i] <- as.numeric(sub(",", ".", aqua18[,i]))  
  aqua18[ind,i] <- aqua18[ind,i]/10
}

# put dates correctly in R
aqua18[,"Date"] <- as.Date(aqua18[,"Date"], "%d/%m/%Y") 
aqua18[,"Date"] <- format(aqua18[,"Date"], format="%d %B %Y")

summary(aqua18)
dim(aqua18)
#[1] 120  27

# HANDLE DETECTIONS  ("Det limit" sheet in the Excel)
# In this sheet you will find the detection limit (call it threshold or Tau). 
# When you dilute and/or age a value, if it goes below the Tau for the variable, then make it 0. 
# The Tau should be the same for all the observations for the same variable

#These are the log values:

detectors  <- read.table("../DATA/log-detectors.tsv",header = TRUE, sep = "\t", dec=".", stringsAsFactors = FALSE)

colnames(detectors)[1] <- "Site"

detectors[1:28,1] <- "UB"; detectors[29:52,1] <- "TU WIEN"; detectors[53:77,1] <- "IST"; detectors[78:102,1] <- "DVGW"; detectors[103:120,1] <- "UH"

colnames(detectors)
#[1] "Site"       "SAMPLES"    "Date"       "EC"         "FE"         "CP"         "SOMCPH"     "GA17PH"    
#[9] "CWPH"       "PGPH"       "PLPH"       "BifSorb"    "BifTot"     "HMBif"      "CWBif"      "PGNeo"     
#[17] "PLBif"      "TLBif"      "BacR"       "Pig2Bac"    "AllBac"     "HF183"      "FEqPCR"     "HMMit"     
#[25] "CWMit"      "PGMit"      "PLMit"      "Acesulfame" "Cyclamate"  "Saccharain" "Sucralose"  "Adeno"     
#[33] "NoV"       

# in fact if you look at the log of the detectors, they are all the same for each column so we just need that vector
#colMeans(detectors[,attributes(aslr$sum)$names])
detectors <- detectors[1,]

# ara faig un SAVE 
write.csv (detectors, file="../DATA/log-detectors2.tsv")


# HANDLE ASSAYS (originally from Persist_150114.xls)

#EVERYTHING IS IDENTICAL EXCEPT NoV tab, I just needed to change the NoV files in the "persist" folder

aged_samples_lr <- function( season , orig_data , correction , mean_coef , plot )
{
  aged_samples_lr_list <- list()    # list that will be returned
  if ( plot ){ par( ask = TRUE ) }  # if plot is TRUE: requiring user key event to continue interpreting code
  for ( attr in c("EC","FE","CP","SOMCPH","GA17PH","CWPH","PGPH","PLPH","BifSorb", "BifTot",
                  "HMBif", "CWBif", "PGNeo", "PLBif","TLBif", "NoV") ){
    
    # reading all the samples for the corresponding attribute and season, BifMol tab
    #Eli -> El Jordi m'ha creat un Script per afegir N0 (Num al temps 0), els dos documents tenen el mateix nom amb .3rd.  
    aged_samples_file_name <- paste( "../DATA/persist-Eli/" , attr , "-" , season , ".3rd.txt" , sep = "" )

    if(attr %in% c("TLBif", "HMBif", "CWBif", "PGNeo", "PLBif") ){
      aged_samples_file_name <- paste( "../DATA/persist-Eli/" , "BifMol" , "-" , season , ".3rd.txt" , sep = "" )
      
    }
    # reading all the samples for the corresponding attribute and season, BactPhg tab
    if(attr %in% c("GA17PH","CWPH","PGPH","PLPH") ){
      aged_samples_file_name <- paste( "../DATA/persist-Eli/" , "BactPhg" , "-" , season , ".3rd.txt" , sep = "" )
      
    }
    
    # Eli -> He afegit la nova columna "n0" on hi ha el valor inicial de cada expermient
    aged_samples <- read.csv( blank.lines.skip=TRUE, file = aged_samples_file_name , header = FALSE , sep = "" , col.names = c( "time" , "value", "n0" ), comment.char="#",dec = ","  )    

        # Eli -> Un cop m'obre l'arxiu se'm crea una nova columna "inact" restant a cada "value" - "n0" 
    aged_samples$inact <- aged_samples$value-aged_samples$n0    
    
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
          log10( median( orig_data[ which( orig_data$CLASS == Human ) , attr ] ) ) - aged_samples[ last_split , 2 ]  
      }
      
      # performing LR over the current essay values and storing in the list of all the essays LR coefficients
    
        # Eli -> for?o a lm a interseccio 0 i canvio "value" per "inact" 
       essays_coef <- c( essays_coef , list( lm( inact ~ time + 0, data = essay_samples )$coefficients ) )
      
      if ( plot ){ plot( essay_samples[ , 1 ] , essay_samples[ , 2 ], main = attr ) }  #plotting the samples and the line
      if ( plot ){ abline( coef = lm( inact ~ time + 0 , data = essay_samples )$coefficients ) } #corresponding to the regression
      
      last_split <- split# updating last_split variable for the next iteration
    }
    
    # converting essay_coef list to a matrix, will have two columns (intercept and slope) and as rows as essays
    essays_coef <- do.call( rbind , essays_coef )
    
    # if mean_coef is TRUE, the mean of all essays slope and increment is returned, 
    # otherwise a matrix with the slopes and increments for each one of the essays is returned
    
    #Eli -> He eliminat ,mean( essays_coef[ , 2 ] ) pq ara nom?s hi ha un valor la "pendent"
    # abans: if ( mean_coef ){ aged_samples_lr_list[[ attr ]] <- c( mean( essays_coef[ , 1 ] ) , mean( essays_coef[ , 2 ] ) )} 
    
    if ( mean_coef ){ aged_samples_lr_list[[ attr ]] <- c( mean( essays_coef[ , 1 ] ) )} 
    else { aged_samples_lr_list[[ attr ]] <- essays_coef }    
  }
  
  aged_samples_lr_list    # returning list with all the results
}

# Compute regressions for ageing essays on both seasions (assuming no correction) to get slope and increment

aslr <- c()
aslr$sum <- aged_samples_lr ( "SUMMER" , c() , correction=FALSE , mean_coef=TRUE , plot=FALSE )
aslr$win <- aged_samples_lr ( "WINTER" , c() , correction=FALSE , mean_coef=TRUE , plot=FALSE )

# Calculate slope/intercepts for non-assays variables where T90 or T99 are given

#Columns with K values ( in tab Bact qPCR Austria) -> BacR, Pig2Bac, AllBac, HF183TaqMan, FEqPCR
#HMMit,CWMit, PGMit, PLMit and Adeno  (given in Mitoc Portugal tab)

#IMPORTANT: some times are in days, so be sure to unify everything to hours

# K is exactly the slope. Now K = -1/T90 = -2/T99, whichever is given
# (intercept, slope) for each variable and season

# -> Eli: He eliminat les interseccions hi havia c(0, -1/T90)!

aslr$sum$BacR        <- c(-1/(0.64*24)); aslr$win$BacR         <- c(-1/(3.57*24));
aslr$sum$Pig2Bac     <- c(-1/(0.68*24)); aslr$win$Pig2Bac      <- c(-1/(2.22*24));
aslr$sum$AllBac      <- c(-1/(5.88*24)); aslr$win$AllBac       <- c(-1/(20.0*24));
aslr$sum$HF183TaqMan <- c(-1/(0.50*24)); aslr$win$HF183TaqMan  <- c(-1/(1.59*24));
aslr$sum$FEqPCR      <- c(-1/(0.77*24)); aslr$win$FEqPCR       <- c(-1/(2.50*24));

# PORTUGAL
# in T_99 for summer and T_90 for winter

# -> Eli: He eliminat les interseccions hi havia c(0, -1/T90)!

aslr$sum$HMMit <- c(-2/(15*24));   aslr$win$HMMit <- c(-1/(15*24));     
aslr$sum$CWMit <- c(-2/(1*24));    aslr$win$CWMit <- c(-1/(9*24));      
aslr$sum$PGMit <- c(-2/(1*24));    aslr$win$PGMit <- c(-1/(6*24));      
aslr$sum$PLMit <- c(-2/(1*24));    aslr$win$PLMit <- c(-1/(6*24));   
aslr$sum$Adeno <- c(-2/(290*24));  aslr$win$Adeno <- c(-1/(290*24));   

# AUSTRIA
# slope = (2 - log10(k))/T

# -> Eli: He eliminat les interseccions hi havia c(0, k)!

aslr$sum$Acesulfame  <- c(0)            ;aslr$win$Acesulfame    <- c(0);  
aslr$sum$Cyclamate   <- c(0.005555676)  ;aslr$win$Cyclamate     <- c(0.005555676);  
aslr$sum$Saccharain  <- c(0.004166757)  ;aslr$win$Saccharain    <- c(0.004166757);
aslr$sum$Sucralose   <- c(0)            ;aslr$win$Sucralose     <- c(0);

# select the 21 selected variables (out of the original 30)

`%!in%` = Negate(`%in%`)

aslr$sum[which(names(aslr$sum) %!in% colnames(aqua18[,modeling.vars]))] <- NULL
aslr$win[which(names(aslr$win) %!in% colnames(aqua18[,modeling.vars]))] <- NULL

#----- At this point ...
colnames(aqua18[,modeling.vars])
#[1] "EC"      "FE"      "CP"      "SOMCPH"  "GA17PH"  "CWPH"    "PGPH"    "HMBif"   "CWBif"   "PGNeo"  
#[11] "PLBif"   "TLBif"   "BacR"    "Pig2Bac" "AllBac"  "HF183"   "FEqPCR"  "CWMit"   "PGMit"   "PLMit"  
#[21] "NoV"    

attributes(aslr$sum)  #same for aslr$win
#[1] "EC"      "FE"      "CP"      "SOMCPH"  "GA17PH"  "CWPH"    "PGPH"    "HMBif"   "CWBif"   "PGNeo"  
#[11] "PLBif"   "TLBif"   "NoV"     "BacR"    "Pig2Bac" "AllBac"  "FEqPCR"  "CWMit"   "PGMit"   "PLMit"  

# ULL! NoV no surt en el mateix lloc (crec que no passa res)

# Remove little represented (unused) animals

rm(Site)
attach(aqua18)
indexes <- (TARGETtype == 'CW' | TARGETtype == 'HM' | TARGETtype == 'PG' | TARGETtype == 'PL')

aqua18.4S <- aqua18[indexes,]

# -------------------- re-factor
aqua18.4S$TARGETtype <- factor(aqua18.4S$TARGETtype)
detach(aqua18)

aqua18 <- aqua18.4S
rm (aqua18.4S)

dim(aqua18)
#[1] 106  27

### save data for later use (for the BIG MATRIX)
aqua18.preBIG <- aqua18

write.csv(aqua18.preBIG, file="aqua18-preBIG.csv")
rm(aqua18.preBIG)

# -------------------- Take ratios and log10

# Anicet's ratios (commented lines correspond to initially discarded vars)
attach(aqua18)

aqua18$SOMCPH.GA17PH <- (aqua18$SOMCPH + 1)/(aqua18$GA17PH + 1)
aqua18$SOMCPH.CWPH <- (aqua18$SOMCPH + 1)/(aqua18$CWPH + 1)
aqua18$SOMCPH.PGPH <- (aqua18$SOMCPH + 1)/(aqua18$PGPH + 1)
#aqua17$SomPhg.PLBactPhg <- (aqua17$SomPhg + 1)/(aqua17$PLBactPhg + 1)

#aqua17$BifTot.BifSorb <- (aqua17$BifTot + 1)/(aqua17$BifSorb + 1)

aqua18$TLBif.HMBif      <- (aqua18$TLBif  + 1)/(aqua18$HMBif  + 1)
aqua18$TLBif.CWBif      <- (aqua18$TLBif  + 1)/(aqua18$CWBif  + 1)
aqua18$TLBif.PGNeo      <- (aqua18$TLBif  + 1)/(aqua18$PGNeo  + 1)
aqua18$TLBif.PLBif      <- (aqua18$TLBif  + 1)/(aqua18$PLBif  + 1)

aqua18$AllBac.BacR     <- (aqua18$AllBac + 1)/(aqua18$BacR   + 1)
aqua18$AllBac.Pig2Bac  <- (aqua18$AllBac + 1)/(aqua18$Pig2Bac + 1)
aqua18$AllBac.HF183    <- (aqua18$AllBac + 1)/(aqua18$HF183 + 1)

# Andreas's ratios
aqua18$FEqPCR.BacR     <- (aqua18$FEqPCR + 1)/(aqua18$BacR   + 1)
aqua18$FEqPCR.Pig2Bac  <- (aqua18$FEqPCR + 1)/(aqua18$Pig2Bac + 1)
aqua18$FEqPCR.HF183    <- (aqua18$FEqPCR + 1)/(aqua18$HF183 + 1)

modeling.ratios <- 28:40

# take logs

for (attr in modeling.vars)
  aqua18[, attr] <- log10(aqua18[, attr] + 1)    #add one and take logs for SINGLE

for (attr in modeling.ratios)
  aqua18[, attr] <- log10(aqua18[, attr])    # we already added one, so safe

# good moment to save the data into a file

write.csv (aqua18, file="aqua18-READY.csv")

# save intercepts/slopes
write.csv (aslr$sum,file="../DATA/intercepts/summer-intercept+slopes.csv",row.names=FALSE)
write.csv (aslr$win,file="../DATA/intercepts/winter-intercept+slopes.csv",row.names=FALSE)

#---------------------------------------------------------------------------------
# STEP 1: DESCRIPTIVE STATISTICS
#---------------------------------------------------------------------------------

library(xtable)
summary(aqua18) # yet again

# basic summary statistics by groups

library(psych) # describeBy
options(digits = 2)

conts <- modeling.vars
categs <- c("Site","SEASON","TARGETtype")

# caldria polir-ho una mica
db.Site <- describeBy (aqua18[,modeling.vars], aqua18$Site)#, mat = TRUE)
#db.Season <- describeBy (aqua18[,modeling.vars], aqua18$SEASON)#, mat = TRUE)
db.TARGET <- describeBy (aqua18[,modeling.vars], aqua18$TARGETtype)#, mat = TRUE)

# caldria fer-ho de 4 en 4, seleccionant-les per grup
# universals
#pairs(aqua18[,4:7], main = "AQV", col = (1:length(levels(aqua18$CLASS)))[unclass(aqua18$CLASS)])
# humanes
#pairs(aqua18[,c(8,11,18,19)], main = "AQV", col = (1:length(levels(aqua18$CLASS)))[unclass(aqua18$CLASS)])

# i tb per TARGETtype

# Function to perform the feature selection for continuous variables: Fisher's F
# (when there are only 2 groups, it corresponds to the t-test)

TAluja.test <- function (target, grid.x, grid.y)
{
  pvalcon <- NULL

  varc <- colnames(aqua18[,modeling.vars])

  for (i in 1:length(varc)) 
    pvalcon[i] <- (oneway.test (aqua18[,varc[i]]~target))$p.value

  pvalcon <- matrix(pvalcon)
  row.names(pvalcon) <- colnames(aqua18)[modeling.vars]
  ppp <- sort(pvalcon[,1])
  
  # Graphical representation
  par (mfrow=c(grid.x,grid.y))

  for (i in 1:(grid.x*grid.y)) 
  {
    ppp.name <- attr(ppp, "names")[i]
    barplot (tapply(aqua18[,ppp.name], target, mean),main=paste("Means by",ppp.name), las=1, cex.names=0.75)
    abline (h=mean(aqua18[,ppp.name]))
    legend (0,mean(aqua18[,ppp.name]),"Global mean",bty="n")
  }
  
  # Ordered list of continuous variables according to their association to target (greater to lesser)
  ppp
}

TAluja.test (aqua18$CLASS, 2, 2)
TAluja.test (aqua18$TARGETtype, 2, 2)

par (mfrow=c(1,1))
boxplot(aqua18[,modeling.vars], las=2)
boxplot(aqua18[,modeling.ratios], las=2)

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=TRUE),],n=numtoreport)
}

mosthighlycorrelated(aqua18[,modeling.vars], 10)
mosthighlycorrelated(aqua18[,modeling.ratios], 10)

hist(aqua18[,"HMBif"])
hist(aqua18[,"CWBif"])
hist(aqua18[,"PGPH"])

# barplots

FC.cat <- cut(aqua18$EC, breaks = c(0,6,7,8,9,10))   
FC.tab <- table(FC.cat)                              
FC.tab
barplot(FC.tab)                                     # bar chart
pie(FC.tab)                                         # pie chart

Site.FC <- table(aqua18$Site, FC.cat)          # contingency table
Site.FC.rel <- round(prop.table(Site.FC, 2), digits=3)      # table of relative frequencies (column-wise)

barplot(Site.FC.rel, yaxt="n", xlab="FC", ylab="proportion", 
        col = c("white", "grey80", "grey40", "grey20", "black"), 
        main = "log10 FC by Site", xlim=c(0,9))          # stacked bar chart

legend("bottomleft", legend=rownames(Site.FC.rel), col="black", 
       fill = c("white", "grey80", "grey40", "black"), cex=0.65)

axis(2, at=seq(0, 1, 0.2))

