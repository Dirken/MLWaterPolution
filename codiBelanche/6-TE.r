#---------------------------------------------------------------------------------
# STEP 6: LOAD & PREPROCESS DATA FRAME (TE)
#---------------------------------------------------------------------------------

# this csv file is obtained by saving from the xls file directly (; and ") and removing the last (empty) rows

aqua18.TE <- read.csv (file="../DATA/Fase2_10ml_No_Mixes_170619.csv",
                       header = TRUE, sep = ";", dec=",", check.names = TRUE, 
                       stringsAsFactors = FALSE)

# remove non-useful rows (the 10ml header and several empty lines) and renumber
# (this has been done partially with Emacs)

aqua18.TE <- subset(aqua18.TE, select=-c(X))
rownames(aqua18.TE) <- 1:nrow(aqua18.TE)

# add site for all obs

Site <- c(rep("UB",times=8),rep("TU WIEN",8),rep("IST",5),rep("DVGW",8),rep("UH",9))
aqua18.TE <- cbind(Site,aqua18.TE)

colnames(aqua18.TE)
# "Site"    "SAMPLES" "Date"    "EC"      "FE"      "CP"      "SOMCPH"  "GA17PH"  "CWPH"    "PGPH"    "HMBif"  
# "CWBif"   "PGNeo"   "PLBif"   "TLBif"   "BacR"    "Pig2Bac" "AllBac"  "HF183"   "FEqPCR"  "CWMit"   "PGMit"  
# "PLMit"   "Nov"    
dim(aqua18.TE)
# 38 obs, 21 vars (+ "Site" "SAMPLES" "Date")

# cannot create target classes (CLASS for human/non-human, TARGETtype for 4 sources)
# source info. not available

# create SEASON

get.season <- function (s)
{ 
  #April to September is SUMMER; the rest is WINTER) 
  season = "WINTER"
  month <- strtoi(strsplit(s,"/")[[1]][2],base=10)
  if (month > 3 && month < 10) {season = "SUMMER"}
  season
}

aqua18.TE$SEASON <- as.factor(sapply(aqua18.TE$Date, get.season))

# NOW TAKE CARE OF WEIRD VALUES and NAs

# there are no missing values this time
which(is.na(aqua18.TE))
# 0

# Replace the "<x" values with 0 or x/10. Go through columns EC(4) to column NoV (24) and make all these columns numeric along the way

modeling.vars.TE <- 4:24

for (i in modeling.vars.TE)
{
  ## <x ---> 0
  #aqua18.TE[ which(grepl("<",aqua18.TE[,i])),i ] <- 0
  #aqua18.TE[,i] <- as.numeric(sub(",", ".", aqua18.TE[,i]))
  
  ## <x ---> x/10
  ind <- which(grepl("<",aqua18.TE[,i]))
  aqua18.TE[ind,i] <- sub("<", "", aqua18.TE[ind,i])
  
  aqua18.TE[,i] <- as.numeric(sub(",", ".", aqua18.TE[,i]))  
  aqua18.TE[ind,i] <- aqua18.TE[ind,i]/10
}

# put dates correctly in R
aqua18.TE[,"Date"] <- as.Date(aqua18.TE[,"Date"], "%d/%m/%Y") 
aqua18.TE[,"Date"] <- format(aqua18.TE[,"Date"], format="%d %B %Y")

# # load detectors
# detectors <- read.csv (file="../DATA/log-detectors2.tsv")

# Nou codi: detectors segons Eli
detectors  <- read.table("../DATA/log-detectors3.csv",header = TRUE, sep = ",", dec=".", stringsAsFactors = FALSE)

#trec la 1a columna, els NAs (variables no usades) no molesten
detectors <- subset(detectors, select=-X)

# -------------------- Take ratios and log10
rm(Site)

# Anicet's ratios (commented lines correspond to initially discarded vars)

aqua18.TE$SOMCPH.GA17PH <- (aqua18.TE$SOMCPH + 1)/(aqua18.TE$GA17PH + 1)
aqua18.TE$SOMCPH.CWPH <- (aqua18.TE$SOMCPH + 1)/(aqua18.TE$CWPH + 1)
aqua18.TE$SOMCPH.PGPH <- (aqua18.TE$SOMCPH + 1)/(aqua18.TE$PGPH + 1)
#aqua17$SomPhg.PLBactPhg <- (aqua17$SomPhg + 1)/(aqua17$PLBactPhg + 1)

#aqua17$BifTot.BifSorb <- (aqua17$BifTot + 1)/(aqua17$BifSorb + 1)

aqua18.TE$TLBif.HMBif      <- (aqua18.TE$TLBif  + 1)/(aqua18.TE$HMBif  + 1)
aqua18.TE$TLBif.CWBif      <- (aqua18.TE$TLBif  + 1)/(aqua18.TE$CWBif  + 1)
aqua18.TE$TLBif.PGNeo      <- (aqua18.TE$TLBif  + 1)/(aqua18.TE$PGNeo  + 1)
aqua18.TE$TLBif.PLBif      <- (aqua18.TE$TLBif  + 1)/(aqua18.TE$PLBif  + 1)

aqua18.TE$AllBac.BacR     <- (aqua18.TE$AllBac + 1)/(aqua18.TE$BacR   + 1)
aqua18.TE$AllBac.Pig2Bac  <- (aqua18.TE$AllBac + 1)/(aqua18.TE$Pig2Bac + 1)
aqua18.TE$AllBac.HF183    <- (aqua18.TE$AllBac + 1)/(aqua18.TE$HF183 + 1)

# Andreas's ratios
aqua18.TE$FEqPCR.BacR     <- (aqua18.TE$FEqPCR + 1)/(aqua18.TE$BacR   + 1)
aqua18.TE$FEqPCR.Pig2Bac  <- (aqua18.TE$FEqPCR + 1)/(aqua18.TE$Pig2Bac + 1)
aqua18.TE$FEqPCR.HF183    <- (aqua18.TE$FEqPCR + 1)/(aqua18.TE$HF183 + 1)


modeling.ratios.TE <- 26:38

# take logs

for (attr in modeling.vars.TE)
  aqua18.TE[, attr] <- log10(aqua18.TE[, attr] + 1)    #add one and take logs for SINGLE

for (attr in modeling.ratios.TE)
  aqua18.TE[, attr] <- log10(aqua18.TE[, attr])    # we already added one, so safe

# save the data into a file
write.csv (aqua18.TE, file="aqua18-READY-TE.csv")

aqua18.TE <- read.csv (file="aqua18-READY-TE.csv")
aqua18.TE <- subset(aqua18.TE, select=-c(X))

#### zero checking
nullrows <- apply (aqua18.TE[,modeling.vars.TE], 1, function(row) all(row ==0 ))
(vv <- which (nullrows==TRUE)) # no n'hi ha cap

res <- colSums( subset(aqua18.TE, select=-c(X,Site,SAMPLES,Date)) ==0)/nrow(aqua18.TE)*100
mean(res); plot.res (res)
# around 35% 

# si no usem ratios # ESTE
res <- colSums( aqua18.TE[,modeling.vars.TE] ==0)/nrow(aqua18.TE)*100
mean(res); plot.res (res)

##############################################################################################################
## modelling and prediction with Anicet & Eli's choices

final.RFmodel <- function (target, vars, ntrees=200)
{
  vars2 <- unlist(lapply(strsplit (vars, "+"), paste, collapse = ""))
  fmla <- as.formula(paste( paste(target," ~ "), paste(vars2, collapse= "+")))
  randomForest (fmla, data=BIGM, ntree=ntrees, proximity=FALSE, keep.forest=TRUE)
}

final.LDAmodel <- function (target, vars)
{
  vars2 <- unlist(lapply(strsplit (vars, "+"), paste, collapse = ""))
  fmla <- as.formula(paste( paste(target," ~ "), paste(vars2, collapse= "+")))
#  l <- lda (x=BIGM[,modeling.vars], grouping=BIGM$CLASS, CV=FALSE)
  l <- lda (fmla, data=BIGM, CV=FALSE)
  predict (l, aqua18.TE[,modeling.vars.TE])$class
}


# 1) Replacing “<x” by 0

RF.modelling.prediction <- function ()
{
  # modelling
  model.single.HnotH <- final.RFmodel ("CLASS", "HF183 + GA17PH")
  model.mollec.HnotH <- final.RFmodel ("CLASS", "HF183 + NoV")
  model.single.4S.1 <- final.RFmodel ("TARGETtype", "HF183 + BacR + PLBif")
  model.single.4S.2 <- final.RFmodel ("TARGETtype", "HF183 + BacR + PLBif + Pig2Bac")
  #model.mollec.4S <- final.RFmodel ("TARGETtype", "NoV+ PGMit + CWMit  +  PLMit + CWBif + PLBif")

  # # prediction
  # p <- as.data.frame(predict (model.single.HnotH, aqua18.TE, type = "prob"))
  # cbind(p, source=predict (model.single.HnotH, aqua18.TE))
  # 
  # p <- as.data.frame(predict (model.mollec.HnotH, aqua18.TE, type = "prob"))
  # cbind(p, source=predict (model.mollec.HnotH, aqua18.TE))
  # 
  # p <- as.data.frame(predict (model.single.4S, aqua18.TE, type = "prob"))
  # cbind(p, source=predict (model.single.4S, aqua18.TE))
  # 
  # p <- as.data.frame(predict (model.mollec.4S, aqua18.TE, type = "prob"))
  # cbind(p, source=predict (model.mollec.4S, aqua18.TE))

  # alltogether
  return ( data.frame (single.HnotH = predict (model.single.HnotH, aqua18.TE), 
                     mollec.HnotH = predict (model.mollec.HnotH, aqua18.TE), 
                     single.4S.1 = predict (model.single.4S.1, aqua18.TE),
                     single.4S.2 = predict (model.single.4S.2, aqua18.TE)) )
}

RF.modelling.prediction ()

# 2) Replacing “<x” by x/10

# with LDA
l <- lda (x=BIGM[,modeling.vars], grouping=BIGM$CLASS, CV=TRUE)
table (Truth=BIGM$CLASS, Preds=l$class)

l <- lda (x=BIGM[,modeling.vars], grouping=BIGM$CLASS, CV=FALSE)
plot(l)

p <- predict (l, aqua18.TE[,modeling.vars.TE])

modelling.prediction ()

# with RFs
RF.modelling.prediction ()
