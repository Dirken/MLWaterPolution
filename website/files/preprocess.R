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