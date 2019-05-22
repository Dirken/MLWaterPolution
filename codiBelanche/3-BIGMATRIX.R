# TO LOAD (for convenience we place it here again)

aqua18.preBIG <- subset(read.csv(file="aqua18-preBIG.csv"), select=-X)
dim(aqua18.preBIG)
#[1] 106  27

# # ARA podria load() el detectors2 instead
# detectors  <- read.table("../DATA/log-detectors.tsv",header = TRUE, sep = "\t", dec=".", stringsAsFactors = FALSE)
# 
# colnames(detectors)[1] <- "Site"
# 
# detectors[1:28,1] <- "UB"; detectors[29:52,1] <- "TU WIEN"; detectors[53:77,1] <- "IST"; detectors[78:102,1] <- "DVGW"; detectors[103:120,1] <- "UH"

# Nou codi: detectors segons Eli
detectors  <- read.table("../DATA/log-detectors3.csv",header = TRUE, sep = ",", dec=".", stringsAsFactors = FALSE)

#trec la 1a columna, els NAs (variables no usades) no molesten
detectors <- subset(detectors, select=-X)

#---------------------------------------------------------------------------------
# STEP 3: CREACIO DE LA BIGMATRIX
#---------------------------------------------------------------------------------

M <- 10000

# "alpha" denotes the dilution degrees and "t" denotes the time in water

alphas <- rlnorm(M, 0, 0.5)
times  <- rexp(M, 2)*100

hist (alphas,probability = TRUE,xlab="Dilution degree (in logs10)",breaks=30); lines(density(alphas),col="red",lwd=1)
hist (times,probability = TRUE,xlab="Time in water (in hours)",breaks=30); lines(density(times),col="red",lwd=1)

# load intercepts/slopes (first row/second row, resp.)
aslr.sum <- read.csv (file="../DATA/intercepts/summer-intercept+slopes.csv")
aslr.win <- read.csv (file="../DATA/intercepts/winter-intercept+slopes.csv")

# a la vieja usanza de C
#INTERCEPT <- 1; SLOPE <- 2
SLOPE <- 1 # Eli

# GENERATE BIG MATRIX by sampling the original
big.matrix <- aqua18.preBIG[sample(1:nrow(aqua18.preBIG),size=M,replace=TRUE),]
#10000 27

# keep original rownumber and renumber rows consecutively
#big.matrix$N.SAMPLE <- rownames(big.matrix)
rownames(big.matrix) <- 1:M

modeling.vars <- 4:24 # copied from the preproc

mydata <- cbind(as.matrix(big.matrix[,modeling.vars]), n=1:M)
  
# ages one row of data
age_instance <- function (instance, aslr) 
{
  for (attr in attributes(aslr)$names)
    if (instance[attr] != 0)
    {
      # newval  <- (val - alpha) + (slope * time)   
      instance[attr] <- ( log10(instance[attr]) - alphas[instance["n"]] ) + ( aslr[[attr]][SLOPE]*times[instance["n"]] )

      ## <x ---> 0
      #if (instance[attr] < detectors[attr]) { instance[attr] <- 0 }
      #else instance[attr] <- 10^instance[attr]
      
      ## <x ---> x/10
      if (instance[attr] < detectors[attr]) { instance[attr] <- instance[attr] - 1 }
      instance[attr] <- 10^instance[attr]
    }
  instance
}

big.matrix.aged.sum <- t(apply( mydata[which(big.matrix$SEASON == "SUMMER"),], 1, age_instance, aslr.sum))
big.matrix.aged.win <- t(apply( mydata[which(big.matrix$SEASON == "WINTER"),], 1, age_instance, aslr.win))

# add back rest of variables
rest <- c("Site","SAMPLES","Date","SEASON","CLASS","TARGETtype")
bigsummer <- cbind(big.matrix.aged.sum, big.matrix[which(big.matrix$SEASON == "SUMMER"), rest])
bigwinter <- cbind(big.matrix.aged.win, big.matrix[which(big.matrix$SEASON == "WINTER"), rest])

BIGM.pre <- subset(rbind(bigsummer, bigwinter), select = -n)
dim(BIGM.pre)
#10000 27

# clean up a bit ...
detach(aqua18)

attach(BIGM.pre)

# re-definition for BIGM
modeling.vars <- 1:21

# Take ratios and logs:

# Anicet's ratios (commented lines correspond to initially discarded vars)

BIGM.pre$SOMCPH.GA17PH <- (BIGM.pre$SOMCPH + 1)/(BIGM.pre$GA17PH + 1)
BIGM.pre$SOMCPH.CWPH <- (BIGM.pre$SOMCPH + 1)/(BIGM.pre$CWPH + 1)
BIGM.pre$SOMCPH.PGPH <- (BIGM.pre$SOMCPH + 1)/(BIGM.pre$PGPH + 1)
#aqua17$SomPhg.PLBactPhg <- (aqua17$SomPhg + 1)/(aqua17$PLBactPhg + 1)

#aqua17$BifTot.BifSorb <- (aqua17$BifTot + 1)/(aqua17$BifSorb + 1)

BIGM.pre$TLBif.HMBif      <- (BIGM.pre$TLBif  + 1)/(BIGM.pre$HMBif  + 1)
BIGM.pre$TLBif.CWBif      <- (BIGM.pre$TLBif  + 1)/(BIGM.pre$CWBif  + 1)
BIGM.pre$TLBif.PGNeo      <- (BIGM.pre$TLBif  + 1)/(BIGM.pre$PGNeo  + 1)
BIGM.pre$TLBif.PLBif      <- (BIGM.pre$TLBif  + 1)/(BIGM.pre$PLBif  + 1)

BIGM.pre$AllBac.BacR     <- (BIGM.pre$AllBac + 1)/(BIGM.pre$BacR   + 1)
BIGM.pre$AllBac.Pig2Bac  <- (BIGM.pre$AllBac + 1)/(BIGM.pre$Pig2Bac + 1)
BIGM.pre$AllBac.HF183    <- (BIGM.pre$AllBac + 1)/(BIGM.pre$HF183 + 1)

# Andreas's ratios
BIGM.pre$FEqPCR.BacR     <- (BIGM.pre$FEqPCR + 1)/(BIGM.pre$BacR   + 1)
BIGM.pre$FEqPCR.Pig2Bac  <- (BIGM.pre$FEqPCR + 1)/(BIGM.pre$Pig2Bac + 1)
BIGM.pre$FEqPCR.HF183    <- (BIGM.pre$FEqPCR + 1)/(BIGM.pre$HF183 + 1)

# re-definition for BIGM
modeling.ratios <- 28:40

for (attr in modeling.vars)
  BIGM.pre[, attr] <- log10(BIGM.pre[, attr] + 1)    #add one and take logs for SINGLE

for (attr in modeling.ratios)
  BIGM.pre[, attr] <- log10(BIGM.pre[, attr])    # we already added one, so safe

# check for possible rows with all '0' in modelling.variables
dim(BIGM.pre)
#[1] 10000    40

nullrows <- apply (BIGM.pre[,modeling.vars], 1, function(row) all(row ==0 ))
(vv <- which (nullrows==TRUE)) # en surten uns 6 (un 0.06%); i cap amb x/10

# les eliminem i formem la matriu BIGM final, que desem a disc
if (length(vv)>0) BIGM <- BIGM.pre[-vv,] else BIGM <- BIGM.pre

write.csv(BIGM, file="BIGMATRIX-<x=x10.csv")

