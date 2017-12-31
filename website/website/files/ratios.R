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
