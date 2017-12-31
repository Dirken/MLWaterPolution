
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