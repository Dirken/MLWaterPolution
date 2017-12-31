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
