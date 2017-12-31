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