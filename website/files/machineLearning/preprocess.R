#---------------------------------------------------------------------------------
# STEP 1:  PREPROCESSING LOADED DATA
#---------------------------------------------------------------------------------
DatasetAux$data <- Dataset$data 
DatasetAux$data$SEASON <- Dataset$season
  
#In the pre-processing we need a code that is able to detect which columns are suitable or not for the machine learning.
numeroFiles <- nrow(DatasetAux$data)

#In our case, we will let the user controling from which threshold of NA's he wants to discard a column.
threshold <- input$percentatgeNAs / 100
DatasetAux$data[, colSums(is.na(DatasetAux$data)) < numeroFiles * threshold]

#Getting targettype allows us to predict later in class. 
DatasetAux$data$TARGETtype <- as.factor(substr(DatasetAux$data[,1],4,5))

#We create the column class in order to predict over a class e.g human vs not human, etc.
DatasetAux$data$CLASS <- 0

#Now we get the data from the variable vs -variable on the Scenario tab. 
#input$myPicker contains the values that we want to predict
#input$myPicker2 contains the values against the ones we want
modeling.vars <- 1:nrow(DatasetAux$data)

for (i in modeling.vars){
  if(match(DatasetAux$data$TARGETtype[i], input$myPicker)){
    DatasetAux$data$CLASS[i] <- 1   
  }
  else if (match(DatasetAux$data$TARGETtype[i], input$myPicker2)){
    DatasetAux$data$CLASS[i] <- -1   
  }
}

# Replace the "<x" values with 0. Go through all columns and make all these columns numeric along the way
for (i in modeling.vars){
  DatasetAux$data[ which(grepl("<",DatasetAux$data[,i])),i ] <- 0
  #DatasetAux$data[,i] <- as.numeric(DatasetAux$data[,i])
}
#season is assigned every time save is clicked in DatasetAux$data$SEASON



#---------------------------------------------------------------------------------
#STEP 2 HANDLE DETECTIONS  ("Det limit" sheet in the Excel)
#In this sheet you will find the detection limit (call it threshold or Tau). 
#When you dilute and/or age a value, if it goes below the Tau for the variable, then make it 0. 
#The Tau should be the same for all the observations for the same variable
#---------------------------------------------------------------------------------
detectors <- Dataset$SLOPES


#logarithms

# for (attr in modeling.vars)
#   aqua17[, attr] <- log10(aqua17[, attr] + 1)    #add one and take logs for SINGLE
# 
# for (attr in modeling.ratios)
#   aqua17[, attr] <- log10(aqua17[, attr])    # we already added one, so safe


################################## 
# SCENARIOS
##################################
library (MASS)
source("FSS.R", local = TRUE)

#training

if(input$algorithm == "LDA"){
  if(input$molecular){
    
  }
  else{#all
    
  }
}
elseif(input$algorithm == "QDA"){
  if(input$molecular){
  }
  else{#all
    
  }
}



################################## 
# BIGMATRIX
##################################


################################## 
# SCENARIOS ON BIGMATRIX
##################################
if(input$algorithm == "LDA"){
  if(input$molecular){
    
  }
  else{#all
    
  }
}
elseif(input$algorithm == "QDA"){
  if(input$molecular){
  }
  else{#all
    
  }
}



  