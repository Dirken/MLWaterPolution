#---------------------------------------------------------------------------------
# STEP 1:  PREPROCESSING LOADED DATA
#---------------------------------------------------------------------------------
#In the pre-processing we need a code that is able to detect which columns are suitable or not for the machine learning.
numeroFiles <- nrow(Dataset$data)

#In our case, we will let the user controling from which threshold of NA's he wants to discard a column.
threshold <- input$percentatgeNAs / 100
Dataset$data[, colSums(is.na(Dataset$data)) < numeroFiles * threshold]

#Getting targettype allows us to predict later in class. 
Dataset$data$TARGETtype <- as.factor(substr(Dataset$data[,1],4,5))

#We create the column class in order to predict over a class e.g human vs not human, etc.
Dataset$data$CLASS <- 0

#Now we get the data from the variable vs -variable on the Scenario tab. 
#input$myPicker contains the values that we want to predict
#input$myPicker2 contains the values against the ones we want
modeling.vars <- 1:nrow(Dataset$data)

for (i in modeling.vars){
  #this is not smart since i am not using what they choose 
  if(match(Dataset$data[i]$TARGETtype, input$myPicker)){
    Dataset$data$CLASS <- 1   
  }
  else if (match(Dataset$data[i]$TARGETtype, input$myPicker2)){
    Dataset$data$CLASS <- -1   
  }
}

# Replace the "<x" values with 0. Go through all columns and make all these columns numeric along the way
for (i in modeling.vars){
  Dataset$data[ which(grepl("<",Dataset$data[,i])),i ] <- 0
  Dataset$data[,i] <- as.numeric(Dataset$data[,i])
}
#season is assigned every time save is clicked.
