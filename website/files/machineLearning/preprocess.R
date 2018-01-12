#---------------------------------------------------------------------------------
# STEP 1:  PREPROCESSING LOADED DATA
#---------------------------------------------------------------------------------
#In the pre-processing we have to have a code that is able to detect which columns are suitable or not for the machine learning.
numeroFiles <- nrow(Dataset$data)

#In our case, we will let the user controling from which threshold of NA's he wants to discard a column.
threshold <- input$percentatgeNAs / 100
Dataset$data[, colSums(is.na(Dataset$data)) < numeroFiles * threshold]

#Getting targettype allows us to predict later in class. 
Dataset$data$TARGETtype <- as.factor(substr(Dataset$data[1],4,5))


#We create the column class in order to predict over a class e.g human vs not human, etc.
Dataset$data$CLASS <- -1



Dataset$data[which(grepl("HM",Dataset$data$SAMPLES)),]$CLASS <- 1     #1 REFERS TO HUMAN, -1 TO ANIMALS

Dataset$data$CLASS <- factor(Dataset$data$CLASS)
levels(Dataset$data$CLASS) <- c("nonhuman","human")



# Replace the "<x" values with 0. Go through all columns and make all these columns numeric along the way

modeling.vars <- 1:nrow(Dataset$data)

for (i in modeling.vars)
{
  Dataset$data[ which(grepl("<",Dataset$data[,i])),i ] <- 0
  Dataset$data[,i] <- as.numeric(Dataset$data[,i])
}

get.season <- function (s)
{ 
}
Dataset$data$SEASON <- as.factor(sapply(Dataset, get.season))

  


