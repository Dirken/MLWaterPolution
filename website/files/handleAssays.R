#---------------------------------------------------------------------------------
#STEP 3 HANDLE ASSAYS ( from Persist_150114.xls)
#---------------------------------------------------------------------------------

#EVERYTHING IS IDENTICAL EXCEPT NoV tab, I just needed to change the NoV files in the "persist" folder

aged_samples_lr <- function( season , orig_data , correction , mean_coef , plot ){
  aged_samples_lr_list <- list()      # list that will be returned
  if ( plot ){ par( ask = TRUE ) }   # if plot is TRUE: requiring user key event to continue interpreting code
  for ( attr in c("EC","FE","CP","SomPhg","HMBactPhg","CWBactPhg","PGBactPhg","PLBactPhg","BifSorb", "BifTot",
                  "HMBif", "CWBif", "PGNeo", "PLBif","TLBif", "NoV") ){
    
    # reading all the samples for the corresponding attribute and season, BifMol tab
    aged_samples_file_name <- paste( "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/website/persist/mediterrani/" , attr , "-" , season , ".txt" , sep = "" )
    if(attr %in% c("TLBif", "HMBif", "CWBif", "PGNeo", "PLBif") ){
      aged_samples_file_name <- paste( "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/website/persist/mediterrani/" , "BifMol" , "-" , season , ".txt" , sep = "" )
    }
    # reading all the samples for the corresponding attribute and season, BactPhg tab
    if(attr %in% c("HMBactPhg","CWBactPhg","PGBactPhg","PLBactPhg") ){
      aged_samples_file_name <- paste( "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website/website/persist/mediterrani/" , "BactPhg" , "-" , season , ".txt" , sep = "" )
    }
    
    aged_samples <- read.csv( blank.lines.skip=TRUE, file = aged_samples_file_name , header = FALSE , sep = "" , col.names = c( "time" , "value" ), comment.char="#",dec = ","  )    
    essays_coef <- list() # where the assays LR coefficients (slope and increment) will be stored
    
    # obtaining in which indexes of the aged samples start each one of the essays (new essay => t=0)
    essay_splits <- c( which( aged_samples$time == 0 ) , length( aged_samples$time ) + 1 )
    essay_splits <- essay_splits[ -1 ] # removing the first (there will always be a zero on first aged_samples row)
    last_split <- 1 # initializing last split variable to the beginning of th first essay
    
    # for each one of the essays of the current attribute and season
    for ( split in essay_splits ){      # obtaining the samples of the current essay      
      essay_samples <- aged_samples[ last_split : ( split - 1 ) , ] # selecting only the essay_samples
      
      #TODO: will we use this? probably not, but if so we need to move HUMAN constant to top
      if ( correction ){    #values are considered slightly diluted and therefore are corrected
        # correction is done using the log10 of the median of the attribute human rows of the original matrix
        essay_samples[ , 2 ] <- essay_samples[ , 2 ] + 
          log10( median( orig_data[ which( orig_data$CLASS == HUMAN ) , attr ] ) ) - aged_samples[ last_split , 2 ]  
      }
      
      # performing LR over the current essay values and storing in the list of all the essays LR coefficients
      essays_coef <- c( essays_coef , list( lm( value ~ time , data = essay_samples )$coefficients ) )
      
      if ( plot ){ plot( essay_samples[ , 1 ] , essay_samples[ , 2 ], main = attr ) }  #plotting the samples and the line
      if ( plot ){ abline( coef = lm( value ~ time , data = essay_samples )$coefficients ) } #corresponding to the regression
      
      last_split <- split# updating last_split variable for the next iteration
    }
    
    # converting essay_coef list to a matrix, will have two columns (intercept and slope) and as rows as essays
    essays_coef <- do.call( rbind , essays_coef )
    
    # if mean_coef is TRUE, the mean of all essays slope and increment is returned, 
    # otherwise a matrix with the slopes and increments for each one of the essays is returned
    if ( mean_coef ){ aged_samples_lr_list[[ attr ]] <- c( mean( essays_coef[ , 1 ] ) , mean( essays_coef[ , 2 ] ) )} 
    else { aged_samples_lr_list[[ attr ]] <- essays_coef }    
  }
  
  aged_samples_lr_list    # returning list with all the results
}