require(graphics)
library(ggplot2)
library(scales)
root <- "C:/Users/Dirken/Downloads/UNI/MLWaterPolution/website" 
folder <- "DATA"
fitxer <- "Cyprus1.csv"
fitxer.llegit <- read.csv(file.path(root, folder, fitxer), header = TRUE, sep = ",",
                            strip.white = TRUE)
# str(fitxer.llegit)
# options(scipen=1)
fitxer.llegit

fitxer.llegit <- 
      sapply(insertRow2(Dataset, newrow ,1), 
        function(x){
          if(is.numeric(x)) {scientific(as.numeric(x)) }
          else if (is.integer(x)){ 
            
          
          }
        }
      ) 
## tot el que son labels tipo <50 i tal, caca de vaca.
fitxer.llegit

