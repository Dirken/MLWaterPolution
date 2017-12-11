require(graphics)
library(ggplot2)
library(scales)
root <- "C:/Users/Meyerhofer/Downloads/UNI/MLWaterPolution/given"
folder <- "DATA"
fitxer <- "Cyprus1.csv"
fitxer.llegit <- read.csv(file.path(root, folder, fitxer), header = TRUE, sep = ",",
                            strip.white = TRUE)
# str(fitxer.llegit)
# options(scipen=1)
fitxer.llegit
dimen <- dim(fitxer.llegit)
fitxer.llegit <- sapply(fitxer.llegit, 
                        function(x){
                          if(is.numeric(x)) {scientific(as.numeric(x)) }
                          else if (is.integer(x)){ }
                          else{
                            x
                          }
                        }
) 

fitxer.llegit

