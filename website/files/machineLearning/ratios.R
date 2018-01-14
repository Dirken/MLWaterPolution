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
