#---------------------------------------------------------------------------------
#STEP 2 HANDLE DETECTIONS  ("Det limit" sheet in the Excel)
#In this sheet you will find the detection limit (call it threshold or Tau). 
#When you dilute and/or age a value, if it goes below the Tau for the variable, then make it 0. 
#The Tau should be the same for all the observations for the same variable
#---------------------------------------------------------------------------------

#These are the log values:

#i have to save the slope when i already assign it...
detectors <- Dataset$SLOPES